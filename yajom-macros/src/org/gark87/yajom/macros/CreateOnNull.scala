package org.gark87.yajom.macros

import org.gark87.yajom.api.BaseMapper
import language.experimental.macros


class CreateOnNull(creator: ObjectCreator) {

  def process[T: c.WeakTypeTag](c: reflect.macros.Context)(expr: c.Expr[T], objectFactoryType: c.Type): c.Expr[T] = {
    import c.universe._

    val reporter = creator.reporter
    var prefix: List[c.universe.Tree] = List()

    def addNullGuards(tree: Tree): Tree = {
      tree match {
        case Apply(Select(qualifier, name), List()) => {
          val getterName = name.decoded
          val getter: MethodSymbol = qualifier.tpe.member(name).asMethod
          val correctParams = getter.paramss match {
            case List(List()) => true
            case _ => false
          }
          val correctName = getterName.startsWith("get") || getterName.startsWith("is")
          val setterName = getterName.replaceFirst("^(is|get)", "set")
          val returnType: c.Type = getter.returnType
          val setter = qualifier.tpe.members.find((x: Symbol) => {
            if (!x.isMethod)
              false
            else {
              val method: MethodSymbol = x.asMethod
              val correctSetterParams = method.paramss match {
                case List(List(termSymbol)) => termSymbol.asTerm.typeSignature == returnType
                case _ => false
              }
              method.isMethod && correctSetterParams && method.isPublic && setterName == method.name.decoded
            }
          })
          val newQ = addNullGuards(qualifier)
          if (!correctName || !correctParams) {
            Select(newQ, name)
          } else if (setter.isEmpty) {
            reporter.error("Cannot find setter")
          } else {
            val synthetic = (0l + scala.reflect.internal.Flags.SYNTHETIC).asInstanceOf[c.universe.FlagSet]
            val resultValue = newTermName(c.fresh("resultValue$"))
            val e = c.Expr[Any](tree)
            val o = creator.createDefaultObject(c)(returnType, objectFactoryType)

            prefix = prefix :+ ValDef(Modifiers(synthetic), resultValue, TypeTree(),
              reify {
                val tmp = e.splice
                if (tmp == null)
                  o.splice
                else
                  tmp
              }.tree
            )
            Ident(resultValue)
          }
        }
        case Apply(fun, args) =>
          Apply(addNullGuards(fun), args)
        case Block(stats, epr) =>
          Block(stats.map((s: Tree) => {
            addNullGuards(s)
          }), epr)
        case ValDef(mods, name, tpt, rhs) =>
          ValDef(mods, name, tpt, addNullGuards(rhs))
        case Ident(name) => Ident(name)
        case a => reporter.error("Too complex expression `" + a + "` for YAJOM:\n1. Quick Fix: extract val without YAJOM\n2. mail gark87 <my_another@mail.ru>")
      }
    }
    val guards: c.universe.Tree = addNullGuards(expr.tree)
    guards match {
      case Block(a, b) => val d: List[c.universe.Tree] = prefix ::: a
        c.Expr[T](Block(d, c.resetAllAttrs(b)))
      case Ident(name) =>
        c.Expr[T](Block(prefix, c.resetAllAttrs(Ident(name))))
      case _ => reporter.error("no need of `nullSafe'")
    }
  }
}

object CreateOnNull {
  def createOnNull[F, M <: BaseMapper[_]](func: F)(implicit m: M): F = macro macroImpl[F, M]

  def macroImpl[F: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(func : c.Expr[F])(m : c.Expr[M]) : c.Expr[F] = {
    import c.universe._

    val reporter = new ErrorReporter(c)
    val creator: ObjectCreator = new ObjectCreator(reporter)

    val createOnNull: CreateOnNull = new CreateOnNull(creator)
    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head
    createOnNull.process[F](c)(func, objectFactoryType)
  }
}
