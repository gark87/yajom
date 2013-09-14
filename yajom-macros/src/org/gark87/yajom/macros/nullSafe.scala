package org.gark87.yajom.macros

import language.experimental.macros
import scala.reflect.runtime.universe.TypeTag
import org.gark87.yajom.api.BaseMapper

object nullSafe {

  def update[F, T, M <: BaseMapper[_]](setter: (T) => Unit, from: F)(implicit m: M, see: TypeTag[T]): Unit = macro mapImpl[F, T, M]

  def mapImpl[F: c.WeakTypeTag, T: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(setter: c.Expr[(T) => Unit], from: c.Expr[F])(m: c.Expr[M], see: c.Expr[TypeTag[T]])
  : c.Expr[Unit] = {
    import c.universe._

    val reporter = new ErrorReporter(c)

    val fromType = from.actualType
    val toType = setter.actualType match {
      case TypeRef(_, _, q) => q.head
      case _ => reporter.internal("Unexpected <to> type")
    }
    val thisRef = This(c.enclosingClass.symbol)
    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head
    val creator: ObjectCreator = new ObjectCreator(reporter)


    def createVal(name: TermName, value: Expr[_], tpe: Type) = ValDef(Modifiers(), name, TypeTree(tpe), value.tree)


    def withNullGuards(expr: c.Expr[(T) => Unit]): c.Expr[(T) => Unit] = {
      var prefix: List[c.universe.Tree] = List()

      def addNullGuards(tree: Tree): Tree = {
        tree match {
          case Apply(Select(qualifier, name), List()) =>
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
              val o = creator.createDefaultObject(c) (returnType, objectFactoryType)

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
          case Apply(fun, args) =>
            Apply(addNullGuards(fun), args)
          case Block(stats, epr) =>
            Block(stats.map((s: Tree) => {
              addNullGuards(s)
            }), epr)
          case ValDef(mods, name, tpt, rhs) =>
            ValDef(mods, name, tpt, addNullGuards(rhs))
          case a => reporter.error("Too hard expression for YAJOM:\n1. Quick Fix: extract val without YAJOM\n2. mail gark87 <my_another@mail.ru>")
        }
      }
      val guards: c.universe.Tree = addNullGuards(expr.tree)
      guards match {
        case Block(a, b) => val d: List[c.universe.Tree] = prefix ::: a
          c.Expr[T => Unit](Block(d, c.resetAllAttrs(b)))
        case _ => reporter.error("no need of `nullSafe'")
      }
    }

    if (fromType == toType) {
      val castedFrom: c.Expr[T] = from.asInstanceOf[c.Expr[T]]
      val guards = withNullGuards(setter)
      c.Expr[Unit](reify {
        guards.splice(castedFrom.splice)
      }.tree)
    } else {
      val fromValueName = newTermName(c.fresh("yajom_fromValue$"))
      val fromValueDef = c.Expr[F](createVal(fromValueName, from, fromType))
      val fromValueIdent = c.Expr[Any](Ident(fromValueName))

      val toValueName = newTermName(c.fresh("yajom_toValue$"))
      val toValueDef = c.Expr[T](createVal(toValueName, creator.createObjectFrom(c)(toType, from.actualType, from, objectFactoryType), toType))
      val toValueIdent = c.Expr[T](Ident(toValueName))

      val mapCall = c.Expr[Any](Apply(Select(thisRef, newTermName("map")), List(Ident(fromValueName), Ident(toValueName))))

      c.Expr[Unit](reify {
        fromValueDef.splice
        if (fromValueIdent.splice != null) {
          toValueDef.splice
          if (toValueIdent.splice != null)
            mapCall.splice
        }
      }.tree)
    }
  }

}

