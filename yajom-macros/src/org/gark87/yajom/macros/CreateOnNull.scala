package org.gark87.yajom.macros

import org.gark87.yajom.base.BaseMapper
import language.experimental.macros
import scala.collection.mutable
import scala.reflect.macros.Universe

class CreateOnNull(creator: ObjectCreator) {


  def findSetter[T: c.WeakTypeTag](c: reflect.macros.Context)
                                  (qualifier: c.Tree, name: c.Name, args: List[c.Tree],
                                   notGetter: (c.universe.MethodSymbol) => c.Tree,
                                   ok: (String, c.Type) => c.Tree): c.Tree = {
    import c.universe._

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
    if (!correctName || !correctParams || !args.isEmpty) {
      notGetter(getter)
    } else if (setter.isEmpty) {
      creator.reporter.error("Cannot find setter for " + name + " @ " + qualifier)
    } else {
      ok(setterName, returnType)
    }
  }

  def process[T: c.WeakTypeTag](c: reflect.macros.Context)(expr: c.Expr[T], objectFactoryType: c.Type): c.Expr[T] = {
    import c.universe._

    val reporter = creator.reporter
    val vars = new mutable.HashMap[String, Tree]()
    var prefix: List[c.universe.Tree] = List()

    val argsConverter = new ArgsConverter(reporter)

    def collectAllArgs(tree: Tree, list : List[List[Tree]]) : (List[List[Tree]], Tree, Tree, Name) = {
      tree match {
        case Apply(call, args) => collectAllArgs(call, args :: list)
        case TypeApply(Select(qualifier, name), typeArgs) => (list, addNullGuards(tree), qualifier, name)
        case Select(qualifier, name) => (list, addNullGuards(tree), qualifier, name)
        case _ => reporter.error("Cannot find Select, have instead: "+ tree)
      }
    }

    def addNullGuards(tree: Tree): Tree = {
      tree match {
        case Apply(select, args) => {
          val allArgs = collectAllArgs(tree, List())
          val name = allArgs._4
          val qualifier = allArgs._3
          val getter: MethodSymbol = qualifier.tpe.member(name).asMethod
          findSetter[T](c)(qualifier, name, args, (getter) => {
            val convert = argsConverter.convert(c)(getter, allArgs._1, vars, objectFactoryType)
            val result = convert.foldLeft[Tree](allArgs._2)((tree: Tree, args: List[Tree]) => {
              Apply(tree, args)
            })
            c.resetAllAttrs(result)
          }, (setterName, returnType) => {
            val resultValue = newTermName(c.fresh("CON_resultValue$"))
            val e = c.Expr[Any](tree)
            val newValue = newTermName(c.fresh("CON_newValue$"))
            val setterValue = c.Expr[T](Block(
              ValDef(Modifiers(), newValue, TypeTree(), creator.createDefaultObject(c)(returnType, objectFactoryType).tree),
              Apply(Select(qualifier, newTermName(setterName)), List(Ident(newValue))),
              Ident(newValue)
            ))

            prefix = prefix :+ ValDef(Modifiers(), resultValue, TypeTree(),
              reify {
                val CON_oldValue = e.splice
                if (CON_oldValue == null) {
                  setterValue.splice
                } else
                  CON_oldValue
              }.tree
            )
            Ident(resultValue)
          })
        }
        case Block(stats, epr) =>
          Block(stats.map((s: Tree) => {
            addNullGuards(s)
          }), addNullGuards(epr))
        case ValDef(mods, name, tpt, rhs) => {
          vars.put(name.decoded, rhs)
          ValDef(mods, name, tpt, c.resetAllAttrs(addNullGuards(rhs)))
        }
        case Select(qualifier, name) => Select(addNullGuards(qualifier), name)
        case Ident(name) => tree
        case This(a) => tree
        case Function(valdefs, body) => tree
        case Literal(literal) => tree
        case TypeApply(Select(qualifier, name), typeArgs) => TypeApply(Select(addNullGuards(qualifier), name), typeArgs)
        case expr1 => reporter.error("Too complex expression `" + expr1 + "` for YAJOM CreateOnNull:\n1. Quick Fix: extract val without CreateOnNull\n2. mail gark87 <my_another@mail.ru>")
      }
    }
    val guards: c.universe.Tree = addNullGuards(expr.tree)
    guards match {
      case Block(a, b) => val d: List[c.universe.Tree] = prefix ::: a
        c.Expr[T](Block(d, c.resetAllAttrs(b)))
      case Function(valdefs, body) => c.Expr[T](Function(valdefs, Block(prefix, body)))
      case any =>
        c.Expr[T](Block(prefix, c.resetAllAttrs(any)))
    }
  }
}

object CreateOnNull {
  def macroImpl[F: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(func: c.Expr[F])(m: c.Expr[M]): c.Expr[F] = {
    import c.universe._

    val reporter = new ErrorReporter(c)
    val creator: ObjectCreator = new ObjectCreator(reporter)

    val createOnNull: CreateOnNull = new CreateOnNull(creator)
    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head
    createOnNull.process[F](c)(func, objectFactoryType)
  }
}
