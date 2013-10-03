package org.gark87.yajom.macros

import org.gark87.yajom.base.BaseMapper
import language.experimental.macros
import scala.collection.mutable
import scala.reflect.macros.Universe

class CreateOnNull {


  def findSetter[T: y.c.WeakTypeTag](y: YajomContext)
                                  (qualifier: y.c.Tree, name: y.c.Name, args: List[y.c.Tree],
                                   notGetter: (y.c.universe.MethodSymbol) => y.c.Tree,
                                   ok: (String, y.c.Type) => y.c.Tree): y.c.Tree = {
    import y.c.universe._

    val getterName = name.decoded
    val getter: MethodSymbol = qualifier.tpe.member(name).asMethod
    val correctParams = getter.paramss match {
      case List(List()) => true
      case _ => false
    }
    val correctName = getterName.startsWith("get") || getterName.startsWith("is")
    val setterName = getterName.replaceFirst("^(is|get)", "set")
    val returnType: y.c.Type = getter.returnType
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
      y.reporter.error("Cannot find setter for " + name + " @ " + qualifier)
    } else {
      ok(setterName, returnType)
    }
  }

  def process[T: y.c.WeakTypeTag](y: YajomContext)(expr: y.c.Expr[T], objectFactoryType: y.c.Type): y.c.Tree = {
    import y.c.universe._

    val reporter = y.reporter
    val vars = new mutable.HashMap[String, Tree]()
    var prefix: List[y.c.universe.Tree] = List()

    val argsConverter = y.argsConverter

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
          findSetter[T](y)(qualifier, name, args, (getter) => {
            val convert = argsConverter.convert(y)(getter, allArgs._1, vars, objectFactoryType)
            val result = convert.foldLeft[Tree](allArgs._2)((tree: Tree, args: List[Tree]) => {
              Apply(tree, args)
            })
            y.c.resetAllAttrs(result)
          }, (setterName, returnType) => {
            val resultValue = newTermName(y.c.fresh("CON_resultValue$"))
            val e = y.c.Expr[Any](tree)
            val newValue = newTermName(y.c.fresh("CON_newValue$"))
            val setterValue = y.c.Expr[T](Block(
              ValDef(Modifiers(), newValue, TypeTree(), y.creator.createDefaultObject(y)(returnType, objectFactoryType)),
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
          ValDef(mods, name, tpt, y.c.resetAllAttrs(addNullGuards(rhs)))
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
    val guards: Tree = addNullGuards(expr.tree)
    guards match {
      case Block(a, b) => val d: List[Tree] = prefix ::: a
        Block(d, y.c.resetAllAttrs(b))
      case Function(valdefs, body) => Function(valdefs, Block(prefix, body))
      case any =>
        Block(prefix, y.c.resetAllAttrs(any))
    }
  }
}

object CreateOnNull {
  def macroImpl[F: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(func: c.Expr[F])(m: c.Expr[M]): c.Expr[F] = {
    import c.universe._

    val y = new YajomContext(c)
    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head
    c.Expr[F](y.createOnNull.process[F](y)(func.asInstanceOf[y.c.Expr[F]], objectFactoryType.asInstanceOf[y.c.Type]).asInstanceOf[c.Tree])
  }
}
