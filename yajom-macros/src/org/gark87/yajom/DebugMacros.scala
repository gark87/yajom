package org.gark87.yajom

import language.experimental.macros
import scala.reflect.runtime.universe.TypeTag
import scala.util.regexp.Base


object DebugMacros {

  def copy[F, T](from: F)(setter: (T) => Unit)(implicit m: BaseMapper, see: TypeTag[T]): Unit = macro mapImpl[F, T]


  def mapImpl[F: c.WeakTypeTag, T: c.WeakTypeTag](c: reflect.macros.Context)(from: c.Expr[F])(setter: c.Expr[(T) => Unit])(m: c.Expr[BaseMapper], see: c.Expr[TypeTag[T]])
  : c.Expr[Unit] = {
    import c.universe._

    val fromType = from.actualType
    val toType = setter.actualType match {
      case TypeRef(_, _, q) => q.head
      case _ => c.abort(c.enclosingPosition, "OOPS")
    }
    val thisRef = m match {
      case Expr(Select(a, _)) => a
      case _ => c.abort(c.enclosingPosition, "OOPS")
    }
    if (fromType == toType) {
      val castedFrom: c.Expr[T] = from.asInstanceOf[c.Expr[T]]
      reify {
        setter.splice(castedFrom.splice)
      }
    } else {
      val freshName = newTermName(c.fresh("fromValue$"))
      val nonTrivialName = newTermName(c.fresh("nonTrivial$"))
      val freshIdent = c.Expr[Any](Ident(freshName))
      val mapCall = c.Expr[Any](Apply(Select(thisRef, newTermName("map")), List(Ident(freshName), Ident(nonTrivialName))))
      def createVal(name: TermName, value: Expr[_], tpe: Type) = ValDef(Modifiers(), name, TypeTree(tpe), value.tree);
      val fromValueCalc = createVal(freshName, reify {
        from.splice
      }, fromType)
      val nonTrivialCall = createVal(nonTrivialName, reify {
        m.splice.create(freshIdent.splice, see.splice, null, setter.splice)
      }, toType)
      val nonTrivialIdent = c.Expr[T](Ident(nonTrivialName))

      val mapCall2 = reify {
        if (freshIdent.splice != null && nonTrivialIdent.splice != null)
          mapCall.splice
      }.tree
      c.Expr[Unit](
        Block(fromValueCalc, nonTrivialCall, mapCall2)
      )
    }
  }

  def nullSafe[T](expr: (T) => Unit)(implicit m: BaseMapper, see: TypeTag[T]): (T) => Unit = macro withNullGuards[T]


  def withNullGuards[T](c: reflect.macros.Context)(expr: c.Expr[(T) => Unit])(m: c.Expr[BaseMapper], see: c.Expr[TypeTag[T]]): c.Expr[(T) => Unit] = {
    import c.universe._

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
          val ReturnType = getter.returnType
          val setter = qualifier.tpe.members.find((x: Symbol) => {
            if (!x.isMethod)
              false
            else {
              val method: MethodSymbol = x.asMethod
              val correctSetterParams = method.paramss match {
                case List(List(termSymbol)) => termSymbol.asTerm.typeSignature == ReturnType
                case _ => false
              }
              method.isMethod && correctSetterParams && method.isPublic && setterName == method.name.decoded
            }
          })
        val newQ = addNullGuards(qualifier);
          if (!correctName || !correctParams) {
            Select(newQ, name)
          } else if (setter.isEmpty) {
            c.abort(c.enclosingPosition, "Cannot find setter")
          } else {
            val synthetic = (0l + scala.reflect.internal.Flags.SYNTHETIC).asInstanceOf[c.universe.FlagSet]
            val setterM = c.Expr[(Any) => Unit](Select(newQ, setter.get.asMethod.name))
            val getterM = c.Expr[() => Any](Select(newQ, getterName))

            val resultValue = newTermName(c.fresh("resultValue$"))
            val runtimeChildTpe = c.Expr[scala.reflect.runtime.universe.TypeTag[Any]](c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, ReturnType))
            prefix = prefix :+ ValDef(Modifiers(synthetic), resultValue, TypeTree(),
                  reify {
                    m.splice.create(null, runtimeChildTpe.splice, getterM.splice, setterM.splice)
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
        case a => a
      }
    }
    val guards: c.universe.Tree = addNullGuards(expr.tree)
    if (prefix.isEmpty)
      c.abort(c.enclosingPosition, "no need of `nullSafe'")
    val p = guards match {
      case Block(a, b) => val d: List[c.universe.Tree] = prefix ::: a; c.Expr[T => Unit](Block(d, c.resetAllAttrs(b)))
      case _ => c.abort(c.enclosingPosition, "no need of `nullSafe'")
    }
    p
  }

}

