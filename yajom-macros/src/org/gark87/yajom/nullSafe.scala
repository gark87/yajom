package org.gark87.yajom

import language.experimental.macros
import scala.reflect.runtime.universe.TypeTag

object nullSafe {

  def update[F, T, M <: BaseMapper[_]](setter: (T) => Unit, from: F)(implicit m: M, see: TypeTag[T]): Unit = macro mapImpl[F, T, M]

  def internal(c: reflect.macros.Context, msg: String): Nothing = {
    c.abort(c.enclosingPosition, "Internal YAJOM error: " + msg + "\nPlease, contact gark87 <my_another@mail.ru>")
  }

  def error(c: reflect.macros.Context, msg: String): Nothing = {
    c.abort(c.enclosingPosition, "YAJOM error: " + msg)
  }

  def mapImpl[F: c.WeakTypeTag, T: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(setter: c.Expr[(T) => Unit], from: c.Expr[F])(m: c.Expr[M], see: c.Expr[TypeTag[T]])
  : c.Expr[Unit] = {
    import c.universe._

    val fromType = from.actualType
    val toType = setter.actualType match {
      case TypeRef(_, _, q) => q.head
      case _ => internal(c, "Unexpected <to> type")
    }
    val thisRef = m match {
      case Expr(Select(a, _)) => a
      case _ => internal(c, "Unexpected <this> ref")
    }
    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head

    def createVal(name: TermName, value: Expr[_], tpe: Type) = ValDef(Modifiers(), name, TypeTree(tpe), value.tree)

    def callObjectFactory(toType: c.Type, fromType: c.Type, from: c.Expr[_]): c.Expr[Any] = {
      val members: MemberScope = objectFactoryType.members
      val candidates: Iterable[Symbol] = members.filter(createMethodsFilter(toType, fromType))
      val size: Int = candidates.size
      val methodName = "create_" + toType.toString.replaceAll("\\W+", "_")
      if (size == 0) {
        val constructor = fromType.member(nme.CONSTRUCTOR)
        if (constructor.isMethod && constructor.asMethod.isPublic) {
          c.Expr[Any](New(TypeTree(fromType)))
        } else {
          error(c, "Cannot find public constructor for: " + fromType + " \nOr " + methodName + "(" + fromType + ") : " + toType + " method @ " + objectFactoryType)
        }
      } else if (size == 1) {
        c.Expr[Any](Apply(Select(Select(thisRef, newTermName("factory")), newTermName(methodName)), if (from == null) List() else List(from.tree)))
      } else {
        error(c, "More than one methods suitable for object creation: " + fromType + " -> " + toType + ":" + candidates.mkString("\n"))
      }
    }

    def createMethodsFilter(toType: c.Type, fromType: c.Type)(s: c.Symbol): Boolean = {
      if (!s.isMethod)
        false
      else {
        val method: MethodSymbol = s.asMethod
        if (method.name.decoded != "create_" + toType.toString.replaceAll("\\W+", "_"))
          false
        else {
          val returnType: Type = method.returnType
          if (!(returnType <:< toType))
            false
          else {
            method.paramss match {
              case List(List(t)) if t.typeSignature <:< fromType => true
              case List(List(t), List()) if t.typeSignature <:< fromType => true
              case _ => false
            }
          }

        }
      }
    }



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
            val newQ = addNullGuards(qualifier)
            if (!correctName || !correctParams) {
              Select(newQ, name)
            } else if (setter.isEmpty) {
              error(c, "Cannot find setter")
            } else {
              val synthetic = (0l + scala.reflect.internal.Flags.SYNTHETIC).asInstanceOf[c.universe.FlagSet]
              val resultValue = newTermName(c.fresh("resultValue$"))
              prefix = prefix :+ ValDef(Modifiers(synthetic), resultValue, TypeTree(),
                callObjectFactory(ReturnType, ReturnType, null).tree
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
      guards match {
        case Block(a, b) => val d: List[c.universe.Tree] = prefix ::: a
          c.Expr[T => Unit](Block(d, c.resetAllAttrs(b)))
        case _ => error(c, "no need of `nullSafe'")
      }
    }

    if (fromType == toType) {
      val castedFrom: c.Expr[T] = from.asInstanceOf[c.Expr[T]]
      reify {
        withNullGuards(setter).splice(castedFrom.splice)
      }
    } else {
      val fromValueName = newTermName(c.fresh("yajom_fromValue$"))
      val fromValueDef = c.Expr[F](createVal(fromValueName, from, fromType))
      val fromValueIdent = c.Expr[Any](Ident(fromValueName))

      val toValueName = newTermName(c.fresh("yajom_toValue$"))
      val toValueDef = c.Expr[T](createVal(toValueName, callObjectFactory(toType, from.actualType, from), toType))
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

