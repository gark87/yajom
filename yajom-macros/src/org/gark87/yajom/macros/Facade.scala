package org.gark87.yajom.macros

import org.gark87.yajom.base.BaseMapper
import language.experimental.macros
import scala.reflect.macros.Context


object Facade {
  def yajomMap[T, F, M <: BaseMapper[_]](setter: (T) => Unit)(from: F)(implicit m: M): Unit = macro yajomMapImpl[T, F, M]

  def yajom[T, M <: BaseMapper[_]](setter: (T) => Unit)(from: T)(implicit m: M): Unit = macro yajomImpl[T, M]

  def yajomOption[T, M <: BaseMapper[_]](setter: (T) => Unit)(from: Option[T])(implicit m: M): Unit = macro optionImpl[T, M]

  def createOnNull[F, M <: BaseMapper[_]](func: F)(implicit m: M): F = macro CreateOnNull.macroImpl[F, M]

  def yajomImpl[T: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(setter: c.Expr[(T) => Unit])(from: c.Expr[T])(m: c.Expr[M])
  : c.Expr[Unit] = {
    import c.universe._

    val y = new YajomContext(c)
    val objectFactoryType = m.actualType.asInstanceOf[TypeRef].args.head.asInstanceOf[y.c.Type]
    val guards = y.c.Expr[(T) => Unit](y.createOnNull.process(y)(setter.asInstanceOf[y.c.Expr[(T) => Unit]], objectFactoryType))
    c.Expr[Unit](reify {
      import scala.reflect.ClassTag
      guards.splice(from.splice)
    }.tree)
  }

  def yajomMapImpl[T: c.WeakTypeTag, F: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(setter: c.Expr[(T) => Unit])(from: c.Expr[F])(m: c.Expr[M])
  : c.Expr[Unit] = {
    import c.universe._

    val thisRef = m match {
      case Expr(Select(a, _)) => a
      case _ => c.abort(c.enclosingPosition, "OOPS")
    }
    val fromType = from.actualType
    val toType = setter.actualType match {
      case TypeRef(_, _, q) => q.head
      case _ => c.abort(c.enclosingPosition, "OOPS")
    }
    val y = new YajomContext(c)
    val objectFactoryType = m.actualType.asInstanceOf[TypeRef].args.head.asInstanceOf[y.c.Type]

    val freshName = newTermName(c.fresh("fromValue$"))
    val nonTrivialName = newTermName(c.fresh("nonTrivial$"))
    val freshIdent = c.Expr[Any](Ident(freshName))
    val mapCall = c.Expr[Any](Apply(Select(thisRef, newTermName("map")), List(Ident(freshName), Ident(nonTrivialName))))
    def createVal(name: TermName, value: Expr[_], tpe: Type) = ValDef(Modifiers(), name, TypeTree(tpe), value.tree)
    val fromValueCalc = createVal(freshName, reify {
      from.splice
    }, fromType)
    val defaultObject = y.creator.createDefaultObject(y)(toType.asInstanceOf[y.c.Type], objectFactoryType)
    val nonTrivialCall = createVal(nonTrivialName, c.Expr[Any](defaultObject.asInstanceOf[c.Tree]), toType)
    val nonTrivialIdent = c.Expr[T](Ident(nonTrivialName))

    val mapCall2 = reify {
      import scala.reflect.ClassTag
      if (nonTrivialIdent.splice != null) {
        mapCall.splice
        setter.splice(nonTrivialIdent.splice)
      }
    }.tree
    c.Expr[Unit](
      Block(fromValueCalc, nonTrivialCall, mapCall2)
    )
  }

  def optionImpl[T: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(setter: c.Expr[(T) => Unit])(from: c.Expr[Option[T]])(m: c.Expr[M])
  : c.Expr[Unit] = {
    import c.universe._

    val y = new YajomContext(c)

    val objectFactoryType: y.c.Type = m.actualType.asInstanceOf[TypeRef].args.head.asInstanceOf[y.c.Type]
    val guards = y.c.Expr[(T) => Unit](y.createOnNull.process(y)(setter.asInstanceOf[y.c.Expr[(T) => Unit]], objectFactoryType))
    c.Expr[Unit](reify {
      import scala.reflect.ClassTag
      val option = from.splice
      option match {
        case Some(x) => guards.splice(x)
        case None =>  null
      }
    }.tree)

  }
}
