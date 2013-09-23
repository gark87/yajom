package org.gark87.yajom.macros

import org.gark87.yajom.base.BaseMapper
import language.experimental.macros


object Facade {
  def yajom[T, M <: BaseMapper[_]](setter: (T) => Unit)(from: T)(implicit m: M): Unit = macro yajomImpl[T, M]

  def yajomOption[T, M <: BaseMapper[_]](setter: (T) => Unit)(from: Option[T])(implicit m: M): Unit = macro optionImpl[T, M]

  def createOnNull[F, M <: BaseMapper[_]](func: F)(implicit m: M): F = macro CreateOnNull.macroImpl[F, M]

  def yajomImpl[T: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(setter: c.Expr[(T) => Unit])(from: c.Expr[T])(m: c.Expr[M])
  : c.Expr[Unit] = {
    import c.universe._

    val reporter = new ErrorReporter(c)

    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head
    val creator: ObjectCreator = new ObjectCreator(reporter)
    val guards = new CreateOnNull(creator).process(c)(setter, objectFactoryType)
    c.Expr[Unit](reify {
      import scala.reflect.ClassTag
      guards.splice(from.splice)
    }.tree)

  }

  def optionImpl[T: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(setter: c.Expr[(T) => Unit])(from: c.Expr[Option[T]])(m: c.Expr[M])
  : c.Expr[Unit] = {
    import c.universe._

    val reporter = new ErrorReporter(c)

    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head
    val creator: ObjectCreator = new ObjectCreator(reporter)
    val guards = new CreateOnNull(creator).process(c)(setter, objectFactoryType)
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
