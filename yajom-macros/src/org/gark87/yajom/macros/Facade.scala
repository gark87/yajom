package org.gark87.yajom.macros

import org.gark87.yajom.base.BaseMapper
import language.experimental.macros


object Facade {
  def yajom[T, M <: BaseMapper[_]](setter: (T) => Unit)(from: T)(implicit m: M): Unit = macro mapImpl[T, M]
  def createOnNull[F, M <: BaseMapper[_]](func: F)(implicit m: M): F = macro CreateOnNull.macroImpl[F, M]


  def mapImpl[T: c.WeakTypeTag, M <: BaseMapper[_]](c: reflect.macros.Context)(setter: c.Expr[(T) => Unit])(from: c.Expr[T])(m: c.Expr[M])
  : c.Expr[Unit] = {
    import c.universe._

    val reporter = new ErrorReporter(c)

//    val fromType = from.actualType
//    val toType = setter.actualType match {
//      case TypeRef(_, _, q) => q.head
//      case _ => reporter.internal("Unexpected <to> type")
//    }
//    val thisRef = This(c.enclosingClass.symbol)
    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head
    val creator: ObjectCreator = new ObjectCreator(reporter)


//    def createVal(name: TermName, value: Expr[_], tpe: Type) = ValDef(Modifiers(), name, TypeTree(tpe), value.tree)

//    if (fromType == toType) {
      val castedFrom: c.Expr[T] = from.asInstanceOf[c.Expr[T]]
      val guards = new CreateOnNull(creator).process(c)(setter, objectFactoryType)
      c.Expr[Unit](reify {
        import scala.reflect.ClassTag
        guards.splice(castedFrom.splice)
      }.tree)
//    } else {
//      val fromValueName = newTermName(c.fresh("yajom_fromValue$"))
//      val fromValueDef = c.Expr[F](createVal(fromValueName, from, fromType))
//      val fromValueIdent = c.Expr[Any](Ident(fromValueName))
//
//      val toValueName = newTermName(c.fresh("yajom_toValue$"))
//      val toValueDef = c.Expr[T](createVal(toValueName, creator.createObjectFrom(c)(toType, from.actualType, from, objectFactoryType), toType))
//      val toValueIdent = c.Expr[T](Ident(toValueName))
//
//      val mapCall = c.Expr[Any](Apply(Select(thisRef, newTermName("map")), List(Ident(fromValueName), Ident(toValueName))))
//
//      c.Expr[Unit](reify {
//        fromValueDef.splice
//        if (fromValueIdent.splice != null) {
//          toValueDef.splice
//          if (toValueIdent.splice != null)
//            mapCall.splice
//        }
//      }.tree)
//    }
  }
}
