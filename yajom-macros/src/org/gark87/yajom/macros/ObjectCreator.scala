package org.gark87.yajom.macros

class ObjectCreator(val reporter : ErrorReporter) {
  def createMethodsFilter(c: reflect.macros.Context)(toType: c.Type, fromType: c.Type)(s: c.Symbol): Boolean = {
    import c.universe._
    if (!s.isMethod)
      false
    else {
      val method: MethodSymbol = s.asMethod
      if (!method.name.decoded.startsWith("create"))
        false
      else {
        val returnType: Type = method.returnType
        if (!(returnType <:< toType.erasure))
          false
        else {
          method.paramss match {
            case List(List(t)) if fromType <:< t.typeSignature => true
            case List(List(t), List()) if fromType <:< t.typeSignature => true
            case _ => false
          }
        }
      }
    }
  }

  def callObjectFactor(c: reflect.macros.Context)(toType: c.Type, fromType: c.Type, from: c.Expr[_], factoryType: c.Type): c.Expr[Any] = {
    import c.universe._
    val members: MemberScope = factoryType.members
    val thisRef = This(c.enclosingClass.symbol)
    val candidates: Iterable[Symbol] = members.filter(createMethodsFilter(c)(toType, fromType))
    val size: Int = candidates.size
    if (size == 0) {
      val constructor = toType.member(nme.CONSTRUCTOR)
      if (constructor.isMethod && constructor.asMethod.isPublic) {
        c.Expr[Any](Apply(Select(New(TypeTree(toType)), nme.CONSTRUCTOR), List()))
      } else {
        reporter.error("Cannot find public constructor for: " + toType + " \nOr create...(" + fromType + ") : " + toType + " method @ " + factoryType)
      }
    } else if (size == 1) {
      val name = candidates.head.name.decoded
      c.Expr[Any](Apply(Select(Select(thisRef, newTermName("factory")), newTermName(name)), if (from == null) List() else List(from.tree)))
    } else {
      reporter.error("More than one methods suitable for object creation: " + fromType + " -> " + toType + ":" + candidates.mkString("\n"))
    }
  }

}
