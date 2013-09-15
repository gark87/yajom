package org.gark87.yajom.macros

import org.gark87.yajom.base.BaseMapper
import language.experimental.macros
import scala.reflect.runtime.universe.TypeTag

/**
 * This class is all about creating new instances by calling `create...()` on ObjectFactory
 * or calling default constructor if no appropriate methods were found
 * @param reporter to report errors
 */
class ObjectCreator(val reporter: ErrorReporter) {

  /**
   * This method tests if s is appropriate `create...` method.
   */
  def createMethodsFilter(c: reflect.macros.Context)(toType: c.Type, predicate: (c.universe.MethodSymbol) => Boolean)
                         (s: c.Symbol): Boolean = {
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
          predicate(method)
        }
      }
    }
  }

  def createDefaultObject[T](c: reflect.macros.Context)(toType: c.Type, factoryType: c.Type): c.Expr[T] = {
    import c.universe._

    def testMethod(method: MethodSymbol): Boolean = {
      method.paramss match {
        case List(List()) => true
        case _ => false
      }
    }

    val members: MemberScope = factoryType.members
    val thisRef = This(c.enclosingClass.symbol)
    val candidates: Iterable[Symbol] = members.filter(createMethodsFilter(c)(toType, testMethod))
    val size: Int = candidates.size
    if (size == 0) {
      val constructor = toType.member(nme.CONSTRUCTOR)
      if (constructor.isMethod && constructor.asMethod.isPublic) {
        c.Expr[T](Apply(Select(New(TypeTree(toType)), nme.CONSTRUCTOR), List()))
      } else {
        reporter.error("Cannot find public constructor for: " + toType + " \nOr create...() : " + toType + " method @ " + factoryType)
      }
    } else if (size == 1) {
      val method: Symbol = candidates.head
      val name = method.name.decoded
      val typeArgs = toType match {
        case TypeRef(pre, sum, args) => args
        case _ => List()
      }

      if (typeArgs.isEmpty) {
        c.Expr[T](Apply(
            Select(Select(thisRef, newTermName("factory")), newTermName(name)),
          List()))
      } else {
        val typeTrees = typeArgs.map((t: Type) => Ident(t.typeSymbol))
        c.Expr[T](Apply(
            TypeApply(
              Select(Select(thisRef, newTermName("factory")), newTermName(name)),
            typeTrees),
          List()))
      }
    } else {
      reporter.error("More than one methods suitable for object creation: " + toType + ":" + candidates.mkString("\n"))
    }
  }

  def createObjectFrom(c: reflect.macros.Context)(toType: c.Type, fromType: c.Type, from: c.Expr[_], factoryType: c.Type): c.Expr[Any] = {
    import c.universe._

    def testMethod(fromType: Type)(method: MethodSymbol): Boolean = {
      method.paramss match {
        case List(List(t)) if fromType <:< t.typeSignature => true
        case List(List(t), List()) if fromType <:< t.typeSignature => true
        case _ => false
      }
    }

    val members: MemberScope = factoryType.members
    val thisRef = This(c.enclosingClass.symbol)
    val candidates: Iterable[Symbol] = members.filter(createMethodsFilter(c)(toType, testMethod(fromType)))
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
      c.Expr[Any](Apply(Select(Select(thisRef, newTermName("factory")), newTermName(name)), List(from.tree)))
    } else {
      reporter.error("More than one methods suitable for object creation: " + fromType + " -> " + toType + ":" + candidates.mkString("\n"))
    }
  }
}


object ObjectCreator {
  def createDefault[T, M <: BaseMapper[_]](implicit m: M, t : TypeTag[T]) = macro macroImpl[T, M]

  def macroImpl[T: c.WeakTypeTag, M <: BaseMapper[_]](c : reflect.macros.Context)(m : c.Expr[M], t : c.Expr[TypeTag[T]]): c.Expr[T] = {
    import c.universe._
    val objectFactoryType: c.Type = m.actualType.asInstanceOf[TypeRef].args.head
    val toType: Type = c.weakTypeOf[T]
    new ObjectCreator(new ErrorReporter(c)).createDefaultObject[T](c)(toType, objectFactoryType)
  }
}