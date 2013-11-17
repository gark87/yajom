package org.gark87.yajom.macros

import org.gark87.yajom.base.BaseMapper
import language.experimental.macros
import scala.reflect.runtime.universe.TypeTag

/**
 * This class is all about creating new instances by calling `create...()` on ObjectFactory
 * or calling default constructor if no appropriate methods were found
 */
class ObjectCreator {

  /**
   * This method tests if s is appropriate `create...` method.
   */
  def createMethodsFilter(y: YajomContext)(toType: y.c.Type, predicate: (y.c.universe.MethodSymbol) => Boolean)
                         (s: y.c.Symbol): Boolean = {
    import y.c.universe._
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

  def createDefaultObject[T](y: YajomContext)(toType: y.c.Type, factoryType: y.c.Type): y.c.Tree = {
    import y.c.universe._

    def testMethod(method: MethodSymbol): Boolean = {
      method.paramss match {
        case List(List()) => true
        case _ => false
      }
    }

    val members: MemberScope = factoryType.members
    val thisRef = This(y.c.enclosingClass.symbol)
    val candidates: Iterable[Symbol] = members.filter(createMethodsFilter(y)(toType, testMethod))
    val size: Int = candidates.size
    if (size == 0) {
      val constructor = toType.member(nme.CONSTRUCTOR)
      if (constructor.isMethod && constructor.asMethod.isPublic) {
        Apply(Select(New(TypeTree(toType)), nme.CONSTRUCTOR), List())
      } else {
        y.reporter.error("Cannot find public constructor for: " + toType + " \nOr create...() : " + toType + " method @ " + factoryType)
      }
    } else if (size == 1) {
      val method: Symbol = candidates.head
      val name = method.name.decoded
      val typeArgs = toType match {
        case TypeRef(pre, sum, args) => args
        case _ => List()
      }

      if (typeArgs.isEmpty) {
        Apply(
            Select(Select(thisRef, newTermName("factory")), newTermName(name)),
          List())
      } else {
        val typeTrees = typeArgs.map((t: Type) => Ident(t.typeSymbol))
        Apply(
            TypeApply(
              Select(Select(thisRef, newTermName("factory")), newTermName(name)),
            typeTrees),
          List())
      }
    } else {
      y.reporter.error("More than one methods suitable for object creation: " + toType + ":" + candidates.mkString("\n"))
    }
  }

  def createObjectFrom(y: YajomContext)(toType: y.c.Type, fromType: y.c.Type, from: y.c.Expr[_], factoryType: y.c.Type): y.c.Tree = {
    import y.c.universe._

    def testMethod(fromType: Type)(method: MethodSymbol): Boolean = {
      method.paramss match {
        case List(List(t)) if fromType <:< t.typeSignature => true
        case List(List(t), List()) if fromType <:< t.typeSignature => true
        case _ => false
      }
    }

    val members: MemberScope = factoryType.members
    val thisRef = This(y.c.enclosingClass.symbol)
    val candidates: Iterable[Symbol] = members.filter(createMethodsFilter(y)(toType, testMethod(fromType)))
    val size: Int = candidates.size
    if (size == 0) {
      val constructor = toType.member(nme.CONSTRUCTOR)
      if (constructor.isMethod && constructor.asMethod.isPublic) {
        Apply(Select(New(TypeTree(toType)), nme.CONSTRUCTOR), List())
      } else {
        y.reporter.error("Cannot find public constructor for: " + toType + " \nOr create...(" + fromType + ") : " + toType + " method @ " + factoryType)
      }
    } else if (size == 1) {
      val name = candidates.head.name.decoded
      Apply(Select(Select(thisRef, newTermName("factory")), newTermName(name)), List(from.tree))
    } else {
      y.reporter.error("More than one methods suitable for object creation: " + fromType + " -> " + toType + ":" + candidates.mkString("\n"))
    }
  }
}


object ObjectCreator {
  def createDefault[T, M <: BaseMapper[_]](implicit m: M, t : TypeTag[T]) = macro macroImpl[T, M]

  def macroImpl[T: c.WeakTypeTag, M <: BaseMapper[_]](c : reflect.macros.Context)(m : c.Expr[M], t : c.Expr[TypeTag[T]]): c.Expr[T] = {
    import c.universe._
    val y: YajomContext = new YajomContext(c)
    val objectFactoryType: y.c.Type = m.actualType.asInstanceOf[TypeRef].args.head.asInstanceOf[y.c.Type]
    val tree = new ObjectCreator().createDefaultObject[T](y)(y.c.weakTypeOf[T], objectFactoryType)
    c.Expr[T](tree.asInstanceOf[c.Tree])
  }
}