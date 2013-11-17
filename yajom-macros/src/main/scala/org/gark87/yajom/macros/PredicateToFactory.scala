package org.gark87.yajom.macros

import scala.reflect.macros.Universe

class PredicateToFactory() {
  def process[T: y.c.WeakTypeTag](y : YajomContext)(expr: y.c.Expr[T => Boolean], objectFactoryType: y.c.Type): y.c.Tree = {
    import y.c.universe._

    val creator = y.creator
    val onNull = y.createOnNull

    def convertEquals(tree: Tree, last: Tree): Tree = {
      tree match {
        case Apply(Select(qualifier, name), List(arg)) => {
          val decoded = name.decoded
          if (decoded == "equals" || decoded == "==") {
            qualifier match {
              case Apply(Select(gQualifier, gName), List()) => {
                onNull.findSetter(y)(gQualifier, gName, List(), (s) => {
                  y.reporter.error("PredicateToFactory works with chained getters only: " + tree)
                }, (setterName, returnType) => {
                  Block(Apply(Select(onNull.process(y)(y.c.Expr[Any](gQualifier), objectFactoryType), newTermName(setterName)), List(arg)), last)
                })
              }
              case _ => y.reporter.error("PredicateToFactory last call before equals should be getter, not: " + qualifier)
            }
          } else {
            y.reporter.error("PredicateToFactory works with && of == or equals: " + tree)
          }
        }
        case _ => {
          y.reporter.error("PredicateToFactory works with && of == or equals: " + tree)
        }
      }
    }
    def convertAnd(tree: Tree, last: Tree): Tree = {
      tree match {
        case Apply(Select(qualifier, name), List(arg)) => {
          val decoded = name.decoded
          if (decoded == "&&") {
            convertAnd(qualifier, convertAnd(arg, last))
          } else {
            convertEquals(tree, last)
          }
        }
        case _ => y.reporter.error("PredicateToFactory works with one-parameter function only(&& and ==): " + tree)
      }
    }
    val tree: Tree = expr.tree
    tree match {
      case Function(List(ValDef(mods, name, tpt, rhs)), body) => {
        val valDef = ValDef(Modifiers(), name, tpt, creator.createDefaultObject[T](y)(tpt.tpe, objectFactoryType).asInstanceOf[y.c.Tree])
        val sss= convertAnd(body, Ident(name))
        Function(List(), Block(valDef, sss))
      }
      case _ => y.reporter.error("Unexpected (waiting for function def with one param): " + tree)
    }
  }
}
