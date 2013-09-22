package org.gark87.yajom.macros

import scala.reflect.macros.Universe

class PredicateToFactory(reporter: ErrorReporter) {
  def process[T: c.WeakTypeTag](c: reflect.macros.Context)(expr: c.Expr[T => Boolean], objectFactoryType: c.Type): c.Expr[T => Boolean] = {
    import c.universe._

    val creator = new ObjectCreator(reporter)
    val onNull = new CreateOnNull(creator)

    def convertEquals(tree: Tree, last: Tree): Tree = {
      tree match {
        case Apply(Select(qualifier, name), List(arg)) => {
          val decoded = name.decoded
          if (decoded == "equals" || decoded == "==") {
            qualifier match {
              case Apply(Select(gQualifier, gName), List()) => {
                onNull.findSetter(c)(gQualifier, gName, List(), (s) => {
                  reporter.error("PredicateToFactory works with chained getters only: " + tree)
                }, (setterName, returnType) => {
                  Block(Apply(Select(onNull.process(c)(c.Expr[Any](gQualifier), objectFactoryType).tree, newTermName(setterName)), List(arg)), last)
                })
              }
              case _ => reporter.error("PredicateToFactory last call before equals should be getter, not: " + qualifier)
            }
          } else {
            reporter.error("PredicateToFactory works with && of == or equals: " + tree)
          }
        }
        case _ => {
          reporter.error("PredicateToFactory works with && of == or equals: " + tree)
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
        case _ => reporter.error("PredicateToFactory works with one-parameter function only(&& and ==): " + tree)
      }
    }
    val tree: Tree = expr.tree
    tree match {
      case Function(List(ValDef(mods, name, tpt, rhs)), body) => {
        val valDef = ValDef(Modifiers(), name, tpt, creator.createDefaultObject[T](c)(tpt.tpe, objectFactoryType).tree)
        val sss= convertAnd(body, Ident(name))
        c.Expr[T => Boolean](Function(List(), Block(valDef, sss)))
      }
      case _ => reporter.error("Unexpected (waiting for function def with one param): " + tree)
    }
  }
}
