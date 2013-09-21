package org.gark87.yajom.macros

import scala.reflect.macros.Universe

class PredicateToFactory(reporter:ErrorReporter) {
  def process[T: c.WeakTypeTag](c: reflect.macros.Context)(expr: c.Expr[T => Boolean]): c.Expr[T => Boolean] = {
    import c.universe._

    def convertEquals(tree : Tree) : Tree = {
      tree match {
        case Apply(Select(qualifier, name), args) => {
          val decoded = name.decoded
          if (decoded == "$amp$amp") {
            tree
          } else if (decoded == "equals" || decoded == "$eq$eq") {
            tree
          } else {
            tree
          }
        }
        case _ => {
          tree
        }
      }
    }
    def convertAnd(tree : Tree, acc : Tree) : Tree = {
      tree match {
        case Apply(Select(qualifier, name), args) => {
          val decoded = name.decoded
          if (decoded == "$amp$amp") {
            tree
          } else if (decoded == "equals" || decoded == "$eq$eq") {
            tree
          } else {
            tree
          }
        }
        case _ => {
          tree
        }
      }
    }
    val tree: Tree = expr.tree
    tree match {
      case Function(List(ValDef(mods, name, tpt, rhs)), body) => {
        val z = tpt;
        convertAnd(body, Function(List(), ))
        expr
      }
      case _ => reporter.error("Unexpected (waiting for function def with one param): " + tree)
    }
  }
}
