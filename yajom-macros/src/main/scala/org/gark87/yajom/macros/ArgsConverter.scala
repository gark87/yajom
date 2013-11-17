package org.gark87.yajom.macros

import scala.collection.mutable
import scala.reflect.macros.{Context, Universe}

class ArgsConverter() {
  def convert(y: YajomContext)(method: y.c.universe.MethodSymbol,
                                         args: List[List[y.c.Tree]],
                                         vars: mutable.HashMap[String, y.c.Tree],
                                         objectFactoryType: y.c.Type): scala.List[scala.List[y.c.Tree]] = {
    import y.c.universe._

    def wrapParameter(annotation: Annotation, option: Option[Tree]): Tree = {
      option match {
        case Some(x) => {
          val name = annotation.tpe.typeSymbol.asClass.fullName
          if (name.endsWith("CreateOnNull")) {
            y.createOnNull.process(y)(y.c.Expr(x), objectFactoryType)
          } else if (name.endsWith("ReturnOnNull")) {
            y.returnOnNull.process(y)(y.c.Expr(x), y.c.Expr(Literal(Constant(false))))
          } else if (name.endsWith("PredicateToFactory")) {
            y.predicateToFactory.process(y)(y.c.Expr(x), objectFactoryType)
          } else {
            x
          }
        }
        case None => y.reporter.error("Cannot find parameter value with annotation" + annotation + " of " + method)
      }
    }

    val paramss = method.paramss
    val map = new mutable.HashMap[String ,Tree]()
    val zip: List[List[(Symbol, Tree)]] = paramss.zip(args).map((a) => a._1.zip(a._2))
    for (list <- zip) {
      for(elem <- list) {
        map.put(elem._1.name.decoded, elem._2)
      }
    }
    zip.map((list: List[(Symbol, Tree)]) => {
      list.map((pair: (Symbol, Tree)) => {
        val expr = pair._2
        pair._1.annotations.filter(_.tpe.typeSymbol.asClass.fullName.startsWith("org.gark87.yajom.annotations.")) match {
          case List() => y.createOnNull.process(y)(y.c.Expr(expr), objectFactoryType)
          case List(annotation) => {
            val annotationArgs = annotation.javaArgs
            if (annotationArgs.size != 1)
              y.reporter.error("Unexpected number or annotation args: " + annotation + " @ " + pair._1.name + " of " + method)
            else {
              val javaArgument: JavaArgument = annotationArgs.head._2
              val paramName: String = javaArgument match {
                case LiteralArgument(Constant(a)) => String.valueOf(a)
                case _ => y.reporter.error("Unexpected annotation value: " + annotation + " @ " + pair._1.name + " of " + method)
              }
              map.get(paramName) match {
                case Some(Ident(name)) => wrapParameter(annotation, vars.get(name.decoded))
                case Some(e) => wrapParameter(annotation, Some(e))
                case a => y.reporter.error("Unexpected parameter value: " + a + " @ " + paramName + " of " + method)
              }
            }
          }
          case _ => y.reporter.error("More than one annotation on parameter " + pair._1.name + " of " + method)
        }
      })
    }).asInstanceOf[scala.List[scala.List[y.c.Tree]]]
  }
}
