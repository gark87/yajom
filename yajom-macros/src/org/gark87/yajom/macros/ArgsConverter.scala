package org.gark87.yajom.macros

import scala.collection.mutable
import scala.reflect.macros.{Context, Universe}

class ArgsConverter(reporter: ErrorReporter) {
  def convert(c: reflect.macros.Context)(method: c.universe.MethodSymbol,
                                         args: List[List[c.Tree]],
                                         vars: mutable.HashMap[String, c.Tree],
                                         objectFactoryType: c.Type): scala.List[scala.List[c.Tree]] = {
    import c.universe._
    val createOnNull: CreateOnNull = new CreateOnNull(new ObjectCreator(reporter))

    def wrapParameter(annotation: Annotation, option: Option[Tree]): Tree = {
      option match {
        case Some(x) => {
          val name = annotation.tpe.typeSymbol.asClass.fullName
          if (name.endsWith("CreateOnNull")) {
            createOnNull.process(c)(c.Expr(x), objectFactoryType).tree
          } else if (name.endsWith("ReturnOnNull")) {
            new ReturnOnNull(reporter).process(c)(c.Expr(x), c.Expr(Literal(Constant(false)))).tree
          } else if (name.endsWith("PredicateToFactory")) {
            new PredicateToFactory(reporter).process(c)(c.Expr(x), objectFactoryType).tree
          } else {
            x
          }
        }
        case None => reporter.error("Cannot find parameter value with annotation" + annotation + " of " + method)
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
          case List() => createOnNull.process(c)(c.Expr(expr), objectFactoryType).tree
          case List(annotation) => {
            val annotationArgs = annotation.javaArgs
            if (annotationArgs.size != 1)
              reporter.error("Unexpected number or annotation args: " + annotation + " @ " + pair._1.name + " of " + method)
            else {
              val javaArgument: JavaArgument = annotationArgs.head._2
              val paramName: String = javaArgument match {
                case LiteralArgument(Constant(a)) => String.valueOf(a)
                case _ => reporter.error("Unexpected annotation value: " + annotation + " @ " + pair._1.name + " of " + method)
              }
              map.get(paramName) match {
                case Some(Ident(name)) => wrapParameter(annotation, vars.get(name.decoded))
                case Some(e) => wrapParameter(annotation, Some(e))
                case a => reporter.error("Unexpected parameter value: " + a + " @ " + paramName + " of " + method)
              }
            }
          }
          case _ => reporter.error("More than one annotation on parameter " + pair._1.name + " of " + method)
        }
      })
    })
  }
}
