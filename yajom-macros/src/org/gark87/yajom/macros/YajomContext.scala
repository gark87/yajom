package org.gark87.yajom.macros

import scala.reflect.macros.Context

class YajomContext(val c: Context) {
  val reporter = new ErrorReporter(c)

  val creator = new ObjectCreator

  val argsConverter = new ArgsConverter

  val createOnNull = new CreateOnNull

  val returnOnNull = new ReturnOnNull

  val predicateToFactory = new PredicateToFactory
}
