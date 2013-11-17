package org.gark87.yajom.macros

import scala.reflect.macros.Context
import org.gark87.yajom.settings.YajomSettings
import scala.tools.nsc.Global

class YajomContext(val c: Context) {
  val reporter = new ErrorReporter(c)

  val creator = new ObjectCreator

  val argsConverter = new ArgsConverter

  val createOnNull = new CreateOnNull

  val returnOnNull = new ReturnOnNull

  val predicateToFactory = new PredicateToFactory

  val settings : YajomSettings = {
    val define = c.universe.asInstanceOf[Global].settings.defines.value.find(_.startsWith("-Dyajom=")).getOrElse("-Dyajom=org.gark87.yajom.settings.DefaultSettings")
    val className = define.substring("-Dyajom=".length)
    val clazz = Class.forName(className)
    clazz.getConstructor().newInstance().asInstanceOf[YajomSettings]
  }
}
