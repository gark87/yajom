package org.gark87.yajom.settings

import scala.reflect.macros.Context

abstract class YajomSettings {
  def expectSetter(c : Context)(name : String, returnType : c.Type) : Option[String]
}
