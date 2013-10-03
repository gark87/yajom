package org.gark87.yajom.settings

import scala.reflect.macros.Context

class DefaultSettings extends YajomSettings {
  def expectSetter(c: Context)(name: String, returnType: c.Type): Option[String] = {
    if (!name.startsWith("get") && !name.startsWith("is"))
      None
    else
      Some(name.replaceFirst("^(is|get)", "set"))
  }
}
