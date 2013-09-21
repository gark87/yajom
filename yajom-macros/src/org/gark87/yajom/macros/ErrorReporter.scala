package org.gark87.yajom.macros

import scala.reflect.macros.Context

/**
 * This is all about error reporting from YAJOM macros
 */
class ErrorReporter(val c: Context) {
  def internal(msg: String): Nothing = {
    c.abort(c.enclosingPosition, "Internal YAJOM error: " + msg + "\nPlease, contact gark87 <my_another@mail.ru>")
  }

  def error(msg: String): Nothing = {
    c.abort(c.enclosingPosition, "YAJOM error: " + msg)
  }
}
