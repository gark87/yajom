package org.gark87.yajom.macros

class ErrorReporter(val c: reflect.macros.Context) {
  def internal(msg: String): Nothing = {
    c.abort(c.enclosingPosition, "Internal YAJOM error: " + msg + "\nPlease, contact gark87 <my_another@mail.ru>")
  }

  def error(msg: String): Nothing = {
    c.abort(c.enclosingPosition, "YAJOM error: " + msg)
  }
}
