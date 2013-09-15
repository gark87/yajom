package org.gark87.yajom.macros

class ArgsConverter(reporter: ErrorReporter) {
  def convert(c: reflect.macros.Context)(method : c.universe.MethodSymbol, args : scala.List[c.Tree]) : scala.List[c.Tree] = {
    args
  }
}
