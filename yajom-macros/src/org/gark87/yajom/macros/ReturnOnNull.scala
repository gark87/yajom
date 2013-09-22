package org.gark87.yajom.macros

class ReturnOnNull(reporter: ErrorReporter) {
  def process[T: c.WeakTypeTag](c: reflect.macros.Context)(expr: c.Expr[T], returnValue: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    var prefix: List[ValDef] = List()

    def addNullGuards(tree: Tree): Tree = {
      tree match {
        case Apply(TypeApply(Select(qualifier, name), typeArgs), args) => {
          Apply(TypeApply(Select(addNullGuards(qualifier), name), typeArgs), args.map((t: Tree) => addNullGuards(t)))
        }
        case Apply(Select(qualifier, name), args) => {
          val saveQ = addNullGuards(qualifier)
          val safeArgs = args.map((t: Tree) => addNullGuards(t))
          val resultValue = newTermName(c.fresh("resultValue$"))
          prefix = prefix :+ ValDef(Modifiers(), resultValue, TypeTree(), Apply(Select(saveQ, name), safeArgs))
          Ident(resultValue)
        }
        case Apply(fun, args) =>
          Apply(addNullGuards(fun), args)
        case Block(stats, epr) =>
          Block(stats.map((s: Tree) => {
            addNullGuards(s)
          }), epr)
        case ValDef(mods, name, tpt, rhs) =>
          ValDef(mods, name, tpt, addNullGuards(rhs))
        case Ident(name) => tree
        case This(a) => tree
        case Function(valdefs, body) => Function(valdefs, addNullGuards(body))
        case Literal(co) => tree
        case a => reporter.error("Too complex expression `" + a + "` for YAJOM ReturnOnNull:\n1. Quick Fix: extract val without YAJOM\n2. mail gark87 <my_another@mail.ru>")
      }
    }
    def foldPrefixes(tree: Tree): Tree = {
      c.resetAllAttrs(prefix.foldRight(tree)((valDef: ValDef, result: Tree) => {
        val reference = c.Expr[Any](Ident(valDef.name))
        val defineVal = c.Expr[Any](valDef)
        val resultExpr = c.Expr[Any](result)
        reify {
          defineVal.splice
          if (reference.splice == null) {
            returnValue.splice
          } else {
            resultExpr.splice
          }
        }.tree
      }))
    }
    val guards: Tree = addNullGuards(expr.tree)
    guards match {
      case Function(valdefs, body) => {
        c.Expr[T](Function(valdefs, foldPrefixes(body)))
      }
      case any =>
        c.Expr[T](foldPrefixes(any))
    }
  }
}
