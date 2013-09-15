package org.gark87.yajom.macros

class ReturnOnNull(reporter : ErrorReporter) {
  def process[T: c.WeakTypeTag](c: reflect.macros.Context)(expr: c.Expr[T], returnValue: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    var prefix: List[c.universe.Tree] = List()

    def addNullGuards(tree: Tree): Tree = {
      tree match {
        case Apply(TypeApply(Select(qualifier, name), typeArgs), args) => {
          Apply(TypeApply(Select(addNullGuards(qualifier), name), typeArgs), args.map((t: Tree) => addNullGuards(t)))
        }
        case Apply(Select(qualifier, name), args) => {
          val e = c.Expr[Any](tree)
          val resultValue = newTermName(c.fresh("resultValue$"))
          prefix = prefix :+ ValDef(Modifiers(), resultValue, TypeTree(),
            reify {
              val tmp = e.splice
              if (tmp == null)
                returnValue
              else
                tmp
            }.tree
          )
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
        case Ident(name) => Ident(name)
        case This(a) => This(a)
        case a => reporter.error("Too complex expression `" + a + "` for YAJOM:\n1. Quick Fix: extract val without YAJOM\n2. mail gark87 <my_another@mail.ru>")
      }
    }
    val guards: c.universe.Tree = addNullGuards(expr.tree)
    guards match {
      case Block(a, b) => val d: List[c.universe.Tree] = prefix ::: a
        c.Expr[T](Block(d, c.resetAllAttrs(b)))
      case Ident(name) =>
        c.Expr[T](Block(prefix, c.resetAllAttrs(Ident(name))))
      case _ => reporter.error("no need of `nullSafe'")
    }
  }
}
