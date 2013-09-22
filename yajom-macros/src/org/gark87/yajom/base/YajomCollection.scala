package org.gark87.yajom.base

import org.gark87.yajom.annotations.{ReturnOnNull, PredicateToFactory}

class YajomCollection[T <: java.lang.Object](val source: java.util.Collection[T]) {

  def find(@ReturnOnNull("predicate") predicate: T => Boolean, @PredicateToFactory("predicate") create: () => T = YajomCollection.defaultFactory): T = {
    val it = source.iterator()
    while (it.hasNext) {
      val next = it.next()
      if (predicate(next))
        return next
    }
    val elem: T = create()
    source.add(elem)
    elem
  }
}

object YajomCollection {
  val defaultFactory: () => Nothing = null
}
