package org.gark87.yajom.base


class YajomCollection[T <: java.lang.Object](val source: java.util.Collection[T]) {

  def find(@ReturnOnNull("predicate") predicate: T => Boolean, @PredicateToFactory("predicate") factory: () => T = YajomCollection.defaultFactory): T = {
    val it = source.iterator()
    while (it.hasNext) {
      val next = it.next()
      if (predicate(next))
        return next
    }
    factory()
  }
}

object YajomCollection {
  val defaultFactory: () => Nothing = throw new IllegalStateException("Should be replaced by YAJOM")
}
