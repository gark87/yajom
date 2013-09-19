package org.gark87.yajom.base


class YajomCollection[T <: java.lang.Object](val source: java.util.Collection[T]) {

  def find(@ReturnOnNull("predicate") predicate : T => Boolean) : T = {
    val it = source.iterator()
    while(it.hasNext) {
      val next = it.next()
      if (predicate(next))
        return next
    }
    null.asInstanceOf[T]
  }
}
