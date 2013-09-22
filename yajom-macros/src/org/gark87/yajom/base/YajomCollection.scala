package org.gark87.yajom.base

import org.gark87.yajom.annotations.{ReturnOnNull, PredicateToFactory}
import scala.reflect.ClassTag

class YajomCollection[T <: java.lang.Object](val source: java.util.Collection[T]) {

  def findTyped[A <: T : ClassTag](@ReturnOnNull("predicate") predicate: A => Boolean, @PredicateToFactory("predicate") create: () => A = YajomCollection.defaultFactory): A = {
    val it = source.iterator()
    while (it.hasNext) {
      val next = it.next()
      next match {
        case typedNext: A =>
          if (predicate(typedNext))
            return typedNext
        case _ =>
      }
    }
    val elem: A = create()
    source.add(elem)
    elem
  }

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
