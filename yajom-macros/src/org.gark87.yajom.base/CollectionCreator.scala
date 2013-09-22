package org.gark87.yajom.base

import java.util

trait CollectionCreator {
  def createList[T](): util.List[T] = new util.ArrayList[T]()

  def createSet[T](): util.Set[T] = new util.HashSet[T]()

  implicit def toCollection[F, T](from: util.Collection[F])(implicit wrap: F => T): util.Set[T] = {
    if (from == null)
      null
    else {
      val result = new util.HashSet[T]()

      val it = from.iterator()
      while (it.hasNext) {
        result.add(wrap(it.next()))
      }
      result
    }
  }
}
