package org.gark87.yajom.base

import collection.JavaConversions._

class YajomCollection[T](val source: java.util.Collection[T]) {
  val seq = source.toSeq

  def find(predicate : T => Boolean) : T = {
    val result: Option[T] = seq.find(predicate)
    result match {
      case None => null.asInstanceOf[T]
      case Some(t) => t
    }
  }
}
