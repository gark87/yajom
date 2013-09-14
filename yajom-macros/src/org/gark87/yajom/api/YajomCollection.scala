package org.gark87.yajom.api
import collection.JavaConversions._

class YajomCollection[T](val source: java.util.Collection[T]) {
  val seq = source.toSeq

  def find(predicate : T => Boolean) : T = throw new IllegalArgumentException("This method should not be called, should be replaced by YAJOM")
}
