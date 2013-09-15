package org.gark87.yajom.base


class BaseMapper[F <: ObjectFactory](val factory:F = new ObjectFactory()) {
  implicit val m = this
  implicit def convertToYajomCollection[T <: java.lang.Object] (source : java.util.Collection[T]) = new YajomCollection[T](source)
}

