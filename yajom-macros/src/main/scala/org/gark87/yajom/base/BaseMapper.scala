package org.gark87.yajom.base

class BaseMapper[F <: ObjectFactory](val factory:F = new ObjectFactory()) {
  implicit val m = this
}

