package org.gark87.yajom.api

class BaseMapper[F <: ObjectFactory](val factory:F = new ObjectFactory()) {
  implicit val m = this
}

