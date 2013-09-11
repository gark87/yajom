package org.gark87.yajom.example

import java.util

trait CollectionCreator {
  def createList[T](ref: Object) : util.List[T] = new util.ArrayList[T]()

  def createSet[T](ref: Object) : util.Set[T] = new util.HashSet[T]()

  def map[F,T](from : util.Collection[F], to : util.Collection[T]) {}
}
