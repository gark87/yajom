package org.gark87.yajom.example

import java.util

trait CollectionCreator {
  def create[T](ref: Object) : util.List[T] = new util.ArrayList[T]()

//  def create[T](ref: Object) : util.Set[T] = new util.HashSet[T]()

}
