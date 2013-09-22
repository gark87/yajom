package org.gark87.yajom.base

class DefaultMapper extends BaseMapper(new DefaultObjectFactory) with CollectionCreator with StringDateConversion {
  implicit def convertToYajomCollection[T <: java.lang.Object] (source : java.util.Collection[T]) = new YajomCollection[T](source)
}
