package org.gark87.yajom.example

import org.gark87.yajom.macros._

import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.{Kid, Employee}
import org.gark87.yajom.base.BaseMapper
import org.gark87.yajom.macros.CreateOnNull._

class SimpleMapper extends BaseMapper(new ExampleObjectFactory()) with StringDateConversion with CollectionCreator {
  implicit def map(from: Person, to: Employee) {
    val details = createOnNull(to.getDetails)
    nullSafe(details.setBirth) = from.getBirthDate
  }
}