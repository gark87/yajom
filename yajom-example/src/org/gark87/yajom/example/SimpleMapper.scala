package org.gark87.yajom.example

import org.gark87.yajom.example.to.Person
import org.gark87.yajom.example.from.Employee
import org.gark87.yajom.base.BaseMapper
import org.gark87.yajom.macros.Facade._

class SimpleMapper extends BaseMapper(new ExampleObjectFactory()) with StringDateConversion with CollectionCreator {
  implicit def map(from: Person, to: Employee) {
    val details = createOnNull(to.getDetails)
    yajom(details.setBirth)(from.getBirthDate)
  }
}