package org.gark87.yajom.example

import org.gark87.yajom.{BaseMapper, nullSafe}

import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.{Kid, Employee}

class ExampleMapper extends BaseMapper {
  implicit def map(from: Person, to: Employee) {
    nullSafe(to.getDetails.setBirth) = from.getBirthDate
    nullSafe(to.getDetails.setBirth) = from.getStrDate
    nullSafe(to.getDetails.setKids) = from.getChildren
  }

  implicit def map(from: Child, to: Kid) {
    nullSafe(to.setKidName) = from.getName
  }
}