package org.gark87.yajom.example

import org.gark87.yajom.BaseMapper
import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.{Kid, Employee}

class ExampleMapper extends BaseMapper {
  implicit def map(from: Person, to: Employee) {
    copy (from.getBirthDate) (nullSafe(to.getDetails.getDetails.setBirth))
    copy (from.getStrDate)   (nullSafe(to.getDetails.setBirth))
    copy (from.getChildren)  (nullSafe(to.getDetails.setKids))
  }

  implicit def map(from: Child, to: Kid) {
    copy (from.getName) {to.setKidName}
  }
}