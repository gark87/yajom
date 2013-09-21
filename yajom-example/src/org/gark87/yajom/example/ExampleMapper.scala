package org.gark87.yajom.example

import org.gark87.yajom.macros._

import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.{Kid, Employee}
import org.gark87.yajom.base.BaseMapper
import org.gark87.yajom.macros.CreateOnNull._

class ExampleMapper extends BaseMapper(new ExampleObjectFactory()) with StringDateConversion with CollectionCreator {
  implicit def map(from: Person, to: Employee) {
    val details = createOnNull(to.getDetails)
//    nullSafe(details.setBirth) = from.getBirthDate
//    nullSafe(details.setBirth) = from.getStrDate
    val a = createOnNull(details.getKids.find( (a: Kid) => a.getKidName.equals("a") && a.getKidName.equals("a")&& a.getKidName.equals("a")))
    nullSafe(a.setAge) = String.valueOf(from.getChildren.get(0).getAge)
  }

  implicit def map(from: Child, to: Kid) {
    nullSafe(to.setKidName) = from.getName
  }
}