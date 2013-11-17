package org.gark87.yajom.example.test

import org.gark87.yajom.base.DefaultMapper
import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.{Kid, Employee}
import org.gark87.yajom.macros.Facade._
import java.util

class CollectionsMapper extends DefaultMapper {
  implicit def toEmployee(from: Person): Employee = {
    val result = new Employee()
    yajom(result.getDetails.setBirth)(from.getBirthDate)
    yajom(result.setLast)(from.getLastName)
    yajom(result.getDetails.setKids)(from.getChildren)
    result
  }

  implicit def toKid(from: Child): Kid = {
    val result = new Kid

    yajom(result.setAge)(from.getAge)
    yajom(result.setKidName)(from.getName)

    result
  }
}

class CollectionsTest extends MapperTest {
  private val mapper = new CollectionsMapper

  test("Only empty Employee with Details shoule be created") {
    val employee = mapper.toEmployee(person)
    assert(null != employee)
    assert(null != employee.getDetails)
    assert(null == employee.getDetails.getBirth)
    assert(null == employee.getLast)
    assert(null == employee.getDetails.getKids)
  }

  test("Empty collection of Kids should be created") {
    person.setChildren(new util.ArrayList[Child])
    val employee = mapper.toEmployee(person)
    assert(0 == employee.getDetails.getKids.size())
  }

  test("Non-empty collection of Kids should be created") {
    person.setChildren(new util.ArrayList[Child])
    val child = new Child()
    child.setAge(12)
    child.setName("John Dow")
    person.getChildren.add(child)
    val employee = mapper.toEmployee(person)
    val kids = employee.getDetails.getKids
    assert(1 == kids.size())
    val kid = kids.iterator().next()
    assert("12" == kid.getAge)
    assert("John Dow" == kid.getKidName)
  }
}
