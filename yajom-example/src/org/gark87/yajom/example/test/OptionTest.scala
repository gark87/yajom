package org.gark87.yajom.example.test

import org.gark87.yajom.base.BaseMapper
import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.Employee
import org.gark87.yajom.macros.Facade._
import java.util.Date
import java.util

class OptionMapper extends BaseMapper {
  implicit def toEmployee(from: Person): Employee = {
    val result = new Employee()

    yajomOption(result.getDetails.setBirth) {
      val children = from.getChildren
      if (children == null || children.isEmpty)
        None
      else
        Some(from.getBirthDate)
    }

    result
  }
}

class OptionTest extends MapperTest {
  private val mapper = new OptionMapper

  test("Default Person") {
    val employee = mapper.toEmployee(person)
    assert(null == employee.getDetails)
  }

  test("Default Person with empty collection") {
    person.setChildren(new util.ArrayList[Child]())
    val employee = mapper.toEmployee(person)
    assert(null == employee.getDetails)
  }

  test("Default Person with non-empty collection") {
    person.setChildren(new util.ArrayList[Child]())
    person.getChildren.add(new Child)
    val employee = mapper.toEmployee(person)
    assert(null != employee.getDetails)
    assert(null == employee.getDetails.getBirth)
  }

  test("Default Person with non-empty collection and Birth-date") {
    person.setChildren(new util.ArrayList[Child]())
    person.getChildren.add(new Child)
    person.setBirthDate(new Date(0))
    val employee = mapper.toEmployee(person)
    assert(null != employee.getDetails)
    assert(new Date(0) == employee.getDetails.getBirth)
  }
}
