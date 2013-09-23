package org.gark87.yajom.example.test

import org.gark87.yajom.base.BaseMapper
import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.Employee
import org.gark87.yajom.macros.Facade._
import org.junit.Assert._
import java.util.Date
import org.junit.Test
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

  @Test def testSimple() {
    val employee = mapper.toEmployee(person)
    assertNull(employee.getDetails)
  }

  @Test def testEmpty() {
    person.setChildren(new util.ArrayList[Child]())
    val employee = mapper.toEmployee(person)
    assertNull(employee.getDetails)
  }

  @Test def testNull() {
    person.setChildren(new util.ArrayList[Child]())
    person.getChildren.add(new Child)
    val employee = mapper.toEmployee(person)
    assertNotNull(employee.getDetails)
    assertNull(employee.getDetails.getBirth)
  }

  @Test def testNotNull() {
    person.setChildren(new util.ArrayList[Child]())
    person.getChildren.add(new Child)
    person.setBirthDate(new Date(0))
    val employee = mapper.toEmployee(person)
    assertNotNull(employee.getDetails)
    assertEquals(new Date(0), employee.getDetails.getBirth)
  }
}
