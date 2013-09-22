package org.gark87.yajom.example.test

import org.gark87.yajom.base.DefaultMapper
import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.{Kid, Employee}
import org.gark87.yajom.macros.Facade._
import org.junit.Assert._
import org.junit.Test
import java.util

class CollectionsMapper extends DefaultMapper {
  implicit def toEmployee(from: Person) : Employee = {
    val result = new Employee()
    yajom(result.getDetails.setBirth)(from.getBirthDate)
    yajom(result.setLast)(from.getLastName)
    yajom(result.getDetails.setKids)(from.getChildren)
    result
  }

  implicit def toKid(from: Child) : Kid = {
    val result = new Kid

    yajom(result.setAge)(from.getAge)
    yajom(result.setKidName)(from.getName)

    result
  }
}

class CollectionsMapperTest extends MapperTest(new CollectionsMapper){
  @Test def testSimple() {
    val employee = mapper.toEmployee(person)
    assertNotNull(employee)
    assertNotNull(employee.getDetails)
    assertNull(employee.getDetails.getBirth)
    assertNull(employee.getLast)
    assertNull(employee.getDetails.getKids)
  }

  @Test def testEmptyCollection() {
    person.setChildren(new util.ArrayList[Child])
    val employee = mapper.toEmployee(person)
    assertEquals(0, employee.getDetails.getKids.size())
  }

  @Test def testFilledCollection() {
    person.setChildren(new util.ArrayList[Child])
    val child = new Child()
    child.setAge(12)
    child.setName("John Dow")
    person.getChildren.add(child)
    val employee = mapper.toEmployee(person)
    val kids = employee.getDetails.getKids
    assertEquals(1, kids.size())
    val kid = kids.iterator().next()
    assertEquals("12", kid.getAge)
    assertEquals("John Dow", kid.getKidName)
  }
}
