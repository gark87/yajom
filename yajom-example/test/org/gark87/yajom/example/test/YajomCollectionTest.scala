package org.gark87.yajom.example.test

import org.gark87.yajom.base.DefaultMapper
import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.{Kid, Employee}
import org.gark87.yajom.macros.Facade._
import org.junit.Assert._
import org.junit.Test
import java.util

class YajomCollectionMapper extends DefaultMapper {
  implicit def toEmployee(from: Person): Employee = {
    val result = new Employee()
    yajom(result.getDetails.getKids.find(_.getAge == 12).setKidName)(from.getChildren.get(0).getName)
    result
  }
}

class ComplexCollectionMapper extends DefaultMapper {
  implicit def toEmployee(from: Person): Employee = {
    val result = new Employee()
    yajom(result.getDetails.getKids.find(_.getAge == "12").setKidName)(from.getChildren.get(0).getName)
    yajom(result.getDetails.getKids.find(_.getAge == "12").setAge)(143)
    result
  }
}

class ExtractedCollectionMapper extends DefaultMapper {
  implicit def toEmployee(from: Person): Employee = {
    val result = new Employee()
    val kid = createOnNull(result.getDetails.getKids.find(_.getAge == "12"))
    yajom(kid.setKidName)(from.getChildren.get(0).getName)
    yajom(kid.setAge)(143)
    result
  }
}

class YajomCollectionTest extends MapperTest {
  @Test def testOne() {
    val mapper = new YajomCollectionMapper
    val child: Child = new Child
    child.setName("Jane Dow")
    person.setChildren(util.Arrays.asList(child))
    val employee = mapper.toEmployee(person)
    val kids: util.Set[Kid] = employee.getDetails.getKids
    assertEquals(1, kids.size())
    val kid: Kid = kids.iterator().next
    assertEquals("12", kid.getAge)
    assertEquals("Jane Dow", kid.getKidName)
  }

  @Test def testTwo() {
    val mapper = new ComplexCollectionMapper
    val child: Child = new Child
    child.setName("Jane Dow")
    person.setChildren(util.Arrays.asList(child))
    val employee = mapper.toEmployee(person)
    val kids: util.Set[Kid] = employee.getDetails.getKids
    assertEquals(1, kids.size())
    val kid: Kid = kids.iterator().next
    assertEquals("143", kid.getAge)
    assertEquals("Jane Dow", kid.getKidName)
  }


  @Test def testExtracted() {
    val mapper = new ExtractedCollectionMapper
    val child: Child = new Child
    child.setName("Jane Dow")
    person.setChildren(util.Arrays.asList(child))
    val employee = mapper.toEmployee(person)
    val kids: util.Set[Kid] = employee.getDetails.getKids
    assertEquals(1, kids.size())
    val kid: Kid = kids.iterator().next
    assertEquals("143", kid.getAge)
    assertEquals("Jane Dow", kid.getKidName)
  }

}
