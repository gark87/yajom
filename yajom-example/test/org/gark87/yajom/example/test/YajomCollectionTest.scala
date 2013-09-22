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
    yajom(result.getDetails.getKids.find(_.getAge == "12").setKidName)(from.getChildren.get(0).getName)
    result
  }
}

class YajomCollectionTest extends MapperTest(new YajomCollectionMapper){
  @Test def testSimple() {
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

}
