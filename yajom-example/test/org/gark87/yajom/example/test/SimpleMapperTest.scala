package org.gark87.yajom.example.test

import org.gark87.yajom.base.BaseMapper
import org.gark87.yajom.example.to.Person
import org.gark87.yajom.example.from.Employee
import org.gark87.yajom.macros.Facade._
import org.junit.Assert._
import java.util.Date
import org.junit.Test

class SimpleMapper extends BaseMapper {
  implicit def toEmployee(from: Person) : Employee  = {
    val result = new Employee()

    val details = createOnNull(result.getDetails)
    yajom(details.setBirth)(from.getBirthDate)

    result
  }
}

class SimpleMapperTest extends MapperTest(new SimpleMapper){
  @Test def testSimple() {
    val employee = mapper.toEmployee(person)
    assertNotNull(employee)
    assertNotNull(employee.getDetails)
    assertNull(employee.getDetails.getBirth)
  }

  @Test def testFilledDetails() {
    person.setBirthDate(new Date(0))
    val employee: Employee = mapper.toEmployee(person)
    assertEquals(new Date(0), employee.getDetails.getBirth)
  }
}
