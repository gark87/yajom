package org.gark87.yajom.example.test

import org.gark87.yajom.base.BaseMapper
import org.gark87.yajom.example.to.{Gender, Person}
import org.gark87.yajom.example.from.{Details, Employee}
import org.gark87.yajom.macros.Facade._
import java.util.Date

class SimpleMapper extends BaseMapper {
  implicit def toEmployee(from: Person): Employee = {
    val result = new Employee()

    val details = createOnNull(result.getDetails)
    yajom(details.setBirth)(from.getBirthDate)

    result
  }
}

class MapSimpleMapper extends BaseMapper {
  def map(from: Person, to: Employee) {
    yajomMap(to.setDetails)(from.getBirthDate)
  }

  def map(from: Date, to: Details) {
    yajom(to.setBirth)(from)
  }
}

class SimpleTest extends MapperTest {
  private val mapper = new SimpleMapper
  private val mapMapper = new MapSimpleMapper

  test("Map Person without Details") {
    val employee = mapper.toEmployee(person)
    val to = new Employee
    mapMapper.map(person, to)
    List(to, employee).foreach(employee => {
      assert(null != employee)
      assert(null != employee.getDetails)
      assert(null == employee.getDetails.getBirth)
    })
  }

  test("Map Person with Details") {
    person.setBirthDate(new Date(0))
    val employee = mapper.toEmployee(person)
    val to = new Employee
    mapMapper.map(person, to)
    List(to, employee).foreach(employee => {
      assert(new Date(0) == employee.getDetails.getBirth)
    })
  }
}
