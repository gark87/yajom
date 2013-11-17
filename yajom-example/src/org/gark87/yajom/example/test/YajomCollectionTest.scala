package org.gark87.yajom.example.test

import org.gark87.yajom.base.DefaultMapper
import org.gark87.yajom.example.to.{Child, Person}
import org.gark87.yajom.example.from.{Girl, Kid, Employee}
import org.gark87.yajom.macros.Facade._
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

class TypedCollectionMapper extends DefaultMapper {
  implicit def toEmployee(from: Person): Employee = {
    val result = new Employee()
    yajom(result.getDetails.getKids.findTyped[Girl](_.isLikeDolls == true).setKidName)(from.getChildren.get(0).getName)
    result
  }
}


class AnyMapper extends DefaultMapper {
  implicit def toEmployee(from: Person): Employee = {
    val result = new Employee()
    yajom(result.getDetails.getKids.any[Kid].setKidName)(from.getChildren.get(0).getName)
    result
  }
}

class YajomCollectionTest extends MapperTest {
  test("ComplexCollectionMapper") {
    val mapper = new ComplexCollectionMapper
    val child: Child = new Child
    child.setName("Jane Dow")
    val person = new Person
    person.setChildren(util.Arrays.asList(child))
    val employee = mapper.toEmployee(person)
    val kids: util.Set[Kid] = employee.getDetails.getKids
    assert(1 == kids.size())
    val kid: Kid = kids.iterator().next
    assert("143" == kid.getAge)
    assert("Jane Dow" == kid.getKidName)
  }

  test("ExtractedCollectionMapper") {
    val mapper = new ExtractedCollectionMapper
    val child: Child = new Child
    child.setName("Jane Dow")
    val person = new Person
    person.setChildren(util.Arrays.asList(child))
    val employee = mapper.toEmployee(person)
    val kids: util.Set[Kid] = employee.getDetails.getKids
    assert(1 == kids.size())
    val kid: Kid = kids.iterator().next
    assert("143" == kid.getAge)
    assert("Jane Dow" == kid.getKidName)
  }

  test("TypedCollectionMapper") {
    val mapper = new TypedCollectionMapper
    val child: Child = new Child
    child.setName("Jane Dow")
    val person = new Person
    person.setChildren(util.Arrays.asList(child))
    val employee = mapper.toEmployee(person)
    val kids: util.Set[Kid] = employee.getDetails.getKids
    assert(1 == kids.size())
    val kid: Kid = kids.iterator().next
    assert("Jane Dow" == kid.getKidName)
    assert(kid.isInstanceOf[Girl])
    assert(kid.asInstanceOf[Girl].isLikeDolls)
  }


  test("AnyMapper") {
    val mapper = new AnyMapper
    val child: Child = new Child
    child.setName("Jane Dow")
    val person = new Person
    person.setChildren(util.Arrays.asList(child))
    val employee = mapper.toEmployee(person)
    val kids: util.Set[Kid] = employee.getDetails.getKids
    assert(1 == kids.size())
    val kid: Kid = kids.iterator().next
    assert("Jane Dow" == kid.getKidName)
  }

}
