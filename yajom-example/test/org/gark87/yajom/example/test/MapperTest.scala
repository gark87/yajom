package org.gark87.yajom.example.test

import org.gark87.yajom.example.to.Person
import org.junit.Before

class MapperTest {
  var person: Person = null

  @Before
  def init() = {
    person = new Person
  }
}
