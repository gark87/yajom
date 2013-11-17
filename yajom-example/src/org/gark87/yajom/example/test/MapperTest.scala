package org.gark87.yajom.example.test

import org.gark87.yajom.example.to.Person
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class MapperTest extends FunSuite with BeforeAndAfter {
  var person: Person = null

  before {
    person = new Person
  }
}
