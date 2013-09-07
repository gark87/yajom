package org.gark87.yajom.example

import java.util
import org.gark87.yajom.example.from._
import org.gark87.yajom.example.to._

object MainApp extends App {
  private val to: Employee = new Employee
  private val from: Person = new Person
  from.setChildren(new util.ArrayList[Child]())
  new ExampleMapper().map(from, to)
}