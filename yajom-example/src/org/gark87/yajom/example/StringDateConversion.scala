package org.gark87.yajom.example

import java.util.Date

trait StringDateConversion {
  def createDate(a: String) : java.util.Date = ???

  def map(from: String, to: Date) = ???

  def createString(a: Any) : String = String.valueOf(a)

//  def map(from: String, to: Date) = ???
}
