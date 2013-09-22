package org.gark87.yajom.example

import java.text.DateFormat

trait StringDateConversion {
  implicit def createDate(a: String) : java.util.Date = DateFormat.getInstance().parse(a)

  implicit def createString(a: Any) : String = String.valueOf(a)
}
