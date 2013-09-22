package org.gark87.yajom.example.test

import org.gark87.yajom.base.BaseMapper
import org.gark87.yajom.example.to.Person
import org.junit.Before

class MapperTest[M <: BaseMapper[_]](val mapper:M) {

    var person : Person = null

    @Before
    def init() = {
      person = new Person
    }
}
