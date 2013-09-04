// child2kid.groovy


import groovy.transform.TypeChecked
import org.from.Kid
import gark87.to.Child
import org.nomin.Mapping.*

mappingFor a: Child, b: Kid
a.name = b.kidName
