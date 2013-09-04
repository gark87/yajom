// person2employee.groovy

import org.from.Employee
import gark87.to.Gender
import gark87.to.Person
import org.nomin.Mapping.*

mappingFor a: Person, b: Employee
a.name = b.name
a.lastName = b.last
a.birthDate = b.details.birth
a.children = b.details.kids

a.strDate = b.details.birth
dateFormat "dd-MM-yyyy"

a.gende = b.details.sex
simple ([Gender.MALE, true], [Gender.FEMALE, false])
a.snn = b.employeeId
convert to_a: { eId -> repositry.findByEmployeeId(eId) }, to_b: { snn -> repository.findBySnn(snn) }