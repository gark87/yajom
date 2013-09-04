import java.util
import org.gark87.from.{Details, Employee, Kid}
import org.gark87.to.{Person, Child}

object DebugExample extends App {
  private val to: Employee = new Employee
  private val from: Person = new Person
  from.setChildren(new util.ArrayList[Child]())
  new Impl().map(from, to)
}