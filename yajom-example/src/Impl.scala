import org.gark87.from._
import org.gark87.to._
import DebugMacros._

class Impl extends Base {
  implicit def map(from: Person, to: Employee) {
    copy (from.getBirthDate) (nullSafe(to.getDetails.getDetails.setBirth))
    copy (from.getStrDate)   (nullSafe(to.getDetails.setBirth))
    copy (from.getChildren)  (nullSafe(to.getDetails.setKids))
  }

  implicit def map(from: Child, to: Kid) {
    copy (from.getName) {to.setKidName}
  }
}