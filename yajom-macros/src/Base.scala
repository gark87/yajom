import java.util
import java.util.Date
import scala.Predef._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}


class Base {
  implicit val m = this

  implicit def map[A, B](from: util.Collection[A], to: util.Collection[B])(implicit map: (A, B) => Unit, typeTag: TypeTag[B]) = {
    for (aa <- from.toArray) {
      val a: A = aa.asInstanceOf[A]
      create(a, typeTag, null, (b: B) => {
        if (b != null) {
          map(a, b)
          to.add(b)
        }
      })

    }
  }

  implicit def map(from: String, to: Date) {
  }

  def create[T](source: Any, typetag: TypeTag[T], getter: () => T, setter : (T) => Unit, param: String*): T = {
    if (getter != null) {
      val value: T = getter()
      if (value != null) {
        value
      } else {
        val value:T = createDefaultValue(typetag, source).asInstanceOf[T]
        setter(value)
        value
      }
    } else {
      val value:T = createDefaultValue(typetag, source).asInstanceOf[T]
      setter(value)
      value
    }
  }

  //  def map(from: String, to: AnyVal){}

  //  def map(from: Any, to: String){}

  def createDefaultValue[T](typetag: TypeTag[T], source: Any, param: String*): Any = {
    typetag.tpe match {
      case t if t =:= ru.typeOf[String] => String.valueOf(source)
      case t if t =:= ru.typeOf[Int] =>
        if (source.getClass == classOf[String]) {
          Integer.parseInt(source.asInstanceOf[String])
        } else {
          null
        }
      case t if t =:= ru.typeOf[java.lang.Double] =>
        if (source.getClass == classOf[String]) {
          java.lang.Double.parseDouble(source.asInstanceOf[String])
        } else {
          null
        }
      case t if t <:< ru.typeOf[java.util.Set[_]] => new util.HashSet()
      case t if t <:< ru.typeOf[java.util.Date]   => new util.Date()

      case t => {
        val ctor = t.member(nme.CONSTRUCTOR).asMethod
        val c = t.typeSymbol.asClass
        val mm = cm.reflectClass(c).reflectConstructor(ctor)
        mm()
      }
    }
  }
}

