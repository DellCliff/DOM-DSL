package dellcliff.html.dom

import scala.scalajs.js.Dynamic
import scala.util.Try

trait Attribute extends NodeOrAttribute {

  def apply(element: Dynamic): Try[Unit]

}

object Attribute {

  sealed class StringOrAttribute[T]

  object StringOrAttribute {

    implicit object AttributeMember extends StringOrAttribute[Attribute]

    implicit object StringMember extends StringOrAttribute[String]

  }

  implicit class AttributeStringOps(val key: String) extends AnyVal {

    def <=(value: Observable[String]) = new Attribute {
      override def apply(element: Dynamic): Try[Unit] = Try {
        value.addObserver(new Observer[String] {
          override def onNext(x: String): Unit =
            element.updateDynamic(key)(x)
        })
      }
    }

    def <~(value: Observable[String]) = new Attribute {
      override def apply(element: Dynamic): Try[Unit] = Try {
        value.addObserver(new Observer[String] {
          override def onNext(x: String): Unit =
            element.setAttribute(key, x)
        })
      }
    }

    def :~(value: String*) = new Attribute {
      override def apply(element: Dynamic): Try[Unit] = Try {
        element.setAttribute(key, value.mkString(" "))
      }
    }

    def :=[T: StringOrAttribute](value: T*) = value.headOption match {
      case None => new Attribute {
        override def apply(props: Dynamic): Try[Unit] = Try {}
      }
      case Some(x: String) => new Attribute {
        override def apply(props: Dynamic): Try[Unit] = Try {
          props.updateDynamic(key)(value.mkString(" "))
        }
      }
      case Some(x: Attribute) => new Attribute {
        override def apply(props: scalajs.js.Dynamic): Try[Unit] = Try {
          if (props.selectDynamic(key) == null || scalajs.js.isUndefined(props.selectDynamic(key)))
            props.updateDynamic(key)(scalajs.js.Dynamic.literal())
          for (vs <- value; v = vs.asInstanceOf[Attribute])
            v.apply(props.selectDynamic(key))
        }
      }
      case other => new Attribute {
        override def apply(props: Dynamic): Try[Unit] = Try {}
      }
    }

    def ->[A](value: PartialFunction[Any, A]) = new Attribute {
      val o: PartialFunction[Any, _] = value orElse { case other => }

      override def apply(element: Dynamic): Try[Unit] = Try {
        element.updateDynamic(key)(o)
      }
    }

  }

  implicit class AttributeSymbolOps(val key: Symbol) extends AnyVal {

    def <=(value: Observable[String]) = key.name <= value

    def <~(value: Observable[String]) = key.name <~ value

    def :~(value: String*) = key.name :~ (value: _*)

    def :=[T: StringOrAttribute](value: T*) = key.name := (value: _*)

    def ->[A](value: PartialFunction[Any, A]) = key.name -> value

  }

}
