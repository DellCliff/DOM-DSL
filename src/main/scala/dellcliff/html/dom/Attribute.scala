package dellcliff.html.dom

import scala.scalajs.js.Dynamic


trait Attribute extends NodeOrAttribute {
  def apply(element: Dynamic): Unit
}

case class StringAttribute
(key: String, value: String) extends Attribute {
  override def apply(element: Dynamic): Unit =
    element.setAttribute(key, value)
}

case class StringListAttribute
(key: String, value: Traversable[String]) extends Attribute {
  override def apply(element: Dynamic): Unit =
    element.setAttribute(key, value.mkString(" "))
}

case class FuncAttribute[A]
(key: String, value: Any => A) extends Attribute {
  override def apply(element: Dynamic): Unit =
    element.asInstanceOf[Dynamic].updateDynamic(key)(value)
}

case class PartialFuncAttribute[A]
(key: String, value: PartialFunction[Any, A]) extends Attribute {
  override def apply(element: Dynamic): Unit = {
    val o: PartialFunction[Any, _] = {
      case other =>
    }
    element.asInstanceOf[Dynamic].updateDynamic(key)(value orElse o)
  }
}

case class AttributeList
(key: String, value: Traversable[StringAttribute]) extends Attribute {
  override def apply(element: Dynamic): Unit = {
    val en = element.asInstanceOf[Dynamic]
    for (StringAttribute(k, v) <- value)
      en.selectDynamic(key).updateDynamic(k)(v)
  }
}

object Attribute {

  implicit class AttributeStringOps(val key: String) extends AnyVal {
    def :=(value: String) = new StringAttribute(key, value)

    def :=(value: String*) = new StringListAttribute(key, value)

    def :=(value: PartialFunction[Any, _]) = new PartialFuncAttribute(key, value)

    def :-(value: PartialFunction[Any, _]) = new PartialFuncAttribute(key, value)

    def :=(value: Any => _) = new FuncAttribute(key, value)

    def :=(value: () => _) = new FuncAttribute(key, { x: Any => value() })

    def :=(value: StringAttribute*) = new AttributeList(key, value)
  }

  implicit class AttributeSymbolOps(val key: Symbol) extends AnyVal {
    def :=(value: String) = new StringAttribute(key.name, value)

    def :=(value: String*) = new StringListAttribute(key.name, value)

    def :=(value: PartialFunction[Any, _]) = new PartialFuncAttribute(key.name, value)

    def :-(value: PartialFunction[Any, _]) = new PartialFuncAttribute(key.name, value)

    def :=(value: Any => _) = new FuncAttribute(key.name, value)

    def :=(value: () => _) = new FuncAttribute(key.name, { x: Any => value() })

    def :=(value: StringAttribute*) = new AttributeList(key.name, value)
  }

}
