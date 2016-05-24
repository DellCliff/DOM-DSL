package dellcliff.html.dom

import scala.scalajs.js.Dynamic
import scala.language.implicitConversions
import scala.util.Try


trait NodeOrAttribute

sealed case class ChildNodesOrAttributes
(nodesOrAttributes: Traversable[NodeOrAttribute]) extends NodeOrAttribute

trait Node extends NodeOrAttribute {
  val node: Try[Dynamic]
}

case class Text(node: Try[Dynamic]) extends Node

case class Comment(node: Try[Dynamic]) extends Node

case class Element(node: Try[Dynamic]) extends Node

case class Fragment(node: Try[Dynamic]) extends Node

object Node {

  def text(value: String): Text =
    Text(Try(Dynamic.global.document.createTextNode(value)))

  def comment(value: String): Comment =
    Comment(Try(Dynamic.global.document.createComment(value)))

  def element(tag: String, attrs: Traversable[Attribute], kids: Traversable[Node]): Element =
    Element(Try {
      val e = Dynamic.global.document.createElement(tag)
      for (attr <- attrs)
        attr.apply(e)
      for (kid <- kids)
        e.appendChild(kid.node.get)
      e
    })

  def fragment(nodes: Traversable[Node]): Fragment =
    Fragment(Try {
      val f = Dynamic.global.document.createDocumentFragment()
      for (node <- nodes)
        f.appendChild(node.node.get)
      f
    })

  implicit def nodeOrAttributesToChildNodesOrAttributes(na: Traversable[NodeOrAttribute]): ChildNodesOrAttributes =
    ChildNodesOrAttributes(na)

  implicit def stringToText(na: String): Text = text(na)

  implicit def stringsChildNodesOrAttributes(na: Traversable[String]): ChildNodesOrAttributes =
    ChildNodesOrAttributes(na.map(text))

  private def attributes(nax: Traversable[NodeOrAttribute]): Traversable[Attribute] =
    nax flatMap {
      case attr: Attribute => List(attr)
      case ChildNodesOrAttributes(k) => attributes(k)
      case other => List()
    }

  private def nodes(nax: Traversable[NodeOrAttribute]): Traversable[Node] =
    nax flatMap {
      case node: Node => List(node)
      case ChildNodesOrAttributes(k) => nodes(k)
      case other => List()
    }

  implicit class StringOps(val value: String) extends AnyVal {

    def text = Node.text(value)

    def comment = Node.comment(value)

    def tag(content: NodeOrAttribute*): Element =
      element(value, attributes(content), nodes(content))

    def tag(content: Traversable[NodeOrAttribute]): Element =
      element(value, attributes(content), nodes(content))

  }

  implicit class StringContextOps(val sc: StringContext) extends AnyVal {

    def text(args: Any*) = Node.text(sc.s(args: _*))

    def comment(args: Any*) = Node.comment(sc.s(args: _*))

  }

  implicit class SymbolOps(val tag: Symbol) extends AnyVal {

    def apply(content: NodeOrAttribute*): Element =
      element(tag.name, attributes(content), nodes(content))

    def apply(content: Traversable[NodeOrAttribute]): Element = {
      element(tag.name, attributes(content), nodes(content))
    }

  }

  implicit class NodeListOps(val nodes: Traversable[Node]) extends AnyVal {

    def fragment: Fragment = Node.fragment(nodes)

  }

  implicit class NodeOrAttributeListOps(val nodes: Traversable[NodeOrAttribute]) extends AnyVal {

    def include: ChildNodesOrAttributes = ChildNodesOrAttributes(nodes)

  }

}
