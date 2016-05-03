package dellcliff.html.dom

import scala.scalajs.js.Dynamic


trait Node extends NodeOrAttribute {
  val node: Dynamic
}

case class Text(node: Dynamic) extends Node

case class Comment(node: Dynamic) extends Node

case class Element(node: Dynamic) extends Node

case class Fragment(node: Dynamic) extends Node

object Node {

  def text(value: String): Text =
    Text(Dynamic.global.document.createTextNode(value))

  def comment(value: String): Comment =
    Comment(Dynamic.global.document.createComment(value))

  def element(tag: String, attrs: Traversable[Attribute], kids: Traversable[Node]): Element = {
    val e = Dynamic.global.document.createElement(tag)
    for (attr <- attrs)
      attr.apply(e)
    for (kid <- kids)
      e.appendChild(kid.node)
    Element(e)
  }

  def fragment(nodes: Traversable[Node]): Fragment = {
    val f = Dynamic.global.document.createDocumentFragment()
    for (node <- nodes)
      f.appendChild(node.node)
    Fragment(f)
  }

  implicit class StringOps(val value: String) extends AnyVal {
    def text = Node.text(value)

    def comment = Node.comment(value)

    def tag(content: NodeOrAttribute*): Element = value.tag(content)

    def tag(content: Traversable[NodeOrAttribute]): Element =
      Symbol(value)(content)
  }

  implicit class StringContextOps(val sc: StringContext) extends AnyVal {
    def text(args: Any*) = Node.text(sc.s(args: _*))

    def comment(args: Any*) = Node.comment(sc.s(args: _*))
  }

  implicit class SymbolOps(val tag: Symbol) extends AnyVal {
    def apply(content: NodeOrAttribute*): Element = tag(content)

    def apply(content: Traversable[NodeOrAttribute]): Element = {
      def attributes(nax: Traversable[NodeOrAttribute]): Traversable[Attribute] =
        nax.flatMap {
          case attr: Attribute => List(attr)
          case ChildNodesOrAttributes(k) => attributes(k)
          case other => List()
        }
      def nodes(nax: Traversable[NodeOrAttribute]): Traversable[Node] =
        nax.flatMap {
          case node: Node => List(node)
          case ChildNodesOrAttributes(k) => nodes(k)
          case other => List()
        }
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
