package dellcliff.html.dom

trait NodeOrAttribute

sealed case class ChildNodesOrAttributes
(nodesOrAttributes: Traversable[NodeOrAttribute]) extends NodeOrAttribute
