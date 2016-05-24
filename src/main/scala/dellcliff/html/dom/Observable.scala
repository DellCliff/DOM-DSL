package dellcliff.html.dom

trait Observer[A] {
  def onNext(x: A): Unit
}

trait Observable[A] {
  def addObserver(observer: Observer[A]): Unit
}
