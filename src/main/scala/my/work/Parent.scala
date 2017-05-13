package my.work

/**
  * Created by jia on 4/1/2017.
  */
class GrandParent(val value: Int)
class Parent(value: Int) extends GrandParent(value)
class Child(value: Int) extends Parent(value)

class Opt[+A](val v: A) {
  def getOrElse[B >: A](default: =>B): B = {
    if (v != null) v else default
  }
}
