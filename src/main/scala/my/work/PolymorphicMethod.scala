package my.work

/**
  * Created by jia on 4/2/2017.
  */
class PolymorphicMethod {
  def foo[A](x: List[A]): A = x.head

  def foo2(x: Parent => Unit): Unit = x(new Parent(1))
}
