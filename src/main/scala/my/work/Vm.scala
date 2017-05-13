package my.work

/**
  * Created by jia on 4/2/2017.
  */
trait Vm [A] {
  def compile: A
  def run: A => Unit
}

case class SimpleVm [A] (compile: A, run: A => Unit) extends Vm[A]