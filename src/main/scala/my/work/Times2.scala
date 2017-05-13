package my.work

/**
  * Created by jia on 7/17/2016.
  */
class Times2 extends (Int => Int) {
  def apply(x: Int): Int = {
    x * 2
  }
}
