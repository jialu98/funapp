package my.work

/**
  * Created by jia on 2/22/2017.
  */
trait Base {
  println("constructor Base ")
  def dowork(x:Int): Int
}

trait Add extends Base {
  println("constructor Add")
  abstract override def dowork(x: Int) = {
    println("add x: " + x)
    super.dowork(x + 10)
  }
}


trait Multiply extends Base {
  println("constructor Multiply")
  abstract override def dowork(x: Int) = {
    println("Multiply x: " + x)
    super.dowork(x * 100)
  }
}

class Sub extends Base {
  println("constructor Sub")
  override def dowork(x: Int) = {
    println("sub x: " + x)
    x
  }
}
