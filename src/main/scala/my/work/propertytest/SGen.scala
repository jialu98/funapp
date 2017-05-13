package my.work.propertytest

/**
  * Created by jia on 5/10/2017.
  */
case class SGen[A](g: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2 = (n: Int) => {
      g(n).flatMap(f(_).g(n))
    }
    SGen(g2)
  }

  def map[B](f: A => B): SGen[B] = SGen(g(_).map(f))
}

object SGen {
  def listOf[A] (g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

}
