package my.work.propertytest

import my.work.random.Rng

import scala.util.Try

/**
  * Created by jia on 5/6/2017.
  */

trait Propaa {

  def check: Result

  def &&(that: Prop): Prop
}

case class Prop(run: (TestCases, Rng) => Result) {

}

object Prop {

  def randomStream[A](g: Gen[A])(rng: Rng): Stream[A] = {
    var wkrng = rng
    Stream.continually {
      val (a, r) = g.sample.run(wkrng)
      wkrng = r
      a
    }
  }

  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(g)(rng).zip(Stream.from(0)).take(n)
          .map { case(a, i) => Try{
            if (f(a)) Passed else Falsified(a.toString, i)}
            .getOrElse(Falsified(a.toString, i))}
          .find(_.isFalsified).getOrElse(Passed)
  }
}

