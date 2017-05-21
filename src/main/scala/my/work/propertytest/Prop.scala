package my.work.propertytest

import my.work.random.Rng

import scala.util.Try

/**
  * Created by jia on 5/6/2017.
  */

case class Prop(run: (MaxSize, TestCases, Rng) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case x => x
    }
  }
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
    (max, n, rng) => randomStream(g)(rng).zip(Stream.from(0)).take(n).map{
      case(a, i) => Try{if (f(a)) Passed else Falsified(a.toString, i)}
        .recover{case e => Falsified(e.toString, i)}.get
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + max -1)/ max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map{p => Prop{ (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }}.toList.reduce( _ && _)
      prop.run(max, n, rng)
  }
}

