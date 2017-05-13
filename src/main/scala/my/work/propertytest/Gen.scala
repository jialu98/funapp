package my.work.propertytest

import my.work.random.Rng
import my.work.stateAction.State

import scala.annotation.tailrec

/**
  * Created by jia on 5/6/2017.
  */
case class Gen[A](sample: State[Rng, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(x => f(x).sample))

  def map[A, B](f: A => B): Gen[B] = Gen(sample.map(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(Gen.listOfN(_, this))
  }

  def listOfN(n: Int): Gen[List[A]] = {
    Gen.listOfN(n, this)
  }

  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    @tailrec
    def go(rng: Rng, start: Int, stopExclusive: Int): (Int, Rng) = {
      val (a, r) = rng.nextInt
      if (start <= a && a < stopExclusive)
        (a, r)
      else
        go(r, start, stopExclusive)
    }
    Gen(State(rng => go(rng, start, stopExclusive)))
  }

  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(Rng.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def weighted[A] (g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    choose(0, 100).flatMap(n => if (n * g1._2 > n * g2._2) g1._1 else g2._1)
  }
}
