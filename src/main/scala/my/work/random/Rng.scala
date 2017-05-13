package my.work.random

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by jia on 4/14/2017.
  */
trait Rng {
  def nextInt: (Int, Rng)
}

object Rng {

  type Rand[+A] = Rng => (A, Rng)

  def nonNegtiveInt(rng: Rng): (Int, Rng) = {
    rng.nextInt
  }

  def double(rng: Rng): (Double, Rng) = {
    val (a, r) = rng.nextInt
    (a.toDouble, r)
  }

  def boolean(rng: Rng): (Boolean, Rng) = {
    val (a, r) = rng.nextInt
    if (a/2 == 1) (true, r) else (false, r)
  }

  def ints(count: Int)(rng: Rng): (List[Int], Rng) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val (c, r) = rng.nextInt
      val (cs, r2) = ints(count - 1)(r)
      (c :: cs, r2)
    }
  }

  def intsTail(count: Int)(rng: Rng): (List[Int], Rng) = {
    @tailrec
    def go(count: Int, list:List[Int], rng: Rng): (List[Int], Rng) ={
      if (count == 0) {
        (list, rng)
      } else {
        val (c, r) = rng.nextInt
        go(count - 1, c :: list, r)
      }
    }

    go(count, Nil, rng)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (n, newRng) = s(rng)
      (f(n), newRng)
    }

  def double2(rng: Rng): Rand[Double] = {
    map(rngp=>rngp.nextInt)(_.toDouble)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f:(A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def intDouble(rng: Rng):((Int, Double), Rng) = {
    both(irng=>irng.nextInt, double2(rng))(rng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def go(la: List[A], lr: List[Rand[A]], rng:Rng): (List[A], Rng) =
      fs match {
        case Nil => (Nil, rng)
        case head::tail =>
          val (a, nrng) = head(rng)
          go(a::la, tail, nrng)
      }

    rngp => {
      go(Nil, fs, rngp)
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft((rngp:Rng) => (List.empty[A], rngp))((acc, f) =>map2(acc,f)((x, y) => y::x))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (x, r) = f(rng)
      g(x)(r)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => (r =>(f(x), r)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f:(A,B) => C): Rand[C] =
    flatMap(ra)(x => map(rb)(y => f(x, y)))
}

class SimpleRng(seed: Long) extends Rng {
  def nextInt: (Int, Rng) = {
    Random.setSeed(seed)
    val newInt = Random.nextInt()
    val nextRng = new SimpleRng(seed + newInt)
    (newInt, nextRng)
  }
}

