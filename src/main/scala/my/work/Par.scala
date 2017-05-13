package my.work

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

/**
  * Created by jia on 4/19/2017.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]
  def run[A] (es: ExecutorService)(a: Par[A]): Future[A] = a(es)
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
      val a1 = a(es).get
      val b1 = b(es).get
      UnitFuture(f(a1, b1))
    }
  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(new Callable[A]{
      def call = a(es).get
    })
  }

  def lazyUnit[A](a: A): Par[A]  = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => {
    lazyUnit(f(a))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]]  = {
    ps match {
      case Nil => unit(Nil)
      case head::tail => map2(head, fork(sequence(tail)))(_::_)
    }
  }

  def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]]  = {
    ps.foldLeft(unit(List.empty[A]))((pl, p) => map2(p, pl)(_::_))
  }

  def map[A, B](p: Par[A])(f: A => B): Par[B] =
    map2(p, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork{
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs =as.map{asyncF{a => if (f(a)) List(a) else Nil}}
    map(sequence(fbs))(_.flatten)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    choices(n(es).get())(es)
  }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    choices(key(es).get())(es)
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    choices(pa(es).get)(es)
  }

  def choiceNUsingChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(x => choices(x))

  def choiceMapUsingChooser[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(x => choices(x))

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    choices(pa(es).get)(es)
  }

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    a(es).get()(es)
  }

  def joinUsingFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x=>x)

  def flatMapUsingJoin[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    val v = map(pa)(choices)
    join(v)
  }

  def map2UsingFlatMap[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    flatMap(a)(x => map(b)(y => f(x, y)))
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(run: Boolean): Boolean = false
  }

}
