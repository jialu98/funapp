package my.work.stateAction

/**
  * Created by jia on 4/16/2017.
  */
case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a1, s1) = run(s)
      f(a1).run(s1)
    })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](s:State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => s.map(b =>f(a, b)))

}

object State {

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ =>((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def modify2[S](f: S => S): State[S, Unit] = get.flatMap{s =>set(f(s))}


  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sts: Seq[State[S, A]]): State[S, List[A]] =
    sts.foldLeft(unit[S, List[A]](List.empty[A]))((acc, st) => acc.map2(st)((seq:List[A], a) => seq:+a))
}
