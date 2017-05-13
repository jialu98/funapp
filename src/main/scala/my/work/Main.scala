package my.work

import my.work.WeekDay.WeekDay
import my.work.WeekDay.Sat
import my.work.WeekDay.Sun
import my.work.random.{Rng, SimpleRng}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Either

/**
  * Created by jia on 7/17/2016.
  */
object Main extends App {

  def positive(i: Int): Either[String, Int] = {
    if (i>0) Right(i) else Left("abc")
  }

  @tailrec
  def continue(conditional: => Boolean)(body: =>Unit): Unit = {
    if (conditional) {
      body
      body
      body
      continue(conditional)(body)
    }
  }

  var count = 0
  continue(count < 5){
    println(s"at $count")
    count+=1
  }

  val pair = (List(100,200,300), List(1,2,3))
  val vv = pair.invert.map{case(x, y) => x - y}

  println("-------------------")


  val listContainer = new Container[List] {
    def put[A](x: A) = List(x)

    def get[A](m: List[A]) = m.head
  }
  implicit val optionContainer = new Container[Some] {
    def put[A](x: A) = Some(x)

    def get[A](s: Some[A]) = s.get
  }
  val myMatch2 = new MyMatch[Some, Int, Int]
  val aaa = myMatch2.tuplize(optionContainer, Some(1), Some(2))
  println(aaa)
  myMatch2.test(Example)

  def isWorkDay(d: WeekDay) = {
    !(d == Sun || d == Sat)
  }

  WeekDay.values filter isWorkDay foreach println

  val initStopped = Service.create()
  val started = initStopped.start()
  val stopped = started.stop()
  val started2 = stopped.start()
  val stopped2 = started2.stop()
  //initStopped.stop()
  //stopped.stop()
  val jsonString =
  """
  {
    "name": "Konrad",
    "favLangs": ["Scala", "Go", "SML"]
  }"""

  val json = new Json(jsonString)
  val name = json.name
  println(name)
  val list = List(1, 2, 3)
  val set = Set(1, 2, 3)
  val map = Map("a" -> 1, "b" -> 2)

  val button = new Button("b1") with Subject[Button] {
    override def click(): Unit = {
      super.click()
      notifyObservers(this)
    }
  }

  val button2 = new Button("b2") with ObservableClicks

  val button3 = new ClickCountObserver

  val op:Opt[Parent] = new Opt[Parent](new Parent(1))
  val p = op.getOrElse("abc")
  println(p)

  def helloWord(s: String) = SimpleVm(s, println)

  def intVm(i: Int) = SimpleVm(i, println)

  def compileRun(vm : Vm[A forSome {type A}]): Unit = {
    vm.run(vm.compile)
  }

  //compileRun(helloWord("a"))
  //compileRun(intVm(1))

  def compileRun2[A](vm : Vm[A]): Unit = {
    vm.run(vm.compile)
  }

  compileRun2(helloWord("a"))
  compileRun2(intVm(1))

  def compileRun3(vm : Vm[A] forSome{type A}): Unit = {
    //vm.run(vm.compile)
  }

  compileRun3(helloWord("a"))
  compileRun3(intVm(1))

  val p2 = new PolymorphicMethod
  val h:String = p2.foo(List("a"))
  val h2:Int = p2.foo(List(1))

  def double(seq: Seq[_]): Seq[Int] = seq match {
    case Nil => Nil
    case head +: tail => (toInt(head) * 2) +: double(tail)
  }

  def toInt(x:Any): Int = x match {
    case i: Int => i
    case s: String => s.toInt
    case x => throw new RuntimeException(s"error $x")
  }
  println("-----------")
  val d5 = double(Seq(1,23))
  val d6 = double(Seq("10","80"))
  println(d5)
  println(d6)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case ::(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case ::(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def length(as: List[Int]): Int = {
    foldRight(as, 0){(a, b) => 1 + b}
  }

  def sum(as: List[Int]): Int ={
    foldLeft(as, 0)(_ + _)
  }

  def append[A](a: List[A], b: List[A]): List[A] = {
    foldRight(a, b){(x:A, y:List[A]) => x::y}
  }

  def flatten[A](a: List[List[A]]): List[A] = {
    foldRight(a, List.empty[A]){(x:List[A], y:List[A]) => foldRight(x, y){(n:A, m:List[A]) => n::m}}
  }

  def add(as: List[Int], num: Int): List[Int] = {
    foldRight(as, Nil: List[Int]){(x: Int, y: List[Int]) => (1 + x)::y}
  }

  def map[A, B](as: List[A], f: A=>B): List[B] = {
    foldRight(as, Nil: List[B]){(x: A, y: List[B]) => f(x)::y}
  }

  def map_2[A, B](as: List[A], f: A=>B): List[B] = {
    val buff = ListBuffer.empty[B]
    foldLeft(as, buff){(tail: ListBuffer[B], head: A) => tail+=f(head)}
    buff.toList
  }
  def filter[A](as: List[A], f: A => Boolean): List[A] = {
    val buff = ListBuffer.empty[A]
    foldLeft(as, buff){(tail: ListBuffer[A], head: A) => if (f(head)) tail += head else tail}
    buff.toList
  }

  def flatMap[A, B](as: List[A], f: A=>List[B]): List[B] = {
    foldRight(as, Nil: List[B]) { (x: A, y: List[B]) => append(f(x), y) }
  }

  def filter_2[A](as: List[A], f: A => Boolean): List[A] = {
    flatMap(as, (x:A )=> if (f(x)) List(x) else Nil)
  }

  def zip(left: List[Int], right: List[Int]): List[Int] = {
    val buff = ListBuffer.empty[Int]
    var wkRight = right
    foldLeft(left, buff){(tail: ListBuffer[Int], head: Int) =>
      tail+= head + wkRight.head
      wkRight = wkRight.tail
      tail}
    buff.toList
  }
  def zip[A, B, C](left: List[A], right: List[B], f: (A, B) => C): List[C] = {
    val buff = ListBuffer.empty[C]
    var wkRight = right
    foldLeft(left, buff){(tail: ListBuffer[C], head: A) =>
      tail += f(head, wkRight.head)
      wkRight = wkRight.tail
      tail}
    buff.toList
  }
  def zip_2[A, B, C](left: List[A], right: List[B], f: (A, B) => C): List[C] = (left, right) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (lh::lt, rh::rt) => ::(f(lh, rh), zip_2(lt, rt, f))
  }

  @tailrec
  def hasSubSeq[A](left: List[A], right: List[A]): Boolean = left match {
    case Nil => right == Nil
    case _ => if (startsWith(left, right))  true else hasSubSeq(left.tail, right)
  }

  @tailrec
  def startsWith[A](left: List[A], right: List[A]): Boolean = (left, right) match {
    case (_, Nil) => true
    case (lh:: lt, rh::rt) if (lh == rh) => startsWith(lt, rt)
    case _ => false
  }

  def size[A](t: Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(lt, rt) => 1 + size(lt) + size(rt)
  }

  def maximum(t: Tree[Int]):Int = t match {
    case Leaf(v) => v
    case Branch(lt, rt) => maximum(lt).max(maximum((rt)))
  }

  def depth[A](t: Tree[A]):Int = t match {
    case Leaf(_) => 0
    case Branch(lt, rt) => 1 + (depth(lt) max depth(rt))
  }

  def map2[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(lt, rt) => Branch(map2(lt, f), map2(rt, f))
  }

  def fold[A, B](t: Tree[A])(f: (A) => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(lt, rt) => g(fold(lt)(f)(g), fold(rt)(f)(g))
  }

  def size_2[A](t: Tree[A]):Int = {
    fold(t)(x => 1)(1+ _ + _)
  }

  def maximum_2(t: Tree[Int]):Int ={
    fold(t)(x => x)(_ max _)
  }

  def depth2[A](t: Tree[A]):Int = {
    fold(t)(x => 0)((y, z) => 1 + (y max z))
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
    fold(t)((a:A) => Leaf(f(a)):Tree[B])(Branch(_, _))}

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] ={
    val aa = a.flatMap{x=>b.map(y=> f(x, y))}
    for(x<-a; y<-b) yield f(x, y)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Option(Nil)
    case head::tail => head.flatMap(x => sequence(tail).map(y => x::y))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Option(Nil)
    case head::tail => f(head).flatMap(x => traverse(tail)(f).map(y => x::y))
  }

  def map3[A, B, E](mayBe: Either[E, A])(f: A => B): Either[E, B] = mayBe match {
    case Left(a)  => Left(a)
    case Right(b) => Right(f(b))
  }

  def flatMap3[A, B, E](mayBe: Either[E, A])(f: A => Either[E, B]): Either[E, B] = mayBe match {
    case Left(a)  => Left(a)
    case Right(b) => f(b)
  }

  def orElse3[A, B>:A, E](mayBe: Either[E, A])(f: => Either[E, B]): Either[E, B] = mayBe match {
    case Left(a)  => f
    case Right(b) => Right(b)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case head::tail => head.right.flatMap{h => sequence(tail).right.map(t => h::t)}
  }

  def traverse3[A, B, E](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case head :: tail => f(head).right.flatMap{h => traverse3(tail)(f).right.map{t => h::t}}
  }

  val simpleRng = new SimpleRng(100)
  val va = simpleRng.nextInt
  println(va._1)
  val va2 = simpleRng.nextInt
  println(va2._1)
  val va3 = va._2.nextInt
  println(va3._1)

  val st = (0 to 100).toStream
  val rr = st.takeWhile{a =>
    println("stream " + a)
    a < 10}
  .foldLeft(0){(b, a) =>
    println("stream-- " + a)
    b + a * 100
  }
  println(s"rr - $rr")

  val ppp = new Person("john", "Smith")
  println(ppp.say("hi,"))

  val ppp2 = new Person()
  println(ppp2.say("hi,"))
}

