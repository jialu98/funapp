package my.work

/**
  * Created by jia on 7/20/2016.
  */
trait Container[M[A]] {
  def put[A](x:A):M[A]
  def get[A](m:M[A]):A
}
