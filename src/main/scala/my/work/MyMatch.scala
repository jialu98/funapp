package my.work

/**
  * Created by jia on 7/17/2016.
  */
class MyMatch[M[_], A, B] {
  def bigger (any: Any):Any = any match {
      case i:Int if i < 0 => i - 1
      case i:Int if i > 0 => i + 1
      case i:String => i + " hello"
    }
  def tuplize[M[_]](c: Container[M], fst:M[A], snd:M[B]) = {
    //val c = implicitly[Container[M]]
    c.put(c.get(fst), c.get(snd))
  }
  def test(obj:Example.type ) = {

  }
}
