package my.work

/**
  * Created by jia on 7/26/2016.
  */
trait GenericCategory [zz[_,_]]{
  def id[A]:A zz A
}
