package my.work.finagle

import scala.concurrent.Future

/**
  * Created by jia on 7/27/2016.
  */
trait Filter[-ReqIn, +RepOut, +ReqOut, -RepIn] extends ((ReqIn, Service[ReqOut,RepIn]) =>Future[RepOut]){
  def andThen[Req2, Rep2](next: Filter[ReqOut, RepIn, Req2, Rep2]) = {
  }
}
