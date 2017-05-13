package my.work.finagle

import scala.concurrent.Future

/**
  * Created by jia on 7/27/2016.
  */
trait Service[-Req, +Rep] extends (Req => Future[Rep])
