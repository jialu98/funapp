package my.work

import scala.language.dynamics
/**
  * Created by jia on 7/27/2016.
  */
class Json(s:String) extends Dynamic{
  def selectDynamic(name:String): Option[String] = parse(s)
  def parse(ss:String) = {Option(ss)}
}
