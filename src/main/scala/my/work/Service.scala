package my.work

/**
  * Created by jia on 7/24/2016.
  */
class Service[State <: ServiceState] private () {
  def start[T >: State <: Stopped]() = this.asInstanceOf[Service[Started]]
  def stop[T >: State <: Started]() = this.asInstanceOf[Service[Stopped]]
}
object Service {
  def create() = new Service[Stopped]
}