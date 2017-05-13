package my.work

/**
  * Created by jia on 7/24/2016.
  */
sealed trait ServiceState
final class Started extends ServiceState
final class Stopped extends ServiceState
