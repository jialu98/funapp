package my.work

/**
  * Created by jia on 4/2/2017.
  */
class VmService[A] {

  def compileRun(vm : Vm[A]): Unit = {
    val s = vm.compile
    vm.run(s)
  }
}
