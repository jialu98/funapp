package my.work

/**
  * Created by jia on 3/22/2017.
  */
trait Observer[S] {
  def receiveUpdate(state: S): Unit
}

trait Subject[S] {
  private var observers: List[Observer[S]] = Nil

  def addObserver(observer: Observer[S]): Unit = {
    observers ::= observer
  }

  def notifyObservers(state: S): Unit = {
    observers.foreach(_.receiveUpdate(state))
  }
}

trait ObservableClicks extends Clickable with Subject[Clickable] {
  override def click(): Unit = {
    super.click()
    notifyObservers(this)
  }
}

trait Clickable {
  def click(): Unit = updateUI()
  def updateUI(): Unit
}

class Button(name: String) extends Clickable {
  def updateUI(): Unit = println("update ui")
}

class ClickCountObserver extends Observer[Clickable] {
  var count = 0
  def receiveUpdate(state: Clickable): Unit = count += 1
}

