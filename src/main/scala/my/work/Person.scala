package my.work

/**
  * Created by jia on 5/8/2017.
  */
class Person(firstName: String, lastName: String) {
  import Person._
  var name = fullName(firstName, lastName)
  var agename = firstName + " " + lastName
  def this() = this("Jack", "Smith")

  def say(word: String): String = word + " " + name
}

object Person {
  private def fullName(firstName: String, lastName: String): String = firstName + " " + lastName
}
