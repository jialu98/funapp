package my.work

import my.work.stateAction.State
import my.work.stateAction.State._
/**
  * Created by jia on 4/16/2017.
  */

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {

  def update(input: Input, machine: Machine): Machine =
    (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Coin, Machine(true, candies, coins)) =>
        Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) =>
        Machine(false, candies - 1, coins)
      case _ =>
        machine
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modify[Machine](update(input, _))))
    s <- get[Machine]
  } yield (s.candies, s.coins)

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val f: Machine => Machine = m => {
      inputs.foldLeft(m)((machine, input) => update(input, machine))
    }
    State(m => {
      val m1 = f(m)
      ((m1.candies, m1.coins), m1)
    })
  }
}