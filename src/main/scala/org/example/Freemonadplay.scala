package org.example

import cats.arrow.FunctionK
import cats.Id
import cats.free.Free
import cats.free.Free.liftF
import scala.io.StdIn
import cats.effect.IO

/**
  * WWhat do we need to make a Free monad algebra
  * a free monad type for ActionA
  * a set of smart constructors for building free monad instances
  * one or several interpreters that can actually run the program
  */

object Freemonadplay extends App {

  // A stands for Algebra
  // Note that actions are just data
  trait ActionA[A]

  case class ReadAction() extends ActionA[String]

  case class WriteAction(output: String) extends ActionA[Unit]

  // Define the Free type for our algebra
  type ActionF[A] = Free[ActionA, A]

  // Smart constructors for the actions
  def read(): ActionF[String] =
    liftF[ActionA, String](ReadAction())

  def write(output: String): ActionF[Unit] =
    liftF[ActionA, Unit](WriteAction(output))

  // You can now write a program
  val program1 = for (
    _ <- write("Hello. What is your name?: ");
    name <- read();
    _ <- write(s"Nice to meet you, $name\n")
  ) yield ()
  // Type Free[ActionA, Unit]

  // And an interpreter is next, using Id
  val idInterpreter: FunctionK[ActionA, Id] = new FunctionK[ActionA, Id] {
    override def apply[A](action: ActionA[A]): Id[A] = action match {
      case ReadAction() =>
        StdIn.readLine().asInstanceOf[Id[A]]
      case WriteAction(out: String) =>
        print(out).asInstanceOf[Id[A]]
    }
  }

  // Run with foldMap
  //val result = program1.foldMap(idInterpreter)

  // An IO interpreter

  def ioInterpreter: FunctionK[ActionA, IO] = new FunctionK[ActionA, IO] {
    def apply[A](action: ActionA[A]): IO[A] = {
      action match {
        case ReadAction() =>
          IO(StdIn.readLine()).asInstanceOf[IO[A]]
        case WriteAction(out: String) =>
          IO(print(out)).asInstanceOf[IO[A]]
      }
    }
  }

  // val result = program1.foldMap(ioInterpreter)
  // result.unsafeRunSync()

  // A TEST interpreter. Here the interactive ReadAction just pulls from a list of strings...

  def testInterpreter(inputs: List[String]): FunctionK[ActionA, Id] = {
    var consumedInputs = inputs

    new FunctionK[ActionA, Id] {
      override def apply[A](action: ActionA[A]): Id[A] = action match {
        case ReadAction() => {
          val next = consumedInputs.head // throws if you run out of strings
          consumedInputs = consumedInputs.tail
          next.asInstanceOf[Id[A]]
        }
        case WriteAction(out: String) =>
          print(out).asInstanceOf[Id[A]]
      }
    }
  }

  // Note interpreter is mutable, not a great idea but you get the point
  val interpreter = testInterpreter(List("Justin", "Nero"))
  program1.foldMap(interpreter)
  program1.foldMap(interpreter)
}
