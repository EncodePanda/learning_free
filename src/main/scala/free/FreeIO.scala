package free

import scalaz._
import Scalaz._


trait IO[A]
case class PrintLine(str: String) extends IO[Unit]
object GetLine extends IO[String]

object IO {

  type DSL[A] = Free[IO, A]

  def printLine(str: String): DSL[Unit] = Free.liftF(PrintLine(str))
  def getLine: DSL[String] = Free.liftF(GetLine)

}

object FreeIO {
  import IO._
  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable.Stack

  val program1: DSL[Unit] = printLine("Hello World!")

  val program2: DSL[String] = for {
    _ <- printLine("Tell me your first name")
    first <- getLine
    _ <- printLine("Tell me your last name")
    last <- getLine
    _ <- printLine(s"Your name is $first $last")
  } yield first + " " + last
}

object Main extends App {
  import FreeIO._

  val consoleInterpeter: IO ~> Id =
    new (IO ~> Id) {
      def apply[A](io: IO[A]): Id[A] = io match {
        case PrintLine(str) => println(str)
        case GetLine => scala.io.StdIn.readLine()
      }
    }

  program1.foldMap(consoleInterpeter)
  program2.foldMap(consoleInterpeter)
}

object Tests extends App {
  import FreeIO._

  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable.Stack

  def listInterpreter(input: Stack[String], output: ListBuffer[String]): IO ~> Id =
    new (IO ~> Id) {
      def apply[A](io: IO[A]): Id[A] = io match {
        case PrintLine(str) =>
          output += str
          ()
        case GetLine => input.pop
      }
    }

  var input = new Stack[String]()
  input.push("pawel")
  input.push("szulc")
  input = input.reverse
  val output = new ListBuffer[String]()

  program2.foldMap(listInterpreter(input, output))

  if(output != ListBuffer("Tell me your first name", "Tell me your last name", "Your name is pawel szulc")) {
    throw new RuntimeException(output.toString)
  }
}

