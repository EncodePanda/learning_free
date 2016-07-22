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


trait Logging[A]
case class Info(str: String) extends Logging[Unit]
case class Error(str: String) extends Logging[Unit]

object Logging {
  def info(str: String): Free[Logging, Unit] = Free.liftF(Info(str))
  def error(str: String): Free[Logging, Unit] = Free.liftF(Error(str))
}

object FreeLogging extends App {

  import Logging._

  val consoleLogInterp: Logging ~> Id =
    new (Logging ~> Id) {
      def apply[A](log: Logging[A]): Id[A] = log match {
        case Info(str) => println(s"Info: $str")
        case Error(str) => println(s"Error: $str")
      }
    }

  val program: Free[Logging, Unit] = for {
    _ <- info("program started")
    _ <- error("program failed")
  } yield ()

  program.foldMap(consoleLogInterp)
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

