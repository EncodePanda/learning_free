package free

import scalaz._
import Scalaz._

trait IO[A]

object IO {

  case class PrintLine(str: String) extends IO[Unit]
  object GetLine extends IO[String]

  class Ops[S[_]](implicit s0 : IO :<: S) {
    def printLine(str: String): Free[S, Unit] = Free.liftF(s0.inj(PrintLine(str)))
    def getLine: Free[S, String] = Free.liftF(s0.inj(GetLine))
  }
}

object IOConsoleInterpreter extends (IO ~> Id) {
  def apply[A](io: IO[A]): Id[A] = io match {
    case IO.PrintLine(str) => println(str)
    case IO.GetLine => scala.io.StdIn.readLine()
  }
}

object FreeIO extends App {

  val ops = new IO.Ops[IO]
  import ops._

  val program: Free[IO, String] = for {
    _ <- printLine("Tell me your first name")
    first <- getLine
    _ <- printLine("Tell me your last name")
    last <- getLine
    _ <- printLine(s"Your name is $first $last")
  } yield first + " " + last

  program.foldMap(IOConsoleInterpreter)
}
