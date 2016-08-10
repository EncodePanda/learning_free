package free

import scalaz._
import Scalaz._

trait IO[A]
case class PrintLine(str: String) extends IO[Unit]
object GetLine extends IO[String]

class IOFrees[F[_]](wrap: IO ~> F) extends Services[IO, F](wrap) {
  def printLine(str: String): Free[F, Unit] = createFree(PrintLine(str))
  def getLine: Free[F, String] = createFree(GetLine)
}

object IO extends IOFrees[IO](new NoWrap[IO])
class IOs[F[_]](implicit I : Inject[IO, F]) extends IOFrees[F](new InjectWrap[IO, F])

object IOConsoleInterpreter extends (IO ~> Id) {
  def apply[A](io: IO[A]): Id[A] = io match {
    case PrintLine(str) => println(str)
    case GetLine => scala.io.StdIn.readLine()
  }
}

object FreeIO extends App {

  import IO._

  val program: Free[IO, String] = for {
    _ <- printLine("Tell me your first name")
    first <- getLine
    _ <- printLine("Tell me your last name")
    last <- getLine
    _ <- printLine(s"Your name is $first $last")
  } yield first + " " + last

  program.foldMap(IOConsoleInterpreter)
}
