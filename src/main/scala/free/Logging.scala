package free

import scalaz._
import Scalaz._

trait Logging[A]
case class Info(str: String) extends Logging[Unit]
case class Error(str: String) extends Logging[Unit]

class LoggingFrees[F[_]](wrap: Logging ~> F) extends Services[Logging, F](wrap) {
  def info(str: String): Free[F, Unit] = createFree(Info(str))
  def error(str: String): Free[F, Unit] = createFree(Error(str))
}

object Logging extends LoggingFrees[Logging](new NoWrap[Logging])
class Loggings[F[_]](implicit I : Inject[Logging, F]) extends LoggingFrees[F](new InjectWrap[Logging, F])

object LoggingConsoleInterpreter extends (Logging ~> Id) {
  def apply[A](log: Logging[A]): Id[A] = log match {
    case Info(str) => println(s"Info: $str")
    case Error(str) => println(s"Error: $str")
  }
}

object FreeLogging extends App {

  import Logging._

  val program: Free[Logging, Unit] = for {
    _ <- info("program started")
    _ <- error("program failed")
  } yield ()

  program.foldMap(LoggingConsoleInterpreter)
}
