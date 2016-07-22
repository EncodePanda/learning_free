package free

import scalaz._
import Scalaz._

trait Logging[A]
case class Info(str: String) extends Logging[Unit]
case class Error(str: String) extends Logging[Unit]

object Logging {
  def info(str: String): Free[Logging, Unit] = Free.liftF(Info(str))
  def error(str: String): Free[Logging, Unit] = Free.liftF(Error(str))
}

class Loggings[F[_]](implicit I : Inject[Logging, F]) {
  def info(str: String): Free[F, Unit] = Free.liftF(I.inj(Info(str)))
  def error(str: String): Free[F, Unit] = Free.liftF(I.inj(Error(str)))
}

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
