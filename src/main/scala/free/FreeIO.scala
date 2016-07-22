package free

import scalaz._
import Scalaz._


trait IO[A]
case class PrintLine(str: String) extends IO[Unit]
object GetLine extends IO[String]

object IO {

  type DSL[A] = Free[IO, A]

  def printLine(str: String): Free[IO, Unit] = Free.liftF(PrintLine(str))
  def getLine: Free[IO, String] = Free.liftF(GetLine)

}


trait Logging[A]
case class Info(str: String) extends Logging[Unit]
case class Error(str: String) extends Logging[Unit]

object Logging {
  def info(str: String): Free[Logging, Unit] = Free.liftF(Info(str))
  def error(str: String): Free[Logging, Unit] = Free.liftF(Error(str))
}

class IOs[F[_]](implicit I : Inject[IO, F]) {

  def printLine(str: String): Free[F, Unit] = Free.liftF(I.inj(PrintLine(str)))
  def getLine: Free[F, String] = Free.liftF(I.inj(GetLine))

}

class Loggings[F[_]](implicit I : Inject[Logging, F]) {

  def info(str: String): Free[F, Unit] = Free.liftF(I.inj(Info(str)))
  def error(str: String): Free[F, Unit] = Free.liftF(I.inj(Error(str)))

}


object Full extends App {


  /*
   :+: taken from Quasar Analytics
   */
  sealed abstract class :+:[F[_], G[_]] {
    type λ[A] = Coproduct[F, G, A]
  }

  implicit class EnrichNT[F[_], H[_]](f: F ~> H) {
    def :+:[G[_]](g: G ~> H): (G :+: F)#λ ~> H = new ((G :+: F)#λ ~> H) {
      def apply[A](fa: (G :+: F)#λ[A]) = fa.run.fold(g, f)
    }
  }

  type Appli[A] = Coproduct[IO, Logging, A]

  def program[S[_]](implicit s0: IO :<: S, s1: Logging :<: S): Free[S, String] = {
    val I = new IOs[S]
    val L = new Loggings[S]

    import I._
    import L._

    for {
      _ <- info("Asking for first and last name")
      _ <- printLine("Tell me your first name")
      first <- getLine
      _ <- printLine("Tell me your last name")
      last <- getLine
      _ <- printLine(s"Your name is $first $last")
      _ <- info(s"Recorded answer $first $last")
    } yield first + " " + last
  }
 
  val interpret: Appli ~> Id = Main.consoleInterpeter :+: FreeLogging.consoleLogInterp
  
  program[Appli].foldMap(interpret)
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

  val program1: Free[IO, Unit] = printLine("Hello World!")

  val program2: Free[IO, String] = for {
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

