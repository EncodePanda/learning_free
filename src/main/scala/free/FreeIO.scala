package free

import EnrichNTOps._
import scalaz._
import Scalaz._

object Full extends App {

  type Eff[A] = Coproduct[IO, Logging, A]

  def program[S[_]](implicit s0: IO :<: S, s1: Logging :<: S): Free[S, String] = {
    val I = new IO.Ops[S]
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
 
  val interpret: Eff ~> Id = IOConsoleInterpreter :+: LoggingConsoleInterpreter
  
  program[Eff].foldMap(interpret)
}





