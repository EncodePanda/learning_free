package free

import scalaz.{~>, :<:, Free}

class Services[S[_], F[_]](wrap: S ~> F) {

  def createFree[A](value: S[A]): Free[F, A] = Free.liftF(wrap(value))
}

class NoWrap[S[_]] extends (S ~> S) {

  override def apply[A](value: S[A]): S[A] = value
}

class InjectWrap[S[_], F[_]](implicit I: S :<: F) extends (S ~> F) {

  override def apply[A](value: S[A]): F[A] = I.inj(value)
}
