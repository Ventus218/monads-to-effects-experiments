package monadtransformers

import cats.data.State
import cats.data.StateT
import cats.Monad
import cps.*

given Monad[Option] with
  def pure[A](x: A): Option[A] = Option(x)
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
    f(a) match
      case None               => None
      case Some(Left(value))  => tailRecM(value)(f)
      case Some(Right(value)) => Some(value)

given [S, F[_]: Monad]: CpsMonad[[A] =>> StateT[F, S, A]]
  with CpsPureMonadInstanceContext[[A] =>> StateT[F, S, A]]
  with
  def flatMap[A, B](fa: StateT[F, S, A])(
      f: A => StateT[F, S, B]
  ): StateT[F, S, B] = fa.flatMap(f)
  def map[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] = fa.map(f)
  def pure[T](t: T): StateT[F, S, T] = StateT.pure(t)

@main
def test: Unit =
  var computation =
    for
      _ <- StateT.modify[Option, Int](_ + 1)
      _ <- StateT.modifyF[Option, Int](a =>
        if a % 2 == 0 then Some(a) else None
      )
      _ <- StateT.modify[Option, Int](_ + 1)
    yield ()
  println:
    computation.run(0)
  println:
    computation.run(1)

  computation = async[[A] =>> StateT[Option, Int, A]] {
    StateT.modify[Option, Int](_ + 1).await
    StateT.modifyF[Option, Int](a => if a % 2 == 0 then Some(a) else None).await
    StateT.modify[Option, Int](_ + 1).await
  }
  println:
    computation.run(0)
  println:
    computation.run(1)
