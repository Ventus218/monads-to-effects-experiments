package future

import cps.*
import scala.util.Try
import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration.Duration
/* Monadless should allow this due to the need of methods to be just source compatible */
// given CpsMonad[Future] with CpsPureMonadInstanceContext[Future] with
//   override def pure[T](t: T): Future[T] = Future(t)
//   override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
//   override def flatMap[A, B](fa: Future[A])(
//       f: A => Future[B]
//   ): Future[B] = fa.flatMap(f)

given (using
    ec: ExecutionContext
): CpsTryMonad[Future] with CpsTryMonadInstanceContext[Future] with

  override def pure[T](t: T): Future[T] = Future(t)

  override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

  override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
    fa.flatMap(f)

  override def error[A](e: Throwable): Future[A] = Future.failed(e)

  override def flatMapTry[A, B](fa: Future[A])(
      f: Try[A] => Future[B]
  ): Future[B] = fa.transformWith(f(_))

@main
def test: Unit =
  given ExecutionContext = scala.concurrent.ExecutionContext.global
  val computation2 = async[Future] {
    val a = Future(println("a")).await
    val b = Future(println("b")).await
    Future
      .sequence(
        Set(
          Future(println("c")),
          Future(println("d")),
          Future(println("e")),
          Future(println("f")),
          Future(println("g")),
          Future(println("h")),
          Future(println("i"))
        )
      )
      .await
    val j = Future(println("j")).await
  }
  Await.ready(computation2, Duration(1, "s"))
