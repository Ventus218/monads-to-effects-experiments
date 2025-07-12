package future

import java.util.concurrent.{ExecutorService, Future as JFuture}
import scala.util.{Try, Failure, Success}

opaque type MyFuture[A] = MyFutureImpl[A]

private enum MyFutureImpl[A]:
  case One(task: () => A)
  case Many[B](tasks: List[() => B]) extends MyFutureImpl[List[B]]
  case FlatMap[A, B](fut: MyFuture[A], f: A => MyFutureImpl[B])
      extends MyFutureImpl[B]
import MyFutureImpl.*

object MyFuture:
  def apply[A](f: => A): MyFuture[A] = One(() => f)
  def concurrently[A](tasks: (() => A)*): MyFuture[List[A]] =
    Many(tasks.toList)

extension [A](f: MyFuture[A])
  def flatMap[B](next: A => MyFuture[B]): MyFuture[B] =
    FlatMap(f, next)

  def map[B](mapper: A => B): MyFuture[B] =
    f.flatMap(res => MyFuture(mapper(res)))

  def run(executor: ExecutorService): Try[A] =
    f match
      case One(task) =>
        Try(executor.submit(() => task()).get())
      case Many(tasks) =>
        Try(tasks.map(t => executor.submit(() => t())).map(_.get()))
      case FlatMap(fut, f) =>
        fut.run(executor) match
          case Failure(exception) => Failure(exception)
          case Success(value)     => f(value).run(executor)

import java.util.concurrent.Executors
import future.MyFuture
import cps.*

given CpsMonad[MyFuture] with CpsPureMonadInstanceContext[MyFuture] with
  override def pure[T](t: T): MyFuture[T] = MyFuture(t)
  override def map[A, B](fa: MyFuture[A])(f: A => B): MyFuture[B] = fa.map(f)
  override def flatMap[A, B](fa: MyFuture[A])(
      f: A => MyFuture[B]
  ): MyFuture[B] = fa.flatMap(f)

@main
def test: Unit =
  val computation2 = async[MyFuture] {
    val a = MyFuture(println("a")).await
    val b = MyFuture(println("b")).await
    val many = MyFuture
      .concurrently(
        () => println("c"),
        () => println("d"),
        () => println("e"),
        () => println("f"),
        () => println("g"),
        () => println("h"),
        () => println("i")
      )
      .await
    val j = MyFuture(println("j")).await
  }
  val exec = Executors.newFixedThreadPool(2)
  computation2.run(exec)
  exec.close()
