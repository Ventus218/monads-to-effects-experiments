package main

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future as JFuture
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object MyFuture:
  opaque type MyFuture[A] = MyFutureImpl[A]

  private enum MyFutureImpl[A]:
    case One(task: () => A)
    case Many[B](tasks: List[() => B]) extends MyFuture[List[B]]
    case FlatMap[A, B](fut: MyFuture[A], f: A => MyFuture[B])
        extends MyFuture[B]
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
