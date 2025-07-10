package main

import cps.*

given CpsMonad[Option] with CpsPureMonadInstanceContext[Option] with
  def pure[T](t: T): Option[T] = Option(t)
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

@main
def testOption: Unit =
  println:
    async[Option] {
      val a = await(Option(1))
      // val b = await(Option.empty[Int])
      val b = await(Option(3))
      a + b
    }

import MyFuture.*
import java.util.concurrent.Executors
given CpsMonad[MyFuture] with CpsPureMonadInstanceContext[MyFuture] with
  override def pure[T](t: T): MyFuture[T] = MyFuture(t)
  override def map[A, B](fa: MyFuture[A])(f: A => B): MyFuture[B] = fa.map(f)
  override def flatMap[A, B](fa: MyFuture[A])(
      f: A => MyFuture[B]
  ): MyFuture[B] = fa.flatMap(f)

@main
def testMyFuture: Unit =
  val computation = for
    a <- MyFuture(println("a"))
    b <- MyFuture(println("b"))
    many <- MyFuture.concurrently(
      () => println("c"),
      () => println("d"),
      () => println("e"),
      () => println("f"),
      () => println("g"),
      () => println("h"),
      () => println("i")
    )
    j <- MyFuture(println("j"))
  yield (many.map(_ => 1))
  val exec = Executors.newFixedThreadPool(2)
  computation.run(exec)

  Thread.sleep(200)
  println()
  println()

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
  computation2.run(exec)
  exec.close()
