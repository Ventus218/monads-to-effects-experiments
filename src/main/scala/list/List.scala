package list

import cps.*

given CpsMonad[List] with CpsPureMonadInstanceContext[List] with
  def pure[T](t: T): List[T] = List(t)
  def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

@main
def test: Unit =
  println:
    async[List] {
      val a = List("1", "2", "3").await
      val b = List("1", "2", "3").await
      a + b
    }
