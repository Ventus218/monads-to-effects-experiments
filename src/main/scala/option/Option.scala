package option

import cps.*

given CpsMonad[Option] with CpsPureMonadInstanceContext[Option] with
  def pure[T](t: T): Option[T] = Option(t)
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

@main
def test: Unit =
  println:
    async[Option] {
      val a = await(Option(1))
      // val b = await(Option.empty[Int])
      val b = await(Option(3))
      a + b
    }
