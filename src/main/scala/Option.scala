import cps.*
import scala.concurrent.Future

given CpsMonad[Option] with CpsPureMonadInstanceContext[Option] with
  def pure[T](t: T): Option[T] = Option(t)
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

@main
def main: Unit =
  val noneMap = Option.empty[Int].map(a => a + 5)
  val noneFlatMap = Option.empty[Int].flatMap(a => Some(a + 5))

  val someMap = Option(3).map(a => a + 5)
  val someFlatMap = Option(3).flatMap(a => Some(a + 5))

  println:
    async[Option] {
      val a = await(Option(1))
      // val b = await(Option.empty[Int])
      val b = await(Option(3))
      a + b
    }
