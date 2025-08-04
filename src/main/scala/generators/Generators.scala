package generators

class Generator[A] extends Iterator[A] {
  override def next(): A = continuation.get.apply()

  override def hasNext: Boolean = continuation.isDefined

  var continuation: Option[() => A] = None

}
def yieldWithContinuation[A](a: A, c: () => Generator[A])(using
    g: Generator[A]
): Generator[A] =
  g.continuation = Some: () =>
    g.continuation = Some(() => c().next())
    a
  g
def yieldFinished[A](a: A)(using g: Generator[A]): Generator[A] =
  g.continuation = Some: () =>
    g.continuation = None
    a
  g

def generator[A](
    f: (Generator[A], Option[Generator[A]]) ?=> Generator[A]
): Gen[A] =
  given g: Generator[A] = summon[Option[Generator[A]]].getOrElse(Generator())
  given Option[Generator[A]] = Some(g)
  f

def backwardCounter(n: Int): Gen[Int] =
  generator:
    n match
      case 0 =>
        yieldFinished(0)
      case _ =>
        yieldWithContinuation(n, () => backwardCounter(n - 1))

object Generators extends App:

  for a <- backwardCounter(5) do println(a)

given [A]: Option[Generator[A]] = None
type Gen[A] = Option[Generator[A]] ?=> Generator[A]
