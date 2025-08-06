package generators

object Generators4:

  enum NextResult[A]:
    case Return(value: Option[A])
    case Yield(value: A)
  import NextResult.*

  opaque type GeneratorInstance[A] = GeneratorImpl[A]
  private class GeneratorImpl[A]:

    def next(): NextResult[A] =
      continuation()
      nextResult
    var nextResult: NextResult[A] =
      Return(None) // This default value is irrelevant
    var continuation: () => Unit = () => ()

  extension [A](g: GeneratorInstance[A])
    def computeNext() = g.next()

    def foreach(f: (A) => Unit): Unit =
      g.next() match
        case Return(Some(value)) => f(value)
        case Return(None)        => ()
        case Yield(value) =>
          f(value)
          foreach(f)

  def _yield[A](a: A, continueWith: => GeneratorInstance[A])(using
      g: GeneratorInstance[A]
  ): GeneratorInstance[A] =
    continueWith
    g.nextResult = Yield(a)
    g

  private def _returnImpl[A](a: Option[A])(using
      g: GeneratorInstance[A]
  ): GeneratorInstance[A] =
    g.nextResult = Return(a)
    g.continuation = () => ()
    g

  def _return[A](a: A)(using
      g: GeneratorInstance[A]
  ): GeneratorInstance[A] = _returnImpl(Some(a))

  def _return[A](using g: GeneratorInstance[A]): GeneratorInstance[A] =
    _returnImpl(None)

  /** This type allows to hide the context parameter in the return type of
    * generator functions
    */
  type Generator[A] =
    Option[GeneratorInstance[A]] ?=> GeneratorInstance[A]

  /** This given instance allows the user to not explicitly set a None as the
    * initial Generator of the recursive generator function
    */
  given [A]: Option[GeneratorInstance[A]] = None

  def generator[A](
      f: (
          GeneratorInstance[A],
          Option[GeneratorInstance[A]]
      ) ?=> GeneratorInstance[A]
  ): Generator[A] =
    val g = summon[Option[GeneratorInstance[A]]].getOrElse(GeneratorImpl())
    g.continuation = () => f(using g, Some(g))
    g

object TestGenerators4 extends App:
  import Generators4.*

  def backwardCounter(n: Int): Generator[Int] =
    // generator will create a GeneratorInstance only if there is no
    // one in scope otherwise it will use the one he found.
    // This means that N recursive calls to generator won't
    // instantiate N GeneratorInstances but just 1.
    generator:
      println(s"Run with param: $n")
      n match
        case 0 =>
          _return
        // _return(0)
        case _ =>
          _yield(n, backwardCounter(n - 1))
        // _yield(n, continueWith = backwardCounter(n - 1))

  // Checking that a Generator can produce no values
  for a <- backwardCounter(0) do println(a)

  // Checking that a Generator can produce multiple values
  for a <- backwardCounter(5) do println(a)
