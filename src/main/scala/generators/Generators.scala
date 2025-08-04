package generators

object Generators:
  opaque type GeneratorInstance[A] <: Iterator[A] = GeneratorImpl[A]
  private class GeneratorImpl[A] extends Iterator[A]:

    /** Returns the computed value and removes it from the "cache"
      */
    override def next(): A =
      val res = nextVal.get
      nextVal = None
      res

    /** `hasNext` is actually producing the next value, is this conceptually
      * wrong?
      */
    override def hasNext: Boolean =
      nextVal = nextVal.orElse(computeNext())
      nextVal.isDefined

    /** Tries to produce the next value
      *
      * It is used by _yield to set a continuation as a side effect
      */
    var computeNext: () => Option[A] = () => None

    /** `nextVal` is used to memoize the value produced by `computeNext`
      */
    private var nextVal: Option[A] = None

  /** Lets a generator function produce a value while also specifying a
    * continuation
    */
  def _yield[A](a: A, continueWith: => GeneratorInstance[A])(using
      g: GeneratorInstance[A]
  ): GeneratorInstance[A] =
    g.computeNext = () =>
      g.computeNext = () =>
        val g = continueWith
        g.hasNext match
          case true  => Some(g.next())
          case false => None
      Some(a)
    g

  /** Marks the end of a generator function without producing any value
    */
  def _yield[A](using g: GeneratorInstance[A]): GeneratorInstance[A] =
    g.computeNext = () => None
    g

  /** This type allows to hide the context parameter in the return type of
    * generator functions
    */
  type Generator[A] = Option[GeneratorInstance[A]] ?=> GeneratorInstance[A]

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
    f(using g, Some(g))

object TestGenerators extends App:
  import Generators.*

  def backwardCounter(n: Int): Generator[Int] =
    println(s"Run with param: $n")
    // generator will create a GeneratorInstance only if there is no
    // one in scope otherwise it will use the one he found.
    // This means that N recursive calls to generator won't
    // instantiate N GeneratorInstances but just 1.
    generator:
      n match
        case 0 =>
          _yield
        case _ =>
          _yield(n, backwardCounter(n - 1))
        // _yield(n, continueWith = backwardCounter(n - 1))

  // Checking that a Generator can produce no values
  for a <- backwardCounter(0) do println(a)

  // Checking that a Generator can produce multiple values
  for a <- backwardCounter(5) do println(a)

  // Checking that multiple calls to hasNext do not trigger
  // the computation multiple times and do not skip values
  val gen = backwardCounter(5)
  gen.hasNext
  gen.hasNext
  gen.hasNext
  println(gen.next())
