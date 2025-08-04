package generators

/** In this third design the evaluation of the next element is triggered by the
  * Generator instantiation and then by each call to next
  */
object Generators3 extends App:
  class Generator[A] extends Iterator[A]:

    /** Returns the computed value and tries to produce a new one
      */
    override def next(): A =
      val res = nextVal.get
      computeNext()
      res

    override def hasNext: Boolean = nextVal.isDefined

    /** A side effecting continuation which is responsible for setting nextVal.
      *
      * It is also used by _yield to set a continuation as a side effect
      */
    var computeNext: () => Unit = () => ()

    /** `nextVal` is used to store the next value
      */
    var nextVal: Option[A] = None

  /** Lets a generator function produce a value while also specifying a
    * continuation
    */
  def _yield[A](a: A, continueWith: => Generator[A])(using
      g: Generator[A]
  ): Generator[A] =
    g.nextVal = Some(a)
    g.computeNext = () => continueWith
    g

  /** Marks the end of a generator function without producing any value
    */
  def _yield[A](using g: Generator[A]): Generator[A] =
    g.nextVal = None
    g.computeNext = () => ()
    g

  /** This type allows to hide the context parameter in the return type of
    * generator functions
    */
  type Gen[A] = Option[Generator[A]] ?=> Generator[A]

  /** This given instance allows the user to not explicitly set a None as the
    * initial Generator of the recursive generator function
    */
  given [A]: Option[Generator[A]] = None

  def generator[A](
      f: (Generator[A], Option[Generator[A]]) ?=> Generator[A]
  ): Gen[A] =
    given g: Generator[A] = summon[Option[Generator[A]]].getOrElse(Generator())
    given Option[Generator[A]] = Some(g)
    f

  def backwardCounter(n: Int): Gen[Int] =
    println(s"Run with param: $n")
    // generator will create a Generator only if there is no
    // one in scope otherwise it will use the one he found.
    // This means that N recursive calls to generator won't
    // instantiate N Generators but just 1.
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
