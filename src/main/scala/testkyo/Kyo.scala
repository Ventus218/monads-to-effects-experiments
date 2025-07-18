package testkyo

import kyo.*

@main
def test: Unit =
  def divide10By(): Int < (Env[Int] & Abort[Absent]) =
    for
      input <- Env.get[Int]
      output = input match
        case 0 => Abort.get(None)
        case v => Abort.get(Some(10 / v))
    yield (output)

  println:
    Abort
      .run:
        Env.run(1):
          divide10By()
      .eval

  def divide10By_direct(): Int < (Env[Int] & Abort[Absent]) =
    defer {
      val input = Env.get[Int].now
      val output = input match
        case 0 => Abort.get(None).now
        case v => 10 / v
      output
    }

  println:
    Abort
      .run:
        Env.run(1):
          divide10By_direct()
      .eval

  /** Dividend is taken by the env.
    *
    * The divisor is decreased by two on every recursive step, this means that
    * invocations with an even divisor will fail in the end.
    *
    * Keeps dividing as long as the divisor is positive
    */
  def keepDividing(divisor: Int): Double < (Abort[Absent] & Env[Double]) =
    defer {
      val dividend = Env.get[Double].now
      divisor match
        case 0 => Abort.get(None).now
        case divisor if divisor > 0 =>
          Env.run(dividend / divisor)(keepDividing(divisor - 2)).now
        case _ => dividend
    }

  println:
    Abort
      .run:
        Env.run(100000d):
          keepDividing(10)
      .eval

  println:
    Abort
      .run:
        Env.run(100000d):
          keepDividing(11)
      .eval
