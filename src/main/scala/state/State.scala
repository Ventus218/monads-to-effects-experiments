package state

opaque type State[S, A] = StateImpl[S, A]
private case class StateImpl[S, A](run: S => (S, A))

object State:
  def apply[S, A](run: S => (S, A)): State[S, A] = StateImpl(run)
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  def same[S]: State[S, Unit] = pure(())
  def get[S]: State[S, S] = inspect(identity)
  def inspect[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))
  def modify[S, A](f: S => S): State[S, Unit] = State(s => (f(s), ()))

extension [S, A](s: State[S, A])
  def run(init: S): (S, A) = s.run(init)
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(a =>
      val (newState, res) = s.run(a)
      f(res).run(newState)
    )
  def map[B](f: A => B): State[S, B] =
    s.flatMap(a => State(init => (init, f(a))))

import cps.*

given [S]: CpsMonad[[A] =>> State[S, A]]
  with CpsPureMonadInstanceContext[[A] =>> State[S, A]]
  with
  def pure[T](t: T): State[S, T] = State.pure(t)
  def map[A, B](fa: State[S, A])(f: A => B): State[S, B] = fa.map(f)
  def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
    fa.flatMap(f)

@main
def test: Unit =
  type IntState[A] = State[Int, A]
  println:
    // async[[A] =>> State[Int, A]] {
    async[IntState] {
      State.modify[Int, Unit](a => a + 2).await
      val temp = State.get.await
      println(s"Intermediate value is $temp")
      State.modify[Int, Unit](a => a + 1).await
    }.run(0)
