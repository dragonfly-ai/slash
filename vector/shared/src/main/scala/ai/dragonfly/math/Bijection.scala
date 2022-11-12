package ai.dragonfly.math

trait Bijection[A, B](using a2b: A => B, b2a: B => A) {
  given Conversion[A, B] with
    def apply(a: A): B = a2b(a)

  given Conversion[B, A] with
    def apply(b: B): A = b2a(b)
}
