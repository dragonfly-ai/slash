package ai.dragonfly.math

import ai.dragonfly.math.example.Demonstrable

import scala.reflect.ClassTag

trait Bijection[A, B](using a2b: A => B, b2a: B => A) {
  given Conversion[A, B] with
    def apply(a: A): B = a2b(a)

  given Conversion[B, A] with
    def apply(b: B): A = b2a(b)
}

object Bijection extends Demonstrable {

  object H2O {

    case class Ice(grams:Double) {
      def +(s:Ice): Ice = Ice(this.grams + s.grams)
      override def toString:String = s"Ice($grams grams)"
    }
    case class Water(grams:Double) {
      def +(s:Water): Water = Water(this.grams + s.grams)
      override def toString:String = s"Water($grams grams)"
    }
    case class Steam(grams:Double) {
      def +(s:Steam): Steam = Steam(this.grams + s.grams)
      override def toString:String = s"Steam($grams grams)"
    }

    given Conversion[Water, Ice] with
      def apply(w:Water):Ice = Ice(w.grams)

    given Conversion[Ice, Water] with
      def apply(i:Ice):Water = Water(i.grams)

    given Conversion[Water, Steam] with
      def apply(w:Water):Steam = Steam(w.grams)

    given Conversion[Steam, Water] with
      def apply(s:Steam):Water = Water(s.grams)

    val freezeAndMelt:Bijection[Water, Ice] = new Bijection[Water, Ice]{}
    val precipitateAndEvaporate:Bijection[Water, Steam] = new Bijection[Water, Steam]{}

  }
  import scala.language.implicitConversions
  import H2O.*
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val antarctica:Ice = Ice(24353500000000000.0)
    val ocean:Water = Water(1.4E21)
    val clouds:Steam = ocean + antarctica;
    sb.append(s"Melt Antarctica, $antarctica, and the mass of the ocean increases from $ocean to ${ocean + antarctica}.  Boil all of that, and the atmosphere includes $clouds.")
  }
  override def name:String = "Bijection"
}