package ai.dragonfly.math.util

trait Demonstrable {
  def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder
  def name:String
}
