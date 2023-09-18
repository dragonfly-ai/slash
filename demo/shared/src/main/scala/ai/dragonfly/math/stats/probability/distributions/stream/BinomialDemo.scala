package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.*
import ai.dragonfly.democrossy.Demonstration


import scala.language.postfixOps
import scala.language.implicitConversions

object BinomialDemo {

  val fixedBinomialDemo = OnlineProbDistDemo[Long, distributions.Binomial, FixedBinomial]("Streaming FixedBinomial", distributions.Binomial(42L, 0.69), FixedBinomial(42L), 1000)

  val openBinomialDemo = new Demonstration {
    override def name: String = "Streaming Binomial"

    def si:Double = 1 + ((Math.random() - 0.5) / 8.0)

    override def demo(): Unit = {
      val sampleSize = 1000
      val idealDist = distributions.Binomial(69, 0.42)
      val streamingDist = Binomial()
      println(s"Estimate $name:\n\tSampling: $idealDist")
      val blockSize:Int = sampleSize / 5
      val end = sampleSize + 1
      for (i <- 1 until end) {
        val ki = idealDist.random() * si
        val ni = idealDist.n * si
        streamingDist.observe(ki.toLong, ni.toLong)
        if (i % blockSize == 0) {
          println(s"\n\t\testimation after $i samples: ${streamingDist.estimate}")
        }
      }
      println(s"\n\tEstimate: ${streamingDist.estimate}\n\tIdeal Distribution: $idealDist\n")
      println(s"\nTest $idealDist.p($idealDist.random())")

      for (i <- 0 until 5) {
        val x = idealDist.random()
        println(s"\n\tp($x) = ${idealDist.p(x)}")
      }
      println("\n")
    }

  }
}