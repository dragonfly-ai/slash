package ai.dragonfly.math.stats.probability.distributions


import ai.dragonfly.math.examples.ProbabilityDistributionDemonstration

object PERT {
  val demo = ProbabilityDistributionDemonstration("PERT", PERT(5.0, 6.0, 11.0))
}

case class PERT(override val min:Double, override val μ:Double, override val MAX:Double) extends ContinuousProbabilityDistribution {

  override val `σ²`:Double = ((μ - min) * (MAX - μ)) / 7.0
  override lazy val σ:Double = Math.sqrt(`σ²`)

  private lazy val underlying:Beta = Beta.fromPERT(this)

  override def p(x:Double):Double = underlying.p(x)

  override def random(): Double = underlying.random()
}
