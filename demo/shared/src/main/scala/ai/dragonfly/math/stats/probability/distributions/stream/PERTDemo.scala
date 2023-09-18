package ai.dragonfly.math.stats.probability.distributions.stream

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.*
import ai.dragonfly.math.stats.probability.distributions
import ai.dragonfly.math.stats.probability.distributions.stream


object PERTDemo extends Demonstration {
  val d:Demonstration = OnlineProbDistDemo[Double, distributions.PERT, stream.PERT](
    "Streaming PERT",
    distributions.PERT(21, 42.0, 69.0),
    /* I understand the superiority of stream.Beta over stream.PERT, but I have reasons! */
    try { new PERT } catch { case UseBetaDistributionInstead(pert) => pert },
    1000
  )
  override def demo(): Unit = {
    println(stream.PERT.doNotUse)
    d.demo()
  }

  override def name: String = "stream.PERT"
}
