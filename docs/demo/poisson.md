## Poisson Distribution


Example

```scala mdoc
import ai.dragonfly.math.stats.probability.distributions.Poisson
import ai.dragonfly.math.vector.*

val dist1 = Poisson(1)
for (i <- 0 to 5) {
  println( s"Poisson(1).pdf($i) = ${dist1.p(i)}" )
}

dist1.μ
dist1.`σ²`

val rand = ai.dragonfly.math.Random.defaultRandom
type N = 10000
val v : NArray[Long] = dist1.sample(10000, rand)

```

### Funsies

Our poisson distribution cross compiles to scalaJS... it turns out to be pretty simple from here, to build up a tiny app to investigate the Poisson distribution.

As we're using native (read - pretty quick) JS arrays behind the scenes - we can rag the ui thread pretty hard (100k samples) before we start to see any lag in our plots and stats.

For funsies - we compare theoretical and empirical distributions. This little app lets you pick a λ and a number of samples. Hover over a bar to see it's theoretical and sampled probability.

```scala mdoc:js:invisible

import com.raquo.laminar.api.L.*
import org.scalajs.dom

val appContainer = dom.document.querySelector(s"#${node.id}")

renderOnDomContentLoaded(
  appContainer,
  livechart.poissonChart()
)
```