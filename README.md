# vector

A Scala 3 vector math and statistics library designed to:

<ol>
<li>Cross compile to JVM, Native and JavaScript platforms</li>
<li>Maximize performance</li>
<li>Minimize memory footprint</li>
<li>Provide convenient syntax</li>
<li>Seamlessly interoperate with other math libraries</li>
<li>Seamlessly interoperate with native languages like JavaScript, C/C++, and Java</li>
<li>Serialize efficiently and compactly by default</li>
</ol>

Features:
- High performance Vector data types with convenient vector math syntax.
- Octree for nearest neighbor search and radial querying.
- Parametric and Estimated (Online/Streaming) Probability Distributions: Gaussian/Normal, Poisson, LogNormal, Binomial (parametric only), Beta, and PERT; each with support for sampling and probability density functions, PDFs.
- Sampleable trait for declaring generative models.
- Math functions: Beta, Factorial, and Gamma functions: B(α, β), x! and Γ(x).
- Geometry: Sample points uniformly from volumes defined by 3D tetrahedrons.  Bresenham Line Drawing Algorithm that invokes a lambda for each discrete point on a line.
- Kernels: Gaussian, Epanechnikov, Uniform, and Discrete.
- Flexible Histogram data structures with Console friendly Text Based Visualizations inspired by <a href="https://github.com/JuliaPlots/UnicodePlots.jl">Julia Plots</a>.
- Bijection[A, B]: an abstraction for bijective implicit conversions.
- BigRandom: scala.util.Random extension methods to generate random BigInt and BigDecimal values.
- Interval and Domain types and objects with support for random sampling.
- Unicode text formatting utility for writing numeric value types in superscript or subscript positions.


<a href="https://dragonfly-ai.github.io/vector/demo">Try the demo</a>.

&nbsp;&nbsp;&nbsp;Instead of case classes, traits, or wrappers, this library represents all runtime vector data as native arrays of double precision floating point values.  However, it also uses Scala 3 features like opaque types, dependent types, and extension methods to decorate the array primitives with convenient syntax, e.g. overloaded operators like `+ - * / += -= *= /=`, and also, by expressing vector dimensionality as a type parameter, can prevent runtime errors resulting from trying to perform vector operations on vectors of mismatched dimensions at compile time.  For example:   

```scala
opaque type Vec[N <: Int] = NArray[Double]

val v2:Vec[2] = Vec[2](1.0, 2.0)
val v3:Vec[3] = Vec[3](1.0, 2.0, 3.0)

println((v2 + v3).show) // compiler error!

// Even though, at runtime, v2 and v3 have the same type,
// the compiler treats them as though they have different, distinct types.
// This eliminates runtime exceptions related to mismatched vector dimensions.
```

&nbsp;&nbsp;&nbsp;Why `NArray[Double]` and not `Array[Double]`?  Because vector relies on <a href="https://github.com/dragonfly-ai/narr">NArr</a>, JavaScript environments store vector data as: `Float64Array` while JVM and Native environments rely on: `Array[Double]`.  This ensures that whichever compilation target you choose, Vector will always reduce to the native array type with the highest available performance.

&nbsp;&nbsp;&nbsp;For a more detailed explanation of the design decisions that have shaped this library, see the <a href="https://dragonfly-ai.github.io/vector/">design notes.</a>

<h3 id="vectormath">More Vector Math Syntax:</h3>

```scala
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vector.*

// create a 3 dimensional vector
val v1:Vec[3] = Vec[3](1.0, 0.5, 0.0)

// print it to the console with expressive unicode text
println(v1.show)  // -> 《³↗〉1.0ᵢ 0.0ⱼ 0.0ₖ〉

// perform various vector math operations
val v2:Vec[3] = Vec[3](0.75, 1.0, 0.5)

val v3:Vec[3] = v1 + v2

println( v3.norm )

v3 -= v1

println( v3.euclideanDistanceTo(v2) )

// perform Vector3 specific operations
import ai.dragonfly.math.vector.Vector3.*

println(v1.x) // .x .y and .z provided by an extension method in Vector3

// compute 3D Vector Cross product
println( (v1 cross v2 ).show )
println( (v1 ⨯ v2 ).show ) // or with a unicode ⨯ operator

import ai.dragonfly.math.Random.*
val r:Random = defaultRandom

// higher dimensional vectors
val v42a:Vec[42] = r.nextVec[42]()
val v42b:Vec[42] = r.nextVec[42]()

println( v42a dot v42b )
println( (v42a - v42b).render() ) // fully customisable render method.
println( (v42a + v42b).csv() ) // output vector sum as comma separated values
println( (v42a + v42b).tsv() ) // output vector sum tab separated values
```

<h3>Parametric Probability Distributions</h3>

```scala
import ai.dragonfly.math.stats.probability.distributions.*

// create a gaussian distribution parametrically
val g:Gaussian = Gaussian(10.0, 42.0)
g.p(7.0)   // evaluate the Probability Density Function at 7.0, in other words: PDF(7.0)
g.random() // randomly sample a value from this gaussian model
```


<h3>Estimated, also called Online or Streaming, Probability Distributions</h3>

```scala
import ai.dragonfly.math.stats.probability.distributions.*

// create a gaussian distribution parametrically
val eg:stream.Gaussian = stream.Gaussian()
// observe some trials
eg.observe(7.0)
eg.observe(7.5)
eg.observe(9.1)
eg.observe(6.3)
eg.observe(8.4)

// estimate a parametric Gaussian distribution from the observations
val pg:Gaussian = eg.estimate
pg.p(8.0)   // estimate the Probability Density Function at 8.0, in other words: PDF'(8.0)
pg.random() // randomly sample a value from the estimated gaussian model
```

<h3>Add the SBT dependency:</h3>

```scala
libraryDependencies += "ai.dragonfly" %%% "vector" % "<LATEST_VERSION>"
```

Unicode Histogram Plot:

```
Histogram: { 
	[-17.00,-11.00 ) 🌑 ︙    ∝ 3.0E-4
	[-11.00, -5.00 ) 🌑 ▕█   ∝ 0.0094
	[ -5.00,  1.00 ) 🌑 ▕████████    ∝ 0.0732
	[  1.00,  7.00 ) 🌒 ▕███████████████████████████    ∝ 0.2447
	[  7.00, 13.00 ) 🌓 ▕███████████████████████████████████████   ∝ 0.3516
	[ 13.00, 19.00 ) 🌔 ▕██████████████████████████▋   ∝ 0.2411`
	[ 19.00, 25.00 ) 🌔 ▕███████▌   ∝ 0.069
	[ 25.00, 31.00 ) 🌔 ▕█   ∝ 0.0101
	[ 31.00, 37.00 ) 🌔 ︙    ∝ 5.0E-4
	[ 37.00, 43.00 ] 🌕 ︰    ∝ 1.0E-4
}
```
These plots label the bins with standardised math notation for open and closed intervals, but also use the unicode glyphs for the phases of the moon to represent the cumulative distribution.

<h2>FAQ:</h2>
<ul>
<li>

`NArray[Double]` is useful, but why exclude `NArray[Float]`?

In Scala.js, the runtime type of `Float` doesn't fully exist, while Scala.js, Scala JVM, and Scala Native all share the same implementation of `Double`.
</li>
</ul>
<br />

Projects that rely on this Library:

https://github.com/dragonfly-ai/matrix

https://github.com/dragonfly-ai/bitfrost

https://github.com/dragonfly-ai/spatial

https://github.com/dragonfly-ai/graphics

https://github.com/dragonfly-ai/img

Acknowledgements:
This library has evolved over years, but owes a lot of its virtues to feedback from the Scala Discord community.  In no particular order:

https://github.com/ekrich

https://github.com/armanbilge

https://github.com/s5bug

https://github.com/BalmungSan