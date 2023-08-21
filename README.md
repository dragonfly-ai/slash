# vector

[![javadoc](https://javadoc.io/badge2/ai.dragonfly/vector_3/javadoc.svg)](https://javadoc.io/doc/ai.dragonfly/vector_3)

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
- Probability Distributions, Parametric and Estimated (Online/Streaming): Gaussian/Normal, Poisson, LogNormal, Binomial (parametric only), Beta, and PERT; each with support for sampling and probability density functions, PDFs.
- Sampleable trait for making types into generative models.
- Math functions: Beta, Factorial, and Gamma functions: B(α, β), x! and Γ(x).  Convenience macros, methods, and case classes for computing logarithms of arbitrary base. 
- Geometry: Sample points uniformly from volumes defined by 3D tetrahedrons.
- Bresenham Line Drawing Algorithm that invokes a lambda for each discrete point on a line.
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
import ai.dragonfly.math.vector.Vec.*

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


<h3>Compile time Logarithms</h3>

&nbsp;&nbsp;&nbsp;To compute `logₓ(y)`, where `x` is any value of type: `Double`, we compute: `log₁₀(y) / log₁₀(x)`.  Which can introduce a lot of computational overhead, especially when done in loops which repeatedly compute the same value for `log₁₀(x)`.  To improve performance and legibility, this library provides a `log[BASE <: Double | Int]` macro that computes the `log₁₀(x)` denominator at compile time; it also clarifies the operation by allowing users to write the base of the log into the type parameter and the operand as a method parameter.  As such, instead of: `log(2 /*base*/, 42 /*operand*/)` we can write: `log[2](42)`.

```scala
import ai.dragonfly.math.*

// Compile time optimized Logarithms of known base:
log[2](42.0)  // Computes log₂(42) at compile time
log[0.5](11.0)  // Computes log₀.₅(11) at compile time

var i: Int = 1; while (i > 0) {
  println( log[2](i) )  // computes half of this operation at compile time
  i = i << 1
}
```

<h3>Runtime Logarithms</h3>
&nbsp;&nbsp;&nbsp;The runtime `Log` class can yield comparable performance in cases when the base of the logarithm can't be known at compile time, or can't be expressed as a constant.

```scala
import ai.dragonfly.math.*
// Use the runtime Log class for:
// a base determined by a value:
import ai.dragonfly.math.Constant.π
val logBasePi: Log = Log(π)
logBasePi(13)
// or any base unknown at compile time,
val logBaseRandom:Log = Log(Math.random())
logBaseRandom(42)
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

<h3>Add the SBT dependency:</h3>

```scala
libraryDependencies += "ai.dragonfly" %%% "vector" % "<LATEST_VERSION>"
```

<h2>FAQ:</h2>
<ul>
<li>

`NArray[Double]` is useful, but why exclude `NArray[Float]`?

In Scala.js, the runtime type of `Float` doesn't fully exist, while Scala.js, Scala JVM, and Scala Native all share the same implementation of `Double`.
</li>
<li>What if a vector's dimension depends on a variable?  Can this library support vectors with dimensionality defined at runtime?

Yes, but with some limitations.  Please consider the following examples:

```scala
// Vectors with lengths defined at runtime.

// method 1:
val l0:Int = r.nextInt (100)
type N0 = l0.type
val rtv0: Vec[N0] = r.nextVec[N0]()
println(rtv0.render())

// method 2:
val l1: Int = r.nextInt(100)
val rtv1: Vec[l1.type] = r.nextVec[l1.type]()
println(rtv1.render())

// For better or worse, this throws a compiler error even though l1 == l2 is true:
val l1: Int = r.nextInt(100)
val l2: Int = 0 + l1
val rtv1: Vec[l1.type] = r.nextVec[l1.type]()
val rtv2: Vec[l2.type] = r.nextVec[l2.type]()
println((rtv1 + rtv2).render())
  
//    [error] 57 |    println((rtv1 + rtv2).render())
//    [error]    |                    ^^^^
//    [error]    |             Found:    (rtv2 : ai.dragonfly.math.vector.Vec[(l2 : Int)])
//    [error]    |             Required: ai.dragonfly.math.vector.Vec[(l1 : Int)]


// However, you can do this:
val l1: Int = r.nextInt(100)
val l2: Int = 0 + l1
val rtv1: Vec[l1.type] = r.nextVec[l1.type]()
val rtv2: Vec[l2.type] = r.nextVec[l2.type]()
println((rtv1 + rtv2.asInstanceOf[Vec[l1.type]]).render())
```

We recommend writing a representation of a vector space that contains a reference to a dimension type, then sharing that type across all of the vectors in that space.  Unfortunately, this means having to think about a concept that no other math library has had, but it can also head off all kinds of run time errors caused by mismatched dimension exceptions.
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
