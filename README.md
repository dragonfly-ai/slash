# vector

A Scala 3 Vector Math and Statistics library designed to simultaneously provide convenient syntax, maximize performance, and cross compile to JVM, Native and Scala.js platforms.  <a href="https://dragonfly-ai.github.io/vector/">Try the demo</a>.

<h3>How to use it:</h3>
Installation with SBT:

```scala
libraryDependencies += "ai.dragonfly" %%% "vector" % "<LATEST_VERSION>"
```

Features:
- High performance, low memory footprint Vector type `Vector[N <: Int]` which consists of nothing more than `Float64Array` in JavaScript environments and `Array[Double]` on JVM and Native platforms.  Extension methods, Opaque Types, and Dependent Types combine to provide convenient syntax for developers without introducing any runtime overhead.
- Customisable Vector data to text converters.
- Parametric and Estimated (Online/Streaming) Probability Distributions: Gaussian/Normal, Poisson, LogNormal, Binomial (parametric only), Beta, and PERT; each with support for sampling and probability density functions, PDFs.
- Math functions: Beta, Factorial, and Gamma functions: B(α, β), x! and Γ(x).
- Geometry: Sample points uniformly from volumes defined by 3D tetrahedrons.  Bresenham Line Drawing Algorithm that invokes a lambda for each discrete point on a line. 
- Kernels: Gaussian, Epanechnikov, Uniform, and Discrete.
- Flexible Histogram data structures with Console friendly Text Based Visualizations inspired by <a href="https://github.com/JuliaPlots/UnicodePlots.jl">Julia Plots</a>.
- Bijection[A, B]: an abstraction for bijective implicit conversions.
- BigRandom: scala.util.Random extension methods to generate random BigInt and BigDecimal values.
- Interval and Domain types and objects with support for random sampling. 
- Unicode text formatting utility for writing integers in superscript or subscript positions. 

<h3>Vector Math Syntax:</h3>

```scala
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vector.*

// create a 3 dimensional vector
val v1:Vector[3] = Vector[3](1.0, 0.5, 0.0)

// print it to the console
println(v1.show)  // -> 《³↗〉1.0ᵢ 0.0ⱼ 0.0ₖ〉

// perform various vector math operations
val v2:Vector[3] = Vector[3](0.75, 1.0, 0.5)

val v3:Vector[3] = v1 + v2

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
val v42a:Vector[42] = r.nextVector[42]()
val v42b:Vector[42] = r.nextVector[42]()

println( v42a dot v42b )
println( (v42a - v42b).render() )
println( (v42a + v42b).csv() ) // output vector sum as comma separated values
println( (v42a + v42b).tsv() ) // output vector sum tab separated values
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

# JavaScript Optimization

Because vector relies on <a href="https://github.com/dragonfly-ai/narr">NArr</a>, JavaScript environments store vector data as:

```scala
var vectorArray:NArray[Double]
```
which is equivalent to:

```scala
var matrixArray:Float64Array
```

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