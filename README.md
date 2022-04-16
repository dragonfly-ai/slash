# vector

A Vector Math and Statistics library that brings machine learning, image processing, scientific/information visualization, and game development alternatives to Scala 3 and Scala.js.

Features:
- Vector trait and its inheritors: Vector2, Vector3, Vector4, and VectorN
- VectorValues type alias that abstracts the underlying Array[Double] and js.Array[Double] for compatibility with native Matrix and Vector libraries on the JVM and/or JavaScript.
- Parametric and Estimated (Online/Streaming) Probability Distributions: Gaussian/Normal, Poisson, LogNormal, Binomial (parametric only), Beta, and PERT; each with support for sampling and probability density functions, PDFs.
- Math functions: Beta, Factorial, and Gamma functions: B(α, β), x! and Γ(x).
- Geometry: Sample points uniformly from the volumes defined by 3D tetrahedrons.
- Visualization: Text based visualization, inspired by [https://github.com/JuliaPlots/UnicodePlots.jl].  Currently supports line charts, scatter plots, and histograms.
- Bijection[A, B]: an abstraction for bijective implicit conversions.
- BigRandom: scala.util.Random extension methods to Generate random BigInt and BigDecimal values.

Unicode Plot:

![Unicode plot of a Linear Regression Model and its Sample Data.](https://github.com/dragonfly-ai/vector/blob/master/RegressionPlot.png "Linear Plot with Scatter Plot")

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
	[ 37.00, 43.00 ) 🌕 ︰    ∝ 1.0E-4
}
```

To use this library with SBT:

<pre>
resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/"
libraryDependencies += "ai.dragonfly.code" %%% "vector" % "0.522"
</pre><br />

Projects that rely on this Library:

https://github.com/dragonfly-ai/matrix

https://github.com/dragonfly-ai/color

https://github.com/dragonfly-ai/spatial

https://github.com/dragonfly-ai/img