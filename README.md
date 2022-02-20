# vector

A Vector Math and Statistics library that brings machine learning, image processing, scientific/information visualization, and game development alternatives to Scala 3 and Scala.js.

Features:
- Vector trait and its inheritors: Vector2, Vector3, Vector4, and VectorN
- VectorValues type alias that abstracts the underlying Array[Double] and js.Array[Double] for compatibility with native Matrix and Vector libraries on the JVM and/or JavaScript.
- Parametric and Estimated (Online/Streaming) Probability Distributions: Gaussian/Normal, Poisson, LogNormal, Binomial (parametric only), Beta, and PERT; each with support for sampling and probability density functions, PDFs.
- Math functions: Beta, Factorial, and Gamma functions: B(α, β), x! and Γ(x).
- Geometry: Sample points uniformly from the volumes defined by 3D tetrahedrons.
- Plain text histogram plotting for console friendly output.
```
Histogram generated from Beta(5.0, 5.0): {
    [ 0.00, 0.10 ) 🌑 ▕    ∝ 0.0013
    [ 0.10, 0.20 ) 🌑 ▕█▉   ∝ 0.0176
    [ 0.20, 0.30 ) 🌑 ▕█████████▍   ∝ 0.0852
    [ 0.30, 0.40 ) 🌒 ▕██████████████████    ∝ 0.1634
    [ 0.40, 0.50 ) 🌓 ▕██████████████████████████▏   ∝ 0.237
    [ 0.50, 0.60 ) 🌓 ▕█████████████████████████▋   ∝ 0.232
    [ 0.60, 0.70 ) 🌔 ▕██████████████████▉   ∝ 0.1703
    [ 0.70, 0.80 ) 🌔 ▕████████▎   ∝ 0.0755
    [ 0.80, 0.90 ) 🌔 ▕█▊   ∝ 0.0168
    [ 0.90, 1.00 ] 🌕 ︙    ∝ 9.0E-4
}
```

To use this library with SBT:

<pre>
resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/"
libraryDependencies += "ai.dragonfly.code" %%% "vector" % "0.41"
</pre><br />

Projects that rely on this Library:

https://github.com/dragonfly-ai/matrix

https://github.com/dragonfly-ai/color

https://github.com/dragonfly-ai/spatial

https://github.com/dragonfly-ai/img