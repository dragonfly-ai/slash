# vector

A Vector Math and Statistics library that brings machine learning, image processing, scientific/information visualization, and game development alternatives to Scala 3 and Scala.js.

Features:
- Vector trait and its inheritors: Vector2, Vector3, Vector4, and VectorN
- VectorValues type alias that abstracts the underlying Array[Double] and js.Array[Double] for compatibility with native Matrix and Vector libraries on the JVM and/or JavaScript.
- Parametric and Estimated (Online/Streaming) Probability Distributions: Gaussian/Normal, Poisson, LogNormal, Beta, and PERT; each with support for sampling and probability density functions, PDFs.
- Math functions: Beta, Factorial, and Gamma functions: B(α, β), x! and Γ(x).
- Geometry: Sample points uniformly from the volumes defined by 3D tetrahedrons.

To use this library with SBT:

<pre>
resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/"
libraryDependencies += "ai.dragonfly.code" %%% "vector" % "0.331"
</pre><br />

Projects that rely on this Library:

https://github.com/dragonfly-ai/matrix

https://github.com/dragonfly-ai/color

https://github.com/dragonfly-ai/spatial

https://github.com/dragonfly-ai/img