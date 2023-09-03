# Introduction

## Importing

To add this library to an sbt project
```scala
libraryDependencies += "ai.dragonfly" %%% "slash" %  "@VERSION@"
```

To use this library in ammonite / mill
```scala
import $ivy.`ai.dragonfly::slash:@VERSION@
```

scala-cli
```scala
using dep ai.dragonfly::slash:@VERSION@
```

## Elevator pitch

```scala mdoc
import ai.dragonfly.math.vector.*
val aVector = Vec[3](1,2,3)
val times2 = aVector * 2
aVector + times2
```
But attemping to add a vector of the wrong dimension will fail at compile time

```scala mdoc:fail
val oopsDimension = Vec.ones[2]
aVector + oopsDimension

```
These typesafe operations are the same on the JVM, Native and JS platforms. They take advantge of a shim of the underlying _native_ array definitions  - thus maximisng performance, so that you don't have to. For further reading, see [Design notes](design/design.md)