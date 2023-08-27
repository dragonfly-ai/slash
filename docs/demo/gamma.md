## Gamma

Try gamma function

```scala mdoc
import ai.dragonfly.math

for ( i <- 1 until 10 ) {
  val i_1:Int = i - 1
  println(s"\tΓ($i):$i_1! => ${math.gamma(i.toDouble)} : ${math.Factorial(i_1)}")
}

```
