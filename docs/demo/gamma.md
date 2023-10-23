## Gamma

Try gamma function

```scala mdoc

for ( i <- 1 until 10 ) {
  val i_1:Int = i - 1
  println(s"\tΓ($i):$i_1! => ${slash.gamma(i.toDouble)} : ${slash.Factorial(i_1)}")
}

```
