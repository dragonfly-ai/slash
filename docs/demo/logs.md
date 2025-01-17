## Logarithms

Example

```scala mdoc
import slash.*
import slash.Constant

log[4](16)

log[10](-1)

log[2.7182818284590452](Constant.e)

println( s"log[2.0](42.0) = ${log[2.0](42.0)}" )

var i: Int = 1;

while (i > 0) {
  println( s"log[2]($i) = ${log[2](i)}" )
  i = i << 1
}


```
