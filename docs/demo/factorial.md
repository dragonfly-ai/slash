## Factorial

Example

```scala mdoc
import scala.language.postfixOps // don't forget this import
import slash.Factorial.!
for (x:Int <- Seq(1, 2, 3, 4, 8, 16, 32, 64, 100)) {
  println(s"${x}! = ${x!}")
}

```
