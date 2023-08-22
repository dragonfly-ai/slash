
<h3>How to Use this Matrix Math Library:</h3>

&nbsp;&nbsp;&nbsp;This matrix library differs most significantly from others like <a href="https://math.nist.gov/javanumerics/jama/">JAMA</a> and <a href="https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/org/apache/commons/math3/linear/">Apache Commons Math</a>, by providing compile time dimensionality checks.  Instead of encoding matrix row and column dimensions with method parameters or `Array[Double].length` values, this library relies on dependent types.  For example:

```scala
// create a 3 x 2 matrix of zeros.
val m:Matrix[3, 2] = Matrix.zeros[3, 2]
```

&nbsp;&nbsp;&nbsp;By encoding the matrix's row and column dimensions into its type, the compiler can prevent a whole category of runtime errors that arise from mismatched matrix dimensions:

```scala
val m0:Matrix[3, 2] = Matrix.zeros[3, 2]
val m1:Matrix[2, 3] = Matrix.zeros[2, 3]

val m2:Matrix[3, 3] = m0 * m1
val m = m2 * m1 // compiler error!
```

&nbsp;&nbsp;&nbsp;Relatedly, many matrix operations like `determinant`, Cholesky decomposition, etc, only pertain to square matrices.  This library relies on type conditioned extension methods so that users simply cannot attempt to invoke these operations on rectangular matrices.  More specifically: 

```scala
extension [MN <: Int] (m: Matrix[MN, MN])(using ValueOf[MN]) {
  def determinant: Double = LU[MN, MN](m).determinant
}
```

&nbsp;&nbsp;&nbsp;Instead of including a `determinant` method directly in the `Matrix` class, this extension method makes a `determinant` method available only for square matrices.  Trying to invoke the `determinant` method on a rectangular metrix, for which M != N, will yield a compiler error.
