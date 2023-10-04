import ai.dragonfly.math.vector.*
import narr.*



class SimpleOps extends munit.FunSuite:

  test("vector addition") {
    val v1 = Vec[2](1.5, 2.5)
    val v2 = Vec[2](2.5, 3.5)
    val vResult = Vec[2](4.0, 6.0)
    assertVecEquals(v1 + v2, vResult )
  }
  test("vector subtraction") {
    val v1 = Vec[2](1.5, 2.5)
    val v2 = Vec[2](2.5, 3.5)
    val vResult = Vec[2](-1.0, -1.0)
    assertVecEquals(v1 - v2, vResult )
  }

   test("scalar addition") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](2.5, 3.5, 4.5)
    assertVecEquals( v1 + 1, vResult )
   }

  test("scalar negation") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](0.5, 1.5, 2.5)
    assertVecEquals( v1 - 1.0, vResult )
   }

  test("scalar multiplication") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](3.0, 5.0, 7.0)
    assertVecEquals( v1 * 2.0, vResult )
   }

  test("scalar division") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](0.75, 1.25, 1.75 )
    assertVecEquals( v1 / 2.0, vResult )
   }

  test("clampedMin") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](2.0, 2.5, 3.5)
    assertVecEquals(v1.clampedMin(2.0), vResult)
  }

  test("clampedMAX") {
    val v1 = Vec[3](1.5, 2.5, 3.5)
    val vResult = Vec[3](1.5, 2.0, 2.0 )
    assertVecEquals( v1.clampedMAX(2.0), vResult )
   }

end SimpleOps

