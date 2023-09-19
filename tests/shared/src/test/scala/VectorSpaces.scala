import ai.dragonfly.math.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.Constant.π

import ai.dragonfly.math.Random.defaultRandom as r

class VectorSpaces extends munit.FunSuite {
  test(" testing VectorSpace ") {
    var runtimeDimension:Int = r.nextInt(42)
    runtimeDimension += 1

    val vs = VectorSpace(runtimeDimension)

    assertEquals(vs.dimension, runtimeDimension)

    val kitchenSink: Vec[vs.N] = ((vs.ones + vs.zeros) * 16) - vs.tabulate( (i:Int) => i / π )

    var i:Int = 0; while (i < vs.dimension) {
      assertEquals(kitchenSink(i), 1*16 - (i / π))
      i += 1
    }

  }
}
