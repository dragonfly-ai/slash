import ai.dragonfly.math.vector.Vec
import narr.NArray

class InstantiateTests extends munit.FunSuite:

   test(" ways of making vecs ") {

    //type dim = 5

    val v = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)
    val v2 = Vec[5](1.0, 2.0, 3.0, 4.0, 5.0)
    val v_fill = Vec.fill[5](1.0)

    val v_zeros = Vec.zeros[5]
    val v_ones = Vec.ones[5]


    val v_rand = Vec.random[5]()
    val v_rand_max_min = Vec.random[5](2.0, 0.5)

    assertEquals(v2.dimension, v2.dimension )
    assertEquals(v2.dimension, v_fill.dimension )
    assertEquals(v2.dimension, v_zeros.dimension )
    assertEquals(v2.dimension, v_rand.dimension )
    assertEquals(v2.dimension, v_rand.dimension )
    assertEquals(v2.dimension, v_rand_max_min.dimension )

   }

end InstantiateTests