package vec

import ai.dragonfly.math.vector.Vec

class VecTests extends munit.FunSuite:

   test("Some basic properties") {

      val v = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)
      val v2 = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)

      assertEquals(v.dimension, v2.dimension )

      // Reference, not value equality!
      assertNotEquals(v, v2 )
   }

   test("element rank") {
      val v = Vec.fromTuple(1.0, 5.0, 3.0, 6.0, 1.0, 5.0)
      /*
      1.0 is the first, but has as tied rank. Take the average - 1.5
      */
      assertEquals(v.elementRanks.csv(),  Array[Double](1.5, 4.5, 3.0, 6, 1.5, 4.5).mkString(","))

      val v2 = Vec.fromTuple(1.0, 5.0, 3.0, 6.0, 1.0, 5.0, 1.0)
      assertEquals(v2.elementRanks.csv(),  Array[Double](2, 5.5, 4.0, 7, 2, 5.5, 2).mkString(","))
   }

end VecTests