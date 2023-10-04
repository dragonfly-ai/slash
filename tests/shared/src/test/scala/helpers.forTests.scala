import ai.dragonfly.math.vector.Vec


inline def assertVecEquals[N <: Int](inline v1: Vec[N], inline v2: Vec[N])(implicit loc: munit.Location): Unit = {
  var i: Int = 0;
  while (i < v1.dimension) {
    munit.Assertions.assertEquals(v1(i), v2(i))
    i += 1
  }
}