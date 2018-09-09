package ai.dragonfly.math.stats

object TestHistograms extends App {

  val sodh: Histogram = new SparseOrderedDiscreteHistogram(10, 0.0, 10.0)
  for (i <- 0 until 10000) {
    sodh(
      10.0 * Math.random(),
      Math.random()
    )
  }
  println(sodh)

  val gm = UnivariateGenerativeModel(sodh)

  println(gm)

  val sodh1: Histogram = new SparseOrderedDiscreteHistogram(10, 0.0, 10.0)

  for (i <- 0 until 10000) {
    sodh1( gm(), 1.0 )
  }

  println(sodh1)

  val sodh2: Histogram = new SparseOrderedDiscreteHistogram(10, 0.0, 10.0)

  sodh2(1 + Math.random(), 5)
  sodh2(2 + Math.random(), 39)
  sodh2(6 + Math.random(), 15)
  sodh2(9 + Math.random(), 25)

  val gm1 = UnivariateGenerativeModel(sodh2)

  val sodh3: Histogram = new SparseOrderedDiscreteHistogram(10, 0.0, 10.0)

  for (i <- 0 until 10000) sodh3( gm1(), 1.0 )

  println(sodh2 + "\n" + sodh3)

  // DenseDiscreteHistogram
  val ddh: Histogram = new DenseDiscreteHistogram(10, 0.0, 10.0)

  ddh(1 + Math.random(), 5)
  ddh(2 + Math.random(), 39)
  ddh(6 + Math.random(), 15)
  ddh(9 + Math.random(), 25)

  val gm2 = UnivariateGenerativeModel(ddh)

  val ddh1: Histogram = new DenseDiscreteHistogram(10, 0.0, 10.0)

  for (i <- 0 until 10000) ddh1( gm2(), 1.0 )

  println(ddh + "\n" + ddh1)


  val sdh: Histogram = new SparseDiscreteHistogram(10, 0.0, 10.0)

  sdh(1 + Math.random(), 5)
  sdh(2 + Math.random(), 39)
  sdh(6 + Math.random(), 15)
  sdh(9 + Math.random(), 25)

  val gm3 = UnivariateGenerativeModel(sdh)

  val sdh1: Histogram = new SparseDiscreteHistogram(10, 0.0, 10.0)

  for (i <- 0 until 10000) sdh1(gm3(), 1.0)

  println(sdh + "\n" + sdh1)

}