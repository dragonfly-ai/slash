package verification

import scala.Console.println

case class ArrayComparison(rows:Int, columns:Int, discrepancies:Int, combinedError:Double) {

  def averageError:Double = if (discrepancies == 0) 0.0 else combinedError / discrepancies


  override def toString: String = s"Made ${rows * columns} element wise comparisons and found $discrepancies discrepancies with error: { combined : $combinedError, average : $averageError }"
}
