package slash

package object native {

  def getExponent(d:Double):Int = {
    val s:String = d.toString
    val parts:Array[String] = s.split("e")
    if (parts.length > 1) {
      Integer.parseInt(parts(1))
    } else 0
  }

}
