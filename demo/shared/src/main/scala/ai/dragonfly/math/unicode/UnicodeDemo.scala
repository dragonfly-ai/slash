package ai.dragonfly.math.unicode

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.unicode.*

object UnicodeDemo extends Demonstration {
  import Console.*
  override def demo():Unit = {

    println(s"Write ${GREEN}Byte$RESET in superscript and subscript:")
    println(s"\texalt(Byte.MinValue) => exalt(${Byte.MinValue}) => ${exalt(Byte.MinValue)}")
    println(s"\texalt(Byte.MaxValue) => exalt(${Byte.MaxValue}) => ${exalt(Byte.MaxValue)}")

    println(s"\tabase(Byte.MinValue) => abase(${Byte.MinValue}) => ${abase(Byte.MinValue)}")
    println(s"\tabase(Byte.MaxValue) => abase(${Byte.MaxValue}) => ${abase(Byte.MaxValue)}")


    println(s"Write ${GREEN}Short$RESET in superscript and subscript:")
    println(s"\texalt(Short.MinValue) => exalt(${Short.MinValue}) => ${exalt(Short.MinValue)}")
    println(s"\texalt(Short.MaxValue) => exalt(${Short.MaxValue}) => ${exalt(Short.MaxValue)}")

    println(s"\tabase(Short.MinValue) => abase(${Short.MinValue}) => ${abase(Short.MinValue)}")
    println(s"\tabase(Short.MaxValue) => abase(${Short.MaxValue}) => ${abase(Short.MaxValue)}")


    println(s"Write ${GREEN}Int$RESET in superscript and subscript:")
    println(s"\texalt(Int.MinValue) => exalt(${Int.MinValue}) => ${exalt(Int.MinValue)}")
    println(s"\texalt(Int.MaxValue) => exalt(${Int.MaxValue}) => ${exalt(Int.MaxValue)}")

    println(s"\tabase(Int.MinValue) => abase(${Int.MinValue}) => ${abase(Int.MinValue)}")
    println(s"\tabase(Int.MaxValue) => abase(${Int.MaxValue}) => ${abase(Int.MaxValue)}")


    println(s"Write ${GREEN}Long$RESET in superscript and subscript:")
    println(s"\texalt(Long.MinValue) => exalt(${Long.MinValue}) => ${exalt(Long.MinValue)}")
    println(s"\texalt(Long.MaxValue) => exalt(${Long.MaxValue}) => ${exalt(Long.MaxValue)}")

    println(s"\tabase(Long.MinValue) => abase(${Long.MinValue}) => ${abase(Long.MinValue)}")
    println(s"\tabase(Long.MaxValue) => abase(${Long.MaxValue}) => ${abase(Long.MaxValue)}")


    println(s"Write ${GREEN}Float$RESET in superscript and subscript:")
    println(s"\texalt(Float.MinValue) => exalt(${Float.MinValue}) => ${exalt(Float.MinValue)}")
    println(s"\texalt(Float.MinPositiveValue) => exalt(${Float.MinPositiveValue}) => ${exalt(Float.MinPositiveValue)}")
    println(s"\texalt(Float.MaxValue) => exalt(${Float.MaxValue}) => ${exalt(Float.MaxValue)}")

    println(s"\tabase(Float.MinValue) => abase(${Float.MinValue}) => ${abase(Float.MinValue)}")
    println(s"\tabase(Float.MinPositiveValue) => abase(${Float.MinPositiveValue}) => ${abase(Float.MinPositiveValue)}")
    println(s"\tabase(Float.MaxValue) => abase(${Float.MaxValue}) => ${abase(Float.MaxValue)}")


    println(s"Write ${GREEN}Double$RESET in superscript and subscript:")
    println(s"\texalt(Double.MinValue) => exalt(${Double.MinValue}) => ${exalt(Double.MinValue)}")
    println(s"\texalt(Double.MinPositiveValue) => exalt(${Double.MinPositiveValue}) => ${exalt(Double.MinPositiveValue)}")
    println(s"\texalt(Double.MaxValue) => exalt(${Double.MaxValue}) => ${exalt(Double.MaxValue)}")

    println(s"\tabase(Double.MinValue) => abase(${Double.MinValue}) => ${abase(Double.MinValue)}")
    println(s"\tabase(Double.MinPositiveValue) => abase(${Double.MinPositiveValue}) => ${abase(Double.MinPositiveValue)}")
    println(s"\tabase(Double.MaxValue) => abase(${Double.MaxValue}) => ${abase(Double.MaxValue)}")
  }
  override def name:String = "Unicode"
}