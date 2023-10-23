/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package slash

package object unicode {

// JavaScript Scientific Notation:
//  -1.7976931348623157e+308
//  5e-324
//  1.7976931348623157e+308

// JVM Scientific Notation:
//  -1.7976931348623157E308
//  4.9E-324
//  1.7976931348623157E308

// Native Scientific Notation:
//  -1.7976931348623157E308
//  4.9E-324
//  1.7976931348623157E308

  def exalt(c:Char):Char = c match {
    case '0' => '⁰'
    case '1' => '¹'
    case '2' => '²'
    case '3' => '³'
    case '4' => '⁴'
    case '5' => '⁵'
    case '6' => '⁶'
    case '7' => '⁷'
    case '8' => '⁸'
    case '9' => '⁹'
    case '.' => 'ᐧ'
    case '-' => '⁻'
    case 'e' => 'ᵉ'
    case 'E' => 'ᵉ'
    case '+' => '⁺'
  }

  def abase(c: Char): Char = c match {
    case '0' => '₀'
    case '1' => '₁'
    case '2' => '₂'
    case '3' => '₃'
    case '4' => '₄'
    case '5' => '₅'
    case '6' => '₆'
    case '7' => '₇'
    case '8' => '₈'
    case '9' => '₉'
    case '.' => '.'
    case '-' => '₋'
    case 'e' => 'ₑ'
    case 'E' => 'ₑ'
    case '+' => '₊'
  }

  private def exalt(numericString: String): String = {
    val charBuffer: Array[Char] = new Array[Char](numericString.length)
    var i: Int = 0
    while (i < numericString.length) {
      charBuffer(i) = exalt(numericString.charAt(i))
      i += 1
    }
    new String(charBuffer)
  }

  private def abase(numericString: String):String = {
    val charBuffer:Array[Char] = new Array[Char](numericString.length)
    var i:Int = 0
    while (i < numericString.length) {
      charBuffer(i) = abase(numericString.charAt(i))
      i += 1
    }
    new String(charBuffer)
  }

  def exalt(b: Byte): String = exalt(b.toString)
  def abase(b: Byte): String = abase(b.toString)

  def exalt(s: Short): String = exalt(s.toString)
  def abase(s: Short): String = abase(s.toString)

  def exalt(i: Int): String = exalt(i.toString)
  def abase(i: Int): String = abase(i.toString)

  def exalt(l: Long): String = exalt(l.toString)
  def abase(l: Long): String = abase(l.toString)

  def exalt(d:Double):String = exalt(d.toString)
  def abase(d:Double):String = abase(d.toString)

  def exalt(f: Float): String = exalt(f.toString)
  def abase(f: Float): String = abase(f.toString)

}

/*
  Unicode symbols chart:
           Math: ℕ √ ∛ ∜ ∫ ∬ ∭ ⨌ ∑ ∏ ∂ ⌈x⌉ ⌊x⌋ ÷ ∆ ⨯ ∇ ∥ ⋆ ∗ ∘ ∙ ⋅
     Comparison: ≤ ≥
            Set: ∅ ⊂ ⊃ ⊄ ⊅ ⊆ ⊇ ⊈ ⊉ ⊊ ⊋ ∈ ∋ ∉ ∌ ∖ ⋂ ⋃ ℕ ℤ ℚ ℝ ℂ ⋯
       Geometry: ∡ ⦛ ∟ ⊾ ⦝ ⊿ ∠ ⦞ ⦢ ⦣ ⦤ ⦥ ∢ ⦠ ⦡ ⟂ ∥ ∦ ⫲ ⫳ ⋕
          Greek: α β μ σ α β δ ε θ λ μ π φ ψ Ω Σ ∏ Δ
            Hat: o⃗ x⃑ x̂ x′ x″ x‴3⃑
      Subscript: ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓ
                 xₐ xₑ xₕ xᵢ xⱼ xₖ xₗ xₘ xₙ xₒ xₚ xᵣ xₛ xₜ xᵤ xᵥ xₓ
                 x₀ x₁ x₂ x₃ x₄ x₅ x₆ x₇ x₈ x₉ x₊ x₋ x₌ x₍ x₎
Subscript Words: ᵥₐₗᵤₑₛ ₐᵥ ₘᵢₙ ₘₐₓ
    Superscript: ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻ ᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁⱽᵂ
                 xᵃ xᵇ xᶜ xᵈ xᵉ xᶠ xᵍ xʰ xⁱ xʲ xᵏ xˡ xᵐ xⁿ xᵒ xᵖ xʳ xˢ xᵗ xᵘ xᵛ xʷ xˣ xʸ xᶻ
                 x⁰ x¹ xⁱ x² x³ x⁴ x⁵ x⁶ x⁷ x⁸ x⁹ x⁺ x⁻ x⁼ x⁽ x⁾ xⁿ
  */