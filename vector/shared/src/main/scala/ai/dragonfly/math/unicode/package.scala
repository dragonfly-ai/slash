package ai.dragonfly.math

package object unicode {

  private val superscriptDigits: Array[String] = Array[String]("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")
  private val subscriptDigits: Array[String] = Array[String]("₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", "₊")

  private def digitMapper(i: Int, digitMap: Array[String]): String = {

    var in: Int = i / 10
    var out: String = digitMap(i % 10)

    while (in > 0) {
      out = digitMap(in % 10) + out
      in = in / 10
    }
    out
  }

  def exalt(i: Int): String = {
    if (i < 0) "⁻" + digitMapper(i * -1, superscriptDigits)
    else digitMapper(i, superscriptDigits)
  }

  def abase(i: Int): String = {
    if (i < 0) "⁻" + digitMapper(i * -1, subscriptDigits)
    else digitMapper(i, subscriptDigits)
  }
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