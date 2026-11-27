module

public import UnivariateML.Family.SkewLSGaussian
public import Mathlib.Analysis.Complex.HasPrimitives
public import Mathlib.Analysis.Calculus.FDeriv.Analytic

set_option linter.style.header false

/-!
# Real analyticity of the Gaussian distribution function

Use a complex primitive of the entire Gaussian kernel, and fix its
additive constant on the real line by the value of the CDF at zero.
-/

@[expose] public section

open Set Real
open UnivariateML.SkewLSGaussian

namespace UnivariateML.AE4Gaussian

/-- The standard Gaussian density is real analytic. -/
theorem analyticAt_phi (x : ℝ) : AnalyticAt ℝ φ x := by
  simp only [show φ = fun t ↦ (1 / sqrt (2 * π)) * exp (-(t^2 / 2)) from
    funext phi_formula]
  fun_prop

/-- The standard Gaussian distribution function is real analytic. -/
theorem analyticAt_Phi (x : ℝ) : AnalyticAt ℝ Φ x := by
  let g (z : ℂ) := (1 / sqrt (2 * π) : ℝ) * Complex.exp (-(z^2 / 2))
  have hg : Differentiable ℂ g := by unfold g; fun_prop
  obtain ⟨G, hG⟩ := hg.isExactOn_univ
  have hG' (z : ℂ) : HasDerivAt G (g z) z := hG z (mem_univ z)
  have hGa := Differentiable.analyticAt (fun z ↦ (hG' z).differentiableAt : Differentiable ℂ G)
  let R (t : ℝ) := (G t).re
  have hR (t : ℝ) : HasDerivAt R (φ t) t := by
    have hd := (hG' (t : ℂ)).real_of_complex
    have he : g (t : ℂ) = (φ t : ℂ) := by
      rw [phi_formula]
      unfold g
      norm_cast
    rw [he, Complex.ofReal_re] at hd
    exact hd
  have hA : AnalyticAt ℝ R x :=
    AnalyticAt.comp (Complex.reCLM.analyticAt _) <|
      AnalyticAt.comp ((hGa (x : ℂ)).restrictScalars (𝕜 := ℝ))
        (Complex.ofRealCLM.analyticAt x)
  have hdiff (t : ℝ) : HasDerivAt (Φ - R) 0 t := by
    simpa only [Pi.sub_apply, sub_self] using (cdf_hasDerivAt t).sub (hR t)
  have hconst (t : ℝ) : Φ t = Φ 0 - R 0 + R t := by
    have h := is_const_of_deriv_eq_zero (fun u ↦ (hdiff u).differentiableAt)
      (fun u ↦ (hdiff u).deriv) t 0
    change Φ t - R t = Φ 0 - R 0 at h
    linarith
  rw [show Φ = fun t ↦ Φ 0 - R 0 + R t from funext hconst]
  exact analyticAt_const.add hA

end UnivariateML.AE4Gaussian

end
