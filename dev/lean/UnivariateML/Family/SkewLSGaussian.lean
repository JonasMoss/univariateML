module

public import Mathlib.Probability.CDF
public import Mathlib.Probability.Distributions.Gaussian.Real
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.Calculus.LHopital
import Mathlib.Tactic

set_option linter.style.header false

/-! # Standard Gaussian CDF facts for the skew and convolution families -/

@[expose] public section

open Set Filter Topology Real MeasureTheory ProbabilityTheory

namespace UnivariateML.SkewLSGaussian

noncomputable section

def Φ : ℝ → ℝ := cdf (gaussianReal 0 1)
def φ : ℝ → ℝ := gaussianPDFReal 0 1

theorem phi_formula (x : ℝ) : φ x = (1 / sqrt (2 * π)) * exp (-(x^2 / 2)) := by
  simp [φ, gaussianPDFReal, neg_div]

theorem phi_pos (x : ℝ) : 0 < φ x :=
  gaussianPDFReal_pos _ _ _ one_ne_zero

theorem phi_integrable : Integrable φ := integrable_gaussianPDFReal _ _

theorem phi_hasDerivAt (x : ℝ) : HasDerivAt φ (-x * φ x) x := by
  have h := (((hasDerivAt_id x).pow 2).div_const 2).neg.exp.const_mul (1 / sqrt (2 * π))
  simp only [id_eq] at h
  convert h using 1
  · ext y
    rw [phi_formula]
    rfl
  · rw [phi_formula]
    simp only [Pi.neg_apply, Pi.pow_apply, id_eq, Nat.cast_ofNat, Nat.reduceSub, pow_one]
    ring

theorem cdf_integral (x : ℝ) : Φ x = ∫ t in Iic x, φ t := by
  rw [Φ, cdf_eq_real, measureReal_def, gaussianReal_apply_eq_integral _ one_ne_zero]
  exact ENNReal.toReal_ofReal (integral_nonneg (fun t ↦ (phi_pos t).le))

theorem cdf_hasDerivAt (x : ℝ) : HasDerivAt Φ (φ x) x := by
  have hc : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  have h := intervalIntegral.integral_hasDerivAt_right (hc.intervalIntegrable 0 x)
    hc.aestronglyMeasurable.stronglyMeasurableAtFilter hc.continuousAt
  have he (t : ℝ) : Φ t = (∫ y in (0 : ℝ)..t, φ y) + Φ 0 := by
    rw [cdf_integral, cdf_integral, ← intervalIntegral.integral_Iic_sub_Iic
      phi_integrable.integrableOn phi_integrable.integrableOn]
    ring
  convert h.add_const (Φ 0) using 1
  exact funext he

theorem cdf_continuous : Continuous Φ :=
  continuous_iff_continuousAt.mpr (fun x ↦ (cdf_hasDerivAt x).continuousAt)

theorem cdf_strictMono : StrictMono Φ :=
  strictMono_of_deriv_pos fun x ↦ by rw [(cdf_hasDerivAt x).deriv]; exact phi_pos x

theorem cdf_pos (x : ℝ) : 0 < Φ x :=
  (cdf_nonneg _ _).trans_lt (cdf_strictMono (sub_lt_self x zero_lt_one))

theorem cdf_lt_one (x : ℝ) : Φ x < 1 :=
  (cdf_strictMono (lt_add_one x)).trans_le (cdf_le_one _ _)

theorem cdf_atBot : Tendsto Φ atBot (𝓝 0) := tendsto_cdf_atBot _
theorem cdf_atTop : Tendsto Φ atTop (𝓝 1) := tendsto_cdf_atTop _

theorem mills :
    Tendsto (fun y ↦ y * exp (y ^ 2 / 2) * Φ (-y)) atTop (𝓝 (1 / sqrt (2 * π))) := by
  let c := 1 / sqrt (2 * π)
  let E (y : ℝ) := exp (-(y ^ 2 / 2))
  have hE : Tendsto E atTop (𝓝 0) := by
    have h := (tendsto_id.atTop_pow₀ (show (2 : ℕ) ≠ 0 by decide) :
      Tendsto (fun x : ℝ ↦ x ^ 2) atTop atTop).atTop_mul_const (show (0 : ℝ) < 1/2 by norm_num)
    simpa [E, Function.comp_def, div_eq_mul_inv] using tendsto_exp_neg_atTop_nhds_zero.comp h
  have hEd (y : ℝ) : HasDerivAt E (-y * E y) y := by
    convert (((hasDerivAt_id y).pow 2).div_const 2).neg.exp using 1
    · rfl
    · change -y * exp (-(y ^ 2 / 2)) = exp (-(y ^ 2 / 2)) * -(2 * y ^ 1 * 1 / 2)
      ring
  have hn : Tendsto (fun y : ℝ ↦ Φ (-y)) atTop (𝓝 0) :=
    cdf_atBot.comp tendsto_neg_atTop_atBot
  have hg : ∀ᶠ y : ℝ in atTop,
      HasDerivAt (fun y ↦ E y / y) (-(E y * (1 + (y⁻¹)^2))) y := by
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with y hy
    convert (hEd y).div (hasDerivAt_id y) hy.ne' using 1
    · rfl
    · change -(E y * (1 + (y⁻¹)^2)) = ((-y * E y) * y - E y * 1) / y^2
      field_simp
      ring
  have hne : ∀ᶠ y : ℝ in atTop, -(E y * (1 + (y⁻¹)^2)) ≠ 0 :=
    Eventually.of_forall fun y ↦ neg_ne_zero.mpr
      (mul_ne_zero (exp_pos _).ne' (by positivity))
  have hg0 : Tendsto (fun y ↦ E y / y) atTop (𝓝 0) := by
    simpa [div_eq_mul_inv] using hE.mul tendsto_inv_atTop_zero
  have hratio : Tendsto (fun y ↦ -φ (-y) / -(E y * (1 + (y⁻¹)^2))) atTop (𝓝 c) := by
    have h : Tendsto (fun y : ℝ ↦ 1 + (y⁻¹)^2) atTop (𝓝 1) := by
      simpa using (tendsto_inv_atTop_zero.pow 2).const_add (1 : ℝ)
    have h' := (tendsto_const_nhds (x := c)).div h (show (1 : ℝ) ≠ 0 by norm_num)
    convert h' using 1
    · ext y
      rw [phi_formula, neg_sq]
      change -(c * E y) / -(E y * (1 + (y⁻¹)^2)) = c / (1 + (y⁻¹)^2)
      have hEy : E y ≠ 0 := (exp_pos _).ne'
      field_simp
    · simp
  have hf : ∀ᶠ y : ℝ in atTop, HasDerivAt (fun y ↦ Φ (-y)) (-φ (-y)) y :=
    Eventually.of_forall fun y ↦ by
      simpa [Function.comp_def] using (cdf_hasDerivAt (-y)).comp y (hasDerivAt_id y).neg
  refine (HasDerivAt.lhopital_zero_atTop hf hg hne hn hg0 hratio).congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with y hy
  dsimp [E]
  rw [div_div_eq_mul_div, div_eq_mul_inv, ← exp_neg, neg_neg]
  ring


theorem cdf_reflect (x : ℝ) : Φ (-x) = 1 - Φ x := by
  let G (x : ℝ) := Φ x + Φ (-x)
  have hd (x : ℝ) : HasDerivAt G 0 x := by
    have he : φ (-x) = φ x := by simp [phi_formula]
    simpa [G, Function.comp_def, he, Pi.add_def] using
      (cdf_hasDerivAt x).add ((cdf_hasDerivAt (-x)).comp x (hasDerivAt_id x).neg)
  have hm : Monotone G := monotone_of_deriv_nonneg (fun x ↦ (hd x).differentiableAt)
    (fun x ↦ by simp [(hd x).deriv])
  have ha : Antitone G := antitone_of_deriv_nonpos (fun x ↦ (hd x).differentiableAt)
    (fun x ↦ by simp [(hd x).deriv])
  have he (y : ℝ) : G y = G x := by
    rcases le_total y x with h | h
    · exact le_antisymm (hm h) (ha h)
    · exact le_antisymm (ha h) (hm h)
  have ht : Tendsto G atTop (𝓝 1) := by
    simpa [G, Function.comp_def] using cdf_atTop.add (cdf_atBot.comp tendsto_neg_atTop_atBot)
  have hc : G x = 1 := tendsto_nhds_unique
    (show Tendsto G atTop (𝓝 (G x)) from tendsto_const_nhds.congr (fun y ↦ (he y).symm)) ht
  dsimp [G] at hc
  linarith

theorem cdf_zero : Φ 0 = 1/2 := by
  have h := cdf_reflect 0
  simp only [neg_zero] at h
  linarith

theorem phi_add_mul_cdf_pos (x : ℝ) : 0 < φ x + x * Φ x := by
  have hE : Tendsto (fun y : ℝ ↦ exp (-(y ^ 2 / 2))) atTop (𝓝 0) := by
    have h := (tendsto_id.atTop_pow₀ (show (2 : ℕ) ≠ 0 by decide) :
      Tendsto (fun x : ℝ ↦ x ^ 2) atTop atTop).atTop_mul_const (show (0 : ℝ) < 1/2 by norm_num)
    simpa [Function.comp_def, div_eq_mul_inv] using tendsto_exp_neg_atTop_nhds_zero.comp h
  let G (x : ℝ) := φ x + x * Φ x
  have hg (x : ℝ) : HasDerivAt G (Φ x) x := by
    convert (phi_hasDerivAt x).add ((hasDerivAt_id x).mul (cdf_hasDerivAt x)) using 1
    · rfl
    · change Φ x = -x * φ x + (1 * Φ x + x * φ x)
      ring
  have hmon : StrictMono G := strictMono_of_deriv_pos fun x ↦ by
    rw [(hg x).deriv]
    exact cdf_pos x
  have hlim : Tendsto (fun y : ℝ ↦ G (-y)) atTop (𝓝 0) := by
    have hp : Tendsto (fun y : ℝ ↦ φ (-y)) atTop (𝓝 0) := by
      simpa [phi_formula, neg_sq] using hE.const_mul (1 / sqrt (2 * π))
    have hq : Tendsto (fun y : ℝ ↦ -y * Φ (-y)) atTop (𝓝 0) := by
      have h : Tendsto (fun y : ℝ ↦ (y * exp (y ^ 2 / 2) * Φ (-y)) *
          exp (-(y ^ 2 / 2))) atTop (𝓝 0) := by simpa using mills.mul hE
      have hn : Tendsto (fun y : ℝ ↦ -(y * exp (y ^ 2 / 2) * Φ (-y) *
          exp (-(y ^ 2 / 2)))) atTop (𝓝 0) := by simpa only [neg_zero] using h.neg
      refine hn.congr fun y ↦ ?_
      rw [show y * exp (y ^ 2 / 2) * Φ (-y) * exp (-(y ^ 2 / 2)) =
        (y * Φ (-y)) * (exp (y ^ 2 / 2) * exp (-(y ^ 2 / 2))) by ring,
        ← exp_add, add_neg_cancel, exp_zero, mul_one, neg_mul]
    simpa [G] using hp.add hq
  have he : ∀ᶠ y : ℝ in atTop, G (-y) ≤ G (x - 1) := by
    filter_upwards [eventually_ge_atTop (1 - x)] with y hy
    exact hmon.monotone (by linarith)
  exact (le_of_tendsto hlim he).trans_lt (hmon (sub_lt_self x zero_lt_one))

theorem cdf_strictLogConcave : StrictConcaveOn ℝ univ (fun x ↦ log (Φ x)) := by
  have hlog (x : ℝ) : HasDerivAt (fun x ↦ log (Φ x)) (φ x / Φ x) x :=
    (cdf_hasDerivAt x).log (cdf_pos x).ne'
  apply strictConcaveOn_univ_of_deriv2_neg (continuous_iff_continuousAt.mpr
    (fun x ↦ (hlog x).continuousAt))
  intro x
  change deriv (deriv (fun z ↦ log (Φ z))) x < 0
  rw [show deriv (fun z ↦ log (Φ z)) = (fun z ↦ φ z / Φ z) from
    funext fun z ↦ (hlog z).deriv]
  change deriv (φ / Φ) x < 0
  rw [((phi_hasDerivAt x).div (cdf_hasDerivAt x) (cdf_pos x).ne').deriv]
  apply div_neg_of_neg_of_pos _ (sq_pos_of_pos (cdf_pos x))
  have hp := mul_pos (phi_pos x) (phi_add_mul_cdf_pos x)
  nlinarith

end

end UnivariateML.SkewLSGaussian
