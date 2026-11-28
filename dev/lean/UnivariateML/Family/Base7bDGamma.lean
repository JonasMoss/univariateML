module

public import UnivariateML.Family.Base7DGamma
import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Analysis.SpecialFunctions.Log.NegMulLog

set_option linter.style.header false

/-! Exact arithmetic in the two-cell discretized Gamma trace test.

## Deviations
The numeric trace expression is strictly below one on the stated rectangle.
The statistical inequality bounding the actual standardized covariance by
this expression, and the analytic profile reduction, remain separate.
-/

@[expose] public section

namespace UnivariateML.Base7bDGamma

open Filter Set Real
open scoped Topology

/-- On two occupied cells `0` and `K`, the proposed sample-only trace
upper bound is strictly below one throughout the explicit histogram class.
The deviance supplies a positive cutoff strictly below one third. -/
theorem two_cell_trace_arithmetic {K : ℕ} (hK : 2 ^ 28 ≤ K)
    {w : ℝ} (hw₀ : 1 / 4 ≤ w) (hw₁ : w ≤ 3 / 4) :
    let m := (1 - w) * (K : ℝ)
    let δ := log (w + m) - (1 - w) * log (K : ℝ)
    let A := 1 / δ
    let B := max |1 / (K : ℝ) - 1 / (m + 1)| |1 / ((K : ℝ) + 1) - 1 / m|
    let G := (1 + (1 + A)⁻¹ ^ 2) / 2
    3 < δ ∧ A ∈ Set.Ioo 0 (1 / 3) ∧
      (w + (A ^ 2 / 4) * (1 - w) * B ^ 2) / G + A / (4 * m ^ 2) < 1 := by
  intro m δ A B G
  have hKr : (2 : ℝ) ^ 28 ≤ (K : ℝ) := by exact_mod_cast hK
  have hKlarge : (100 : ℝ) ≤ K := (by norm_num : (100 : ℝ) ≤ 2 ^ 28).trans hKr
  have hKp : 0 < (K : ℝ) := by linarith
  have hw : 0 < 1 - w := by linarith
  have hwp : 0 ≤ w := by linarith
  have hm : 0 < m := mul_pos hw hKp
  have hlog := log_le_log (by positivity : 0 < (2 : ℝ) ^ 28) hKr
  rw [log_pow] at hlog
  have hlogm := log_le_log hm (show m ≤ w + m by linarith)
  change log ((1 - w) * (K : ℝ)) ≤ _ at hlogm
  rw [log_mul hw.ne' hKp.ne'] at hlogm
  have hlogw := log_le_log (by norm_num : (0 : ℝ) < 1 / 4)
    (show (1 : ℝ) / 4 ≤ 1 - w by linarith)
  rw [log_div one_ne_zero (by norm_num : (4 : ℝ) ≠ 0), log_one, zero_sub,
    show (4 : ℝ) = 2 ^ (2 : ℕ) by norm_num, log_pow] at hlogw
  norm_num only [Nat.cast_ofNat] at hlog hlogw
  have hl2 : 0 < log (2 : ℝ) := log_pos (by norm_num)
  have hlogK : 0 ≤ log (K : ℝ) := by linarith
  have hdev : 3 < δ := by
    have H := mul_le_mul_of_nonneg_right hw₀ hlogK
    dsimp [δ]
    nlinarith only [hlog, hlogm, hlogw, H, log_two_gt_d9]
  have hA : A ∈ Set.Ioo 0 (1 / 3) :=
    ⟨one_div_pos.mpr (by linarith), one_div_lt_one_div_of_lt (by norm_num) hdev⟩
  have hG : (25 : ℝ) / 32 ≤ G := by
    have hI : (3 : ℝ) / 4 ≤ (1 + A)⁻¹ := by
      rw [← one_div, le_div_iff₀ (by linarith [hA.1])]
      linarith [hA.2]
    have H := pow_le_pow_left₀ (by norm_num : (0 : ℝ) ≤ 3 / 4) hI 2
    dsimp [G]
    rw [show ((3 : ℝ) / 4) ^ 2 = 9 / 16 by norm_num] at H
    linarith
  have hmp : (K : ℝ) / 4 ≤ m := by
    dsimp [m]
    nlinarith [mul_nonneg (show 0 ≤ 3 / 4 - w by linarith) hKp.le]
  have hm25 : 25 ≤ m := by linarith
  have hB₀ : 0 ≤ B := (abs_nonneg _).trans (le_max_left _ _)
  have hB : B ≤ 1 / 20 := by
    have hb₁ : 1 / (K : ℝ) ≤ 1 / 100 := one_div_le_one_div_of_le (by norm_num) hKlarge
    have hb₂ : 1 / ((K : ℝ) + 1) ≤ 1 / 100 :=
      one_div_le_one_div_of_le (by norm_num) (by linarith)
    have hb₃ : 1 / m ≤ 1 / 25 := one_div_le_one_div_of_le (by norm_num) hm25
    have hb₄ : 1 / (m + 1) ≤ 1 / 25 :=
      one_div_le_one_div_of_le (by norm_num) (by linarith)
    have hmain (a b : ℝ) (ha : 0 ≤ a) (hb : 0 ≤ b)
        (ha' : a ≤ 1 / 100) (hb' : b ≤ 1 / 25) : |a - b| ≤ 1 / 20 :=
      abs_le.mpr ⟨by linarith, by linarith⟩
    exact max_le (hmain _ _ (by positivity) (by positivity) hb₁ hb₄)
      (hmain _ _ (by positivity) (by positivity) hb₂ hb₃)
  refine ⟨hdev, hA, ?_⟩
  have hnum : w + (A ^ 2 / 4) * (1 - w) * B ^ 2 ≤ (3 : ℝ) / 4 + 1 / 14400 := by
    have hprod : (A ^ 2 / 4) * (1 - w) * B ^ 2 ≤ (1 : ℝ) / 14400 := calc
      _ ≤ ((1 / 3 : ℝ) ^ 2 / 4) * 1 * (1 / 20 : ℝ) ^ 2 := by
        gcongr
        · exact hA.2.le
        · linarith
      _ = _ := by norm_num
    linarith
  have hleft : (w + (A ^ 2 / 4) * (1 - w) * B ^ 2) / G ≤
      ((3 : ℝ) / 4 + 1 / 14400) / (25 / 32) := by gcongr
  have hm2 : (25 : ℝ) ^ 2 ≤ m ^ 2 := pow_le_pow_left₀ (by norm_num) hm25 2
  have hright : A / (4 * m ^ 2) ≤ (1 / 3 : ℝ) / (4 * 25 ^ 2) := by
    gcongr
    exact hA.2.le
  norm_num at hleft hright
  linarith

/-- The written zero-cell moment bound, once established for the
normalized shape score, gives a strict positive small-shape collar.
The digamma term only needs continuity at one in this limit step. -/
theorem small_shape_score_pos_of_moment_bound (P ψ : ℝ → ℝ)
    {m w : ℝ} (_hm : 0 < m) (hw : w < 1) (hψ : ContinuousAt ψ 1)
    (hbound : ∀ a > 0, log a - log (m + 1) +
      (1 - w * exp (a / m)) / a - ψ (1 + a) ≤ deriv P a) :
    ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < deriv P a := by
  let f : ℝ → ℝ := fun a ↦ a * log a - a * log (m + 1) +
    (1 - w * exp (a / m)) - a * ψ (1 + a)
  have hc : ContinuousAt f 0 := by
    have ha : ContinuousAt (fun a : ℝ ↦ a) 0 := continuousAt_id
    have hs : ContinuousAt (fun a : ℝ ↦ ψ (1 + a)) 0 := by
      have H : ContinuousAt (fun a : ℝ ↦ 1 + a) 0 := by fun_prop
      exact (show ContinuousAt ψ (1 + (0 : ℝ)) by simpa using hψ).comp H
    exact (((continuous_mul_log.continuousAt.sub (ha.mul continuousAt_const)).add
      (continuousAt_const.sub (continuousAt_const.mul (by fun_prop)))).sub (ha.mul hs))
  have hf : Tendsto f (𝓝 0) (𝓝 (1 - w)) := by
    have h0 : f 0 = 1 - w := by simp [f]
    rw [← h0]
    exact hc.tendsto
  have he : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < f a :=
    nhdsWithin_le_nhds (hf.eventually (Ioi_mem_nhds (sub_pos.mpr hw)))
  filter_upwards [he, show ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a from self_mem_nhdsWithin] with a ha hapos
  have H := mul_le_mul_of_nonneg_left (hbound a hapos) hapos.le
  have heq : f a = a * (log a - log (m + 1) +
      (1 - w * exp (a / m)) / a - ψ (1 + a)) := by
    dsimp [f]
    field_simp [hapos.ne']
  rw [← heq] at H
  exact pos_of_mul_pos_right (ha.trans_le H) hapos.le

end UnivariateML.Base7bDGamma

end
