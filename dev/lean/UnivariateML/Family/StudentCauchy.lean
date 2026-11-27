module

public import UnivariateML.Family.Cauchy
public import UnivariateML.Family.TDist

import Mathlib.Tactic

set_option linter.style.header false

/-! # The shape-one Student model -/

@[expose] public section

open Set Real

namespace UnivariateML.StudentCauchy

noncomputable section

private theorem shift_eq {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    TDist.logLik x m 1 s =
      Cauchy.logLik x m s + n*(TDist.logNormalizer 1 + log π) := by
  have h (y : ℝ) :
      TDist.logNormalizer 1 - log s - (1+1)/2 * log (1+(y-m)^2/(1*s^2)) =
        Cauchy.logDensity m s y + (TDist.logNormalizer 1 + log π) := by
    have hd : 0 < (y-m)^2+s^2 := by positivity
    have hr : 1+(y-m)^2/(1*s^2) = ((y-m)^2+s^2)/s^2 := by field_simp; ring
    rw [hr, log_div hd.ne' (by positivity), log_pow, Cauchy.logDensity,
      show (m-y)^2+s^2 = (y-m)^2+s^2 by ring]
    ring
  unfold TDist.logLik Cauchy.logLik
  simp_rw [h]
  simp [Finset.sum_add_distrib, mul_add]

private theorem max_transfer {n : ℕ} (x : Fin n → ℝ) (p : ℝ × ℝ) (hp : p ∈ Cauchy.domain) :
    IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p ↔
      IsMaxOn (fun w : ℝ × ℝ ↦ Cauchy.logLik x w.1 w.2) Cauchy.domain p := by
  simp only [isMaxOn_iff]
  constructor <;> intro hm w hw <;> specialize hm w hw
  · simpa only [shift_eq x p.1 hp.2, shift_eq x w.1 hw.2, add_le_add_iff_right] using hm
  · simpa only [shift_eq x p.1 hp.2, shift_eq x w.1 hw.2, add_le_add_iff_right] using hm

/-- At df one, joint attainment requires strict half-mass counts or
the balanced two-point exception. -/
theorem lst_df_one_attains_iff {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    (∃ p ∈ Cauchy.domain, IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p) ↔
      (∀ a, 2 * TDist.multiplicity x a < n) ∨
        ∃ a b, a ≠ b ∧ 2 * TDist.multiplicity x a = n ∧ ∀ i, x i = a ∨ x i = b := by
  rw [show (∃ p ∈ Cauchy.domain,
      IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p) ↔
      ∃ p ∈ Cauchy.domain, IsMaxOn (fun w : ℝ × ℝ ↦ Cauchy.logLik x w.1 w.2) Cauchy.domain p
    from exists_congr fun p ↦ and_congr_right (max_transfer x p)]
  simpa only [TDist.multiplicity, Cauchy.multiplicity] using Cauchy.joint_exists_iff hn x

/-- At df one, the full joint fit is unique exactly with strict half
masses. The balanced two-point case is excluded by the strict inequality. -/
theorem lst_df_one_unique_iff {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    (∃! p : ℝ × ℝ, p ∈ Cauchy.domain ∧
      IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p) ↔
      ∀ a, 2 * TDist.multiplicity x a < n := by
  have h (p : ℝ × ℝ) :
      (p ∈ Cauchy.domain ∧
        IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p) ↔
      p ∈ Cauchy.domain ∧ IsMaxOn (fun w ↦ Cauchy.logLik x w.1 w.2) Cauchy.domain p := by
    exact and_congr_right (max_transfer x p)
  simpa only [h, TDist.multiplicity, Cauchy.multiplicity] using Cauchy.jointMLE_iff hn x

/-- At df one and with balanced two-point data, all positive-scale
joint maximizers lie on the open upper semicircle with those endpoints. -/
theorem lst_df_one_balanced_argmax {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {a b : ℝ} (hab : a < b) (ha : 2 * TDist.multiplicity x a = n)
    (hx : ∀ i, x i = a ∨ x i = b) (p : ℝ × ℝ) :
    (p ∈ Cauchy.domain ∧
      IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p) ↔
      a < p.1 ∧ p.1 < b ∧ 0 < p.2 ∧ p.2^2 = (p.1 - a) * (b - p.1) := by
  rw [and_congr_right (max_transfer x p)]
  exact Set.ext_iff.mp (Cauchy.balanced_joint_argmax hn x hab ha hx) p

end
end UnivariateML.StudentCauchy
end
