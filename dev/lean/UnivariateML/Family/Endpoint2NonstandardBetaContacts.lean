module

public import UnivariateML.Family.Endpoint2NonstandardBeta
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
public import Mathlib.Basic.ENNReal.Real
public import Mathlib.Basic.ENNReal.BigOperators

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Extended beta factors at a held endpoint

The free endpoint strictly contains the sample. This lets the held factors
be evaluated before a free-endpoint contact is considered.
-/

@[expose] public section

open Real Set
open scoped BigOperators ENNReal

namespace UnivariateML.Endpoint2NonstandardBetaContacts

noncomputable section

variable {n : ℕ}

/-- The closed beta factor at the held end, before multiplication by the
finite width, normalizer and free-side factors. -/
def heldFactor (z b W : ℝ) : ℝ≥0∞ :=
  if z = 0 then
    if b < 1 then ⊤ else if b = 1 then 1 else 0
  else ENNReal.ofReal ((z / W) ^ (b - 1))

/-- Extended likelihood at a strictly containing free endpoint. -/
def strictWidthLikelihood (z : Fin n → ℝ) (a b W : ℝ) : ℝ≥0∞ := by
  classical
  exact if 0 < W ∧ (∀ i, 0 ≤ z i ∧ z i < W) then
    ∏ i, (ENNReal.ofReal (exp (-Beta.logBeta (b,a)) / W * ((W - z i) / W) ^ (a - 1)) *
      heldFactor (z i) b W)
  else 0

/-- Negative distances violate support for every strictly containing
width. -/
theorem incompatible_zero (z : Fin n → ℝ) {a b W : ℝ}
    (h : ∃ i, z i < 0) : strictWidthLikelihood z a b W = 0 := by
  obtain ⟨i,hi⟩ := h
  have he : ¬ (0 < W ∧ ∀ i, 0 ≤ z i ∧ z i < W) := fun hp ↦ hi.not_ge (hp.2 i).1
  simp [strictWidthLikelihood, he]

/-- When some data touch the held endpoint, its subunit shape gives an
infinite likelihood, its superunit shape gives zero, and unit shape gives
a finite positive likelihood. These rules apply on either reflected side. -/
theorem contact_likelihood {z : Fin n → ℝ} {a b W : ℝ}
    (hW : 0 < W) (hz : ∀ i, 0 ≤ z i ∧ z i < W) (hcontact : ∃ i, z i = 0) :
    (b < 1 → strictWidthLikelihood z a b W = ⊤) ∧
      (1 < b → strictWidthLikelihood z a b W = 0) ∧
      (b = 1 → 0 < strictWidthLikelihood z a b W ∧ strictWidthLikelihood z a b W < ⊤) := by
  classical
  obtain ⟨j,hj⟩ := hcontact
  let A (i : Fin n) : ℝ≥0∞ :=
    ENNReal.ofReal (exp (-Beta.logBeta (b,a)) / W * ((W-z i)/W) ^ (a-1))
  let f := fun i ↦ A i * heldFactor (z i) b W
  have hA (i : Fin n) : 0 < A i := ENNReal.ofReal_pos.mpr
    (mul_pos (div_pos (exp_pos _) hW) (rpow_pos_of_pos (div_pos (sub_pos.mpr (hz i).2) hW) _))
  have he : strictWidthLikelihood z a b W = ∏ i, f i := by
    simp only [strictWidthLikelihood, ite_eq_left (And.intro hW hz), f, A]
  rw [he]
  refine ⟨?_, ?_, ?_⟩
  · intro h
    have hp (i : Fin n) : f i ≠ 0 := by
      apply mul_ne_zero (hA i).ne'
      by_cases hi : z i = 0
      · simp [heldFactor,hi,h]
      · have hzi : 0 < z i := lt_of_le_of_ne (hz i).1 (Ne.symm hi)
        exact (show 0 < heldFactor (z i) b W from by
          simp only [heldFactor, ite_eq_right hi]
          exact ENNReal.ofReal_pos.mpr (rpow_pos_of_pos (div_pos hzi hW) _)).ne'
    have htop : f j = ⊤ := by
      unfold f
      rw [heldFactor, ite_eq_left hj, ite_eq_left h]
      exact WithTop.mul_top (hA j).ne'
    rw [← Finset.mul_prod_erase Finset.univ f (Finset.mem_univ j), htop]
    exact WithTop.top_mul (Finset.prod_ne_zero_iff.mpr (fun i _ ↦ hp i))
  · intro h
    apply Finset.prod_eq_zero (Finset.mem_univ j)
    simp [f, heldFactor, hj, h.not_gt, h.ne']
  · intro h
    have hF (i : Fin n) : heldFactor (z i) b W = 1 := by
      by_cases hi : z i = 0 <;> simp [heldFactor,h,hi]
    have hpF (i : Fin n) : f i = A i := by simp [f,hF]
    simp_rw [hpF]
    refine ⟨bot_lt_iff_ne_bot.mpr (Finset.prod_ne_zero_iff.mpr fun i _ ↦ (hA i).ne'), ?_⟩
    apply lt_top_iff_ne_top.mpr
    rw [← ENNReal.ofReal_prod_of_nonneg]
    · exact ENNReal.ofReal_ne_top
    · intro i _
      exact mul_nonneg (div_nonneg (exp_pos _).le hW.le)
        (rpow_nonneg (div_nonneg (sub_nonneg.mpr (hz i).2.le) hW.le) _)

end

end UnivariateML.Endpoint2NonstandardBetaContacts

end
