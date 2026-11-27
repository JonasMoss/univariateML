module

public import Mathlib.Analysis.Calculus.Deriv.Mul
public import Mathlib.Analysis.Calculus.LocalExtr.Rolle
public import Mathlib.Analysis.SpecialFunctions.ExpDeriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
public import Mathlib.Data.Finset.Sort

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! # Real exponential sums
The bound counts distinct zeros by the cardinality of any finite set of zeros.
-/

@[expose] public section

namespace UnivariateML.ExpSum

open scoped BigOperators

private theorem ordered_identity (s : Finset ℝ) (c : ℝ → ℝ)
    (x : Fin s.card → ℝ) (hx : StrictMono x)
    (hz : ∀ i, ∑ r ∈ s, c r * Real.exp (r * x i) = 0) :
    ∀ r ∈ s, c r = 0 := by
  classical
  induction s using Finset.induction_on generalizing c with
  | empty => simp
  | @insert a s ha ih =>
    let y : Fin (s.card + 1) → ℝ := fun i ↦ x (Fin.cast (Finset.card_insert_of_notMem ha).symm i)
    have hy : StrictMono y := fun _ _ h ↦ hx h
    let f : ℝ → ℝ := fun t ↦ c a + ∑ r ∈ s, c r * Real.exp ((r - a) * t)
    let g : ℝ → ℝ := fun t ↦ ∑ r ∈ s, (c r * (r - a)) * Real.exp ((r - a) * t)
    have hd (t : ℝ) : HasDerivAt f (g t) t := by
      apply HasDerivAt.const_add
      apply HasDerivAt.fun_sum
      intro r _
      have hlinear : HasDerivAt (fun u : ℝ ↦ (r - a) * u) ((r - a) * 1) t :=
        (hasDerivAt_id t).const_mul (r - a)
      simpa [mul_assoc, mul_comm, mul_left_comm] using hlinear.exp.const_mul (c r)
    have hf (i : Fin (s.card + 1)) : f (y i) = 0 := by
      have h := hz (Fin.cast (Finset.card_insert_of_notMem ha).symm i)
      rw [Finset.sum_insert ha] at h
      change c a * Real.exp (a * y i) +
        (∑ r ∈ s, c r * Real.exp (r * y i)) = 0 at h
      have he (r : ℝ) :
          Real.exp ((r - a) * y i) = Real.exp (r * y i) * Real.exp (-(a * y i)) := by
        rw [← Real.exp_add]
        congr 1
        ring
      simp only [f, he, ← mul_assoc, ← Finset.sum_mul]
      have hm := congrArg (fun v : ℝ ↦ v * Real.exp (-(a * y i))) h
      simpa [add_mul, mul_assoc, ← Real.exp_add] using hm
    have hrolle (i : Fin s.card) :
        ∃ t, (y i.castSucc < t ∧ t < y i.succ) ∧ g t = 0 :=
      exists_hasDerivAt_eq_zero (hy Fin.castSucc_lt_succ)
        (fun t _ ↦ (hd t).continuousAt.continuousWithinAt)
        ((hf _).trans (hf _).symm) (fun t _ ↦ hd t)
    choose z hzI hzg using hrolle
    have hzm : StrictMono z := by
      intro i j hij
      exact (hzI i).2.trans_le ((hy.monotone (show i.succ ≤ j.castSucc by
        exact Nat.succ_le_of_lt hij)).trans (hzI j).1.le)
    -- Shift the remaining rates by a. Equivalently multiply the differentiated
    -- equation by exp(a*t), keeping the original rate set for the induction.
    have hcoef : ∀ r ∈ s, c r * (r - a) = 0 := by
      apply ih (fun r ↦ c r * (r - a)) z hzm
      intro i
      have h := congrArg (fun v : ℝ ↦ v * Real.exp (a * z i)) (hzg i)
      change (∑ r ∈ s, (c r * (r - a)) * Real.exp ((r - a) * z i)) *
        Real.exp (a * z i) = 0 * _ at h
      rw [Finset.sum_mul, zero_mul] at h
      convert h using 1
      apply Finset.sum_congr rfl
      intro r _
      rw [mul_assoc (c r * (r - a)), ← Real.exp_add]
      congr 2
      ring
    have hc (r : ℝ) (hr : r ∈ s) : c r = 0 := by
      exact (mul_eq_zero.mp (hcoef r hr)).resolve_right
        (sub_ne_zero.mpr (fun h ↦ ha (h ▸ hr)))
    have hca : c a = 0 := by
      simpa [f, Finset.sum_eq_zero (fun r hr ↦ mul_eq_zero.mpr (Or.inl (hc r hr)))] using hf 0
    intro r hr
    rcases Finset.mem_insert.mp hr with rfl | hr
    · exact hca
    · exact hc r hr

/-- If there are at least as many distinct zeros as distinct rates, all
coefficients (indexed by rate, after merging) vanish. -/
theorem identity_of_card_le (s z : Finset ℝ) (c : ℝ → ℝ) (h : s.card ≤ z.card)
    (hz : ∀ t ∈ z, ∑ r ∈ s, c r * Real.exp (r * t) = 0) :
    ∀ r ∈ s, c r = 0 := by
  let x : Fin s.card ↪o ℝ := (Fin.castLEOrderEmb h).trans (z.orderEmbOfFin rfl)
  exact ordered_identity s c x x.strictMono
    (fun i ↦ hz _ (z.orderEmbOfFin_mem rfl _))

/-- A nontrivial sum has strictly fewer distinct zeros than rates. -/
theorem card_zeros_lt (s z : Finset ℝ) (c : ℝ → ℝ)
    (hc : ∃ r ∈ s, c r ≠ 0)
    (hz : ∀ t ∈ z, ∑ r ∈ s, c r * Real.exp (r * t) = 0) :
    z.card < s.card := by
  by_contra! h
  obtain ⟨r, hr, hcr⟩ := hc
  exact hcr (identity_of_card_le s z c h hz r hr)

/-- The same distinct-zero bound for real powers on the positive half-line. -/
theorem card_positive_zeros_lt (s z : Finset ℝ) (c : ℝ → ℝ)
    (hc : ∃ r ∈ s, c r ≠ 0) (hp : ∀ x ∈ z, 0 < x)
    (hz : ∀ x ∈ z, ∑ r ∈ s, c r * x ^ r = 0) :
    z.card < s.card := by
  classical
  have hinj : Set.InjOn Real.log (↑z : Set ℝ) := fun _ hx _ hy he ↦
    (Real.strictMonoOn_log.injOn (hp _ hx) (hp _ hy)) he
  rw [← Finset.card_image_of_injOn hinj]
  apply card_zeros_lt s _ c hc
  intro t ht
  obtain ⟨x, hx, rfl⟩ := Finset.mem_image.mp ht
  simpa [Real.rpow_def_of_pos (hp _ hx), mul_comm] using hz x hx

/-- Vanishing on a nonempty open interval forces all merged coefficients to vanish. -/
theorem identity_of_interval (s : Finset ℝ) (c : ℝ → ℝ) {a b : ℝ} (hab : a < b)
    (hz : ∀ t ∈ Set.Ioo a b, ∑ r ∈ s, c r * Real.exp (r * t) = 0) :
    ∀ r ∈ s, c r = 0 := by
  obtain ⟨z, hzsub, hzcard⟩ := (Set.Ioo_infinite hab).exists_subset_card_eq s.card
  exact identity_of_card_le s z c hzcard.ge (fun t ht ↦ hz t (hzsub ht))

end UnivariateML.ExpSum

end
