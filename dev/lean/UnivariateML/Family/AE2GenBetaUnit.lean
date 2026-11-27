module

public import UnivariateML.Generic.UniqueAE
import Mathlib.Analysis.SpecialFunctions.Log.Basic
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Generalized beta with held upper shape one

This is the closed upper-endpoint branch. The original free positive
shapes enter the log likelihood only through their product. The general
nonunit upper-shape almost-everywhere results are outside this module.
-/

@[expose] public section

open Set

namespace UnivariateML.AE2GenBetaUnit

noncomputable section

variable {n : ℕ}

/-- Total weighted log distance from the scale. -/
def logDistance (w : Fin n → ℕ) (x : Fin n → ℝ) (s : ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (Real.log s - Real.log (x i))

/-- The unit-upper-shape log likelihood, up to a common data-only constant. -/
def likelihood (w : Fin n → ℕ) (x : Fin n → ℝ) (s : ℝ) (p : ℝ × ℝ) : ℝ :=
  (∑ i, (w i : ℝ)) * Real.log (p.1 * p.2) - (p.1 * p.2) * logDistance w x s

private theorem rate_unique {W D : ℝ} (hW : 0 < W) (hD : 0 < D) {r : ℝ} (hr : 0 < r) :
    W * Real.log r - r * D ≤ W * Real.log (W / D) - (W / D) * D ∧
    (W * Real.log r - r * D = W * Real.log (W / D) - (W / D) * D → r = W / D) := by
  have hR := div_pos hW hD
  have hlog := Real.log_div hr.ne' hR.ne'
  have hratio : W * (r / (W / D) - 1) = r * D - W := by
    field_simp [hW.ne', hD.ne']
  have hopt : W / D * D = W := div_mul_cancel₀ W hD.ne'
  have hle := mul_le_mul_of_nonneg_left (Real.log_le_sub_one_of_pos (div_pos hr hR)) hW.le
  rw [hlog, hratio] at hle
  constructor
  · rw [hopt]
    linarith
  · intro h
    by_contra hn
    have hne : r / (W / D) ≠ 1 := by
      intro hq
      exact hn (by simpa only [one_mul] using (div_eq_iff hR.ne').mp hq)
    have hl := mul_lt_mul_of_pos_left (Real.log_lt_sub_one_of_pos (div_pos hr hR) hne) hW
    rw [hlog, hratio] at hl
    rw [hopt] at h
    linarith

private theorem total_pos (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (i : Fin n) :
    0 < ∑ i, (w i : ℝ) :=
  Finset.sum_pos' (fun j _ ↦ Nat.cast_nonneg (w j))
    ⟨i, Finset.mem_univ _, by exact_mod_cast hw i⟩

private theorem distance_pos (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (x : Fin n → ℝ)
    (hx : ∀ i, 0 < x i) (s : ℝ) (hs : ∀ i, x i ≤ s) {j : Fin n} (hj : x j < s) :
    0 < logDistance w x s := by
  apply Finset.sum_pos'
  · intro i _
    exact mul_nonneg (Nat.cast_nonneg _) (sub_nonneg.mpr (Real.log_le_log (hx i) (hs i)))
  · exact ⟨j, Finset.mem_univ _, mul_pos (by exact_mod_cast hw j)
      (sub_pos.mpr (Real.log_lt_log (hx j) hj))⟩

private theorem fixed_rate_iff (w : Fin n → ℕ) (x : Fin n → ℝ) (s : ℝ)
    (hW : 0 < ∑ i, (w i : ℝ)) (hD : 0 < logDistance w x s)
    (p : ℝ × ℝ) (hp : p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) :
    IsMaxOn (likelihood w x s) (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) p ↔
      p.1 * p.2 = (∑ i, (w i : ℝ)) / logDistance w x s := by
  set r := (∑ i, (w i : ℝ)) / logDistance w x s
  have hr : 0 < r := div_pos hW hD
  have h := rate_unique hW hD (mul_pos hp.1 hp.2)
  constructor
  · intro hmax
    apply h.2 (le_antisymm h.1 ?_)
    have hh := hmax (show (r, (1 : ℝ)) ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) from ⟨hr, one_pos⟩)
    change likelihood w x s (r, 1) ≤ likelihood w x s p at hh
    simpa only [likelihood, mul_one, r] using hh
  · intro hp' q hq
    have hh := (rate_unique hW hD (mul_pos hq.1 hq.2)).1
    change likelihood w x s q ≤ likelihood w x s p
    simpa only [likelihood, hp', r] using hh

/-- At fixed scale and unit upper shape, the whole positive maximizing set
is the ridge with rate equal to the reciprocal weighted mean log distance. -/
theorem held_scale_unit_ridge (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (s : ℝ) (hx : ∀ i, 0 < x i) (hs : ∀ i, x i < s)
    (hn : 0 < n) :
    {p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) |
      IsMaxOn (likelihood w x s) (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) p} =
    {p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) |
      p.1 * p.2 = (∑ i, (w i : ℝ)) / logDistance w x s} := by
  let j : Fin n := ⟨0, hn⟩
  ext p
  constructor
  · rintro ⟨hp, hm⟩
    exact ⟨hp, (fixed_rate_iff w x s (total_pos w hw j)
      (distance_pos w hw x hx s (fun i ↦ (hs i).le) (hs j)) p hp).1 hm⟩
  · rintro ⟨hp, he⟩
    exact ⟨hp, (fixed_rate_iff w x s (total_pos w hw j)
      (distance_pos w hw x hx s (fun i ↦ (hs i).le) (hs j)) p hp).2 he⟩

/-- When the scale is free, every unit-upper-shape winner on a nonconstant
sample uses the sample maximum as scale and the same reciprocal-mean rate. -/
theorem unit_ridge_at_max (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (hx : ∀ i, 0 < x i) (i j : Fin n) (hmax : ∀ k, x k ≤ x i)
    (hgap : x j < x i) :
    let S : Set (ℝ × ℝ × ℝ) := {z | (∀ k, x k ≤ z.1) ∧
      z.2 ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)}
    {z ∈ S | IsMaxOn (fun z ↦ likelihood w x z.1 z.2) S z} =
      {z ∈ S | z.1 = x i ∧ z.2.1 * z.2.2 = (∑ k, (w k : ℝ)) / logDistance w x (x i)} := by
  intro S
  let T : Set (ℝ × ℝ) := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
  have hpos := distance_pos w hw x hx (x i) hmax hgap
  have htotal := total_pos w hw j
  have hdom (z : ℝ × ℝ × ℝ) (hz : z ∈ S) : (x i, z.2) ∈ S := ⟨hmax, hz.2⟩
  have hcmp (z : ℝ × ℝ × ℝ) (hz : z ∈ S) :
      likelihood w x z.1 z.2 ≤ likelihood w x (x i) z.2 ∧
        (z.1 ≠ x i → likelihood w x z.1 z.2 < likelihood w x (x i) z.2) := by
    have hp : 0 < z.2.1 * z.2.2 := mul_pos hz.2.1 hz.2.2
    have hle : logDistance w x (x i) ≤ logDistance w x z.1 := by
      apply Finset.sum_le_sum
      intro k _
      exact mul_le_mul_of_nonneg_left (sub_le_sub_right (Real.log_le_log (hx i) (hz.1 i)) _)
        (Nat.cast_nonneg _)
    constructor
    · unfold likelihood
      exact sub_le_sub_left (mul_le_mul_of_nonneg_left hle hp.le) _
    · intro hne
      have hh : logDistance w x (x i) < logDistance w x z.1 := by
        apply Finset.sum_lt_sum
        · intro k _
          exact mul_le_mul_of_nonneg_left (sub_le_sub_right (Real.log_le_log (hx i) (hz.1 i)) _)
            (Nat.cast_nonneg _)
        · refine ⟨i, Finset.mem_univ _, ?_⟩
          exact mul_lt_mul_of_pos_left (sub_lt_sub_right
            (Real.log_lt_log (hx i) (lt_of_le_of_ne (hz.1 i) (Ne.symm hne))) _)
            (by exact_mod_cast hw i)
      unfold likelihood
      exact sub_lt_sub_left (mul_lt_mul_of_pos_left hh hp) _
  ext z
  constructor
  · rintro ⟨hz, hm⟩
    have he : z.1 = x i := by
      by_contra hne
      exact ((hcmp z hz).2 hne).not_ge (hm (hdom z hz))
    have hfix : IsMaxOn (likelihood w x (x i)) T z.2 := by
      intro p hp
      have hh := hm (show (x i, p) ∈ S from ⟨hmax, hp⟩)
      change likelihood w x (x i) p ≤ likelihood w x z.1 z.2 at hh
      change likelihood w x (x i) p ≤ likelihood w x (x i) z.2
      simpa only [he] using hh
    exact ⟨hz, he, (fixed_rate_iff w x (x i) htotal hpos z.2 hz.2).1 hfix⟩
  · rintro ⟨hz, he, hr⟩
    refine ⟨hz, fun q hq ↦ ?_⟩
    have hfix := (fixed_rate_iff w x (x i) htotal hpos z.2 hz.2).2 hr
    change likelihood w x q.1 q.2 ≤ likelihood w x z.1 z.2
    simpa only [he] using (hcmp q hq).1.trans (hfix hq.2)

-- The two atlas examples are the preceding ridge formulas with these rates.
example :
    (∑ _ : Fin 2, (1 : ℝ)) /
        logDistance (fun _ ↦ 1) ![(1 : ℝ) / 2, 1 / 4] 1 =
      2 / (3 * Real.log 2) := by
  have h4 : Real.log (4 : ℝ) = 2 * Real.log 2 := by
    rw [show (4 : ℝ) = 2 ^ (2 : ℕ) by norm_num, Real.log_pow]
    norm_num
  simp only [logDistance, Fin.sum_univ_two, Nat.cast_one, Matrix.cons_val_zero,
    Matrix.cons_val_one, one_mul, Real.log_one]
  rw [one_div, one_div, Real.log_inv, Real.log_inv, h4]
  congr 1 <;> ring

example :
    (∑ _ : Fin 2, (1 : ℝ)) /
        logDistance (fun _ ↦ 1) ![(1 : ℝ) / 2, 1] 1 =
      2 / Real.log 2 := by
  simp [logDistance, Fin.sum_univ_two, one_div, Real.log_inv]

end
end UnivariateML.AE2GenBetaUnit
end
