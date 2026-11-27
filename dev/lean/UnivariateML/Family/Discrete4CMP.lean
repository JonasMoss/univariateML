module

public import UnivariateML.Family.Discrete3CMP

public import Mathlib.Analysis.Normed.Group.Tannery

import Mathlib.Tactic

set_option linter.style.header false

/-!
# CMP held dispersion and the all-free degenerate branches

## Deviations

The all-free result covers the explicit nonattainment cases and zeros.
It does not establish attainment for the other samples.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators Classical

namespace UnivariateML.Discrete4CMP

/-- Every admissible held dispersion, including zero on the geometric
chart, has a unique maximizing rate. -/
theorem held_dispersion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {ν : ℝ} (hν : 0 ≤ ν) :
    let D := {r : ℝ | (r,ν) ∈ CMP.rateDomain}
    ∃! r, r ∈ D ∧ IsMaxOn (fun q ↦ CMP.likelihood x q ν) D r := by
  rcases eq_or_lt_of_le hν with he | he
  · subst ν
    have hD : {r : ℝ | (r,0) ∈ CMP.rateDomain} = Ico 0 1 := by
      ext r
      simp only [mem_ofPred_eq, CMP.rateDomain, le_refl, true_and, CMP.domain, lt_irrefl,
        false_or, mem_Ico]
      constructor
      · rintro (rfl | ⟨hr,hr'⟩)
        · exact ⟨le_rfl,zero_lt_one⟩
        · exact ⟨hr.le, (Real.log_neg_iff hr).mp hr'⟩
      · rintro ⟨hr,hr'⟩
        rcases eq_or_lt_of_le hr with h | h
        · exact Or.inl h.symm
        · exact Or.inr ⟨h,Real.log_neg h hr'⟩
    change ∃! r, r ∈ {r : ℝ | (r,0) ∈ CMP.rateDomain} ∧
      IsMaxOn (fun q ↦ CMP.likelihood x q 0) {r : ℝ | (r,0) ∈ CMP.rateDomain} r
    rw [hD]
    let S : ℝ := ∑ i, (x i : ℝ)
    have hS : 0 ≤ S := Finset.sum_nonneg (fun _ _ ↦ Nat.cast_nonneg _)
    have hn' : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
    have hden : 0 < (n : ℝ)+S := by positivity
    let r := S/(n+S)
    have hr : r ∈ Ico (0 : ℝ) 1 :=
      ⟨div_nonneg hS hden.le,(div_lt_one hden).mpr (by linarith)⟩
    refine ⟨r,⟨hr,(Discrete3CMP.held_zero_dispersion hn x hr).mpr rfl⟩,?_⟩
    rintro q ⟨hq,hmax⟩
    exact (Discrete3CMP.held_zero_dispersion hn x hq).mp hmax
  · have hD : {r : ℝ | (r,ν) ∈ CMP.rateDomain} = Ici 0 := by
      ext r
      simp only [mem_ofPred_eq, CMP.rateDomain, he.le, true_and, CMP.domain, he, true_or, and_true]
      constructor
      · rintro (rfl | h)
        · exact le_rfl
        · exact h.le
      · intro h
        rcases eq_or_lt_of_le h with hr | hr
        · exact Or.inl hr.symm
        · exact Or.inr hr
    change ∃! r, r ∈ {r : ℝ | (r,ν) ∈ CMP.rateDomain} ∧
      IsMaxOn (fun q ↦ CMP.likelihood x q ν) {r : ℝ | (r,ν) ∈ CMP.rateDomain} r
    rw [hD]
    exact (Discrete3CMP.held_positive_dispersion hn x he).1

/-- All zeros attain exactly on the zero-rate fiber. Any sample supported
on adjacent cells and containing a positive count has an excluded supremum. -/
theorem all_free_degenerate {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    ((∀ i, x i = 0) → ∀ p ∈ CMP.rateDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ CMP.likelihood x q.1 q.2) CMP.rateDomain p ↔ p.1 = 0) ∧
    ((∃ i, x i ≠ 0) → (∃ c, ∀ i, x i = c ∨ x i = c+1) →
      ¬ ∃ p ∈ CMP.rateDomain,
        IsMaxOn (fun q : ℝ × ℝ ↦ CMP.likelihood x q.1 q.2) CMP.rateDomain p) := by
  refine ⟨fun hx p hp ↦ CMP.all_zero_maximizers hn x hx p hp, ?_⟩
  rintro hx ⟨c,hc⟩
  exact (CMP.adjacent_full_unattained_supremum hn x hc hx).2

private theorem exposed_ray (η s b : ℝ) (F : Finset ℕ) (hne : F.Nonempty)
    (hs : ∀ k, b+s*k ≤ CMP.logFactorial k)
    (he : ∀ k, b+s*k = CMP.logFactorial k ↔ k ∈ F) (k : ℕ) :
    Tendsto (fun t : ℝ ↦ CMP.likelihood (fun _ : Fin 1 ↦ k) (exp (η+t*s)) (1+t))
      atTop (𝓝 ((if k ∈ F then CMP.weight η 1 k else 0) / ∑ j ∈ F, CMP.weight η 1 j)) := by
  classical
  let f (t : ℝ) (j : ℕ) := CMP.weight η 1 j * exp (t * (b+s*j-CMP.logFactorial j))
  let g (j : ℕ) := if j ∈ F then CMP.weight η 1 j else 0
  have hlim (j : ℕ) : Tendsto (fun t ↦ f t j) atTop (𝓝 (g j)) := by
    by_cases hf : j ∈ F
    · have H : b+s*j-CMP.logFactorial j = 0 := sub_eq_zero.mpr ((he j).mpr hf)
      simp [f,g,hf,H]
    · have H : b+s*j-CMP.logFactorial j < 0 := sub_neg.mpr
        (lt_of_le_of_ne (hs j) (fun h ↦ hf ((he j).mp h)))
      have H' := (tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg H)).const_mul
        (CMP.weight η 1 j)
      simpa [f,g,hf] using H'
  have hdom : ∀ᶠ t in atTop, ∀ j, ‖f t j‖ ≤ CMP.weight η 1 j := by
    filter_upwards [eventually_ge_atTop (0 : ℝ)] with t ht j
    have H : exp (t*(b+s*j-CMP.logFactorial j)) ≤ 1 :=
      exp_le_one_iff.mpr (mul_nonpos_of_nonneg_of_nonpos ht (sub_nonpos.mpr (hs j)))
    have H' : 0 ≤ f t j := mul_nonneg (exp_pos _).le (exp_pos _).le
    rw [Real.norm_eq_abs, abs_of_nonneg H']
    simpa [f] using mul_le_mul_of_nonneg_left H (show 0 ≤ CMP.weight η 1 j from (exp_pos _).le)
  have hsum := tendsto_tsum_of_dominated_convergence
    (CMP.summable_weight (η := η) (ν := 1) (Or.inl zero_lt_one)) hlim hdom
  have hg : (∑' j, g j) = ∑ j ∈ F, CMP.weight η 1 j := by
    rw [tsum_eq_sum (s := F) (fun j hj ↦ by simp [g,hj])]
    exact Finset.sum_congr rfl fun j hj ↦ by simp [g,hj]
  rw [hg] at hsum
  have hpos : 0 < ∑ j ∈ F, CMP.weight η 1 j :=
    Finset.sum_pos (fun _ _ ↦ exp_pos _) hne
  apply ((hlim k).div hsum hpos.ne').congr
  intro t
  have H (j : ℕ) : f t j = exp (t*b) * CMP.weight (η+t*s) (1+t) j := by
    unfold f CMP.weight
    rw [← exp_add, ← exp_add]
    congr 1
    ring
  change f t k / (∑' j, f t j) = _
  simp only [H, tsum_mul_left, CMP.likelihood, ite_eq_right (exp_pos _).ne',
    log_exp, Fin.prod_univ_one]
  rw [mul_div_mul_left _ _ (exp_pos _).ne']

/-- The exposed adjacent-cell ray has a two-point limiting distribution.
Its two positive masses are proportional to the base weights on those cells. -/
theorem adjacent_path (c : ℕ) (η : ℝ) (k : ℕ) :
    let s := log ((c : ℝ)+1)
    Tendsto (fun t : ℝ ↦ CMP.likelihood (fun _ : Fin 1 ↦ k) (exp (η+t*s)) (1+t))
      atTop (𝓝 ((if k ∈ ({c,c+1} : Finset ℕ) then CMP.weight η 1 k else 0) /
        (CMP.weight η 1 c + CMP.weight η 1 (c+1)))) := by
  dsimp
  let s := log ((c : ℝ)+1)
  let b := CMP.logFactorial c-s*c
  have hs (j : ℕ) : b+s*j ≤ CMP.logFactorial j := by
    have H := (CMP.adjacent_supporting_line c j).1
    dsimp [b,s]
    linarith only [H]
  have he (j : ℕ) : b+s*j = CMP.logFactorial j ↔ j ∈ ({c,c+1} : Finset ℕ) := by
    have H := (CMP.adjacent_supporting_line c j).2
    have H' : b+s*j = CMP.logFactorial c + log ((c : ℝ)+1) * ((j : ℝ)-c) := by
      unfold b s
      ring
    rw [H',H]
    simp
  have H := exposed_ray η s b {c,c+1} (by simp) hs he k
  simpa [Finset.sum_pair (show c ≠ c+1 by omega),s] using H

/-- A ray with slope strictly between the two adjacent supporting slopes
concentrates at the positive count between them. -/
theorem positive_point_path (c : ℕ) (hc : 0 < c) (k : ℕ) :
    let s := (log (c : ℝ)+log ((c : ℝ)+1))/2
    Tendsto (fun t : ℝ ↦ CMP.likelihood (fun _ : Fin 1 ↦ k) (exp (t*s)) (1+t))
      atTop (𝓝 (if k = c then 1 else 0)) := by
  dsimp
  let s := (log (c : ℝ)+log ((c : ℝ)+1))/2
  let b := CMP.logFactorial c-s*c
  have h₁ : log (c : ℝ) < log ((c : ℝ)+1) :=
    log_lt_log (Nat.cast_pos.mpr hc) (by linarith)
  have hf (j : ℕ) : j ≠ c → b+s*j < CMP.logFactorial j := by
    intro hj
    rcases lt_or_gt_of_ne hj with h | h
    · have H := (CMP.adjacent_supporting_line (c-1) j).1
      have He := (CMP.adjacent_supporting_line (c-1) c).2.mpr
        (Or.inr (Nat.sub_add_cancel hc).symm)
      have hcast : (((c-1 : ℕ) : ℝ)+1) = c := by rw [Nat.cast_sub hc, Nat.cast_one]; ring
      rw [hcast] at H He
      have hJ : (j : ℝ) < c := by exact_mod_cast h
      dsimp [b,s]
      nlinarith only [H, He, mul_pos (sub_pos.mpr h₁) (sub_pos.mpr hJ)]
    · have H := (CMP.adjacent_supporting_line c j).1
      have hJ : (c : ℝ) < j := by exact_mod_cast h
      dsimp [b,s]
      nlinarith only [H, mul_pos (sub_pos.mpr h₁) (sub_pos.mpr hJ)]
  have hc' : b+s*c = CMP.logFactorial c := by unfold b; ring
  have hs (j : ℕ) : b+s*j ≤ CMP.logFactorial j := by
    by_cases hj : j = c
    · exact hj.symm ▸ hc'.le
    · exact (hf j hj).le
  have he (j : ℕ) : b+s*j = CMP.logFactorial j ↔ j ∈ ({c} : Finset ℕ) := by
    by_cases hj : j = c
    · subst j
      simp [hc']
    · simp [hj,(hf j hj).ne]
  have H := exposed_ray 0 s b {c} (by simp) hs he k
  by_cases hk : k = c
  · subst k
    simpa [s, (show CMP.weight 0 1 c ≠ 0 from (exp_pos _).ne')] using H
  · simpa [s,hk] using H

end UnivariateML.Discrete4CMP

end
