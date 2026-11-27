module

public import UnivariateML.Family.GEV

import Mathlib.Tactic

set_option linter.style.header false

/-!
# GEV with a held nuisance coefficient

## Deviations

The statements solve the reverse-exponential member of each held-nuisance
fit and compute the positive-shape collapse path at shape one. The global
comparison over the fitted shape interval is separate.
-/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.HeldSweepGEV

open GEV

private theorem reverse_support {n : ℕ} {x : Fin n → ℝ} {m s : ℝ} (hs : 0 < s) :
    (m, s) ∈ domain x (-1) ↔ ∀ i, x i ≤ m + s := by
  change (0 < s ∧ ∀ i, (if (-1 : ℝ) = -1 then
    0 ≤ supportArg (-1) (x i) (m, s) else 0 < supportArg (-1) (x i) (m, s))) ↔ _
  simp only [ite_true, hs, true_and, supportArg, neg_one_mul, ← sub_eq_add_neg,
    sub_nonneg, div_le_iff₀ hs, one_mul, sub_le_iff_le_add, add_comm s m]

private theorem reverse_value {n : ℕ} (x : Fin n → ℝ) (m s : ℝ) :
    logLik x (-1) (m, s) =
      -(n : ℝ) * log s - n + ((∑ i, x i) - n * m) / s := by
  have h (i : Fin n) :
      -log s + (-1 / (-1 : ℝ) - 1) * log (supportArg (-1) (x i) (m, s)) -
        supportArg (-1) (x i) (m, s) ^ (-1 / (-1 : ℝ)) =
      (-log s - 1) + (x i - m) / s := by
    norm_num [supportArg]
    ring
  simp only [logLik, ite_eq_right (show (-1 : ℝ) ≠ 0 by norm_num), h, Finset.sum_add_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  rw [← Finset.sum_div, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  ring

/-- At held scale and shape minus one, the unique location puts the upper
endpoint at the sample maximum. -/
theorem reverse_held_scale {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (j : Fin n)
    (hj : ∀ i, x i ≤ x j) {s : ℝ} (hs : 0 < s) :
    ∃! m, (m, s) ∈ domain x (-1) ∧
      IsMaxOn (fun u ↦ logLik x (-1) (u, s)) {u | (u, s) ∈ domain x (-1)} m := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let m := x j - s
  have hm : (m, s) ∈ domain x (-1) :=
    (reverse_support hs).mpr (fun i ↦ by dsimp [m]; linarith [hj i])
  have hlt (u : ℝ) (hu : (u, s) ∈ domain x (-1)) (hne : u ≠ m) :
      logLik x (-1) (u, s) < logLik x (-1) (m, s) := by
    have hleft : m ≤ u := by dsimp [m]; linarith [(reverse_support hs).mp hu j]
    have hdu : m < u := lt_of_le_of_ne hleft hne.symm
    rw [reverse_value, reverse_value]
    exact add_lt_add_right ((div_lt_div_iff_of_pos_right hs).mpr
      (sub_lt_sub_left (mul_lt_mul_of_pos_left hdu hn') _)) _
  refine ⟨m, ⟨hm, fun u hu ↦ ?_⟩, ?_⟩
  · by_cases he : u = m
    · subst u
      exact le_rfl
    · exact (hlt u hu he).le
  · rintro u ⟨hu, hmax⟩
    exact by_contra fun hne ↦ (hlt u hu hne).not_ge
      (hmax (show m ∈ {u | (u, s) ∈ domain x (-1)} from hm))

/-- At held location below the maximum and no larger than the sample mean,
the reverse-exponential optimum puts the endpoint at the sample maximum. -/
theorem reverse_held_location {n : ℕ} {x : Fin n → ℝ} (j : Fin n)
    (hj : ∀ i, x i ≤ x j) {m : ℝ} (hm : m < x j)
    (havg : (n : ℝ) * m ≤ ∑ i, x i) :
    ∃! s, (m, s) ∈ domain x (-1) ∧
      IsMaxOn (fun v ↦ logLik x (-1) (m, v)) {v | (m, v) ∈ domain x (-1)} s := by
  let s := x j - m
  have hs : 0 < s := sub_pos.mpr hm
  have hn' : (0 : ℝ) < n := by exact_mod_cast (Nat.zero_lt_of_lt j.isLt)
  have hsd : (m, s) ∈ domain x (-1) :=
    (reverse_support hs).mpr (fun i ↦ by dsimp [s]; linarith [hj i])
  have hlt (v : ℝ) (hv : (m, v) ∈ domain x (-1)) (hne : v ≠ s) :
      logLik x (-1) (m, v) < logLik x (-1) (m, s) := by
    have hvp : 0 < v := hv.1
    have hle : s ≤ v := by dsimp [s]; linarith [(reverse_support hvp).mp hv j]
    have hsv : s < v := lt_of_le_of_ne hle hne.symm
    have hlog := log_lt_log hs hsv
    have hdiv := div_le_div_of_nonneg_left (sub_nonneg.mpr havg) hs hle
    rw [reverse_value, reverse_value]
    linarith [mul_lt_mul_of_neg_left hlog (neg_neg_of_pos hn')]
  refine ⟨s, ⟨hsd, fun v hv ↦ ?_⟩, ?_⟩
  · by_cases he : v = s
    · subst v
      exact le_rfl
    · exact (hlt v hv he).le
  · rintro v ⟨hv, hmax⟩
    exact by_contra fun hne ↦ (hlt v hv hne).not_ge
      (hmax (show s ∈ {v | (m, v) ∈ domain x (-1)} from hsd))

private noncomputable def collapseResidual (d s : ℝ) : ℝ :=
  if d = 0 then -1 else -2 * log (s + d) - s / (s + d)

private theorem collapse_formula {n : ℕ} (x : Fin n → ℝ) {m s : ℝ}
    (hm : ∀ i, m ≤ x i) (hs : 0 < s) :
    let c := (Finset.univ.filter (fun i ↦ x i = m)).card
    logLik x 1 (m, s) =
      ((n : ℝ) - 2 * c) * log s + ∑ i, collapseResidual (x i - m) s := by
  dsimp only
  have h (i : Fin n) :
      (if (1 : ℝ) = 0 then -log s - (x i - m) / s - exp (-((x i - m) / s))
        else -log s + (-1 / (1 : ℝ) - 1) * log (supportArg 1 (x i) (m, s)) -
          supportArg 1 (x i) (m, s) ^ (-1 / (1 : ℝ))) =
      (if x i = m then -log s else log s) + collapseResidual (x i - m) s := by
    have hd : 0 < s + (x i - m) := add_pos_of_pos_of_nonneg hs (sub_nonneg.mpr (hm i))
    have he : supportArg 1 (x i) (m, s) = (s + (x i - m)) / s := by
      unfold supportArg
      field_simp
    by_cases hx : x i = m
    · simp [hx, collapseResidual, supportArg, sub_eq_add_neg]
    · simp only [one_ne_zero, ite_false, he, div_one, rpow_neg_one, inv_div,
        log_div hd.ne' hs.ne', collapseResidual, sub_ne_zero.mpr hx, hx]
      ring
  simp only [logLik, h, Finset.sum_add_distrib]
  congr 1
  have hterm (i : Fin n) :
      (if x i = m then -log s else log s) = log s - (if x i = m then 2 * log s else 0) := by
    split_ifs <;> ring
  simp only [hterm, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← Finset.sum_filter]
  ring

private theorem collapse_residual_limit {n : ℕ} (x : Fin n → ℝ) {m : ℝ} (hm : ∀ i, m ≤ x i) :
    Tendsto (fun s ↦ ∑ i, collapseResidual (x i - m) s) (𝓝[>] (0 : ℝ))
      (𝓝 (∑ i, if x i = m then (-1 : ℝ) else -2 * log (x i - m))) := by
  apply tendsto_finsetSum
  intro i _
  unfold collapseResidual
  by_cases he : x i = m
  · simp [he]
  · have hp : 0 < x i - m := sub_pos.mpr (lt_of_le_of_ne (hm i) (Ne.symm he))
    have hc : ContinuousAt (fun s : ℝ ↦ -2 * log (s + (x i - m)) - s / (s + (x i - m))) 0 := by
      fun_prop (disch := simpa using hp.ne')
    have ht := hc.continuousWithinAt (s := Ioi 0)
    change Tendsto _ _ (𝓝 (-2 * log (0 + (x i - m)) - 0 / (0 + (x i - m)))) at ht
    simpa only [sub_ne_zero.mpr he, he, ite_false, zero_add, zero_div, sub_zero]
      using ht

/-- At held sample minimum, a strict majority of zero residuals yields an
unbounded likelihood along the feasible shape-one path. -/
theorem held_minimum_majority_collapse {n : ℕ} (x : Fin n → ℝ) {m : ℝ}
    (hm : ∀ i, m ≤ x i)
    (hc : n < 2 * (Finset.univ.filter (fun i ↦ x i = m)).card) :
    (∀ s > 0, (m, s) ∈ domain x 1) ∧
      Tendsto (fun s ↦ logLik x 1 (m, s)) (𝓝[>] (0 : ℝ)) atTop := by
  refine ⟨fun s hs ↦ ⟨hs, ?_⟩, ?_⟩
  · intro i
    simp only [supportArg, one_mul, ite_eq_right (show (1 : ℝ) ≠ -1 by norm_num)]
    exact add_pos_of_pos_of_nonneg one_pos (div_nonneg (sub_nonneg.mpr (hm i)) hs.le)
  · let c := (Finset.univ.filter (fun i ↦ x i = m)).card
    have hc' : (n : ℝ) < 2 * (c : ℝ) := by exact_mod_cast hc
    have ht := (tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg (sub_neg.mpr hc')).atTop_add
      (collapse_residual_limit x hm)
    apply ht.congr'
    filter_upwards [eventually_mem_nhdsWithin] with s (hs : 0 < s)
    exact (collapse_formula x hm hs).symm

/-- At exact half contact, the shape-one held-location collapse reaches
the signed finite value of the joint density on the log scale. -/
theorem held_minimum_half_path {n : ℕ} (x : Fin n → ℝ) {m : ℝ}
    (hm : ∀ i, m ≤ x i)
    (hc : n = 2 * (Finset.univ.filter (fun i ↦ x i = m)).card) :
    Tendsto (fun s ↦ logLik x 1 (m, s)) (𝓝[>] (0 : ℝ))
      (𝓝 (∑ i, if x i = m then (-1 : ℝ) else -2 * log (x i - m))) := by
  apply (collapse_residual_limit x hm).congr'
  filter_upwards [eventually_mem_nhdsWithin] with s (hs : 0 < s)
  have h : (n : ℝ) = 2 * ((Finset.univ.filter (fun i ↦ x i = m)).card : ℝ) := by
    exact_mod_cast hc
  rw [collapse_formula x hm hs, h, sub_self, zero_mul, zero_add]

end UnivariateML.HeldSweepGEV

end
