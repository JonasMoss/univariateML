module

public import UnivariateML.Family.Discrete3Skellam
public import Mathlib.Analysis.Convex.Deriv

import Mathlib.Data.Nat.Choose.Vandermonde
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Skellam with a positive rate held

Conditional uniqueness follows from a strict Turán inequality for the
paired-count series, proved by Vandermonde’s identity.
-/

@[expose] public section

open Set Real Filter

namespace UnivariateML.HeldSweepSkellam

open Discrete3Skellam

private theorem attainedOn_nonneg {f : ℝ → ℝ} (hc : ContinuousOn f (Ici 0))
    (ht : ∃ R : ℝ, ∀ v ≥ 0, R < v → f v < f 1) :
    ∃ v ∈ Ici 0, IsMaxOn f (Ici 0) v := by
  obtain ⟨R, hR⟩ := ht
  let B := max 1 R
  have hB : 1 ≤ B := le_max_left _ _
  have hsub : Icc (0 : ℝ) B ⊆ Ici 0 := fun _ h ↦ h.1
  obtain ⟨v, hv, hm⟩ := isCompact_Icc.exists_isMaxOn
    ⟨1, zero_le_one, hB⟩ (hc.mono hsub)
  refine ⟨v, hsub hv, fun w hw ↦ ?_⟩
  by_cases h : w ≤ B
  · exact hm (show w ∈ Icc 0 B from ⟨hw, h⟩)
  · exact (hR w hw ((le_max_right 1 R).trans_lt (lt_of_not_ge h))).le.trans
      (hm (show 1 ∈ Icc 0 B from ⟨zero_le_one, hB⟩))

/-- A positive first rate held gives an attained maximum over nonnegative
second rates, with positive likelihood for every finite signed sample. -/
theorem held_first_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) :
    ∃ v ∈ Ici 0, IsMaxOn (fun w ↦ likelihood x (r, w)) (Ici 0) v := by
  have hc : ContinuousOn (likelihood x) domain :=
    continuousOn_finsetProd _ fun i _ ↦ mass_continuousOn (x i)
  have hpos : 0 < likelihood x (r, 1) :=
    Finset.prod_pos (fun i _ ↦ mass_pos (x i) hr one_pos)
  apply attainedOn_nonneg
  · exact hc.comp (continuous_const.prodMk continuous_id).continuousOn
      (fun _ hw ↦ ⟨hr.le, hw⟩)
  · obtain ⟨R, hR⟩ := likelihood_uniform_tail hn x hpos
    exact ⟨R, fun v hv hlt ↦ hR (r, v) ⟨hr.le, hv⟩ (Or.inr hlt)⟩

/-- Reflecting the counts and swapping both rates preserves the likelihood. -/
theorem likelihood_reflection {n : ℕ} (x : Fin n → ℤ) (p : ℝ × ℝ) :
    likelihood (fun i ↦ -x i) p.swap = likelihood x p := by
  unfold likelihood
  apply Finset.prod_congr rfl
  intro i _
  simp only [mass, Prod.swap, neg_neg, mul_comm]

/-- A positive second rate held gives an attained maximum over nonnegative
first rates. -/
theorem held_second_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) :
    ∃ v ∈ Ici 0, IsMaxOn (fun w ↦ likelihood x (w, r)) (Ici 0) v := by
  obtain ⟨v, hv, hm⟩ := held_first_attainment hn (fun i ↦ -x i) hr
  refine ⟨v, hv, fun w hw ↦ ?_⟩
  have hh : likelihood (fun i ↦ -x i) (r, w) ≤ likelihood (fun i ↦ -x i) (r, v) := hm hw
  change likelihood x (w, r) ≤ likelihood x (v, r)
  rw [← likelihood_reflection x (w, r), ← likelihood_reflection x (v, r)]
  exact hh

noncomputable section

private def radialTerm (k j : ℕ) (t : ℝ) : ℝ :=
  t^j/(j.factorial : ℝ)/((j+k).factorial : ℝ)

private def radialSeries (k : ℕ) (t : ℝ) : ℝ :=
  ∑' j, radialTerm k j t

private theorem radialTerm_bound (k j : ℕ) {t R : ℝ} (ht : |t| ≤ R) :
    ‖radialTerm k j t‖ ≤ R^j/(j.factorial : ℝ) := by
  have hj : 0 ≤ (j.factorial : ℝ) := by positivity
  have hk : 1 ≤ ((j+k).factorial : ℝ) := by
    exact_mod_cast Nat.factorial_pos (j+k)
  simp only [radialTerm, Real.norm_eq_abs, abs_div, abs_pow,
    abs_of_nonneg hj, abs_of_nonneg (zero_le_one.trans hk)]
  exact (div_le_self (by positivity) hk).trans
    (div_le_div_of_nonneg_right (pow_le_pow_left₀ (abs_nonneg t) ht _) hj)

private theorem radialTerm_summable (k : ℕ) (t : ℝ) :
    Summable (fun j ↦ radialTerm k j t) :=
  Summable.of_norm_bounded (Real.summable_pow_div_factorial |t|)
    (fun j ↦ radialTerm_bound k j le_rfl)

private def radialTermDeriv (k : ℕ) (t : ℝ) : ℕ → ℝ
  | 0 => 0
  | j+1 => radialTerm (k+1) j t

private theorem radialTerm_deriv (k j : ℕ) (t : ℝ) :
    HasDerivAt (radialTerm k j) (radialTermDeriv k t j) t := by
  cases j with
  | zero =>
    have he : radialTerm k 0 = fun _ : ℝ ↦ ((k.factorial : ℝ)⁻¹) := by
      funext y
      simp [radialTerm]
    rw [he, radialTermDeriv]
    exact hasDerivAt_const t _
  | succ j =>
    have h := (((hasDerivAt_id t).pow j.succ).div_const (j.succ.factorial : ℝ)).div_const
      ((j.succ+k).factorial : ℝ)
    convert h using 1
    · rfl
    · rw [radialTermDeriv]
      change t^j/(j.factorial : ℝ)/((j+(k+1)).factorial : ℝ) =
        ((j.succ : ℝ)*t^(j.succ-1)*1)/(j.succ.factorial : ℝ)/((j.succ+k).factorial : ℝ)
      rw [show j.succ+k = j+(k+1) by omega, Nat.factorial_succ, Nat.cast_mul]
      simp only [Nat.succ_sub_one, mul_one]
      field_simp [show (j.succ : ℝ) ≠ 0 by positivity]

private theorem radialSeries_deriv (k : ℕ) (t : ℝ) :
    HasDerivAt (radialSeries k) (radialSeries (k+1) t) t := by
  let R := |t|+1
  let u : ℕ → ℝ := fun j ↦ if j = 0 then 0 else R^(j-1)/((j-1).factorial : ℝ)
  have hu : Summable u := by
    apply (summable_nat_add_iff (f := u) 1).mp
    simpa [u] using Real.summable_pow_div_factorial R
  have hb (j : ℕ) (y : ℝ) (hy : y ∈ Ioo (-R) R) : ‖radialTermDeriv k y j‖ ≤ u j := by
    cases j with
    | zero => simp [radialTermDeriv, u]
    | succ j =>
      have hv : |y| ≤ R := abs_le.mpr ⟨hy.1.le, hy.2.le⟩
      simpa [radialTermDeriv, u] using radialTerm_bound (k+1) j hv
  have ht : t ∈ Ioo (-R) R := by
    dsimp [R]
    exact ⟨by linarith [neg_abs_le t], by linarith [le_abs_self t]⟩
  have h := hasDerivAt_tsum_of_isPreconnected hu isOpen_Ioo
    (convex_Ioo (-R) R).isPreconnected (fun j y _ ↦ radialTerm_deriv k j y)
    hb ht (radialTerm_summable k t) ht
  have hs : Summable (radialTermDeriv k t) := by
    apply (summable_nat_add_iff 1).mp
    exact radialTerm_summable (k+1) t
  have he : (∑' j, radialTermDeriv k t j) = radialSeries (k+1) t := by
    rw [hs.tsum_eq_zero_add]
    exact zero_add _
  rw [he] at h
  exact h

private def cauchyCoeff (k l n : ℕ) (t : ℝ) : ℝ :=
  ∑ j ∈ Finset.range (n + 1), radialTerm k j t * radialTerm l (n - j) t

private theorem choose_convolution (k l n : ℕ) :
    (∑ j ∈ Finset.range (n + 1), n.choose j * (n + k + l).choose (n - j + l)) =
      (n + (n + k + l)).choose (n + l) := by
  rw [Nat.add_choose_eq, Finset.Nat.sum_antidiagonal_eq_sum_range_succ_mk]
  calc
    _ = ∑ j ∈ Finset.range (n + 1), n.choose j * (n + k + l).choose (n + l - j) := by
      apply Finset.sum_congr rfl
      intro j hj
      have hj' := Finset.mem_range_succ_iff.mp hj
      rw [show n + l - j = n - j + l by omega]
    _ = _ := by
      apply Finset.sum_subset (Finset.range_mono (by omega))
      intro j _ hj
      have hlt : n < j := by simpa only [Finset.mem_range_succ_iff, not_le] using hj
      rw [Nat.choose_eq_zero_of_lt hlt, zero_mul]

private theorem cauchyCoeff_formula (k l n : ℕ) (t : ℝ) :
    cauchyCoeff k l n t =
      t^n / ((n.factorial : ℝ) * ((n + k + l).factorial : ℝ)) *
        ((n + (n + k + l)).choose (n + l) : ℝ) := by
  unfold cauchyCoeff
  rw [← choose_convolution k l n]
  push_cast
  rw [Finset.mul_sum]
  apply Finset.sum_congr rfl
  intro j hj
  have hj' := Finset.mem_range_succ_iff.mp hj
  have hk : n - j + l ≤ n + k + l := by omega
  have h₁ := Nat.choose_mul_factorial_mul_factorial hj'
  have h₂ := Nat.choose_mul_factorial_mul_factorial hk
  rw [show n + k + l - (n - j + l) = j + k by omega] at h₂
  have h₁' : (n.choose j : ℝ) * (j.factorial : ℝ) * ((n - j).factorial : ℝ) =
      (n.factorial : ℝ) := by exact_mod_cast h₁
  have h₂' : ((n + k + l).choose (n - j + l) : ℝ) *
      ((n - j + l).factorial : ℝ) * ((j + k).factorial : ℝ) =
      ((n + k + l).factorial : ℝ) := by exact_mod_cast h₂
  rw [radialTerm, radialTerm, ← h₁', ← h₂']
  have hpow : t^j * t^(n - j) = t^n := by rw [← pow_add, Nat.add_sub_of_le hj']
  rw [← hpow]
  have hp₁ : (n.choose j : ℝ) ≠ 0 := by exact_mod_cast (Nat.choose_pos hj').ne'
  have hp₂ : ((n + k + l).choose (n - j + l) : ℝ) ≠ 0 :=
    by exact_mod_cast (Nat.choose_pos hk).ne'
  field_simp [hp₁, hp₂, Nat.factorial_ne_zero]

private theorem radial_turan (k : ℕ) {t : ℝ} (ht : 0 ≤ t) :
    radialSeries k t * radialSeries (k + 2) t <
      radialSeries (k + 1) t * radialSeries (k + 1) t := by
  have hp (a : ℕ) : 0 ≤ (fun j ↦ radialTerm a j t) := fun j ↦ by unfold radialTerm; positivity
  have hprod (a b : ℕ) : Summable (fun ij : ℕ × ℕ ↦ radialTerm a ij.1 t * radialTerm b ij.2 t) :=
    (radialTerm_summable a t).mul_of_nonneg (radialTerm_summable b t) (hp a) (hp b)
  have hsum (a b : ℕ) : Summable (fun n ↦ cauchyCoeff a b n t) :=
    summable_sum_mul_range_of_summable_mul
      (f := fun j ↦ radialTerm a j t) (g := fun j ↦ radialTerm b j t) (hprod a b)
  have he (a b : ℕ) : radialSeries a t * radialSeries b t = ∑' n, cauchyCoeff a b n t :=
    (radialTerm_summable a t).tsum_mul_tsum_eq_tsum_sum_range (radialTerm_summable b t) (hprod a b)
  have hc (n : ℕ) :
      (((n + (n + k + (k + 2))).choose (n + (k + 2)) : ℕ) : ℝ) <
        ((n + (n + (k + 1) + (k + 1))).choose (n + (k + 1)) : ℝ) := by
    let m := n + k + 1
    have hpos : 0 < (2 * m).choose m := Nat.choose_pos (by omega)
    have hh := Nat.choose_succ_right_eq (2 * m) m
    rw [show 2 * m - m = m by omega] at hh
    have hlt : (2 * m).choose (m + 1) < (2 * m).choose m := by
      by_contra! h
      have hmul := Nat.mul_le_mul_right (m + 1) h
      rw [hh, Nat.mul_succ] at hmul
      omega
    exact_mod_cast (show (n + (n + k + (k + 2))).choose (n + (k + 2)) <
      (n + (n + (k + 1) + (k + 1))).choose (n + (k + 1)) by
        convert hlt using 2 <;> omega)
  have hd (n : ℕ) : cauchyCoeff k (k + 2) n t ≤ cauchyCoeff (k + 1) (k + 1) n t := by
    rw [cauchyCoeff_formula, cauchyCoeff_formula,
      show n + k + (k + 2) = n + (k + 1) + (k + 1) by omega]
    apply mul_le_mul_of_nonneg_left _ (by positivity)
    simpa only [show n + k + (k + 2) = n + (k + 1) + (k + 1) by omega] using (hc n).le
  rw [he, he]
  refine Summable.tsum_lt_tsum hd (i := 0) ?_ (hsum k (k + 2)) (hsum (k + 1) (k + 1))
  rw [cauchyCoeff_formula, cauchyCoeff_formula,
    show 0 + k + (k + 2) = 0 + (k + 1) + (k + 1) by omega]
  apply mul_lt_mul_of_pos_left _ (by positivity)
  simpa only [show 0 + k + (k + 2) = 0 + (k + 1) + (k + 1) by omega] using hc 0

private theorem radialSeries_pos (k : ℕ) {t : ℝ} (ht : 0 ≤ t) : 0 < radialSeries k t := by
  have hn (j : ℕ) : 0 ≤ radialTerm k j t := by unfold radialTerm; positivity
  have h0 : 0 < radialTerm k 0 t := by simp [radialTerm, Nat.factorial_pos]
  exact h0.trans_le ((radialTerm_summable k t).le_tsum 0 fun j _ ↦ hn j)

private theorem mass_nat_factor (k : ℕ) (a b : ℝ) :
    mass (k : ℤ) (a,b) = exp (-(a+b))*a^k*radialSeries k (a*b) := by
  unfold mass radialSeries
  rw [← tsum_mul_left]
  apply tsum_congr
  intro j
  simp only [Int.toNat_natCast, Int.toNat_of_nonpos (show -(k : ℤ) ≤ 0 by omega),
    Nat.add_zero, poissonMass, radialTerm, neg_add, exp_add, mul_pow, pow_add]
  ring

private theorem radial_log_strict (k : ℕ) {r : ℝ} (hr : 0 < r) :
    StrictConcaveOn ℝ (Ici 0) (fun v ↦ log (radialSeries k (r * v))) := by
  let f := fun v ↦ log (radialSeries k (r * v))
  let slope := fun v ↦ r * (radialSeries (k + 1) (r * v) / radialSeries k (r * v))
  have hrd (j : ℕ) (v : ℝ) : HasDerivAt (fun w ↦ radialSeries j (r * w))
      (r * radialSeries (j + 1) (r * v)) v := by
    have h := (radialSeries_deriv j (r * v)).comp v ((hasDerivAt_id v).const_mul r)
    simpa only [Function.comp_def, id_eq, mul_one, mul_comm] using h
  have hf (v : ℝ) (hv : 0 ≤ v) : HasDerivAt f (slope v) v := by
    convert (hrd k v).log (radialSeries_pos k (mul_nonneg hr.le hv)).ne' using 1
    simp [slope, mul_div_assoc]
  have hanti : StrictAntiOn slope (Ioi 0) := by
    apply strictAntiOn_of_deriv_neg (convex_Ioi (0 : ℝ))
    · intro v hv
      have hv' : 0 < v := hv
      exact (((hrd (k + 1) v).div (hrd k v)
        (radialSeries_pos k (mul_pos hr hv').le).ne').const_mul r).continuousAt.continuousWithinAt
    · intro v hv
      rw [interior_Ioi] at hv
      have hv' : 0 < v := hv
      have hpos := radialSeries_pos k (mul_pos hr hv').le
      have hs := ((hrd (k + 1) v).div (hrd k v) hpos.ne').const_mul r
      change HasDerivAt slope _ v at hs
      rw [hs.deriv]
      apply mul_neg_of_pos_of_neg hr
      apply div_neg_of_neg_of_pos _ (sq_pos_of_pos hpos)
      have ht := mul_lt_mul_of_pos_left (radial_turan k (mul_pos hr hv').le) hr
      rw [show k + 1 + 1 = k + 2 by omega]
      nlinarith only [ht]
  apply StrictAntiOn.strictConcaveOn_of_deriv (convex_Ici (0 : ℝ))
    (fun v hv ↦ (hf v hv).continuousAt.continuousWithinAt)
  rw [interior_Ici]
  intro v hv w hw h
  rw [(hf v (le_of_lt hv)).deriv, (hf w (le_of_lt hw)).deriv]
  exact hanti hv hw h

private theorem count_factor (k : ℤ) (r v : ℝ) :
    mass k (r, v) = exp (-(r + v)) * r^k.toNat * v^(-k).toNat *
      radialSeries k.natAbs (r * v) := by
  cases k with
  | ofNat k =>
    change mass (k : ℤ) (r, v) = exp (-(r + v)) * r^(k : ℤ).toNat *
      v^(-(k : ℤ)).toNat * radialSeries (k : ℤ).natAbs (r * v)
    rw [mass_nat_factor, Int.toNat_natCast, Int.natAbs_natCast,
      Int.toNat_of_nonpos (show -(k : ℤ) ≤ 0 by omega), pow_zero, mul_one]
  | negSucc k =>
    have h := likelihood_reflection (fun _ : Fin 1 ↦ (Int.negSucc k)) (r, v)
    simp only [likelihood, Fin.prod_univ_one, Prod.swap] at h
    rw [← h, show -(Int.negSucc k) = ((k + 1 : ℕ) : ℤ) by omega, mass_nat_factor]
    simp only [Int.toNat_natCast, Int.toNat_negSucc, Int.natAbs_negSucc, pow_zero, mul_one]
    rw [add_comm v r, mul_comm v r]

private theorem zero_likelihood_of_negative {n : ℕ} {x : Fin n → ℤ} {r : ℝ}
    (hx : ∃ i, x i < 0) :
    likelihood x (r, 0) = 0 := by
  obtain ⟨i, hi⟩ := hx
  unfold likelihood
  apply Finset.prod_eq_zero (Finset.mem_univ i)
  rw [count_factor, zero_pow (show (-x i).toNat ≠ 0 by omega)]
  ring

private theorem sample_log_strict {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) {D : Set ℝ} (hD : Convex ℝ D)
    (hsub : D ⊆ Ici 0)
    (hsafe : ∀ v ∈ D, ∀ i, 0 < v ∨ (-x i).toNat = 0) :
    StrictConcaveOn ℝ D (fun v ↦ log (likelihood x (r, v))) := by
  let g := fun (i : Fin n) (v : ℝ) ↦
    -(r + v) + ((x i).toNat : ℝ) * log r + ((-x i).toNat : ℝ) * log v +
      log (radialSeries (x i).natAbs (r * v))
  have hbase (v : ℝ) (hv : v ∈ D) (i : Fin n) : 0 < v^(-x i).toNat := by
    rcases hsafe v hv i with h | h
    · exact pow_pos h _
    · rw [h, pow_zero]
      exact one_pos
  have hm (v : ℝ) (hv : v ∈ D) (i : Fin n) : 0 < mass (x i) (r, v) := by
    rw [count_factor]
    exact mul_pos (mul_pos (mul_pos (exp_pos _) (pow_pos hr _)) (hbase v hv i))
      (radialSeries_pos _ (mul_nonneg hr.le (hsub hv)))
  have he (v : ℝ) (hv : v ∈ D) :
      log (likelihood x (r, v)) = ∑ i, g i v := by
    rw [likelihood, log_prod (fun i _ ↦ (hm v hv i).ne')]
    apply Finset.sum_congr rfl
    intro i _
    rw [count_factor,
      log_mul (mul_pos (mul_pos (exp_pos _) (pow_pos hr _)) (hbase v hv i)).ne'
        (radialSeries_pos _ (mul_nonneg hr.le (hsub hv))).ne',
      log_mul (mul_pos (exp_pos _) (pow_pos hr _)).ne' (hbase v hv i).ne',
      log_mul (exp_pos _).ne' (pow_pos hr _).ne', log_exp, log_pow, log_pow]
  refine ⟨hD, fun a ha b hb hab u v hu hv huv ↦ ?_⟩
  have hc := hD ha hb hu.le hv.le huv
  change u • log (likelihood x (r, a)) + v • log (likelihood x (r, b)) <
    log (likelihood x (r, u • a + v • b))
  rw [he a ha, he b hb, he _ hc]
  simp only [smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  apply Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  intro i _
  have hrad := (radial_log_strict (x i).natAbs hr).2 (hsub ha) (hsub hb) hab hu hv huv
  have hlog : u * (((-x i).toNat : ℝ) * log a) + v * (((-x i).toNat : ℝ) * log b) ≤
      ((-x i).toNat : ℝ) * log (u * a + v * b) := by
    by_cases h : (-x i).toNat = 0
    · simp [h]
    · have h₁ := (hsafe a ha i).resolve_right h
      have h₂ := (hsafe b hb i).resolve_right h
      have hh := strictConcaveOn_log_Ioi.concaveOn.2 h₁ h₂ hu.le hv.le huv
      simp only [smul_eq_mul] at hh
      calc
        _ = ((-x i).toNat : ℝ) * (u * log a + v * log b) := by ring
        _ ≤ _ := mul_le_mul_of_nonneg_left hh (Nat.cast_nonneg _)
  simp only [smul_eq_mul] at hrad
  dsimp only [g]
  nlinarith only [hrad, hlog, congrArg (fun t : ℝ ↦ t *
    (-r + ((x i).toNat : ℝ) * log r)) huv]

/-- Holding the first rate positive gives a unique maximizer on the closed
second-rate half-line, including the feasible Poisson endpoint. -/
theorem held_first_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) :
    ∃! v, v ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (r, w)) (Ici 0) v := by
  obtain ⟨a, ha, hma⟩ := held_first_attainment hn x hr
  have hpos {v : ℝ} (hv : IsMaxOn (fun w ↦ likelihood x (r, w)) (Ici 0) v) :
      0 < likelihood x (r, v) :=
    (Finset.prod_pos (fun i _ ↦ mass_pos (x i) hr one_pos)).trans_le
      (hv (show 1 ∈ Ici (0 : ℝ) from zero_le_one))
  refine ⟨a, ⟨ha, hma⟩, ?_⟩
  rintro b ⟨hb, hmb⟩
  by_cases hx : ∃ i, x i < 0
  · have hp {v : ℝ} (hv : v ∈ Ici (0 : ℝ))
        (hm : IsMaxOn (fun w ↦ likelihood x (r, w)) (Ici 0) v) : 0 < v := by
      by_contra! h
      have he : v = 0 := le_antisymm h hv
      exact (hpos hm).ne' (he ▸ zero_likelihood_of_negative (r := r) hx)
    have hs := sample_log_strict hn x hr (convex_Ioi (0 : ℝ))
      (fun _ h ↦ le_of_lt h) (fun _ h _ ↦ Or.inl h)
    have hmax {v : ℝ} (hm : IsMaxOn (fun w ↦ likelihood x (r, w)) (Ici 0) v) :
        IsMaxOn (fun w ↦ log (likelihood x (r, w))) (Ioi 0) v := by
      intro w hw
      exact log_le_log (Finset.prod_pos (fun i _ ↦ mass_pos (x i) hr hw)) (hm (le_of_lt hw))
    exact hs.eq_of_isMaxOn (hmax hmb) (hmax hma) (hp hb hmb) (hp ha hma)
  · have hq (i : Fin n) : (-x i).toNat = 0 := by
      push Not at hx
      exact Int.toNat_of_nonpos (by linarith [hx i])
    have hs := sample_log_strict hn x hr (convex_Ici (0 : ℝ)) (fun _ h ↦ h)
      (fun _ _ i ↦ Or.inr (hq i))
    have hmax {v : ℝ} (hm : IsMaxOn (fun w ↦ likelihood x (r, w)) (Ici 0) v) :
        IsMaxOn (fun w ↦ log (likelihood x (r, w))) (Ici 0) v := by
      intro w hw
      apply log_le_log _ (hm hw)
      apply Finset.prod_pos
      intro i _
      rw [count_factor, hq i, pow_zero, mul_one]
      exact mul_pos (mul_pos (exp_pos _) (pow_pos hr _))
        (radialSeries_pos _ (mul_nonneg hr.le hw))
    exact hs.eq_of_isMaxOn (hmax hmb) (hmax hma) hb ha

/-- Holding the second rate positive gives a unique first-rate fit. -/
theorem held_second_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) :
    ∃! v, v ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (w, r)) (Ici 0) v := by
  have h (v : ℝ) : likelihood (fun i ↦ -x i) (r, v) = likelihood x (v, r) :=
    likelihood_reflection x (v, r)
  obtain ⟨v, hv, hu⟩ := held_first_unique hn (fun i ↦ -x i) hr
  have hf : (fun v ↦ likelihood (fun i ↦ -x i) (r, v)) =
      (fun v ↦ likelihood x (v, r)) := funext h
  rw [hf] at hv hu
  exact ⟨v, hv, hu⟩

end

end UnivariateML.HeldSweepSkellam

end
