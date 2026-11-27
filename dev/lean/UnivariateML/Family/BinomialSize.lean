module

public import Mathlib.Analysis.Calculus.Deriv.Slope
public import Mathlib.Analysis.Calculus.LHopital
public import Mathlib.Analysis.Convex.Jensen
public import Mathlib.Analysis.Convex.Mul
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecificLimits.Basic
public import Mathlib.Data.Nat.Choose.Basic
public import Mathlib.Data.Nat.Factorial.BigOperators
public import UnivariateML.Family.Discrete
public import UnivariateML.Generic.Limit
public import UnivariateML.Generic.Profile

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Binomial likelihood with both parameters free

The profile is written in inverse size. Its continuous extension at inverse
size zero is the Poisson log-likelihood at the empirical mean.

## Deviations

The first-order coefficient for the total log-likelihood is
`m * (mean - variance) / 2`, with the opposite sign to that in the task brief.
-/

@[expose] public section

open scoped BigOperators Topology
open Finset Filter Set Real

namespace UnivariateML.BinomialSize

noncomputable section

def mean {m : ℕ} (x : Fin m → ℕ) : ℝ := (∑ i, (x i : ℝ)) / m

def variance {m : ℕ} (x : Fin m → ℕ) : ℝ :=
  (∑ i, ((x i : ℝ) - mean x) ^ 2) / m

def feasible {m : ℕ} (x : Fin m → ℕ) : Set ℕ := {N | ∀ i, x i ≤ N}

def poissonLogLik {m : ℕ} (x : Fin m → ℕ) : ℝ :=
  ∑ i, ((x i : ℝ) * log (mean x) - log ((x i).factorial : ℝ) - mean x)

def entropyRemainder (z : ℝ) : ℝ := (1 - z) * log (1 - z) + z

private theorem entropyRemainder_deriv {z : ℝ} (hz : z ≠ 1) :
    HasDerivAt entropyRemainder (-log (1 - z)) z := by
  have h := (hasDerivAt_id z).const_sub 1
  convert (h.mul (h.log (sub_ne_zero.mpr (Ne.symm hz)))).add (hasDerivAt_id z) using 1
  · rfl
  · simp only [id_eq]
    field_simp
    ring

private theorem entropy_second_order :
    Tendsto (fun z : ℝ ↦ entropyRemainder z / z ^ 2) (𝓝[≠] 0) (𝓝 (1 / 2)) := by
  have hl : HasDerivAt (fun z : ℝ ↦ -log (1 - z)) 1 0 := by
    simpa only [id_eq, sub_zero, div_one, neg_neg, Pi.neg_def] using
      (((hasDerivAt_id (0 : ℝ)).const_sub 1).log (by norm_num)).neg
  have hs := hl.tendsto_slope
  rw [slope_fun_def_field] at hs
  simp only [sub_zero, log_one, neg_zero] at hs
  have he := entropyRemainder_deriv (z := 0) (by norm_num)
  refine HasDerivAt.lhopital_zero_nhdsNE (f' := fun z ↦ -log (1 - z))
    (g' := fun z ↦ 2 * z) ?_ (.of_forall fun z ↦ by simpa using hasDerivAt_pow 2 z)
    ?_ ?_ ?_ ?_
  · filter_upwards [eventually_ne_nhds (show (0 : ℝ) ≠ 1 by norm_num) |>.filter_mono
      nhdsWithin_le_nhds] with z hz
    exact entropyRemainder_deriv hz
  · filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := {0}ᶜ)] with z hz
    exact mul_ne_zero (by norm_num) hz
  · simpa [ContinuousWithinAt, entropyRemainder] using
      he.continuousAt.continuousWithinAt (s := ({0}ᶜ : Set ℝ))
  · simpa only [zero_pow (by decide : 2 ≠ 0), id_eq] using
      ((tendsto_id : Tendsto (id : ℝ → ℝ) (𝓝 0) (𝓝 0)).mono_left
        nhdsWithin_le_nhds).pow 2
  · convert hs.div_const 2 using 1
    ext z
    ring

private theorem hasDerivAt_entropy_quotient (a : ℝ) :
    HasDerivAt (fun t : ℝ ↦ entropyRemainder (a * t) / t) (a ^ 2 / 2) 0 := by
  by_cases ha : a = 0
  · simpa [ha, entropyRemainder] using hasDerivAt_const (0 : ℝ) (0 : ℝ)
  have hm : Tendsto (fun t : ℝ ↦ a * t) (𝓝[≠] 0) (𝓝[≠] 0) := by
    refine tendsto_nhdsWithin_iff.mpr ⟨?_, ?_⟩
    · have hc : Continuous (fun t : ℝ ↦ a * t) := continuous_const.mul continuous_id
      simpa [ContinuousWithinAt] using
        hc.continuousAt (x := 0) |>.continuousWithinAt (s := {0}ᶜ)
    · filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := {0}ᶜ)] with t ht
      exact mul_ne_zero ha ht
  rw [hasDerivAt_iff_tendsto_slope, slope_fun_def_field]
  convert (entropy_second_order.comp hm).const_mul (a ^ 2) using 1
  · ext t
    simp only [Function.comp_apply, mul_zero, div_zero, sub_zero]
    field_simp
  · congr 1
    ring

def inverseProfile {m : ℕ} (x : Fin m → ℕ) (t : ℝ) : ℝ :=
  poissonLogLik x + (∑ i, ∑ j ∈ range (x i), log (1 - (j : ℝ) * t)) +
    (m : ℝ) * (entropyRemainder (mean x * t) / t)

def profile {m : ℕ} (x : Fin m → ℕ) (N : ℕ) : ℝ :=
  inverseProfile x (N : ℝ)⁻¹

private theorem sum_range_cast (k : ℕ) :
    (∑ j ∈ range k, (j : ℝ)) = (k : ℝ) * ((k : ℝ) - 1) / 2 := by
  induction k with
  | zero => simp
  | succ k ih =>
    rw [sum_range_succ, ih, Nat.cast_succ]
    ring

private theorem mean_mul {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    (m : ℝ) * mean x = ∑ i, (x i : ℝ) := by
  unfold mean
  have hm' : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  field_simp

private theorem variance_mul {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    (m : ℝ) * variance x = ∑ i, (x i : ℝ) ^ 2 - m * (mean x) ^ 2 := by
  have hm' : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  unfold variance
  rw [mul_div_cancel₀ _ hm']
  have h : (∑ i, ((x i : ℝ) - mean x) ^ 2) =
      ∑ i, ((x i : ℝ) ^ 2 - 2 * mean x * x i + (mean x) ^ 2) := by
    apply sum_congr rfl
    intro i _
    ring
  rw [h, sum_add_distrib, sum_sub_distrib, ← mul_sum, ← mean_mul x hm]
  simp
  ring

private theorem factorialMoment {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    (∑ i, ∑ j ∈ range (x i), (j : ℝ)) =
      (m : ℝ) * (variance x + (mean x) ^ 2 - mean x) / 2 := by
  simp only [sum_range_cast, mul_sub_one, ← sum_div, sum_sub_distrib, ← pow_two]
  have hv := variance_mul x hm
  rw [← mean_mul x hm]
  linarith only [hv, show (m : ℝ) * (variance x + (mean x) ^ 2 - mean x) =
    m * variance x + m * (mean x) ^ 2 - m * mean x by ring]

/-- First-order coefficient at the Poisson face, for total log-likelihood. -/
theorem inverseProfile_hasDerivAt {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    HasDerivAt (inverseProfile x) ((m : ℝ) * (mean x - variance x) / 2) 0 := by
  have hl (j : ℕ) : HasDerivAt (fun t : ℝ ↦ log (1 - (j : ℝ) * t)) (-(j : ℝ)) 0 := by
    simpa using (((hasDerivAt_id (0 : ℝ)).const_mul (j : ℝ)).const_sub 1).log (by simp)
  have hs := HasDerivAt.fun_sum (u := Finset.univ) (fun i (_ : i ∈ Finset.univ) ↦
    HasDerivAt.fun_sum (u := range (x i)) (fun j (_ : j ∈ range (x i)) ↦ hl j))
  have h := ((hasDerivAt_const (0 : ℝ) (poissonLogLik x)).add hs).add
    ((hasDerivAt_entropy_quotient (mean x)).const_mul (m : ℝ))
  convert h using 1
  · rfl
  · simp only [zero_add, sum_neg_distrib, factorialMoment x hm]
    ring

@[simp] theorem inverseProfile_zero {m : ℕ} (x : Fin m → ℕ) :
    inverseProfile x 0 = poissonLogLik x := by simp [inverseProfile, entropyRemainder]

/-- The integer-size profile converges to the Poisson log-likelihood at the mean. -/
theorem profile_tendsto_poisson {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    Tendsto (profile x) atTop (𝓝 (poissonLogLik x)) := by
  change Tendsto (fun N : ℕ ↦ inverseProfile x (N : ℝ)⁻¹) atTop _
  simpa only [Function.comp_def, inverseProfile_zero] using
    (inverseProfile_hasDerivAt x hm).continuousAt.tendsto.comp
      (tendsto_inv_atTop_nhds_zero_nat (𝕜 := ℝ))

private theorem eventually_feasible {m : ℕ} (x : Fin m → ℕ) :
    ∀ᶠ N in atTop, N ∈ feasible x := by
  refine (eventually_ge_atTop (∑ i, x i)).mono fun N hN i ↦ ?_
  exact (single_le_sum (f := x) (fun _ _ ↦ Nat.zero_le _) (mem_univ i)).trans hN

/-- Underdispersion makes all sufficiently large sizes beat the Poisson value. -/
theorem eventually_poisson_lt_profile {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hv : variance x < mean x) :
    ∀ᶠ N in atTop, poissonLogLik x < profile x N := by
  have hd : 0 < (m : ℝ) * (mean x - variance x) / 2 := by positivity
  have ht : Tendsto (fun N : ℕ ↦ (N : ℝ)⁻¹) atTop (𝓝[≠] (0 : ℝ)) := by
    refine tendsto_nhdsWithin_iff.mpr ⟨tendsto_inv_atTop_nhds_zero_nat, ?_⟩
    filter_upwards [eventually_ge_atTop (1 : ℕ)] with N hN
    have h : 0 < (N : ℝ) := by exact_mod_cast (show 0 < N by omega)
    exact (inv_pos.mpr h).ne'
  have hs := ((inverseProfile_hasDerivAt x hm).tendsto_slope.comp ht).eventually
    (eventually_gt_nhds hd)
  filter_upwards [hs, eventually_ge_atTop (1 : ℕ)] with N hs hN
  have h : 0 < (N : ℝ) := by exact_mod_cast (show 0 < N by omega)
  change 0 < slope (inverseProfile x) 0 (N : ℝ)⁻¹ at hs
  rw [slope_def_field, sub_zero, inverseProfile_zero] at hs
  exact sub_pos.mp ((div_pos_iff_of_pos_right (inv_pos.mpr h)).mp hs)

/-- A profile value strictly above the face implies a finite nonempty maximizing set. -/
private theorem finite_of_beats {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) {N : ℕ}
    (hN : N ∈ feasible x) (hbeat : poissonLogLik x < profile x N) :
    {K ∈ feasible x | IsMaxOn (profile x) (feasible x) K}.Nonempty ∧
      {K ∈ feasible x | IsMaxOn (profile x) (feasible x) K}.Finite := by
  classical
  obtain ⟨B, hB⟩ := eventually_atTop.mp
    ((profile_tendsto_poisson x hm).eventually (eventually_lt_nhds hbeat))
  let s := (range B).filter (fun K ↦ K ∈ feasible x)
  have hNs : N ∈ s := mem_filter.mpr ⟨mem_range.mpr (by
    by_contra! h
    exact (lt_irrefl _) (hB N h)), hN⟩
  obtain ⟨K, hKs, hK⟩ := exists_max_image s (profile x) ⟨N, hNs⟩
  have hmax : IsMaxOn (profile x) (feasible x) K := by
    intro J hJ
    by_cases hJB : J < B
    · exact hK J (mem_filter.mpr ⟨mem_range.mpr hJB, hJ⟩)
    · exact (hB J (le_of_not_gt hJB)).le.trans (hK N hNs)
  refine ⟨⟨K, (mem_filter.mp hKs).2, hmax⟩, (finite_Iio B).subset ?_⟩
  intro J hJ
  exact lt_of_not_ge fun hJB ↦ (not_lt_of_ge (hJ.2 hN)) (hB J hJB)

/-- Underdispersed samples have a nonempty finite set of maximizing sizes. -/
theorem finite_profile_maximizers_of_underdispersion {m : ℕ} (x : Fin m → ℕ)
    (hm : 0 < m) (hv : variance x < mean x) :
    {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N}.Nonempty ∧
      {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N}.Finite := by
  obtain ⟨N, hN, hbeat⟩ :=
    ((eventually_feasible x).and (eventually_poisson_lt_profile x hm hv)).exists
  exact finite_of_beats x hm hN hbeat

private theorem log_choose_scaled {N k : ℕ} (hN : 0 < N) (hk : k ≤ N) :
    log (N.choose k : ℝ) = (∑ j ∈ range k, log (1 - (j : ℝ) * (N : ℝ)⁻¹)) +
      (k : ℝ) * log N - log (k.factorial : ℝ) := by
  have hNr : 0 < (N : ℝ) := by exact_mod_cast hN
  have hc : 0 < (N.choose k : ℝ) := by exact_mod_cast Nat.choose_pos hk
  have hf : 0 < (k.factorial : ℝ) := by positivity
  have hd := congrArg (fun d : ℕ ↦ (d : ℝ)) (Nat.descFactorial_eq_factorial_mul_choose N k)
  push_cast at hd
  have hp (j : ℕ) (hj : j ∈ range k) : 0 < (N : ℝ) - j := by
    rw [sub_pos]
    exact_mod_cast (mem_range.mp hj |>.trans_le hk)
  have hl : (∑ j ∈ range k, log ((N : ℝ) - j)) = log (k.factorial : ℝ) + log (N.choose k : ℝ) := by
    rw [← log_mul hf.ne' hc.ne', ← hd, Nat.descFactorial_eq_prod_range, Nat.cast_prod,
      log_prod (fun j hj ↦ by
        rw [Nat.cast_sub ((mem_range.mp hj).le.trans hk)]
        exact (hp j hj).ne')]
    exact sum_congr rfl fun j hj ↦ by rw [Nat.cast_sub ((mem_range.mp hj).le.trans hk)]
  have he : (∑ j ∈ range k, log ((N : ℝ) - j)) =
      (∑ j ∈ range k, log (1 - (j : ℝ) * (N : ℝ)⁻¹)) + (k : ℝ) * log N := by
    have hconst : (∑ _j ∈ range k, log (N : ℝ)) = (k : ℝ) * log N := by
      simp [nsmul_eq_mul]
    rw [← hconst, ← sum_add_distrib]
    apply sum_congr rfl
    intro j hj
    have h : 1 - (j : ℝ) * (N : ℝ)⁻¹ = ((N : ℝ) - j) / N := by
      field_simp
    rw [h, log_div (hp j hj).ne' hNr.ne']
    ring
  linarith only [hl, he]

/-- The stable expression equals the ordinary profiled binomial log-likelihood. -/
theorem profile_eq_binomial_logLik {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hN : 0 < N) (hx : N ∈ feasible x) :
    profile x N = ∑ i, (log (N.choose (x i) : ℝ) +
      (x i : ℝ) * log (mean x / N) + ((N : ℝ) - x i) * log (1 - mean x / N)) := by
  have hNr : 0 < (N : ℝ) := by exact_mod_cast hN
  simp_rw [log_choose_scaled hN (hx _), log_div hmean.ne' hNr.ne']
  unfold profile inverseProfile poissonLogLik entropyRemainder
  simp only [← div_eq_mul_inv]
  have hsum : (∑ i, ((N : ℝ) - x i) * log (1 - mean x / N)) =
      ((m : ℝ) * N - m * mean x) * log (1 - mean x / N) := by
    rw [← sum_mul, sum_sub_distrib, ← mean_mul x hm]
    simp [nsmul_eq_mul]
  calc
    _ = (∑ i, ((x i : ℝ) * log (mean x) - log ((x i).factorial : ℝ))) +
        (∑ i, ∑ j ∈ range (x i), log (1 - (j : ℝ) / N)) +
        ((m : ℝ) * N - m * mean x) * log (1 - mean x / N) := by
      rw [sum_sub_distrib (g := fun _ ↦ mean x)]
      simp only [sum_const, card_univ, Fintype.card_fin, nsmul_eq_mul, div_inv_eq_mul]
      have h : (1 - mean x / N) * (N : ℝ) = N - mean x := by
        field_simp
      rw [add_mul, mul_right_comm (1 - mean x / N) (log (1 - mean x / N)) (N : ℝ),
        h, div_mul_cancel₀ _ hNr.ne']
      ring
    _ = _ := by
      rw [sum_add_distrib, hsum, ← sum_add_distrib]
      congr 1
      exact sum_congr rfl fun i _ ↦ by ring

/-- One trapezoidal bound for a power, by pairing reflected geometric-sum terms. -/
private theorem pow_step_le (j p : ℕ) :
    ((j : ℝ) + 1) ^ (p + 1) - (j : ℝ) ^ (p + 1) ≤
      ((p : ℝ) + 1) * (((j : ℝ) + 1) ^ p + (j : ℝ) ^ p) / 2 := by
  let a : ℝ := j
  let b : ℝ := a + 1
  let f : ℕ → ℝ := fun i ↦ b ^ i * a ^ (p - i)
  have ha : 0 ≤ a := by dsimp [a]; positivity
  have hab : a ≤ b := by dsimp [b]; linarith
  have hgeom : (∑ i ∈ range (p + 1), f i) = b ^ (p + 1) - a ^ (p + 1) := by
    simpa [f, b] using geom_sum₂_mul b a (p + 1)
  have hsum := sum_le_sum (s := range (p + 1)) (f := fun i ↦ f i + f (p - i))
    (g := fun _ ↦ b ^ p + a ^ p) (fun i hi ↦ by
      have hip : i ≤ p := by have := mem_range.mp hi; omega
      have hn : p - (p - i) = i := by omega
      have hs : i + (p - i) = p := by omega
      have h₁ : a ^ i ≤ b ^ i := pow_le_pow_left₀ ha hab _
      have h₂ : a ^ (p - i) ≤ b ^ (p - i) := pow_le_pow_left₀ ha hab _
      have h₃ := mul_nonneg (sub_nonneg.mpr h₁) (sub_nonneg.mpr h₂)
      dsimp [f]
      rw [hn]
      have hbb : b ^ i * b ^ (p - i) = b ^ p := by rw [← pow_add, hs]
      have haa : a ^ i * a ^ (p - i) = a ^ p := by rw [← pow_add, hs]
      nlinarith only [h₃, hbb, haa])
  rw [sum_add_distrib, show (∑ i ∈ range (p + 1), f (p - i)) =
      ∑ i ∈ range (p + 1), f i by
    simpa using sum_range_reflect f (p + 1), hgeom, sum_const, card_range,
    nsmul_eq_mul, Nat.cast_succ] at hsum
  change b ^ (p + 1) - a ^ (p + 1) ≤ _
  change _ ≤ ((p : ℝ) + 1) * (b ^ p + a ^ p) / 2
  linarith only [hsum]

private theorem pow_trapezoid (p : ℕ) (hp : 0 < p) (k : ℕ) :
    (k : ℝ) ^ (p + 1) ≤ ((p : ℝ) + 1) *
      ((∑ j ∈ range (k + 1), (j : ℝ) ^ p) - (k : ℝ) ^ p / 2) := by
  induction k with
  | zero => simp [hp.ne']
  | succ k ih =>
    have h := pow_step_le k p
    rw [sum_range_succ, Nat.cast_succ]
    linarith only [ih, h,
      show ((p : ℝ) + 1) * ((∑ j ∈ range (k + 1), (j : ℝ) ^ p) +
          ((k : ℝ) + 1) ^ p - ((k : ℝ) + 1) ^ p / 2) =
        ((p : ℝ) + 1) * ((∑ j ∈ range (k + 1), (j : ℝ) ^ p) - (k : ℝ) ^ p / 2) +
        ((p : ℝ) + 1) * (((k : ℝ) + 1) ^ p + (k : ℝ) ^ p) / 2 by ring]

/-- The discrete uniform average of a power dominates its continuous uniform average. -/
private theorem pow_grid_le (p : ℕ) (hp : 0 < p) (k : ℕ) :
    ((k : ℝ) + 1) * (k : ℝ) ^ p / ((p : ℝ) + 1) ≤
      ∑ j ∈ range (k + 1), (j : ℝ) ^ p := by
  by_cases hk : k = 0
  · simp [hk, hp.ne']
  have hk' : 0 < (k : ℝ) := by exact_mod_cast Nat.pos_of_ne_zero hk
  have hP : 0 < (p : ℝ) + 1 := by positivity
  have hchord : (∑ j ∈ range (k + 1), (j : ℝ) ^ p) ≤
      ((k : ℝ) + 1) * (k : ℝ) ^ p / 2 := by
    calc
      _ ≤ ∑ j ∈ range (k + 1), ((j : ℝ) / k) * (k : ℝ) ^ p := by
        apply sum_le_sum
        intro j hj
        have hj' : (j : ℝ) ≤ k := by exact_mod_cast Nat.lt_succ_iff.mp (mem_range.mp hj)
        have hw : (j : ℝ) / k ≤ 1 := (div_le_one hk').mpr hj'
        simpa only [smul_eq_mul, zero_pow hp.ne', mul_zero, zero_add,
          div_mul_cancel₀ _ hk'.ne'] using
          (convexOn_pow (𝕜 := ℝ) p).2
            (show (0 : ℝ) ∈ Set.Ici 0 by simp) (show (k : ℝ) ∈ Set.Ici 0 from hk'.le)
            (sub_nonneg.mpr hw) (by positivity : 0 ≤ (j : ℝ) / k)
            (sub_add_cancel 1 ((j : ℝ) / k))
      _ = _ := by
        simp_rw [div_mul_eq_mul_div, ← sum_div, ← sum_mul, sum_range_cast, Nat.cast_succ]
        field_simp
        ring
  have ht := pow_trapezoid p hp k
  rw [pow_succ] at ht
  apply (div_le_iff₀ hP).2
  have h₁ := mul_le_mul_of_nonneg_left hchord (show 0 ≤ (p : ℝ) + 1 by positivity)
  have h₂ := mul_le_mul_of_nonneg_left ht (show 0 ≤ (k : ℝ) + 1 by positivity)
  apply le_of_mul_le_mul_left _ hk'
  nlinarith only [h₁, h₂]

private theorem square_grid (k : ℕ) :
    (∑ j ∈ range (k + 1), (j : ℝ) ^ 2) =
      ((k : ℝ) + 1) * (k : ℝ) ^ 2 / 3 + ((k : ℝ) + 1) * k / 6 := by
  induction k with
  | zero => norm_num
  | succ k ih =>
    rw [sum_range_succ, ih, Nat.cast_succ]
    ring

private theorem size_biased_power_bound {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hv : mean x ≤ variance x) (p : ℕ) :
    (m : ℝ) * mean x ^ (p + 1) ≤
      ∑ i, (x i : ℝ) * (((x i - 1 : ℕ) : ℝ) ^ p) := by
  let S : ℝ := ∑ i, (x i : ℝ)
  let w : Fin m → ℝ := fun i ↦ (x i : ℝ) / S
  let y : Fin m → ℝ := fun i ↦ ((x i - 1 : ℕ) : ℝ)
  have hS : 0 < S := by
    change 0 < ∑ i, (x i : ℝ)
    rw [← mean_mul x hm]
    positivity
  have hw : ∑ i, w i = 1 := by
    simp only [w, ← sum_div]
    exact div_self hS.ne'
  have hy (i : Fin m) : (x i : ℝ) * y i = (x i : ℝ) * ((x i : ℝ) - 1) := by
    by_cases hi : x i = 0
    · simp [hi]
    · simp [y, Nat.cast_sub (show 1 ≤ x i by omega)]
  have hcenter : mean x ≤ ∑ i, w i * y i := by
    simp_rw [w, div_mul_eq_mul_div, ← sum_div, hy]
    apply (le_div_iff₀ hS).2
    simp only [mul_sub_one, ← pow_two, sum_sub_distrib, ← mean_mul x hm]
    have h := variance_mul x hm
    change mean x * (∑ i, (x i : ℝ)) ≤ _
    rw [← mean_mul x hm]
    have h' := mul_le_mul_of_nonneg_left hv (show (0 : ℝ) ≤ m by positivity)
    nlinarith only [h, h']
  have hj := (convexOn_pow (𝕜 := ℝ) p).map_sum_le
    (t := Finset.univ) (w := w) (p := y) (fun i _ ↦ by dsimp [w]; positivity) hw
    (fun i _ ↦ show 0 ≤ y i by dsimp [y]; positivity)
  simp only [smul_eq_mul] at hj
  have hpower := (pow_le_pow_left₀ hmean.le hcenter p).trans hj
  have hscaled := mul_le_mul_of_nonneg_right hpower hS.le
  simp_rw [w, div_mul_eq_mul_div, ← sum_div, div_mul_cancel₀ _ hS.ne'] at hscaled
  change mean x ^ p * (∑ i, (x i : ℝ)) ≤ _ at hscaled
  rw [← mean_mul x hm] at hscaled
  rw [pow_succ]
  linarith only [hscaled, show (m : ℝ) * (mean x ^ p * mean x) =
    mean x ^ p * (m * mean x) by ring]

private theorem sample_power_bound {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hv : mean x ≤ variance x) (p : ℕ) (hp : 0 < p) :
    (m : ℝ) * mean x ^ (p + 1) / ((p : ℝ) + 1) ≤
      ∑ i, ∑ j ∈ range (x i), (j : ℝ) ^ p := by
  refine (div_le_div_of_nonneg_right (size_biased_power_bound x hm hmean hv p)
    (by positivity)).trans ?_
  rw [sum_div]
  apply sum_le_sum
  intro i _
  cases hi : x i with
  | zero => simp
  | succ k => simpa [Nat.cast_succ] using pow_grid_le p hp k

private theorem sample_square_bound {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hv : mean x ≤ variance x) :
    (m : ℝ) * mean x ^ 3 / 3 < ∑ i, ∑ j ∈ range (x i), (j : ℝ) ^ 2 := by
  have hs : ∃ i, 1 < x i := by
    by_contra! h
    have hz : (∑ i, ∑ j ∈ range (x i), (j : ℝ)) = 0 := by
      apply sum_eq_zero
      intro i _
      rw [sum_range_cast]
      rcases Nat.le_one_iff_eq_zero_or_eq_one.mp (h i) with hi | hi <;> simp [hi]
    rw [factorialMoment x hm] at hz
    have hp : 0 < (m : ℝ) * (variance x + mean x ^ 2 - mean x) / 2 := by
      have : 0 ≤ variance x - mean x := sub_nonneg.mpr hv
      have : 0 < variance x + mean x ^ 2 - mean x := by nlinarith [sq_pos_of_pos hmean]
      positivity
    exact hp.ne' hz
  obtain ⟨i, hi⟩ := hs
  refine (div_le_div_of_nonneg_right (size_biased_power_bound x hm hmean hv 2)
    (by norm_num : (0 : ℝ) ≤ 3)).trans_lt ?_
  rw [sum_div]
  apply sum_lt_sum
  · intro k _
    cases hk : x k with
    | zero => simp
    | succ l => norm_num [Nat.cast_succ] at ⊢
                exact_mod_cast (pow_grid_le 2 (by decide) l)
  · refine ⟨i, mem_univ _, ?_⟩
    obtain ⟨k, hk⟩ := Nat.exists_eq_succ_of_ne_zero (show x i ≠ 0 by omega)
    rw [hk, square_grid]
    simp only [Nat.succ_sub_one, Nat.cast_succ]
    have hp : 0 < (k : ℝ) := by exact_mod_cast (show 0 < k by omega)
    have : 0 < ((k : ℝ) + 1) * k / 6 := by positivity
    linarith

private theorem entropy_series {z : ℝ} (hz : |z| < 1) :
    HasSum (fun n : ℕ ↦ z ^ (n + 2) / (((n : ℝ) + 1) * ((n : ℝ) + 2)))
      (entropyRemainder z) := by
  have h := hasSum_pow_div_log_of_abs_lt_one hz
  have hs := (hasSum_nat_add_iff' 1).mpr h
  simp only [sum_range_one, zero_add, pow_one] at hs
  convert (h.mul_left z).sub hs using 1
  · ext n
    simp only [Nat.cast_add, Nat.cast_one]
    rw [show n + 1 + 1 = n + 2 by omega, pow_succ z (n + 1)]
    field_simp
    ring
  · unfold entropyRemainder
    ring

private theorem constant_of_mean_eq_size {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hx : N ∈ feasible x) (he : mean x = N) : ∀ i, x i = N := by
  have hs : (∑ i, ((N : ℝ) - x i)) = 0 := by
    rw [sum_sub_distrib, ← mean_mul x hm, he]
    simp [nsmul_eq_mul]
  have h := (sum_eq_zero_iff_of_nonneg (fun i (_ : i ∈ Finset.univ) ↦
    sub_nonneg.mpr (show (x i : ℝ) ≤ N by exact_mod_cast hx i))).mp hs
  intro i
  exact_mod_cast (sub_eq_zero.mp (h i (mem_univ i))).symm

/-- At or above Poisson dispersion, every feasible finite size is strictly worse. -/
theorem profile_lt_poisson_of_overdispersion {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hv : mean x ≤ variance x) (hx : N ∈ feasible x) :
    profile x N < poissonLogLik x := by
  have hμN : mean x < N := lt_of_le_of_ne (Discrete.avg_le x hx) (fun he ↦ by
    have hc := constant_of_mean_eq_size x hm hx he
    have h0 : variance x = 0 := by simp [variance, he, hc]
    linarith)
  have hN : 0 < (N : ℝ) := hmean.trans hμN
  let t : ℝ := (N : ℝ)⁻¹
  have ht : 0 < t := inv_pos.mpr hN
  let a : ℕ → ℝ := fun n ↦
    (m : ℝ) * mean x ^ (n + 2) / (((n : ℝ) + 1) * ((n : ℝ) + 2)) * t ^ (n + 1)
  let b : ℕ → ℝ := fun n ↦
    (∑ i, ∑ j ∈ range (x i), (j : ℝ) ^ (n + 1)) / ((n : ℝ) + 1) * t ^ (n + 1)
  have hab (n : ℕ) : a n ≤ b n := by
    have h := sample_power_bound x hm hmean hv (n + 1) (by omega)
    have h' := div_le_div_of_nonneg_right h (show 0 ≤ (n : ℝ) + 1 by positivity)
    apply mul_le_mul_of_nonneg_right _ (by positivity)
    simpa only [a, b, Nat.cast_succ, div_div, mul_comm ((n : ℝ) + 1),
      show (n : ℝ) + 1 + 1 = (n : ℝ) + 2 by ring] using h'
  have hab₁ : a 1 < b 1 := by
    dsimp [a, b]
    have h := div_lt_div_of_pos_right (sample_square_bound x hm hmean hv)
      (show 0 < (1 : ℝ) + 1 by norm_num)
    have ht2 : 0 < t ^ 2 := by positivity
    have hh := mul_lt_mul_of_pos_right h ht2
    norm_num [div_div] at hh ⊢
    exact hh
  have hr (y : ℝ) (hy : 0 ≤ y) (hyn : y < N) : |y * t| < 1 := by
    rw [abs_of_nonneg (mul_nonneg hy ht.le)]
    exact (div_lt_one hN).mpr hyn
  have ha : HasSum a ((m : ℝ) * (entropyRemainder (mean x * t) / t)) := by
    convert ((entropy_series (hr (mean x) hmean.le hμN)).div_const t).mul_left (m : ℝ)
      using 1
    ext n
    simp only [a, mul_pow, pow_succ t (n + 1)]
    field_simp
  have hb : HasSum b (∑ i, ∑ j ∈ range (x i), -log (1 - (j : ℝ) * t)) := by
    have h (i : Fin m) (j : ℕ) (hj : j ∈ range (x i)) :
        HasSum (fun n : ℕ ↦ ((j : ℝ) * t) ^ (n + 1) / ((n : ℝ) + 1))
          (-log (1 - (j : ℝ) * t)) :=
      hasSum_pow_div_log_of_abs_lt_one (hr j (by positivity) (by
        exact_mod_cast ((mem_range.mp hj).trans_le (hx i))))
    convert hasSum_sum (s := Finset.univ) (fun i _ ↦
      hasSum_sum (s := range (x i)) (fun j hj ↦ h i j hj)) using 1
    ext n
    simp only [b, mul_pow, ← sum_div, ← sum_mul]
    ring
  have hlt := ha.summable.tsum_lt_tsum hab hab₁ hb.summable
  rw [ha.tsum_eq, hb.tsum_eq] at hlt
  simp only [sum_neg_distrib] at hlt
  change poissonLogLik x + (∑ i, ∑ j ∈ range (x i), log (1 - (j : ℝ) * t)) +
    (m : ℝ) * (entropyRemainder (mean x * t) / t) < poissonLogLik x
  linarith

/-- At or above Poisson dispersion, the Poisson value is the profile supremum. -/
theorem profile_sSup_eq_poisson {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hv : mean x ≤ variance x) :
    sSup (profile x '' feasible x) = poissonLogLik x := by
  have hupper : poissonLogLik x ∈ upperBounds (profile x '' feasible x) := by
    rintro _ ⟨N, hN, rfl⟩
    exact (profile_lt_poisson_of_overdispersion x hm hmean hv hN).le
  obtain ⟨N, hN⟩ := (eventually_feasible x).exists
  refine le_antisymm (csSup_le ⟨profile x N, mem_image_of_mem _ hN⟩ hupper) ?_
  exact Generic.limit_le_sSup (p := id) (eventually_feasible x)
    (profile_tendsto_poisson x hm) ⟨poissonLogLik x, hupper⟩

/-- The dossier's exact existence criterion for the profiled likelihood. -/
theorem profile_maximizer_iff_underdispersion {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) :
    {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N}.Nonempty ↔
      variance x < mean x := by
  refine ⟨?_, fun h ↦ (finite_profile_maximizers_of_underdispersion x hm h).1⟩
  rintro ⟨N, hN, hmax⟩
  by_contra! hv
  have hle : poissonLogLik x ≤ profile x N :=
    Generic.limit_le_upperBound (p := id) (eventually_feasible x)
      (profile_tendsto_poisson x hm) (by
        rintro _ ⟨K, hK, rfl⟩
        exact hmax hK)
  exact (profile_lt_poisson_of_overdispersion x hm hmean hv hN).not_ge hle

private theorem pow_mul_pow_eq_aggregate (s f : ℕ) {p : ℝ} (hp : p ∈ Set.Icc 0 1) :
    p ^ s * (1 - p) ^ f = Discrete.Binomial.likelihood s f p := by
  by_cases h₀ : p = 0
  · subst p
    cases s <;> simp [Discrete.Binomial.likelihood, Nat.cast_add_one_ne_zero]
  by_cases h₁ : p = 1
  · subst p
    cases f <;> simp [Discrete.Binomial.likelihood, Nat.cast_add_one_ne_zero]
  have hp₀ : 0 < p := lt_of_le_of_ne hp.1 (Ne.symm h₀)
  have hp₁ : 0 < 1 - p := sub_pos.mpr (lt_of_le_of_ne hp.2 h₁)
  rw [Discrete.Binomial.likelihood, ite_eq_right (by simp [h₀, h₁]), exp_add,
    ← log_pow, ← log_pow, exp_log (pow_pos hp₀ _), exp_log (pow_pos hp₁ _)]

private theorem sample_eq_aggregate {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hx : N ∈ feasible x) {p : ℝ} (hp : p ∈ Set.Icc 0 1) :
    Discrete.Binomial.sampleLikelihood x N p =
      (∏ i, (N.choose (x i) : ℝ)) *
        Discrete.Binomial.likelihood ((m : ℝ) * mean x) (m * (N - mean x)) p := by
  unfold Discrete.Binomial.sampleLikelihood
  simp_rw [mul_assoc, prod_mul_distrib, prod_pow_eq_pow_sum]
  rw [pow_mul_pow_eq_aggregate _ _ hp]
  have hs : (↑(∑ i, x i) : ℝ) = m * mean x := by
    rw [Nat.cast_sum, mean_mul x hm]
  have hf : (↑(∑ i, (N - x i)) : ℝ) = m * ((N : ℝ) - mean x) := by
    rw [Nat.cast_sum]
    have hcast (i : Fin m) : ((N - x i : ℕ) : ℝ) = (N : ℝ) - x i := Nat.cast_sub (hx i)
    simp_rw [hcast]
    rw [sum_sub_distrib, ← mean_mul x hm]
    simp [nsmul_eq_mul, mul_sub]
  rw [hs, hf]

private theorem fixed_size_profile {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hN : N ∈ feasible x) :
    mean x / N ∈ Set.Icc 0 1 ∧
      ∀ p ∈ Set.Icc 0 1,
        IsMaxOn (Discrete.Binomial.sampleLikelihood x N) (Set.Icc 0 1) p ↔ p = mean x / N := by
  have hμN : mean x ≤ N := Discrete.avg_le x hN
  have hNr : 0 < (N : ℝ) := hmean.trans_le hμN
  have hp : mean x / N ∈ Set.Icc 0 1 := ⟨by positivity, (div_le_one hNr).2 hμN⟩
  have hs : 0 ≤ (m : ℝ) * mean x := by positivity
  have hf : 0 ≤ (m : ℝ) * ((N : ℝ) - mean x) := by positivity
  have htotal : (m : ℝ) * mean x + m * ((N : ℝ) - mean x) = m * N := by ring
  have ht : 0 < (m : ℝ) * mean x + m * ((N : ℝ) - mean x) := by
    rw [htotal]
    positivity
  have he : ((m : ℝ) * mean x) / (m * mean x + m * ((N : ℝ) - mean x)) =
      mean x / N := by
    rw [htotal]
    have hm' : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
    field_simp
  have hc : 0 < ∏ i, (N.choose (x i) : ℝ) :=
    prod_pos fun i _ ↦ by exact_mod_cast Nat.choose_pos (hN i)
  refine ⟨hp, fun p hpp ↦ ?_⟩
  have h := Discrete.Binomial.fixed_size_mle hs hf ht p hpp
  rw [he] at h
  rw [← h]
  constructor
  · intro hmax q hq
    have hle := hmax hq
    change Discrete.Binomial.sampleLikelihood x N q ≤
      Discrete.Binomial.sampleLikelihood x N p at hle
    rw [sample_eq_aggregate x hm hN hq, sample_eq_aggregate x hm hN hpp] at hle
    exact le_of_mul_le_mul_left hle hc
  · intro hmax q hq
    change Discrete.Binomial.sampleLikelihood x N q ≤
      Discrete.Binomial.sampleLikelihood x N p
    rw [sample_eq_aggregate x hm hN hq, sample_eq_aggregate x hm hN hpp]
    exact mul_le_mul_of_nonneg_left (hmax hq) hc.le

/-- Exponentiating the profile recovers the value of the original likelihood at its fiber peak. -/
theorem exp_profile_eq_sampleLikelihood {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hN : N ∈ feasible x) :
    exp (profile x N) = Discrete.Binomial.sampleLikelihood x N (mean x / N) := by
  have hμN : mean x ≤ N := Discrete.avg_le x hN
  have hNr : 0 < (N : ℝ) := hmean.trans_le hμN
  have hNpos : 0 < N := by exact_mod_cast hNr
  rw [profile_eq_binomial_logLik x hm hmean hNpos hN]
  change exp (Discrete.Binomial.sizeLogLik x (mean x / N) N) = _
  rcases hμN.eq_or_lt with he | hl
  · have hc := constant_of_mean_eq_size x hm hN he
    simp [he, hc, hNr.ne', Discrete.Binomial.sampleLikelihood, Discrete.Binomial.sizeLogLik]
  · have hp : mean x / N ∈ Set.Ioo 0 1 := ⟨by positivity, (div_lt_one hNr).2 hl⟩
    rw [Discrete.Binomial.sizeLogLik_eq_log x hp hN]
    exact exp_log ((Discrete.Binomial.sample_likelihood_pos_iff x ⟨hp.1.le, hp.2.le⟩).2
      ⟨hN, fun h ↦ (hp.1.ne' h).elim, fun h ↦ (hp.2.ne h).elim⟩)

def parameterDomain : Set (ℕ × ℝ) := Set.univ ×ˢ Set.Icc 0 1

/-- Joint maximizers have the maximizing sizes and their unique conditional probabilities. -/
theorem joint_maximizers_eq_profile_graph {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) :
    {q ∈ parameterDomain | IsMaxOn (fun q ↦ Discrete.Binomial.sampleLikelihood x q.1 q.2)
      parameterDomain q} =
      (fun N : ℕ ↦ (N, mean x / N)) '' {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N} := by
  let L := Discrete.Binomial.sampleLikelihood x
  let D : Set (ℕ × ℝ) := feasible x ×ˢ Set.Icc 0 1
  have hz (N : ℕ) (hN : N ∉ feasible x) (p : ℝ) : L N p = 0 := by
    obtain ⟨i, hi⟩ := not_forall.mp hN
    apply prod_eq_zero (mem_univ i)
    simp [Nat.choose_eq_zero_of_lt (lt_of_not_ge hi)]
  have hsub : D ⊆ parameterDomain := fun _ hq ↦ ⟨trivial, hq.2⟩
  have hf (N : ℕ) (hN : N ∈ feasible x) :=
    fixed_size_profile x hm hmean hN
  have hb (N : ℕ) (hN : N ∈ feasible x) :
      mean x / N ∈ Set.Icc 0 1 ∧ IsMaxOn (L N) (Set.Icc 0 1) (mean x / N) :=
    ⟨(hf N hN).1, ((hf N hN).2 _ (hf N hN).1).2 rfl⟩
  have hv (N : ℕ) (hN : N ∈ feasible x) : L N (mean x / N) = exp (profile x N) :=
    (exp_profile_eq_sampleLikelihood x hm hmean hN).symm
  have hdomain : {q ∈ parameterDomain | IsMaxOn (fun q ↦ L q.1 q.2) parameterDomain q} =
      {q ∈ D | IsMaxOn (fun q ↦ L q.1 q.2) D q} := by
    ext ⟨N, p⟩
    constructor
    · rintro ⟨hp, hmax⟩
      have hN : N ∈ feasible x := by
        by_contra hn
        obtain ⟨K, hK⟩ := (eventually_feasible x).exists
        have hle := hmax (hsub (show (K, mean x / K) ∈ D from ⟨hK, (hb K hK).1⟩))
        change L K (mean x / K) ≤ L N p at hle
        rw [hz N hn p, hv K hK] at hle
        exact (exp_pos _).not_ge hle
      exact ⟨⟨hN, hp.2⟩, hmax.of_subset hsub⟩
    · rintro ⟨hp, hmax⟩
      refine ⟨hsub hp, ?_⟩
      rintro ⟨K, q⟩ hq
      change L K q ≤ L N p
      by_cases hK : K ∈ feasible x
      · exact hmax (show (K, q) ∈ D from ⟨hK, hq.2⟩)
      · rw [hz K hK q]
        have hle := hmax (show (N, mean x / N) ∈ D from ⟨hp.1, (hb N hp.1).1⟩)
        change L N (mean x / N) ≤ L N p at hle
        rw [hv N hp.1] at hle
        exact (exp_pos _).le.trans hle
  change {q ∈ parameterDomain | IsMaxOn (fun q ↦ L q.1 q.2) parameterDomain q} = _
  rw [hdomain]
  change {q ∈ feasible x ×ˢ Set.Icc 0 1 |
    IsMaxOn (fun q ↦ L q.1 q.2) (feasible x ×ˢ Set.Icc 0 1) q} = _
  rw [Generic.joint_maximizers_eq_profile_graph hb (fun N hN p hp h ↦ ((hf N hN).2 p hp).1 h)]
  congr 1
  ext N
  change (N ∈ feasible x ∧ IsMaxOn (fun K ↦ L K (mean x / K)) (feasible x) N) ↔
    (N ∈ feasible x ∧ IsMaxOn (profile x) (feasible x) N)
  refine and_congr_right fun hN ↦ ?_
  constructor
  · intro h K hK
    have hle := h hK
    change L K (mean x / K) ≤ L N (mean x / N) at hle
    rw [hv K hK, hv N hN] at hle
    exact exp_le_exp.mp hle
  · intro h K hK
    change L K (mean x / K) ≤ L N (mean x / N)
    rw [hv K hK, hv N hN]
    exact exp_le_exp.mpr (h hK)

/-- A finite joint binomial MLE exists exactly in the underdispersed positive-mean case. -/
theorem joint_mle_iff_underdispersion {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) :
    {q ∈ parameterDomain | IsMaxOn (fun q ↦ Discrete.Binomial.sampleLikelihood x q.1 q.2)
      parameterDomain q}.Nonempty ↔ variance x < mean x := by
  rw [joint_maximizers_eq_profile_graph x hm hmean, image_nonempty]
  exact profile_maximizer_iff_underdispersion x hm hmean

/-- Positive-mean underdispersed samples have finitely many maximizing parameter pairs. -/
theorem finite_joint_maximizers_of_underdispersion {m : ℕ} (x : Fin m → ℕ)
    (hm : 0 < m) (hmean : 0 < mean x) (hv : variance x < mean x) :
    {q ∈ parameterDomain | IsMaxOn (fun q ↦ Discrete.Binomial.sampleLikelihood x q.1 q.2)
      parameterDomain q}.Finite := by
  rw [joint_maximizers_eq_profile_graph x hm hmean]
  exact (finite_profile_maximizers_of_underdispersion x hm hv).2.image _

/-- For a nonempty all-zero sample, the maximizing set consists of both parameter axes. -/
theorem all_zero_maximizers {m : ℕ} (hm : 0 < m) (q : ℕ × ℝ)
    (hq : q ∈ parameterDomain) :
    IsMaxOn (fun p ↦ Discrete.Binomial.sampleLikelihood (fun _ : Fin m ↦ 0) p.1 p.2)
      parameterDomain q ↔ q.1 = 0 ∨ q.2 = 0 := by
  have hvalue (p : ℕ × ℝ) : Discrete.Binomial.sampleLikelihood (fun _ : Fin m ↦ 0)
      p.1 p.2 = (1 - p.2) ^ (p.1 * m) := by
    simp [Discrete.Binomial.sampleLikelihood, ← pow_mul]
  constructor
  · intro h
    by_contra! hn
    have hle := h (show ((0 : ℕ), (0 : ℝ)) ∈ parameterDomain from ⟨trivial, by simp⟩)
    change Discrete.Binomial.sampleLikelihood _ 0 0 ≤
      Discrete.Binomial.sampleLikelihood _ q.1 q.2 at hle
    simp only [show Discrete.Binomial.sampleLikelihood (fun _ : Fin m ↦ 0) 0 0 = 1 by
      simpa using hvalue (0, 0), hvalue q] at hle
    have hp : q.2 ∈ Set.Icc (0 : ℝ) 1 := hq.2
    have hlt : 1 - q.2 < 1 := by linarith [lt_of_le_of_ne hp.1 hn.2.symm]
    exact (not_le_of_gt (pow_lt_one₀ (sub_nonneg.mpr hp.2) hlt
      (Nat.mul_ne_zero hn.1 hm.ne'))) hle
  · intro hz p hp
    have hp' : p.2 ∈ Set.Icc (0 : ℝ) 1 := hp.2
    have hle : (1 - p.2) ^ (p.1 * m) ≤ 1 :=
      pow_le_one₀ (sub_nonneg.mpr hp'.2) (by linarith [hp'.1])
    change Discrete.Binomial.sampleLikelihood _ p.1 p.2 ≤
      Discrete.Binomial.sampleLikelihood _ q.1 q.2
    rw [hvalue p, hvalue q]
    rcases hz with hz | hz <;> simpa [hz] using hle

end

end UnivariateML.BinomialSize
