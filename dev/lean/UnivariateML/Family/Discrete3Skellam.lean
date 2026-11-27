module

public import Mathlib.Analysis.Calculus.SmoothSeries
public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.Calculus.LocalExtr.Basic
public import Mathlib.Probability.Distributions.Poisson.Basic
public import Mathlib.Analysis.SpecialFunctions.Stirling
public import Mathlib.Analysis.Normed.Group.FunctionSeries
import Mathlib.Tactic
public import Mathlib.Topology.Order.Compact
public import Mathlib.Analysis.SpecialFunctions.Log.Basic

set_option linter.style.header false

/-! Skellam likelihood from the Poisson convolution on the closed quadrant.
Uniform atom bounds give joint attainment. On the mean-zero single-magnitude
slice, the radial series fixes the sign of the scaled derivative on either
side of its turning point, yielding uniqueness. -/

@[expose] public section

namespace UnivariateML.Discrete3Skellam

open Real Set Filter
open scoped Topology BigOperators

noncomputable section

/-- The varying part of average log likelihood at fixed positive product. -/
def fixedProduct (z d u : ℝ) : ℝ :=
  -(z / 2) * (exp u + exp (-u)) + d * u

/-- At fixed product the rates corresponding to a stationary log ratio
are strictly better than all other log ratios. -/
theorem fixedProduct_strict {z d t u : ℝ} (hz : 0 < z)
    (hroot : d = (z/2) * (exp t - exp (-t))) (hne : u ≠ t) :
    fixedProduct z d u < fixedProduct z d t := by
  have hp := mul_lt_mul_of_pos_left (Real.add_one_lt_exp (sub_ne_zero.mpr hne)) (exp_pos t)
  have hm := mul_lt_mul_of_pos_left
    (Real.add_one_lt_exp (sub_ne_zero.mpr (Ne.symm hne))) (exp_pos (-t))
  rw [← exp_add, show t + (u-t) = u by ring] at hp
  rw [← exp_add, show -t + (t-u) = -u by ring] at hm
  have h := mul_lt_mul_of_pos_left (add_lt_add hp hm) (show 0 < z/2 by positivity)
  unfold fixedProduct
  rw [hroot]
  nlinarith only [h]

/-- For mean-zero data, unequal positive rates lose to the equal-rate
point at the same product. This comparison does not require a Bessel bound. -/
theorem mean_zero_diagonal {z u : ℝ} (hz : 0 < z) (hu : u ≠ 0) :
    fixedProduct z 0 u < fixedProduct z 0 0 := by
  exact fixedProduct_strict hz (by simp) hu

/-- A continuous quadrant likelihood with a positive value and vanishing
uniform tails attains its maximum, including on the axes. -/
theorem attained_of_uniform_tail {L : ℝ × ℝ → ℝ}
    (hc : ContinuousOn L (Ici 0 ×ˢ Ici 0))
    {p : ℝ × ℝ} (hp : p ∈ Ici 0 ×ˢ Ici 0) (hpos : 0 < L p)
    (htail : ∀ ε > 0, ∃ R : ℝ, ∀ q ∈ Ici 0 ×ˢ Ici 0,
      (R < q.1 ∨ R < q.2) → L q < ε) :
    ∃ q ∈ Ici 0 ×ˢ Ici 0, IsMaxOn L (Ici 0 ×ˢ Ici 0) q := by
  obtain ⟨R, hR⟩ := htail (L p) hpos
  have hpR : p.1 ≤ R ∧ p.2 ≤ R := by
    constructor
    · exact le_of_not_gt fun h ↦ (lt_irrefl _) (hR p hp (Or.inl h))
    · exact le_of_not_gt fun h ↦ (lt_irrefl _) (hR p hp (Or.inr h))
  have hsub : Icc (0 : ℝ) R ×ˢ Icc (0 : ℝ) R ⊆ Ici 0 ×ˢ Ici 0 :=
    fun _ h ↦ ⟨h.1.1, h.2.1⟩
  have hpin : p ∈ Icc (0 : ℝ) R ×ˢ Icc (0 : ℝ) R := ⟨⟨hp.1, hpR.1⟩, hp.2, hpR.2⟩
  obtain ⟨q, hq, hmax⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn
    ⟨p, hpin⟩ (hc.mono hsub)
  refine ⟨q, hsub hq, fun y hy ↦ ?_⟩
  by_cases hyR : y.1 ≤ R ∧ y.2 ≤ R
  · exact hmax ⟨⟨hy.1, hyR.1⟩, hy.2, hyR.2⟩
  · push Not at hyR
    by_cases h : y.1 ≤ R
    · exact (hR y hy (Or.inr (hyR h))).le.trans (hmax hpin)
    · exact (hR y hy (Or.inl (lt_of_not_ge h))).le.trans (hmax hpin)

/-- Poisson atom, with the usual rate-zero endpoint supplied by natural powers. -/
def poissonMass (r : ℝ) (j : ℕ) : ℝ := exp (-r)*r^j/(j.factorial : ℝ)

private theorem poisson_hasSum {r : ℝ} (hr : 0 ≤ r) : HasSum (poissonMass r) 1 :=
  ProbabilityTheory.hasSum_one_poissonMeasure ⟨r, hr⟩

private theorem poisson_nonneg {r : ℝ} (hr : 0 ≤ r) (j : ℕ) :
    0 ≤ poissonMass r j := by unfold poissonMass; positivity

private theorem poisson_pos {r : ℝ} (hr : 0 < r) (j : ℕ) :
    0 < poissonMass r j := by unfold poissonMass; positivity

private theorem poisson_le_one {r : ℝ} (hr : 0 ≤ r) (j : ℕ) :
    poissonMass r j ≤ 1 := by
  rw [← (poisson_hasSum hr).tsum_eq]
  exact (poisson_hasSum hr).summable.le_tsum j fun i _ ↦ poisson_nonneg hr i

private theorem poisson_shift_summable {r : ℝ} (hr : 0 ≤ r) (a : ℕ) :
    Summable (fun j ↦ poissonMass r (j+a)) :=
  (poisson_hasSum hr).summable.comp_injective (fun _ _ h ↦ Nat.add_right_cancel h)

private theorem poisson_shift_le_one {r : ℝ} (hr : 0 ≤ r) (a : ℕ) :
    (∑' j, poissonMass r (j+a)) ≤ 1 := by
  rw [← (poisson_hasSum hr).tsum_eq]
  exact (poisson_shift_summable hr a).tsum_le_tsum_of_inj
    (fun j ↦ j+a) (fun _ _ h ↦ Nat.add_right_cancel h)
    (fun j _ ↦ poisson_nonneg hr j) (fun _ ↦ le_rfl) (poisson_hasSum hr).summable

/-- Skellam mass as a nonnegative convolution series on the closed quadrant. -/
def mass (k : ℤ) (p : ℝ × ℝ) : ℝ :=
  ∑' j, poissonMass p.1 (j+k.toNat) * poissonMass p.2 (j+(-k).toNat)

/-- Closed nonnegative rate quadrant. -/
def domain : Set (ℝ × ℝ) := Ici 0 ×ˢ Ici 0

private theorem mass_summable (k : ℤ) {p : ℝ × ℝ} (hp : p ∈ domain) :
    Summable (fun j ↦ poissonMass p.1 (j+k.toNat) * poissonMass p.2 (j+(-k).toNat)) :=
  Summable.of_nonneg_of_le
    (fun _ ↦ mul_nonneg (poisson_nonneg hp.1 _) (poisson_nonneg hp.2 _))
    (fun _ ↦ mul_le_of_le_one_left (poisson_nonneg hp.2 _) (poisson_le_one hp.1 _))
    (poisson_shift_summable hp.2 _)

private theorem mass_nonneg (k : ℤ) {p : ℝ × ℝ} (hp : p ∈ domain) :
    0 ≤ mass k p := tsum_nonneg fun _ ↦
  mul_nonneg (poisson_nonneg hp.1 _) (poisson_nonneg hp.2 _)

/-- Every signed count has strictly positive mass at positive rates. -/
theorem mass_pos (k : ℤ) {p : ℝ × ℝ} (h₁ : 0 < p.1) (h₂ : 0 < p.2) :
    0 < mass k p := by
  have hh : p ∈ domain := ⟨h₁.le, h₂.le⟩
  have h := (mass_summable k hh).le_tsum 0
    (fun j _ ↦ mul_nonneg (poisson_nonneg h₁.le _) (poisson_nonneg h₂.le _))
  exact (mul_pos (poisson_pos h₁ (0+k.toNat)) (poisson_pos h₂ (0+(-k).toNat))).trans_le h

private theorem mass_le_bound_first (k : ℤ) {p : ℝ × ℝ} (hp : p ∈ domain)
    {M : ℝ} (hM : 0 ≤ M) (h : ∀ j, poissonMass p.1 j ≤ M) :
    mass k p ≤ M := by
  have hs := (poisson_shift_summable hp.2 (-k).toNat).mul_left M
  calc
    mass k p ≤ ∑' j, M * poissonMass p.2 (j+(-k).toNat) :=
      (mass_summable k hp).tsum_le_tsum
        (fun j ↦ mul_le_mul_of_nonneg_right (h _) (poisson_nonneg hp.2 _)) hs
    _ = M * ∑' j, poissonMass p.2 (j+(-k).toNat) := tsum_mul_left
    _ ≤ M := by
      simpa only [mul_one] using
        (mul_le_mul_of_nonneg_left (poisson_shift_le_one hp.2 (-k).toNat) hM)

private theorem mass_reflection (k : ℤ) (p : ℝ × ℝ) :
    mass (-k) (p.2,p.1) = mass k p := by
  unfold mass
  apply tsum_congr
  intro j
  simp [mul_comm]

private theorem mass_le_bound_second (k : ℤ) {p : ℝ × ℝ} (hp : p ∈ domain)
    {M : ℝ} (hM : 0 ≤ M) (h : ∀ j, poissonMass p.2 j ≤ M) :
    mass k p ≤ M := by
  rw [← mass_reflection]
  exact mass_le_bound_first (-k) ⟨hp.2, hp.1⟩ hM h

private theorem mass_le_one (k : ℤ) {p : ℝ × ℝ} (hp : p ∈ domain) :
    mass k p ≤ 1 := mass_le_bound_first k hp zero_le_one (poisson_le_one hp.1)

private theorem poisson_continuous (j : ℕ) : Continuous (fun r ↦ poissonMass r j) := by
  unfold poissonMass
  fun_prop

private theorem poisson_rect_bound {r R : ℝ} (hr : r ∈ Icc 0 R) (j : ℕ) :
    poissonMass r j ≤ exp R * poissonMass R j := by
  have hpow := pow_le_pow_left₀ hr.1 hr.2 j
  have he : exp (-r) ≤ 1 := exp_le_one_iff.mpr (neg_nonpos.mpr hr.1)
  have hn : 0 ≤ r^j := pow_nonneg hr.1 _
  calc
    _ ≤ r^j/(j.factorial : ℝ) := by
      unfold poissonMass
      apply div_le_div_of_nonneg_right _ (by positivity)
      simpa only [one_mul] using mul_le_mul_of_nonneg_right he hn
    _ ≤ R^j/(j.factorial : ℝ) := div_le_div_of_nonneg_right hpow (by positivity)
    _ = _ := by
      unfold poissonMass
      rw [← mul_div_assoc, ← mul_assoc, ← exp_add, add_neg_cancel, exp_zero, one_mul]

private theorem mass_continuousOn_rect (k : ℤ) {R : ℝ} (hR : 0 ≤ R) :
    ContinuousOn (mass k) (Icc 0 R ×ˢ Icc 0 R) := by
  unfold mass
  apply continuousOn_tsum
    (u := fun j ↦ exp R * poissonMass R (j+(-k).toNat))
  · intro j
    exact (((poisson_continuous (j+k.toNat)).comp continuous_fst).mul
      ((poisson_continuous (j+(-k).toNat)).comp continuous_snd)).continuousOn
  · exact (poisson_shift_summable hR (-k).toNat).mul_left _
  · intro j q hq
    have ha : 0 ≤ q.1 := hq.1.1
    have hb : 0 ≤ q.2 := hq.2.1
    rw [Real.norm_of_nonneg (mul_nonneg (poisson_nonneg ha _) (poisson_nonneg hb _))]
    exact (mul_le_of_le_one_left (poisson_nonneg hb _) (poisson_le_one ha _)).trans
      (poisson_rect_bound hq.2 _)

/-- The convolution mass is continuous throughout the quadrant, including its axes. -/
theorem mass_continuousOn (k : ℤ) : ContinuousOn (mass k) domain := by
  intro p hp
  let R := max p.1 p.2 + 1
  have h₁ : p.1 < R := (le_max_left _ _).trans_lt (lt_add_of_pos_right _ zero_lt_one)
  have h₂ : p.2 < R := (le_max_right _ _).trans_lt (lt_add_of_pos_right _ zero_lt_one)
  have hR : 0 ≤ R := hp.1.trans h₁.le
  have hRp : p ∈ Icc 0 R ×ˢ Icc 0 R := ⟨⟨hp.1, h₁.le⟩, hp.2, h₂.le⟩
  apply (mass_continuousOn_rect k hR p hRp).mono_of_mem_nhdsWithin
  have hloc : Iio R ×ˢ Iio R ∈ 𝓝 p :=
    (isOpen_Iio.prod isOpen_Iio).mem_nhds (show p ∈ Iio R ×ˢ Iio R from ⟨h₁,h₂⟩)
  filter_upwards [self_mem_nhdsWithin, mem_nhdsWithin_of_mem_nhds hloc] with q hq hqR
  exact ⟨⟨hq.1, hqR.1.le⟩, hq.2, hqR.2.le⟩

private theorem poisson_le_at_own_rate {r : ℝ} (hr : 0 ≤ r) {j : ℕ} (hj : 0 < j) :
    poissonMass r j ≤ poissonMass (j : ℝ) j := by
  have hj' : (0 : ℝ) < j := by exact_mod_cast hj
  rcases eq_or_lt_of_le hr with h | h
  · rw [← h]
    simp only [poissonMass, neg_zero, exp_zero, zero_pow hj.ne', mul_zero, zero_div]
    exact (poisson_pos hj' j).le
  · have he := Real.log_le_sub_one_of_pos (div_pos h hj')
    rw [log_div h.ne' hj'.ne'] at he
    have hmul := mul_le_mul_of_nonneg_left he hj'.le
    have hratio : (j : ℝ)*(r/(j : ℝ)-1) = r-j := by
      field_simp [hj'.ne']
    rw [hratio] at hmul
    have hp : (j : ℝ)*log r-r ≤ (j : ℝ)*log (j : ℝ)-j := by linarith only [hmul]
    have hbound := exp_le_exp.mpr hp
    simp only [sub_eq_add_neg, exp_add, exp_nat_mul, exp_log h, exp_log hj'] at hbound
    unfold poissonMass
    exact div_le_div_of_nonneg_right (by simpa only [mul_comm] using hbound) (by positivity)

private theorem poisson_at_own_rate_le {j : ℕ} (hj : 0 < j) :
    poissonMass (j : ℝ) j ≤ 1/sqrt (2*π*(j : ℝ)) := by
  have hj' : (0 : ℝ) < j := by exact_mod_cast hj
  have hS : 0 < sqrt (2*π*(j : ℝ)) := sqrt_pos.mpr (by positivity)
  have he : exp (-(j : ℝ))*(j : ℝ)^j = ((j : ℝ)/exp 1)^j := by
    rw [div_pow, ← exp_nat_mul, mul_one, div_eq_inv_mul, exp_neg]
  unfold poissonMass
  rw [he]
  apply (div_le_div_iff₀ (by positivity : 0 < (j.factorial : ℝ)) hS).mpr
  simpa only [one_mul, mul_one, mul_comm] using Stirling.le_factorial_stirling j

/-- All Poisson atoms vanish uniformly as the rate tends to infinity. -/
theorem poisson_atoms_uniform_tail {ε : ℝ} (hε : 0 < ε) :
    ∃ R : ℝ, ∀ r > R, 0 ≤ r → ∀ j, poissonMass r j < ε := by
  have hbig : Tendsto (fun j : ℕ ↦ 1/sqrt (2*π*(j : ℝ))) atTop (𝓝 0) := by
    have h := (tendsto_natCast_atTop_atTop (R := ℝ)).const_mul_atTop (by positivity : 0 < 2*π)
    simpa only [one_div, Function.comp_def] using tendsto_inv_atTop_zero.comp
      (tendsto_sqrt_atTop.comp h)
  obtain ⟨N, hN⟩ := eventually_atTop.mp (hbig.eventually (Iio_mem_nhds hε))
  let D := Finset.range (max N 1)
  have hsmall : ∀ᶠ r : ℝ in atTop, ∀ j ∈ D, poissonMass r j < ε := by
    apply (eventually_all_finset D).mpr
    intro j _
    have h : Tendsto (fun r ↦ poissonMass r j) atTop (𝓝 0) := by
      simpa only [poissonMass, mul_comm, zero_div] using
        (tendsto_pow_mul_exp_neg_atTop_nhds_zero j).div_const (j.factorial : ℝ)
    exact h.eventually (Iio_mem_nhds hε)
  obtain ⟨R, hR⟩ := eventually_atTop.mp hsmall
  refine ⟨R, fun r hr hr0 j ↦ ?_⟩
  by_cases hj : j ∈ D
  · exact hR r hr.le j hj
  · have hj' : max N 1 ≤ j := Nat.le_of_not_gt (fun h ↦ hj (Finset.mem_range.mpr h))
    have hjp : 0 < j := lt_of_lt_of_le Nat.zero_lt_one ((le_max_right _ _).trans hj')
    exact ((poisson_le_at_own_rate hr0 hjp).trans (poisson_at_own_rate_le hjp)).trans_lt
      (hN j ((le_max_left _ _).trans hj'))

/-- Joint likelihood of a finite signed sample. -/
def likelihood {n : ℕ} (x : Fin n → ℤ) (p : ℝ × ℝ) : ℝ :=
  ∏ i, mass (x i) p

private theorem likelihood_le_mass {n : ℕ} (x : Fin n → ℤ) (i : Fin n)
    {p : ℝ × ℝ} (hp : p ∈ domain) :
    likelihood x p ≤ mass (x i) p := by
  unfold likelihood
  simpa only [Finset.prod_singleton] using Finset.prod_le_prod_of_subset_of_le_one₀
    (f := fun j : Fin n ↦ mass (x j) p) (Finset.subset_univ {i})
    (fun j _ ↦ mass_nonneg (x j) hp) (fun j _ _ ↦ mass_le_one (x j) hp)

/-- The Skellam likelihood tends uniformly to zero if either rate diverges. -/
theorem likelihood_uniform_tail {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {ε : ℝ} (hε : 0 < ε) :
    ∃ R : ℝ, ∀ q ∈ domain, (R < q.1 ∨ R < q.2) → likelihood x q < ε := by
  obtain ⟨R, hR⟩ := poisson_atoms_uniform_tail (show 0 < ε/2 by positivity)
  let i : Fin n := ⟨0, hn⟩
  refine ⟨R, fun q hq htail ↦ (likelihood_le_mass x i hq).trans_lt ?_⟩
  have hε' : 0 ≤ ε/2 := by positivity
  have h : mass (x i) q ≤ ε/2 := by
    rcases htail with h₁ | h₂
    · exact mass_le_bound_first (x i) hq hε' fun j ↦ (hR q.1 h₁ hq.1 j).le
    · exact mass_le_bound_second (x i) hq hε' fun j ↦ (hR q.2 h₂ hq.2 j).le
  linarith only [h, hε]

/-- Every nonempty signed integer sample has an attained Skellam maximum
on the closed nonnegative rate quadrant. -/
theorem joint_attained {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    ∃ p ∈ domain, IsMaxOn (likelihood x) domain p := by
  have hL : ContinuousOn (likelihood x) domain :=
    continuousOn_finsetProd _ fun i _ ↦ mass_continuousOn (x i)
  have hpos : 0 < likelihood x (1,1) :=
    Finset.prod_pos (fun i _ ↦ mass_pos (x i) one_pos one_pos)
  exact attained_of_uniform_tail hL (show ((1,1) : ℝ × ℝ) ∈ domain from
    ⟨show (0 : ℝ) ≤ 1 from zero_le_one, show (0 : ℝ) ≤ 1 from zero_le_one⟩) hpos (fun ε hε ↦ likelihood_uniform_tail hn x hε)

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

private theorem factorial_ratio (j k : ℕ) (t : ℝ) :
    radialTerm k j t = ((j : ℝ)+(k : ℝ)+1)*radialTerm (k+1) j t := by
  unfold radialTerm
  rw [show j+(k+1) = (j+k)+1 by omega, Nat.factorial_succ, Nat.cast_mul]
  push_cast
  field_simp [show (j : ℝ)+(k : ℝ)+1 ≠ 0 by positivity]

private theorem radialSeries_ode (k : ℕ) (t : ℝ) :
    t*radialSeries (k+2) t+((k : ℝ)+1)*radialSeries (k+1) t = radialSeries k t := by
  let A : ℕ → ℝ := fun j ↦ if j = 0 then 0 else t*radialTerm (k+2) (j-1) t
  have hA : Summable A := by
    apply (summable_nat_add_iff (f := A) 1).mp
    simpa [A] using (radialTerm_summable (k+2) t).mul_left t
  have hAt : (∑' j, A j) = t*radialSeries (k+2) t := by
    rw [hA.tsum_eq_zero_add]
    simp [A, radialSeries, tsum_mul_left]
  rw [← hAt, radialSeries, ← tsum_mul_left,
    ← hA.tsum_add ((radialTerm_summable (k+1) t).mul_left ((k : ℝ)+1))]
  unfold radialSeries
  apply tsum_congr
  intro j
  rw [factorial_ratio j k]
  have he : A j = (j : ℝ)*radialTerm (k+1) j t := by
    cases j with
    | zero => simp [A]
    | succ j =>
      simp only [A, Nat.succ_ne_zero, ↓reduceIte, Nat.succ_sub_one]
      unfold radialTerm
      rw [show j.succ+(k+1) = j+(k+2) by omega, Nat.factorial_succ, Nat.cast_mul, pow_succ]
      field_simp [show (j.succ : ℝ) ≠ 0 by positivity]
  rw [he]
  ring

private theorem mass_nat_factor (k : ℕ) (a b : ℝ) :
    mass (k : ℤ) (a,b) = exp (-(a+b))*a^k*radialSeries k (a*b) := by
  unfold mass radialSeries
  rw [← tsum_mul_left]
  apply tsum_congr
  intro j
  simp only [Int.toNat_natCast, Int.toNat_of_nonpos (show -(k : ℤ) ≤ 0 by omega),
    Nat.add_zero, poissonMass, radialTerm, neg_add, exp_add, mul_pow, pow_add]
  ring

private def diagonalMass (k : ℕ) (r : ℝ) : ℝ :=
  exp (-2*r)*r^k*radialSeries k (r^2)

private theorem mass_diag_nat (k : ℕ) (r : ℝ) :
    mass (k : ℤ) (r,r) = diagonalMass k r := by
  rw [mass_nat_factor, diagonalMass, show -(r+r) = -2*r by ring, pow_two]

private def radialSlope (k : ℕ) (r : ℝ) : ℝ :=
  2*r^2*radialSeries (k+1) (r^2) + ((k : ℝ)-2*r)*radialSeries k (r^2)

private def radialFlux (k : ℕ) (r : ℝ) : ℝ :=
  exp (2*r)*r^k*radialSlope k r

private theorem prefactor_deriv (k : ℕ) (c : ℝ) {r : ℝ} (hr : 0 < r) :
    HasDerivAt (fun v ↦ exp (c*v)*v^k)
      (exp (c*r)*r^k*(c*r+(k : ℝ))/r) r := by
  have hpow : (k : ℝ)*r^(k-1) = (k : ℝ)*r^k/r := by
    cases k with
    | zero => simp
    | succ k =>
      rw [Nat.succ_sub_one, pow_succ]
      field_simp [hr.ne']
  have h := (((hasDerivAt_id r).const_mul c).exp).mul ((hasDerivAt_id r).pow k)
  convert h using 1
  · rfl
  · change _ = exp (c*r)*(c*1)*r^k+exp (c*r)*((k : ℝ)*r^(k-1)*1)
    rw [mul_one, mul_one, hpow]
    field_simp [hr.ne']

private theorem radialSquare_deriv (k : ℕ) (r : ℝ) :
    HasDerivAt (fun v ↦ radialSeries k (v^2)) (radialSeries (k+1) (r^2)*(2*r)) r := by
  have h : HasDerivAt (fun v : ℝ ↦ v^2) (2*r) r := by
    convert (hasDerivAt_id r).pow 2 using 1
    · rfl
    · norm_num
  exact (radialSeries_deriv k (r^2)).comp r h

private theorem diagonalMass_deriv (k : ℕ) {r : ℝ} (hr : 0 < r) :
    HasDerivAt (diagonalMass k) (exp (-2*r)*r^k/r*radialSlope k r) r := by
  have h := (prefactor_deriv k (-2) hr).mul (radialSquare_deriv k r)
  convert h using 1
  · rfl
  · change _ = exp (-2*r)*r^k*(-2*r+(k : ℝ))/r*radialSeries k (r^2) +
        exp (-2*r)*r^k*(radialSeries (k+1) (r^2)*(2*r))
    unfold radialSlope
    field_simp [hr.ne']
    ring

private theorem radialSlope_deriv (k : ℕ) (r : ℝ) :
    HasDerivAt (radialSlope k)
      (4*r*radialSeries (k+1) (r^2)+
        4*r^3*radialSeries (k+2) (r^2)-2*radialSeries k (r^2)+
        ((k : ℝ)-2*r)*(2*r*radialSeries (k+1) (r^2))) r := by
  have h := ((((hasDerivAt_id r).pow 2).const_mul 2).mul (radialSquare_deriv (k+1) r)).add
    (((hasDerivAt_const r (k : ℝ)).sub ((hasDerivAt_id r).const_mul 2)).mul
      (radialSquare_deriv k r))
  convert h using 1
  · rfl
  · change _ = (2*(2*r^(2-1)*1))*radialSeries (k+1) (r^2) +
        (2*r^2)*(radialSeries (k+1+1) (r^2)*(2*r)) +
      ((0-2*1)*radialSeries k (r^2)+((k : ℝ)-2*r)*(radialSeries (k+1) (r^2)*(2*r)))
    ring

private theorem radialFlux_deriv (k : ℕ) {r : ℝ} (hr : 0 < r) :
    HasDerivAt (radialFlux k)
      (exp (2*r)*r^k/r*((k : ℝ)^2-2*r)*radialSeries k (r^2)) r := by
  have h := (prefactor_deriv k 2 hr).mul (radialSlope_deriv k r)
  convert h using 1
  · rfl
  · change _ = (exp (2*r)*r^k*(2*r+(k : ℝ))/r)*radialSlope k r+
      exp (2*r)*r^k*(4*r*radialSeries (k+1) (r^2)+
        4*r^3*radialSeries (k+2) (r^2)-2*radialSeries k (r^2)+
        ((k : ℝ)-2*r)*(2*r*radialSeries (k+1) (r^2)))
    have hODE := radialSeries_ode k (r^2)
    have hcore : (2*r+(k : ℝ))*radialSlope k r+
          r*(4*r*radialSeries (k+1) (r^2)+
            4*r^3*radialSeries (k+2) (r^2)-2*radialSeries k (r^2)+
            ((k : ℝ)-2*r)*(2*r*radialSeries (k+1) (r^2))) =
        ((k : ℝ)^2-2*r)*radialSeries k (r^2) := by
      unfold radialSlope
      linear_combination (4*r^2)*hODE
    rw [mul_assoc (exp (2*r)*r^k/r), ← hcore]
    field_simp [hr.ne']

private theorem radialSeries_pos (k : ℕ) {t : ℝ} (ht : 0 ≤ t) : 0 < radialSeries k t := by
  have hn (j : ℕ) : 0 ≤ radialTerm k j t := by unfold radialTerm; positivity
  have h0 : 0 < radialTerm k 0 t := by simp [radialTerm, Nat.factorial_pos]
  exact h0.trans_le ((radialTerm_summable k t).le_tsum 0 fun j _ ↦ hn j)

private theorem diagonal_deriv_eq_scaled_flux (k : ℕ) {r : ℝ} (hr : 0 < r) :
    deriv (diagonalMass k) r = (exp (-4*r)/r)*radialFlux k r := by
  rw [(diagonalMass_deriv k hr).deriv, radialFlux]
  have he : exp (-4*r)*exp (2*r) = exp (-2*r) := by rw [← exp_add]; congr 1; ring
  linear_combination (r^k/r*radialSlope k r)*(-he)

private theorem diagonalMass_continuous (k : ℕ) : Continuous (diagonalMass k) := by
  have h : Continuous (fun r : ℝ ↦ radialSeries k (r^2)) :=
    continuous_iff_continuousAt.mpr fun r ↦ (radialSquare_deriv k r).continuousAt
  have h' : Continuous (fun r : ℝ ↦ exp (-2*r)*r^k) := by fun_prop
  exact h'.mul h

private theorem diagonalMass_pos (k : ℕ) {r : ℝ} (hr : 0 < r) :
    0 < diagonalMass k r :=
  mul_pos (mul_pos (exp_pos _) (pow_pos hr _)) (radialSeries_pos k (sq_nonneg r))

private theorem radialFlux_pos_until {k : ℕ} (hk : 0 < k)
    {r : ℝ} (hr : r ∈ Ioc (0 : ℝ) ((k : ℝ)^2/2)) : 0 < radialFlux k r := by
  let c := (k : ℝ)^2/2
  have hmono : StrictMonoOn (radialFlux k) (Ioc 0 c) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioc _ _)
      (fun t ht ↦ (radialFlux_deriv k ht.1).continuousAt.continuousWithinAt)
    intro t ht
    rw [interior_Ioc] at ht
    rw [(radialFlux_deriv k ht.1).deriv]
    apply mul_pos (mul_pos (div_pos (mul_pos (exp_pos _) (pow_pos ht.1 _)) ht.1)
      (show 0 < (k : ℝ)^2-2*t by dsimp [c] at ht; linarith only [ht.2]))
    exact radialSeries_pos k (sq_nonneg t)
  by_contra! h
  have hf : StrictAntiOn (diagonalMass k) (Icc 0 r) := by
    apply strictAntiOn_of_deriv_neg (convex_Icc _ _) (diagonalMass_continuous k).continuousOn
    intro t ht
    rw [interior_Icc] at ht
    rw [diagonal_deriv_eq_scaled_flux k ht.1]
    exact mul_neg_of_pos_of_neg (div_pos (exp_pos _) ht.1) ((hmono
      ⟨ht.1, ht.2.le.trans hr.2⟩ hr ht.2).trans_le h)
  have hg : diagonalMass k 0 = 0 := by simp [diagonalMass, zero_pow hk.ne']
  have hlt := hf (show (0 : ℝ) ∈ Icc 0 r from ⟨le_rfl, hr.1.le⟩)
    (show r ∈ Icc 0 r from ⟨hr.1.le, le_rfl⟩) hr.1
  rw [hg] at hlt
  exact (not_lt_of_ge (diagonalMass_pos k hr.1).le) hlt

private theorem diagonal_critical_unique {k : ℕ} (hk : 0 < k)
    {r s : ℝ} (hr : 0 < r) (hs : 0 < s)
    (h₁ : deriv (diagonalMass k) r = 0) (h₂ : deriv (diagonalMass k) s = 0) : r = s := by
  let c := (k : ℝ)^2/2
  have hk' : (0 : ℝ) < k := by exact_mod_cast hk
  have hc : 0 < c := by dsimp [c]; positivity
  have hzero (u : ℝ) (hu : 0 < u) (hdu : deriv (diagonalMass k) u = 0) :
      u ∈ Ioi c ∧ radialFlux k u = 0 := by
    rw [diagonal_deriv_eq_scaled_flux k hu] at hdu
    have hz := (mul_eq_zero.mp hdu).resolve_left (div_pos (exp_pos _) hu).ne'
    have hcu : c < u := by
      by_contra! hn
      exact (radialFlux_pos_until hk ⟨hu, hn⟩).ne' hz
    exact ⟨hcu, hz⟩
  have hanti : StrictAntiOn (radialFlux k) (Ioi c) := by
    apply strictAntiOn_of_deriv_neg (convex_Ioi _)
      (fun t ht ↦ (radialFlux_deriv k (hc.trans ht)).continuousAt.continuousWithinAt)
    intro t ht
    rw [interior_Ioi] at ht
    have ht' : 0 < t := hc.trans ht
    rw [(radialFlux_deriv k ht').deriv]
    apply mul_neg_of_neg_of_pos _ (radialSeries_pos k (sq_nonneg t))
    apply mul_neg_of_pos_of_neg (by positivity)
    have hh : c < t := ht
    dsimp [c] at hh
    linarith only [hh]
  exact hanti.injOn (hzero r hr h₁).1 (hzero s hs h₂).1
    ((hzero r hr h₁).2.trans (hzero s hs h₂).2.symm)

private theorem mass_zero_left {k : ℤ} (hk : 0 < k) (b : ℝ) :
    mass k (0,b) = 0 := by
  unfold mass
  have hpos : 0 < k.toNat := by omega
  have h (j : ℕ) : poissonMass 0 (j+k.toNat) = 0 := by
    simp [poissonMass, zero_pow (show j+k.toNat ≠ 0 by omega)]
  simp [h]

private theorem mass_signed_factor {k : ℕ} {l : ℤ} (hl : l = (k : ℤ) ∨ l = -(k : ℤ))
    (a b : ℝ) :
    mass l (a,b) = (exp (-(a+b))*radialSeries k (a*b))*
      (a^l.toNat*b^(-l).toNat) := by
  rcases hl with rfl | rfl
  · rw [mass_nat_factor]
    rw [Int.toNat_natCast, Int.toNat_of_nonpos (show -(k : ℤ) ≤ 0 by omega), pow_zero]
    ring
  · rw [mass_reflection (k : ℤ) (b,a), mass_nat_factor, neg_neg, Int.toNat_natCast,
      Int.toNat_of_nonpos (show -(k : ℤ) ≤ 0 by omega), pow_zero]
    rw [add_comm b a, mul_comm b a]
    ring

private theorem signed_sums_eq {n : ℕ} {x : Fin n → ℤ} (h : ∑ i, x i = 0) :
    (∑ i, (x i).toNat) = ∑ i, (-x i).toNat := by
  have he : (∑ i, (((x i).toNat : ℤ)-(((-x i).toNat : ℤ)))) = ∑ i, x i :=
    Finset.sum_congr rfl (fun i _ ↦ by omega)
  rw [Finset.sum_sub_distrib, ← Nat.cast_sum, ← Nat.cast_sum, h, sub_eq_zero] at he
  exact_mod_cast he

private theorem signed_likelihood_factor {n : ℕ} {x : Fin n → ℤ} {k : ℕ}
    (hx : ∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ)) (a b : ℝ) :
    likelihood x (a,b) = (exp (-(a+b))*radialSeries k (a*b))^n *
      (a^(∑ i, (x i).toNat)*b^(∑ i, (-x i).toNat)) := by
  unfold likelihood
  simp_rw [mass_signed_factor (hx _) a b]
  rw [Finset.prod_mul_distrib]
  simp only [Finset.prod_const, Finset.card_univ, Fintype.card_fin]
  congr 1
  rw [Finset.prod_mul_distrib, Finset.prod_pow_eq_pow_sum, Finset.prod_pow_eq_pow_sum]

private theorem signed_likelihood_diag_strict {n : ℕ} (hn : 0 < n) {x : Fin n → ℤ} {k : ℕ}
    (hx : ∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ)) (hmean : ∑ i, x i = 0)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hne : a ≠ b) :
    likelihood x (a,b) < likelihood x (sqrt (a*b),sqrt (a*b)) := by
  let r := sqrt (a*b)
  have hr : 0 < r := sqrt_pos.mpr (mul_pos ha hb)
  have hr2 : r*r = a*b := by
    dsimp [r]
    rw [← pow_two, sq_sqrt (mul_pos ha hb).le]
  have hadd : r+r < a+b := by
    by_contra! h
    have hprod := mul_nonneg (sub_nonneg.mpr h)
      (show 0 ≤ r+r+(a+b) by positivity)
    nlinarith only [hprod, hr2, sq_pos_of_ne_zero (sub_ne_zero.mpr hne)]
  change likelihood x (a,b) < likelihood x (r,r)
  rw [signed_likelihood_factor hx, signed_likelihood_factor hx, hr2,
    signed_sums_eq hmean, ← mul_pow, ← mul_pow, hr2]
  have hU := radialSeries_pos k (mul_pos ha hb).le
  exact mul_lt_mul_of_pos_right
    (pow_lt_pow_left₀ (mul_lt_mul_of_pos_right (exp_lt_exp.mpr (neg_lt_neg hadd)) hU)
      (mul_pos (exp_pos _) hU).le hn.ne') (pow_pos (mul_pos ha hb) _)

private theorem signed_likelihood_diagonal {n : ℕ} {x : Fin n → ℤ} {k : ℕ}
    (hx : ∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ)) (r : ℝ) :
    likelihood x (r,r) = (diagonalMass k r)^n := by
  have h (i : Fin n) : mass (x i) (r,r) = diagonalMass k r := by
    rcases hx i with hi | hi
    · rw [hi, mass_diag_nat]
    · rw [hi, mass_reflection (k : ℤ) (r,r), mass_diag_nat]
  simp [likelihood, h]

/-- A mean-zero sample with one positive observed magnitude and both signs
has exactly one maximizer, on the positive diagonal. -/
theorem joint_unique_one_magnitude {n : ℕ} (hn : 0 < n) {x : Fin n → ℤ}
    {k : ℕ} (hk : 0 < k) (hx : ∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ))
    (hmean : ∑ i, x i = 0) (hpos : ∃ i, 0 < x i) (hneg : ∃ i, x i < 0) :
    ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p := by
  have hconst (p : ℝ × ℝ) (hp : p ∈ domain) (hm : IsMaxOn (likelihood x) domain p) :
      0 < p.1 ∧ 0 < p.2 ∧ p.1 = p.2 := by
    have hbase : ((1,1) : ℝ × ℝ) ∈ domain :=
      ⟨show (0 : ℝ) ≤ 1 from zero_le_one, show (0 : ℝ) ≤ 1 from zero_le_one⟩
    have hbasepos : 0 < likelihood x (1,1) :=
      Finset.prod_pos (fun i _ ↦ mass_pos (x i) one_pos one_pos)
    have hL : 0 < likelihood x p := hbasepos.trans_le (hm hbase)
    have ha : 0 < p.1 := by
      rcases lt_or_eq_of_le (show 0 ≤ p.1 from hp.1) with hp' | h
      · exact hp'
      · obtain ⟨i, hi⟩ := hpos
        have hz : mass (x i) p = 0 := by
          have hpair : p = (0,p.2) := Prod.ext h.symm rfl
          rw [hpair]
          exact mass_zero_left hi p.2
        have hle := likelihood_le_mass x i hp
        rw [hz] at hle
        exact False.elim ((not_le_of_gt hL) hle)
    have hb : 0 < p.2 := by
      rcases lt_or_eq_of_le (show 0 ≤ p.2 from hp.2) with hp' | h
      · exact hp'
      · obtain ⟨i, hi⟩ := hneg
        have hz : mass (x i) p = 0 := by
          rw [← mass_reflection]
          have hpair : (p.2,p.1) = (0,p.1) := Prod.ext h.symm rfl
          rw [hpair]
          exact mass_zero_left (neg_pos.mpr hi) p.1
        have hle := likelihood_le_mass x i hp
        rw [hz] at hle
        exact False.elim ((not_le_of_gt hL) hle)
    refine ⟨ha, hb, ?_⟩
    by_contra hne
    let r := sqrt (p.1*p.2)
    have hr : 0 < r := sqrt_pos.mpr (mul_pos ha hb)
    have hbetter := signed_likelihood_diag_strict hn hx hmean ha hb hne
    exact (not_lt_of_ge (hm (show (r,r) ∈ domain from ⟨hr.le,hr.le⟩))) hbetter
  have hroot (p : ℝ × ℝ) (hm : IsMaxOn (likelihood x) domain p)
      (hr : 0 < p.1) (he : p.1 = p.2) : deriv (diagonalMass k) p.1 = 0 := by
    have hs : IsMaxOn (diagonalMass k) (Ioi 0) p.1 := by
      intro t ht
      have ht' : 0 < t := ht
      have h := hm (show (t,t) ∈ domain from ⟨ht'.le, ht'.le⟩)
      change likelihood x (t,t) ≤ likelihood x p at h
      have hp' : p = (p.1,p.1) := Prod.ext rfl he.symm
      rw [hp', signed_likelihood_diagonal hx, signed_likelihood_diagonal hx] at h
      exact (pow_le_pow_iff_left₀ (diagonalMass_pos k ht').le (diagonalMass_pos k hr).le hn.ne').mp h
    exact (hs.isLocalMax (Ioi_mem_nhds hr)).deriv_eq_zero
  obtain ⟨p, hp, hm⟩ := joint_attained hn x
  obtain ⟨ha, _, he⟩ := hconst p hp hm
  refine ⟨p, ⟨hp, hm⟩, fun q hq ↦ ?_⟩
  obtain ⟨ha', _, he'⟩ := hconst q hq.1 hq.2
  have h := diagonal_critical_unique hk ha' ha (hroot q hq.2 ha' he') (hroot p hm ha he)
  exact Prod.ext h (he'.symm.trans (h.trans he))

end

end UnivariateML.Discrete3Skellam

end
