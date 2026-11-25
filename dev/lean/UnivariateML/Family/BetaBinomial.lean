module

public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.Convex.Jensen
public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Topology.Order.Compact
public import Mathlib.Analysis.SpecialFunctions.FrullaniIntegral
public import Mathlib.Analysis.SpecialFunctions.ImproperIntegrals
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Fixed-size beta-binomial likelihood

The sample consists of natural counts bounded by the fixed size. The two
shape parameters are positive. Dispersion is measured using variance with
divisor equal to the sample size.

## Deviations

The statements concern exact real arithmetic. They do not model the
floating-point certificate or fits with a held shape or unknown size.
The dossier calls necessity conjectural for sizes at least three. Here the
strict comparison follows the global proof in the beta-binomial math report.
-/

@[expose] public section

open Set Filter Topology Real MeasureTheory
open scoped BigOperators

namespace UnivariateML.BetaBinomial

noncomputable section

variable {m n : ℕ}

def risingLog (k : ℕ) (a : ℝ) : ℝ := ∑ j ∈ Finset.range k, log (a + j)

def logLik (m : ℕ) (x : Fin n → ℕ) (a b : ℝ) : ℝ :=
  ∑ i, (log (m.choose (x i)) + risingLog (x i) a + risingLog (m - x i) b -
    risingLog m (a + b))

def binomialLogLik (m : ℕ) (x : Fin n → ℕ) (p : ℝ) : ℝ :=
  ∑ i, (log (m.choose (x i)) + (x i : ℝ) * log p + (m - x i : ℕ) * log (1 - p))

def countSum (x : Fin n → ℕ) : ℝ := ∑ i, (x i : ℝ)

def squareSum (x : Fin n → ℕ) : ℝ := ∑ i, (x i : ℝ)^2

def mean (x : Fin n → ℕ) : ℝ := countSum x / n

def proportion (m : ℕ) (x : Fin n → ℕ) : ℝ := mean x / m

def variance (x : Fin n → ℕ) : ℝ := (∑ i, ((x i : ℝ) - mean x)^2) / n

def delta (m : ℕ) (x : Fin n → ℕ) : ℝ :=
  (m : ℝ) * n * squareSum x - ((m : ℝ) - 1) * countSum x ^ 2 -
    (m : ℝ) * n * countSum x

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

variable {x : Fin n → ℕ}

private theorem variance_formula (hn : 0 < n) (x : Fin n → ℕ) :
    variance x = squareSum x / n - mean x ^ 2 := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  unfold variance squareSum
  simp_rw [sub_sq, Finset.sum_add_distrib, Finset.sum_sub_distrib,
    ← Finset.sum_mul, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  rw [← Finset.mul_sum]
  change (_ - 2 * countSum x * mean x + n * mean x ^ 2) / n = _
  unfold mean
  field_simp
  ring

/-- The signed moment statistic is the scaled excess over binomial variance. -/
theorem delta_identity (hm : 0 < m) (hn : 0 < n) (x : Fin n → ℕ) :
    delta m x = (m : ℝ) * (n : ℝ)^2 *
      (variance x - m * proportion m x * (1 - proportion m x)) := by
  have hM : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  rw [variance_formula hn]
  unfold delta proportion mean
  field_simp
  ring

/-- Log-likelihood in mean and inverse concentration coordinates, extended to the binomial face. -/
def faceLogLik (m : ℕ) (x : Fin n → ℕ) (p s : ℝ) : ℝ :=
  ∑ i, (log (m.choose (x i)) +
    (∑ j ∈ Finset.range (x i), log (p + (j : ℝ) * s)) +
    (∑ j ∈ Finset.range (m - x i), log (1 - p + (j : ℝ) * s)) -
    (∑ j ∈ Finset.range m, log (1 + (j : ℝ) * s)))

private theorem face_eq_logLik (hx : ∀ i, x i ≤ m) {p s : ℝ}
    (hp : 0 < p) (hp' : p < 1) (hs : 0 < s) :
    faceLogLik m x p s = logLik m x (p / s) ((1 - p) / s) := by
  have term (c : ℝ) (hc : 0 < c) (k : ℕ) :
      risingLog k (c / s) = (∑ j ∈ Finset.range k, log (c + (j : ℝ) * s)) -
        k * log s := by
    unfold risingLog
    have h (j : ℕ) : log (c / s + j) = log (c + (j : ℝ) * s) - log s := by
      rw [show c / s + j = (c + (j : ℝ) * s) / s by field_simp]
      exact log_div (by positivity) hs.ne'
    simp [h, Finset.sum_sub_distrib]
  unfold faceLogLik logLik
  apply Finset.sum_congr rfl
  intro i _
  rw [show p / s + (1 - p) / s = 1 / s by ring, term p hp, term (1 - p) (by linarith),
    term 1 zero_lt_one, Nat.cast_sub (hx i)]
  ring

private theorem face_zero (x : Fin n → ℕ) (p : ℝ) :
    faceLogLik m x p 0 = binomialLogLik m x p := by
  simp [faceLogLik, binomialLogLik]

private theorem face_hasDerivAt (x : Fin n → ℕ) {p : ℝ} (hp : 0 < p) (hp' : p < 1) :
    HasDerivAt (faceLogLik m x p)
      (∑ i, ((∑ j ∈ Finset.range (x i), (j : ℝ)) / p +
        (∑ j ∈ Finset.range (m - x i), (j : ℝ)) / (1 - p) -
        (∑ j ∈ Finset.range m, (j : ℝ)))) 0 := by
  have h (c : ℝ) (hc : 0 < c) (k : ℕ) :
      HasDerivAt (fun s : ℝ ↦ ∑ j ∈ Finset.range k, log (c + (j : ℝ) * s))
        ((∑ j ∈ Finset.range k, (j : ℝ)) / c) 0 := by
    rw [Finset.sum_div]
    apply HasDerivAt.fun_sum
    intro j _
    simpa using
      (((hasDerivAt_id (0 : ℝ)).const_mul (j : ℝ)).const_add c).log (by simpa using hc.ne')
  apply HasDerivAt.fun_sum
  intro i _
  simpa only [Pi.add_def, Pi.sub_def, div_one] using
    (((h p hp (x i)).const_add (log (m.choose (x i)))).add
      (h (1 - p) (sub_pos.mpr hp') (m - x i))).sub (h 1 zero_lt_one m)

/-- Positive shape pairs approaching the binomial face have the stated log-likelihood limit. -/
theorem logLik_tendsto (hx : ∀ i, x i ≤ m) {p : ℝ} (hp : 0 < p) (hp' : p < 1) :
    Tendsto (fun s : ℝ ↦ logLik m x (p / s) ((1 - p) / s))
      (𝓝[>] 0) (𝓝 (binomialLogLik m x p)) := by
  rw [← face_zero]
  apply (face_hasDerivAt (m := m) x hp hp').continuousAt.continuousWithinAt.tendsto.congr'
  filter_upwards [self_mem_nhdsWithin] with s hs
  exact face_eq_logLik hx hp hp' hs

private theorem likelihood_nonempty (m : ℕ) (x : Fin n → ℕ) :
    ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain).Nonempty :=
  ⟨_, (1, 1), ⟨zero_lt_one, zero_lt_one⟩, rfl⟩

private theorem binomial_le_upperBound (hx : ∀ i, x i ≤ m) {p b : ℝ}
    (hp : 0 < p) (hp' : p < 1)
    (hb : b ∈ upperBounds ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain)) :
    binomialLogLik m x p ≤ b := by
  refine le_of_tendsto (logLik_tendsto hx hp hp') ?_
  filter_upwards [self_mem_nhdsWithin] with s hs
  exact hb ⟨(p / s, (1 - p) / s), ⟨div_pos hp hs, div_pos (sub_pos.mpr hp') hs⟩, rfl⟩

private theorem sum_range_cast (k : ℕ) :
    (∑ j ∈ Finset.range k, (j : ℝ)) = (k : ℝ) * ((k : ℝ) - 1) / 2 := by
  induction k with
  | zero => simp
  | succ k ih =>
    rw [Finset.sum_range_succ, ih, Nat.cast_succ]
    ring

private theorem sum_reflected (hx : ∀ i, x i ≤ m) :
    (∑ i, (m - x i : ℕ) : ℝ) = (n : ℝ) * m - countSum x := by
  simp [Nat.cast_sub (hx _), Finset.sum_sub_distrib, countSum]

private theorem sum_reflected_sq (hx : ∀ i, x i ≤ m) :
    (∑ i, ((m - x i : ℕ) : ℝ)^2) = (n : ℝ) * (m : ℝ)^2 -
      2 * m * countSum x + squareSum x := by
  simp_rw [Nat.cast_sub (hx _), sub_sq, Finset.sum_add_distrib,
    Finset.sum_sub_distrib, ← Finset.mul_sum]
  simp [countSum, squareSum]

private theorem countSum_eq (hm : 0 < m) (hn : 0 < n) (x : Fin n → ℕ) :
    countSum x = (n : ℝ) * m * proportion m x := by
  have hM : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  unfold proportion mean
  field_simp

/-- The empirical proportion maximizes the binomial log-likelihood over open probabilities. -/
theorem binomial_isMaxOn (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1) :
    IsMaxOn (binomialLogLik m x) (Ioo 0 1) (proportion m x) := by
  let u := proportion m x
  intro p hp
  change binomialLogLik m x p ≤ binomialLogLik m x u
  have hup : 0 < 1 - u := sub_pos.mpr hu'
  have hpp : 0 < 1 - p := sub_pos.mpr hp.2
  have hp₀ : 0 < p := hp.1
  have H := log_le_sub_one_of_pos (div_pos hp₀ hu)
  have H' := log_le_sub_one_of_pos (div_pos hpp hup)
  rw [log_div hp₀.ne' hu.ne'] at H
  rw [log_div hpp.ne' hup.ne'] at H'
  have A := mul_le_mul_of_nonneg_left H hu.le
  have B := mul_le_mul_of_nonneg_left H' hup.le
  have hpu : u * (p / u - 1) = p - u := by
    have h : u ≠ 0 := hu.ne'
    field_simp
  have hpv : (1 - u) * ((1 - p) / (1 - u) - 1) = (1 - p) - (1 - u) := by
    field_simp [hup.ne']
  change u * (log p - log u) ≤ u * (p / u - 1) at A
  rw [hpu] at A
  rw [hpv] at B
  have C : u * log p + (1 - u) * log (1 - p) ≤
      u * log u + (1 - u) * log (1 - u) := by linarith
  have hS : countSum x = (n : ℝ) * m * u := countSum_eq hm hn x
  have hf (r : ℝ) : binomialLogLik m x r =
      (∑ i, log (m.choose (x i))) +
        ((n : ℝ) * m) * (u * log r + (1 - u) * log (1 - r)) := by
    unfold binomialLogLik
    simp only [Finset.sum_add_distrib, ← Finset.sum_mul]
    rw [sum_reflected hx, show (∑ i, (x i : ℝ)) = countSum x from rfl, hS]
    ring
  rw [hf, hf]
  gcongr

/-- The inverse-concentration score at the best binomial equals the scaled moment statistic. -/
theorem boundary_score (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1) :
    HasDerivAt (faceLogLik m x (proportion m x))
      (delta m x / (2 * n * m * proportion m x * (1 - proportion m x))) 0 := by
  set u := proportion m x
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hM : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  have hV : 1 - u ≠ 0 := (sub_pos.mpr hu').ne'
  have hsum (z : Fin n → ℕ) :
      (∑ i, ∑ j ∈ Finset.range (z i), (j : ℝ)) =
        (squareSum z - countSum z) / 2 := by
    simp_rw [sum_range_cast, mul_sub, ← sq, mul_one, ← Finset.sum_div,
      Finset.sum_sub_distrib, squareSum, countSum]
  have hr : squareSum (fun i ↦ m - x i) =
      (n : ℝ) * (m : ℝ)^2 - 2 * m * countSum x + squareSum x := sum_reflected_sq hx
  have hr' : countSum (fun i ↦ m - x i) = (n : ℝ) * m - countSum x := sum_reflected hx
  have hu₀ : u ≠ 0 := hu.ne'
  have hS : countSum x = (n : ℝ) * m * u := countSum_eq hm hn x
  convert face_hasDerivAt x hu hu' using 1
  simp only [Finset.sum_sub_distrib, Finset.sum_add_distrib, ← Finset.sum_div,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  rw [hsum x, hsum (fun i ↦ m - x i), hr, hr', sum_range_cast, delta, hS]
  field_simp
  ring

/-- Overdispersion gives positive inverse concentration with likelihood above the binomial face. -/
theorem exists_above_binomial (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1)
    (hvar : (m : ℝ) * proportion m x * (1 - proportion m x) < variance x) :
    ∃ s > 0, binomialLogLik m x (proportion m x) <
      logLik m x (proportion m x / s) ((1 - proportion m x) / s) := by
  have hd : 0 < delta m x := by
    rw [delta_identity hm hn]
    have hm' : (0 : ℝ) < m := by exact_mod_cast hm
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    exact mul_pos (mul_pos hm' (pow_pos hn' _)) (sub_pos.mpr hvar)
  have hs : 0 < delta m x / (2 * n * m * proportion m x * (1 - proportion m x)) := by
    have hm' : (0 : ℝ) < m := by exact_mod_cast hm
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    exact div_pos hd (mul_pos (mul_pos (mul_pos (mul_pos (by norm_num) hn') hm') hu)
      (sub_pos.mpr hu'))
  have H := (boundary_score hm hn hx hu hu').tendsto_slope_zero
  have He := H.eventually (Ioi_mem_nhds hs)
  have Hn : 𝓝[>] (0 : ℝ) ≤ 𝓝[≠] 0 := nhdsWithin_mono _ (fun _ h ↦ (ne_of_gt h))
  have Hp := He.filter_mono Hn
  have Hb : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
  obtain ⟨s, h₁, h₂⟩ := (Hp.and Hb).exists
  refine ⟨s, h₂, ?_⟩
  rw [← face_eq_logLik hx hu hu' h₂, ← face_zero]
  have Hs : 0 < s⁻¹ * (faceLogLik m x (proportion m x) s -
      faceLogLik m x (proportion m x) 0) := by simpa [smul_eq_mul] using h₁
  exact sub_pos.mp (pos_of_mul_pos_right Hs (inv_pos.mpr h₂).le)

-- Integral interpolation of the normalized rising-factorial logarithm.
private def kernel (z t : ℝ) : ℝ := (1 - exp (-z * t)) / (z * (1 - exp (-t)))

private def kernelAverage (z t : ℝ) : ℝ := ∫ w in (0 : ℝ)..1, exp (-z * t * w)

private theorem kernelAverage_formula {z t : ℝ} (hz : 0 < z) (ht : 0 < t) :
    kernelAverage z t = (1 - exp (-z * t)) / (z * t) := by
  have hc : -z * t ≠ 0 := mul_ne_zero (neg_ne_zero.mpr hz.ne') ht.ne'
  unfold kernelAverage
  rw [intervalIntegral.integral_comp_mul_left _ hc, integral_exp]
  simp
  field_simp [hz.ne', ht.ne']
  ring

private theorem kernel_eq_average {z t : ℝ} (hz : 0 < z) (ht : 0 < t) :
    kernel z t = kernelAverage z t / kernelAverage 1 t := by
  have he : exp (-t) < 1 := by simpa using exp_lt_exp.mpr (neg_neg_of_pos ht)
  rw [kernel, kernelAverage_formula hz ht, kernelAverage_formula zero_lt_one ht]
  simp only [neg_one_mul, one_mul]
  field_simp [hz.ne', ht.ne', (sub_pos.mpr he).ne']

private theorem kernelAverage_pos {z t : ℝ} (hz : 0 < z) (ht : 0 < t) :
    0 < kernelAverage z t := by
  rw [kernelAverage_formula hz ht]
  apply div_pos _ (mul_pos hz ht)
  apply sub_pos.mpr
  simpa using exp_lt_exp.mpr (mul_neg_of_neg_of_pos (neg_neg_of_pos hz) ht)

private theorem kernelAverage_mono {z y t : ℝ} (ht : 0 < t) (hzy : z ≤ y) :
    kernelAverage y t ≤ kernelAverage z t := by
  have hc (r : ℝ) : Continuous (fun w : ℝ ↦ exp (-r * t * w)) := by fun_prop
  apply intervalIntegral.integral_mono_on zero_le_one
    ((hc y).intervalIntegrable _ _) ((hc z).intervalIntegrable _ _)
  intro w hw
  exact exp_le_exp.mpr (by nlinarith [mul_nonneg (mul_nonneg (sub_nonneg.mpr hzy) ht.le) hw.1])

private theorem kernel_le_one {z t : ℝ} (hz : 1 ≤ z) (ht : 0 < t) :
    kernel z t ≤ 1 := by
  rw [kernel_eq_average (zero_lt_one.trans_le hz) ht, div_le_one (kernelAverage_pos zero_lt_one ht)]
  exact kernelAverage_mono ht hz

private theorem kernel_lower {z t : ℝ} (hz : 0 < z) (ht : 0 < t) :
    exp (-z * t) ≤ kernel z t := by
  have hc : Continuous (fun w : ℝ ↦ exp (-z * t * w)) := by fun_prop
  have h : exp (-z * t) ≤ kernelAverage z t := by
    unfold kernelAverage
    convert intervalIntegral.integral_mono_on (μ := volume) zero_le_one
      (continuous_const.intervalIntegrable 0 1) (hc.intervalIntegrable 0 1)
      (fun w hw ↦ exp_le_exp.mpr (show -z * t ≤ -z * t * w by
        nlinarith [mul_nonneg (mul_nonneg hz.le ht.le) (sub_nonneg.mpr hw.2)])) using 1
    simp
  have h₁ : kernelAverage 1 t ≤ 1 := by
    simpa [kernelAverage] using kernelAverage_mono ht (show (0 : ℝ) ≤ 1 by norm_num)
  rw [kernel_eq_average hz ht, le_div_iff₀ (kernelAverage_pos zero_lt_one ht)]
  exact (mul_le_of_le_one_right (exp_pos _).le h₁).trans h

private def integrand (a z t : ℝ) : ℝ := exp (-a * t) / t * (1 - kernel z t)

private def interpolatedLog (a z : ℝ) : ℝ := log a + ∫ t in Ioi 0, integrand a z t

private theorem integrand_nonneg {a z t : ℝ} (hz : 1 ≤ z) (ht : 0 < t) :
    0 ≤ integrand a z t :=
  mul_nonneg (div_nonneg (exp_pos _).le ht.le) (sub_nonneg.mpr (kernel_le_one hz ht))

private theorem integrand_integrable {a z : ℝ} (ha : 0 < a) (hz : 1 ≤ z) :
    IntegrableOn (integrand a z) (Ioi 0) := by
  have hg := (integrableOn_exp_mul_Ioi (neg_neg_of_pos ha) 0).const_mul z
  apply hg.mono' (by
    unfold integrand kernel
    fun_prop)
  filter_upwards [ae_restrict_mem (measurableSet_Ioi (a := (0 : ℝ)))] with t ht
  have ht' : 0 < t := ht
  rw [Real.norm_eq_abs, abs_of_nonneg (integrand_nonneg hz ht')]
  have h := kernel_lower (zero_lt_one.trans_le hz) ht'
  have h' := add_one_le_exp (-z * t)
  change exp (-a * t) / t * (1 - kernel z t) ≤ z * exp (-a * t)
  calc
    _ ≤ exp (-a * t) / t * (z * t) :=
      mul_le_mul_of_nonneg_left (by linarith) (by positivity)
    _ = _ := by field_simp

private theorem kernel_cast {k : ℕ} (hk : 0 < k) {t : ℝ} (ht : 0 < t) :
    kernel k t = (∑ j ∈ Finset.range k, exp (-(j : ℝ) * t)) / k := by
  have he : exp (-t) ≠ 1 := ne_of_lt (by simpa using exp_lt_exp.mpr (neg_neg_of_pos ht))
  have hk' : (k : ℝ) ≠ 0 := by exact_mod_cast hk.ne'
  simp_rw [show ∀ j : ℕ, exp (-(j : ℝ) * t) = exp (-t) ^ j by
    intro j
    rw [← exp_nat_mul]
    congr 1
    ring]
  rw [geom_sum_eq he, ← exp_nat_mul, kernel]
  rw [show (k : ℝ) * -t = -(k : ℝ) * t by ring]
  field_simp [sub_ne_zero.mpr he, sub_ne_zero.mpr he.symm]
  ring

private def logDifferenceIntegrand (a j t : ℝ) : ℝ :=
  t⁻¹ * (exp (- (a * t)) - exp (- ((a + j) * t)))

private theorem logDifference_integrable {a j : ℝ} (ha : 0 < a) (hj : 0 ≤ j) :
    IntegrableOn (logDifferenceIntegrand a j) (Ioi 0) := by
  apply ((integrableOn_exp_mul_Ioi (neg_neg_of_pos ha) 0).const_mul j).mono'
    (by unfold logDifferenceIntegrand; fun_prop)
  filter_upwards [ae_restrict_mem (measurableSet_Ioi (a := (0 : ℝ)))] with t ht
  have ht' : 0 < t := ht
  have hE : exp (-j * t) ≤ 1 := by
    simpa using exp_le_exp.mpr (show -j * t ≤ 0 by nlinarith [mul_nonneg hj ht'.le])
  have hE' : 1 - exp (-j * t) ≤ j * t := by linarith [add_one_le_exp (-j * t)]
  have hfac : logDifferenceIntegrand a j t = exp (-a * t) / t * (1 - exp (-j * t)) := by
    unfold logDifferenceIntegrand
    rw [show -((a + j) * t) = -a * t + -j * t by ring, exp_add,
      show -(a * t) = -a * t by ring]
    ring
  rw [hfac, Real.norm_eq_abs, abs_of_nonneg (by positivity)]
  calc
    _ ≤ exp (-a * t) / t * (j * t) := mul_le_mul_of_nonneg_left hE' (by positivity)
    _ = _ := by field_simp

private theorem logDifference_integral {a j : ℝ} (ha : 0 < a) (hj : 0 ≤ j) :
    (∫ t in Ioi 0, logDifferenceIntegrand a j t) = log (a + j) - log a := by
  let f := fun t : ℝ ↦ exp (-t)
  have hc : Continuous f := by dsimp [f]; fun_prop
  have hL : Tendsto f (𝓝[>] 0) (𝓝 1) := by
    simpa [f] using hc.continuousAt.continuousWithinAt.tendsto (x := (0 : ℝ)) (s := Ioi 0)
  have hR : Tendsto f atTop (𝓝 0) := tendsto_exp_atBot.comp tendsto_neg_atTop_atBot
  have h := Frullani.integral_Ioi_eq (hc.continuousOn.locallyIntegrableOn measurableSet_Ioi) ha
    (add_pos_of_pos_of_nonneg ha hj) hL hR (logDifference_integrable ha hj)
  simpa [f, smul_eq_mul, logDifferenceIntegrand, log_div (by positivity : a + j ≠ 0) ha.ne'] using h

private theorem interpolatedLog_cast {a : ℝ} (ha : 0 < a) {k : ℕ} (hk : 0 < k) :
    interpolatedLog a k = risingLog k a / k := by
  have hk' : (k : ℝ) ≠ 0 := by exact_mod_cast hk.ne'
  have hEq : (∫ t in Ioi (0 : ℝ), integrand a k t) =
      ∫ t in Ioi (0 : ℝ), (k : ℝ)⁻¹ * ∑ j ∈ Finset.range k, logDifferenceIntegrand a j t := by
    apply setIntegral_congr_fun measurableSet_Ioi
    intro t ht
    rw [integrand, kernel_cast hk ht]
    have he (j : ℕ) : exp (-((a + j) * t)) = exp (-a * t) * exp (-(j : ℝ) * t) := by
      rw [← exp_add]
      congr 1
      ring
    simp only [logDifferenceIntegrand, he, ← Finset.mul_sum, Finset.sum_sub_distrib,
      Finset.sum_const, Finset.card_range, nsmul_eq_mul]
    field_simp
  unfold interpolatedLog risingLog
  rw [hEq, integral_const_mul,
    integral_finsetSum _ (fun j _ ↦ logDifference_integrable ha (Nat.cast_nonneg j))]
  simp_rw [logDifference_integral ha (Nat.cast_nonneg _), Finset.sum_sub_distrib,
    Finset.sum_const, Finset.card_range, nsmul_eq_mul]
  field_simp
  ring

private theorem kernel_convex {t : ℝ} (ht : 0 < t) :
    ConvexOn ℝ (Ici 1) (fun z ↦ kernel z t) := by
  refine ⟨convex_Ici 1, ?_⟩
  intro z hz y hy r s hr hs hrs
  simp only [smul_eq_mul]
  have hzy : 1 ≤ r * z + s * y := by
    nlinarith [mul_nonneg hr (sub_nonneg.mpr hz), mul_nonneg hs (sub_nonneg.mpr hy)]
  rw [kernel_eq_average (zero_lt_one.trans_le hzy) ht,
    kernel_eq_average (zero_lt_one.trans_le hz) ht, kernel_eq_average (zero_lt_one.trans_le hy) ht,
    ← mul_div_assoc, ← mul_div_assoc, ← add_div]
  apply (div_le_div_iff_of_pos_right (kernelAverage_pos zero_lt_one ht)).mpr
  have hc (q : ℝ) : Continuous (fun w : ℝ ↦ exp (-q * t * w)) := by fun_prop
  unfold kernelAverage
  rw [← intervalIntegral.integral_const_mul, ← intervalIntegral.integral_const_mul,
    ← intervalIntegral.integral_add ((hc z).const_mul r |>.intervalIntegrable 0 1)
      ((hc y).const_mul s |>.intervalIntegrable 0 1)]
  apply intervalIntegral.integral_mono_on zero_le_one ((hc _).intervalIntegrable _ _)
    (((hc z).const_mul r).add ((hc y).const_mul s) |>.intervalIntegrable _ _)
  intro w _
  have H := convexOn_exp.2 (mem_univ (-z * t * w)) (mem_univ (-y * t * w)) hr hs hrs
  simp only [smul_eq_mul] at H
  rw [show -(r * z + s * y) * t * w =
    r * (-z * t * w) + s * (-y * t * w) by ring]
  exact H

private theorem interpolatedLog_mono {a : ℝ} (ha : 0 < a) :
    MonotoneOn (interpolatedLog a) (Ici 1) := by
  intro z hz y hy hzy
  unfold interpolatedLog
  apply add_le_add_right
  apply integral_mono_ae (integrand_integrable ha hz) (integrand_integrable ha hy)
  filter_upwards [ae_restrict_mem (measurableSet_Ioi (a := (0 : ℝ)))] with t ht
  have H : kernel y t ≤ kernel z t := by
    rw [kernel_eq_average (zero_lt_one.trans_le hy) ht,
      kernel_eq_average (zero_lt_one.trans_le hz) ht]
    exact div_le_div_of_nonneg_right (kernelAverage_mono ht hzy)
      (kernelAverage_pos zero_lt_one ht).le
  have ht' : 0 < t := ht
  exact mul_le_mul_of_nonneg_left (sub_le_sub_left H 1) (by positivity : 0 ≤ exp (-a * t) / t)

private theorem interpolatedLog_concave {a : ℝ} (ha : 0 < a) :
    ConcaveOn ℝ (Ici 1) (interpolatedLog a) := by
  refine ⟨convex_Ici 1, ?_⟩
  intro z hz y hy r s hr hs hrs
  simp only [smul_eq_mul]
  have hzy : 1 ≤ r * z + s * y := by
    nlinarith [mul_nonneg hr (sub_nonneg.mpr hz), mul_nonneg hs (sub_nonneg.mpr hy)]
  have H : (∫ t in Ioi (0 : ℝ), r * integrand a z t + s * integrand a y t) ≤
      ∫ t in Ioi (0 : ℝ), integrand a (r * z + s * y) t := by
    apply integral_mono_ae (((integrand_integrable ha hz).const_mul r).add
      ((integrand_integrable ha hy).const_mul s)) (integrand_integrable ha hzy)
    filter_upwards [ae_restrict_mem (measurableSet_Ioi (a := (0 : ℝ)))] with t ht
    have ht' : 0 < t := ht
    have Hk := (kernel_convex ht').2 hz hy hr hs hrs
    simp only [smul_eq_mul] at Hk
    have hm := mul_le_mul_of_nonneg_left Hk (show 0 ≤ exp (-a * t) / t by positivity)
    dsimp [integrand]
    nlinarith only [hm, congrArg (fun q : ℝ ↦ q * (exp (-a * t) / t)) hrs]
  rw [integral_add ((integrand_integrable ha hz).const_mul r)
    ((integrand_integrable ha hy).const_mul s), integral_const_mul, integral_const_mul] at H
  unfold interpolatedLog
  linarith only [H, congrArg (fun q : ℝ ↦ q * log a) hrs]

-- The strictly increasing hyperbolic increment gives the strict scaling inequality.
private def logSinhRatio (c r : ℝ) : ℝ := log (sinh (c * r) / (c * r))

private def sinhScore (c r : ℝ) : ℝ := c * cosh (c * r) / sinh (c * r) - r⁻¹

private theorem logSinhRatio_deriv {c r : ℝ} (hc : 0 < c) (hr : 0 < r) :
    HasDerivAt (logSinhRatio c) (sinhScore c r) r := by
  have hp := mul_pos hc hr
  have hs := sinh_pos_iff.mpr hp
  have hd := (hasDerivAt_id r).const_mul c
  have H := (hd.sinh.log hs.ne').sub (hd.log hp.ne')
  have he : logSinhRatio c =ᶠ[𝓝 r] fun q ↦ log (sinh (c * q)) - log (c * q) := by
    filter_upwards [Ioi_mem_nhds hr] with q hq
    exact log_div (sinh_pos_iff.mpr (mul_pos hc hq)).ne' (mul_pos hc hq).ne'
  apply HasDerivAt.congr_of_eventuallyEq _ he
  convert H using 1
  · simp [Pi.sub_def]
  · unfold sinhScore
    simp only [id_eq, mul_one]
    field_simp [hc.ne', hr.ne', hs.ne']

private theorem sinhScore_deriv {c r : ℝ} (hc : 0 < c) (hr : 0 < r) :
    HasDerivAt (sinhScore c) (1 / r^2 - c^2 / sinh (c * r)^2) r := by
  have hs := sinh_pos_iff.mpr (mul_pos hc hr)
  have hd := (hasDerivAt_id r).const_mul c
  have H := ((hd.cosh.const_mul c).div hd.sinh hs.ne').sub
    ((hasDerivAt_id r).inv hr.ne')
  convert H using 1
  · funext q
    simp [sinhScore, Pi.div_def, Pi.inv_def]
  · change _ = (c * (sinh (c * r) * (c * 1)) * sinh (c * r) -
      c * cosh (c * r) * (cosh (c * r) * (c * 1))) / sinh (c * r)^2 - -1 / r^2
    rw [show c * (sinh (c * r) * (c * 1)) * sinh (c * r) -
      c * cosh (c * r) * (cosh (c * r) * (c * 1)) = -(c ^ 2) by
        linear_combination -(c ^ 2) * cosh_sq_sub_sinh_sq (c * r)]
    ring

private theorem sinhScore_strictMono {c : ℝ} (hc : 0 < c) :
    StrictMonoOn (sinhScore c) (Ioi 0) := by
  apply strictMonoOn_of_deriv_pos (convex_Ioi _)
    (fun r hr ↦ (sinhScore_deriv hc hr).continuousAt.continuousWithinAt)
  intro r hr
  rw [interior_Ioi] at hr
  have hr' : 0 < r := hr
  rw [(sinhScore_deriv hc hr').deriv, sub_pos]
  have H := self_lt_sinh_iff.mpr (mul_pos hc hr')
  apply (div_lt_div_iff₀ (by positivity) (by positivity)).mpr
  have hc' := mul_pos hc hr'
  have hsum := add_pos (sinh_pos_iff.mpr hc') hc'
  nlinarith only [mul_pos (sub_pos.mpr H) hsum]

private theorem logSinhRatio_increment {c L : ℝ} (hc : 0 < c) (hL : 0 < L) :
    StrictMonoOn (fun d ↦ logSinhRatio c (L + d) - logSinhRatio c d) (Ioi 0) := by
  have H (d : ℝ) (hd : 0 < d) := ((logSinhRatio_deriv hc (add_pos hL hd)).comp d
    ((hasDerivAt_id d).const_add L)).sub (logSinhRatio_deriv hc hd)
  apply strictMonoOn_of_deriv_pos (convex_Ioi _)
    (fun d hd ↦ (H d hd).continuousAt.continuousWithinAt)
  intro d hd
  rw [interior_Ioi] at hd
  rw [(H d hd).deriv, mul_one, sub_pos]
  exact sinhScore_strictMono hc hd (add_pos hL hd) (by linarith)

private theorem kernel_rescale {L d t : ℝ} (hL : 0 < L) (hd : 0 < d) (ht : 0 < t) :
    kernel (1 + L / d) (d * t) =
      exp (-(L * (t / 2)) + (logSinhRatio (t / 2) (L + d) - logSinhRatio (t / 2) d)) := by
  have hc : 0 < t / 2 := by positivity
  have h₁ := mul_pos hc (add_pos hL hd)
  have h₂ := mul_pos hc hd
  have aux (q : ℝ) : 1 - exp (-(2 * q)) = 2 * exp (-q) * sinh q := by
    rw [Real.sinh_eq, show -(2 * q) = -q + -q by ring, exp_add]
    have h : exp (-q) * exp q = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
    linarith only [h]
  unfold kernel logSinhRatio
  rw [exp_add, exp_sub, exp_log (div_pos (sinh_pos_iff.mpr h₁) h₁),
    exp_log (div_pos (sinh_pos_iff.mpr h₂) h₂)]
  rw [show -(1 + L / d) * (d * t) = -(2 * (t / 2 * (L + d))) by field_simp; ring,
    show -(d * t) = -(2 * (t / 2 * d)) by ring, aux, aux]
  rw [show -(t / 2 * (L + d)) = -(L * (t / 2)) + -(t / 2 * d) by ring, exp_add]
  have hs := (sinh_pos_iff.mpr h₂).ne'
  have hld := (add_pos hL hd).ne'
  have hz : 1 + L / d ≠ 0 := by positivity
  field_simp [hd.ne', ht.ne', hs, hld, hz, (exp_pos _).ne']
  ring

private theorem interpolatedLog_scaling {a w M : ℝ}
    (ha : 0 < a) (hw : 0 < w) (hw' : w < 1) (hM : 1 < M) :
    interpolatedLog a (1 + (M - 1) * w) < log w + interpolatedLog (a / w) M := by
  let L := M - 1
  let d := w⁻¹
  let z := 1 + L * w
  have hL : 0 < L := sub_pos.mpr hM
  have hd : 1 < d := by
    dsimp [d]
    simpa only [one_div] using (one_lt_div hw).2 hw'
  have hz : 1 ≤ z := le_add_of_nonneg_right (mul_nonneg hL.le hw.le)
  have hKernel (t : ℝ) (ht : 0 < t) : kernel M t < kernel z (d * t) := by
    have H := logSinhRatio_increment (show 0 < t / 2 by positivity) hL
      (show (1 : ℝ) ∈ Ioi 0 from zero_lt_one) (zero_lt_one.trans hd) hd
    rw [show kernel M t = kernel (1 + L / 1) (1 * t) by dsimp [L]; ring_nf,
      show z = 1 + L / d by simp [z, d, div_eq_mul_inv, inv_inv],
      kernel_rescale hL zero_lt_one ht, kernel_rescale hL (zero_lt_one.trans hd) ht]
    exact exp_lt_exp.mpr (add_lt_add_right H _)
  let f := fun t ↦ d * integrand a z (d * t)
  have hfi : IntegrableOn f (Ioi 0) := by
    apply Integrable.const_mul _ d
    exact (integrableOn_Ioi_comp_mul_left_iff (integrand a z) 0 (zero_lt_one.trans hd)).2
      (by simpa using integrand_integrable ha hz)
  have hgi := integrand_integrable (div_pos ha hw) hM.le
  have hlt (t : ℝ) (ht : 0 < t) : f t < integrand (a / w) M t := by
    have he : d * (exp (-a * (d * t)) / (d * t)) = exp (-(a / w) * t) / t := by
      have h : -a * (d * t) = -(a / w) * t := by simp [d, div_eq_mul_inv]; ring
      rw [h]
      field_simp [show d ≠ 0 from (zero_lt_one.trans hd).ne']
    dsimp [f, integrand]
    rw [← mul_assoc, he]
    gcongr
    exact hKernel t ht
  have H : (∫ t in Ioi (0 : ℝ), f t) < ∫ t in Ioi (0 : ℝ), integrand (a / w) M t := by
    rw [← sub_pos, ← integral_sub hgi hfi]
    apply (setIntegral_pos_iff_support_of_nonneg_ae _ (hgi.sub hfi)).2
    · have hset : Function.support (fun t ↦ integrand (a / w) M t - f t) ∩ Ioi 0 = Ioi 0 := by
        apply inter_eq_right.mpr
        intro t ht
        exact (sub_pos.mpr (hlt t ht)).ne'
      change 0 < volume (Function.support (fun t ↦ integrand (a / w) M t - f t) ∩ Ioi 0)
      rw [hset]
      simp
    · filter_upwards [ae_restrict_mem (measurableSet_Ioi (a := (0 : ℝ)))] with t ht
      exact (sub_pos.mpr (hlt t ht)).le
  have hchange : (∫ t in Ioi (0 : ℝ), f t) = ∫ t in Ioi (0 : ℝ), integrand a z t := by
    unfold f
    rw [integral_const_mul]
    simpa [smul_eq_mul] using integral_comp_mul_left_Ioi' (integrand a z) 0 (zero_lt_one.trans hd)
  rw [hchange] at H
  have hlog := log_div ha.ne' hw.ne'
  change interpolatedLog a z < _
  unfold interpolatedLog
  linarith only [H, hlog]

-- Jensen with size-biased weights. A zero count carries weight zero.
private theorem numerator_bound {a w : ℝ} (ha : 0 < a) (hw : 0 < w) (hw' : w < 1)
    (hm : 2 ≤ m) (x : Fin n → ℕ) (hS : 0 < countSum x)
    (hQ : squareSum x ≤ countSum x * (1 + ((m : ℝ) - 1) * w)) :
    (∑ i, risingLog (x i) a) < countSum x * (log w + risingLog m (a / w) / m) := by
  let S := countSum x
  let z := 1 + ((m : ℝ) - 1) * w
  let wt := fun i : Fin n ↦ (x i : ℝ) / S
  let k := fun i : Fin n ↦ (max 1 (x i) : ℕ)
  have hm' : (1 : ℝ) < m := by exact_mod_cast (show 1 < m by omega)
  have hwt : ∑ i, wt i = 1 := by
    simp only [wt, ← Finset.sum_div]
    exact div_self hS.ne'
  have hk (i : Fin n) : (1 : ℝ) ≤ k i := by exact_mod_cast (le_max_left 1 (x i))
  have hmoment : (∑ i, wt i * (k i : ℝ)) = squareSum x / S := by
    rw [squareSum, Finset.sum_div]
    apply Finset.sum_congr rfl
    intro i _
    by_cases hi : x i = 0
    · simp [wt, hi]
    · simp only [wt, k, max_eq_right (show 1 ≤ x i by omega)]
      ring
  have hlow : (1 : ℝ) ≤ ∑ i, wt i * (k i : ℝ) := by
    calc
      1 = ∑ i, wt i := hwt.symm
      _ ≤ _ := Finset.sum_le_sum (fun i _ ↦
        le_mul_of_one_le_right (by dsimp [wt]; positivity) (hk i))
  have hj := (interpolatedLog_concave ha).le_map_sum
    (t := Finset.univ) (w := wt) (p := fun i ↦ (k i : ℝ))
    (fun i _ ↦ by dsimp [wt]; positivity) hwt (fun i _ ↦ hk i)
  simp only [smul_eq_mul] at hj
  have hu : (∑ i, wt i * (k i : ℝ)) ≤ z := by
    rw [hmoment]
    exact (div_le_iff₀ hS).2 (by simpa [S, z, mul_comm] using hQ)
  have hterm (i : Fin n) : wt i * interpolatedLog a (k i) = risingLog (x i) a / S := by
    by_cases hi : x i = 0
    · simp [wt, hi, risingLog]
    · have hxi : 0 < x i := by omega
      simp only [wt, k, max_eq_right (show 1 ≤ x i by omega), interpolatedLog_cast ha hxi]
      field_simp [show (x i : ℝ) ≠ 0 by exact_mod_cast hi]
  simp_rw [hterm, ← Finset.sum_div] at hj
  have hs := interpolatedLog_scaling ha hw hw' hm'
  rw [interpolatedLog_cast (div_pos ha hw) (show 0 < m by omega)] at hs
  have hbound := hj.trans (interpolatedLog_mono ha hlow (hlow.trans hu) hu)
  exact ((div_lt_iff₀ hS).1 (hbound.trans_lt hs)).trans_le (by rw [mul_comm])

private theorem risingLog_shape_concave (m : ℕ) {a b u : ℝ}
    (ha : 0 < a) (hb : 0 < b) (hu : 0 < u) (hu' : u < 1) :
    u * risingLog m (a / u) + (1 - u) * risingLog m (b / (1 - u)) ≤
      risingLog m (a + b) := by
  unfold risingLog
  rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
  apply Finset.sum_le_sum
  intro j _
  have hv := sub_pos.mpr hu'
  have hp : 0 < a / u + (j : ℝ) := by positivity
  have hq : 0 < b / (1 - u) + (j : ℝ) := by positivity
  have H := strictConcaveOn_log_Ioi.concaveOn.2 hp hq hu.le hv.le (by ring)
  simp only [smul_eq_mul] at H
  rw [show u * (a / u + (j : ℝ)) + (1 - u) * (b / (1 - u) + (j : ℝ)) =
      a + b + j by field_simp [hu.ne', hv.ne']; ring] at H
  exact H

/-- Underdispersion and exact equidispersion both give strict global binomial dominance. -/
theorem necessity (hm : 2 ≤ m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1)
    (hV : variance x ≤ (m : ℝ) * proportion m x * (1 - proportion m x))
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    logLik m x a b < binomialLogLik m x (proportion m x) := by
  let u := proportion m x
  let v := 1 - u
  let y := fun i : Fin n ↦ m - x i
  have hv : 0 < v := sub_pos.mpr hu'
  have hv' : v < 1 := by dsimp [v]; linarith
  have hm₀ : 0 < m := by omega
  have hM : (0 : ℝ) < m := by exact_mod_cast hm₀
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hS : countSum x = (n : ℝ) * m * u := countSum_eq hm₀ hn x
  have hY : countSum y = (n : ℝ) * m * v := by
    change (∑ i, (m - x i : ℕ) : ℝ) = _
    rw [sum_reflected hx, hS]
    dsimp [v]
    ring
  have hmean : mean x = (m : ℝ) * u := by
    change countSum x / n = _
    rw [hS]
    field_simp
  rw [variance_formula hn, hmean] at hV
  have hV' : squareSum x ≤ (n : ℝ) * ((m * u)^2 + m * u * v) := by
    have H : squareSum x / n ≤ (m * u)^2 + m * u * v := by linarith only [hV]
    exact (div_le_iff₀ hN).mp H |>.trans_eq (by ring)
  have hQ : squareSum x ≤ countSum x * (1 + ((m : ℝ) - 1) * u) := by
    rw [hS]
    calc
      _ ≤ _ := hV'
      _ = _ := by dsimp [v]; ring
  have hQY : squareSum y ≤ countSum y * (1 + ((m : ℝ) - 1) * v) := by
    change (∑ i, ((m - x i : ℕ) : ℝ)^2) ≤ _
    rw [sum_reflected_sq hx, hY, hS]
    dsimp [v] at hV' ⊢
    linarith only [hV']
  have hA := numerator_bound ha hu hu' hm x (by rw [hS]; positivity) hQ
  have hB := numerator_bound hb hv hv' hm y (by rw [hY]; positivity) hQY
  have H := mul_le_mul_of_nonneg_left (risingLog_shape_concave m ha hb hu hu') hN.le
  rw [hS] at hA
  rw [hY] at hB
  have hnorm (w l f : ℝ) : (n : ℝ) * m * w * (l + f / m) =
      (n : ℝ) * m * w * l + n * (w * f) := by field_simp
  rw [hnorm] at hA hB
  unfold logLik binomialLogLik
  simp only [Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.sum_mul,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  change _ + (∑ i, risingLog (x i) a) + (∑ i, risingLog (y i) b) - _ <
    _ + countSum x * log u + countSum y * log v
  rw [hS, hY]
  linarith only [hA, hB, H]

/-- The binomial likelihood is the unattained supremum in the nonpositive-dispersion regime. -/
theorem binomial_sup_of_nonpos (hm : 2 ≤ m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1)
    (hV : variance x ≤ (m : ℝ) * proportion m x * (1 - proportion m x)) :
    sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) =
      binomialLogLik m x (proportion m x) := by
  have hbound : binomialLogLik m x (proportion m x) ∈
      upperBounds ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) := by
    rintro _ ⟨⟨a, b⟩, ⟨ha, hb⟩, rfl⟩
    exact (necessity hm hn hx hu hu' hV ha hb).le
  exact le_antisymm (csSup_le (likelihood_nonempty m x) hbound)
    (binomial_le_upperBound hx hu hu' (fun _ h ↦ le_csSup ⟨_, hbound⟩ h))

-- Escape bounds in the coordinates that extend to the binomial face.
private def chooseConstant (m : ℕ) (x : Fin n → ℕ) : ℝ := ∑ i, log (m.choose (x i))

private def faceFactorLog (k : ℕ) (p s : ℝ) : ℝ :=
  ∑ j ∈ Finset.range k, log (p + (j : ℝ) * s)

private def observation (k l : ℕ) (p s : ℝ) : ℝ :=
  faceFactorLog k p s + faceFactorLog l (1 - p) s - faceFactorLog (k + l) 1 s

private theorem face_observations (hx : ∀ i, x i ≤ m) (p s : ℝ) :
    faceLogLik m x p s = chooseConstant m x + ∑ i, observation (x i) (m - x i) p s := by
  unfold faceLogLik chooseConstant observation faceFactorLog
  have hi (i : Fin n) : x i + (m - x i) = m := Nat.add_sub_of_le (hx i)
  simp only [hi, Finset.sum_add_distrib, Finset.sum_sub_distrib]
  ring

private theorem faceFactorLog_append (k l : ℕ) (p s : ℝ) :
    faceFactorLog (k + l) p s = faceFactorLog k p s + faceFactorLog l (p + k * s) s := by
  unfold faceFactorLog
  rw [Finset.sum_range_add]
  congr 1
  apply Finset.sum_congr rfl
  intro j _
  push_cast
  congr 1
  ring

private theorem faceFactorLog_mono (k : ℕ) {p q s : ℝ}
    (hp : 0 < p) (hpq : p ≤ q) (hs : 0 ≤ s) :
    faceFactorLog k p s ≤ faceFactorLog k q s := by
  apply Finset.sum_le_sum
  intro j _
  exact log_le_log (by positivity) (add_le_add_left hpq _)

private theorem faceFactorLog_first (k : ℕ) (p s : ℝ) :
    faceFactorLog (k + 1) p s = log p + faceFactorLog k (p + s) s := by
  rw [show k + 1 = 1 + k by omega, faceFactorLog_append]
  simp [faceFactorLog]

private theorem observation_le_zero (k l : ℕ) {p s : ℝ} (hp : 0 < p) (hp' : p < 1)
    (hs : 0 ≤ s) : observation k l p s ≤ 0 := by
  unfold observation
  rw [faceFactorLog_append]
  have H := faceFactorLog_mono k hp hp'.le hs
  have H' := faceFactorLog_mono l (sub_pos.mpr hp')
    (show 1 - p ≤ 1 + k * s by linarith [mul_nonneg (Nat.cast_nonneg k) hs]) hs
  linarith only [H, H']

private theorem observation_le_log_left (k l : ℕ) (hk : 0 < k)
    {p s : ℝ} (hp : 0 < p) (hp' : p < 1) (hs : 0 ≤ s) :
    observation k l p s ≤ log p := by
  obtain ⟨k, rfl⟩ := Nat.exists_eq_succ_of_ne_zero hk.ne'
  unfold observation
  rw [faceFactorLog_append (k + 1) l, faceFactorLog_first k p, faceFactorLog_first k 1, log_one]
  have H := faceFactorLog_mono k (show 0 < p + s by linarith)
    (show p + s ≤ 1 + s by linarith) hs
  have H' := faceFactorLog_mono l (sub_pos.mpr hp')
    (show 1 - p ≤ 1 + (k + 1 : ℕ) * s by
      linarith [mul_nonneg (Nat.cast_nonneg (k + 1)) hs]) hs
  linarith only [H, H']

private theorem observation_swap (k l : ℕ) (p s : ℝ) :
    observation k l p s = observation l k (1 - p) s := by
  unfold observation
  rw [show 1 - (1 - p) = p by ring, Nat.add_comm k l]
  ring

private theorem observation_tail (k l : ℕ) (hk : 0 < k) (hl : 0 < l)
    {p s : ℝ} (hp : 0 < p) (hp' : p < 1) (hs : 0 ≤ s) :
    observation k l p s ≤ -log (1 + s) := by
  obtain ⟨k, rfl⟩ := Nat.exists_eq_succ_of_ne_zero hk.ne'
  obtain ⟨l, rfl⟩ := Nat.exists_eq_succ_of_ne_zero hl.ne'
  unfold observation
  rw [faceFactorLog_first k p, faceFactorLog_first l (1 - p),
    show (k + 1) + (l + 1) = ((k + l) + 1) + 1 by omega,
    faceFactorLog_first (k + l + 1), log_one,
    faceFactorLog_append (k + l) 1,
    faceFactorLog_append k l, show faceFactorLog 1 (1 + s + (k + l : ℕ) * s) s =
      log (1 + s + (k + l : ℕ) * s) by simp [faceFactorLog]]
  have A := faceFactorLog_mono k (show 0 < p + s by linarith)
    (show p + s ≤ 1 + s by linarith) hs
  have B := faceFactorLog_mono l (show 0 < 1 - p + s by linarith)
    (show 1 - p + s ≤ 1 + s + k * s by
      linarith [mul_nonneg (Nat.cast_nonneg k) hs]) hs
  have C := log_nonpos hp.le hp'.le
  have D := log_nonpos (sub_nonneg.mpr hp'.le) (by linarith : 1 - p ≤ 1)
  have E := log_le_log (show 0 < 1 + s by linarith)
    (le_add_of_nonneg_right (mul_nonneg (Nat.cast_nonneg (k + l)) hs))
  linarith only [A, B, C, D, E]

private theorem obs_sum_le (i : Fin n) {p s : ℝ} (hp : 0 < p) (hp' : p < 1) (hs : 0 ≤ s) :
    (∑ j, observation (x j) (m - x j) p s) ≤ observation (x i) (m - x i) p s := by
  rw [← Finset.add_sum_erase _ _ (Finset.mem_univ i)]
  exact add_le_of_nonpos_right (Finset.sum_nonpos (fun j _ ↦ observation_le_zero _ _ hp hp' hs))

private theorem face_eq_at_shapes (hx : ∀ i, x i ≤ m) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    faceLogLik m x (a / (a + b)) (1 / (a + b)) = logLik m x a b := by
  have h := add_pos ha hb
  rw [face_eq_logLik hx (div_pos ha h) ((div_lt_one h).2 (by linarith)) (one_div_pos.mpr h)]
  congr 1
  · field_simp [h.ne']
  · field_simp [h.ne']
    ring

private theorem face_continuousOn (x : Fin n → ℕ) :
    ContinuousOn (fun q : ℝ × ℝ ↦ faceLogLik m x q.1 q.2) (Ioo 0 1 ×ˢ Ici 0) := by
  intro q hq
  have h₁ : 0 < q.1 := hq.1.1
  have h₂ : 0 < 1 - q.1 := sub_pos.mpr hq.1.2
  have h₃ : 0 ≤ q.2 := hq.2
  have H (c : ℝ × ℝ → ℝ) (hc : ContinuousAt c q) (hc' : 0 < c q) (k : ℕ) :
      ContinuousAt (fun r : ℝ × ℝ ↦ ∑ j ∈ Finset.range k, log (c r + (j : ℝ) * r.2)) q := by
    apply tendsto_finsetSum (Finset.range k)
    intro j _
    have H : ContinuousAt (fun r : ℝ × ℝ ↦ c r + (j : ℝ) * r.2) q :=
      hc.add (continuousAt_const.mul continuous_snd.continuousAt)
    have H' : c q + (j : ℝ) * q.2 ≠ 0 := by positivity
    exact (continuousAt_log H').comp (f := fun r : ℝ × ℝ ↦ c r + (j : ℝ) * r.2) H
  apply ContinuousAt.continuousWithinAt
  apply tendsto_finsetSum Finset.univ
  intro i _
  exact (((H Prod.fst continuous_fst.continuousAt h₁ (x i)).const_add _).add
    (H (fun r ↦ 1 - r.1) (continuousAt_const.sub continuous_fst.continuousAt) h₂ (m - x i))).sub
    (H (fun _ ↦ 1) continuousAt_const zero_lt_one m)

set_option maxHeartbeats 800000 in
-- The compact maximum requires elaboration in both coordinate systems.
private theorem compact_attainment (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hi : ∃ i, 0 < x i ∧ x i < m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1)
    {p s : ℝ} (hp : 0 < p) (hp' : p < 1) (hs : 0 < s)
    (hwin : binomialLogLik m x (proportion m x) < faceLogLik m x p s) :
    ∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q := by
  obtain ⟨i, hi₁, hi₂⟩ := hi
  have hil : 0 < m - x i := Nat.sub_pos_of_lt hi₂
  let C := chooseConstant m x
  let B := faceLogLik m x p s
  let e := exp (B - C - 1)
  let T := exp (C - B + 1)
  have he : 0 < e := exp_pos _
  have hT : 0 < T := exp_pos _
  let K := Icc e (1 - e) ×ˢ Icc 0 T
  have hbnd (r t : ℝ) (hr : 0 < r) (hr' : r < 1) (ht : 0 ≤ t)
      (h : B ≤ faceLogLik m x r t) : (r, t) ∈ K := by
    have H := obs_sum_le (x := x) (m := m) i hr hr' ht
    have A := observation_le_log_left (x i) (m - x i) hi₁ hr hr' ht
    have D := observation_le_log_left (m - x i) (x i) hil
      (sub_pos.mpr hr') (show 1 - r < 1 by linarith) ht
    rw [← observation_swap] at D
    have E := observation_tail (x i) (m - x i) hi₁ hil hr hr' ht
    rw [face_observations hx] at h
    change B ≤ C + _ at h
    have H₁ : B - C - 1 ≤ log r := by linarith only [h, H, A]
    have H₂ : B - C - 1 ≤ log (1 - r) := by linarith only [h, H, D]
    have H₃ : log (1 + t) ≤ C - B + 1 := by linarith only [h, H, E]
    have R₁ := exp_le_exp.mpr H₁
    have R₂ := exp_le_exp.mpr H₂
    have R₃ := exp_le_exp.mpr H₃
    rw [exp_log hr] at R₁
    rw [exp_log (sub_pos.mpr hr')] at R₂
    rw [exp_log (show 0 < 1 + t by linarith)] at R₃
    exact ⟨⟨R₁, by change r ≤ 1 - e; linarith only [show e ≤ 1 - r from R₂]⟩,
      ⟨ht, by change t ≤ T; linarith only [show 1 + t ≤ T from R₃]⟩⟩
  have hsub : K ⊆ Ioo 0 1 ×ˢ Ici 0 := by
    rintro ⟨r, t⟩ ⟨⟨hr, hr'⟩, ⟨ht, _⟩⟩
    exact ⟨⟨he.trans_le hr, lt_of_le_of_lt hr' (by linarith)⟩, ht⟩
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  have hpK := hbnd p s hp hp' hs.le le_rfl
  obtain ⟨⟨r, t⟩, hrt, hmax⟩ :=
    hK.exists_isMaxOn ⟨_, hpK⟩ ((face_continuousOn x).mono hsub)
  have hmax' (u v : ℝ) (huv : (u, v) ∈ K) :
      faceLogLik m x u v ≤ faceLogLik m x r t := hmax huv
  have hM : B ≤ faceLogLik m x r t := hmax' p s hpK
  have hr : 0 < r := (hsub hrt).1.1
  have hr' : r < 1 := (hsub hrt).1.2
  have ht : 0 < t := by
    have ht₀ : 0 ≤ t := (hsub hrt).2
    apply lt_of_le_of_ne ht₀
    intro ht
    have H : binomialLogLik m x r ≤ binomialLogLik m x (proportion m x) :=
      binomial_isMaxOn hm hn hx hu hu' (show r ∈ Ioo 0 1 from ⟨hr, hr'⟩)
    rw [← ht, face_zero] at hM
    exact (not_lt_of_ge (hM.trans H)) hwin
  refine ⟨(r / t, (1 - r) / t), ⟨div_pos hr ht, div_pos (sub_pos.mpr hr') ht⟩, ?_⟩
  rintro ⟨a, b⟩ ⟨ha, hb⟩
  change 0 < a at ha
  change 0 < b at hb
  change logLik m x a b ≤ logLik m x (r / t) ((1 - r) / t)
  rw [← face_eq_at_shapes hx ha hb, ← face_eq_logLik hx hr hr' ht]
  by_cases h : B ≤ faceLogLik m x (a / (a + b)) (1 / (a + b))
  · have hab := add_pos ha hb
    have hK' := hbnd (a / (a + b)) (1 / (a + b))
      (div_pos ha hab) ((div_lt_one hab).2 (by linarith)) (one_div_pos.mpr hab).le h
    exact hmax' (a / (a + b)) (1 / (a + b)) hK'
  · exact (lt_of_not_ge h).le.trans hM

private theorem proportion_interior (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hi : ∃ i, 0 < x i ∧ x i < m) : 0 < proportion m x ∧ proportion m x < 1 := by
  obtain ⟨i, hi₁, hi₂⟩ := hi
  have hM : (0 : ℝ) < m := by exact_mod_cast hm
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hS := countSum_eq hm hn x
  have h₁ : 0 < countSum x := by
    unfold countSum
    exact (show (0 : ℝ) < x i by exact_mod_cast hi₁).trans_le
      (Finset.single_le_sum (fun j _ ↦ Nat.cast_nonneg (x j)) (Finset.mem_univ i))
  have h₂ : 0 < (n : ℝ) * m - countSum x := by
    rw [← sum_reflected hx]
    exact (show (0 : ℝ) < (m - x i : ℕ) by exact_mod_cast Nat.sub_pos_of_lt hi₂).trans_le
      (Finset.single_le_sum (fun j _ ↦ Nat.cast_nonneg (m - x j)) (Finset.mem_univ i))
  rw [hS] at h₁ h₂
  exact ⟨pos_of_mul_pos_right h₁ (mul_pos hN hM).le,
    (mul_lt_mul_iff_right₀ (mul_pos hN hM)).1 (by linarith only [h₂])⟩

/-- With an interior count, strict overdispersion gives a finite global maximum. -/
theorem sufficiency (hm : 2 ≤ m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hi : ∃ i, 0 < x i ∧ x i < m)
    (hvar : (m : ℝ) * proportion m x * (1 - proportion m x) < variance x) :
    ∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q := by
  have hm₀ : 0 < m := by omega
  obtain ⟨hu, hu'⟩ := proportion_interior hm₀ hn hx hi
  obtain ⟨s, hs, hw⟩ := exists_above_binomial hm₀ hn hx hu hu' hvar
  rw [← face_eq_logLik hx hu hu' hs] at hw
  exact compact_attainment hm₀ hn hx hi hu hu' hu hu' hs hw

/-- For a sample with an interior observation, finite attainment is equivalent to overdispersion. -/
theorem exists_max_iff (hm : 2 ≤ m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hi : ∃ i, 0 < x i ∧ x i < m) :
    (∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q) ↔
      (m : ℝ) * proportion m x * (1 - proportion m x) < variance x := by
  refine ⟨?_, sufficiency hm hn hx hi⟩
  rintro ⟨⟨a, b⟩, ⟨ha, hb⟩, hmax⟩
  obtain ⟨hu, hu'⟩ := proportion_interior (show 0 < m by omega) hn hx hi
  have H := binomial_le_upperBound hx hu hu' (b := logLik m x a b) (by
    rintro _ ⟨q, hq, rfl⟩
    exact hmax hq)
  by_contra! h
  exact (not_le_of_gt (necessity hm hn hx hu hu' h ha hb)) H

private theorem likelihood_bddAbove (hx : ∀ i, x i ≤ m) :
    BddAbove ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) := by
  refine ⟨chooseConstant m x, ?_⟩
  rintro _ ⟨⟨a, b⟩, ⟨ha, hb⟩, rfl⟩
  change 0 < a at ha
  change 0 < b at hb
  change logLik m x a b ≤ chooseConstant m x
  have hab := add_pos ha hb
  rw [← face_eq_at_shapes hx ha hb, face_observations hx]
  exact add_le_of_nonpos_right (Finset.sum_nonpos fun i _ ↦ observation_le_zero _ _
    (div_pos ha hab) ((div_lt_one hab).2 (by linarith)) (one_div_pos.mpr hab).le)

/-- The best binomial value bounds the beta-binomial supremum from below. -/
theorem bestBinomial_le_sup (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1) :
    binomialLogLik m x (proportion m x) ≤
      sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) :=
  binomial_le_upperBound hx hu hu' (fun _ h ↦ le_csSup (likelihood_bddAbove hx) h)

private theorem endpoint_sup_aux (hm : 0 < m) (hn : 0 < n)
    (h : (∀ i, x i = 0) ∨ (∀ i, x i = m)) :
    sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) = 0 ∧
      ∀ q ∈ domain, logLik m x q.1 q.2 < 0 := by
  have hx : ∀ i, x i ≤ m := fun i ↦ h.elim (fun H ↦ H i ▸ Nat.zero_le m) (fun H ↦ (H i).le)
  have hC : chooseConstant m x = 0 := by
    rcases h with H | H <;> simp [chooseConstant, H]
  have hlt : ∀ q ∈ domain, logLik m x q.1 q.2 < 0 := by
    rintro ⟨a, b⟩ ⟨ha, hb⟩
    change 0 < a at ha
    change 0 < b at hb
    change logLik m x a b < 0
    have hab := add_pos ha hb
    let p := a / (a + b)
    let s := 1 / (a + b)
    have hp : 0 < p := div_pos ha hab
    have hp' : p < 1 := (div_lt_one hab).2 (by linarith)
    have hs : 0 ≤ s := (one_div_pos.mpr hab).le
    let i : Fin n := ⟨0, hn⟩
    rw [← face_eq_at_shapes hx ha hb, face_observations hx, hC, zero_add]
    refine (obs_sum_le i hp hp' hs).trans_lt ?_
    rcases h with H | H
    · rw [H i, Nat.sub_zero, observation_swap]
      exact (observation_le_log_left m 0 hm (sub_pos.mpr hp')
        (show 1 - p < 1 by linarith) hs).trans_lt
        (log_neg (sub_pos.mpr hp') (by linarith))
    · rw [H i, Nat.sub_self]
      exact (observation_le_log_left m 0 hm hp hp' hs).trans_lt (log_neg hp hp')
  refine ⟨le_antisymm (csSup_le (likelihood_nonempty m x) ?_) ?_, hlt⟩
  · rintro _ ⟨q, hq, rfl⟩
    exact (hlt q hq).le
  · have H (p : ℝ) (hp : 0 < p) (hp' : p < 1) :
        (n : ℝ) * m * log (1 - p) ≤
          sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) := by
      have B {r : ℝ} (hr : 0 < r) (hr' : r < 1) :=
        binomial_le_upperBound hx hr hr' (fun _ h ↦ le_csSup (likelihood_bddAbove hx) h)
      rcases h with H | H
      · simpa [binomialLogLik, H, mul_assoc] using B hp hp'
      · simpa [binomialLogLik, H, mul_assoc] using B (sub_pos.mpr hp') (show 1 - p < 1 by linarith)
    have hc : ContinuousAt (fun p : ℝ ↦ (n : ℝ) * m * log (1 - p)) 0 := by
      apply ContinuousAt.const_mul
      exact (continuousAt_log (by norm_num : (1 : ℝ) - 0 ≠ 0)).comp
        (continuousAt_const.sub continuousAt_id)
    have ht := hc.continuousWithinAt (s := Ioi 0) |>.tendsto
    have ht' : Tendsto (fun p : ℝ ↦ (n : ℝ) * m * log (1 - p)) (𝓝[>] 0) (𝓝 0) := by
      simpa using ht
    apply le_of_tendsto ht'
    have hsmall : ∀ᶠ p : ℝ in 𝓝[>] 0, p < 1 :=
      nhdsWithin_le_nhds (Iio_mem_nhds (zero_lt_one : (0 : ℝ) < 1))
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)), hsmall] with p hp hp'
    exact H p hp hp'

/-- An all-zero sample has log-likelihood supremum zero, strictly above every finite value. -/
theorem allZero_sup (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i = 0) :
    sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) = 0 ∧
      ∀ q ∈ domain, logLik m x q.1 q.2 < 0 :=
  endpoint_sup_aux hm hn (Or.inl hx)

/-- An all-size sample has log-likelihood supremum zero, strictly above every finite value. -/
theorem allSize_sup (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i = m) :
    sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) = 0 ∧
      ∀ q ∈ domain, logLik m x q.1 q.2 < 0 :=
  endpoint_sup_aux hm hn (Or.inr hx)

end

end UnivariateML.BetaBinomial
