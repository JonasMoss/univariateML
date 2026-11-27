module

public import UnivariateML.Family.TDist
public import UnivariateML.Family.StudentFaces
public import UnivariateML.Family.StudentCauchy
public import UnivariateML.Family.SkewLSGaussian
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.SpecialFunctions.ImproperIntegrals
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
public import Mathlib.MeasureTheory.Integral.DominatedConvergence

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Vertical collapse in Azzalini skew Student

The Student CDF is defined by normalizing the integral of the Student kernel.
The collapse-power limit uses continuity and positivity at finite arguments.

## Deviations

The unrestricted divergence theorem uses zero slant and applies to every
nonempty sample. The fixed-df collapse paths are vertical at the supplied
location. Moving-location collapse envelopes need additional estimates.
-/

@[expose] public section

open Set Filter Topology Real MeasureTheory

namespace UnivariateML.StudentSkew

noncomputable section

/-- The Student CDF as a normalized integral of its positive density kernel. -/
def studentCDF (ν z : ℝ) : ℝ :=
  (∫ y in Iic z, TDist.density 0 ν 1 y) /
    ∫ y, TDist.density 0 ν 1 y

private theorem student_continuous {ν : ℝ} (hν : 0 < ν) :
    Continuous (TDist.density 0 ν 1) := by
  unfold TDist.density
  apply continuous_exp.comp
  refine continuous_const.sub (continuous_const.mul (Continuous.log (by fun_prop) ?_))
  intro y
  positivity

private theorem student_even (ν y : ℝ) :
    TDist.density 0 ν 1 (-y) = TDist.density 0 ν 1 y := by simp [TDist.density]

private theorem student_positive (ν y : ℝ) : 0 < TDist.density 0 ν 1 y := exp_pos _

private theorem student_integrable {ν : ℝ} (hν : 0 < ν) :
    Integrable (TDist.density 0 ν 1) := by
  let C := exp (TDist.logNormalizer ν + (ν+1)/2*log ν)
  have ht : IntegrableOn (fun y : ℝ ↦ C * y^(-(ν+1))) (Ioi 1) :=
    (integrableOn_Ioi_rpow_of_lt (by linarith : -(ν+1) < -1) zero_lt_one).const_mul C
  have hr : IntegrableOn (TDist.density 0 ν 1) (Ioi 1) := by
    refine ht.mono' (student_continuous hν).aestronglyMeasurable ?_
    rw [ae_restrict_iff' measurableSet_Ioi]
    refine Eventually.of_forall fun y hy ↦ ?_
    have hy' : 0 < y := zero_lt_one.trans hy
    rw [Real.norm_eq_abs, abs_of_pos (student_positive ν y)]
    dsimp [C]
    rw [rpow_def_of_pos hy', ← exp_add]
    unfold TDist.density
    simp only [sub_zero, log_one, one_pow, mul_one, exp_le_exp]
    have hlog := log_le_log (show 0 < y^2/ν by positivity)
      (show y^2/ν ≤ 1+y^2/ν by linarith)
    rw [log_div (by positivity : y^2 ≠ 0) hν.ne', log_pow] at hlog
    push_cast at hlog
    have h := mul_le_mul_of_nonneg_left hlog (by positivity : 0 ≤ (ν+1)/2)
    nlinarith only [h]
  have hl : IntegrableOn (TDist.density 0 ν 1) (Iio (-1)) := by
    have H := ((Measure.measurePreserving_neg (volume : Measure ℝ)).integrableOn_comp_preimage
      (Homeomorph.neg ℝ).measurableEmbedding).2 hr
    simpa [Function.comp_def, student_even, neg_preimage, neg_Ioi] using H
  have hc : IntegrableOn (TDist.density 0 ν 1) (Icc (-1) 1) :=
    (student_continuous hν).continuousOn.integrableOn_compact
    (isCompact_Icc (a := (-1 : ℝ)) (b := 1))
  have hUnion : Iio (-1 : ℝ) ∪ Icc (-1) 1 ∪ Ioi 1 = univ := by
    ext x
    simp only [mem_union, mem_Iio, mem_Icc, mem_Ioi, mem_univ, iff_true]
    by_cases h : x < -1
    · exact Or.inl (Or.inl h)
    · by_cases h' : x ≤ 1
      · exact Or.inl (Or.inr ⟨le_of_not_gt h,h'⟩)
      · exact Or.inr (lt_of_not_ge h')
  have H := (hl.union hc).union hr
  rw [hUnion, integrableOn_univ] at H
  exact H

private theorem mass_positive {ν : ℝ} (hν : 0 < ν) :
    0 < ∫ y, TDist.density 0 ν 1 y :=
  integral_pos_of_integrable_nonneg_ne_zero (student_continuous hν) (student_integrable hν)
    (fun y ↦ (student_positive ν y).le) (student_positive ν 0).ne'

private theorem cdf_hasDerivAt {ν : ℝ} (hν : 0 < ν) (z : ℝ) :
    HasDerivAt (studentCDF ν) (TDist.density 0 ν 1 z / ∫ y, TDist.density 0 ν 1 y) z := by
  have hc := student_continuous hν
  have hi := student_integrable hν
  have h := intervalIntegral.integral_hasDerivAt_right (hc.intervalIntegrable 0 z)
    hc.aestronglyMeasurable.stronglyMeasurableAtFilter hc.continuousAt
  have he (t : ℝ) : studentCDF ν t =
      ((∫ y in (0 : ℝ)..t, TDist.density 0 ν 1 y) +
        ∫ y in Iic (0 : ℝ), TDist.density 0 ν 1 y) / ∫ y, TDist.density 0 ν 1 y := by
    unfold studentCDF
    rw [← intervalIntegral.integral_Iic_sub_Iic hi.integrableOn hi.integrableOn]
    ring
  convert (h.add_const (∫ y in Iic (0 : ℝ), TDist.density 0 ν 1 y)).div_const
    (∫ y, TDist.density 0 ν 1 y) using 1
  exact funext he

private theorem cdf_continuous {ν : ℝ} (hν : 0 < ν) : Continuous (studentCDF ν) :=
  continuous_iff_continuousAt.mpr (fun z ↦ (cdf_hasDerivAt hν z).continuousAt)

private theorem cdf_strictMono {ν : ℝ} (hν : 0 < ν) : StrictMono (studentCDF ν) := by
  apply strictMono_of_deriv_pos
  intro z
  rw [(cdf_hasDerivAt hν z).deriv]
  exact div_pos (student_positive ν z) (mass_positive hν)

private theorem cdf_pos {ν : ℝ} (hν : 0 < ν) (z : ℝ) : 0 < studentCDF ν z := by
  have hnonneg (t : ℝ) : 0 ≤ studentCDF ν t :=
    div_nonneg (integral_nonneg (fun y ↦ (student_positive ν y).le)) (mass_positive hν).le
  exact (hnonneg (z-1)).trans_lt (cdf_strictMono hν (by linarith : z-1 < z))

private theorem cdf_reflect {ν : ℝ} (hν : 0 < ν) (z : ℝ) :
    studentCDF ν (-z) = 1 - studentCDF ν z := by
  -- Reflect the lower half-line directly to the complementary upper half-line.
  have he := integral_comp_neg_Iic (-z) (TDist.density 0 ν 1)
  simp_rw [student_even, neg_neg] at he
  have h' := integral_add_compl (s := Iic z) measurableSet_Iic (student_integrable hν)
  rw [compl_Iic, ← he] at h'
  unfold studentCDF
  apply (div_eq_iff (mass_positive hν).ne').mpr
  calc
    _ = (∫ y, TDist.density 0 ν 1 y) - ∫ y in Iic z, TDist.density 0 ν 1 y := by linarith
    _ = _ := by field_simp [(mass_positive hν).ne']

private theorem cdf_zero {ν : ℝ} (hν : 0 < ν) : studentCDF ν 0 = 1/2 := by
  have h := cdf_reflect hν 0
  rw [neg_zero] at h
  linarith

/-- Skew Student likelihood with ordinary scale. The CDF argument is
written after cancelling a positive ordinary scale. -/
def likelihood {n : ℕ} (x : Fin n → ℝ) (μ ν α a : ℝ) : ℝ :=
  TDist.likelihood x μ ν a *
    ∏ i, 2 * studentCDF (ν+1)
      (α * (x i-μ) * sqrt ((ν+1)/(ν*a^2+(x i-μ)^2)))

/-- Positive-scale collapse trace after removing its scale power. -/
def collapseTrace {n : ℕ} (x : Fin n → ℝ) (μ ν α : ℝ) : ℝ :=
  exp (TDist.collapseLogValue x μ ν) *
    ∏ i, 2 * studentCDF (ν+1)
      (α * (x i-μ) * sqrt ((ν+1)/(x i-μ)^2))

private theorem zero_slant {n : ℕ} (x : Fin n → ℝ) (μ a : ℝ) {ν : ℝ} (hν : 0 < ν) :
    likelihood x μ ν 0 a = TDist.likelihood x μ ν a := by
  simp [likelihood, cdf_zero (by positivity : 0 < ν+1)]

private theorem argument_limit {ν : ℝ} (α r : ℝ) :
    Tendsto (fun a : ℝ ↦ α*r*sqrt ((ν+1)/(ν*a^2+r^2))) (𝓝[>] 0)
      (𝓝 (α*r*sqrt ((ν+1)/r^2))) := by
  by_cases hr : r = 0
  · simp [hr]
  · have hc : ContinuousAt (fun a : ℝ ↦ ν*a^2+r^2) 0 := by fun_prop
    have h := ((continuousAt_const (y := ν+1)).div hc
      (by simpa using pow_ne_zero 2 hr)).sqrt.const_mul (α*r)
    simpa using h.tendsto.mono_left nhdsWithin_le_nhds

/-- Along a vertical atom path at held positive df and finite slant, the
likelihood divided by the collapse power tends to the explicit CDF product.
Zero residuals contribute the contact factor one. -/
theorem skew_collapse_power {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) :
    Tendsto (fun a ↦ likelihood x μ ν α a / a ^ TDist.exponent x μ ν) (𝓝[>] 0)
      (𝓝 (collapseTrace x μ ν α)) := by
  have hprod := tendsto_finsetProd Finset.univ fun i _ ↦
    (((cdf_continuous (by positivity : 0 < ν+1)).continuousAt.tendsto).comp
      (argument_limit (ν := ν) α (x i-μ))).const_mul 2
  refine ((TDist.collapse_power x μ hν).1.mul hprod).congr ?_
  intro a
  simp only [likelihood, Function.comp_apply]
  rw [mul_div_right_comm]

/-- For any held finite slant and positive df, a negative exponent gives
an infinite likelihood along the vertical atom-collapse path. -/
theorem skew_negative_path {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν < 0) :
    Tendsto (likelihood x μ ν α) (𝓝[>] 0) atTop := by
  have hp : 0 < collapseTrace x μ ν α := by
    unfold collapseTrace
    exact mul_pos (exp_pos _) (Finset.prod_pos fun _ _ ↦ mul_pos two_pos
      (cdf_pos (by positivity) _))
  have ht := (skew_collapse_power x μ α hν).pos_mul_atTop hp (tendsto_rpow_neg_nhdsGT_zero he)
  refine ht.congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with a ha
  have ha' : 0 < a := ha
  exact div_mul_cancel₀ _ (rpow_pos_of_pos ha' _).ne'

/-- At critical exponent the vertical skew Student likelihood tends to the
collapse trace. The theorem holds for all finite slants. -/
theorem skew_critical_path {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν = 0) :
    Tendsto (likelihood x μ ν α) (𝓝[>] 0) (𝓝 (collapseTrace x μ ν α)) ∧
      0 < collapseTrace x μ ν α := by
  refine ⟨?_, ?_⟩
  · simpa [he] using skew_collapse_power x μ α hν
  · unfold collapseTrace
    exact mul_pos (exp_pos _) (Finset.prod_pos fun _ _ ↦ mul_pos two_pos
      (cdf_pos (by positivity) _))

/-- At zero slant, a negative exponent forces scale-collapse divergence
at any fixed positive df. -/
theorem symmetric_collapse_unbounded {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν < 0) :
    Tendsto (likelihood x μ ν 0) (𝓝[>] 0) atTop := by
  change Tendsto (fun a ↦ likelihood x μ ν 0 a) (𝓝[>] 0) atTop
  simpa only [zero_slant x μ _ hν] using TDist.collapse_unbounded x μ hν he

/-- The unrestricted positive-df skew Student likelihood is unbounded for
every nonempty sample. A vertical zero-slant path through any observation
works at a sufficiently small held df. -/
theorem unrestricted_unbounded {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    ¬ BddAbove ((fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.1 p.2.2.1 p.2.2.2) ''
      {p | 0 < p.2.1 ∧ 0 < p.2.2.2}) := by
  let i : Fin n := ⟨0, hn⟩
  let k : ℝ := TDist.multiplicity x (x i)
  have hk : 1 ≤ k := by
    have hk' : 0 < TDist.multiplicity x (x i) := by
      unfold TDist.multiplicity
      exact Finset.card_pos.mpr ⟨i, by simp⟩
    dsimp [k]
    exact_mod_cast hk'
  let ν : ℝ := 1/(n+1)
  have hν : 0 < ν := by dsimp [ν]; positivity
  have he : TDist.exponent x (x i) ν < 0 := by
    change ν*((n : ℝ)-k)-k < 0
    have hv : ν*((n : ℝ)+1) = 1 := by
      dsimp [ν]
      exact div_mul_cancel₀ _ (by positivity)
    have h := mul_lt_mul_of_pos_left (show (n : ℝ)-k < n+1 by linarith) hν
    linarith
  rintro ⟨b,hb⟩
  have hpos : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
  obtain ⟨a,ha,hgt⟩ := (hpos.and
    ((symmetric_collapse_unbounded x (x i) hν he).eventually_gt_atTop b)).exists
  exact hgt.not_ge (hb ⟨(x i,ν,0,a),⟨hν,ha⟩,rfl⟩)

private theorem cdf_atBot (ν : ℝ) : Tendsto (studentCDF ν) atBot (𝓝 0) := by
  have H := (tendsto_integral_Iic_zero (f := TDist.density 0 ν 1) (μ := volume)
    (a := id) tendsto_id).div_const (∫ y, TDist.density 0 ν 1 y)
  change Tendsto (fun z ↦ studentCDF ν z) atBot (𝓝 0)
  simpa [studentCDF] using H

private theorem cdf_atTop {ν : ℝ} (hν : 0 < ν) : Tendsto (studentCDF ν) atTop (𝓝 1) := by
  have H := ((cdf_atBot ν).comp tendsto_neg_atTop_atBot).const_sub 1
  simpa [Function.comp_def, cdf_reflect hν] using H

private theorem cdf_linear_paths {ν : ℝ} (hν : 0 < ν) (c : ℝ) :
    Tendsto (fun α ↦ 2 * studentCDF ν (α*c)) atTop
      (𝓝 (if 0 < c then 2 else if c = 0 then 1 else 0)) ∧
    Tendsto (fun α ↦ 2 * studentCDF ν (α*c)) atBot
      (𝓝 (if c < 0 then 2 else if c = 0 then 1 else 0)) := by
  rcases lt_trichotomy c 0 with h | rfl | h
  · have htop := ((cdf_atBot ν).comp (tendsto_id.atTop_mul_const_of_neg h)).const_mul 2
    have hbot := ((cdf_atTop hν).comp (tendsto_id.atBot_mul_const_of_neg h)).const_mul 2
    exact ⟨by simpa [Function.comp_def, h.ne, not_lt_of_ge h.le] using htop,
      by simpa [Function.comp_def, h] using hbot⟩
  · simp [cdf_zero hν]
  · have htop := ((cdf_atTop hν).comp (tendsto_id.atTop_mul_const h)).const_mul 2
    have hbot := ((cdf_atBot ν).comp (tendsto_id.atBot_mul_const h)).const_mul 2
    exact ⟨by simpa [Function.comp_def, h] using htop,
      by simpa [Function.comp_def, h.ne', not_lt_of_ge h.le] using hbot⟩

/-- At fixed location, positive scale, and positive df, the two slant
limits are the corresponding oriented half-t products. An observation at
the fixed location has factor one in either limit. -/
theorem skew_slant_paths {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    Tendsto (fun α ↦ likelihood x μ ν α a) atTop
      (𝓝 (TDist.likelihood x μ ν a *
        ∏ i, if μ < x i then 2 else if x i = μ then 1 else 0)) ∧
    Tendsto (fun α ↦ likelihood x μ ν α a) atBot
      (𝓝 (TDist.likelihood x μ ν a *
        ∏ i, if x i < μ then 2 else if x i = μ then 1 else 0)) := by
  let b (i : Fin n) := sqrt ((ν+1)/(ν*a^2+(x i-μ)^2))
  have hb (i : Fin n) : 0 < b i := by dsimp [b]; positivity
  have hpos (i : Fin n) : 0 < (x i-μ)*b i ↔ μ < x i := by
    exact (mul_pos_iff_of_pos_right (hb i)).trans sub_pos
  have hneg (i : Fin n) : (x i-μ)*b i < 0 ↔ x i < μ := by
    simpa only [zero_mul, sub_neg] using
      (show (x i-μ)*b i < 0*b i ↔ x i-μ < 0 from mul_lt_mul_iff_left₀ (hb i))
  have heq (i : Fin n) : (x i-μ)*b i = 0 ↔ x i = μ := by
    simp [(hb i).ne', sub_eq_zero]
  have H (i : Fin n) := cdf_linear_paths (by positivity : 0 < ν+1) ((x i-μ)*b i)
  have htop := (tendsto_finsetProd Finset.univ fun i _ ↦ (H i).1).const_mul
    (TDist.likelihood x μ ν a)
  have hbot := (tendsto_finsetProd Finset.univ fun i _ ↦ (H i).2).const_mul
    (TDist.likelihood x μ ν a)
  exact ⟨by simpa only [likelihood, b, mul_assoc, hpos, heq] using htop,
    by simpa only [likelihood, b, mul_assoc, hneg, heq] using hbot⟩

private theorem standard_normal_eq (y : ℝ) :
    TDist.normalDensity 0 1 y = SkewLSGaussian.φ y := by
  have h : exp (log (2*π)/2) = sqrt (2*π) := by
    rw [sqrt_eq_rpow, rpow_def_of_pos (by positivity)]
    congr 1
    ring
  rw [TDist.normalDensity, SkewLSGaussian.phi_formula]
  simp only [log_one, sub_zero, one_pow, div_one]
  rw [show -(log (2*π)/2)-y^2/2 = -(log (2*π)/2)+(-(y^2/2)) by ring,
    exp_add, exp_neg, h, one_div]

private theorem standard_density_normal (y : ℝ) :
    Tendsto (fun ν ↦ TDist.density 0 ν 1 y) atTop (𝓝 (SkewLSGaussian.φ y)) := by
  rw [← standard_normal_eq]
  exact TDist.density_tendsto_normal 0 y one_pos tendsto_const_nhds

private theorem kernel_dominates {ν : ℝ} (hν : 2 ≤ ν) (y : ℝ) :
    log (1+y^2/2) ≤ (ν+1)/2 * log (1+y^2/ν) := by
  have hv : 0 < ν := by linarith
  let u := 2/ν
  have hu : 0 ≤ u := by dsimp [u]; positivity
  have hu' : u ≤ 1 := (div_le_one hv).mpr hν
  have H := strictConcaveOn_log_Ioi.concaveOn.2
    (show 1+y^2/2 ∈ Ioi (0 : ℝ) from show 0 < 1+y^2/2 by positivity)
    (show 1 ∈ Ioi (0 : ℝ) from one_pos)
    hu (sub_nonneg.mpr hu') (show u+(1-u)=1 by ring)
  simp only [smul_eq_mul, log_one, mul_zero, add_zero, mul_one] at H
  have harg : u*(1+y^2/2)+(1-u) = 1+y^2/ν := by dsimp [u]; ring
  rw [harg] at H
  have H' := mul_le_mul_of_nonneg_left H (show 0 ≤ ν/2 by positivity)
  have hP : ν/2*u = 1 := by dsimp [u]; field_simp
  rw [← mul_assoc, hP, one_mul] at H'
  exact H'.trans (mul_le_mul_of_nonneg_right (by linarith)
    (log_nonneg (le_add_of_nonneg_right (by positivity))))

private theorem integral_limit (μ : Measure ℝ) (hμ : μ ≤ volume) :
    Tendsto (fun ν ↦ ∫ y, TDist.density 0 ν 1 y ∂μ) atTop
      (𝓝 (∫ y, SkewLSGaussian.φ y ∂μ)) := by
  let C := SkewLSGaussian.φ 0 + 1
  have hC : 0 < C := by dsimp [C]; linarith [SkewLSGaussian.phi_pos 0]
  let B := fun y : ℝ ↦ C * (1+y^2/2)⁻¹
  have hb : Integrable B := by
    have h := (integrable_inv_one_add_mul_sq
      (b := sqrt (1/2)) (sqrt_pos.mpr (by norm_num : 0 < (1 : ℝ)/2)).ne').const_mul C
    refine h.congr (Eventually.of_forall fun y ↦ ?_)
    simp [B, mul_pow, div_eq_mul_inv, mul_comm]
  apply tendsto_integral_filter_of_dominated_convergence B
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with ν hν
    exact (student_continuous hν).aestronglyMeasurable
  · have hmode := (standard_density_normal 0).eventually
      (Iio_mem_nhds (show SkewLSGaussian.φ 0 < C by dsimp [C]; linarith))
    filter_upwards [hmode,eventually_ge_atTop (2 : ℝ)] with ν hv hν
    refine Eventually.of_forall fun y ↦ ?_
    have h := exp_le_exp.mpr (neg_le_neg (kernel_dominates hν y))
    rw [exp_neg, exp_neg, exp_log (by positivity)] at h
    rw [Real.norm_eq_abs, abs_of_pos (student_positive ν y)]
    have he : TDist.density 0 ν 1 y = TDist.density 0 ν 1 0 *
        exp (-((ν+1)/2 * log (1+y^2/ν))) := by
      simp [TDist.density, sub_eq_add_neg, exp_add]
    rw [he]
    exact mul_le_mul hv.le (by simpa only [exp_neg] using h) (exp_pos _).le hC.le
  · exact hb.mono_measure hμ
  · exact Eventually.of_forall standard_density_normal

private theorem cdf_normal_fixed (z : ℝ) :
    Tendsto (fun ν ↦ studentCDF ν z) atTop (𝓝 (SkewLSGaussian.Φ z)) := by
  have hA := integral_limit (volume.restrict (Iic z)) Measure.restrict_le_self
  have hB := integral_limit volume le_rfl
  have hMass : (∫ y, SkewLSGaussian.φ y) = 1 :=
    ProbabilityTheory.integral_gaussianPDFReal_eq_one 0 one_ne_zero
  rw [hMass] at hB
  rw [SkewLSGaussian.cdf_integral]
  have h := hA.div hB one_ne_zero
  change Tendsto (fun ν ↦ (∫ y in Iic z, TDist.density 0 ν 1 y) /
    (∫ y, TDist.density 0 ν 1 y)) atTop _ at h
  simpa only [studentCDF, div_one] using h

private theorem cdf_normal_moving {ι : Type*} {l : Filter ι} {ν z : ι → ℝ} {u : ℝ}
    (hν : Tendsto ν l atTop) (hz : Tendsto z l (𝓝 u)) :
    Tendsto (fun j ↦ studentCDF (ν j) (z j)) l (𝓝 (SkewLSGaussian.Φ u)) := by
  have hp := hν.eventually_gt_atTop (0 : ℝ)
  refine tendsto_order.mpr ⟨?_, ?_⟩
  · intro a ha
    have h : ∀ᶠ w : ℝ in 𝓝[<] u, a < SkewLSGaussian.Φ w :=
      ((SkewLSGaussian.cdf_continuous.continuousAt (x := u)).tendsto.mono_left
        nhdsWithin_le_nhds).eventually (Ioi_mem_nhds ha)
    have hdom : ∀ᶠ w : ℝ in 𝓝[<] u, w < u := self_mem_nhdsWithin
    obtain ⟨w,hw,hwa⟩ := (hdom.and h).exists
    filter_upwards [hp, hz.eventually (Ioi_mem_nhds hw),
      ((cdf_normal_fixed w).comp hν).eventually (Ioi_mem_nhds hwa)] with j hj hjw hja
    exact hja.trans (cdf_strictMono hj hjw)
  · intro a ha
    have h : ∀ᶠ w : ℝ in 𝓝[>] u, SkewLSGaussian.Φ w < a :=
      ((SkewLSGaussian.cdf_continuous.continuousAt (x := u)).tendsto.mono_left
        nhdsWithin_le_nhds).eventually (Iio_mem_nhds ha)
    have hdom : ∀ᶠ w : ℝ in 𝓝[>] u, u < w := self_mem_nhdsWithin
    obtain ⟨w,hw,hwa⟩ := (hdom.and h).exists
    filter_upwards [hp, hz.eventually (Iio_mem_nhds hw),
      ((cdf_normal_fixed w).comp hν).eventually (Iio_mem_nhds hwa)] with j hj hjw hja
    exact (cdf_strictMono hj hjw).trans hja

private theorem cdf_normal_right {ι : Type*} {l : Filter ι} {ν z : ι → ℝ}
    (hν : Tendsto ν l atTop) (hz : Tendsto z l atTop) :
    Tendsto (fun j ↦ studentCDF (ν j) (z j)) l (𝓝 1) := by
  have hp := hν.eventually_gt_atTop (0 : ℝ)
  refine tendsto_order.mpr ⟨?_, ?_⟩
  · intro a ha
    obtain ⟨w,hw⟩ := (SkewLSGaussian.cdf_atTop.eventually (Ioi_mem_nhds ha)).exists
    filter_upwards [hp, hz.eventually_gt_atTop w,
      ((cdf_normal_fixed w).comp hν).eventually (Ioi_mem_nhds hw)] with j hj hjw hja
    exact hja.trans (cdf_strictMono hj hjw)
  · intro a ha
    filter_upwards [hp] with j hj
    have H := cdf_pos hj (-z j)
    rw [cdf_reflect hj] at H
    linarith

private theorem cdf_normal_left {ι : Type*} {l : Filter ι} {ν z : ι → ℝ}
    (hν : Tendsto ν l atTop) (hz : Tendsto z l atBot) :
    Tendsto (fun j ↦ studentCDF (ν j) (z j)) l (𝓝 0) := by
  have hp := hν.eventually_gt_atTop (0 : ℝ)
  have h := (cdf_normal_right hν (tendsto_neg_atBot_atTop.comp hz)).const_sub 1
  rw [sub_self] at h
  refine h.congr' ?_
  filter_upwards [hp] with j hj
  simp [cdf_reflect hj, sub_sub_cancel]

private theorem shape_argument_limit {a : ℝ} (ha : 0 < a) (r : ℝ) :
    Tendsto (fun ν ↦ r * sqrt ((ν+1)/(ν*a^2+r^2))) atTop (𝓝 (r/a)) := by
  have h1 := (tendsto_const_nhds (x := (1 : ℝ))).div_atTop tendsto_id
  have h2 := (tendsto_const_nhds (x := r^2)).div_atTop tendsto_id
  have h := (((h1.const_add 1).div (h2.const_add (a^2))
    (by positivity : a^2 + 0 ≠ 0)).sqrt).const_mul r
  simp only [add_zero, one_div, sqrt_inv, sqrt_sq ha.le, ← div_eq_mul_inv] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with ν hν
  have hr : (1+1/ν)/(a^2+r^2/ν) = (ν+1)/(ν*a^2+r^2) := by
    field_simp [hν.ne', (show 0 < ν*a^2+r^2 by positivity).ne',
      (show 0 < a^2+r^2/ν by positivity).ne']
  rw [← hr]
  simp [div_eq_mul_inv]

private theorem base_normal {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun ν ↦ TDist.likelihood x μ ν a) atTop
      (𝓝 (∏ i, TDist.normalDensity μ a (x i))) := by
  refine (tendsto_finsetProd Finset.univ fun i _ ↦
    TDist.density_tendsto_normal μ (x i) ha tendsto_const_nhds).congr (fun ν ↦ ?_)
  rw [TDist.likelihood, TDist.logLik, exp_sum]
  rfl

/-- At fixed finite slant, location and positive scale, the infinite-df
likelihood is the corresponding skew-normal product. -/
theorem skew_normal_path {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun ν ↦ likelihood x μ ν α a) atTop
      (𝓝 ((∏ i, TDist.normalDensity μ a (x i)) *
        ∏ i, 2*SkewLSGaussian.Φ (α*(x i-μ)/a))) := by
  have h (i : Fin n) :
      Tendsto (fun ν ↦ 2*studentCDF (ν+1)
        (α*(x i-μ)*sqrt ((ν+1)/(ν*a^2+(x i-μ)^2)))) atTop
        (𝓝 (2*SkewLSGaussian.Φ (α*(x i-μ)/a))) := by
    have H := (cdf_normal_moving (tendsto_atTop_add_const_right _ (1 : ℝ) tendsto_id)
      ((shape_argument_limit ha (x i-μ)).const_mul α)).const_mul (2 : ℝ)
    simpa [mul_assoc, mul_div_assoc] using H
  exact (base_normal x μ ha).mul (tendsto_finsetProd Finset.univ fun i _ ↦ h i)

/-- Along a joint infinite-df and infinite-slant path at fixed location
and positive scale, the two orientations tend to half-normal likelihood
products, with contact factor one at residual zero. -/
theorem skew_joint_infinite_paths {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {a : ℝ} (ha : 0 < a)
    {ι : Type*} {l : Filter ι} {ν α : ι → ℝ} (hν : Tendsto ν l atTop) :
    (Tendsto α l atTop →
      Tendsto (fun j ↦ likelihood x μ (ν j) (α j) a) l
        (𝓝 ((∏ i, TDist.normalDensity μ a (x i)) *
          ∏ i, if μ < x i then 2 else if x i = μ then 1 else 0))) ∧
    (Tendsto α l atBot →
      Tendsto (fun j ↦ likelihood x μ (ν j) (α j) a) l
        (𝓝 ((∏ i, TDist.normalDensity μ a (x i)) *
          ∏ i, if x i < μ then 2 else if x i = μ then 1 else 0))) := by
  have hu : Tendsto (fun j ↦ ν j+1) l atTop := tendsto_atTop_add_const_right _ _ hν
  have hp := hu.eventually_gt_atTop (0 : ℝ)
  let k (i : Fin n) (j : ι) := (x i-μ)*sqrt ((ν j+1)/(ν j*a^2+(x i-μ)^2))
  have hk (i : Fin n) : Tendsto (k i) l (𝓝 ((x i-μ)/a)) :=
    (shape_argument_limit ha (x i-μ)).comp hν
  let G (i : Fin n) (j : ι) := 2*studentCDF (ν j+1) (α j*k i j)
  have hcontact (i : Fin n) (hi : x i = μ) : Tendsto (G i) l (𝓝 1) := by
    refine tendsto_const_nhds.congr' ?_
    filter_upwards [hp] with j hj
    simp [G, k, hi, cdf_zero hj]
  have hbase := (base_normal x μ ha).comp hν
  constructor
  · intro hα
    have H (i : Fin n) : Tendsto (G i) l
        (𝓝 (if μ < x i then 2 else if x i = μ then 1 else 0)) := by
      rcases lt_trichotomy (x i) μ with hi | hi | hi
      · have h := (hk i).neg_mul_atTop (div_neg_of_neg_of_pos (sub_neg.mpr hi) ha) hα
        simpa [G, mul_comm, not_lt_of_ge hi.le, hi.ne] using (cdf_normal_left hu h).const_mul 2
      · simpa [hi] using hcontact i hi
      · have h := (hk i).pos_mul_atTop (div_pos (sub_pos.mpr hi) ha) hα
        simpa [G, mul_comm, hi] using (cdf_normal_right hu h).const_mul 2
    simpa only [likelihood, G, k, mul_assoc, Function.comp_def] using
      hbase.mul (tendsto_finsetProd Finset.univ fun i _ ↦ H i)
  · intro hα
    have H (i : Fin n) : Tendsto (G i) l
        (𝓝 (if x i < μ then 2 else if x i = μ then 1 else 0)) := by
      rcases lt_trichotomy (x i) μ with hi | hi | hi
      · have h := (hk i).neg_mul_atBot (div_neg_of_neg_of_pos (sub_neg.mpr hi) ha) hα
        simpa [G, mul_comm, hi] using (cdf_normal_right hu h).const_mul 2
      · simpa [hi] using hcontact i hi
      · have h := (hk i).pos_mul_atBot (div_pos (sub_pos.mpr hi) ha) hα
        simpa [G, mul_comm, not_lt_of_ge hi.le, hi.ne'] using (cdf_normal_left hu h).const_mul 2
    simpa only [likelihood, G, k, mul_assoc, Function.comp_def] using
      hbase.mul (tendsto_finsetProd Finset.univ fun i _ ↦ H i)

private theorem likelihood_positive {n : ℕ} (x : Fin n → ℝ) (μ α a : ℝ) {ν : ℝ}
    (hν : 0 < ν) : 0 < likelihood x μ ν α a := by
  unfold likelihood TDist.likelihood
  exact mul_pos (exp_pos _) (Finset.prod_pos fun _ _ ↦ mul_pos two_pos
    (cdf_pos (by positivity) _))

private theorem likelihood_le_base {n : ℕ} (x : Fin n → ℝ) (μ α a : ℝ) {ν : ℝ}
    (hν : 0 < ν) : likelihood x μ ν α a ≤ (2 : ℝ)^n * TDist.likelihood x μ ν a := by
  unfold likelihood
  rw [mul_comm ((2 : ℝ)^n)]
  apply mul_le_mul_of_nonneg_left _ (exp_pos _).le
  rw [show (2 : ℝ)^n = ∏ _i : Fin n, (2 : ℝ) by simp]
  apply Finset.prod_le_prod₀
  · exact fun i _ ↦ (mul_pos two_pos (cdf_pos (by positivity : 0 < ν+1) _)).le
  · intro i _
    have H (t : ℝ) : studentCDF (ν+1) t < 1 := by
      have h := cdf_pos (by positivity : 0 < ν+1) (-t)
      rw [cdf_reflect (by positivity)] at h
      linarith
    exact mul_le_of_le_one_right two_pos.le (H _).le

private theorem scale_continuousOn {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) : ContinuousOn (likelihood x μ ν α) (Ioi 0) := by
  unfold likelihood
  apply ContinuousOn.mul
  · unfold TDist.likelihood TDist.logLik
    apply continuous_exp.comp_continuousOn
    apply continuousOn_finsetSum
    intro i _
    refine (continuousOn_const.sub (continuousOn_id.log (fun a ha ↦
      (show 0 < a from ha).ne'))).sub (continuousOn_const.mul (ContinuousOn.log ?_ ?_))
    · exact continuousOn_const.add (continuousOn_const.div (by fun_prop)
        (fun a ha ↦ by have h : 0 < a := ha; positivity))
    · intro a ha
      have h : 0 < a := ha
      positivity
  · apply continuousOn_finsetProd
    intro i _
    apply continuousOn_const.mul
    apply (cdf_continuous (by positivity : 0 < ν+1)).comp_continuousOn
    apply continuousOn_const.mul
    exact continuous_sqrt.comp_continuousOn (continuousOn_const.div (by fun_prop)
      (fun a ha ↦ by have h : 0 < a := ha; positivity))

private theorem vertical_small {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : 0 < TDist.exponent x μ ν) :
    Tendsto (likelihood x μ ν α) (𝓝[>] 0) (𝓝 0) := by
  have hp := (continuousAt_rpow_const 0 (TDist.exponent x μ ν) (.inr he.le)).tendsto
  have hpow : Tendsto (fun a : ℝ ↦ a ^ TDist.exponent x μ ν) (𝓝[>] 0) (𝓝 0) := by
    simpa [zero_rpow he.ne'] using hp.mono_left nhdsWithin_le_nhds
  have H := (skew_collapse_power x μ α hν).mul hpow
  rw [mul_zero] at H
  refine H.congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with a ha
  exact div_mul_cancel₀ _ (rpow_pos_of_pos ha _).ne'

private theorem vertical_large {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) : Tendsto (likelihood x μ ν α) atTop (𝓝 0) := by
  have h := StudentFaces.lst_fixed_df_remote_ends x hn hν
    (m := fun _ : ℝ ↦ μ) (s := id) (eventually_gt_atTop (0 : ℝ)) (Or.inl tendsto_id)
  have ht := h.const_mul ((2 : ℝ)^n)
  rw [mul_zero] at ht
  exact squeeze_zero' (Eventually.of_forall fun a ↦ (likelihood_positive x μ α a hν).le)
    (Eventually.of_forall fun a ↦ likelihood_le_base x μ α a hν) ht

private theorem scale_above_faces {f : ℝ → ℝ} {C : ℝ}
    (hc : ContinuousOn f (Ioi 0)) (h0 : Tendsto f (𝓝[>] 0) (𝓝 C))
    (hlarge : Tendsto f atTop (𝓝 0)) {a : ℝ} (ha : 0 < a) (hf : 0 < f a) (hC : C ≤ f a) :
    ∃ s ∈ Ioi 0, IsMaxOn f (Ioi 0) s := by
  by_cases hm : IsMaxOn f (Ioi 0) a
  · exact ⟨a,ha,hm⟩
  rw [isMaxOn_iff] at hm
  push Not at hm
  obtain ⟨b,hb,hb'⟩ := hm
  have hzb : 0 < b := hb
  have hf' : 0 < f b := hf.trans hb'
  obtain ⟨ε,hε,hsmall⟩ := Metric.mem_nhdsWithin_iff.mp
    (h0.eventually (Iio_mem_nhds (hC.trans_lt hb')))
  obtain ⟨B,hB⟩ := eventually_atTop.mp (hlarge.eventually (Iio_mem_nhds hf'))
  let u := max B b
  let l := min b ε / 2
  have hl : 0 < l := half_pos (lt_min hzb hε)
  have hlb : l ≤ b := by dsimp [l]; linarith [min_le_left b ε]
  have hlε : l < ε := by dsimp [l]; linarith [min_le_right b ε]
  have hbu : b ≤ u := le_max_right _ _
  have hsub : Icc l u ⊆ Ioi 0 := fun _ hx ↦ hl.trans_le hx.1
  obtain ⟨p,hp,hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨b,hlb,hbu⟩ (hc.mono hsub)
  refine ⟨p,hsub hp,fun s hs ↦ ?_⟩
  by_cases hx : s ∈ Icc l u
  · exact hmax hx
  apply le_trans _ (hmax (show b ∈ Icc l u from ⟨hlb,hbu⟩))
  by_cases hlo : s < l
  · have hball : s ∈ Metric.ball 0 ε := by
      rw [Metric.mem_ball, Real.dist_eq, sub_zero, abs_of_pos hs]
      exact hlo.trans hlε
    exact (hsmall ⟨hball,hs⟩).le
  · have hhi : u ≤ s := by
      by_contra! hn
      exact hx ⟨le_of_not_gt hlo, hn.le⟩
    exact (hB s ((le_max_left B b).trans hhi)).le

/-- At held location, slant and df, a positive collapse exponent gives
a finite scale maximum. At criticality a finite fit exists exactly if a
positive scale reaches the scale-zero trace. A negative exponent excludes
attainment. -/
theorem held_slant_df_location_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ α : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (∃ a ∈ Ioi 0, IsMaxOn (likelihood x μ ν α) (Ioi 0) a) ↔
      0 < TDist.exponent x μ ν ∨
        (TDist.exponent x μ ν = 0 ∧ ∃ a > 0, collapseTrace x μ ν α ≤ likelihood x μ ν α a) := by
  constructor
  · rintro ⟨a,ha,hm⟩
    rcases lt_trichotomy (TDist.exponent x μ ν) 0 with he | he | he
    · have hpos : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
      obtain ⟨s,hs,hgt⟩ := (hpos.and
        ((skew_negative_path x μ α hν he).eventually_gt_atTop (likelihood x μ ν α a))).exists
      exact (hgt.not_ge (hm hs)).elim
    · refine Or.inr ⟨he,a,ha,?_⟩
      apply le_of_tendsto (skew_critical_path x μ α hν he).1
      filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
      exact hm hs
    · exact Or.inl he
  · rintro (he | ⟨he,a,ha,hC⟩)
    · exact scale_above_faces (scale_continuousOn x μ α hν) (vertical_small x μ α hν he)
        (vertical_large x hn μ α hν) one_pos (likelihood_positive x μ α 1 hν)
        (likelihood_positive x μ α 1 hν).le
    · exact scale_above_faces (scale_continuousOn x μ α hν) (skew_critical_path x μ α hν he).1
        (vertical_large x hn μ α hν) ha (likelihood_positive x μ α a hν) hC

/-- With location, slant and df held, small scale has its three exponent
regimes, and the large-scale end vanishes. -/
theorem held_slant_df_location_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ α : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (TDist.exponent x μ ν < 0 → Tendsto (likelihood x μ ν α) (𝓝[>] 0) atTop) ∧
    (TDist.exponent x μ ν = 0 →
      Tendsto (likelihood x μ ν α) (𝓝[>] 0) (𝓝 (collapseTrace x μ ν α)) ∧
      0 < collapseTrace x μ ν α) ∧
    (0 < TDist.exponent x μ ν → Tendsto (likelihood x μ ν α) (𝓝[>] 0) (𝓝 0)) ∧
    Tendsto (likelihood x μ ν α) atTop (𝓝 0) :=
  ⟨skew_negative_path x μ α hν, skew_critical_path x μ α hν,
    vertical_small x μ α hν, vertical_large x hn μ α hν⟩

/-- At held zero slant and df one, the sample `(-1,1)` has exactly
the positive-scale upper unit semicircle of maximizers. -/
theorem skew_cauchy_arc (p : ℝ × ℝ) :
    (p ∈ Cauchy.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood ![-1,1] q.1 1 0 q.2) Cauchy.domain p) ↔
      0 < p.2 ∧ p.1^2+p.2^2 = 1 := by
  let x : Fin 2 → ℝ := ![-1,1]
  have hcount : 2 * TDist.multiplicity x (-1) = 2 := by
    norm_num [x, TDist.multiplicity, Finset.univ_fin2, Finset.filter_insert]
  have hx (i : Fin 2) : x i = -1 ∨ x i = 1 := by fin_cases i <;> simp [x]
  have H := StudentCauchy.lst_df_one_balanced_argmax x (by decide) (by norm_num : (-1 : ℝ) < 1)
    hcount hx p
  change (p ∈ Cauchy.domain ∧ IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 1 0 q.2)
    Cauchy.domain p) ↔ _
  simp only [zero_slant x _ _ one_pos, TDist.likelihood, isMaxOn_iff, exp_le_exp]
  simp only [isMaxOn_iff] at H
  rw [H]
  constructor
  · rintro ⟨_,_,hs,h⟩
    exact ⟨hs,by nlinarith⟩
  · rintro ⟨hs,h⟩
    have hs' := sq_pos_of_pos hs
    refine ⟨?_,?_,hs,by nlinarith⟩
    · by_contra! hh
      nlinarith [mul_nonneg (show 0 ≤ -p.1-1 by linarith) (show 0 ≤ -p.1+1 by linarith)]
    · by_contra! hh
      nlinarith [mul_nonneg (show 0 ≤ p.1-1 by linarith) (show 0 ≤ p.1+1 by linarith)]

private theorem paired_slant_strict {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (σ : Equiv.Perm (Fin n)) (hpair : ∀ i, x (σ i) - μ = -(x i-μ))
    (hx : ∃ i, x i ≠ μ) {ν α s : ℝ} (hν : 0 < ν) (hs : 0 < s) (hα : α ≠ 0) :
    likelihood x μ ν α s < likelihood x μ ν 0 s := by
  let z (i : Fin n) := α*(x i-μ)*sqrt ((ν+1)/(ν*s^2+(x i-μ)^2))
  let F (i : Fin n) := 2 * studentCDF (ν+1) (z i)
  have hv : 0 < ν+1 := by positivity
  have hz (i : Fin n) : z (σ i) = -z i := by
    dsimp [z]
    rw [hpair, neg_sq]
    ring
  have hF (i : Fin n) : F (σ i) = 2-F i := by
    simp only [F, hz, cdf_reflect hv]
    ring
  have hP (i : Fin n) : 0 < F i := mul_pos two_pos (cdf_pos hv _)
  have hle (i : Fin n) : F i * F (σ i) ≤ 1 := by
    rw [hF]
    nlinarith [sq_nonneg (F i-1)]
  obtain ⟨i,hi⟩ := hx
  have hFi : F i ≠ 1 := by
    intro he
    have hCDF : studentCDF (ν+1) (z i) = studentCDF (ν+1) 0 := by
      rw [cdf_zero hv]
      dsimp [F] at he
      linarith
    have hZ := (cdf_strictMono hv).injective hCDF
    have h' : sqrt ((ν+1)/(ν*s^2+(x i-μ)^2)) ≠ 0 := by positivity
    exact (mul_ne_zero (mul_ne_zero hα (sub_ne_zero.mpr hi)) h') hZ
  have hlt : F i * F (σ i) < 1 := by
    rw [hF]
    nlinarith [sq_pos_of_ne_zero (sub_ne_zero.mpr hFi)]
  have hprod : (∏ j, F j)^2 < 1 := by
    have h := Finset.prod_lt_prod₀ (s := Finset.univ)
      (fun j _ ↦ mul_pos (hP j) (hP (σ j)))
      (fun j _ ↦ hle j) ⟨i,Finset.mem_univ _,hlt⟩
    rw [Finset.prod_mul_distrib, Equiv.prod_comp σ F] at h
    simpa [pow_two] using h
  have hp : (∏ j, F j) < 1 := by
    by_contra! hh
    have h := (sq_le_sq₀ zero_le_one (zero_le_one.trans hh)).2 hh
    exact hprod.not_ge (by simpa only [one_pow] using h)
  rw [zero_slant x μ s hν]
  change TDist.likelihood x μ ν s * (∏ j, F j) < TDist.likelihood x μ ν s
  exact mul_lt_of_lt_one_right (exp_pos _) hp

set_option maxHeartbeats 650000 in
-- The exact product comparison and the reparameterized maximizing set elaborate together.
/-- At held df and location, sign-paired residual data with positive
collapse exponent have exactly one joint scale and slant maximum. Its
slant is zero. Pairing is expressed by a residual-reversing permutation. -/
theorem paired_held_df_location_unique {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ : ℝ)
    (σ : Equiv.Perm (Fin n)) (hpair : ∀ i, x (σ i) - μ = -(x i-μ))
    {ν : ℝ} (hν : 0 < ν) (he : 0 < TDist.exponent x μ ν) :
    ∃ s > 0, ∀ p : ℝ × ℝ,
      (p ∈ univ ×ˢ Ioi 0 ∧
        IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x μ ν q.1 q.2) (univ ×ˢ Ioi 0) p) ↔
      p = (0,s) := by
  have hx : ∃ i, x i ≠ μ := by
    by_contra! h
    rw [TDist.exponent_all_tied x μ ν h] at he
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    linarith
  have hS := (TDist.heldLocation_maximum x μ hν hx).2 he
  obtain ⟨s,⟨hs,hr⟩,huniq⟩ := hS.1
  have hMax := (hS.2 s hs).mpr hr
  have hw (a : ℝ) (ha : 0 < a) (b : ℝ) : likelihood x μ ν b a ≤ likelihood x μ ν 0 a := by
    by_cases h : b = 0
    · rw [h]
    · exact (paired_slant_strict x μ σ hpair hx hν ha h).le
  refine ⟨s,hs,fun p ↦ ?_⟩
  constructor
  · rintro ⟨hp,hm⟩
    have ha : 0 < p.2 := hp.2
    have hb : p.1 = 0 := by
      by_contra h
      exact (paired_slant_strict x μ σ hpair hx hν ha h).not_ge
        (isMaxOn_iff.mp hm (0,p.2) ⟨mem_univ _,ha⟩)
    have h' : IsMaxOn (TDist.logLik x μ ν) (Ioi 0) p.2 := by
      intro a ha'
      have H := isMaxOn_iff.mp hm (0,a) (show (0,a) ∈ univ ×ˢ Ioi (0 : ℝ) from
        ⟨mem_univ _,ha'⟩)
      change likelihood x μ ν 0 a ≤ likelihood x μ ν p.1 p.2 at H
      rw [hb, zero_slant x μ a hν, zero_slant x μ p.2 hν] at H
      exact exp_le_exp.mp H
    exact Prod.ext hb (huniq p.2 ⟨ha,(hS.2 p.2 ha).mp h'⟩)
  · rintro rfl
    refine ⟨⟨mem_univ _,hs⟩,fun q hq ↦ ?_⟩
    have H : likelihood x μ ν 0 q.2 ≤ likelihood x μ ν 0 s := by
      rw [zero_slant x μ q.2 hν, zero_slant x μ s hν]
      exact exp_le_exp.mpr (hMax hq.2)
    exact (hw q.2 hq.2 q.1).trans H

end
end UnivariateML.StudentSkew
end
