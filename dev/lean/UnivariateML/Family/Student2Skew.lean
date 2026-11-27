module

public import Mathlib.MeasureTheory.Integral.Prod

public import UnivariateML.Family.StudentSkew
public import UnivariateML.Family.Student2Free
public import UnivariateML.Family.Student2LocationScaleHeldDf
public import UnivariateML.Family.Student2WidthWitness

import Mathlib.Tactic
import Mathlib.Topology.Instances.EReal.Lemmas
import Mathlib.Topology.Sequences
import Mathlib.Analysis.SpecialFunctions.Sqrt

public import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Analysis.Complex.HasPrimitives
public import Mathlib.Analysis.Analytic.IsolatedZeros

set_option linter.style.header false

/-!
# Skew Student escape and attainment

The ordinary Student product bounds skew Student uniformly in slant.
Moving contact and displacement limits identify the eligible excluded
heights. Compact superlevels give the finite comparison under the holds.
-/

@[expose] public section

open Set Filter Topology Real MeasureTheory

namespace UnivariateML.Student2Skew

open StudentSkew

/-- The normalized Student CDF lies in the unit interval. -/
theorem cdf_bounds (ν z : ℝ) : 0 ≤ StudentSkew.studentCDF ν z ∧
    StudentSkew.studentCDF ν z ≤ 1 := by
  have hpos (y : ℝ) : 0 ≤ TDist.density 0 ν 1 y := (exp_pos _).le
  have hB : 0 ≤ ∫ y, TDist.density 0 ν 1 y := integral_nonneg hpos
  have hn : 0 ≤ ∫ y in Iic z, TDist.density 0 ν 1 y := integral_nonneg hpos
  refine ⟨div_nonneg hn hB, ?_⟩
  unfold StudentSkew.studentCDF
  by_cases h : Integrable (TDist.density 0 ν 1)
  · have hmono := integral_mono_measure
      (μ := volume.restrict (Iic z)) (ν := volume) Measure.restrict_le_self
      (Eventually.of_forall hpos) h
    rcases hB.eq_or_lt with hzero | hstrict
    · rw [← hzero, div_zero]
      norm_num
    · exact (div_le_one hstrict).mpr hmono
  · rw [integral_undef h, div_zero]
    norm_num

private theorem dominated {n : ℕ} (x : Fin n → ℝ) (b ν α a : ℝ) :
    0 ≤ StudentSkew.likelihood x b ν α a ∧
      StudentSkew.likelihood x b ν α a ≤ (2 : ℝ)^n * TDist.likelihood x b ν a := by
  unfold StudentSkew.likelihood
  constructor
  · exact mul_nonneg (exp_pos _).le
      (Finset.prod_nonneg fun _ _ ↦ mul_nonneg zero_le_two (cdf_bounds _ _).1)
  · rw [mul_comm ((2 : ℝ)^n)]
    apply mul_le_mul_of_nonneg_left _ (exp_pos _).le
    rw [show (2 : ℝ)^n = ∏ _ : Fin n, (2 : ℝ) by simp]
    exact Finset.prod_le_prod₀ (fun _ _ ↦ mul_nonneg zero_le_two (cdf_bounds _ _).1)
      (fun _ _ ↦ mul_le_of_le_one_right zero_le_two (cdf_bounds _ _).2)

/-- On the fitted df branch, a strict majority is exactly the unbounded
case. Remote and strictly subcritical collapse bounds hold uniformly
over arbitrary finite or divergent slant sequences. -/
theorem fitted_majority_and_vanishing_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2.1}
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    ((¬ BddAbove (L '' D)) ↔ ∃ b, n < 2 * TDist.multiplicity x b) ∧
    (∀ {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ},
      (∀ᶠ j in l, 0 < a j) → (∀ᶠ j in l, 1 ≤ v j) →
      (Tendsto a l atTop ∨ Tendsto m l (cocompact ℝ)) →
      Tendsto (fun j ↦ StudentSkew.likelihood x (m j) (v j) (α j) (a j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b q : ℝ},
      Tendsto m l (𝓝 b) → Tendsto a l (𝓝[>] 0) → Tendsto v l (𝓝 q) →
      0 < q → 0 < TDist.exponent x b q →
      Tendsto (fun j ↦ StudentSkew.likelihood x (m j) (v j) (α j) (a j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b : ℝ},
      Tendsto m l (𝓝 b) → Tendsto a l (𝓝[>] 0) → Tendsto v l atTop →
      (∃ i, x i ≠ b) →
      Tendsto (fun j ↦ StudentSkew.likelihood x (m j) (v j) (α j) (a j)) l (𝓝 0)) := by
  dsimp only
  let D : Set (ℝ × ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2.1}
  let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
  let T := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
  let E : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2}
  refine ⟨⟨?_,?_⟩,?_,?_,?_⟩
  · intro h
    by_contra! hm
    have hb : BddAbove (T '' E) := not_not.mp fun hu ↦
      not_exists.mpr (fun b ↦ not_lt_of_ge (hm b)) ((Student2Free.lst_all_free_decision x hn).1.mp hu)
    obtain ⟨B,hB⟩ := hb
    apply h
    refine ⟨(2 : ℝ)^n * B, ?_⟩
    rintro _ ⟨⟨b,a,v,α⟩,hp,rfl⟩
    have h' : T (b,a,v) ≤ B := hB ⟨(b,a,v),hp,rfl⟩
    exact (dominated x b v α a).2.trans (mul_le_mul_of_nonneg_left h' (by positivity))
  · rintro ⟨b,hb⟩ ⟨B,hB⟩
    have hb' : (n : ℝ) < 2 * (TDist.multiplicity x b : ℝ) := by exact_mod_cast hb
    have he : TDist.exponent x b 1 < 0 := by
      unfold TDist.exponent
      linarith
    have hp : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
    obtain ⟨a,ha,hbad⟩ := (hp.and
      ((StudentSkew.symmetric_collapse_unbounded x b zero_lt_one he).eventually_gt_atTop B)).exists
    have H : L (b,a,1,0) ≤ B := hB ⟨(b,a,1,0),⟨ha,le_rfl⟩,rfl⟩
    exact hbad.not_ge H
  · intro ι l m a v α ha hv ht
    have h := (Student2Normal.moving_shape_remote_ends x hn ha hv ht).const_mul ((2 : ℝ)^n)
    rw [mul_zero] at h
    exact squeeze_zero' (Eventually.of_forall fun j ↦ (dominated x (m j) (v j) (α j) (a j)).1)
      (Eventually.of_forall fun j ↦ (dominated x (m j) (v j) (α j) (a j)).2) h
  · intro ι l m a v α b q hm ha hv hq he
    have h := ((Student2MovingCollapse.moving_shape_collapse x b hq hm ha hv).1 he).const_mul ((2 : ℝ)^n)
    rw [mul_zero] at h
    exact squeeze_zero' (Eventually.of_forall fun j ↦ (dominated x (m j) (v j) (α j) (a j)).1)
      (Eventually.of_forall fun j ↦ (dominated x (m j) (v j) (α j) (a j)).2) h
  · intro ι l m a v α b hm ha hv hx
    have h := (Student2Normal.zero_scale_normal_corner x hm ha hv hx).const_mul ((2 : ℝ)^n)
    rw [mul_zero] at h
    exact squeeze_zero' (Eventually.of_forall fun j ↦ (dominated x (m j) (v j) (α j) (a j)).1)
      (Eventually.of_forall fun j ↦ (dominated x (m j) (v j) (α j) (a j)).2) h

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

/-- The Student CDF is positive at every finite argument and positive shape. -/
theorem cdf_pos {ν : ℝ} (hν : 0 < ν) (z : ℝ) : 0 < studentCDF ν z := by
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

private theorem moving_normal_multiplier {ι : Type*} {l : Filter ι}
    {m a v : ι → ℝ} {b s : ℝ} (y : ℝ) (hs : 0 < s)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s)) (hv : Tendsto v l atTop) :
    Tendsto (fun j ↦ sqrt ((v j+1)/(v j * (a j)^2+(y-m j)^2))) l
      (𝓝 s⁻¹) := by
  have hr := (tendsto_const_nhds (x := y)).sub hm
  have hnum := ((tendsto_const_nhds (x := (1 : ℝ))).div_atTop hv).const_add 1
  have hden := (ha.pow 2).add ((hr.pow 2).div_atTop hv)
  have H := (hnum.div hden (by positivity)).sqrt
  simp only [add_zero, one_div, sqrt_inv, sqrt_sq hs.le] at H
  refine H.congr' ?_
  filter_upwards [hv.eventually_gt_atTop 0, ha.eventually (Ioi_mem_nhds hs)] with j hj hj'
  change 0 < a j at hj'
  have h : (1+1/v j)/((a j)^2+(y-m j)^2/v j) =
      (v j+1)/(v j*(a j)^2+(y-m j)^2) := by
    field_simp [hj.ne', (show 0 < v j*(a j)^2+(y-m j)^2 by positivity).ne',
      (show 0 < (a j)^2+(y-m j)^2/v j by positivity).ne']
  change sqrt ((1 + (v j)⁻¹) / ((a j)^2 + (y - m j)^2 / v j)) = _
  rw [← one_div, h]

private theorem moving_normal_argument {ι : Type*} {l : Filter ι}
    {m a v : ι → ℝ} {b s : ℝ} (y : ℝ) (hs : 0 < s)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s)) (hv : Tendsto v l atTop) :
    Tendsto (fun j ↦ (y - m j) * sqrt ((v j+1)/(v j * (a j)^2+(y-m j)^2))) l
      (𝓝 ((y-b)/s)) := by
  simpa only [div_eq_mul_inv] using ((tendsto_const_nhds (x := y)).sub hm).mul
    (moving_normal_multiplier y hs hm ha hv)

/-- If df diverges and the other coordinates converge at positive
scale, the skew Student likelihood reaches the matched skew-normal
even with moving location, scale and finite slant. -/
theorem moving_skew_normal_face {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b s β : ℝ}
    (hs : 0 < s) (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s))
    (hv : Tendsto v l atTop) (hα : Tendsto α l (𝓝 β)) :
    Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l
      (𝓝 ((∏ i, TDist.normalDensity b s (x i)) *
        ∏ i, 2 * SkewLSGaussian.Φ (β * ((x i-b)/s)))) := by
  have hν : Tendsto (fun j ↦ v j+1) l atTop := tendsto_atTop_add_const_right _ _ hv
  have hF (i : Fin n) :
      Tendsto (fun j ↦ 2 * studentCDF (v j+1)
        (α j * (x i-m j) * sqrt ((v j+1)/(v j*(a j)^2+(x i-m j)^2)))) l
          (𝓝 (2 * SkewLSGaussian.Φ (β * ((x i-b)/s)))) := by
    have harg := hα.mul (moving_normal_argument (x i) hs hm ha hv)
    have H := (cdf_normal_moving hν harg).const_mul (2 : ℝ)
    refine H.congr (fun j ↦ ?_)
    simp only [← mul_assoc]
  exact (Student2Normal.moving_normal_face x hs hm ha hv).mul
    (tendsto_finsetProd Finset.univ fun i _ ↦ hF i)

private theorem logGamma_differentiable_pos {z : ℝ} (hz : 0 < z) :
    DifferentiableAt ℝ (fun t : ℝ ↦ log (Gamma t)) z :=
  (differentiableAt_Gamma (fun m ↦ ne_of_gt
    ((neg_nonpos.mpr (Nat.cast_nonneg m)).trans_lt hz))).log (Gamma_pos_of_pos hz).ne'

private theorem logGamma_continuous_pos {z : ℝ} (hz : 0 < z) :
    ContinuousAt (fun t : ℝ ↦ log (Gamma t)) z :=
  (logGamma_differentiable_pos hz).continuousAt

private theorem normalizer_continuous {ν : ℝ} (hν : 0 < ν) :
    ContinuousAt TDist.logNormalizer ν := by
  unfold TDist.logNormalizer
  refine ContinuousAt.sub (ContinuousAt.sub ?_ ?_) ?_
  · exact (logGamma_continuous_pos (show 0 < (ν+1)/2 by positivity)).comp'
      (f := fun t : ℝ ↦ (t+1)/2) (x := ν) (by fun_prop)
  · exact (logGamma_continuous_pos (show 0 < ν/2 by positivity)).comp'
      (f := fun t : ℝ ↦ t/2) (x := ν) (by fun_prop)
  · exact ((by fun_prop : Continuous (fun t : ℝ ↦ t*π)).continuousAt.log
      (by positivity)).div_const 2


private theorem density_limit {ι : Type*} {l : Filter ι}
    {m a v : ι → ℝ} {b s q : ℝ} (y : ℝ) (hs : 0 < s) (hq : 0 < q)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s)) (hv : Tendsto v l (𝓝 q)) :
    Tendsto (fun j ↦ TDist.density (m j) (v j) (a j) y) l (𝓝 (TDist.density b q s y)) := by
  have hR := (((tendsto_const_nhds (x := y)).sub hm).pow 2).div (hv.mul (ha.pow 2)) (by positivity)
  have hN := (normalizer_continuous hq).tendsto.comp hv
  have H := (hN.sub (ha.log hs.ne')).sub
    (((hv.add_const 1).div_const 2).mul ((hR.const_add 1).log (by positivity)))
  exact continuous_exp.continuousAt.tendsto.comp H

private theorem integral_shape_limit {ι : Type*} {l : Filter ι} [l.IsCountablyGenerated]
    {v : ι → ℝ} {q : ℝ}
    (hq : 0 < q) (hv : Tendsto v l (𝓝 q)) (μ : Measure ℝ) (hμ : μ ≤ volume) :
    Tendsto (fun j ↦ ∫ y, TDist.density 0 (v j) 1 y ∂μ) l
      (𝓝 (∫ y, TDist.density 0 q 1 y ∂μ)) := by
  let η := q/2
  let V := q+1
  let C := TDist.logNormalizer q + 1
  have hη : 0 < η := half_pos hq
  have hV : 0 < V := by dsimp [V]; positivity
  have hην : η < q := half_lt_self hq
  have hqV : q < V := by dsimp [V]; linarith
  let B := fun y ↦ exp (C + (η+1)/2 * log (V/η) - TDist.logNormalizer η) *
    TDist.density 0 η 1 y
  apply tendsto_integral_filter_of_dominated_convergence B
  · filter_upwards [hv.eventually (Ioi_mem_nhds hq)] with j hj
    exact (student_continuous hj).aestronglyMeasurable
  · have hC := ((normalizer_continuous hq).tendsto.comp hv).eventually
      (Iio_mem_nhds (show TDist.logNormalizer q < C by dsimp [C]; linarith))
    filter_upwards [hC, hv.eventually (Ioi_mem_nhds hην), hv.eventually (Iio_mem_nhds hqV)]
      with j hj hηj hVj
    refine Eventually.of_forall fun y ↦ ?_
    have hv' : 0 < v j := hη.trans hηj
    have hVy : 1+y^2/η ≤ (V/η) * (1+y^2/(v j)) := by
      have hR : y^2 / V ≤ y^2 / (v j) :=
        div_le_div_of_nonneg_left (sq_nonneg _) hv' hVj.le
      have h := mul_le_mul_of_nonneg_left hR hV.le
      rw [mul_div_cancel₀ _ hV.ne'] at h
      have he : η ≤ V := hην.le.trans hqV.le
      calc
        1 + y^2/η = (η + y^2)/η := by field_simp
        _ ≤ (V * (1 + y^2/(v j)))/η := by
          apply div_le_div_of_nonneg_right _ hη.le
          nlinarith only [he, h]
        _ = (V/η) * (1+y^2/(v j)) := by ring
    have hl := log_le_log (show 0 < 1+y^2/η by positivity) hVy
    rw [log_mul (by positivity) (by positivity)] at hl
    have hnon : 0 ≤ log (1+y^2/v j) := log_nonneg (le_add_of_nonneg_right (by positivity))
    have h₁ := mul_le_mul_of_nonneg_left hl (show 0 ≤ (η+1)/2 by positivity)
    have h₂ := mul_le_mul_of_nonneg_right (show (η+1)/2 ≤ (v j+1)/2 by linarith) hnon
    rw [Real.norm_eq_abs, abs_of_pos (student_positive _ _)]
    dsimp [B]
    rw [TDist.density, TDist.density, ← exp_add]
    simp only [sub_zero, log_one, one_pow, mul_one, exp_le_exp]
    change TDist.logNormalizer (v j) < C at hj
    nlinarith only [h₁,h₂,hj]
  · exact ((student_integrable hη).const_mul _).mono_measure hμ
  · exact Eventually.of_forall fun y ↦ density_limit y zero_lt_one hq
      tendsto_const_nhds tendsto_const_nhds hv

private theorem cdf_shape_fixed {ι : Type*} {l : Filter ι} {v : ι → ℝ} {q : ℝ}
    (hq : 0 < q) (hv : Tendsto v l (𝓝 q)) (z : ℝ) :
    Tendsto (fun j ↦ studentCDF (v j) z) l (𝓝 (studentCDF q z)) := by
  have H : Tendsto (fun t ↦ studentCDF t z) (𝓝 q) (𝓝 (studentCDF q z)) :=
    (integral_shape_limit hq tendsto_id (volume.restrict (Iic z)) Measure.restrict_le_self).div
      (integral_shape_limit hq tendsto_id volume le_rfl) (mass_positive hq).ne'
  exact H.comp hv

private theorem cdf_shape_moving {ι : Type*} {l : Filter ι}
    {v z : ι → ℝ} {q u : ℝ} (hq : 0 < q)
    (hv : Tendsto v l (𝓝 q)) (hz : Tendsto z l (𝓝 u)) :
    Tendsto (fun j ↦ studentCDF (v j) (z j)) l (𝓝 (studentCDF q u)) := by
  have hp := hv.eventually (Ioi_mem_nhds hq)
  refine tendsto_order.mpr ⟨?_, ?_⟩
  · intro b hb
    have h : ∀ᶠ w : ℝ in 𝓝[<] u, b < studentCDF q w :=
      (((cdf_continuous hq).continuousAt (x := u)).tendsto.mono_left
        nhdsWithin_le_nhds).eventually (Ioi_mem_nhds hb)
    have hdom : ∀ᶠ w : ℝ in 𝓝[<] u, w < u := self_mem_nhdsWithin
    obtain ⟨w,hw,hwb⟩ := (hdom.and h).exists
    filter_upwards [hp, hz.eventually (Ioi_mem_nhds hw),
      (cdf_shape_fixed hq hv w).eventually (Ioi_mem_nhds hwb)] with j hj hjw hjb
    exact hjb.trans (cdf_strictMono hj hjw)
  · intro b hb
    have h : ∀ᶠ w : ℝ in 𝓝[>] u, studentCDF q w < b :=
      (((cdf_continuous hq).continuousAt (x := u)).tendsto.mono_left
        nhdsWithin_le_nhds).eventually (Iio_mem_nhds hb)
    have hdom : ∀ᶠ w : ℝ in 𝓝[>] u, u < w := self_mem_nhdsWithin
    obtain ⟨w,hw,hwb⟩ := (hdom.and h).exists
    filter_upwards [hp, hz.eventually (Iio_mem_nhds hw),
      (cdf_shape_fixed hq hv w).eventually (Iio_mem_nhds hwb)] with j hj hjw hjb
    exact (cdf_strictMono hj hjw).trans hjb

/-- The skew Student likelihood is jointly continuous when df, scale,
location and slant approach finite admissible coordinates. -/
theorem moving_finite_skew_face {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b s q β : ℝ}
    (hs : 0 < s) (hq : 0 < q)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s))
    (hv : Tendsto v l (𝓝 q)) (hα : Tendsto α l (𝓝 β)) :
    Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l
      (𝓝 (likelihood x b q β s)) := by
  have hB := tendsto_finsetProd Finset.univ fun i _ ↦ density_limit (x i) hs hq hm ha hv
  have hF (i : Fin n) :
      Tendsto (fun j ↦ 2 * studentCDF (v j+1)
        (α j * (x i-m j) * sqrt ((v j+1)/(v j*(a j)^2+(x i-m j)^2)))) l
      (𝓝 (2 * studentCDF (q+1) (β * (x i-b) * sqrt ((q+1)/(q*s^2+(x i-b)^2))))) := by
    have hr := (tendsto_const_nhds (x := x i)).sub hm
    have h := (hα.mul hr).mul (((hv.add_const 1).div
      ((hv.mul (ha.pow 2)).add (hr.pow 2)) (by positivity)).sqrt)
    exact (cdf_shape_moving (by positivity : 0 < q+1) (hv.add_const 1) h).const_mul 2
  have H := hB.mul (tendsto_finsetProd Finset.univ fun i _ ↦ hF i)
  simp_rw [TDist.density, ← exp_sum] at H
  exact H

private theorem cdf_atBot (ν : ℝ) : Tendsto (studentCDF ν) atBot (𝓝 0) := by
  have H := (tendsto_integral_Iic_zero (f := TDist.density 0 ν 1) (μ := volume)
    (a := id) tendsto_id).div_const (∫ y, TDist.density 0 ν 1 y)
  change Tendsto (fun z ↦ studentCDF ν z) atBot (𝓝 0)
  simpa [studentCDF] using H

private theorem cdf_atTop {ν : ℝ} (hν : 0 < ν) : Tendsto (studentCDF ν) atTop (𝓝 1) := by
  have H := ((cdf_atBot ν).comp tendsto_neg_atTop_atBot).const_sub 1
  simpa [Function.comp_def, cdf_reflect hν] using H

private theorem cdf_shape_top {ι : Type*} {l : Filter ι} {v z : ι → ℝ} {q : ℝ}
    (hq : 0 < q) (hv : Tendsto v l (𝓝 q)) (hz : Tendsto z l atTop) :
    Tendsto (fun j ↦ studentCDF (v j) (z j)) l (𝓝 1) := by
  refine tendsto_order.mpr ⟨?_,?_⟩
  · intro B hB
    obtain ⟨w,hw⟩ := ((cdf_atTop hq).eventually (Ioi_mem_nhds hB)).exists
    filter_upwards [hv.eventually (Ioi_mem_nhds hq), hz.eventually_gt_atTop w,
      (cdf_shape_fixed hq hv w).eventually (Ioi_mem_nhds hw)] with j hj hw' hjB
    exact hjB.trans (cdf_strictMono hj hw')
  · intro B hB
    exact Eventually.of_forall fun j ↦ ((cdf_bounds (v j) (z j)).2).trans_lt hB

private theorem cdf_shape_bot {ι : Type*} {l : Filter ι} {v z : ι → ℝ} {q : ℝ}
    (hq : 0 < q) (hv : Tendsto v l (𝓝 q)) (hz : Tendsto z l atBot) :
    Tendsto (fun j ↦ studentCDF (v j) (z j)) l (𝓝 0) := by
  refine tendsto_order.mpr ⟨?_,?_⟩
  · intro B hB
    exact Eventually.of_forall fun j ↦ hB.trans_le (cdf_bounds (v j) (z j)).1
  · intro B hB
    obtain ⟨w,hw⟩ := ((cdf_atBot q).eventually (Iio_mem_nhds hB)).exists
    filter_upwards [hv.eventually (Ioi_mem_nhds hq), hz.eventually_lt_atBot w,
      (cdf_shape_fixed hq hv w).eventually (Iio_mem_nhds hw)] with j hj hw' hjB
    exact (cdf_strictMono hj hw').trans hjB

private theorem oriented_factors {n : ℕ} (x : Fin n → ℝ) {ι : Type*} {l : Filter ι}
    {b ε : ℝ} {α : ι → ℝ} {f : ℝ → ι → ℝ} {g : ι → ℝ → ℝ} {c : ℝ}
    (hε : ε^2 = 1) (hα : Tendsto (fun j ↦ ε * α j) l atTop)
    (hBase : ∀ y ≠ b, ∃ k > 0, Tendsto (f y) l (𝓝 ((y-b)*k)))
    (hTop : ∀ {z : ι → ℝ}, Tendsto z l atTop → Tendsto (fun j ↦ g j (z j)) l (𝓝 2))
    (hBot : ∀ {z : ι → ℝ}, Tendsto z l atBot → Tendsto (fun j ↦ g j (z j)) l (𝓝 0))
    (hC : Tendsto (fun j ↦ g j (α j * f b j)) l (𝓝 c)) :
    Tendsto (fun j ↦ ∏ i, g j (α j * f (x i) j)) l
      (𝓝 (∏ i, if 0 < ε * (x i-b) then 2 else if x i = b then c else 0)) := by
  apply tendsto_finsetProd
  intro i _
  by_cases hi : x i = b
  · simpa [hi] using hC
  · obtain ⟨k,hk,hk'⟩ := hBase (x i) hi
    have hs : Tendsto (fun j ↦ ε * f (x i) j) l (𝓝 (ε*(x i-b)*k)) := by
      simpa only [mul_assoc] using hk'.const_mul ε
    have heq (j : ι) : (ε * α j) * (ε * f (x i) j) = α j * f (x i) j := by
      calc
        _ = ε^2 * (α j * f (x i) j) := by ring
        _ = _ := by rw [hε, one_mul]
    have hne : ε*(x i-b) ≠ 0 :=
      mul_ne_zero (by intro h; simp [h] at hε) (sub_ne_zero.mpr hi)
    rcases lt_or_gt_of_ne hne with hp | hp
    · have ht := (hα.atTop_mul_neg (mul_neg_of_neg_of_pos hp hk) hs).congr heq
      simpa [hp.not_gt, hi] using hBot ht
    · have ht := (hα.atTop_mul_pos (mul_pos hp hk) hs).congr heq
      simpa [hp] using hTop ht

/-- At positive limiting scale, a slant tail has the oriented half-t
or half-normal limit. The contact factor is evaluated at the actual
limiting contact argument, including both infinite contact rates. -/
theorem moving_oriented_faces {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b s ε : ℝ} {t : EReal}
    (hs : 0 < s) (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s))
    (hε : ε^2 = 1) (hα : Tendsto (fun j ↦ ε * α j) l atTop)
    (hc : Tendsto (fun j ↦ ((α j * (b-m j) *
      sqrt ((v j+1)/(v j*(a j)^2+(b-m j)^2)) : ℝ) : EReal)) l (𝓝 t)) :
    (∀ q > 0, Tendsto v l (𝓝 q) →
      Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l
        (𝓝 (TDist.likelihood x b q s *
          ∏ i, if 0 < ε*(x i-b) then 2 else if x i = b then
            EReal.rec 0 (fun u ↦ 2 * studentCDF (q+1) u) 2 t else 0))) ∧
    (Tendsto v l atTop → Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l
      (𝓝 ((∏ i, TDist.normalDensity b s (x i)) *
        ∏ i, if 0 < ε*(x i-b) then 2 else if x i = b then
          EReal.rec 0 (fun u ↦ 2 * SkewLSGaussian.Φ u) 2 t else 0))) := by
  let f := fun y j ↦ (y-m j) * sqrt ((v j+1)/(v j*(a j)^2+(y-m j)^2))
  let g := fun j z ↦ 2 * studentCDF (v j+1) z
  have hc' : Tendsto (fun j ↦ ((α j * f b j : ℝ) : EReal)) l (𝓝 t) := by
    simpa only [f, ← mul_assoc] using hc
  have H (B : ℝ) (P : ℝ)
      (ht : Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l (𝓝 B))
      (hF : Tendsto (fun j ↦ ∏ i, g j (α j * f (x i) j)) l (𝓝 P)) :
      Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l (𝓝 (B * P)) := by
    simpa only [likelihood, f, g, ← mul_assoc] using ht.mul hF
  constructor
  · intro q hq hv
    have hv' : Tendsto (fun j ↦ v j+1) l (𝓝 (q+1)) := hv.add_const 1
    have hq' : 0 < q+1 := by positivity
    have hTop {z : ι → ℝ} (hz : Tendsto z l atTop) :
        Tendsto (fun j ↦ g j (z j)) l (𝓝 2) := by
      simpa only [mul_one] using (cdf_shape_top hq' hv' hz).const_mul (2 : ℝ)
    have hBot {z : ι → ℝ} (hz : Tendsto z l atBot) :
        Tendsto (fun j ↦ g j (z j)) l (𝓝 0) := by
      simpa only [mul_zero] using (cdf_shape_bot hq' hv' hz).const_mul (2 : ℝ)
    have hF : Tendsto (fun j ↦ g j (α j * f b j)) l
        (𝓝 (EReal.rec 0 (fun u ↦ 2 * studentCDF (q+1) u) 2 t)) := by
      cases t using EReal.rec with
      | bot => exact hBot (EReal.tendsto_coe_nhds_bot_iff.mp hc')
      | top => exact hTop (EReal.tendsto_coe_nhds_top_iff.mp hc')
      | coe u => exact (cdf_shape_moving hq' hv' (EReal.tendsto_coe.mp hc')).const_mul 2
    have hBase (y : ℝ) (_ : y ≠ b) : ∃ k > 0, Tendsto (f y) l (𝓝 ((y-b)*k)) := by
      let k := sqrt ((q+1)/(q*s^2+(y-b)^2))
      have hk : 0 < k := by dsimp [k]; positivity
      have hr := (tendsto_const_nhds (x := y)).sub hm
      exact ⟨k,hk,hr.mul (((hv.add_const 1).div
        ((hv.mul (ha.pow 2)).add (hr.pow 2)) (by positivity)).sqrt)⟩
    have hB := tendsto_finsetProd Finset.univ fun i _ ↦ density_limit (x i) hs hq hm ha hv
    simp_rw [TDist.density, ← exp_sum] at hB
    exact H _ _ hB (oriented_factors x hε hα hBase hTop hBot hF)
  · intro hv
    have hv' : Tendsto (fun j ↦ v j+1) l atTop := tendsto_atTop_add_const_right _ _ hv
    have hTop {z : ι → ℝ} (hz : Tendsto z l atTop) :
        Tendsto (fun j ↦ g j (z j)) l (𝓝 2) := by
      simpa only [mul_one] using (cdf_normal_right hv' hz).const_mul (2 : ℝ)
    have hBot {z : ι → ℝ} (hz : Tendsto z l atBot) :
        Tendsto (fun j ↦ g j (z j)) l (𝓝 0) := by
      simpa only [mul_zero] using (cdf_normal_left hv' hz).const_mul (2 : ℝ)
    have hF : Tendsto (fun j ↦ g j (α j * f b j)) l
        (𝓝 (EReal.rec 0 (fun u ↦ 2 * SkewLSGaussian.Φ u) 2 t)) := by
      cases t using EReal.rec with
      | bot => exact hBot (EReal.tendsto_coe_nhds_bot_iff.mp hc')
      | top => exact hTop (EReal.tendsto_coe_nhds_top_iff.mp hc')
      | coe u => exact (cdf_normal_moving hv' (EReal.tendsto_coe.mp hc')).const_mul 2
    have hBase (y : ℝ) (_ : y ≠ b) : ∃ k > 0, Tendsto (f y) l (𝓝 ((y-b)*k)) :=
      ⟨s⁻¹,inv_pos.mpr hs,by simpa only [f, div_eq_mul_inv] using moving_normal_argument y hs hm ha hv⟩
    exact H _ _ (Student2Normal.moving_normal_face x hs hm ha hv)
      (oriented_factors x hε hα hBase hTop hBot hF)

/-- The finite-slant multiplier of the critical symmetric atom trace. -/
noncomputable def atomSlantFactor {n : ℕ} (x : Fin n → ℝ) (b q β d : ℝ) : ℝ :=
  ∏ i, 2 * studentCDF (q+1)
    (β * (if x i = b then -d * sqrt ((q+1)/(q+d^2)) else
      (x i-b) * sqrt ((q+1)/(x i-b)^2)))

private theorem off_atom_argument {ι : Type*} {l : Filter ι} {m a v : ι → ℝ}
    {b q y : ℝ} (hy : y ≠ b)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 0)) (hv : Tendsto v l (𝓝 q)) :
    Tendsto (fun j ↦ (y-m j) * sqrt ((v j+1)/(v j*(a j)^2+(y-m j)^2))) l
      (𝓝 ((y-b) * sqrt ((q+1)/(y-b)^2))) := by
  have hr := (tendsto_const_nhds (x := y)).sub hm
  have h := hr.mul (((hv.add_const 1).div
    ((hv.mul (ha.pow 2)).add (hr.pow 2))
    (by simpa using (sq_pos_of_ne_zero (sub_ne_zero.mpr hy)).ne')).sqrt)
  simpa only [zero_pow (by norm_num : (2 : ℕ) ≠ 0), mul_zero, zero_add, Pi.div_def] using h

private theorem atom_arg_eq {b m a v : ℝ} (ha : 0 < a) (hv : 0 < v) :
    (b-m) * sqrt ((v+1)/(v*a^2+(b-m)^2)) =
      -((m-b)/a) * sqrt ((v+1)/(v+((m-b)/a)^2)) := by
  have h : v*a^2+(b-m)^2 = (v+((m-b)/a)^2)*a^2 := by
    field_simp [ha.ne']
    ring
  rw [h, ← div_div, sqrt_div (by positivity), sqrt_sq ha.le]
  ring

/-- At a critical skew Student atom with finite limiting displacement
and signed df rate, the finite-slant and oriented infinite-slant layers
are the base trace times the corresponding CDF product. -/
theorem moving_critical_skew_faces {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b q d r : ℝ}
    (hq : 0 < q) (he : TDist.exponent x b q = 0)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q))
    (hd : Tendsto (fun j ↦ (m j-b)/a j) l (𝓝 d))
    (hr : Tendsto (fun j ↦ (v j-q)*log (a j)) l (𝓝 r)) :
    let K := exp (((n : ℝ) - TDist.multiplicity x b)*r + TDist.collapseLogValue x b q -
      (q+1)/2 * TDist.multiplicity x b * log (1+d^2/q))
    let L := fun j ↦ likelihood x (m j) (v j) (α j) (a j)
    (∀ β, Tendsto α l (𝓝 β) → Tendsto L l (𝓝 (K * atomSlantFactor x b q β d))) ∧
    (∀ ε : ℝ, ε^2 = 1 → Tendsto (fun j ↦ ε * α j) l atTop →
      ∀ t : EReal, Tendsto (fun j ↦ ((α j * (b-m j) *
        sqrt ((v j+1)/(v j*(a j)^2+(b-m j)^2)) : ℝ) : EReal)) l (𝓝 t) →
      Tendsto L l (𝓝 (K * ∏ i, if 0 < ε*(x i-b) then 2 else if x i = b then
        EReal.rec 0 (fun u ↦ 2 * studentCDF (q+1) u) 2 t else 0))) := by
  dsimp only
  let f := fun y j ↦ (y-m j) * sqrt ((v j+1)/(v j*(a j)^2+(y-m j)^2))
  let g := fun j z ↦ 2 * studentCDF (v j+1) z
  have hv' : Tendsto (fun j ↦ v j+1) l (𝓝 (q+1)) := hv.add_const 1
  have hq' : 0 < q+1 := by positivity
  obtain ⟨ha0,hapos⟩ := tendsto_nhdsWithin_iff.mp ha
  have hB := (Student2MovingCollapse.moving_critical_atom_base x hn b hq he hm ha hv).1 d r hd hr
  have HC : Tendsto (f b) l (𝓝 (-d * sqrt ((q+1)/(q+d^2)))) := by
    have H := hd.neg.mul (((hv.add_const 1).div (hv.add (hd.pow 2)) (by positivity)).sqrt)
    refine H.congr' ?_
    filter_upwards [hapos,hv.eventually (Ioi_mem_nhds hq)] with j hja hjv
    exact (atom_arg_eq (b := b) (m := m j) hja hjv).symm
  constructor
  · intro β hβ
    have H (i : Fin n) : Tendsto (fun j ↦ f (x i) j) l (𝓝 (if x i = b then
        -d*sqrt ((q+1)/(q+d^2)) else (x i-b)*sqrt ((q+1)/(x i-b)^2))) := by
      by_cases hi : x i = b
      · simpa [hi] using HC
      · simpa [hi] using off_atom_argument hi hm ha0 hv
    have hF := tendsto_finsetProd Finset.univ fun i _ ↦
      (cdf_shape_moving hq' hv' (hβ.mul (H i))).const_mul (2 : ℝ)
    simpa only [likelihood, atomSlantFactor, f, ← mul_assoc] using hB.mul hF
  · intro ε hε hα t ht
    have ht' : Tendsto (fun j ↦ ((α j * f b j : ℝ) : EReal)) l (𝓝 t) := by
      simpa only [f, ← mul_assoc] using ht
    have hTop {z : ι → ℝ} (hz : Tendsto z l atTop) :
        Tendsto (fun j ↦ g j (z j)) l (𝓝 2) := by
      simpa only [mul_one] using (cdf_shape_top hq' hv' hz).const_mul (2 : ℝ)
    have hBot {z : ι → ℝ} (hz : Tendsto z l atBot) :
        Tendsto (fun j ↦ g j (z j)) l (𝓝 0) := by
      simpa only [mul_zero] using (cdf_shape_bot hq' hv' hz).const_mul (2 : ℝ)
    have hC : Tendsto (fun j ↦ g j (α j * f b j)) l
        (𝓝 (EReal.rec 0 (fun u ↦ 2 * studentCDF (q+1) u) 2 t)) := by
      cases t using EReal.rec with
      | bot => exact hBot (EReal.tendsto_coe_nhds_bot_iff.mp ht')
      | top => exact hTop (EReal.tendsto_coe_nhds_top_iff.mp ht')
      | coe u => exact (cdf_shape_moving hq' hv' (EReal.tendsto_coe.mp ht')).const_mul 2
    have hBase (y : ℝ) (hy : y ≠ b) : ∃ k > 0, Tendsto (f y) l (𝓝 ((y-b)*k)) := by
      let k := sqrt ((q+1)/(y-b)^2)
      have hk : 0 < k := sqrt_pos.mpr (div_pos hq' (sq_pos_of_ne_zero (sub_ne_zero.mpr hy)))
      exact ⟨k,hk,off_atom_argument hy hm ha0 hv⟩
    have hF := oriented_factors x hε hα hBase hTop hBot hC
    simpa only [likelihood, f, g, ← mul_assoc] using hB.mul hF

/-- The skew-normal face in the direct parameterization. -/
noncomputable def skewNormalHeight {n : ℕ} (x : Fin n → ℝ) (b s β : ℝ) : ℝ :=
  (∏ i, TDist.normalDensity b s (x i)) * ∏ i, 2 * SkewLSGaussian.Φ (β*((x i-b)/s))

/-- The sign and contact weights in an oriented one-sided face. -/
noncomputable def orientedWeight {n : ℕ} (x : Fin n → ℝ) (b ε c : ℝ) : ℝ :=
  ∏ i, if 0 < ε*(x i-b) then 2 else if x i = b then c else 0

/-- The critical symmetric value including displacement and signed rate. -/
noncomputable def criticalBase {n : ℕ} (x : Fin n → ℝ) (b q d r : ℝ) : ℝ :=
  exp (((n : ℝ) - TDist.multiplicity x b)*r + TDist.collapseLogValue x b q -
    (q+1)/2 * TDist.multiplicity x b * log (1+d^2/q))

private theorem right_zero : Tendsto (id : ℝ → ℝ) (𝓝[>] 0) (𝓝 0) :=
  nhdsWithin_le_nhds

private theorem right_pos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin

private theorem square_inv_right :
    Tendsto (fun t : ℝ ↦ (t^2)⁻¹) (𝓝[>] 0) atTop := by
  apply tendsto_inv_nhdsGT_zero.comp
  refine tendsto_nhdsWithin_iff.mpr ⟨by simpa using right_zero.pow 2, ?_⟩
  exact right_pos.mono fun _ h ↦ sq_pos_of_pos h

private theorem orientation_tail (ε : ℝ) (hε : ε^2 = 1) :
    Tendsto (fun t : ℝ ↦ ε * (ε * (t^2)⁻¹)) (𝓝[>] 0) atTop := by
  refine square_inv_right.congr fun t ↦ ?_
  rw [← mul_assoc, ← pow_two, hε, one_mul]

private theorem upper_reaches_skew_normal {n : ℕ} (x : Fin n → ℝ) {B : ℝ}
    (h : ∀ b s v β, 0 < s → 1 ≤ v → likelihood x b v β s ≤ B)
    (b s β : ℝ) (hs : 0 < s) : skewNormalHeight x b s β ≤ B := by
  apply le_of_tendsto (moving_skew_normal_face x hs (l := atTop) (v := id)
    (m := fun _ : ℝ ↦ b) (a := fun _ ↦ s) (α := fun _ ↦ β)
    tendsto_const_nhds tendsto_const_nhds tendsto_id tendsto_const_nhds)
  exact (eventually_ge_atTop (1 : ℝ)).mono fun v hv ↦ h b s v β hs hv

private theorem upper_reaches_oriented {n : ℕ} (x : Fin n → ℝ) {B : ℝ}
    (h : ∀ b s v β, 0 < s → 1 ≤ v → likelihood x b v β s ≤ B)
    (b s ε : ℝ) (hs : 0 < s) (hε : ε^2 = 1) :
    (∀ q ≥ 1, TDist.likelihood x b q s * orientedWeight x b ε 2 ≤ B) ∧
    ((∏ i, TDist.normalDensity b s (x i)) * orientedWeight x b ε 2 ≤ B) := by
  let m := fun t : ℝ ↦ b - ε*t
  let α := fun t : ℝ ↦ ε * (t^2)⁻¹
  have hm : Tendsto m (𝓝[>] 0) (𝓝 b) := by
    simpa only [m, id_eq, mul_zero, sub_zero] using (right_zero.const_mul ε).const_sub b
  have hp : Tendsto (fun t ↦ ε * α t) (𝓝[>] 0) atTop := orientation_tail ε hε
  have hArg (v : ℝ → ℝ) (k : ℝ) (hk : 0 < k)
      (hM : Tendsto (fun t ↦ sqrt ((v t+1)/(v t*s^2+(b-m t)^2))) (𝓝[>] 0) (𝓝 k)) :
      Tendsto (fun t ↦ ((α t*(b-m t)*sqrt ((v t+1)/(v t*s^2+(b-m t)^2)) : ℝ) : EReal))
        (𝓝[>] 0) (𝓝 ⊤) := by
    apply EReal.tendsto_coe_nhds_top_iff.mpr
    refine (tendsto_inv_nhdsGT_zero.atTop_mul_pos hk hM).congr' ?_
    filter_upwards [right_pos] with t ht
    have hId : α t*(b-m t) = t⁻¹ := by
      dsimp [α, m]
      rw [show b-(b-ε*t) = ε*t by ring]
      calc
        ε*(t^2)⁻¹*(ε*t) = ε^2 * ((t^2)⁻¹*t) := by ring
        _ = _ := by rw [hε, one_mul]; field_simp [ht.ne']
    change _ = α t*(b-m t)*_
    rw [hId]
  constructor
  · intro q hq
    have hq' := zero_lt_one.trans_le hq
    have hR : Tendsto (fun t ↦ b-m t) (𝓝[>] 0) (𝓝 0) := by
      simpa using hm.const_sub b
    let k := sqrt ((q+1)/(q*s^2))
    have hk : 0 < k := by dsimp [k]; positivity
    have hM : Tendsto (fun t ↦ sqrt ((q+1)/(q*s^2+(b-m t)^2))) (𝓝[>] 0) (𝓝 k) := by
      have H := (tendsto_const_nhds (x := q+1)).div
        ((hR.pow 2).const_add (q*s^2)) (by positivity)
      simpa only [k, Pi.div_def, zero_pow (by norm_num : (2 : ℕ) ≠ 0), add_zero] using H.sqrt
    have H := (moving_oriented_faces x hs hm (a := fun _ ↦ s) (v := fun _ ↦ q)
      tendsto_const_nhds hε hp (hArg (fun _ ↦ q) k hk hM)).1 q hq' tendsto_const_nhds
    change TDist.likelihood x b q s * orientedWeight x b ε 2 ≤ B
    exact le_of_tendsto H (Eventually.of_forall fun t ↦ h (m t) s q (α t) hs hq)
  · let v := fun t : ℝ ↦ 1+t⁻¹
    have hv : Tendsto v (𝓝[>] 0) atTop :=
      tendsto_atTop_add_const_left _ _ tendsto_inv_nhdsGT_zero
    have hM := moving_normal_multiplier b hs hm (a := fun _ ↦ s) tendsto_const_nhds hv
    have H := (moving_oriented_faces x hs hm (a := fun _ ↦ s)
      tendsto_const_nhds hε hp (hArg v s⁻¹ (inv_pos.mpr hs) hM)).2 hv
    exact le_of_tendsto H (right_pos.mono fun t ht ↦ h (m t) s (v t) (α t) hs (by
      dsimp [v]; exact le_add_of_nonneg_right (inv_pos.mpr ht).le))

private theorem upper_reaches_atom {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {B : ℝ}
    (b q : ℝ) (hq' : 0 < q) (he : TDist.exponent x b q = 0)
    (h : ∀ b s β, 0 < s → likelihood x b q β s ≤ B) :
    (∀ β d, criticalBase x b q d 0 * atomSlantFactor x b q β d ≤ B) ∧
    (∀ ε, ε^2 = 1 → exp (TDist.collapseLogValue x b q) * orientedWeight x b ε 2 ≤ B) := by
  have hr : Tendsto (fun t : ℝ ↦ (q-q)*log t) (𝓝[>] 0) (𝓝 0) := by simp
  constructor
  · intro β d
    let m := fun t : ℝ ↦ b+d*t
    have hm : Tendsto m (𝓝[>] 0) (𝓝 b) := by
      simpa only [m,id_eq,mul_zero,add_zero] using (right_zero.const_mul d).const_add b
    have hd : Tendsto (fun t : ℝ ↦ (m t-b)/t) (𝓝[>] 0) (𝓝 d) := by
      refine tendsto_const_nhds.congr' ?_
      filter_upwards [right_pos] with t ht
      change d = (b+d*t-b)/t
      rw [add_sub_cancel_left, mul_div_cancel_right₀ _ ht.ne']
    have H := (moving_critical_skew_faces x hn hq' he hm (a := id) (v := fun _ ↦ q)
      (α := fun _ ↦ β) tendsto_id tendsto_const_nhds hd hr).1 β tendsto_const_nhds
    exact le_of_tendsto H (right_pos.mono fun t ht ↦ h (m t) t β ht)
  · intro ε hε
    let m := fun t : ℝ ↦ b-ε*t^2
    let α := fun t : ℝ ↦ ε*(t^2)⁻¹
    have hm : Tendsto m (𝓝[>] 0) (𝓝 b) := by
      simpa only [m,id_eq,zero_pow (by norm_num : (2 : ℕ) ≠ 0),mul_zero,sub_zero]
        using ((right_zero.pow 2).const_mul ε).const_sub b
    have hd : Tendsto (fun t : ℝ ↦ (m t-b)/t) (𝓝[>] 0) (𝓝 0) := by
      refine (show Tendsto (fun t : ℝ ↦ -ε*t) (𝓝[>] 0) (𝓝 0) by
        simpa only [mul_zero,id_eq] using right_zero.const_mul (-ε)).congr' ?_
      filter_upwards [right_pos] with t ht
      dsimp [m]
      field_simp [ht.ne']
      ring
    have hk : 0 < sqrt ((q+1)/q) := by positivity
    have hS : Tendsto (fun t : ℝ ↦ sqrt ((q+1)/(q+((m t-b)/t)^2))) (𝓝[>] 0)
        (𝓝 (sqrt ((q+1)/q))) := by
      have H := (tendsto_const_nhds (x := q+1)).div ((hd.pow 2).const_add q) (by positivity)
      simpa only [Pi.div_def,zero_pow (by norm_num : (2 : ℕ) ≠ 0),add_zero] using H.sqrt
    have hc : Tendsto (fun t : ℝ ↦ ((α t*(b-m t)*
        sqrt ((q+1)/(q*t^2+(b-m t)^2)) : ℝ) : EReal)) (𝓝[>] 0) (𝓝 ⊤) := by
      apply EReal.tendsto_coe_nhds_top_iff.mpr
      refine (tendsto_inv_nhdsGT_zero.atTop_mul_pos hk hS).congr' ?_
      filter_upwards [right_pos] with t ht
      change t⁻¹ * _ = α t * (b-m t) * _
      rw [mul_assoc, atom_arg_eq ht hq', ← mul_assoc]
      congr 1
      have hD : -((m t-b)/t) = ε*t := by dsimp [m]; field_simp [ht.ne']; ring
      rw [hD]
      dsimp [α]
      symm
      calc
        _ = ε^2 * ((t^2)⁻¹*t) := by ring
        _ = _ := by rw [hε,one_mul]; field_simp [ht.ne']
    have H := (moving_critical_skew_faces x hn hq' he hm (a := id) (v := fun _ ↦ q)
      tendsto_id tendsto_const_nhds hd hr).2 ε hε (orientation_tail ε hε) ⊤ hc
    have hLim : Tendsto (fun t ↦ likelihood x (m t) q (α t) t) (𝓝[>] 0)
        (𝓝 (exp (TDist.collapseLogValue x b q)*orientedWeight x b ε 2)) := by
      simpa [orientedWeight] using H
    exact le_of_tendsto hLim (right_pos.mono fun t ht ↦ h (m t) t (α t) ht)

private abbrev Param := ℝ × ℝ × ℝ × ℝ

private def fitted : Set Param := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2.1}

private noncomputable def value {n : ℕ} (x : Fin n → ℝ) (p : Param) : ℝ :=
  likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1

/-- The excluded heights in the all-free fitted skew Student model.
The one-sided faces use the sharp moving-contact factor. -/
def fittedFaces {n : ℕ} (x : Fin n → ℝ) : Set ℝ :=
  {z | (∃ b s β, 0 < s ∧ z = skewNormalHeight x b s β) ∨
    (∃ b s v ε, 0 < s ∧ 1 ≤ v ∧ ε^2 = 1 ∧
      z = TDist.likelihood x b v s * orientedWeight x b ε 2) ∨
    (∃ b s ε, 0 < s ∧ ε^2 = 1 ∧
      z = (∏ i, TDist.normalDensity b s (x i)) * orientedWeight x b ε 2) ∨
    (∃ b β d, TDist.exponent x b 1 = 0 ∧
      z = criticalBase x b 1 d 0 * atomSlantFactor x b 1 β d) ∨
    (∃ b ε, TDist.exponent x b 1 = 0 ∧ ε^2 = 1 ∧
      z = exp (TDist.collapseLogValue x b 1) * orientedWeight x b ε 2)}

private theorem fittedFaces_nonempty {n : ℕ} (x : Fin n → ℝ) :
    (fittedFaces x).Nonempty :=
  ⟨_, Or.inl ⟨0,1,0,zero_lt_one,rfl⟩⟩

private theorem upper_reaches_faces {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {B : ℝ}
    (h : B ∈ upperBounds (value x '' fitted)) : B ∈ upperBounds (fittedFaces x) := by
  have hv (b s v β : ℝ) (hs : 0 < s) (hv : 1 ≤ v) : likelihood x b v β s ≤ B :=
    show value x (b,s,v,β) ≤ B from h ⟨(b,s,v,β),⟨hs,hv⟩,rfl⟩
  rintro _ (⟨b,s,β,hs,rfl⟩ | ⟨b,s,q,ε,hs,hq,hε,rfl⟩ | ⟨b,s,ε,hs,hε,rfl⟩ |
    ⟨b,β,d,he,rfl⟩ | ⟨b,ε,he,hε,rfl⟩)
  · exact upper_reaches_skew_normal x hv b s β hs
  · exact (upper_reaches_oriented x hv b s ε hs hε).1 q hq
  · exact (upper_reaches_oriented x hv b s ε hs hε).2
  · exact (upper_reaches_atom x hn b 1 zero_lt_one he (fun b s β hs ↦ hv b s 1 β hs le_rfl)).1 β d
  · exact (upper_reaches_atom x hn b 1 zero_lt_one he (fun b s β hs ↦ hv b s 1 β hs le_rfl)).2 ε hε

private theorem weight_le {n : ℕ} (x : Fin n → ℝ) (b ε : ℝ) {c : ℝ}
    (hc : 0 ≤ c) (hc' : c ≤ 2) :
    0 ≤ orientedWeight x b ε c ∧ orientedWeight x b ε c ≤ orientedWeight x b ε 2 := by
  have h (i : Fin n) : 0 ≤ (if 0 < ε*(x i-b) then 2 else if x i = b then c else 0) := by
    split_ifs <;> positivity
  constructor
  · exact Finset.prod_nonneg fun i _ ↦ h i
  · apply Finset.prod_le_prod₀ (fun i _ ↦ h i)
    intro i _
    split_ifs <;> linarith

private theorem extended_factor_bounds (F : ℝ → ℝ) (h : ∀ z, 0 ≤ F z ∧ F z ≤ 2)
    (t : EReal) : (0 : ℝ) ≤ EReal.rec 0 F 2 t ∧ EReal.rec (0 : ℝ) F (2 : ℝ) t ≤ (2 : ℝ) := by
  cases t using EReal.rec with
  | bot => exact ⟨le_rfl,zero_le_two⟩
  | top => exact ⟨zero_le_two,le_rfl⟩
  | coe z => exact h z

private theorem cdf2_bounds (q z : ℝ) : 0 ≤ 2*studentCDF q z ∧ 2*studentCDF q z ≤ 2 :=
  ⟨mul_nonneg zero_le_two (cdf_bounds q z).1,
    mul_le_of_le_one_right zero_le_two (cdf_bounds q z).2⟩

private theorem phi2_bounds (z : ℝ) : 0 ≤ 2*SkewLSGaussian.Φ z ∧ 2*SkewLSGaussian.Φ z ≤ 2 :=
  ⟨mul_nonneg zero_le_two (SkewLSGaussian.cdf_pos z).le,
    mul_le_of_le_one_right zero_le_two (SkewLSGaussian.cdf_lt_one z).le⟩

private theorem slant_subseq (α : ℕ → ℝ) {t : EReal}
    (h : Tendsto (fun j ↦ (α j : EReal)) atTop (𝓝 t)) :
    (∃ β, Tendsto α atTop (𝓝 β)) ∨ ∃ ε : ℝ, ε^2 = 1 ∧
      Tendsto (fun j ↦ ε*α j) atTop atTop := by
  cases t using EReal.rec with
  | bot =>
    refine Or.inr ⟨-1,by norm_num,?_⟩
    simpa only [neg_one_mul, Function.comp_def] using tendsto_neg_atBot_atTop.comp (EReal.tendsto_coe_nhds_bot_iff.mp h)
  | coe β => exact Or.inl ⟨β,EReal.tendsto_coe.mp h⟩
  | top => exact Or.inr ⟨1,by norm_num,by simpa using EReal.tendsto_coe_nhds_top_iff.mp h⟩

private theorem strict_exponent {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 1 < q) (he : 0 ≤ TDist.exponent x b 1) :
    0 < TDist.exponent x b q := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have h : 0 < (n : ℝ) - TDist.multiplicity x b := by
    unfold TDist.exponent at he
    linarith
  have hp := mul_pos (sub_pos.mpr hq) h
  unfold TDist.exponent at he ⊢
  nlinarith only [hp,he]

private theorem vanish_under_base {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ}
    (h : Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l (𝓝 0)) :
    Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l (𝓝 0) := by
  have H := h.const_mul ((2 : ℝ)^n)
  rw [mul_zero] at H
  exact squeeze_zero' (Eventually.of_forall fun j ↦ (dominated x (m j) (v j) (α j) (a j)).1)
    (Eventually.of_forall fun j ↦ (dominated x (m j) (v j) (α j) (a j)).2) H

private theorem orient_side_bounded {n : ℕ} (x : Fin n → ℝ) {m a v α : ℕ → ℝ}
    {b s ε A B : ℝ} (hB : B ∈ upperBounds (fittedFaces x)) (hAB : B < A)
    (hval : ∀ j, A ≤ likelihood x (m j) (v j) (α j) (a j))
    (hs : 0 < s) (hm : Tendsto m atTop (𝓝 b)) (ha : Tendsto a atTop (𝓝 s))
    (hε : ε^2 = 1) (hεα : Tendsto (fun j ↦ ε*α j) atTop atTop)
    (hv : (∃ q ≥ 1, Tendsto v atTop (𝓝 q)) ∨ Tendsto v atTop atTop) : False := by
  let z : ℕ → EReal := fun j ↦ ↑(α j*(b-m j)*sqrt ((v j+1)/(v j*(a j)^2+(b-m j)^2)))
  obtain ⟨c,φ,hφ,hz⟩ := CompactSpace.tendsto_subseq z
  have hT := hφ.tendsto_atTop
  have h := moving_oriented_faces x hs (hm.comp hT) (ha.comp hT) hε (hεα.comp hT) hz
  rcases hv with ⟨q,hq,hqv⟩ | hqv
  · have H := h.1 q (zero_lt_one.trans_le hq) (hqv.comp hT)
    have hc := extended_factor_bounds (fun t ↦ 2*studentCDF (q+1) t) (cdf2_bounds _) c
    have hu := (weight_le x b ε hc.1 hc.2).2
    have hn := mul_le_mul_of_nonneg_left hu (exp_pos (TDist.logLik x b q s)).le
    exact hAB.not_ge ((ge_of_tendsto' H (fun j ↦ hval (φ j))).trans
      (hn.trans (hB (Or.inr (Or.inl ⟨b,s,q,ε,hs,hq,hε,rfl⟩)))))
  · have H := h.2 (hqv.comp hT)
    have hc := extended_factor_bounds (fun t ↦ 2*SkewLSGaussian.Φ t) phi2_bounds c
    have hu := (weight_le x b ε hc.1 hc.2).2
    have hnon : 0 ≤ ∏ i, TDist.normalDensity b s (x i) :=
      Finset.prod_nonneg fun _ _ ↦ (exp_pos _).le
    have hn := mul_le_mul_of_nonneg_left hu hnon
    exact hAB.not_ge ((ge_of_tendsto' H (fun j ↦ hval (φ j))).trans
      (hn.trans (hB (Or.inr (Or.inr (Or.inl ⟨b,s,ε,hs,hε,rfl⟩))))))

private theorem critical_side_bounded {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {m a v α : ℕ → ℝ} {b A B : ℝ} (hB : B ∈ upperBounds (fittedFaces x)) (hAB : B < A)
    (hA : 0 < A) (hval : ∀ j, A ≤ likelihood x (m j) (v j) (α j) (a j))
    (he : TDist.exponent x b 1 = 0) (hm : Tendsto m atTop (𝓝 b))
    (ha : Tendsto a atTop (𝓝[>] 0)) (hv : Tendsto v atTop (𝓝 1))
    (hv₁ : ∀ᶠ j in atTop, 1 ≤ v j)
    (hα : (∃ β, Tendsto α atTop (𝓝 β)) ∨ ∃ ε : ℝ, ε^2 = 1 ∧
      Tendsto (fun j ↦ ε*α j) atTop atTop) : False := by
  let δ := fun j ↦ (m j-b)/a j
  let ρ := fun j ↦ (v j-1)*log (a j)
  let p : ℕ → EReal × EReal := fun j ↦ (δ j,ρ j)
  obtain ⟨w,ψ,hψ,hw⟩ := CompactSpace.tendsto_subseq p
  have ht := hψ.tendsto_atTop
  have hd : Tendsto (fun j ↦ (δ (ψ j) : EReal)) atTop (𝓝 w.1) := (continuous_fst.tendsto w).comp hw
  have hr : Tendsto (fun j ↦ (ρ (ψ j) : EReal)) atTop (𝓝 w.2) := (continuous_snd.tendsto w).comp hw
  have H := Student2MovingCollapse.moving_critical_atom_base x hn b zero_lt_one he
    (hm.comp ht) (ha.comp ht) (hv.comp ht)
  have hZero (h : Tendsto (fun j ↦ TDist.likelihood x (m (ψ j)) (v (ψ j)) (a (ψ j))) atTop (𝓝 0)) :
      False := hA.not_ge (ge_of_tendsto' (vanish_under_base (α := α ∘ ψ) x h) (fun j ↦ hval (ψ j)))
  have hρ : ∀ᶠ j in atTop, ρ (ψ j) ≤ 0 := ht.eventually (by
    filter_upwards [hv₁,(tendsto_log_nhdsGT_zero.comp ha).eventually_lt_atBot 0] with j hj hj'
    exact mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hj) hj'.le)
  generalize w.1 = wd at hd
  generalize w.2 = wr at hr
  cases wd using EReal.rec with
  | bot =>
    have hd' := (tendsto_neg_atBot_atTop.comp (EReal.tendsto_coe_nhds_bot_iff.mp hd)).atTop_pow₀
      (by norm_num : (2 : ℕ) ≠ 0)
    exact hZero (H.2.2 (ht.eventually hv₁) (by simpa [Function.comp_def,δ] using hd'))
  | top =>
    exact hZero (H.2.2 (ht.eventually hv₁)
      ((EReal.tendsto_coe_nhds_top_iff.mp hd).atTop_pow₀ (by norm_num)))
  | coe d =>
    have hd' := EReal.tendsto_coe.mp hd
    cases wr using EReal.rec with
    | bot => exact hZero (H.2.1 (EReal.tendsto_coe_nhds_bot_iff.mp hr))
    | top =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_top_iff.mp hr).eventually_gt_atTop 0).and hρ).exists
      exact hj.not_ge hj'
    | coe r =>
      have hr' := EReal.tendsto_coe.mp hr
      have hr₀ : r ≤ 0 := le_of_tendsto hr' hρ
      have hcount : (TDist.multiplicity x b : ℝ) ≤ n := by
        exact_mod_cast (Finset.card_filter_le (s := Finset.univ) (fun i ↦ x i = b)).trans_eq
          (by simp : (Finset.univ : Finset (Fin n)).card = n)
      have hpow : ((n : ℝ) - TDist.multiplicity x b) * r ≤ 0 :=
        mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hcount) hr₀
      have hK : criticalBase x b 1 d r ≤ criticalBase x b 1 d 0 := by
        unfold criticalBase
        apply exp_le_exp.mpr
        linarith
      have hK' : criticalBase x b 1 d r ≤ exp (TDist.collapseLogValue x b 1) := by
        unfold criticalBase
        apply exp_le_exp.mpr
        have hlog : 0 ≤ log (1+d^2/(1 : ℝ)) := log_nonneg (le_add_of_nonneg_right (by positivity))
        nlinarith only [hpow,mul_nonneg (show 0 ≤ (TDist.multiplicity x b : ℝ) by positivity) hlog]
      rcases hα with ⟨β,hα⟩ | ⟨ε,hε,hα⟩
      · have H := (moving_critical_skew_faces x hn zero_lt_one he (hm.comp ht) (ha.comp ht)
          (hv.comp ht) hd' hr').1 β (hα.comp ht)
        have hnon : 0 ≤ atomSlantFactor x b 1 β d :=
          Finset.prod_nonneg fun _ _ ↦ (cdf2_bounds _ _).1
        have h := mul_le_mul_of_nonneg_right hK hnon
        exact hAB.not_ge ((ge_of_tendsto' H (fun j ↦ hval (ψ j))).trans
          (h.trans (hB (Or.inr (Or.inr (Or.inr (Or.inl ⟨b,β,d,he,rfl⟩)))))))
      · let z : ℕ → EReal := fun j ↦ ↑(α (ψ j)*(b-m (ψ j))*
          sqrt ((v (ψ j)+1)/(v (ψ j)*(a (ψ j))^2+(b-m (ψ j))^2)))
        obtain ⟨c,φ,hφ,hc⟩ := CompactSpace.tendsto_subseq z
        have ht' := hφ.tendsto_atTop
        have H := (moving_critical_skew_faces x hn zero_lt_one he (hm.comp (ht.comp ht'))
          (ha.comp (ht.comp ht')) (hv.comp (ht.comp ht')) (hd'.comp ht') (hr'.comp ht')).2
          ε hε (hα.comp (ht.comp ht')) c hc
        have hc' := extended_factor_bounds (fun t ↦ 2*studentCDF 2 t) (cdf2_bounds 2) c
        have hW := weight_le x b ε hc'.1 hc'.2
        have h := mul_le_mul hK' hW.2 hW.1 (exp_pos _).le
        have hL : A ≤ criticalBase x b 1 d r *
            orientedWeight x b ε (EReal.rec 0 (fun t ↦ 2*studentCDF 2 t) 2 c) := by
          simpa only [criticalBase,orientedWeight,one_add_one_eq_two] using
            ge_of_tendsto' H (fun j ↦ hval (ψ (φ j)))
        exact hAB.not_ge (hL.trans
          (h.trans (hB (Or.inr (Or.inr (Or.inr (Or.inr ⟨b,ε,he,hε,rfl⟩)))))))

private theorem all_free_superlevel_compact {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (he : ∀ b, 0 ≤ TDist.exponent x b 1) {A B : ℝ}
    (hB : B ∈ upperBounds (fittedFaces x)) (hAB : B < A) (hA : 0 < A) :
    IsCompact {p ∈ fitted | A ≤ value x p} := by
  apply IsSeqCompact.isCompact
  intro p hp
  let u : ℕ → EReal × EReal × EReal × EReal := fun j ↦
    ((p j).1,(p j).2.1,(p j).2.2.1,(p j).2.2.2)
  obtain ⟨z,φ,hφ,hu⟩ := CompactSpace.tendsto_subseq u
  let m := fun j ↦ (p (φ j)).1
  let a := fun j ↦ (p (φ j)).2.1
  let v := fun j ↦ (p (φ j)).2.2.1
  let α := fun j ↦ (p (φ j)).2.2.2
  have hm : Tendsto (fun j ↦ (m j : EReal)) atTop (𝓝 z.1) :=
    (continuous_fst.tendsto z).comp hu
  have ha : Tendsto (fun j ↦ (a j : EReal)) atTop (𝓝 z.2.1) :=
    ((continuous_fst.comp continuous_snd).tendsto z).comp hu
  have hv : Tendsto (fun j ↦ (v j : EReal)) atTop (𝓝 z.2.2.1) :=
    ((continuous_fst.comp (continuous_snd.comp continuous_snd)).tendsto z).comp hu
  have hα : Tendsto (fun j ↦ (α j : EReal)) atTop (𝓝 z.2.2.2) :=
    ((continuous_snd.comp (continuous_snd.comp continuous_snd)).tendsto z).comp hu
  have ha₀ : ∀ᶠ j in atTop, 0 < a j := Eventually.of_forall fun j ↦ (hp (φ j)).1.1
  have hv₁ : ∀ᶠ j in atTop, 1 ≤ v j := Eventually.of_forall fun j ↦ (hp (φ j)).1.2
  have hV (j : ℕ) : A ≤ likelihood x (m j) (v j) (α j) (a j) := (hp (φ j)).2
  have hZero (h : Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) atTop (𝓝 0)) :
      False := hA.not_ge (ge_of_tendsto' (vanish_under_base (α := α) x h) hV)
  have hRemote (h : Tendsto a atTop atTop ∨ Tendsto m atTop (cocompact ℝ)) : False :=
    hZero (Student2Normal.moving_shape_remote_ends x hn ha₀ hv₁ h)
  have hA' := slant_subseq α hα
  have hV' : (∃ q ≥ 1, Tendsto v atTop (𝓝 q)) ∨ Tendsto v atTop atTop := by
    generalize z.2.2.1 = q at hv
    cases q using EReal.rec with
    | bot =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_bot_iff.mp hv).eventually_lt_atBot 1).and hv₁).exists
      exact (hj.not_ge hj').elim
    | top => exact Or.inr (EReal.tendsto_coe_nhds_top_iff.mp hv)
    | coe q =>
      have ht := EReal.tendsto_coe.mp hv
      exact Or.inl ⟨q,ge_of_tendsto ht hv₁,ht⟩
  generalize z.1 = zm at hm
  generalize z.2.1 = za at ha
  cases zm using EReal.rec with
  | bot =>
    exact (hRemote (.inr ((EReal.tendsto_coe_nhds_bot_iff.mp hm).mono_right
      (by rw [cocompact_eq_atBot_atTop]; exact le_sup_left)))).elim
  | top =>
    exact (hRemote (.inr ((EReal.tendsto_coe_nhds_top_iff.mp hm).mono_right
      (by rw [cocompact_eq_atBot_atTop]; exact le_sup_right)))).elim
  | coe b =>
    have hm' := EReal.tendsto_coe.mp hm
    cases za using EReal.rec with
    | bot =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_bot_iff.mp ha).eventually_lt_atBot 0).and ha₀).exists
      exact (not_lt_of_ge (show 0 ≤ a j from hj'.le) hj).elim
    | top => exact (hRemote (.inl (EReal.tendsto_coe_nhds_top_iff.mp ha))).elim
    | coe s =>
      have hs := EReal.tendsto_coe.mp ha
      have hs' := ge_of_tendsto hs (ha₀.mono fun _ hj ↦ hj.le)
      rcases hs'.eq_or_lt with hzero | hpos
      · subst s
        have hscale : Tendsto a atTop (𝓝[>] 0) := tendsto_nhdsWithin_iff.mpr ⟨hs,ha₀⟩
        rcases hV' with ⟨q,hq,hq'⟩ | hq
        · have vanish (hp : 0 < TDist.exponent x b q) : False :=
            hZero ((Student2MovingCollapse.moving_shape_collapse x b (zero_lt_one.trans_le hq)
              hm' hscale hq').1 hp)
          rcases hq.eq_or_lt with hEq | hlt
          · subst q
            rcases (he b).eq_or_lt with hE | hE
            · exact (critical_side_bounded x hn hB hAB hA hV hE.symm hm' hscale hq' hv₁ hA').elim
            · exact (vanish hE).elim
          · exact (vanish (strict_exponent x hn b hlt (he b))).elim
        · have hnon : ∃ i, x i ≠ b := by
            by_contra! h
            have H := he b
            rw [TDist.exponent_all_tied x b 1 h] at H
            have hn' : (0 : ℝ) < n := by exact_mod_cast hn
            linarith
          exact (hZero (Student2Normal.zero_scale_normal_corner x hm' hscale hq hnon)).elim
      · rcases hA' with ⟨β,hβ⟩ | ⟨ε,hε,hεα⟩
        · rcases hV' with ⟨q,hq,hq'⟩ | hq
          · have h := moving_finite_skew_face x hpos (zero_lt_one.trans_le hq) hm' hs hq' hβ
            exact ⟨(b,s,q,β), ⟨⟨hpos,hq⟩,ge_of_tendsto' h hV⟩, φ,hφ,
              hm'.prodMk_nhds (hs.prodMk_nhds (hq'.prodMk_nhds hβ))⟩
          · have ht := moving_skew_normal_face x hpos hm' hs hq hβ
            exact (hAB.not_ge ((ge_of_tendsto' ht hV).trans
              (hB (Or.inl ⟨b,s,β,hpos,rfl⟩)))).elim
        · exact (orient_side_bounded x hB hAB hV hpos hm' hs hε hεα hV').elim

private theorem fitted_continuous {n : ℕ} (x : Fin n → ℝ) : ContinuousOn (value x) fitted := by
  intro p hp
  exact (moving_finite_skew_face x hp.1 (zero_lt_one.trans_le hp.2)
    (continuous_fst.tendsto p) ((continuous_fst.comp continuous_snd).tendsto p)
    ((continuous_fst.comp (continuous_snd.comp continuous_snd)).tendsto p)
    ((continuous_snd.comp (continuous_snd.comp continuous_snd)).tendsto p)).mono_left
      nhdsWithin_le_nhds

private theorem candidate_reaching_faces {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (he : ∀ b, 0 ≤ TDist.exponent x b 1)
    {p : Param} (hp : p ∈ fitted) (hf : sSup (fittedFaces x) ≤ value x p)
    (hF : BddAbove (fittedFaces x)) :
    ∃ q ∈ fitted, IsMaxOn (value x) fitted q := by
  by_cases hm : IsMaxOn (value x) fitted p
  · exact ⟨p,hp,hm⟩
  · rw [IsMaxOn, IsMaxFilter, eventually_principal] at hm
    push Not at hm
    obtain ⟨w,hw,hw'⟩ := hm
    let K := {q ∈ fitted | value x w ≤ value x q}
    have hK : IsCompact K := all_free_superlevel_compact x hn he
      (isLUB_csSup (fittedFaces_nonempty x) hF).1 (hf.trans_lt hw')
      (show 0 < value x w from mul_pos (exp_pos _)
        (Finset.prod_pos fun i _ ↦ mul_pos two_pos
          (cdf_pos (by have := hw.2; linarith : 0 < w.2.2.1+1) _)))
    obtain ⟨q,hq,hqm⟩ := hK.exists_isMaxOn ⟨w,hw,le_rfl⟩
      ((fitted_continuous x).mono fun _ h ↦ h.1)
    exact ⟨q,hq.1,fun z hz ↦ if h : value x w ≤ value x z then hqm ⟨hz,h⟩
      else (le_of_not_ge h).trans hq.2⟩

/-- The all-free fitted skew Student decision. The only eligible
excluded heights are the skew-normal, the two oriented half families,
and the finite and infinite slant critical atom layers. A strict
majority gives divergence. Otherwise a stationary or df-floor candidate
must reach the excluded supremum, with equality allowed. -/
theorem fitted_all_free_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2.1}
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let B := sSup (fittedFaces x)
    ((¬ BddAbove (L '' D)) ↔ ∃ b, n < 2 * TDist.multiplicity x b) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 2 * TDist.multiplicity x b ≤ n) ∧ ∃ p ∈ D,
      ((1 < p.2.2.1 ∧ fderiv ℝ (fun q ↦ log (L q)) p = 0) ∨
        (p.2.2.1 = 1 ∧ ∀ b a β, 0 < a → likelihood x b 1 β a ≤ L p)) ∧ B ≤ L p) ∧
    ((∀ b, 2 * TDist.multiplicity x b ≤ n) →
      IsLUB (fittedFaces x) B ∧ (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)) := by
  dsimp only
  let B := sSup (fittedFaces x)
  let L := value x
  let D := fitted
  change ((¬ BddAbove (L '' D)) ↔ ∃ b, n < 2 * TDist.multiplicity x b) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 2 * TDist.multiplicity x b ≤ n) ∧ ∃ p ∈ D,
      ((1 < p.2.2.1 ∧ fderiv ℝ (fun q ↦ log (L q)) p = 0) ∨
        (p.2.2.1 = 1 ∧ ∀ b a β, 0 < a → likelihood x b 1 β a ≤ L p)) ∧ B ≤ L p) ∧
    ((∀ b, 2 * TDist.multiplicity x b ≤ n) →
      IsLUB (fittedFaces x) B ∧ (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
  have hub := (fitted_majority_and_vanishing_ends.{0,0,0} x hn).1
  have hE (h : ∀ b, 2 * TDist.multiplicity x b ≤ n) (b : ℝ) :
      0 ≤ TDist.exponent x b 1 := by
    have H : (2 : ℝ) * TDist.multiplicity x b ≤ n := by exact_mod_cast h b
    unfold TDist.exponent
    linarith
  have hF (h : ∀ b, 2 * TDist.multiplicity x b ≤ n) : BddAbove (fittedFaces x) := by
    have ht : BddAbove (L '' D) := by
      by_contra hh
      obtain ⟨b,hb⟩ := hub.mp hh
      exact hb.not_ge (h b)
    obtain ⟨K,hK⟩ := ht
    exact ⟨K, upper_reaches_faces x hn hK⟩
  have hreach {A : ℝ} (h : A ∈ upperBounds (L '' D)) : B ≤ A :=
    csSup_le (fittedFaces_nonempty x) (upper_reaches_faces x hn h)
  have hpos (p : Param) (hp : p ∈ D) : 0 < L p := by
    have hv : 1 ≤ p.2.2.1 := hp.2
    exact mul_pos (exp_pos _) (Finset.prod_pos fun _ _ ↦ mul_pos two_pos
      (cdf_pos (by linarith : 0 < p.2.2.1+1) _))
  refine ⟨hub, ⟨?_,?_⟩, ?_⟩
  · rintro ⟨p,hp,hm⟩
    have hmax : L p ∈ upperBounds (L '' D) := by
      rintro _ ⟨q,hq,rfl⟩
      exact hm hq
    have hM (b : ℝ) : 2 * TDist.multiplicity x b ≤ n := by
      by_contra! h
      exact (hub.mpr ⟨b,h⟩) ⟨L p,hmax⟩
    refine ⟨hM,p,hp,?_,hreach hmax⟩
    rcases (show 1 ≤ p.2.2.1 from hp.2).eq_or_lt with h | h
    · refine Or.inr ⟨h.symm,fun b a β ha ↦ ?_⟩
      change L (b,a,1,β) ≤ L p
      exact hm (show (b,a,1,β) ∈ D from ⟨ha,le_rfl⟩)
    · refine Or.inl ⟨h,?_⟩
      let U : Set Param := {p | 0 < p.2.1 ∧ 1 < p.2.2.1}
      have hOpen : IsOpen U :=
        (isOpen_lt continuous_const (continuous_fst.comp continuous_snd)).inter
          (isOpen_lt continuous_const (continuous_fst.comp (continuous_snd.comp continuous_snd)))
      have hLog : IsMaxOn (fun q ↦ log (L q)) U p := by
        intro q hq
        have hq' : q ∈ D := ⟨hq.1,(show 1 < q.2.2.1 from hq.2).le⟩
        exact log_le_log (hpos q hq') (hm hq')
      exact (hLog.isLocalMax (hOpen.mem_nhds ⟨hp.1,h⟩)).fderiv_eq_zero
  · rintro ⟨h,p,hp,_,hB⟩
    exact candidate_reaching_faces x hn (hE h) hp hB (hF h)
  · intro h
    have hfaces := isLUB_csSup (fittedFaces_nonempty x) (hF h)
    have hN : 0 ≤ B := ((mul_nonneg
      (Finset.prod_nonneg fun i _ ↦ (exp_pos (-(log (2*π)/2) - log 1 - (x i-0)^2/1^2/2)).le)
      (Finset.prod_nonneg fun _ _ ↦ (phi2_bounds _).1)) :
        0 ≤ skewNormalHeight x 0 1 0).trans (hfaces.1 (Or.inl ⟨0,1,0,zero_lt_one,rfl⟩))
    refine ⟨hfaces,fun A hA ↦ all_free_superlevel_compact x hn (hE h) hfaces.1 hA (hN.trans_lt hA),⟨?_,?_⟩⟩
    · intro hNo
      have hStrict (p : Param) (hp : p ∈ D) : L p < B := lt_of_not_ge fun hB ↦
        hNo (candidate_reaching_faces x hn (hE h) hp hB (hF h))
      exact ⟨⟨by rintro _ ⟨p,hp,rfl⟩; exact (hStrict p hp).le, fun A hA ↦ hreach hA⟩,hStrict⟩
    · rintro ⟨hL,hlt⟩ ⟨p,hp,hm⟩
      exact (hlt p hp).not_ge (hL.2 (show L p ∈ upperBounds (L '' D) by
        rintro _ ⟨q,hq,rfl⟩
        exact hm hq))

/-- Location chart with an optional held value. -/
def locations (μ : Option ℝ) : Set ℝ := (μ.map singleton).getD univ

/-- Shape chart with the fitted floor if no shape is held. -/
def shapes (ν : Option ℝ) : Set ℝ := (ν.map singleton).getD (Ici 1)

private theorem locations_closed (μ : Option ℝ) : IsClosed (locations μ) := by
  cases μ
  · exact isClosed_univ
  · exact isClosed_singleton

private theorem shapes_closed (ν : Option ℝ) : IsClosed (shapes ν) := by
  cases ν
  · exact isClosed_Ici
  · exact isClosed_singleton

private theorem shape_floor (ν : Option ℝ) {v : ℝ} (h : v ∈ shapes ν) : ν.getD 1 ≤ v := by
  cases ν with
  | none => exact h
  | some q =>
    have he : v = q := h
    exact le_of_eq he.symm

private theorem floor_feasible (ν : Option ℝ) : ν.getD 1 ∈ shapes ν := by
  cases ν
  · exact le_rfl
  · exact mem_singleton _

/-- The location, ordinary scale and df chart under finite held slant. -/
def fixedSlantDomain (μ ν : Option ℝ) : Set (ℝ × ℝ × ℝ) :=
  {p | p.1 ∈ locations μ ∧ 0 < p.2.1 ∧ p.2.2 ∈ shapes ν}

/-- Excluded heights for the held-slant mask, with optional held location
and df. Fixing location forces displacement zero in the critical layer. -/
def fixedSlantFaces {n : ℕ} (x : Fin n → ℝ) (β : ℝ) (μ ν : Option ℝ) : Set ℝ :=
  {z | z = 0 ∨
    (ν = none ∧ ∃ b ∈ locations μ, ∃ a > 0, z = skewNormalHeight x b a β) ∨
    (∃ b ∈ locations μ, ∃ d, (μ = none ∨ d = 0) ∧ TDist.exponent x b (ν.getD 1) = 0 ∧
      z = criticalBase x b (ν.getD 1) d 0 * atomSlantFactor x b (ν.getD 1) β d)}

private theorem fixedSlant_upper_faces {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) (μ ν : Option ℝ) (hq : 0 < ν.getD 1) {B : ℝ}
    (h : ∀ b a v, b ∈ locations μ → 0 < a → v ∈ shapes ν → likelihood x b v β a ≤ B) :
    B ∈ upperBounds (fixedSlantFaces x β μ ν) := by
  rintro _ (rfl | ⟨rfl,b,hb,a,ha,rfl⟩ | ⟨b,hb,d,hd,he,rfl⟩)
  · let b := μ.getD 0
    have hb : b ∈ locations μ := by cases μ <;> simp [b,locations]
    exact (dominated x b (ν.getD 1) β 1).1.trans (h b 1 _ hb zero_lt_one (floor_feasible ν))
  · apply le_of_tendsto (moving_skew_normal_face x ha (l := atTop) (v := id)
      (m := fun _ : ℝ ↦ b) (a := fun _ ↦ a) (α := fun _ ↦ β)
      tendsto_const_nhds tendsto_const_nhds tendsto_id tendsto_const_nhds)
    exact (eventually_ge_atTop (1 : ℝ)).mono fun v hv ↦ h b a v hb ha hv
  · let q := ν.getD 1
    let m := fun t : ℝ ↦ b+d*t
    have hm : Tendsto m (𝓝[>] 0) (𝓝 b) := by
      simpa only [m,id_eq,mul_zero,add_zero] using (right_zero.const_mul d).const_add b
    have hd' : Tendsto (fun t ↦ (m t-b)/t) (𝓝[>] 0) (𝓝 d) := by
      refine tendsto_const_nhds.congr' ?_
      filter_upwards [right_pos] with t ht
      change d = (b+d*t-b)/t
      rw [add_sub_cancel_left, mul_div_cancel_right₀ _ ht.ne']
    have hr : Tendsto (fun t : ℝ ↦ (q-q)*log t) (𝓝[>] 0) (𝓝 0) := by simp
    have H := (moving_critical_skew_faces x hn hq he hm (a := id) (v := fun _ ↦ q)
      (α := fun _ ↦ β) tendsto_id tendsto_const_nhds hd' hr).1 β tendsto_const_nhds
    apply le_of_tendsto H
    filter_upwards [right_pos] with t ht
    have hmt : m t ∈ locations μ := by
      rcases hd with rfl | rfl
      · exact mem_univ _
      · simpa [m] using hb
    exact h (m t) t q hmt ht (floor_feasible ν)

private theorem fixed_slant_critical_le {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) (μ ν : Option ℝ) {m a v : ℕ → ℝ} {b A B : ℝ}
    (hq : 0 < ν.getD 1) (he : TDist.exponent x b (ν.getD 1) = 0)
    (hB : B ∈ upperBounds (fixedSlantFaces x β μ ν)) (hAB : B < A)
    (hA : 0 < A) (hval : ∀ j, A ≤ likelihood x (m j) (v j) β (a j))
    (hb : b ∈ locations μ) (hmOK : ∀ j, m j ∈ locations μ)
    (hm : Tendsto m atTop (𝓝 b)) (ha : Tendsto a atTop (𝓝[>] 0))
    (hv : Tendsto v atTop (𝓝 (ν.getD 1))) (hv₁ : ∀ᶠ j in atTop, ν.getD 1 ≤ v j) : False := by
  let q := ν.getD 1
  let δ := fun j ↦ (m j-b)/a j
  let ρ := fun j ↦ (v j-q)*log (a j)
  let p : ℕ → EReal × EReal := fun j ↦ (δ j,ρ j)
  obtain ⟨w,ψ,hψ,hw⟩ := CompactSpace.tendsto_subseq p
  have ht := hψ.tendsto_atTop
  have hd : Tendsto (fun j ↦ (δ (ψ j) : EReal)) atTop (𝓝 w.1) := (continuous_fst.tendsto w).comp hw
  have hr : Tendsto (fun j ↦ (ρ (ψ j) : EReal)) atTop (𝓝 w.2) := (continuous_snd.tendsto w).comp hw
  have H := Student2MovingCollapse.moving_critical_atom_base x hn b hq he
    (hm.comp ht) (ha.comp ht) (hv.comp ht)
  have hZero (h : Tendsto (fun j ↦ TDist.likelihood x (m (ψ j)) (v (ψ j)) (a (ψ j))) atTop (𝓝 0)) :
      False := hA.not_ge (ge_of_tendsto' (vanish_under_base (α := fun _ ↦ β) x h) (fun j ↦ hval (ψ j)))
  have hρ : ∀ᶠ j in atTop, ρ (ψ j) ≤ 0 := ht.eventually (by
    filter_upwards [hv₁,(tendsto_log_nhdsGT_zero.comp ha).eventually_lt_atBot 0] with j hj hj'
    exact mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hj) hj'.le)
  generalize w.1 = wd at hd
  generalize w.2 = wr at hr
  cases wd using EReal.rec with
  | bot =>
    have hd' := (tendsto_neg_atBot_atTop.comp (EReal.tendsto_coe_nhds_bot_iff.mp hd)).atTop_pow₀
      (by norm_num : (2 : ℕ) ≠ 0)
    exact hZero (H.2.2 (ht.eventually hv₁) (by simpa [Function.comp_def,δ] using hd'))
  | top =>
    exact hZero (H.2.2 (ht.eventually hv₁)
      ((EReal.tendsto_coe_nhds_top_iff.mp hd).atTop_pow₀ (by norm_num)))
  | coe d =>
    have hd' := EReal.tendsto_coe.mp hd
    have hd0 : μ = none ∨ d = 0 := by
      cases μ with
      | none => exact Or.inl rfl
      | some m₀ =>
        refine Or.inr (tendsto_nhds_unique hd' ?_)
        have hb' : b = m₀ := hb
        have H (j : ℕ) : m j = m₀ := hmOK j
        simp [δ,H,hb']
    cases wr using EReal.rec with
    | bot => exact hZero (H.2.1 (EReal.tendsto_coe_nhds_bot_iff.mp hr))
    | top =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_top_iff.mp hr).eventually_gt_atTop 0).and hρ).exists
      exact hj.not_ge hj'
    | coe r =>
      have hr' := EReal.tendsto_coe.mp hr
      have hr₀ : r ≤ 0 := le_of_tendsto hr' hρ
      have hcount : (TDist.multiplicity x b : ℝ) ≤ n := by
        exact_mod_cast (Finset.card_filter_le (s := Finset.univ) (fun i ↦ x i = b)).trans_eq
          (by simp : (Finset.univ : Finset (Fin n)).card = n)
      have hpow : ((n : ℝ) - TDist.multiplicity x b) * r ≤ 0 :=
        mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hcount) hr₀
      have hK : criticalBase x b q d r ≤ criticalBase x b q d 0 := by
        unfold criticalBase
        apply exp_le_exp.mpr
        linarith
      have H := (moving_critical_skew_faces x hn hq he (hm.comp ht) (ha.comp ht)
        (hv.comp ht) hd' hr').1 β tendsto_const_nhds
      have hnon : 0 ≤ atomSlantFactor x b q β d :=
        Finset.prod_nonneg fun _ _ ↦ (cdf2_bounds _ _).1
      have h := mul_le_mul_of_nonneg_right hK hnon
      exact hAB.not_ge ((ge_of_tendsto' H (fun j ↦ hval (ψ j))).trans
        (h.trans (hB (Or.inr (Or.inr ⟨b,hb,d,hd0,he,rfl⟩)))))

private theorem held_slant_superlevel {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) (μ ν : Option ℝ) (hq₀ : 0 < ν.getD 1)
    (he : ∀ b ∈ locations μ, 0 ≤ TDist.exponent x b (ν.getD 1)) {A B : ℝ}
    (hB : B ∈ upperBounds (fixedSlantFaces x β μ ν)) (hAB : B < A) (hA : 0 < A) :
    IsCompact {p ∈ fixedSlantDomain μ ν | A ≤ likelihood x p.1 p.2.2 β p.2.1} := by
  apply IsSeqCompact.isCompact
  intro p hp
  let u : ℕ → EReal × EReal × EReal := fun j ↦ ((p j).1,(p j).2.1,(p j).2.2)
  obtain ⟨z,φ,hφ,hu⟩ := CompactSpace.tendsto_subseq u
  let m := fun j ↦ (p (φ j)).1
  let a := fun j ↦ (p (φ j)).2.1
  let v := fun j ↦ (p (φ j)).2.2
  have hm : Tendsto (fun j ↦ (m j : EReal)) atTop (𝓝 z.1) :=
    (continuous_fst.tendsto z).comp hu
  have ha : Tendsto (fun j ↦ (a j : EReal)) atTop (𝓝 z.2.1) :=
    ((continuous_fst.comp continuous_snd).tendsto z).comp hu
  have hv : Tendsto (fun j ↦ (v j : EReal)) atTop (𝓝 z.2.2) :=
    ((continuous_snd.comp continuous_snd).tendsto z).comp hu
  have hmOK (j : ℕ) : m j ∈ locations μ := (hp (φ j)).1.1
  have hvOK (j : ℕ) : v j ∈ shapes ν := (hp (φ j)).1.2.2
  have ha₀ : ∀ᶠ j in atTop, 0 < a j := Eventually.of_forall fun j ↦ (hp (φ j)).1.2.1
  have hv₀ : ∀ᶠ j in atTop, ν.getD 1 ≤ v j := Eventually.of_forall fun j ↦ shape_floor ν (hvOK j)
  have hV (j : ℕ) : A ≤ likelihood x (m j) (v j) β (a j) := (hp (φ j)).2
  have hZero (h : Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) atTop (𝓝 0)) :
      False := hA.not_ge (ge_of_tendsto' (vanish_under_base (α := fun _ ↦ β) x h) hV)
  have hRemote (h : Tendsto a atTop atTop ∨ Tendsto m atTop (cocompact ℝ)) : False := by
    apply hZero
    cases ν with
    | none => exact Student2Normal.moving_shape_remote_ends x hn ha₀ hv₀ h
    | some q =>
      have hvq (j : ℕ) : v j = q := hvOK j
      refine (StudentFaces.lst_fixed_df_remote_ends x hn (show 0 < q from hq₀) ha₀ h).congr ?_
      intro j
      simp [hvq]
  have hV' : (∃ q ∈ shapes ν, Tendsto v atTop (𝓝 q)) ∨
      (ν = none ∧ Tendsto v atTop atTop) := by
    generalize z.2.2 = q at hv
    cases q using EReal.rec with
    | bot =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_bot_iff.mp hv).eventually_lt_atBot
        (ν.getD 1)).and hv₀).exists
      exact (hj.not_ge hj').elim
    | top =>
      have ht := EReal.tendsto_coe_nhds_top_iff.mp hv
      cases ν with
      | none => exact Or.inr ⟨rfl,ht⟩
      | some q =>
        obtain ⟨j,hj⟩ := (ht.eventually_gt_atTop q).exists
        exact (hj.not_ge (le_of_eq (show v j = q from hvOK j))).elim
    | coe q =>
      have ht := EReal.tendsto_coe.mp hv
      exact Or.inl ⟨q,(shapes_closed ν).mem_of_tendsto ht (Eventually.of_forall hvOK),ht⟩
  generalize z.1 = zm at hm
  generalize z.2.1 = za at ha
  cases zm using EReal.rec with
  | bot =>
    exact (hRemote (.inr ((EReal.tendsto_coe_nhds_bot_iff.mp hm).mono_right
      (by rw [cocompact_eq_atBot_atTop]; exact le_sup_left)))).elim
  | top =>
    exact (hRemote (.inr ((EReal.tendsto_coe_nhds_top_iff.mp hm).mono_right
      (by rw [cocompact_eq_atBot_atTop]; exact le_sup_right)))).elim
  | coe b =>
    have hm' := EReal.tendsto_coe.mp hm
    have hb := (locations_closed μ).mem_of_tendsto hm' (Eventually.of_forall hmOK)
    cases za using EReal.rec with
    | bot =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_bot_iff.mp ha).eventually_lt_atBot 0).and ha₀).exists
      exact (not_lt_of_ge (show 0 ≤ a j from hj'.le) hj).elim
    | top => exact (hRemote (.inl (EReal.tendsto_coe_nhds_top_iff.mp ha))).elim
    | coe s =>
      have hs := EReal.tendsto_coe.mp ha
      have hs' := ge_of_tendsto hs (ha₀.mono fun _ hj ↦ hj.le)
      rcases hs'.eq_or_lt with hzero | hpos
      · subst s
        have hscale : Tendsto a atTop (𝓝[>] 0) := tendsto_nhdsWithin_iff.mpr ⟨hs,ha₀⟩
        rcases hV' with ⟨q,hq,hq'⟩ | ⟨_,hq⟩
        · have hq'' := shape_floor ν hq
          have vanish (hp : 0 < TDist.exponent x b q) : False :=
            hZero ((Student2MovingCollapse.moving_shape_collapse x b (hq₀.trans_le hq'')
              hm' hscale hq').1 hp)
          rcases hq''.eq_or_lt with hEq | hlt
          · rw [← hEq] at hq'
            rcases (he b hb).eq_or_lt with hE | hE
            · exact (fixed_slant_critical_le x hn β μ ν hq₀ hE.symm hB hAB hA hV hb hmOK
                hm' hscale hq' hv₀).elim
            · exact (vanish (hEq ▸ hE)).elim
          · have hn' : (0 : ℝ) < n := by exact_mod_cast hn
            have hE := he b hb
            have h : 0 < (n : ℝ) - TDist.multiplicity x b := by
              by_contra! hh
              unfold TDist.exponent at hE
              linarith [mul_nonpos_of_nonneg_of_nonpos hq₀.le hh]
            have hprod := mul_pos (sub_pos.mpr hlt) h
            unfold TDist.exponent at hE
            exact (vanish (by unfold TDist.exponent; nlinarith only [hprod,hE])).elim
        · have hnon : ∃ i, x i ≠ b := by
            by_contra! h
            have H := he b hb
            rw [TDist.exponent_all_tied x b (ν.getD 1) h] at H
            have hn' : (0 : ℝ) < n := by exact_mod_cast hn
            linarith
          exact (hZero (Student2Normal.zero_scale_normal_corner x hm' hscale hq hnon)).elim
      · rcases hV' with ⟨q,hq,hq'⟩ | ⟨hfree,hq⟩
        · have H := moving_finite_skew_face x hpos (hq₀.trans_le (shape_floor ν hq))
            hm' hs hq' (α := fun _ ↦ β) tendsto_const_nhds
          exact ⟨(b,s,q),⟨⟨hb,hpos,hq⟩,ge_of_tendsto' H hV⟩,φ,hφ,
            hm'.prodMk_nhds (hs.prodMk_nhds hq')⟩
        · have H := moving_skew_normal_face x hpos hm' hs hq (α := fun _ ↦ β) tendsto_const_nhds
          exact (hAB.not_ge ((ge_of_tendsto' H hV).trans
            (hB (Or.inr (Or.inl ⟨hfree,b,hb,s,hpos,rfl⟩))))).elim

private theorem gaussian_peak_bound {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) (h : ∃ i, x i ≠ b) :
    ∃ C, ∀ a > 0, (∏ i, TDist.normalDensity b a (x i)) ≤ C := by
  obtain ⟨i,hi⟩ := h
  let d := (x i-b)^2
  have hd : 0 < d := sq_pos_of_ne_zero (sub_ne_zero.mpr hi)
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hS : d ≤ ∑ j, (x j-b)^2 :=
    Finset.single_le_sum (f := fun j ↦ (x j-b)^2) (fun _ _ ↦ sq_nonneg _) (Finset.mem_univ i)
  refine ⟨exp ((n : ℝ) * (-(log (2*π)/2)) + n/2 * (log n - log d - 1)), fun a ha ↦ ?_⟩
  let u := d / ((n : ℝ)*a^2)
  have hu : 0 < u := by dsimp [u]; positivity
  have hlog : log u = log d - log n - 2*log a := by
    dsimp [u]
    rw [log_div hd.ne' (by positivity), log_mul hn'.ne' (by positivity), log_pow]
    norm_num
    ring
  have H := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos hu) (show 0 ≤ (n : ℝ)/2 by positivity)
  have he : (n : ℝ)/2*u = d/a^2/2 := by dsimp [u]; field_simp
  rw [mul_sub, he, hlog] at H
  simp_rw [TDist.normalDensity, ← exp_sum]
  apply exp_le_exp.mpr
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← Finset.sum_div]
  have hS' := div_le_div_of_nonneg_right (div_le_div_of_nonneg_right hS (sq_nonneg a))
    (by norm_num : 0 ≤ (2 : ℝ))
  nlinarith only [H,hS']

private theorem fixed_slant_faces_bounded {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) (μ ν : Option ℝ) (hq : 0 < ν.getD 1)
    (he : ∀ b ∈ locations μ, 0 ≤ TDist.exponent x b (ν.getD 1)) :
    BddAbove (fixedSlantFaces x β μ ν) := by
  let q := ν.getD 1
  have hNormal : ∃ C, ν = none → ∀ b ∈ locations μ, ∀ a > 0, skewNormalHeight x b a β ≤ C := by
    cases ν with
    | some => exact ⟨0,by simp⟩
    | none =>
      cases μ with
      | none =>
        have hBound : BddAbove (value x '' fitted) := by
          by_contra hh
          obtain ⟨b,hb⟩ := (fitted_majority_and_vanishing_ends.{0,0,0} x hn).1.mp hh
          have H : (n : ℝ) < 2*TDist.multiplicity x b := by exact_mod_cast hb
          have hh := he b (mem_univ _)
          change 0 ≤ TDist.exponent x b 1 at hh
          unfold TDist.exponent at hh
          linarith
        obtain ⟨C,hC⟩ := hBound
        exact ⟨C,fun _ b _ a ha ↦ upper_reaches_skew_normal x
          (fun b a v β ha hv ↦ show value x (b,a,v,β) ≤ C from hC ⟨(b,a,v,β),⟨ha,hv⟩,rfl⟩)
          b a β ha⟩
      | some b =>
        have hx : ∃ i, x i ≠ b := by
          by_contra! hx
          have H := he b (show b ∈ locations (.some b) from mem_singleton b)
          change 0 ≤ TDist.exponent x b 1 at H
          rw [TDist.exponent_all_tied x b 1 hx] at H
          have hn' : (0 : ℝ) < n := by exact_mod_cast hn
          linarith
        obtain ⟨C,hC⟩ := gaussian_peak_bound x hn b hx
        refine ⟨C * (2 : ℝ)^n, fun _ m hm a ha ↦ ?_⟩
        have hm' : m = b := hm
        subst m
        unfold skewNormalHeight
        have hF : (∏ i, 2*SkewLSGaussian.Φ (β*((x i-b)/a))) ≤ (2 : ℝ)^n := by
          rw [show (2 : ℝ)^n = ∏ _ : Fin n, (2 : ℝ) by simp]
          exact Finset.prod_le_prod₀ (fun _ _ ↦ (phi2_bounds _).1) (fun _ _ ↦ (phi2_bounds _).2)
        exact mul_le_mul (hC a ha) hF (Finset.prod_nonneg fun _ _ ↦ (phi2_bounds _).1)
          ((Finset.prod_nonneg fun _ _ ↦ (exp_pos _).le).trans (hC a ha))
  obtain ⟨C,hC⟩ := hNormal
  let M := ∑ i, exp (TDist.collapseLogValue x (x i) q) * (2 : ℝ)^n
  refine ⟨max (max 0 C) M, ?_⟩
  rintro _ (rfl | ⟨hfree,b,hb,a,ha,rfl⟩ | ⟨b,hb,d,hd,hE,rfl⟩)
  · exact (le_max_left _ _).trans (le_max_left _ _)
  · exact (hC hfree b hb a ha).trans ((le_max_right _ _).trans (le_max_left _ _))
  · have hk : 0 < TDist.multiplicity x b := by
      by_contra! hz
      have hzero := Nat.eq_zero_of_le_zero hz
      have H : TDist.exponent x b q = 0 := hE
      unfold TDist.exponent at H
      rw [hzero,Nat.cast_zero,sub_zero,sub_zero] at H
      exact (mul_pos hq (by exact_mod_cast hn : (0 : ℝ) < n)).ne' H
    obtain ⟨i,hi⟩ := Finset.card_pos.mp hk
    have hi' : x i = b := (Finset.mem_filter.mp hi).2
    have hbase : criticalBase x b q d 0 ≤ exp (TDist.collapseLogValue x b q) := by
      unfold criticalBase
      rw [mul_zero,zero_add]
      exact exp_le_exp.mpr (sub_le_self _ (mul_nonneg (by positivity)
        (log_nonneg (le_add_of_nonneg_right (by positivity)))))
    have hF : atomSlantFactor x b q β d ≤ (2 : ℝ)^n := by
      rw [show (2 : ℝ)^n = ∏ _ : Fin n, (2 : ℝ) by simp]
      exact Finset.prod_le_prod₀ (fun _ _ ↦ (cdf2_bounds _ _).1) (fun _ _ ↦ (cdf2_bounds _ _).2)
    have H := mul_le_mul hbase hF (Finset.prod_nonneg fun _ _ ↦ (cdf2_bounds _ _).1) (exp_pos _).le
    refine H.trans (le_trans ?_ (le_max_right _ _))
    rw [← hi']
    exact Finset.single_le_sum (f := fun j ↦ exp (TDist.collapseLogValue x (x j) q) * (2 : ℝ)^n)
      (fun _ _ ↦ by positivity) (Finset.mem_univ i)

/-- Scores of the free coordinates under held slant. When estimated df
is at the fitted floor, compare the conditional nuisance likelihoods. -/
def fixedSlantCandidate {n : ℕ} (x : Fin n → ℝ) (β : ℝ) (μ ν : Option ℝ)
    (p : ℝ × ℝ × ℝ) : Prop :=
  deriv (fun a ↦ log (likelihood x p.1 p.2.2 β a)) p.2.1 = 0 ∧
  (μ = none → deriv (fun b ↦ log (likelihood x b p.2.2 β p.2.1)) p.1 = 0) ∧
  (ν = none → (1 < p.2.2 →
      deriv (fun v ↦ log (likelihood x p.1 v β p.2.1)) p.2.2 = 0) ∧
    (p.2.2 = 1 → ∀ b ∈ locations μ, ∀ a > 0, likelihood x b 1 β a ≤ likelihood x p.1 p.2.2 β p.2.1))

/-- The joined held-slant criterion and failure comparison, with
optional held location and held positive shape. All reachable atoms are
tested, and a feasible candidate tied with the boundary may attain. -/
theorem fixed_slant_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) (μ ν : Option ℝ) (hq : 0 < ν.getD 1) :
    let D := fixedSlantDomain μ ν
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
    let E := fun b ↦ TDist.exponent x b (ν.getD 1)
    let F := fixedSlantFaces x β μ ν
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ∃ b ∈ locations μ, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b ∈ locations μ, 0 ≤ E b) ∧
      ∃ p ∈ D, fixedSlantCandidate x β μ ν p ∧ B ≤ L p) ∧
    ((∀ b ∈ locations μ, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)) := by
  dsimp only
  let D := fixedSlantDomain μ ν
  let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
  let E := fun b ↦ TDist.exponent x b (ν.getD 1)
  let F := fixedSlantFaces x β μ ν
  let B := sSup F
  have hne : F.Nonempty := ⟨0,Or.inl rfl⟩
  have hfaces (he : ∀ b ∈ locations μ, 0 ≤ E b) : IsLUB F B :=
    isLUB_csSup hne (fixed_slant_faces_bounded x hn β μ ν hq he)
  have hr {C : ℝ} (h : C ∈ upperBounds (L '' D)) : B ≤ C :=
    csSup_le hne (fixedSlant_upper_faces x hn β μ ν hq
      (fun b a v hb ha hv ↦ h ⟨(b,a,v),⟨hb,ha,hv⟩,rfl⟩))
  have hu (b : ℝ) (hb : b ∈ locations μ) (he : E b < 0) : ¬ BddAbove (L '' D) := by
    rintro ⟨C,hC⟩
    obtain ⟨a,ha,hbad⟩ := (right_pos.and
      ((StudentSkew.skew_negative_path x b β hq he).eventually_gt_atTop C)).exists
    exact hbad.not_ge (show L (b,a,ν.getD 1) ≤ C from
      hC ⟨(b,a,ν.getD 1),⟨hb,ha,floor_feasible ν⟩,rfl⟩)
  have hmax (he : ∀ b ∈ locations μ, 0 ≤ E b) (p : ℝ × ℝ × ℝ) (hp : p ∈ D) (h : B ≤ L p) :
      ∃ q ∈ D, IsMaxOn L D q := by
    by_cases hm : IsMaxOn L D p
    · exact ⟨p,hp,hm⟩
    rw [IsMaxOn, IsMaxFilter, eventually_principal] at hm
    push Not at hm
    obtain ⟨w,hw,hw'⟩ := hm
    have hF := (hfaces he).1
    let K := {p ∈ D | L w ≤ L p}
    have hK : IsCompact K := held_slant_superlevel x hn β μ ν hq he hF (h.trans_lt hw')
      ((hF (Or.inl rfl)).trans_lt (h.trans_lt hw'))
    have hcont : ContinuousOn L K := by
      intro q hq'
      have hp := hq'.1
      exact (moving_finite_skew_face x hp.2.1 (hq.trans_le (shape_floor ν hp.2.2))
        (continuous_fst.tendsto q) ((continuous_fst.comp continuous_snd).tendsto q)
        ((continuous_snd.comp continuous_snd).tendsto q) tendsto_const_nhds).mono_left
          nhdsWithin_le_nhds
    obtain ⟨q,hq',hqm⟩ := hK.exists_isMaxOn ⟨w,hw,le_rfl⟩ hcont
    exact ⟨q,hq'.1,fun z hz ↦ if hh : L w ≤ L z then hqm ⟨hz,hh⟩
      else (le_of_not_ge hh).trans hq'.2⟩
  have hSup (he : ∀ b ∈ locations μ, 0 ≤ E b) (hnm : ¬ ∃ p ∈ D, IsMaxOn L D p) :
      IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B := by
    have hh (p : ℝ × ℝ × ℝ) (hp : p ∈ D) : L p < B :=
      lt_of_not_ge fun h ↦ hnm (hmax he p hp h)
    exact ⟨⟨by rintro _ ⟨p,hp,rfl⟩; exact (hh p hp).le,fun _ hB ↦ hr hB⟩,hh⟩
  change ((¬ BddAbove (L '' D)) ↔ ∃ b ∈ locations μ, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b ∈ locations μ, 0 ≤ E b) ∧
      ∃ p ∈ D, fixedSlantCandidate x β μ ν p ∧ B ≤ L p) ∧
    ((∀ b ∈ locations μ, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
  refine ⟨⟨?_,?_⟩,⟨?_,?_⟩,?_⟩
  · intro h
    by_contra! he
    by_cases hm : ∃ p ∈ D, IsMaxOn L D p
    · obtain ⟨p,hp,hm⟩ := hm
      exact h ⟨L p,by rintro _ ⟨z,hz,rfl⟩; exact hm hz⟩
    · exact h ⟨B,(hSup he hm).1.1⟩
  · rintro ⟨b,hb,he⟩
    exact hu b hb he
  · rintro ⟨p,hp,hm⟩
    have hpmax : L p ∈ upperBounds (L '' D) := by rintro _ ⟨z,hz,rfl⟩; exact hm hz
    have he (b : ℝ) (hb : b ∈ locations μ) : 0 ≤ E b := le_of_not_gt fun h ↦ hu b hb h ⟨L p,hpmax⟩
    have hlog (z : ℝ × ℝ × ℝ) (hz : z ∈ D) : log (L z) ≤ log (L p) := by
      have hv := hq.trans_le (shape_floor ν hz.2.2)
      have hpos : 0 < L z := mul_pos (exp_pos _) (Finset.prod_pos fun _ _ ↦ mul_pos two_pos
        (cdf_pos (by positivity : 0 < z.2.2+1) _))
      exact log_le_log hpos (hm hz)
    refine ⟨he,p,hp,⟨?_,?_,?_⟩,hr hpmax⟩
    · have h : IsLocalMax (fun a ↦ log (likelihood x p.1 p.2.2 β a)) p.2.1 := by
        filter_upwards [Ioi_mem_nhds (show 0 < p.2.1 from hp.2.1)] with a ha
        exact hlog (p.1,a,p.2.2) ⟨hp.1,ha,hp.2.2⟩
      exact h.deriv_eq_zero
    · intro hμ
      have h : IsLocalMax (fun b ↦ log (likelihood x b p.2.2 β p.2.1)) p.1 :=
        Eventually.of_forall fun b ↦ hlog (b,p.2.1,p.2.2)
          ⟨by rw [hμ]; exact mem_univ _,hp.2.1,hp.2.2⟩
      exact h.deriv_eq_zero
    · intro hν
      constructor
      · intro hv
        have h : IsLocalMax (fun v ↦ log (likelihood x p.1 v β p.2.1)) p.2.2 := by
          filter_upwards [Ioi_mem_nhds hv] with v hv'
          exact hlog (p.1,p.2.1,v) ⟨hp.1,hp.2.1,by rw [hν]; exact (show 1 < v from hv').le⟩
        exact h.deriv_eq_zero
      · intro hv b hb a ha
        change L (b,a,1) ≤ L p
        exact hm (show (b,a,1) ∈ D from ⟨hb,ha,by rw [hν]; exact le_rfl⟩)
  · rintro ⟨he,p,hp,_,h⟩
    exact hmax he p hp h
  · intro he
    have hF := hfaces he
    refine ⟨hF,fun A hA ↦ held_slant_superlevel x hn β μ ν hq he hF.1 hA
      ((hF.1 (Or.inl rfl)).trans_lt hA),⟨hSup he,?_⟩⟩
    rintro ⟨hL,hlt⟩ ⟨p,hp,hm⟩
    exact (hlt p hp).not_ge (hL.2 (show L p ∈ upperBounds (L '' D) by
      rintro _ ⟨z,hz,rfl⟩
      exact hm hz))

/-- With slant held, the normal and critical displacement layers are the excluded faces. -/
theorem held_alpha_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) :
    let D := fixedSlantDomain none none
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
    let E := fun b ↦ TDist.exponent x b 1
    let F := fixedSlantFaces x β none none
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ∃ b ∈ locations none, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b ∈ locations none, 0 ≤ E b) ∧
      ∃ p ∈ D, fixedSlantCandidate x β none none p ∧ B ≤ L p) ∧
    ((∀ b ∈ locations none, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)) := by
  exact fixed_slant_decision x hn β none none (by norm_num)

/-- At held slant and df, only critical atom displacement can compete at an excluded end. -/
theorem held_alpha_df_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) {q : ℝ} (hq : 0 < q) :
    let D := fixedSlantDomain none (some q)
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
    let E := fun b ↦ TDist.exponent x b q
    let F := fixedSlantFaces x β none (some q)
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ∃ b ∈ locations none, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b ∈ locations none, 0 ≤ E b) ∧
      ∃ p ∈ D, fixedSlantCandidate x β none (some q) p ∧ B ≤ L p) ∧
    ((∀ b ∈ locations none, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)) := by
  exact fixed_slant_decision x hn β none (some q) hq

/-- At held slant and location, critical collapse has displacement zero. -/
theorem held_alpha_location_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β b : ℝ) :
    let D := fixedSlantDomain (some b) none
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
    let E := fun b ↦ TDist.exponent x b 1
    let F := fixedSlantFaces x β (some b) none
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ∃ y ∈ locations (some b), E y < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ y ∈ locations (some b), 0 ≤ E y) ∧
      ∃ p ∈ D, fixedSlantCandidate x β (some b) none p ∧ B ≤ L p) ∧
    ((∀ y ∈ locations (some b), 0 ≤ E y) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)) := by
  exact fixed_slant_decision x hn β (some b) none (by norm_num)

/-- Permitted scales with an optional hold. -/
def scales (ω : Option ℝ) : Set ℝ := (ω.map singleton).getD univ

private theorem scales_closed (ω : Option ℝ) : IsClosed (scales ω) := by
  cases ω
  · exact isClosed_univ
  · exact isClosed_singleton

/-- The maximal eligible one-sided contact is two for moving location
and one when location is held. -/
def contactWeight (μ : Option ℝ) : ℝ := μ.elim 2 (fun _ ↦ 1)

/-- Fitted skew Student chart with slant free and optional nuisance holds. -/
def freeSlantDomain (μ ν ω : Option ℝ) : Set (ℝ × ℝ × ℝ × ℝ) :=
  {p | p.1 ∈ locations μ ∧ 0 < p.2.1 ∧ p.2.1 ∈ scales ω ∧ p.2.2.1 ∈ shapes ν}

/-- Excluded heights with unrestricted slant, respecting all nuisance holds. -/
def freeSlantFaces {n : ℕ} (x : Fin n → ℝ) (μ ν ω : Option ℝ) : Set ℝ :=
  {z | z = 0 ∨
    (ν = none ∧ ∃ b ∈ locations μ, ∃ s ∈ scales ω, ∃ β, 0 < s ∧ z = skewNormalHeight x b s β) ∨
    (∃ b ∈ locations μ, ∃ s ∈ scales ω, ∃ q ∈ shapes ν, ∃ ε, 0 < s ∧ ε^2 = 1 ∧
      z = TDist.likelihood x b q s * orientedWeight x b ε (contactWeight μ)) ∨
    (ν = none ∧ ∃ b ∈ locations μ, ∃ s ∈ scales ω, ∃ ε, 0 < s ∧ ε^2 = 1 ∧
      z = (∏ i, TDist.normalDensity b s (x i)) * orientedWeight x b ε (contactWeight μ)) ∨
    (ω = none ∧ ∃ b ∈ locations μ, ∃ β d, (μ = none ∨ d = 0) ∧
      TDist.exponent x b (ν.getD 1) = 0 ∧
      z = criticalBase x b (ν.getD 1) d 0 * atomSlantFactor x b (ν.getD 1) β d) ∨
    (ω = none ∧ ∃ b ∈ locations μ, ∃ ε, TDist.exponent x b (ν.getD 1) = 0 ∧ ε^2 = 1 ∧
      z = exp (TDist.collapseLogValue x b (ν.getD 1)) * orientedWeight x b ε (contactWeight μ))}

private theorem rec_zero (F : ℝ → ℝ) : EReal.rec 0 F 2 (0 : EReal) = F 0 := rfl

private theorem freeSlant_bound_pos_scale_faces {n : ℕ} (x : Fin n → ℝ) (μ : Option ℝ)
    {B : ℝ} (b s ε : ℝ) (hb : b ∈ locations μ) (hs : 0 < s) (hε : ε^2 = 1)
    (v : ℝ → ℝ)
    (h : ∀ᶠ t : ℝ in 𝓝[>] 0, ∀ m ∈ locations μ, ∀ β, likelihood x m (v t) β s ≤ B) :
    (∀ q > 0, Tendsto v (𝓝[>] 0) (𝓝 q) →
      TDist.likelihood x b q s * orientedWeight x b ε (contactWeight μ) ≤ B) ∧
    (Tendsto v (𝓝[>] 0) atTop →
      (∏ i, TDist.normalDensity b s (x i)) * orientedWeight x b ε (contactWeight μ) ≤ B) := by
  let α := fun t : ℝ ↦ ε*(t^2)⁻¹
  let m := fun t : ℝ ↦ μ.elim (b-ε*t) (fun _ ↦ b)
  let c : EReal := μ.elim ⊤ (fun _ ↦ 0)
  have hm : Tendsto m (𝓝[>] 0) (𝓝 b) := by
    cases μ
    · simpa only [m,Option.elim_none,id_eq,mul_zero,sub_zero] using (right_zero.const_mul ε).const_sub b
    · exact tendsto_const_nhds
  have hmOK (t : ℝ) : m t ∈ locations μ := by
    cases μ
    · exact mem_univ _
    · exact hb
  have hc (k : ℝ) (hk : 0 < k)
      (hM : Tendsto (fun t ↦ sqrt ((v t+1)/(v t*s^2+(b-m t)^2))) (𝓝[>] 0) (𝓝 k)) :
      Tendsto (fun t ↦ ((α t*(b-m t)*sqrt ((v t+1)/(v t*s^2+(b-m t)^2)) : ℝ) : EReal))
        (𝓝[>] 0) (𝓝 c) := by
    cases μ with
    | none =>
      apply EReal.tendsto_coe_nhds_top_iff.mpr
      refine (tendsto_inv_nhdsGT_zero.atTop_mul_pos hk hM).congr' ?_
      filter_upwards [right_pos] with t ht
      have hId : α t*(b-m t) = t⁻¹ := by
        change ε*(t^2)⁻¹*(b-(b-ε*t)) = _
        rw [show b-(b-ε*t) = ε*t by ring]
        calc
          _ = ε^2 * ((t^2)⁻¹*t) := by ring
          _ = _ := by rw [hε, one_mul]; field_simp [ht.ne']
      change _ = α t*(b-m t)*_
      rw [hId]
    | some b₀ => simp [m,c]
  constructor
  · intro q hq hv
    have hr : Tendsto (fun t ↦ b-m t) (𝓝[>] 0) (𝓝 0) := by simpa using hm.const_sub b
    let k := sqrt ((q+1)/(q*s^2))
    have hk : 0 < k := by dsimp [k]; positivity
    have hM : Tendsto (fun t ↦ sqrt ((v t+1)/(v t*s^2+(b-m t)^2))) (𝓝[>] 0) (𝓝 k) := by
      have H := (hv.add_const 1).div ((hv.mul_const (s^2)).add (hr.pow 2)) (by positivity)
      simpa only [k, Pi.div_def, zero_pow (by norm_num : (2 : ℕ) ≠ 0), add_zero] using H.sqrt
    have H := (moving_oriented_faces x hs hm (a := fun _ ↦ s) tendsto_const_nhds hε
      (orientation_tail ε hε) (hc k hk hM)).1 q hq hv
    have H' := le_of_tendsto H (h.mono fun t ht ↦ ht (m t) (hmOK t) (α t))
    cases μ
    · exact H'
    · simpa [c,contactWeight,orientedWeight,rec_zero,cdf_zero (by positivity : 0 < q+1)] using H'
  · intro hv
    have hM := moving_normal_multiplier b hs hm (a := fun _ ↦ s) tendsto_const_nhds hv
    have H := (moving_oriented_faces x hs hm (a := fun _ ↦ s) tendsto_const_nhds hε
      (orientation_tail ε hε) (hc s⁻¹ (inv_pos.mpr hs) hM)).2 hv
    have H' := le_of_tendsto H (h.mono fun t ht ↦ ht (m t) (hmOK t) (α t))
    cases μ
    · exact H'
    · simpa [c,contactWeight,orientedWeight,rec_zero,SkewLSGaussian.cdf_zero] using H'

private theorem held_contact_atom_reach {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b q B : ℝ} (hq : 0 < q) (he : TDist.exponent x b q = 0)
    (h : ∀ a > 0, ∀ β, likelihood x b q β a ≤ B) (ε : ℝ) (hε : ε^2 = 1) :
    exp (TDist.collapseLogValue x b q) * orientedWeight x b ε 1 ≤ B := by
  have hd : Tendsto (fun t : ℝ ↦ (b-b)/id t) (𝓝[>] 0) (𝓝 0) := by simp
  have hr : Tendsto (fun t : ℝ ↦ (q-q)*log (id t)) (𝓝[>] 0) (𝓝 0) := by simp
  let α := fun t : ℝ ↦ ε*(t^2)⁻¹
  have hc : Tendsto (fun t : ℝ ↦ ((α t*(b-b)*sqrt ((q+1)/(q*(id t)^2+(b-b)^2)) : ℝ) : EReal))
      (𝓝[>] 0) (𝓝 (0 : EReal)) := by simp
  have H := (moving_critical_skew_faces x hn hq he (m := fun _ ↦ b) (a := id) (v := fun _ ↦ q)
    tendsto_const_nhds tendsto_id tendsto_const_nhds hd hr).2
      ε hε (orientation_tail ε hε) 0 hc
  have H' := le_of_tendsto H (right_pos.mono fun t ht ↦ h t ht (α t))
  simpa [orientedWeight,rec_zero,cdf_zero (by positivity : 0 < q+1)] using H'

private theorem freeSlant_upper_faces {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ ν ω : Option ℝ) (hq : 0 < ν.getD 1) (hs₀ : 0 < ω.getD 1) {B : ℝ}
    (h : ∀ p ∈ freeSlantDomain μ ν ω, value x p ≤ B) :
    B ∈ upperBounds (freeSlantFaces x μ ν ω) := by
  have fit (b a q β : ℝ) (hb : b ∈ locations μ) (ha : 0 < a)
      (ha' : a ∈ scales ω) (hq : q ∈ shapes ν) : likelihood x b q β a ≤ B :=
    show value x (b,a,q,β) ≤ B from h _ ⟨hb,ha,ha',hq⟩
  rintro _ (rfl | ⟨rfl,b,hb,s,hs',β,hs,rfl⟩ | ⟨b,hb,s,hs',q,hq',ε,hs,hε,rfl⟩ |
    ⟨rfl,b,hb,s,hs',ε,hs,hε,rfl⟩ | ⟨rfl,b,hb,β,d,hd,he,rfl⟩ | ⟨rfl,b,hb,ε,he,hε,rfl⟩)
  · let b := μ.getD 0
    let s := ω.getD 1
    have hb : b ∈ locations μ := by cases μ <;> simp [b,locations]
    have hs : s ∈ scales ω := by cases ω <;> simp [s,scales]
    exact (dominated x b (ν.getD 1) 0 s).1.trans (fit b s _ 0 hb hs₀ hs (floor_feasible ν))
  · apply le_of_tendsto (moving_skew_normal_face x hs (l := atTop) (v := id)
      (m := fun _ : ℝ ↦ b) (a := fun _ ↦ s) (α := fun _ ↦ β)
      tendsto_const_nhds tendsto_const_nhds tendsto_id tendsto_const_nhds)
    exact (eventually_ge_atTop (1 : ℝ)).mono fun v hv ↦ fit b s v β hb hs hs' hv
  · exact (freeSlant_bound_pos_scale_faces x μ b s ε hb hs hε (fun _ ↦ q)
      (Eventually.of_forall fun _ m hm β ↦ fit m s q β hm hs hs' hq')).1 q
      (hq.trans_le (shape_floor ν hq')) tendsto_const_nhds
  · let q := fun t : ℝ ↦ 1+t⁻¹
    have hq' : Tendsto q (𝓝[>] 0) atTop :=
      tendsto_atTop_add_const_left _ _ tendsto_inv_nhdsGT_zero
    exact (freeSlant_bound_pos_scale_faces x μ b s ε hb hs hε q (right_pos.mono
      fun t ht m hm β ↦ fit m s (q t) β hm hs hs' (le_add_of_nonneg_right (inv_pos.mpr ht).le))).2 hq'
  · have H := fixedSlant_upper_faces x hn β μ ν hq (fun m s q hm hs hq ↦
      fit m s q β hm hs (mem_univ _) hq)
    exact H (Or.inr (Or.inr ⟨b,hb,d,hd,he,rfl⟩))
  · cases μ with
    | none =>
      exact (upper_reaches_atom x hn b (ν.getD 1) hq he
        (fun m a β ha ↦ fit m a _ β (mem_univ _) ha (mem_univ _) (floor_feasible ν))).2 ε hε
    | some b₀ =>
      exact held_contact_atom_reach x hn hq he
        (fun a ha β ↦ fit b a _ β hb ha (mem_univ _) (floor_feasible ν)) ε hε

private theorem held_contact_factor_bounds {m a v α : ℕ → ℝ} (μ : Option ℝ)
    {b : ℝ} (hb : b ∈ locations μ) (hm : ∀ j, m j ∈ locations μ)
    {t : EReal} (F : ℝ → ℝ) (hF : ∀ r, 0 ≤ F r ∧ F r ≤ 2) (h0 : F 0 = 1)
    (ht : Tendsto (fun j ↦ ((α j*(b-m j)*sqrt ((v j+1)/(v j*(a j)^2+(b-m j)^2)) : ℝ)
      : EReal)) atTop (𝓝 t)) :
    (0 : ℝ) ≤ EReal.rec 0 F 2 t ∧ EReal.rec 0 F 2 t ≤ contactWeight μ := by
  cases μ with
  | none => exact extended_factor_bounds F hF t
  | some b₀ =>
    have hb' : b = b₀ := hb
    have hm' (j : ℕ) : m j = b₀ := hm j
    have hlim : Tendsto (fun j ↦ ((α j*(b-m j)*sqrt ((v j+1)/(v j*(a j)^2+(b-m j)^2)) : ℝ)
        : EReal)) atTop (𝓝 (0 : EReal)) := by
      simp [hb',hm']
    have h := tendsto_nhds_unique ht hlim
    rw [h,rec_zero,h0]
    exact ⟨zero_le_one,le_rfl⟩

private theorem oriented_weight_le {n : ℕ} (x : Fin n → ℝ) (b ε : ℝ) {c C : ℝ}
    (hc : 0 ≤ c) (hC : c ≤ C) :
    0 ≤ orientedWeight x b ε c ∧ orientedWeight x b ε c ≤ orientedWeight x b ε C := by
  have h (i : Fin n) : 0 ≤ (if 0 < ε*(x i-b) then 2 else if x i = b then c else 0) := by
    split_ifs <;> positivity
  constructor
  · exact Finset.prod_nonneg fun i _ ↦ h i
  · apply Finset.prod_le_prod₀ (fun i _ ↦ h i)
    intro i _
    split_ifs <;> linarith

private theorem restricted_oriented_side {n : ℕ} (x : Fin n → ℝ) {m a v α : ℕ → ℝ}
    (μ ν ω : Option ℝ) (hq₀ : 0 < ν.getD 1)
    {b s ε A B : ℝ} (hB : B ∈ upperBounds (freeSlantFaces x μ ν ω)) (hAB : B < A)
    (hval : ∀ j, A ≤ likelihood x (m j) (v j) (α j) (a j))
    (hb : b ∈ locations μ) (hmOK : ∀ j, m j ∈ locations μ) (hs : 0 < s)
    (hs' : s ∈ scales ω) (hm : Tendsto m atTop (𝓝 b)) (ha : Tendsto a atTop (𝓝 s))
    (hε : ε^2 = 1) (hεα : Tendsto (fun j ↦ ε*α j) atTop atTop)
    (hv : (∃ q ∈ shapes ν, Tendsto v atTop (𝓝 q)) ∨
      (ν = none ∧ Tendsto v atTop atTop)) : False := by
  let z : ℕ → EReal := fun j ↦ ↑(α j*(b-m j)*sqrt ((v j+1)/(v j*(a j)^2+(b-m j)^2)))
  obtain ⟨c,φ,hφ,hz⟩ := CompactSpace.tendsto_subseq z
  have hT := hφ.tendsto_atTop
  have h := moving_oriented_faces x hs (hm.comp hT) (ha.comp hT) hε (hεα.comp hT) hz
  rcases hv with ⟨q,hq,hqv⟩ | ⟨hfree,hqv⟩
  · have hq' := hq₀.trans_le (shape_floor ν hq)
    have H := h.1 q hq' (hqv.comp hT)
    have hc := held_contact_factor_bounds (m := m ∘ φ) (a := a ∘ φ) (v := v ∘ φ) (α := α ∘ φ)
      μ hb (fun j ↦ hmOK (φ j)) (fun t ↦ 2*studentCDF (q+1) t) (cdf2_bounds _)
      (by rw [cdf_zero (by positivity : 0 < q+1)]; norm_num) hz
    have hu := (oriented_weight_le x b ε hc.1 hc.2).2
    have hn := mul_le_mul_of_nonneg_left hu (exp_pos (TDist.logLik x b q s)).le
    have hface : TDist.likelihood x b q s * orientedWeight x b ε (contactWeight μ) ∈
        freeSlantFaces x μ ν ω := Or.inr (Or.inr (Or.inl ⟨b,hb,s,hs',q,hq,ε,hs,hε,rfl⟩))
    exact hAB.not_ge ((ge_of_tendsto' H (fun j ↦ hval (φ j))).trans (hn.trans (hB hface)))
  · have H := h.2 (hqv.comp hT)
    have hc := held_contact_factor_bounds (m := m ∘ φ) (a := a ∘ φ) (v := v ∘ φ) (α := α ∘ φ)
      μ hb (fun j ↦ hmOK (φ j)) (fun t ↦ 2*SkewLSGaussian.Φ t) phi2_bounds
      (by rw [SkewLSGaussian.cdf_zero]; norm_num) hz
    have hu := (oriented_weight_le x b ε hc.1 hc.2).2
    have hnon : 0 ≤ ∏ i, TDist.normalDensity b s (x i) :=
      Finset.prod_nonneg fun _ _ ↦ (exp_pos _).le
    have hn := mul_le_mul_of_nonneg_left hu hnon
    have hface : (∏ i, TDist.normalDensity b s (x i)) * orientedWeight x b ε (contactWeight μ) ∈
        freeSlantFaces x μ ν ω := Or.inr (Or.inr (Or.inr (Or.inl ⟨hfree,b,hb,s,hs',ε,hs,hε,rfl⟩)))
    exact hAB.not_ge ((ge_of_tendsto' H (fun j ↦ hval (φ j))).trans (hn.trans (hB hface)))

private theorem restricted_critical_side {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ ν : Option ℝ) {m a v α : ℕ → ℝ} {b A B : ℝ}
    (hq : 0 < ν.getD 1) (he : TDist.exponent x b (ν.getD 1) = 0)
    (hB : B ∈ upperBounds (freeSlantFaces x μ ν none)) (hAB : B < A)
    (hA : 0 < A) (hval : ∀ j, A ≤ likelihood x (m j) (v j) (α j) (a j))
    (hb : b ∈ locations μ) (hmOK : ∀ j, m j ∈ locations μ)
    (hm : Tendsto m atTop (𝓝 b)) (ha : Tendsto a atTop (𝓝[>] 0))
    (hv : Tendsto v atTop (𝓝 (ν.getD 1))) (hv₁ : ∀ᶠ j in atTop, ν.getD 1 ≤ v j)
    (hα : (∃ β, Tendsto α atTop (𝓝 β)) ∨ ∃ ε : ℝ, ε^2 = 1 ∧
      Tendsto (fun j ↦ ε*α j) atTop atTop) : False := by
  let q := ν.getD 1
  let δ := fun j ↦ (m j-b)/a j
  let ρ := fun j ↦ (v j-q)*log (a j)
  let p : ℕ → EReal × EReal := fun j ↦ (δ j,ρ j)
  obtain ⟨w,ψ,hψ,hw⟩ := CompactSpace.tendsto_subseq p
  have ht := hψ.tendsto_atTop
  have hd : Tendsto (fun j ↦ (δ (ψ j) : EReal)) atTop (𝓝 w.1) := (continuous_fst.tendsto w).comp hw
  have hr : Tendsto (fun j ↦ (ρ (ψ j) : EReal)) atTop (𝓝 w.2) := (continuous_snd.tendsto w).comp hw
  have H := Student2MovingCollapse.moving_critical_atom_base x hn b hq he
    (hm.comp ht) (ha.comp ht) (hv.comp ht)
  have hZero (h : Tendsto (fun j ↦ TDist.likelihood x (m (ψ j)) (v (ψ j)) (a (ψ j))) atTop (𝓝 0)) :
      False := hA.not_ge (ge_of_tendsto' (vanish_under_base (α := α ∘ ψ) x h) (fun j ↦ hval (ψ j)))
  have hρ : ∀ᶠ j in atTop, ρ (ψ j) ≤ 0 := ht.eventually (by
    filter_upwards [hv₁,(tendsto_log_nhdsGT_zero.comp ha).eventually_lt_atBot 0] with j hj hj'
    exact mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hj) hj'.le)
  generalize w.1 = wd at hd
  generalize w.2 = wr at hr
  cases wd using EReal.rec with
  | bot =>
    have hd' := (tendsto_neg_atBot_atTop.comp (EReal.tendsto_coe_nhds_bot_iff.mp hd)).atTop_pow₀
      (by norm_num : (2 : ℕ) ≠ 0)
    exact hZero (H.2.2 (ht.eventually hv₁) (by simpa [Function.comp_def,δ] using hd'))
  | top =>
    exact hZero (H.2.2 (ht.eventually hv₁)
      ((EReal.tendsto_coe_nhds_top_iff.mp hd).atTop_pow₀ (by norm_num)))
  | coe d =>
    have hd' := EReal.tendsto_coe.mp hd
    have hd0 : μ = none ∨ d = 0 := by
      cases μ with
      | none => exact Or.inl rfl
      | some m₀ =>
        refine Or.inr (tendsto_nhds_unique hd' ?_)
        have hb' : b = m₀ := hb
        have H (j : ℕ) : m j = m₀ := hmOK j
        simp [δ,H,hb']
    cases wr using EReal.rec with
    | bot => exact hZero (H.2.1 (EReal.tendsto_coe_nhds_bot_iff.mp hr))
    | top =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_top_iff.mp hr).eventually_gt_atTop 0).and hρ).exists
      exact hj.not_ge hj'
    | coe r =>
      have hr' := EReal.tendsto_coe.mp hr
      have hr₀ : r ≤ 0 := le_of_tendsto hr' hρ
      have hcount : (TDist.multiplicity x b : ℝ) ≤ n := by
        exact_mod_cast (Finset.card_filter_le (s := Finset.univ) (fun i ↦ x i = b)).trans_eq
          (by simp : (Finset.univ : Finset (Fin n)).card = n)
      have hpow : ((n : ℝ) - TDist.multiplicity x b) * r ≤ 0 :=
        mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hcount) hr₀
      have hK : criticalBase x b q d r ≤ criticalBase x b q d 0 := by
        unfold criticalBase
        apply exp_le_exp.mpr
        linarith
      rcases hα with ⟨β,hβ⟩ | ⟨ε,hε,hεα⟩
      · have H := (moving_critical_skew_faces x hn hq he (hm.comp ht) (ha.comp ht)
          (hv.comp ht) hd' hr').1 β (hβ.comp ht)
        have hnon : 0 ≤ atomSlantFactor x b q β d :=
          Finset.prod_nonneg fun _ _ ↦ (cdf2_bounds _ _).1
        have h := mul_le_mul_of_nonneg_right hK hnon
        exact hAB.not_ge ((ge_of_tendsto' H (fun j ↦ hval (ψ j))).trans
          (h.trans (hB (Or.inr (Or.inr (Or.inr (Or.inr (Or.inl
            ⟨rfl,b,hb,β,d,hd0,he,rfl⟩))))))))
      · have hK' : criticalBase x b q d r ≤ exp (TDist.collapseLogValue x b q) := by
          unfold criticalBase
          apply exp_le_exp.mpr
          have hlog : 0 ≤ log (1+d^2/q) := log_nonneg (le_add_of_nonneg_right (by positivity))
          have hLoss : 0 ≤ (q+1)/2 * TDist.multiplicity x b * log (1+d^2/q) :=
            mul_nonneg (by positivity) hlog
          linarith only [hpow,hLoss]
        let z : ℕ → EReal := fun j ↦ ↑(α (ψ j)*(b-m (ψ j))*
          sqrt ((v (ψ j)+1)/(v (ψ j)*(a (ψ j))^2+(b-m (ψ j))^2)))
        obtain ⟨c,φ,hφ,hc⟩ := CompactSpace.tendsto_subseq z
        have ht' := hφ.tendsto_atTop
        have H := (moving_critical_skew_faces x hn hq he (hm.comp (ht.comp ht'))
          (ha.comp (ht.comp ht')) (hv.comp (ht.comp ht')) (hd'.comp ht') (hr'.comp ht')).2
          ε hε (hεα.comp (ht.comp ht')) c hc
        have hc' := held_contact_factor_bounds (m := m ∘ ψ ∘ φ) (a := a ∘ ψ ∘ φ)
          (v := v ∘ ψ ∘ φ) (α := α ∘ ψ ∘ φ) μ hb (fun j ↦ hmOK (ψ (φ j)))
          (fun t ↦ 2*studentCDF (q+1) t) (cdf2_bounds (q+1))
          (by rw [cdf_zero (by positivity : 0 < q+1)]; norm_num) hc
        have hW := oriented_weight_le x b ε hc'.1 hc'.2
        have h := mul_le_mul hK' hW.2 hW.1 (exp_pos _).le
        exact hAB.not_ge ((ge_of_tendsto' H (fun j ↦ hval (ψ (φ j)))).trans
          (h.trans (hB (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr
            ⟨rfl,b,hb,ε,he,hε,rfl⟩))))))))

private theorem free_slant_superlevel {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ ν ω : Option ℝ) (hq₀ : 0 < ν.getD 1) (hs₀ : 0 < ω.getD 1)
    (he : ω = none → ∀ b ∈ locations μ, 0 ≤ TDist.exponent x b (ν.getD 1)) {A B : ℝ}
    (hB : B ∈ upperBounds (freeSlantFaces x μ ν ω)) (hAB : B < A) (hA : 0 < A) :
    IsCompact {p ∈ freeSlantDomain μ ν ω | A ≤ value x p} := by
  apply IsSeqCompact.isCompact
  intro p hp
  let u : ℕ → EReal × EReal × EReal × EReal := fun j ↦
    ((p j).1,(p j).2.1,(p j).2.2.1,(p j).2.2.2)
  obtain ⟨z,φ,hφ,hu⟩ := CompactSpace.tendsto_subseq u
  let m := fun j ↦ (p (φ j)).1
  let a := fun j ↦ (p (φ j)).2.1
  let v := fun j ↦ (p (φ j)).2.2.1
  let α := fun j ↦ (p (φ j)).2.2.2
  have hm : Tendsto (fun j ↦ (m j : EReal)) atTop (𝓝 z.1) :=
    (continuous_fst.tendsto z).comp hu
  have ha : Tendsto (fun j ↦ (a j : EReal)) atTop (𝓝 z.2.1) :=
    ((continuous_fst.comp continuous_snd).tendsto z).comp hu
  have hv : Tendsto (fun j ↦ (v j : EReal)) atTop (𝓝 z.2.2.1) :=
    ((continuous_fst.comp (continuous_snd.comp continuous_snd)).tendsto z).comp hu
  have hα : Tendsto (fun j ↦ (α j : EReal)) atTop (𝓝 z.2.2.2) :=
    ((continuous_snd.comp (continuous_snd.comp continuous_snd)).tendsto z).comp hu
  have hA' := slant_subseq α hα
  have haOK (j : ℕ) : a j ∈ scales ω := (hp (φ j)).1.2.2.1
  have hmOK (j : ℕ) : m j ∈ locations μ := (hp (φ j)).1.1
  have hvOK (j : ℕ) : v j ∈ shapes ν := (hp (φ j)).1.2.2.2
  have ha₀ : ∀ᶠ j in atTop, 0 < a j := Eventually.of_forall fun j ↦ (hp (φ j)).1.2.1
  have hv₀ : ∀ᶠ j in atTop, ν.getD 1 ≤ v j := Eventually.of_forall fun j ↦ shape_floor ν (hvOK j)
  have hV (j : ℕ) : A ≤ likelihood x (m j) (v j) (α j) (a j) := (hp (φ j)).2
  have hZero (h : Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) atTop (𝓝 0)) :
      False := hA.not_ge (ge_of_tendsto' (vanish_under_base (α := α) x h) hV)
  have hRemote (h : Tendsto a atTop atTop ∨ Tendsto m atTop (cocompact ℝ)) : False := by
    apply hZero
    cases ν with
    | none => exact Student2Normal.moving_shape_remote_ends x hn ha₀ hv₀ h
    | some q =>
      have hvq (j : ℕ) : v j = q := hvOK j
      refine (StudentFaces.lst_fixed_df_remote_ends x hn (show 0 < q from hq₀) ha₀ h).congr ?_
      intro j
      simp [hvq]
  have hV' : (∃ q ∈ shapes ν, Tendsto v atTop (𝓝 q)) ∨
      (ν = none ∧ Tendsto v atTop atTop) := by
    generalize z.2.2.1 = q at hv
    cases q using EReal.rec with
    | bot =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_bot_iff.mp hv).eventually_lt_atBot
        (ν.getD 1)).and hv₀).exists
      exact (hj.not_ge hj').elim
    | top =>
      have ht := EReal.tendsto_coe_nhds_top_iff.mp hv
      cases ν with
      | none => exact Or.inr ⟨rfl,ht⟩
      | some q =>
        obtain ⟨j,hj⟩ := (ht.eventually_gt_atTop q).exists
        exact (hj.not_ge (le_of_eq (show v j = q from hvOK j))).elim
    | coe q =>
      have ht := EReal.tendsto_coe.mp hv
      exact Or.inl ⟨q,(shapes_closed ν).mem_of_tendsto ht (Eventually.of_forall hvOK),ht⟩
  generalize z.1 = zm at hm
  generalize z.2.1 = za at ha
  cases zm using EReal.rec with
  | bot =>
    exact (hRemote (.inr ((EReal.tendsto_coe_nhds_bot_iff.mp hm).mono_right
      (by rw [cocompact_eq_atBot_atTop]; exact le_sup_left)))).elim
  | top =>
    exact (hRemote (.inr ((EReal.tendsto_coe_nhds_top_iff.mp hm).mono_right
      (by rw [cocompact_eq_atBot_atTop]; exact le_sup_right)))).elim
  | coe b =>
    have hm' := EReal.tendsto_coe.mp hm
    have hb := (locations_closed μ).mem_of_tendsto hm' (Eventually.of_forall hmOK)
    cases za using EReal.rec with
    | bot =>
      obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_bot_iff.mp ha).eventually_lt_atBot 0).and ha₀).exists
      exact (not_lt_of_ge (show 0 ≤ a j from hj'.le) hj).elim
    | top => exact (hRemote (.inl (EReal.tendsto_coe_nhds_top_iff.mp ha))).elim
    | coe s =>
      have hs := EReal.tendsto_coe.mp ha
      have hs' := ge_of_tendsto hs (ha₀.mono fun _ hj ↦ hj.le)
      have hsOK := (scales_closed ω).mem_of_tendsto hs (Eventually.of_forall haOK)
      rcases hs'.eq_or_lt with hzero | hpos
      · subst s
        have hf : ω = none := by
          cases ω with
          | none => rfl
          | some σ =>
            have heq : (0 : ℝ) = σ := hsOK
            exact ((show 0 < σ from hs₀).ne' heq.symm).elim
        subst ω
        have hscale : Tendsto a atTop (𝓝[>] 0) := tendsto_nhdsWithin_iff.mpr ⟨hs,ha₀⟩
        rcases hV' with ⟨q,hq,hq'⟩ | ⟨_,hq⟩
        · have hq'' := shape_floor ν hq
          have vanish (hp : 0 < TDist.exponent x b q) : False :=
            hZero ((Student2MovingCollapse.moving_shape_collapse x b (hq₀.trans_le hq'')
              hm' hscale hq').1 hp)
          rcases hq''.eq_or_lt with hEq | hlt
          · rw [← hEq] at hq'
            rcases (he rfl b hb).eq_or_lt with hE | hE
            · exact (restricted_critical_side x hn μ ν hq₀ hE.symm hB hAB hA hV hb hmOK
                hm' hscale hq' hv₀ hA').elim
            · exact (vanish (hEq ▸ hE)).elim
          · have hn' : (0 : ℝ) < n := by exact_mod_cast hn
            have hE := he rfl b hb
            have h : 0 < (n : ℝ) - TDist.multiplicity x b := by
              by_contra! hh
              unfold TDist.exponent at hE
              linarith [mul_nonpos_of_nonneg_of_nonpos hq₀.le hh]
            have hprod := mul_pos (sub_pos.mpr hlt) h
            unfold TDist.exponent at hE
            exact (vanish (by unfold TDist.exponent; nlinarith only [hprod,hE])).elim
        · have hnon : ∃ i, x i ≠ b := by
            by_contra! h
            have H := he rfl b hb
            rw [TDist.exponent_all_tied x b (ν.getD 1) h] at H
            have hn' : (0 : ℝ) < n := by exact_mod_cast hn
            linarith
          exact (hZero (Student2Normal.zero_scale_normal_corner x hm' hscale hq hnon)).elim
      · rcases hA' with ⟨β,hβ⟩ | ⟨ε,hε,hεα⟩
        · rcases hV' with ⟨q,hq,hq'⟩ | ⟨hfree,hq⟩
          · have H := moving_finite_skew_face x hpos (hq₀.trans_le (shape_floor ν hq)) hm' hs hq' hβ
            exact ⟨(b,s,q,β),⟨⟨hb,hpos,hsOK,hq⟩,ge_of_tendsto' H hV⟩,φ,hφ,
              hm'.prodMk_nhds (hs.prodMk_nhds (hq'.prodMk_nhds hβ))⟩
          · have H := moving_skew_normal_face x hpos hm' hs hq hβ
            exact (hAB.not_ge ((ge_of_tendsto' H hV).trans
              (hB (Or.inr (Or.inl ⟨hfree,b,hb,s,hsOK,β,hpos,rfl⟩))))).elim
        · exact (restricted_oriented_side x μ ν ω hq₀ hB hAB hV hb hmOK hpos hsOK hm' hs
            hε hεα hV').elim

private theorem fixed_scale_skew_bounded {n : ℕ} (x : Fin n → ℝ) (μ ν : Option ℝ)
    {s : ℝ} (hs : 0 < s) (hq₀ : 0 < ν.getD 1) :
    BddAbove (value x '' freeSlantDomain μ ν (some s)) := by
  have hmode : ∃ C, ∀ v ∈ shapes ν, exp (TDist.logNormalizer v) ≤ C := by
    cases ν with
    | some q =>
      refine ⟨exp (TDist.logNormalizer q), ?_⟩
      intro v (hv : v = q)
      rw [hv]
    | none =>
      let N := SkewLSGaussian.φ 0 + 1
      have H := standard_density_normal 0
      simp only [TDist.density,sub_self,log_one,sub_zero,zero_pow (by norm_num : (2 : ℕ) ≠ 0),
        zero_div,add_zero,mul_zero] at H
      obtain ⟨V,hV⟩ := eventually_atTop.mp
        (H.eventually (Iio_mem_nhds (show SkewLSGaussian.φ 0 < N by dsimp [N]; linarith)))
      have hcont : ContinuousOn (fun v ↦ exp (TDist.logNormalizer v)) (Icc 1 V) :=
        fun v hv ↦ (continuous_exp.continuousAt.comp (normalizer_continuous
          (zero_lt_one.trans_le hv.1))).continuousWithinAt
      obtain ⟨C,hC⟩ := isCompact_Icc.bddAbove_image hcont
      refine ⟨max C N, fun v hv ↦ ?_⟩
      by_cases h : V ≤ v
      · exact (hV v h).le.trans (le_max_right _ _)
      · exact (hC ⟨v,⟨hv,le_of_not_ge h⟩,rfl⟩).trans (le_max_left _ _)
  obtain ⟨C,hC⟩ := hmode
  refine ⟨(2 : ℝ)^n * (C/s)^n, ?_⟩
  rintro _ ⟨⟨b,a,v,α⟩,hp,rfl⟩
  have ha : a = s := hp.2.2.1
  have hv : v ∈ shapes ν := hp.2.2.2
  have hv₀ := hq₀.trans_le (shape_floor ν hv)
  subst a
  apply (dominated x b v α s).2.trans
  apply mul_le_mul_of_nonneg_left _ (by positivity)
  unfold TDist.likelihood TDist.logLik
  rw [exp_sum, ← (show (∏ _ : Fin n, C/s) = (C/s)^n by simp [div_pow])]
  apply Finset.prod_le_prod₀ (fun _ _ ↦ (exp_pos _).le)
  intro i _
  have hK : 0 ≤ log (1+(x i-b)^2/(v*s^2)) := log_nonneg (le_add_of_nonneg_right (by positivity))
  exact (exp_le_exp.mpr (sub_le_self _ (mul_nonneg (by positivity) hK))).trans (by
    rw [exp_sub,exp_log hs]
    exact div_le_div_of_nonneg_right (hC v hv) hs.le)

private theorem zero_slant_identity {n : ℕ} (x : Fin n → ℝ) (b s : ℝ) {q : ℝ} (hq : 0 < q) :
    likelihood x b q 0 s = TDist.likelihood x b q s := by
  simp [likelihood,cdf_zero (by positivity : 0 < q+1)]

private theorem free_slant_no_negative_bounded {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ ν ω : Option ℝ) (hq : 0 < ν.getD 1) (hs : 0 < ω.getD 1)
    (he : ω = none → ∀ b ∈ locations μ, 0 ≤ TDist.exponent x b (ν.getD 1)) :
    BddAbove (value x '' freeSlantDomain μ ν ω) := by
  cases ω with
  | some s => exact fixed_scale_skew_bounded x μ ν hs hq
  | none =>
    have H : BddAbove ((fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 0 p.2.1) ''
        fixedSlantDomain μ ν) := by
      by_contra h
      obtain ⟨b,hb,hb'⟩ := (fixed_slant_decision x hn 0 μ ν hq).1.mp h
      exact hb'.not_ge (he rfl b hb)
    obtain ⟨B,hB⟩ := H
    refine ⟨(2 : ℝ)^n*B, ?_⟩
    rintro _ ⟨⟨b,a,q,α⟩,hp,rfl⟩
    have h₁ := hB (show likelihood x b q 0 a ∈
      (fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 0 p.2.1) '' fixedSlantDomain μ ν from
      ⟨(b,a,q),⟨hp.1,hp.2.1,hp.2.2.2⟩,rfl⟩)
    change likelihood x b q 0 a ≤ B at h₁
    rw [zero_slant_identity x b a (hq.trans_le (shape_floor ν hp.2.2.2))] at h₁
    exact (dominated x b q α a).2.trans (mul_le_mul_of_nonneg_left h₁ (by positivity))

/-- The free-coordinate score conditions with slant free, including
conditional nuisance maximization at the estimated df floor. -/
def freeSlantCandidate {n : ℕ} (x : Fin n → ℝ) (μ ν ω : Option ℝ)
    (p : ℝ × ℝ × ℝ × ℝ) : Prop :=
  deriv (fun α ↦ log (likelihood x p.1 p.2.2.1 α p.2.1)) p.2.2.2 = 0 ∧
  (ω = none → deriv (fun a ↦ log (likelihood x p.1 p.2.2.1 p.2.2.2 a)) p.2.1 = 0) ∧
  (μ = none → deriv (fun b ↦ log (likelihood x b p.2.2.1 p.2.2.2 p.2.1)) p.1 = 0) ∧
  (ν = none → (1 < p.2.2.1 →
      deriv (fun q ↦ log (likelihood x p.1 q p.2.2.2 p.2.1)) p.2.2.1 = 0) ∧
    (p.2.2.1 = 1 → ∀ b ∈ locations μ, ∀ a ∈ scales ω, ∀ β, 0 < a →
      likelihood x b 1 β a ≤ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1))

/-- The joint escape and attainment decision with slant free. Holds
remove ineligible faces, including all collapse when scale is fixed.
Every superlevel strictly above the excluded supremum is compact. -/
theorem free_slant_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ ν ω : Option ℝ) (hq : 0 < ν.getD 1) (hs : 0 < ω.getD 1) :
    let D := freeSlantDomain μ ν ω
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let E := fun b ↦ TDist.exponent x b (ν.getD 1)
    let F := freeSlantFaces x μ ν ω
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ω = none ∧ ∃ b ∈ locations μ, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (ω = none → ∀ b ∈ locations μ, 0 ≤ E b) ∧
      ∃ p ∈ D, freeSlantCandidate x μ ν ω p ∧ B ≤ L p) ∧
    ((ω = none → ∀ b ∈ locations μ, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)) := by
  dsimp only
  let D := freeSlantDomain μ ν ω
  let L := value x
  let E := fun b ↦ TDist.exponent x b (ν.getD 1)
  let F := freeSlantFaces x μ ν ω
  let B := sSup F
  have hne : F.Nonempty := ⟨0,Or.inl rfl⟩
  have upperFace {C : ℝ} (h : C ∈ upperBounds (L '' D)) : C ∈ upperBounds F :=
    freeSlant_upper_faces x hn μ ν ω hq hs (fun p hp ↦ h ⟨p,hp,rfl⟩)
  have hfaces (he : ω = none → ∀ b ∈ locations μ, 0 ≤ E b) : IsLUB F B := by
    obtain ⟨C,hC⟩ := free_slant_no_negative_bounded x hn μ ν ω hq hs he
    exact isLUB_csSup hne ⟨C,upperFace hC⟩
  have hr {C : ℝ} (h : C ∈ upperBounds (L '' D)) : B ≤ C := csSup_le hne (upperFace h)
  have hu (hω : ω = none) (b : ℝ) (hb : b ∈ locations μ) (he : E b < 0) :
      ¬ BddAbove (L '' D) := by
    rintro ⟨C,hC⟩
    obtain ⟨a,ha,hbad⟩ := (right_pos.and
      ((StudentSkew.skew_negative_path x b 0 hq he).eventually_gt_atTop C)).exists
    exact hbad.not_ge (show L (b,a,ν.getD 1,0) ≤ C from
      hC ⟨(b,a,ν.getD 1,0),⟨hb,ha,by rw [hω]; exact mem_univ _,floor_feasible ν⟩,rfl⟩)
  have hmax (he : ω = none → ∀ b ∈ locations μ, 0 ≤ E b)
      (p : Param) (hp : p ∈ D) (h : B ≤ L p) : ∃ q ∈ D, IsMaxOn L D q := by
    by_cases hm : IsMaxOn L D p
    · exact ⟨p,hp,hm⟩
    rw [IsMaxOn, IsMaxFilter, eventually_principal] at hm
    push Not at hm
    obtain ⟨w,hw,hw'⟩ := hm
    have H := (hfaces he).1
    let K := {p ∈ D | L w ≤ L p}
    have hK : IsCompact K := free_slant_superlevel x hn μ ν ω hq hs he H (h.trans_lt hw')
      ((H (Or.inl rfl)).trans_lt (h.trans_lt hw'))
    have hcont : ContinuousOn L K := by
      intro p hp
      have hp' := hp.1
      exact (moving_finite_skew_face x hp'.2.1 (hq.trans_le (shape_floor ν hp'.2.2.2))
        (continuous_fst.tendsto p) ((continuous_fst.comp continuous_snd).tendsto p)
        ((continuous_fst.comp (continuous_snd.comp continuous_snd)).tendsto p)
        ((continuous_snd.comp (continuous_snd.comp continuous_snd)).tendsto p)).mono_left
          nhdsWithin_le_nhds
    obtain ⟨q,hq',hqm⟩ := hK.exists_isMaxOn ⟨w,hw,le_rfl⟩ hcont
    exact ⟨q,hq'.1,fun z hz ↦ if hh : L w ≤ L z then hqm ⟨hz,hh⟩
      else (le_of_not_ge hh).trans hq'.2⟩
  have hSup (he : ω = none → ∀ b ∈ locations μ, 0 ≤ E b)
      (hnm : ¬ ∃ p ∈ D, IsMaxOn L D p) : IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B := by
    have hh (p : Param) (hp : p ∈ D) : L p < B :=
      lt_of_not_ge fun h ↦ hnm (hmax he p hp h)
    exact ⟨⟨by rintro _ ⟨p,hp,rfl⟩; exact (hh p hp).le,fun _ hB ↦ hr hB⟩,hh⟩
  change ((¬ BddAbove (L '' D)) ↔ ω = none ∧ ∃ b ∈ locations μ, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (ω = none → ∀ b ∈ locations μ, 0 ≤ E b) ∧
      ∃ p ∈ D, freeSlantCandidate x μ ν ω p ∧ B ≤ L p) ∧
    ((ω = none → ∀ b ∈ locations μ, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
  refine ⟨⟨?_,?_⟩,⟨?_,?_⟩,?_⟩
  · intro h
    by_contra! he
    exact h (free_slant_no_negative_bounded x hn μ ν ω hq hs he)
  · rintro ⟨hω,b,hb,he⟩
    exact hu hω b hb he
  · rintro ⟨p,hp,hm⟩
    have hpmax : L p ∈ upperBounds (L '' D) := by rintro _ ⟨z,hz,rfl⟩; exact hm hz
    have he (hω : ω = none) (b : ℝ) (hb : b ∈ locations μ) : 0 ≤ E b :=
      le_of_not_gt fun h ↦ hu hω b hb h ⟨L p,hpmax⟩
    have hlog (z : Param) (hz : z ∈ D) : log (L z) ≤ log (L p) := by
      have hv := hq.trans_le (shape_floor ν hz.2.2.2)
      have hpos : 0 < L z := mul_pos (exp_pos _) (Finset.prod_pos fun _ _ ↦ mul_pos two_pos
        (cdf_pos (by positivity : 0 < z.2.2.1+1) _))
      exact log_le_log hpos (hm hz)
    refine ⟨he,p,hp,⟨?_,?_,?_,?_⟩,hr hpmax⟩
    · have h : IsLocalMax (fun α ↦ log (likelihood x p.1 p.2.2.1 α p.2.1)) p.2.2.2 :=
        Eventually.of_forall fun α ↦ hlog (p.1,p.2.1,p.2.2.1,α) hp
      exact h.deriv_eq_zero
    · intro hω
      have h : IsLocalMax (fun a ↦ log (likelihood x p.1 p.2.2.1 p.2.2.2 a)) p.2.1 := by
        filter_upwards [Ioi_mem_nhds (show 0 < p.2.1 from hp.2.1)] with a ha
        exact hlog (p.1,a,p.2.2.1,p.2.2.2)
          ⟨hp.1,ha,by rw [hω]; exact mem_univ _,hp.2.2.2⟩
      exact h.deriv_eq_zero
    · intro hμ
      have h : IsLocalMax (fun b ↦ log (likelihood x b p.2.2.1 p.2.2.2 p.2.1)) p.1 :=
        Eventually.of_forall fun b ↦ hlog (b,p.2.1,p.2.2.1,p.2.2.2)
          ⟨by rw [hμ]; exact mem_univ _,hp.2⟩
      exact h.deriv_eq_zero
    · intro hν
      constructor
      · intro hv
        have h : IsLocalMax (fun v ↦ log (likelihood x p.1 v p.2.2.2 p.2.1)) p.2.2.1 := by
          filter_upwards [Ioi_mem_nhds hv] with v hv'
          exact hlog (p.1,p.2.1,v,p.2.2.2)
            ⟨hp.1,hp.2.1,hp.2.2.1,by rw [hν]; exact (show 1 < v from hv').le⟩
        exact h.deriv_eq_zero
      · intro hv b hb a ha β ha'
        change L (b,a,1,β) ≤ L p
        exact hm (show (b,a,1,β) ∈ D from ⟨hb,ha',ha,by rw [hν]; exact le_rfl⟩)
  · rintro ⟨he,p,hp,_,h⟩
    exact hmax he p hp h
  · intro he
    have hF := hfaces he
    refine ⟨hF,fun A hA ↦ free_slant_superlevel x hn μ ν ω hq hs he hF.1 hA
      ((hF.1 (Or.inl rfl)).trans_lt hA),⟨hSup he,?_⟩⟩
    rintro ⟨hL,hlt⟩ ⟨p,hp,hm⟩
    exact (hlt p hp).not_ge (hL.2 (show L p ∈ upperBounds (L '' D) by
      rintro _ ⟨z,hz,rfl⟩
      exact hm hz))

/-- At held df, the eligible excluded heights are the two oriented
half-t faces and the critical displacement and slant layers. -/
theorem held_df_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {q : ℝ} (hq : 0 < q) :
    let D := freeSlantDomain none (some q) none
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let E := fun b ↦ TDist.exponent x b q
    let F := freeSlantFaces x none (some q) none
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ (none : Option ℝ) = none ∧ ∃ b ∈ locations none, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ ((none : Option ℝ) = none → ∀ b ∈ locations none, 0 ≤ E b) ∧
      ∃ p ∈ D, freeSlantCandidate x none (some q) none p ∧ B ≤ L p) ∧
    (((none : Option ℝ) = none → ∀ b ∈ locations none, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)) := by
  exact free_slant_decision x hn none (some q) none hq zero_lt_one

private theorem student_tail_lower {q b : ℝ} (hq : 0 < q) (hb : 0 < b) :
    b * TDist.density 0 q 1 b / q < ∫ y in Ioi b, TDist.density 0 q 1 y := by
  let f := TDist.density 0 q 1
  let C := f b * b^(q+1)
  let g := fun y : ℝ ↦ C * y^(-(q+1))
  have hI : Integrable g (volume.restrict (Ioi b)) :=
    (integrableOn_Ioi_rpow_of_lt (by linarith : -(q+1) < -1) hb).const_mul C
  have hfi : Integrable f (volume.restrict (Ioi b)) := (student_integrable hq).integrableOn
  have hstrict (y : ℝ) (hy : y ∈ Ioi b) : g y < f y := by
    have hby : b < y := hy
    have hy₀ := hb.trans hby
    have he : 1+y^2/q < (1+b^2/q)*(y^2/b^2) := by
      have hh : b^2 < y^2 := sq_lt_sq₀ hb.le hy₀.le |>.mpr hby
      rw [← mul_div_assoc]
      apply (lt_div_iff₀ (sq_pos_of_pos hb)).mpr
      have he : (1+b^2/q)*y^2 - (1+y^2/q)*b^2 = y^2-b^2 := by ring
      linarith only [hh,he]
    have hlog := log_lt_log (show 0 < 1+y^2/q by positivity) he
    rw [log_mul (by positivity) (by positivity),log_div (by positivity) (by positivity),
      log_pow,log_pow] at hlog
    push_cast at hlog
    have hpen := mul_lt_mul_of_pos_left hlog (show 0 < (q+1)/2 by positivity)
    dsimp [g,C,f,TDist.density]
    rw [rpow_def_of_pos hb, rpow_def_of_pos hy₀, ← exp_add, ← exp_add]
    simp only [sub_zero,log_one,one_pow,mul_one,exp_lt_exp]
    nlinarith only [hpen]
  have hdiff : 0 ≤ᵐ[volume.restrict (Ioi b)] fun y ↦ f y - g y := by
    change ∀ᵐ y ∂volume.restrict (Ioi b), 0 ≤ f y - g y
    rw [ae_restrict_iff' measurableSet_Ioi]
    exact Eventually.of_forall fun y hy ↦ sub_nonneg.mpr (hstrict y hy).le
  have hsupport : Ioi b ⊆ Function.support (fun y ↦ f y - g y) := by
    intro y hy
    exact (sub_pos.mpr (hstrict y hy)).ne'
  have hpos : 0 < ∫ y in Ioi b, f y - g y := by
    apply (integral_pos_iff_support_of_nonneg_ae hdiff (hfi.sub hI)).mpr
    apply lt_of_lt_of_le _ (measure_mono hsupport)
    rw [Measure.restrict_apply measurableSet_Ioi,inter_self,Real.volume_Ioi]
    exact ENNReal.zero_lt_top
  rw [integral_sub hfi hI] at hpos
  have hg : ∫ y in Ioi b, g y = b * f b / q := by
    unfold g
    rw [integral_const_mul, integral_Ioi_rpow_of_lt (by linarith : -(q+1) < -1) hb,
      show -(q+1)+1 = -q by ring]
    have hbpow : b^(q+1)*b^(-q) = b := by
      rw [← rpow_add hb,show q+1 + -q = 1 by ring,rpow_one]
    dsimp [C]
    calc
      _ = (f b * (b^(q+1)*b^(-q))) / q := by ring
      _ = _ := by rw [hbpow]; ring
  linarith only [hpos,hg]

/-- The Student reverse hazard lies above the negative-tail power
threshold. This strict inequality controls the critical skew scale score. -/
theorem student_reverse_hazard_bound {q : ℝ} (hq : 0 < q) (z : ℝ) :
    0 < q + z * ((TDist.density 0 q 1 z / ∫ y, TDist.density 0 q 1 y) / studentCDF q z) := by
  let M := ∫ y, TDist.density 0 q 1 y
  have hM : 0 < M := mass_positive hq
  have hc := cdf_pos hq z
  by_cases hz : 0 ≤ z
  · have h : 0 ≤ z * ((TDist.density 0 q 1 z / M) / studentCDF q z) :=
      mul_nonneg hz (div_nonneg (div_nonneg (exp_pos _).le hM.le) hc.le)
    exact add_pos_of_pos_of_nonneg hq h
  · let b := -z
    have hb : 0 < b := neg_pos.mpr (lt_of_not_ge hz)
    have hz' : z = -b := by dsimp [b]; ring
    have he := integral_comp_neg_Iic (-b) (TDist.density 0 q 1)
    simp_rw [student_even,neg_neg] at he
    have hC : studentCDF q z = (∫ y in Ioi b, TDist.density 0 q 1 y) / M := by
      rw [hz',studentCDF,he]
    have h := (div_lt_iff₀ hq).mp (student_tail_lower hq hb)
    have H : b * (TDist.density 0 q 1 b / M) < q * ((∫ y in Ioi b, TDist.density 0 q 1 y) / M) := by
      rw [← mul_div_assoc, ← mul_div_assoc, mul_comm q]
      exact div_lt_div_of_pos_right h hM
    rw [← hC] at H
    have H' := (div_lt_iff₀ hc).mpr H
    change 0 < q + z * ((TDist.density 0 q 1 z / M) / studentCDF q z)
    rw [hz',student_even,neg_mul,← mul_div_assoc]
    rw [hz'] at H'
    linarith only [H']

private noncomputable def argument (q α r t : ℝ) : ℝ :=
  α*r*sqrt ((q+1)/(q*(exp t)^2+r^2))

private theorem arg_hasDerivAt {q : ℝ} (hq : 0 < q) (α r t : ℝ) :
    HasDerivAt (argument q α r) (-(argument q α r t) * (q*(exp t)^2/(q*(exp t)^2+r^2))) t := by
  let D := fun u ↦ q*(exp u)^2+r^2
  have hD (u : ℝ) : 0 < D u := by dsimp [D]; positivity
  have hD' : HasDerivAt D (2*q*(exp t)^2) t := by
    convert (((hasDerivAt_exp t).pow 2).const_mul q).add_const (r^2) using 1
    norm_num
    ring
  let C := α*r*sqrt (q+1)
  have H := (hasDerivAt_const t C).div (hD'.sqrt (hD t).ne') (sqrt_pos.mpr (hD t)).ne'
  have he (u : ℝ) : argument q α r u = C / sqrt (D u) := by
    dsimp [argument,C,D]
    rw [sqrt_div (by positivity), mul_div_assoc]
  convert H using 1
  · exact funext he
  · rw [he]
    simp only [zero_mul,zero_sub]
    have hS : sqrt (D t) ≠ 0 := (sqrt_pos.mpr (hD t)).ne'
    field_simp [hS,(hD t).ne']
    rw [sq_sqrt (hD t).le]

private noncomputable def reverseHazard (q z : ℝ) : ℝ :=
  (TDist.density 0 q 1 z / ∫ y, TDist.density 0 q 1 y) / studentCDF q z

private theorem log_cdf_scale_deriv {q : ℝ} (hq : 0 < q) (α r t : ℝ) :
    HasDerivAt (fun u ↦ log (2*studentCDF (q+1) (argument q α r u)))
      (-(argument q α r t * reverseHazard (q+1) (argument q α r t)) *
        (q*(exp t)^2/(q*(exp t)^2+r^2))) t := by
  let z := argument q α r t
  have hq' : 0 < q+1 := by positivity
  have hp := cdf_pos hq' z
  have H := (((cdf_hasDerivAt hq' z).comp t (arg_hasDerivAt hq α r t)).const_mul 2).log
    (mul_pos two_pos hp).ne'
  convert H using 1
  · rfl
  · change -(z * reverseHazard (q+1) z) * _ = _
    simp only [Function.comp_apply, show argument q α r t = z from rfl]
    unfold reverseHazard
    field_simp [hp.ne',(mass_positive hq').ne']

/-- At fixed positive df and location, a critical exponent makes the
likelihood strictly decrease with scale at every finite slant. -/
theorem held_location_critical_strict_scale {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b α : ℝ) {q : ℝ} (hq : 0 < q) (he : TDist.exponent x b q = 0) :
    StrictAntiOn (likelihood x b q α) (Ioi 0) := by
  have hq' : 0 < q+1 := by positivity
  let f := fun t ↦ log (likelihood x b q α (exp t))
  have hnon : ∃ i, x i ≠ b := by
    by_contra! h
    rw [TDist.exponent_all_tied x b q h] at he
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    linarith
  have hf : StrictAnti f := by
    apply strictAnti_of_deriv_neg
    intro t
    let r := fun i : Fin n ↦ x i-b
    let U := q*(exp t)^2
    let S (i : Fin n) := (q+1)*(r i)^2/(U+(r i)^2) -
      argument q α (r i) t * reverseHazard (q+1) (argument q α (r i) t) *
        (U/(U+(r i)^2))
    have H := (TDist.hasDerivAt_logLik x b hq t).add
      (HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ log_cdf_scale_deriv hq α (r i) t)
    have H' : HasDerivAt f ((∑ i, S i) - n) t := by
      convert H using 1
      · ext u
        unfold f likelihood TDist.likelihood
        rw [log_mul (exp_ne_zero _) (Finset.prod_ne_zero_iff.mpr
          fun i _ ↦ (mul_pos two_pos (cdf_pos hq' _)).ne'), log_exp,
          log_prod (fun i _ ↦ (mul_pos two_pos (cdf_pos hq' _)).ne')]
        rfl
      · unfold TDist.scaleScore S r U
        rw [Finset.sum_sub_distrib]
        simp_rw [neg_mul, Finset.sum_neg_distrib]
        ring
    have hneg : (∑ i, S i) - n < 0 := by
      let Q (i : Fin n) : ℝ := if x i = b then 0 else q+1
      have hdiag (i : Fin n) (hi : x i = b) : S i = Q i := by simp [S,Q,r,hi,argument]
      have hlt (i : Fin n) (hi : x i ≠ b) : S i < Q i := by
        have hU : 0 < U := by dsimp [U]; positivity
        have hD : 0 < U+(r i)^2 := by positivity
        have hR := student_reverse_hazard_bound hq' (argument q α (r i) t)
        change 0 < (q+1) + argument q α (r i) t * reverseHazard (q+1) (argument q α (r i) t) at hR
        have hL := mul_pos hR (div_pos hU hD)
        rw [show Q i = q+1 by simp [Q,hi]]
        have hnorm : (q+1)*(r i)^2/(U+(r i)^2) + (q+1)*(U/(U+(r i)^2)) = q+1 := by
          field_simp [hD.ne']
          ring
        unfold S
        linarith only [hnorm,hL]
      have hSum : (∑ i, Q i) = n := by
        classical
        have hc : ((Finset.univ.filter (fun i ↦ x i ≠ b)).card : ℝ) =
            n - TDist.multiplicity x b := by
          have H := Finset.card_filter_add_card_filter_not (s := (Finset.univ : Finset (Fin n)))
            (fun i ↦ x i = b)
          rw [Finset.card_univ,Fintype.card_fin] at H
          change TDist.multiplicity x b + (Finset.univ.filter (fun i ↦ x i ≠ b)).card = n at H
          have H' : (TDist.multiplicity x b : ℝ) +
              ((Finset.univ.filter (fun i ↦ x i ≠ b)).card : ℝ) = n := by exact_mod_cast H
          linarith
        have hQ : (∑ i, Q i) = ((Finset.univ.filter (fun i ↦ x i ≠ b)).card : ℝ) * (q+1) := by
          rw [← nsmul_eq_mul, ← Finset.sum_const, Finset.sum_filter]
          apply Finset.sum_congr rfl
          intro i _
          by_cases hi : x i = b <;> simp [Q,hi]
        rw [hQ,hc]
        unfold TDist.exponent at he
        nlinarith only [he]
      obtain ⟨i,hi⟩ := hnon
      have h := Finset.sum_lt_sum (s := Finset.univ)
        (fun j _ ↦ if hj : x j = b then (hdiag j hj).le else (hlt j hj).le)
        ⟨i,Finset.mem_univ _,hlt i hi⟩
      linarith only [h,hSum]
    rw [H'.deriv]
    exact hneg
  intro s hs t ht hst
  have h := hf (log_lt_log (show 0 < s from hs) hst)
  change log (likelihood x b q α (exp (log t))) < log (likelihood x b q α (exp (log s))) at h
  rw [exp_log hs,exp_log ht] at h
  by_contra! h'
  have hp : 0 < likelihood x b q α s := mul_pos (exp_pos _)
    (Finset.prod_pos fun _ _ ↦ mul_pos two_pos (cdf_pos hq' _))
  exact h.not_ge (log_le_log hp h')

private theorem one_side_improves {n : ℕ} (x : Fin n → ℝ) {q a b : ℝ}
    (hq : 0 < q) (ha : 0 < a) (β ε : ℝ)
    (hx : ∀ i, 0 ≤ ε*(x i-b)) (hx' : ∃ i, 0 < ε*(x i-b)) :
    likelihood x b q β a < likelihood x b q (β+ε) a := by
  unfold likelihood
  apply mul_lt_mul_of_pos_left _ (exp_pos _)
  have hq' : 0 < q+1 := by positivity
  let f (i : Fin n) := sqrt ((q+1)/(q*a^2+(x i-b)^2))
  have hf (i : Fin n) : 0 < f i := by dsimp [f]; positivity
  apply Finset.prod_lt_prod₀ (fun i _ ↦ mul_pos two_pos (cdf_pos hq' _))
  · intro i _
    apply mul_le_mul_of_nonneg_left _ zero_le_two
    apply (cdf_strictMono hq').monotone
    have h := mul_nonneg (hx i) (hf i).le
    change β*(x i-b)*f i ≤ (β+ε)*(x i-b)*f i
    nlinarith only [h]
  · obtain ⟨i,hi⟩ := hx'
    refine ⟨i,Finset.mem_univ _,?_⟩
    apply mul_lt_mul_of_pos_left _ two_pos
    apply cdf_strictMono hq'
    have h := mul_pos hi (hf i)
    change β*(x i-b)*f i < (β+ε)*(x i-b)*f i
    nlinarith only [h]

private theorem oriented_zero_of_wrong_side {n : ℕ} (x : Fin n → ℝ) (b ε c : ℝ)
    (h : ∃ i, ε*(x i-b) < 0) : orientedWeight x b ε c = 0 := by
  obtain ⟨i,hi⟩ := h
  have hi' : x i ≠ b := by intro h; simp [h] at hi
  exact Finset.prod_eq_zero (Finset.mem_univ i) (by simp [hi.not_gt,hi'])

set_option maxHeartbeats 1000000 in
-- Comparing the held chart to the general product chart expands several nested predicates.
/-- With df and location held, a finite fit exists exactly at strictly
positive exponent and mixed residual signs. Below zero there is
divergence. With critical exponent or a single sign the excluded height
is the exact unattained supremum. -/
theorem held_df_location_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 0 < q) :
    let D := freeSlantDomain (some b) (some q) none
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let E := TDist.exponent x b q
    let mixed := (∃ i, b < x i) ∧ (∃ i, x i < b)
    let B := sSup (freeSlantFaces x (some b) (some q) none)
    ((∃ p ∈ D, IsMaxOn L D p) ↔ 0 < E ∧ mixed) ∧
    ((¬ BddAbove (L '' D)) ↔ E < 0) ∧
    (E = 0 ∨ (0 < E ∧ ¬ mixed) →
      IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B) ∧
    (0 < E → ∃! a, 0 < a ∧ TDist.scaleScore x b q a = 0) := by
  dsimp only
  let D := freeSlantDomain (some b) (some q) none
  let L := value x
  let E := TDist.exponent x b q
  let mixed := (∃ i, b < x i) ∧ (∃ i, x i < b)
  let F := freeSlantFaces x (some b) (some q) none
  let B := sSup F
  have H := free_slant_decision x hn (some b) (some q) none hq zero_lt_one
  have hE (he : 0 ≤ E) : (none : Option ℝ) = none →
      ∀ y ∈ locations (some b), 0 ≤ TDist.exponent x y q := by
    rintro _ y (rfl : y = b)
    exact he
  have hpos (a β : ℝ) : 0 < likelihood x b q β a :=
    mul_pos (exp_pos _) (Finset.prod_pos fun _ _ ↦
      mul_pos two_pos (cdf_pos (by positivity : 0 < q+1) _))
  have hnon (he : 0 ≤ E) : ∃ i, x i ≠ b := by
    by_contra! h
    change 0 ≤ TDist.exponent x b q at he
    rw [TDist.exponent_all_tied x b q h] at he
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    linarith
  have hfit : (∃ p ∈ D, IsMaxOn L D p) ↔ 0 < E ∧ mixed := by
    constructor
    · rintro ⟨⟨m,a,v,β⟩,hp,hm⟩
      have hmeq : m = b := hp.1
      have hveq : v = q := hp.2.2.2
      subst m v
      have ha : 0 < a := hp.2.1
      have hbound : BddAbove (L '' D) := ⟨L (b,a,q,β), by
        rintro _ ⟨z,hz,rfl⟩
        exact hm hz⟩
      have noNeg : 0 ≤ E := le_of_not_gt fun he ↦
        (H.1.mpr ⟨rfl,b,mem_singleton _,he⟩) hbound
      have hStrict : 0 < E := lt_of_le_of_ne noNeg (fun he ↦ by
        have hh := held_location_critical_strict_scale x hn b β hq he.symm
          (show a/2 ∈ Ioi (0 : ℝ) from half_pos ha) (show a ∈ Ioi (0 : ℝ) from ha)
          (half_lt_self ha)
        have Hm : L (b,a/2,q,β) ≤ L (b,a,q,β) :=
          hm (show (b,a/2,q,β) ∈ D from ⟨mem_singleton _,half_pos ha,mem_univ _,mem_singleton _⟩)
        exact hh.not_ge Hm)
      refine ⟨hStrict,?_⟩
      by_contra hmix
      have noMax (ε : ℝ) (he : ∀ i, 0 ≤ ε*(x i-b)) (he' : ∃ i, 0 < ε*(x i-b)) : False := by
        have Hm : L (b,a,q,β+ε) ≤ L (b,a,q,β) :=
          hm (show (b,a,q,β+ε) ∈ D from ⟨mem_singleton _,ha,mem_univ _,mem_singleton _⟩)
        exact (one_side_improves x hq ha β ε he he').not_ge Hm
      by_cases hP : ∃ i, b < x i
      · have hN : ∀ i, b ≤ x i := fun i ↦ le_of_not_gt fun hi ↦ hmix ⟨hP,i,hi⟩
        exact noMax 1 (fun i ↦ by simpa using sub_nonneg.mpr (hN i))
          (by simpa only [one_mul,sub_pos] using hP)
      · push Not at hP
        obtain ⟨i,hi⟩ := hnon noNeg
        have hN : x i < b := lt_of_le_of_ne (hP i) hi
        exact noMax (-1) (fun j ↦ by simpa using sub_nonneg.mpr (hP j))
          ⟨i,by simpa using sub_pos.mpr hN⟩
    · rintro ⟨he,hpos',hneg'⟩
      have hZero : (0 : ℝ) ∈ upperBounds F := by
        rintro _ (rfl | ⟨hf,_⟩ | ⟨y,hy,a,ha,q',hq',ε,ha',hε,rfl⟩ |
          ⟨hf,_⟩ | ⟨_,y,hy,β,d,hd,hc,rfl⟩ | ⟨_,y,hy,ε,hc,_,rfl⟩)
        · exact le_rfl
        · cases hf
        · have hy' : y = b := hy
          subst y
          have hs : ε = 1 ∨ ε = -1 := by
            have H : (ε-1)*(ε+1) = 0 := by nlinarith only [hε]
            rcases mul_eq_zero.mp H with h | h
            · exact Or.inl (by linarith)
            · exact Or.inr (by linarith)
          have H : orientedWeight x b ε (contactWeight (some b)) = 0 := by
            apply oriented_zero_of_wrong_side
            rcases hs with rfl | rfl
            · simpa only [one_mul,sub_neg] using hneg'
            · simpa only [neg_one_mul,neg_neg,neg_lt_zero,sub_pos] using hpos'
          rw [H,mul_zero]
        · cases hf
        · have hy' : y = b := hy
          subst y
          exact (he.ne' hc).elim
        · have hy' : y = b := hy
          subst y
          exact (he.ne' hc).elim
      let w : Param := (b,1,q,0)
      have hw : w ∈ D := ⟨mem_singleton _,zero_lt_one,mem_univ _,mem_singleton _⟩
      have hA : 0 < L w := hpos 1 0
      let K := {p ∈ D | L w ≤ L p}
      have hK : IsCompact K := free_slant_superlevel x hn (some b) (some q) none hq
        zero_lt_one (hE he.le) hZero hA hA
      have hcont : ContinuousOn L K := by
        intro z hz
        have hp := hz.1
        have hv : z.2.2.1 = q := hp.2.2.2
        exact (moving_finite_skew_face x hp.2.1 (show 0 < z.2.2.1 by rw [hv]; exact hq)
          (continuous_fst.tendsto z) ((continuous_fst.comp continuous_snd).tendsto z)
          ((continuous_fst.comp (continuous_snd.comp continuous_snd)).tendsto z)
          ((continuous_snd.comp (continuous_snd.comp continuous_snd)).tendsto z)).mono_left
            nhdsWithin_le_nhds
      obtain ⟨p,hp,hm⟩ := hK.exists_isMaxOn ⟨w,hw,le_rfl⟩ hcont
      exact ⟨p,hp.1,fun z hz ↦ if h : L w ≤ L z then hm ⟨hz,h⟩ else (le_of_not_ge h).trans hp.2⟩
  have hub : (¬ BddAbove (L '' D)) ↔ E < 0 := by
    constructor
    · intro h
      obtain ⟨_,y,hy,he⟩ := H.1.mp h
      have hy' : y = b := hy
      subst y
      exact he
    · intro he
      exact H.1.mpr ⟨rfl,b,mem_singleton _,he⟩
  refine ⟨hfit,hub,?_,?_⟩
  · intro h
    have he : 0 ≤ E := h.elim (fun h ↦ le_of_eq h.symm) (fun h ↦ h.1.le)
    have hNo : ¬ ∃ p ∈ D, IsMaxOn L D p := fun h' ↦ by
      obtain ⟨he',hm⟩ := hfit.mp h'
      rcases h with h | h
      · exact he'.ne' h
      · exact h.2 hm
    exact ((H.2.2 (hE he)).2.2.mp hNo)
  · intro he
    exact ((TDist.heldLocation_maximum x b hq (hnon he.le)).2 he).1

/-- At fixed positive scale, collapse is absent. The matched
skew-normal, half-Student and half-normal faces determine attainment. -/
theorem held_scale_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ} (hs : 0 < s) :
    let D := freeSlantDomain none none (some s)
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let F := freeSlantFaces x none none (some s)
    let B := sSup F
    BddAbove (L '' D) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ D, freeSlantCandidate x none none (some s) p ∧ B ≤ L p) ∧
    IsLUB F B ∧ (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B) := by
  have H := free_slant_decision x hn none none (some s) zero_lt_one hs
  simpa only [Option.some_ne_none, false_and, iff_false, not_not, false_implies, true_implies, true_and] using H

/-- At held location, only a majority there gives divergence. The
normal and oriented half faces keep the held contact, and only
zero-scale collapse at that location is eligible. -/
theorem held_location_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ) :
    let D := freeSlantDomain (some b) none none
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let E := TDist.exponent x b 1
    let F := freeSlantFaces x (some b) none none
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ E < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ 0 ≤ E ∧
      ∃ p ∈ D, freeSlantCandidate x (some b) none none p ∧ B ≤ L p) ∧
    (0 ≤ E → IsLUB F B ∧ (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)) := by
  simpa [locations] using free_slant_decision x hn (some b) none none zero_lt_one zero_lt_one


/-- Log density of the finite skew-Student fit at one observation. -/
noncomputable def spatialLogDensity (m q α s y : ℝ) : ℝ :=
  (log (TDist.density m q s y)) +
    log (2*studentCDF (q+1) (α*(y-m)*sqrt ((q+1)/(q*s^2+(y-m)^2))))

private noncomputable def logTail (q α s ε : ℝ) : ℝ :=
  TDist.logNormalizer q-log s+(q+1)/2*log (q*s^2)+
    log (2*studentCDF (q+1) (α*ε*sqrt (q+1)))

private theorem log_density_tail {q s ε : ℝ} (hq : 0 < q) (hs : 0 < s) (hε : ε^2 = 1)
    (m α : ℝ) :
    Tendsto (fun y : ℝ ↦ spatialLogDensity m q α s (ε*y)+(q+1)*log y)
      atTop (𝓝 (logTail q α s ε)) := by
  let r := fun y : ℝ ↦ ε-m/y
  let V := fun y : ℝ ↦ q*(s/y)^2+(r y)^2
  have hInv : Tendsto (fun y : ℝ ↦ y⁻¹) atTop (𝓝 0) := tendsto_inv_atTop_zero
  have hDiv (c : ℝ) : Tendsto (fun y : ℝ ↦ c/y) atTop (𝓝 0) := by
    simpa only [div_eq_mul_inv,mul_zero] using hInv.const_mul c
  have hR : Tendsto r atTop (𝓝 ε) := by
    simpa only [r,sub_zero] using (hDiv m).const_sub ε
  have hV : Tendsto V atTop (𝓝 1) := by
    simpa [V,hε] using (((hDiv s).pow 2).const_mul q).add (hR.pow 2)
  have hK : Tendsto (fun y ↦ α*r y*sqrt ((q+1)/V y))
      atTop (𝓝 (α*ε*sqrt (q+1))) := by
    simpa only [div_one,Function.comp_apply,Pi.div_apply] using (hR.const_mul α).mul
      (continuous_sqrt.continuousAt.tendsto.comp (tendsto_const_nhds.div hV one_ne_zero))
  have hC : Tendsto (fun y ↦ log (2*studentCDF (q+1) (α*r y*sqrt ((q+1)/V y))))
      atTop (𝓝 (log (2*studentCDF (q+1) (α*ε*sqrt (q+1))))) := by
    exact (((cdf_continuous (show 0 < q+1 by linarith)).continuousAt.tendsto.comp hK).const_mul 2).log
      (by have := cdf_pos (show 0 < q+1 by linarith) (α*ε*sqrt (q+1)); positivity)
  have hvLog := hV.log one_ne_zero
  have h := ((hvLog.const_mul (-(q+1)/2)).const_add
    (TDist.logNormalizer q-log s+(q+1)/2*log (q*s^2))).add hC
  have h' : Tendsto (fun y ↦ TDist.logNormalizer q-log s+(q+1)/2*log (q*s^2) +
      (-(q+1)/2)*log (V y) +
      log (2*studentCDF (q+1) (α*r y*sqrt ((q+1)/V y))))
      atTop (𝓝 (logTail q α s ε)) := by
    simpa only [log_one,mul_zero,add_zero,logTail] using h
  refine h'.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with y hy
  have hr : ε*y-m = y*r y := by unfold r; field_simp
  have hV' : 0 < V y := by unfold V; positivity
  have hfac : q*s^2+(ε*y-m)^2 = y^2*V y := by
    rw [hr]
    unfold V
    field_simp
  have hK : α*(ε*y-m)*sqrt ((q+1)/(q*s^2+(ε*y-m)^2)) =
      α*r y*sqrt ((q+1)/V y) := by
    rw [hfac,hr,show (q+1)/(y^2*V y) = ((q+1)/V y)/y^2 by ring, sqrt_div (by positivity),
      sqrt_sq hy.le]
    field_simp
  have hL : 1+(ε*y-m)^2/(q*s^2) = (y^2*V y)/(q*s^2) := by
    rw [← hfac]
    field_simp
  rw [spatialLogDensity,TDist.density,log_exp,hK,hL,
    log_div (by positivity : y^2*V y ≠ 0) (by positivity : q*s^2 ≠ 0),
    log_mul (by positivity : y^2 ≠ 0) hV'.ne', log_pow]
  ring

private theorem log_density_derivative {m q α s : ℝ} (hq : 0 < q) (hs : 0 < s) (y : ℝ) :
    DifferentiableAt ℝ (spatialLogDensity m q α s) y := by
  let z := fun y : ℝ ↦ α*(y-m)*sqrt ((q+1)/(q*s^2+(y-m)^2))
  have hz : DifferentiableAt ℝ z y := by
    unfold z
    apply DifferentiableAt.mul (by fun_prop)
    apply DifferentiableAt.sqrt _ (by positivity)
    exact (differentiableAt_const (c := q+1)).div (by fun_prop) (by positivity)
  have h1 : DifferentiableAt ℝ (fun y ↦ log (2*studentCDF (q+1) (z y))) y := by
    have hz' := (cdf_hasDerivAt (show 0 < q+1 by linarith) (z y)).differentiableAt.comp y hz
    exact (hz'.const_mul 2).log
      (mul_pos two_pos (cdf_pos (show 0 < q+1 by linarith) (z y))).ne'
  have h2 : DifferentiableAt ℝ (fun y ↦ log (TDist.density m q s y)) y := by
    simp_rw [TDist.density,log_exp]
    have hh : DifferentiableAt ℝ (fun y : ℝ ↦ log (1+(y-m)^2/(q*s^2))) y :=
      (by fun_prop : DifferentiableAt ℝ (fun y : ℝ ↦ 1+(y-m)^2/(q*s^2)) y).log (by positivity)
    exact (differentiableAt_const (c := TDist.logNormalizer q-log s)).sub (hh.const_mul ((q+1)/2))
  exact h2.add h1

/-- Spatial log-density score of a finite skew-Student fit. -/
noncomputable def spatialScore (m q α s y : ℝ) : ℝ :=
  deriv (spatialLogDensity m q α s) y

private theorem equal_scores_constant {m k q v α β s a : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a)
    (he : ∀ y, spatialScore m q α s y = spatialScore k v β a y) :
    ∃ C, ∀ y, spatialLogDensity m q α s y-spatialLogDensity k v β a y = C := by
  let F := fun y : ℝ ↦ spatialLogDensity m q α s y-spatialLogDensity k v β a y
  have hd (y : ℝ) : HasDerivAt F 0 y := by
    have h := (log_density_derivative (m := m) (α := α) hq hs y).hasDerivAt.sub
      (log_density_derivative (m := k) (α := β) hv ha y).hasDerivAt
    convert h using 1
    exact (sub_eq_zero.mpr (he y)).symm
  have ord (x y : ℝ) (hxy : x < y) : F x = F y := by
    obtain ⟨z,_,hz⟩ := exists_hasDerivAt_eq_slope F (fun _ ↦ 0) hxy
      (fun t _ ↦ (hd t).continuousAt.continuousWithinAt) (fun t _ ↦ hd t)
    exact (sub_eq_zero.mp ((div_eq_zero_iff.mp hz.symm).resolve_right (sub_ne_zero.mpr hxy.ne'))).symm
  refine ⟨F 0,fun y ↦ ?_⟩
  rcases lt_trichotomy y 0 with h | rfl | h
  · exact ord y 0 h
  · rfl
  · exact (ord 0 y h).symm

private theorem equal_log_differences_shape_slant {m k q v α β s a C : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a)
    (he : ∀ y, spatialLogDensity m q α s y-spatialLogDensity k v β a y = C) :
    q = v ∧ α = β := by
  have hqv : q = v := by
    have H := (log_density_tail hq hs (by norm_num : (1:ℝ)^2 = 1) m α).sub
      (log_density_tail hv ha (by norm_num : (1:ℝ)^2 = 1) k β)
    have hfin : Tendsto (fun y : ℝ ↦ C+(q-v)*log y) atTop
        (𝓝 (logTail q α s 1-logTail v β a 1)) := by
      refine H.congr' (Filter.Eventually.of_forall fun y ↦ ?_)
      simp only [one_mul]
      linear_combination he y
    rcases lt_trichotomy (q-v) 0 with h | h | h
    · exact (not_tendsto_atBot_of_tendsto_nhds hfin
        (tendsto_atBot_add_const_left _ C (tendsto_log_atTop.const_mul_atTop_of_neg h))).elim
    · exact sub_eq_zero.mp h
    · exact (not_tendsto_atTop_of_tendsto_nhds hfin
        (tendsto_atTop_add_const_left _ C (tendsto_log_atTop.const_mul_atTop h))).elim
  subst v
  have hlim {ε : ℝ} (hε : ε^2 = 1) : logTail q α s ε-logTail q β a ε = C := by
    have H := (log_density_tail hq hs hε m α).sub (log_density_tail hq ha hε k β)
    refine tendsto_nhds_unique H (tendsto_const_nhds.congr' ?_)
    exact Filter.Eventually.of_forall fun y ↦ by
      change C = (spatialLogDensity m q α s (ε*y)+(q+1)*log y)-
        (spatialLogDensity k q β a (ε*y)+(q+1)*log y)
      linear_combination -(he (ε*y))
  have hPlus := hlim (by norm_num : (1:ℝ)^2 = 1)
  have hMinus := hlim (by norm_num : (-1:ℝ)^2 = 1)
  unfold logTail at hPlus hMinus
  have hq' : 0 < q+1 := by linarith
  have hSQ : 0 < sqrt (q+1) := sqrt_pos.mpr hq'
  have hMono : StrictMono (fun z ↦ log (2*studentCDF (q+1) z)) := fun x y hxy ↦
    log_lt_log (mul_pos two_pos (cdf_pos hq' x))
      (mul_lt_mul_of_pos_left (cdf_strictMono hq' hxy) two_pos)
  refine ⟨rfl,?_⟩
  rcases lt_trichotomy α β with hh | hh | hh
  · have hP : α*1*sqrt (q+1) < β*1*sqrt (q+1) := by
      simpa using mul_lt_mul_of_pos_right hh hSQ
    have hM : β*(-1)*sqrt (q+1) < α*(-1)*sqrt (q+1) :=
      mul_lt_mul_of_pos_right (by linarith) hSQ
    exact False.elim (by linarith [hMono hP,hMono hM])
  · exact hh
  · have hP : β*1*sqrt (q+1) < α*1*sqrt (q+1) := by
      simpa using mul_lt_mul_of_pos_right hh hSQ
    have hM : α*(-1)*sqrt (q+1) < β*(-1)*sqrt (q+1) :=
      mul_lt_mul_of_pos_right (by linarith) hSQ
    exact False.elim (by linarith [hMono hP,hMono hM])


private theorem spatialLogDensity_standardize {m q α s z : ℝ} (hq : 0 < q) (hs : 0 < s) :
    spatialLogDensity m q α s (m+s*z) = spatialLogDensity 0 q α 1 z-log s := by
  unfold spatialLogDensity TDist.density
  simp only [log_exp,sub_zero,one_pow,mul_one,log_one]
  rw [show m+s*z-m = s*z by ring]
  have hratio : (s*z)^2/(q*s^2) = z^2/q := by field_simp
  have hsqrt : α*(s*z)*sqrt ((q+1)/(q*s^2+(s*z)^2)) =
      α*z*sqrt ((q+1)/(q+z^2)) := by
    rw [show q*s^2+(s*z)^2 = (q+z^2)*s^2 by ring,
      show (q+1)/((q+z^2)*s^2) = ((q+1)/(q+z^2))/s^2 by field_simp, sqrt_div (by positivity),
      sqrt_sq hs.le]
    field_simp
  rw [hratio,hsqrt]
  ring

/-- The finite skew-Student spatial scores identify all four coefficients.
The parameter separation also applies after restricting any subset of
coefficients to their held values. -/
theorem skew_spatial_score_separation {m k q v α β s a : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a)
    (he : ∀ y, spatialScore m q α s y = spatialScore k v β a y) :
    m = k ∧ q = v ∧ α = β ∧ s = a := by
  obtain ⟨C,hC⟩ := equal_scores_constant hq hv hs ha he
  obtain ⟨rfl,rfl⟩ := equal_log_differences_shape_slant hq hv hs ha hC
  have hlim : logTail q α s 1-logTail q α a 1 = C := by
    have H := (log_density_tail hq hs (by norm_num : (1:ℝ)^2=1) m α).sub
      (log_density_tail hq ha (by norm_num : (1:ℝ)^2=1) k α)
    refine tendsto_nhds_unique H (tendsto_const_nhds.congr' ?_)
    exact Filter.Eventually.of_forall fun y ↦ by simp only [one_mul]; linear_combination -(hC y)
  have hC' : C = q*(log s-log a) := by
    unfold logTail at hlim
    rw [log_mul hq.ne' (by positivity : s^2 ≠ 0), log_mul hq.ne' (by positivity : a^2 ≠ 0),
      log_pow,log_pow] at hlim
    linear_combination -hlim
  let g := spatialLogDensity 0 q α 1
  have hpos (y : ℝ) : 0 < 2*studentCDF (q+1) (α*y*sqrt ((q+1)/(q+y^2))) :=
    mul_pos two_pos (cdf_pos (by linarith) _)
  have hB : BddAbove (Set.range g) := by
    refine ⟨TDist.logNormalizer q+log 2,?_⟩
    rintro _ ⟨y,rfl⟩
    have h1 : 0 ≤ log (1+y^2/q) := log_nonneg (le_add_of_nonneg_right (by positivity))
    have h2 : log (2*studentCDF (q+1) (α*y*sqrt ((q+1)/(q+y^2)))) ≤ log 2 :=
      log_le_log (hpos y) (by
        simpa using mul_le_mul_of_nonneg_left (cdf_bounds (q+1) _).2 zero_le_two)
    unfold g spatialLogDensity TDist.density
    simp only [log_exp,log_one,sub_zero,one_pow,mul_one]
    linarith [mul_nonneg (show 0 ≤ (q+1)/2 by positivity) h1]
  have hu (y : ℝ) : g y ≤ sSup (Set.range g) := le_csSup hB ⟨y,rfl⟩
  have chart (y : ℝ) {b w : ℝ} (hw : 0 < w) :
      spatialLogDensity b q α w y = g ((y-b)/w)-log w := by
    have H := spatialLogDensity_standardize (m := b) (α := α) (z := (y-b)/w) hq hw
    convert H using 2
    field_simp
    ring
  let D := C+log s-log a
  have eqG (y : ℝ) : g ((y-m)/s)-g ((y-k)/a) = D := by
    have h := hC y
    rw [chart y hs,chart y ha] at h
    unfold D
    linarith
  have hb₁ (z : ℝ) : g z ≤ sSup (Set.range g)+D := by
    have h := eqG (m+s*z)
    have hh : (m+s*z-m)/s = z := by field_simp; ring
    rw [hh] at h
    linarith [hu ((m+s*z-k)/a)]
  have hb₂ (z : ℝ) : g z ≤ sSup (Set.range g)-D := by
    have h := eqG (k+a*z)
    have hh : (k+a*z-k)/a = z := by field_simp; ring
    rw [hh] at h
    linarith [hu ((k+a*z-m)/s)]
  have hD : D = 0 := by
    have h1 := csSup_le (Set.range_nonempty g) (show ∀ u ∈ Set.range g, u ≤ sSup (Set.range g)+D from
      fun _ ⟨z,hz⟩ ↦ hz ▸ hb₁ z)
    have h2 := csSup_le (Set.range_nonempty g) (show ∀ u ∈ Set.range g, u ≤ sSup (Set.range g)-D from
      fun _ ⟨z,hz⟩ ↦ hz ▸ hb₂ z)
    linarith
  have hsa : s = a := by
    have h : (q+1)*(log s-log a) = 0 := by dsimp [D] at hD; linarith
    have h' := (mul_eq_zero.mp h).resolve_left (show q+1 ≠ 0 by positivity)
    exact Real.log_injOn_pos hs ha (sub_eq_zero.mp h')
  subst a
  refine ⟨?_,rfl,rfl,rfl⟩
  by_contra hmk
  let δ := (m-k)/s
  have hδ : δ ≠ 0 := div_ne_zero (sub_ne_zero.mpr hmk) hs.ne'
  have periodic (z : ℝ) : g (z+δ) = g z := by
    have h := eqG (m+s*z)
    rw [hD,show (m+s*z-m)/s = z by field_simp; ring,
      show (m+s*z-k)/s = z+δ by dsimp [δ]; field_simp; ring] at h
    exact (sub_eq_zero.mp h).symm
  have perAbs (z : ℝ) : g (z+|δ|) = g z := by
    rcases le_total 0 δ with hh | hh
    · rw [abs_of_nonneg hh]
      exact periodic z
    · rw [abs_of_nonpos hh]
      have h := periodic (z-δ)
      rw [sub_add_cancel] at h
      simpa only [sub_eq_add_neg] using h.symm
  have hNat (n : ℕ) : g ((n:ℝ)*|δ|) = g 0 := by
    induction n with
    | zero => simp
    | succ n ih =>
      rw [Nat.cast_succ,add_mul,one_mul,perAbs,ih]
  have hN : Tendsto (fun n : ℕ ↦ (n : ℝ)*|δ|) atTop atTop :=
    tendsto_natCast_atTop_atTop.atTop_mul_const (abs_pos.mpr hδ)
  have hL := (log_density_tail hq zero_lt_one (by norm_num : (1:ℝ)^2=1) 0 α).comp hN
  have hT : Tendsto (fun n : ℕ ↦ g 0+(q+1)*log ((n:ℝ)*|δ|)) atTop
      (𝓝 (logTail q α 1 1)) := by
    refine hL.congr' (Filter.Eventually.of_forall fun n ↦ ?_)
    change g (1*((n:ℝ)*|δ|))+(q+1)*log ((n:ℝ)*|δ|) = _
    rw [one_mul,hNat]
  exact (not_tendsto_atTop_of_tendsto_nhds hT)
    (tendsto_atTop_add_const_left _ (g 0)
      ((tendsto_log_atTop.comp hN).const_mul_atTop (by positivity : 0 < q+1)))


private theorem cdf_analyticAt {q : ℝ} (hq : 0 < q) (x : ℝ) :
    AnalyticAt ℝ (studentCDF q) x := by
  let U := fun z : ℂ ↦ 1+z^2/(q : ℂ)
  let N := ∫ t, TDist.density 0 q 1 t
  let f := fun z : ℂ ↦ Complex.exp ((TDist.logNormalizer q : ℂ) -
    ((q+1)/2 : ℂ)*Complex.log (U z)) / (N : ℂ)
  have hh (t : ℝ) : U t = ((1+t^2/q : ℝ) : ℂ) := by unfold U; norm_cast
  have hu : U x ∈ Complex.slitPlane := by
    rw [hh,Complex.ofReal_mem_slitPlane]
    exact zero_lt_one.trans_le (le_add_of_nonneg_right (by positivity))
  have hU : Continuous U := by unfold U; fun_prop
  obtain ⟨r,hr,hsub⟩ := Metric.mem_nhds_iff.mp
    (hU.continuousAt (Complex.isOpen_slitPlane.mem_nhds hu))
  have hf : DifferentiableOn ℂ f (Metric.ball (x : ℂ) r) := by
    intro z hz
    have hL : AnalyticAt ℂ (fun y ↦ Complex.log (U y)) z :=
      (by unfold U; fun_prop : AnalyticAt ℂ U z).clog (hsub hz)
    have hE : AnalyticAt ℂ (fun y ↦ (TDist.logNormalizer q : ℂ) -
          ((q+1)/2 : ℂ)*Complex.log (U y)) z := analyticAt_const.sub (analyticAt_const.mul hL)
    have hF : AnalyticAt ℂ f z := hE.cexp.div_const
    exact hF.differentiableAt.differentiableWithinAt
  obtain ⟨g,hg⟩ := hf.isExactOn_ball
  have hgA : AnalyticAt ℂ g (x : ℂ) := by
    apply Complex.analyticAt_iff_eventually_differentiableAt.mpr
    filter_upwards [Metric.ball_mem_nhds _ hr] with z hz
    exact (hg z hz).differentiableAt
  have hmatch (t : ℝ) : (f t).re = TDist.density 0 q 1 t/N := by
    have he : f t = ((exp (TDist.logNormalizer q - ((q+1)/2)*log (1+t^2/q))/N : ℝ) : ℂ) := by
      unfold f
      push_cast
      rw [hh, ← Complex.ofReal_log (by positivity : 0 ≤ 1+t^2/q)]
    rw [he,Complex.ofReal_re]
    simp [TDist.density]
  let G := fun t : ℝ ↦ (g t).re
  let C := studentCDF q x-G x
  have hA : AnalyticAt ℝ (fun t ↦ G t+C) x := (hgA.re_ofReal).add analyticAt_const
  apply hA.congr
  filter_upwards [Metric.ball_mem_nhds x hr] with t ht
  have hdiff (z : ℝ) (hz : z ∈ Metric.ball x r) :
      HasDerivAt (fun u ↦ studentCDF q u-G u) 0 z := by
    have hz' : (z : ℂ) ∈ Metric.ball (x : ℂ) r := by
      rwa [Metric.mem_ball,Complex.isometry_ofReal.dist_eq]
    have h := (cdf_hasDerivAt hq z).sub ((hg (z : ℂ) hz').real_of_complex)
    have he : TDist.density 0 q 1 z/N - (f z).re = 0 := by rw [hmatch,sub_self]
    exact (h.congr_deriv he)
  have hx : x ∈ Metric.ball x r := Metric.mem_ball_self hr
  have ord {a b : ℝ} (ha : a ∈ Metric.ball x r) (hb : b ∈ Metric.ball x r) (hab : a < b) :
      studentCDF q a-G a = studentCDF q b-G b := by
    have hI : Icc a b ⊆ Metric.ball x r := by
      intro u hu
      rw [Metric.mem_ball,Real.dist_eq,abs_lt] at ha hb ⊢
      exact ⟨by linarith [hu.1],by linarith [hu.2]⟩
    obtain ⟨z,_,hz⟩ := exists_hasDerivAt_eq_slope _ (fun _ ↦ 0) hab
      (fun u hu ↦ (hdiff u (hI hu)).continuousAt.continuousWithinAt)
      (fun u hu ↦ hdiff u (hI (Ioo_subset_Icc_self hu)))
    exact (sub_eq_zero.mp ((div_eq_zero_iff.mp hz.symm).resolve_right
      (sub_ne_zero.mpr hab.ne'))).symm
  have he : studentCDF q t-G t = studentCDF q x-G x := by
    rcases lt_trichotomy t x with h | rfl | h
    · exact ord ht hx h
    · rfl
    · exact (ord hx ht h).symm
  dsimp [C]
  linarith

/-- The finite skew-Student spatial score is analytic at every
observation, for every positive shape and scale and any finite slant. -/
theorem skew_spatial_score_analytic {m q α s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    AnalyticOnNhd ℝ (spatialScore m q α s) univ := by
  intro y _
  let z := fun y : ℝ ↦ α*(y-m)*sqrt ((q+1)/(q*s^2+(y-m)^2))
  have hz : AnalyticAt ℝ z y := by
    unfold z
    apply AnalyticAt.mul (by fun_prop)
    let w := fun y : ℝ ↦ (q+1)/(q*s^2+(y-m)^2)
    have hw : AnalyticAt ℝ w y := by
      unfold w
      exact analyticAt_const.div (by fun_prop) (by positivity)
    have h (t : ℝ) : 0 < w t := by unfold w; positivity
    have hz := (((hw.log (h y)).mul analyticAt_const).rexp' :
      AnalyticAt ℝ (fun t ↦ exp (log (w t)*(1/2))) y)
    apply hz.congr
    filter_upwards with t
    rw [sqrt_eq_rpow, rpow_def_of_pos (h t)]
    rfl
  have h1 : AnalyticAt ℝ (fun y ↦ log (2*studentCDF (q+1) (z y))) y := by
    have hz' := (cdf_analyticAt (show 0 < q+1 by linarith) (z y)).comp (f := z) hz
    exact (analyticAt_const.mul hz').log
      (mul_pos two_pos (cdf_pos (show 0 < q+1 by linarith) (z y)))
  have h2 : AnalyticAt ℝ (fun y ↦ log (TDist.density m q s y)) y := by
    simp_rw [TDist.density,log_exp]
    have hh : AnalyticAt ℝ (fun y : ℝ ↦ log (1+(y-m)^2/(q*s^2))) y :=
      (by fun_prop : AnalyticAt ℝ (fun y : ℝ ↦ 1+(y-m)^2/(q*s^2)) y).log (by positivity)
    exact analyticAt_const.sub (analyticAt_const.mul hh)
  exact (h2.add h1).deriv


/-- Distinct finite skew-Student parameters cannot have equal spatial
scores on any observation neighborhood. -/
theorem skew_scores_identified_on_neighborhood {m k q v α β s a z : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a)
    (he : ∀ᶠ y in 𝓝 z, spatialScore m q α s y = spatialScore k v β a y) :
    m = k ∧ q = v ∧ α = β ∧ s = a := by
  have hh : ∃ᶠ y in 𝓝[≠] z, spatialScore m q α s y = spatialScore k v β a y :=
    (he.filter_mono nhdsWithin_le_nhds).frequently
  have hall := (skew_spatial_score_analytic (m := m) (α := α) hq hs).eqOn_of_preconnected_of_frequently_eq
    (skew_spatial_score_analytic (m := k) (α := β) hv ha) isPreconnected_univ (mem_univ z) hh
  exact skew_spatial_score_separation hq hv hs ha (fun x ↦ hall (mem_univ x))


private noncomputable def studentTwoRatio (x : ℝ) : ℝ := x/sqrt (2+x^2)

private theorem studentTwoRatio_deriv (x : ℝ) :
    HasDerivAt studentTwoRatio (2/(sqrt (2+x^2))^3) x := by
  have hB : 0 < 2+x^2 := by positivity
  have hS := ((((hasDerivAt_id x).pow 2).const_add (2:ℝ)).sqrt hB.ne')
  have h := (hasDerivAt_id x).div hS (sqrt_pos.mpr hB).ne'
  convert h using 1
  · rfl
  · simp only [id_eq,Nat.cast_ofNat, Nat.reduceSub,pow_one,mul_one,Pi.pow_apply]
    generalize hR : sqrt (2+x^2) = r
    have hr : 0 < r := hR ▸ sqrt_pos.mpr hB
    have hr2 : r^2 = 2+x^2 := hR ▸ sq_sqrt hB.le
    field_simp [hr.ne']
    nlinarith

private theorem studentTwoRatio_atTop : Tendsto studentTwoRatio atTop (𝓝 1) := by
  have h : Tendsto (fun t : ℝ ↦ sqrt (1+2*(t⁻¹)^2)) atTop (𝓝 1) := by
    have hh : Tendsto (fun t : ℝ ↦ 1+2*(t⁻¹)^2) atTop (𝓝 1) := by
      simpa only [zero_pow two_ne_zero,mul_zero,add_zero] using
        (((tendsto_inv_atTop_zero.pow 2).const_mul (2:ℝ)).const_add 1)
    have h' := (continuous_sqrt.continuousAt (x := (1:ℝ))).tendsto.comp hh
    rw [sqrt_one] at h'
    exact h'.congr' (Filter.Eventually.of_forall fun _ ↦ rfl)
  have hI : Tendsto (fun t : ℝ ↦ (sqrt (1+2*(t⁻¹)^2))⁻¹) atTop (𝓝 1) := by
    simpa only [inv_one] using h.inv₀ one_ne_zero
  refine hI.congr' ?_
  filter_upwards [eventually_gt_atTop (0:ℝ)] with t ht
  unfold studentTwoRatio
  have he : 2+t^2 = (1+2*(t⁻¹)^2)*t^2 := by field_simp; ring
  rw [he,sqrt_mul (by positivity),sqrt_sq ht.le]
  field_simp

private theorem student_two_density (t : ℝ) :
    TDist.density 0 2 1 t = exp (TDist.logNormalizer 2)*(sqrt 2)^3/(sqrt (2+t^2))^3 := by
  have hi : 0 < 1+t^2/2 := by positivity
  have hs : 0 < sqrt (1+t^2/2) := sqrt_pos.mpr hi
  rw [TDist.density]
  simp only [log_one,sub_zero,one_pow,mul_one]
  have hE : exp (-((2+1)/2:ℝ)*log (1+t^2/2)) = 1/(sqrt (1+t^2/2))^3 := by
    rw [show -((2+1)/2:ℝ)*log (1+t^2/2) = -(3*log (sqrt (1+t^2/2))) by
      rw [log_sqrt hi.le]; ring,exp_neg,show (3:ℝ)=((3:ℕ):ℝ) by norm_num,exp_nat_mul,exp_log hs,one_div]
  rw [sub_eq_add_neg,exp_add,← neg_mul,hE,
    show 1+t^2/2 = (2+t^2)/2 by ring,sqrt_div (by positivity)]
  field_simp

/-- Closed Student CDF at shape two, valid at both signs and at zero. -/
theorem student_cdf_two (t : ℝ) :
    2*studentCDF 2 t = 1+t/sqrt (2+t^2) := by
  let N := ∫ y, TDist.density 0 2 1 y
  let C := exp (TDist.logNormalizer 2)*(sqrt 2)^3/(2*N)
  let F := fun z ↦ studentCDF 2 z-C*studentTwoRatio z
  have hd (z : ℝ) : HasDerivAt F 0 z := by
    have h := (cdf_hasDerivAt (by norm_num : (0:ℝ)<2) z).sub ((studentTwoRatio_deriv z).const_mul C)
    have hh : TDist.density 0 2 1 z/N-C*(2/(sqrt (2+z^2))^3)=0 := by
      rw [student_two_density]
      unfold C
      ring
    exact h.congr_deriv hh
  have h0 : F 0 = 1/2 := by
    unfold F studentTwoRatio
    rw [cdf_zero (by norm_num : (0:ℝ)<2)]
    ring
  have hor (x y : ℝ) (hxy : x < y) : F x = F y := by
    obtain ⟨z,_,hz⟩ := exists_hasDerivAt_eq_slope F (fun _ ↦ 0) hxy
      (fun t _ ↦ (hd t).continuousAt.continuousWithinAt) (fun t _ ↦ hd t)
    exact (sub_eq_zero.mp ((div_eq_zero_iff.mp hz.symm).resolve_right (sub_ne_zero.mpr hxy.ne'))).symm
  have hF (z : ℝ) : studentCDF 2 z = C*studentTwoRatio z+1/2 := by
    have hh : F z = F 0 := by
      rcases lt_trichotomy z 0 with h | rfl | h
      · exact hor z 0 h
      · rfl
      · exact (hor 0 z h).symm
    rw [h0] at hh
    dsimp [F] at hh
    linarith
  have hC : C = 1/2 := by
    have hh := (studentTwoRatio_atTop.const_mul C).add_const (1/2 : ℝ)
    have he := (cdf_atTop (by norm_num : (0:ℝ)<2)).congr (fun z ↦ hF z)
    have hy := tendsto_nhds_unique hh he
    linarith
  rw [hF,hC]
  unfold studentTwoRatio
  ring

/-- Shape-one skew-Student density after dividing out its fixed
normalizing coefficient. -/
theorem skew_cauchy_single_term {m a α y : ℝ} (ha : 0 < a) :
    exp (spatialLogDensity m 1 α a y) =
      exp (TDist.logNormalizer 1) * (a/(a^2+(y-m)^2)) *
        (1+α*(y-m)/sqrt (a^2+(1+α^2)*(y-m)^2)) := by
  let r := y-m
  have hQ : 0 < a^2+r^2 := by positivity
  have hA : 0 < a^2+(1+α^2)*r^2 := by positivity
  change exp (log (TDist.density m 1 a y) +
    log (2*studentCDF (1+1) (α*r*sqrt ((1+1)/(1*a^2+r^2))))) = _
  rw [exp_add,exp_log (by unfold TDist.density; positivity),
    exp_log (mul_pos two_pos (cdf_pos (by norm_num : (0:ℝ)<1+1) _))]
  rw [show (1:ℝ)+1 = 2 by norm_num,student_cdf_two]
  have hsk : (α*r*sqrt (2/(1*a^2+r^2)))/
      sqrt (2+(α*r*sqrt (2/(1*a^2+r^2)))^2) =
      α*r/sqrt (a^2+(1+α^2)*r^2) := by
    rw [one_mul,sqrt_div (by norm_num)]
    have h2 : (α*r*(sqrt 2/sqrt (a^2+r^2)))^2 =
        2*α^2*r^2/(a^2+r^2) := by
      rw [mul_pow,div_pow,sq_sqrt (by norm_num),sq_sqrt hQ.le]
      ring
    rw [h2,show 2+2*α^2*r^2/(a^2+r^2) =
      (2*(a^2+(1+α^2)*r^2))/(a^2+r^2) by field_simp; ring,
      sqrt_div (by positivity),sqrt_mul (by norm_num)]
    field_simp
  rw [hsk]
  unfold TDist.density
  change exp (TDist.logNormalizer 1-log a-((1+1)/2)*log (1+r^2/(1*a^2))) *
    (1+α*r/sqrt (a^2+(1+α^2)*r^2)) = _
  rw [show ((1+1)/2:ℝ)=1 by norm_num,one_mul,one_mul,exp_sub,exp_sub,exp_log ha,
    exp_log (by positivity : 0 < 1+r^2/a^2)]
  change _ = exp (TDist.logNormalizer 1)*(a/(a^2+r^2))*(1+α*r/sqrt (a^2+(1+α^2)*r^2))
  field_simp

/-- The noncontact collapse trace score cannot agree locally with a
finite skew-Student score. Clearing the pole would give a nonzero
analytic constant at the collapsed atom. -/
theorem skew_collapse_score_separated {m q α s b k z : ℝ} (hq : 0 < q) (hs : 0 < s)
    (hk : 0 < k) (hz : z ≠ b) :
    ¬ (∀ᶠ y in 𝓝 z, spatialScore m q α s y = -k/(y-b)) := by
  intro he
  have ha := skew_spatial_score_analytic (m := m) (α := α) hq hs
  let G := fun y ↦ (y-b)*spatialScore m q α s y
  have hG : AnalyticOnNhd ℝ G univ := fun y hy ↦
    (by fun_prop : AnalyticAt ℝ (fun x : ℝ ↦ x-b) y).mul (ha y hy)
  have hK : AnalyticOnNhd ℝ (fun _ : ℝ ↦ -k) univ := fun _ _ ↦ analyticAt_const
  have he' : ∀ᶠ y in 𝓝 z, G y = -k := by
    filter_upwards [he,isOpen_compl_singleton.mem_nhds (show z ∈ ({b}:Set ℝ)ᶜ from hz)] with y h hy
    have hby : y-b ≠ 0 := sub_ne_zero.mpr hy
    unfold G
    rw [h]
    field_simp
  have hh : ∃ᶠ y in 𝓝[≠] z, G y = -k := (he'.filter_mono nhdsWithin_le_nhds).frequently
  have heq := hG.eqOn_of_preconnected_of_frequently_eq hK isPreconnected_univ (mem_univ z) hh (mem_univ b)
  simp [G] at heq
  linarith


/-- Zero slant evaluates exactly to the ordinary Student likelihood.
This transports score candidates and their exact winning heights. -/
theorem zero_slant_likelihood_eq {n : ℕ} (x : Fin n → ℝ) (m a : ℝ) {q : ℝ} (hq : 0 < q) :
    StudentSkew.likelihood x m q 0 a = TDist.likelihood x m q a := by
  exact zero_slant_identity x m a hq

/-- At a held location, sign pairing bounds the skew-t likelihood above
by the ordinary Student likelihood for every positive shape and scale. -/
theorem paired_likelihood_le_student {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    (e : Equiv.Perm (Fin n)) (he : ∀ i, x (e i)-m = -(x i-m))
    {q s : ℝ} (hq : 0 < q) (α : ℝ) :
    StudentSkew.likelihood x m q α s ≤ TDist.likelihood x m q s := by
  let z (i : Fin n) := α*(x i-m)*sqrt ((q+1)/(q*s^2+(x i-m)^2))
  let P (i : Fin n) := 2*studentCDF (q+1) (z i)
  have hq' : 0 < q+1 := by positivity
  have hPos (i : Fin n) : 0 < P i := mul_pos two_pos (cdf_pos hq' _)
  have hz (i : Fin n) : z (e i) = -z i := by dsimp [z]; rw [he,neg_sq]; ring
  have hp (i : Fin n) : P (e i) = 2-P i := by unfold P; rw [hz,cdf_reflect hq']; ring
  have hle (i : Fin n) : P i*P (e i) ≤ 1 := by rw [hp]; nlinarith [sq_nonneg (P i-1)]
  have HH := Finset.prod_le_prod₀ (s := Finset.univ)
    (fun i _ ↦ mul_nonneg (hPos i).le (hPos (e i)).le) (fun i _ ↦ hle i)
  rw [Finset.prod_mul_distrib,Equiv.prod_comp e P,← pow_two,Finset.prod_const_one] at HH
  have hp1 : (∏ i, P i) ≤ 1 := by
    have h0 : 0 ≤ ∏ i, P i := Finset.prod_nonneg (fun i _ ↦ (hPos i).le)
    exact (sq_le_sq₀ h0 zero_le_one).1 (by simpa using HH)
  change TDist.likelihood x m q s*(∏ i, P i) ≤ TDist.likelihood x m q s
  exact mul_le_of_le_one_right (exp_pos _).le hp1

/-- The sample likelihood is the product of the positive point
densities underlying the spatial-score chart. -/
theorem skew_likelihood_spatial_product {n : ℕ} (x : Fin n → ℝ)
    (m α : ℝ) {q s : ℝ} (hq : 0 < q) :
    StudentSkew.likelihood x m q α s =
      ∏ i, exp (spatialLogDensity m q α s (x i)) := by
  unfold StudentSkew.likelihood TDist.likelihood TDist.logLik
  rw [exp_sum,← Finset.prod_mul_distrib]
  apply Finset.prod_congr rfl
  intro i _
  unfold spatialLogDensity
  rw [exp_add,exp_log (by unfold TDist.density; positivity),
    exp_log (mul_pos two_pos (cdf_pos (show 0 < q+1 by positivity) _))]
  rfl


/-- The ordinary standardized Student density integrates to one on the
entire fitted shape branch. This also identifies the denominator in
the normalized Student CDF. -/
theorem fitted_student_density_mass {q : ℝ} (hq : 1 ≤ q) :
    (∫ x : ℝ, TDist.density 0 q 1 x) = 1 := by
  have hq0 : 0 < q := zero_lt_one.trans_le hq
  let h := (q+1)/2
  let K : ℝ → ℝ := fun t ↦ t^(h-1)*exp (-t)
  let f := fun t x : ℝ ↦ K t*exp (-(t/q)*x^2)
  let μ := volume.restrict (Ioi (0:ℝ))
  have hh : 0 < h := by unfold h; positivity
  have hK : Continuous K := by
    unfold K
    exact (continuous_rpow_const (show 0 ≤ h-1 by unfold h; linarith)).mul
      (by fun_prop)
  have hX {t : ℝ} (ht : 0 < t) :
      (∫ x, f t x) = sqrt (π*q)*(t^(q/2-1)*exp (-t)) := by
    have hz : 0 < t/q := div_pos ht hq0
    unfold f
    rw [integral_const_mul,integral_gaussian]
    have harg : π/(t/q) = π*q/t := by field_simp
    rw [harg,sqrt_div (by positivity),show sqrt t = t^(1/2:ℝ) from sqrt_eq_rpow t]
    unfold K
    have he : t^(h-1)/t^(1/2:ℝ) = t^(q/2-1) := by
      rw [← rpow_sub ht]
      congr 1
      unfold h
      ring
    linear_combination sqrt (π*q)*exp (-t)*he
  have hmeas : AEStronglyMeasurable (Function.uncurry f) (μ.prod volume) := by
    apply Continuous.aestronglyMeasurable
    change Continuous (fun p : ℝ × ℝ ↦ K p.1*exp (-(p.1/q)*p.2^2))
    fun_prop
  have hInt : Integrable (Function.uncurry f) (μ.prod volume) := by
    rw [integrable_prod_iff hmeas]
    constructor
    · filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
      have ht' : 0 < t := ht
      exact (integrable_exp_neg_mul_sq (div_pos ht' hq0)).const_mul (K t)
    · have i : Integrable (fun t ↦ sqrt (π*q)*(t^(q/2-1)*exp (-t))) μ := by
        have hi := integrableOn_rpow_mul_exp_neg_mul_rpow (s := q/2-1) (p := 1) (b := 1)
          (by linarith) zero_lt_one zero_lt_one
        have hi' : Integrable (fun t ↦ t^(q/2-1)*exp (-t)) μ := by
          apply Integrable.congr hi
          exact ae_of_all _ fun t ↦ by simp
        exact hi'.const_mul _
      apply Integrable.congr i
      filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
      have ht' : 0 < t := ht
      have ho : ∀ y, ‖f t y‖ = f t y := fun y ↦ Real.norm_of_nonneg (by
        unfold f K
        exact mul_nonneg (mul_nonneg (rpow_nonneg ht'.le _) (exp_pos _).le) (exp_pos _).le)
      simpa only [Function.uncurry_apply_pair,ho] using (hX ht').symm
  have hT (x : ℝ) : (∫ t, f t x ∂μ) = (1/(1+x^2/q))^h*Gamma h := by
    have he : (fun t ↦ f t x) = fun t ↦ t^(h-1)*exp (-((1+x^2/q)*t)) := by
      funext t
      unfold f K
      rw [mul_assoc,← exp_add]
      congr 2
      ring
    unfold μ
    rw [he]
    exact integral_rpow_mul_exp_neg_mul_Ioi hh (by positivity)
  have hN : exp (TDist.logNormalizer q) =
      Gamma h/(Gamma (q/2)*sqrt (q*π)) := by
    unfold TDist.logNormalizer h
    rw [exp_sub,exp_sub,exp_log (Gamma_pos_of_pos (by positivity)),
      exp_log (Gamma_pos_of_pos (by positivity))]
    have hh : exp (log (q*π)/2) = sqrt (q*π) := by
      rw [sqrt_eq_rpow,rpow_def_of_pos (by positivity)]
      congr 1
      ring
    rw [hh,div_div]
  let C := 1/(Gamma (q/2)*sqrt (q*π))
  have hD (x : ℝ) : TDist.density 0 q 1 x = C*(∫ t, f t x ∂μ) := by
    unfold TDist.density
    simp only [log_one,sub_zero,one_pow,mul_one]
    rw [sub_eq_add_neg,exp_add,hN,hT]
    unfold C
    have hp : 0 < 1+x^2/q := by positivity
    rw [rpow_def_of_pos (by positivity),log_div one_ne_zero hp.ne',log_one,zero_sub]
    have he : -log (1+x^2/q)*h = -(h*log (1+x^2/q)) := by ring
    rw [he]
    change (Gamma h/(Gamma (q/2)*sqrt (q*π)))*exp (-(h*log (1+x^2/q))) = _
    ring
  simp_rw [hD]
  rw [integral_const_mul,← integral_integral_swap hInt]
  have hi : (∫ t, (∫ x, f t x) ∂μ) = sqrt (π*q)*Gamma (q/2) := by
    have he : ∀ᵐ t ∂μ, (∫ x, f t x) = sqrt (π*q)*(t^(q/2-1)*exp (-t)) :=
      (ae_restrict_iff' measurableSet_Ioi).2
        (ae_of_all volume fun t (ht : t ∈ Ioi (0:ℝ)) ↦ hX (mem_Ioi.mp ht))
    rw [integral_congr_ae he,integral_const_mul]
    change sqrt (π*q)*(∫ t in Ioi (0:ℝ), t^(q/2-1)*exp (-t)) = _
    congr 1
    rw [Gamma_eq_integral (half_pos hq0)]
    apply integral_congr_ae
    exact ae_of_all _ fun t ↦ mul_comm _ _
  rw [hi, mul_comm π q]
  unfold C
  field_simp



private theorem fitted_density_log_shape_upper {q x : ℝ} (hq : 1 ≤ q) :
    deriv (fun v ↦ log (TDist.density 0 v 1 x)) q ≤ 5/4 := by
  have hp := zero_lt_one.trans_le hq
  rw [(Student2WidthWitness.ordinary_log_density_shape_deriv (z := x) hp).deriv]
  have hN := (Student2Half.student_log_normalizer_score_bounds hp).2
  have hN' : 1/(4*q^2) ≤ (1:ℝ)/4 := by
    apply div_le_div_of_nonneg_left (by norm_num) (by norm_num)
    nlinarith [mul_nonneg (sub_nonneg.mpr hq) (show 0 ≤ q+1 by positivity)]
  have hG : (q+1)*x^2/(2*q*(q+x^2)) ≤ 1 := by
    apply (div_le_one (by positivity)).mpr
    have h := mul_nonneg (sub_nonneg.mpr hq) (sq_nonneg x)
    nlinarith [mul_pos (show 0<2*q by positivity) hp]
  have hL : 0 ≤ log (1+x^2/q) := log_nonneg (by linarith [div_nonneg (sq_nonneg x) hp.le])
  linarith

/-- On the fitted branch, increasing shape by a finite amount can
increase a fixed-argument Student CDF by at most this exponential factor. -/
theorem fitted_student_cdf_shape_ratio {q v : ℝ} (hq : 1 ≤ q) (hv : q ≤ v) (z : ℝ) :
    studentCDF v z ≤ exp ((5/4:ℝ)*(v-q))*studentCDF q z := by
  have hq' := zero_lt_one.trans_le hq
  have hv' := hq'.trans_le hv
  let C := exp ((5/4:ℝ)*(v-q))
  have hPoint (x : ℝ) : TDist.density 0 v 1 x ≤ C*TDist.density 0 q 1 x := by
    let G := fun t ↦ log (TDist.density 0 t 1 x)-(5/4:ℝ)*t
    have H {t : ℝ} (ht : 0 < t) :
        HasDerivAt G (deriv (fun v ↦ log (TDist.density 0 v 1 x)) t-5/4) t := by
      have h := (((Student2WidthWitness.ordinary_log_density_shape_deriv (z := x) ht).differentiableAt.hasDerivAt).sub
        ((hasDerivAt_id t).const_mul (5/4:ℝ)))
      change HasDerivAt G (_-((5/4:ℝ)*1)) t at h
      simpa only [mul_one] using h
    have hA : AntitoneOn G (Icc q v) := by
      apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
      · exact fun t ht ↦ (H (hq'.trans_le ht.1)).continuousAt.continuousWithinAt
      · intro t ht
        rw [interior_Icc] at ht
        exact (H (hq'.trans ht.1)).differentiableAt.differentiableWithinAt
      · intro t ht
        rw [interior_Icc] at ht
        rw [(H (hq'.trans ht.1)).deriv]
        exact sub_nonpos.mpr (fitted_density_log_shape_upper (hq.trans ht.1.le))
    have h := hA ⟨le_rfl,hv⟩ ⟨hv,le_rfl⟩ hv
    change log (TDist.density 0 v 1 x)-(5/4:ℝ)*v ≤
      log (TDist.density 0 q 1 x)-(5/4:ℝ)*q at h
    unfold C
    rw [← exp_log (student_positive v x),← exp_log (student_positive q x),
      ← exp_add,exp_le_exp]
    linarith
  unfold studentCDF
  rw [fitted_student_density_mass hq,fitted_student_density_mass (hq.trans hv),div_one,div_one,
    ← integral_const_mul]
  exact integral_mono ((student_integrable hv').integrableOn)
    (((student_integrable hq').integrableOn (s := Iic z)).const_mul C) hPoint

/-- For positive arguments, growth of the fitted Student CDF is at
most proportional to growth of its argument. -/
theorem fitted_student_cdf_argument_ratio {q a b : ℝ} (hq : 1 ≤ q) (ha : 0 < a) (hab : a ≤ b) :
    studentCDF q b ≤ (b/a)*studentCDF q a := by
  have hq' := zero_lt_one.trans_le hq
  let f := TDist.density 0 q 1
  have hf (z : ℝ) (hz : 0 < z) : z*f z ≤ studentCDF q z := by
    have hPoint (t : ℝ) (ht : t ∈ Ioc (0:ℝ) z) : f z ≤ f t := by
      unfold f TDist.density
      simp only [sub_zero,log_one,one_pow,mul_one]
      rw [exp_le_exp]
      have hsq : t^2 ≤ z^2 := pow_le_pow_left₀ ht.1.le ht.2 2
      have h := log_le_log (show 0 < 1+t^2/q by positivity)
        (show 1+t^2/q ≤ 1+z^2/q by gcongr)
      linarith [mul_le_mul_of_nonneg_left h (show 0 ≤ (q+1)/2 by positivity)]
    have hI : (∫ t in Ioc (0:ℝ) z, f z) ≤ ∫ t in Ioc (0:ℝ) z, f t := by
      apply integral_mono_ae
      · exact integrableOn_const (hs := by simp [Real.volume_Ioc, lt_top_iff_ne_top])
      · exact (student_integrable hq').integrableOn
      · exact (ae_restrict_iff' measurableSet_Ioc).2 (ae_of_all _ hPoint)
    have hC : (∫ _ in Ioc (0:ℝ) z, f z) = z*f z := by
      rw [setIntegral_const,Measure.real,Real.volume_Ioc,sub_zero,ENNReal.toReal_ofReal hz.le,smul_eq_mul]
    rw [hC] at hI
    apply hI.trans
    unfold studentCDF
    rw [fitted_student_density_mass hq,div_one]
    exact setIntegral_mono_set ((student_integrable hq').integrableOn)
      (Eventually.of_forall (fun _ ↦ (exp_pos _).le)) (Eventually.of_forall (fun _ h ↦ h.2))
  let G := fun z ↦ studentCDF q z/z
  have hD (z : ℝ) (hz : 0 < z) :
      HasDerivAt G ((f z*z-studentCDF q z)/z^2) z := by
    have h := cdf_hasDerivAt hq' z
    rw [fitted_student_density_mass hq,div_one] at h
    have H := h.fun_div (hasDerivAt_id z) hz.ne'
    change HasDerivAt G ((f z*z-studentCDF q z*1)/z^2) z at H
    simpa only [mul_one] using H
  have hA : AntitoneOn G (Ioi 0) := by
    apply antitoneOn_of_deriv_nonpos (convex_Ioi _)
    · exact fun z hz ↦ (hD z hz).continuousAt.continuousWithinAt
    · intro z hz
      rw [interior_Ioi] at hz
      exact (hD z hz).differentiableAt.differentiableWithinAt
    · intro z hz
      rw [interior_Ioi] at hz
      rw [(hD z hz).deriv]
      exact div_nonpos_of_nonpos_of_nonneg (by linarith [hf z hz]) (sq_nonneg _)
  have h := hA ha (ha.trans_le hab) hab
  change studentCDF q b/b ≤ studentCDF q a/a at h
  have hB := ha.trans_le hab
  have H := (div_le_div_iff₀ hB ha).1 h
  rw [div_mul_eq_mul_div]
  exact (le_div_iff₀ ha).2 (by simpa only [mul_comm b] using H)


/-- Along the positive-residual chart, moving the skew-CDF argument
as shape rises from one adds at most one quarter of the shape
increment to the log-CDF upper comparison. -/
theorem positive_slant_cdf_shape_floor_ratio {q A s x : ℝ} (hq : 1 ≤ q)
    (hA : 0 < A) (hs : 0 < s) (hx : 0 < x) :
    studentCDF (q+1) (A*x*sqrt ((q+1)/(q*s^2+x^2))) ≤
      exp ((3/2:ℝ)*(q-1))*studentCDF 2 (A*x*sqrt (2/(s^2+x^2))) := by
  let a := A*x*sqrt (2/(s^2+x^2))
  let b := A*x*sqrt ((q+1)/(q*s^2+x^2))
  have ha : 0 < a := by unfold a; positivity
  have hb : 0 < b := by unfold b; positivity
  have hq0 : 0 < q := zero_lt_one.trans_le hq
  have hbound : b ≤ exp ((q-1)/4)*a := by
    have hR : (q+1)/(q*s^2+x^2) ≤ ((q+1)/2)*(2/(s^2+x^2)) := by
      rw [show ((q+1)/2)*(2/(s^2+x^2)) = (q+1)/(s^2+x^2) by ring]
      exact div_le_div_of_nonneg_left (by positivity) (by positivity)
        (by nlinarith [mul_nonneg (sub_nonneg.mpr hq) (sq_nonneg s)])
    have hS : sqrt ((q+1)/2) ≤ exp ((q-1)/4) := by
      rw [sqrt_eq_rpow,rpow_def_of_pos (by positivity),exp_le_exp]
      linarith [log_le_sub_one_of_pos (show 0 < (q+1)/2 by positivity)]
    calc
      b ≤ A*x*sqrt (((q+1)/2)*(2/(s^2+x^2))) := by
        unfold b
        exact mul_le_mul_of_nonneg_left (sqrt_le_sqrt hR) (by positivity)
      _ = sqrt ((q+1)/2)*a := by
        rw [sqrt_mul (by positivity)]
        unfold a
        ring
      _ ≤ _ := mul_le_mul_of_nonneg_right hS ha.le
  have hArg : studentCDF 2 b ≤ exp ((q-1)/4)*studentCDF 2 a := by
    by_cases H : a ≤ b
    · refine (fitted_student_cdf_argument_ratio (by norm_num : (1:ℝ)≤2) ha H).trans ?_
      exact mul_le_mul_of_nonneg_right ((div_le_iff₀ ha).2 hbound) (cdf_bounds 2 a).1
    · refine (cdf_strictMono (by norm_num : (0:ℝ)<2)).monotone (le_of_not_ge H) |>.trans ?_
      exact le_mul_of_one_le_left (cdf_bounds 2 a).1 (one_le_exp (by linarith))
  have h := fitted_student_cdf_shape_ratio (by norm_num : (1:ℝ)≤2)
    (show (2:ℝ) ≤ q+1 by linarith) b
  calc
    _ ≤ exp ((5/4:ℝ)*(q+1-2))*studentCDF 2 b := h
    _ ≤ exp ((5/4:ℝ)*(q+1-2))*(exp ((q-1)/4)*studentCDF 2 a) :=
      mul_le_mul_of_nonneg_left hArg (exp_pos _).le
    _ = _ := by rw [← mul_assoc,← exp_add]; congr 2 <;> ring


private theorem gaussian_cdf_analytic (x : ℝ) : AnalyticAt ℝ SkewLSGaussian.Φ x := by
  let f := fun z : ℂ ↦ Complex.exp (-(z^2/2))/(sqrt (2*π) : ℝ)
  have hf : Differentiable ℂ f := by unfold f; fun_prop
  obtain ⟨g,hg⟩ := hf.isExactOn_univ
  have hgA : AnalyticAt ℂ g (x:ℂ) := by
    apply Complex.analyticAt_iff_eventually_differentiableAt.mpr
    exact Eventually.of_forall fun z ↦ (hg z (mem_univ z)).differentiableAt
  let G := fun t : ℝ ↦ (g t).re
  have hdiff (t : ℝ) : HasDerivAt (fun u ↦ SkewLSGaussian.Φ u-G u) 0 t := by
    have hd := (SkewLSGaussian.cdf_hasDerivAt t).sub ((hg (t:ℂ) (mem_univ _)).real_of_complex)
    apply hd.congr_deriv
    rw [SkewLSGaussian.phi_formula]
    have hF : (f t).re = exp (-(t^2/2))/sqrt (2*π) := by
      unfold f
      push_cast
      rw [← Complex.ofReal_pow,← Complex.ofReal_ofNat,← Complex.ofReal_div,
        ← Complex.ofReal_neg,← Complex.ofReal_exp,← Complex.ofReal_div,Complex.ofReal_re]
    rw [hF]
    ring
  let C := SkewLSGaussian.Φ x-G x
  have hA : AnalyticAt ℝ (fun t ↦ G t+C) x := (hgA.re_ofReal).add analyticAt_const
  apply hA.congr
  exact Eventually.of_forall fun t ↦ by
    have h := is_const_of_deriv_eq_zero
      (fun u ↦ (hdiff u).differentiableAt) (fun u ↦ (hdiff u).deriv) t x
    change SkewLSGaussian.Φ t-G t = C at h
    linarith

/-- The finite skew-normal log density with location, positive scale
and finite slant, used to compare observation scores. -/
noncomputable def skewNormalSpatialLogDensity (b a β y : ℝ) : ℝ :=
  log (TDist.normalDensity b a y)+log (2*SkewLSGaussian.Φ (β*((y-b)/a)))

/-- Polynomial skew-t tails and the Gaussian tail bound rule out
finite/skew-normal spatial score agreement on any neighborhood. -/
theorem skew_normal_score_separated {m q α s b a β z : ℝ} (hq : 0 < q) (hs : 0 < s)
    (ha : 0 < a) :
    ¬ (∀ᶠ y in 𝓝 z, spatialScore m q α s y = deriv (skewNormalSpatialLogDensity b a β) y) := by
  let F := fun y ↦ spatialLogDensity m q α s y-skewNormalSpatialLogDensity b a β y
  have hA (y : ℝ) : AnalyticAt ℝ (skewNormalSpatialLogDensity b a β) y := by
    unfold skewNormalSpatialLogDensity TDist.normalDensity
    simp only [log_exp]
    have hArg : AnalyticAt ℝ (fun u : ℝ ↦ β*((u-b)/a)) y := by fun_prop
    have hF := (gaussian_cdf_analytic (β*((y-b)/a))).comp (f := fun u ↦ β*((u-b)/a)) hArg
    have hL := (analyticAt_const.mul hF).log (mul_pos two_pos (SkewLSGaussian.cdf_pos _))
    exact (by fun_prop : AnalyticAt ℝ
      (fun u : ℝ ↦ -(log (2*π)/2)-log a-((u-b)^2/a^2)/2) y).add hL
  intro he
  have hSame := (skew_spatial_score_analytic (m := m) (α := α) hq hs).eqOn_of_preconnected_of_frequently_eq
    (fun y _ ↦ (hA y).deriv) isPreconnected_univ (mem_univ z)
    ((he.filter_mono nhdsWithin_le_nhds).frequently)
  have hDiff (y : ℝ) : HasDerivAt F 0 y := by
    have h := ((log_density_derivative (m := m) (α := α) hq hs y).hasDerivAt).sub
      (hA y).differentiableAt.hasDerivAt
    apply h.congr_deriv
    exact sub_eq_zero.mpr (hSame (mem_univ y))
  have hConst (y : ℝ) : F y = F 0 := is_const_of_deriv_eq_zero
    (fun u ↦ (hDiff u).differentiableAt) (fun u ↦ (hDiff u).deriv) y 0
  have hTail : Tendsto (fun y : ℝ ↦ spatialLogDensity m q α s y+(q+1)*log y)
      atTop (𝓝 (logTail q α s 1)) := by
    simpa only [one_mul] using log_density_tail hq hs (ε := 1) (by norm_num) m α
  let C := logTail q α s 1-1
  let N := -(log (2*π)/2)-log a+log 2
  have hUpper (y : ℝ) : skewNormalSpatialLogDensity b a β y ≤ N-(y-b)^2/(2*a^2) := by
    unfold skewNormalSpatialLogDensity TDist.normalDensity N
    rw [log_exp]
    have h := log_le_log (mul_pos two_pos (SkewLSGaussian.cdf_pos (β*((y-b)/a))))
      (mul_le_of_le_one_right zero_le_two (SkewLSGaussian.cdf_lt_one _).le)
    linarith [show ((y-b)^2/a^2)/2=(y-b)^2/(2*a^2) by ring]
  have hEventually : ∀ᶠ y : ℝ in atTop, C < spatialLogDensity m q α s y+(q+1)*log y :=
    hTail.eventually (Ioi_mem_nhds (by unfold C; linarith))
  have ht1 : 0 < 8*a^2*(q+2) := by positivity
  have ht2 : 0 < abs b+1 := by positivity
  have hAll : ∀ᶠ y : ℝ in atTop, C < spatialLogDensity m q α s y+(q+1)*log y ∧
      2*(abs b+1) ≤ y ∧ 8*a^2*(q+2) ≤ y ∧ N+F 0-C+1 ≤ y := by
    filter_upwards [hEventually,eventually_ge_atTop (2*(abs b+1)),
      eventually_ge_atTop (8*a^2*(q+2)),eventually_ge_atTop (N+F 0-C+1)] with y h1 h2 h3 h4
    exact ⟨h1,h2,h3,h4⟩
  obtain ⟨y,hy,h1,h2,h3⟩ := hAll.exists
  have hy0 : 0 < y := (by positivity : 0 < 2*(abs b+1)).trans_le h1
  have hY : y/2 ≤ y-b := by linarith [le_abs_self b]
  have hQ : (y/2)^2 ≤ (y-b)^2 := pow_le_pow_left₀ (by positivity) hY 2
  have hR : (q+2)*y ≤ (y-b)^2/(2*a^2) := by
    apply (le_div_iff₀ (by positivity)).2
    have h := mul_le_mul_of_nonneg_right h2 hy0.le
    nlinarith only [h,hQ]
  have hLog : log y ≤ y := (log_le_sub_one_of_pos hy0).trans (by linarith)
  have hLog' := mul_le_mul_of_nonneg_left hLog (show 0 ≤ q+1 by positivity)
  have hC := hConst y
  have hU := hUpper y
  unfold F at hC h3
  linarith

/-- An oriented half-Student has the base Student score on its strict
support. Matching a finite skew-t score there forces the same base
parameters and zero slant. -/
theorem skew_half_student_score_identified {m q α s b v a z : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a)
    (he : ∀ᶠ y in 𝓝 z, spatialScore m q α s y =
      deriv (fun t ↦ log (TDist.density b v a t)) y) :
    m = b ∧ q = v ∧ α = 0 ∧ s = a := by
  apply skew_scores_identified_on_neighborhood hq hv hs ha
  filter_upwards [he] with y hy
  have ht : spatialLogDensity b v 0 a = (fun t ↦ log (TDist.density b v a t)) := by
    funext t
    simp [spatialLogDensity,cdf_zero (show 0 < v+1 by positivity)]
  simpa only [spatialScore,ht] using hy


/-- If finite and oriented half-Student scores match on a
neighborhood, the base parameters coincide and the oriented face
has strictly greater likelihood whenever the sample is compatible
and has a strict-support observation. -/
theorem skew_half_student_equal_score_strict_height {n : ℕ} (x : Fin n → ℝ)
    {m q α s b v a z ε c : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a) (hc : 1 ≤ c)
    (hSupport : ∀ i, x i = b ∨ 0 < ε*(x i-b)) (hSome : ∃ i, 0 < ε*(x i-b))
    (he : ∀ᶠ y in 𝓝 z, spatialScore m q α s y =
      deriv (fun t ↦ log (TDist.density b v a t)) y) :
    likelihood x m q α s < TDist.likelihood x b v a * orientedWeight x b ε c := by
  obtain ⟨hm,hqv,hα,hsa⟩ := skew_half_student_score_identified hq hv hs ha he
  subst m q α s
  rw [zero_slant_likelihood_eq _ _ _ hv]
  have hW : 1 < orientedWeight x b ε c := by
    unfold orientedWeight
    rw [← Finset.prod_const_one (s := (Finset.univ : Finset (Fin n)))]
    apply Finset.prod_lt_prod₀
    · exact fun _ _ ↦ zero_lt_one
    · intro i _
      rcases hSupport i with h | h
      · simpa [h] using hc
      · simp [h]
    · obtain ⟨i,hi⟩ := hSome
      exact ⟨i,Finset.mem_univ i,by simp [hi]⟩
  exact lt_mul_of_one_lt_right (exp_pos _) hW


end UnivariateML.Student2Skew
end
