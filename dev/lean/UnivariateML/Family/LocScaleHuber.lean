module

public import UnivariateML.Family.Huber
public import UnivariateML.Family.ClosedForm
public import UnivariateML.Family.LocScaleCommon
public import Mathlib.Analysis.SpecialFunctions.Gaussian.GaussianIntegral

import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Analysis.Convex.Deriv
import Mathlib.Analysis.Real.Pi.Bounds
import Mathlib.MeasureTheory.Integral.IntegralEqImproper
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Free-tuning normalized Huber likelihoods

The normalizer is included as a function of the cutoff, so comparisons
across cutoffs use the normalized family.

## Deviations

The free-nuisance results here give the constant-data branches, face
paths, conditional-scale uniqueness and the exact held-scale example.
They leave the general profiled stationary-shape comparison and
exhaustion of the free-nuisance ends open. At held location and scale
the stationary list is finite, including the loss joins.
-/

@[expose] public section

open Set Filter Real MeasureTheory
open scoped Topology Interval
open UnivariateML.Huber

namespace UnivariateML.LocScaleHuber

/-- Normalizer of the standardized Huber density at positive cutoff. -/
noncomputable def normalizer (c : ℝ) : ℝ :=
  2 * (∫ t in (0 : ℝ)..c, exp (-(t^2/2))) + 2 * exp (-(c^2/2)) / c

/-- Normalized log likelihood with cutoff, location and scale arguments. -/
noncomputable def logLikelihood {n : ℕ} (x : Fin n → ℝ) (c m s : ℝ) : ℝ :=
  ∑ j, (-log (s * normalizer c) - rho c ((x j - m)/s))

private theorem normalizer_pos {c : ℝ} (hc : 0 < c) : 0 < normalizer c := by
  unfold normalizer
  have h : 0 ≤ ∫ t in (0 : ℝ)..c, exp (-(t^2/2)) :=
    intervalIntegral.integral_nonneg_of_forall hc.le (fun t ↦ (exp_pos _).le)
  positivity

private theorem normalizer_transfer {n : ℕ} (x : Fin n → ℝ) {c m s : ℝ}
    (hc : 0 < c) (hs : 0 < s) :
    logLikelihood x c m s = Huber.logLik c x (m,s) - n * log (normalizer c) := by
  unfold logLikelihood Huber.logLik LocationScale.logLik
  have he (j : Fin n) : -log (s*normalizer c) - rho c ((x j-m)/s) =
      (-log s - rho c ((x j-m)/s)) - log (normalizer c) := by
    rw [log_mul hs.ne' (normalizer_pos hc).ne']
    ring
  rw [Finset.sum_congr rfl (fun j _ ↦ he j), Finset.sum_sub_distrib]
  simp [nsmul_eq_mul]

/-- Constant data at the common location have unbounded normalized
likelihood even with any positive tuning cutoff held along the path. -/
theorem constant_scale_collapse {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {m c : ℝ} (hc : 0 < c) (hx : ∀ j, x j = m) :
    ∀ B : ℝ, ∃ s > 0, B < logLikelihood x c m s := by
  intro B
  obtain ⟨s,hs,h⟩ := heldLocation_unbounded hn (ε := c) hx (B + n * log (normalizer c))
  refine ⟨s,hs,?_⟩
  rw [normalizer_transfer x hc hs]
  linarith

/-- With both location and scale free on constant data, the normalized
likelihood is unbounded and has no finite maximizing tuple. -/
theorem all_free_constant_failure {n : ℕ} (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    let S := Ioi (0 : ℝ) ×ˢ LocationScale.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ logLikelihood x p.1 p.2.1 p.2.2
    (¬ BddAbove (L '' S)) ∧ ¬ ∃ p ∈ S, IsMaxOn L S p := by
  let x : Fin n → ℝ := fun _ ↦ m
  let S := Ioi (0 : ℝ) ×ˢ LocationScale.domain
  let L (p : ℝ × (ℝ × ℝ)) := logLikelihood x p.1 p.2.1 p.2.2
  have h : ¬ BddAbove (L '' S) := by
    rintro ⟨B,hB⟩
    obtain ⟨s,hs,hlt⟩ := constant_scale_collapse hn x zero_lt_one (fun _ ↦ rfl) B
    have hp : (1,m,s) ∈ S := ⟨zero_lt_one,mem_univ _,hs⟩
    exact hlt.not_ge (hB (mem_image_of_mem L hp))
  exact ⟨h, fun ⟨_,hp,hm⟩ ↦ h ⟨_, (hm.isLUB hp).1⟩⟩

private theorem normalizer_atTop : Tendsto normalizer atTop (𝓝 (sqrt (2*π))) := by
  have hi : Integrable (fun t : ℝ ↦ exp (-(t^2/2))) := by
    convert integrable_exp_neg_mul_sq (show (0 : ℝ) < 1/2 by norm_num) using 1
    ext t
    congr 1
    ring
  have hg : (∫ t in Ioi (0 : ℝ), exp (-(t^2/2))) = sqrt (2*π)/2 := by
    convert integral_gaussian_Ioi (1/2) using 1
    · apply integral_congr_ae
      exact Eventually.of_forall fun t ↦ by ring
    · congr 2
      ring
  have hI : Tendsto (fun c : ℝ ↦ 2 * ∫ t in (0 : ℝ)..c, exp (-(t^2/2))) atTop
      (𝓝 (sqrt (2*π))) := by
    have h := (intervalIntegral_tendsto_integral_Ioi (0 : ℝ) hi.integrableOn tendsto_id).const_mul 2
    rw [hg, mul_div_cancel₀ _ (by norm_num : (2 : ℝ) ≠ 0)] at h
    exact h
  have ht : Tendsto (fun c : ℝ ↦ 2*exp (-(c^2/2))/c) atTop (𝓝 0) := by
    have hb : Tendsto (fun c : ℝ ↦ 2/c) atTop (𝓝 0) := by
      simpa only [div_eq_mul_inv, mul_zero] using
        (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul 2
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hb ?_ ?_
    · exact (eventually_gt_atTop (0 : ℝ)).mono fun _ hc ↦ by positivity
    · refine (eventually_gt_atTop (0 : ℝ)).mono fun c hc ↦ ?_
      apply div_le_div_of_nonneg_right _ hc.le
      exact mul_le_of_le_one_right (by norm_num) (exp_le_one_iff.mpr (by nlinarith))
  have h := hI.add ht
  rw [add_zero] at h
  exact h

/-- At fixed location and positive scale, increasing cutoff without bound
gives the matched normal log likelihood. -/
theorem normal_face_path {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    Tendsto (fun c ↦ logLikelihood x c m s) atTop (𝓝 (Normal.logLik x m s)) := by
  have hπ : 0 < 2*π := by positivity
  have hn := (normalizer_atTop.const_mul s).log (mul_pos hs (sqrt_pos.mpr hπ)).ne'
  have hterm (j : Fin n) :
      Tendsto (fun c ↦ -log (s*normalizer c) - rho c ((x j-m)/s)) atTop
        (𝓝 (-log (s*sqrt (2*π)) - ((x j-m)/s)^2/2)) := by
    have h : Tendsto (fun c ↦ -log (s*normalizer c) - ((x j-m)/s)^2/2) atTop
        (𝓝 (-log (s*sqrt (2*π)) - ((x j-m)/s)^2/2)) := by
      exact hn.neg.sub_const (((x j-m)/s)^2/2)
    refine h.congr' ?_
    filter_upwards [eventually_ge_atTop |(x j-m)/s|] with c hc
    have hc' : 0 ≤ c := (abs_nonneg _).trans hc
    rw [rho_eq_piecewise hc', ite_eq_left hc]
  have h := tendsto_finsetSum Finset.univ (fun j _ ↦ hterm j)
  have he : (∑ j, (-log (s*sqrt (2*π)) - ((x j-m)/s)^2/2)) = Normal.logLik x m s := by
    rw [Normal.logLik_formula x m hs]
    simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
      nsmul_eq_mul, log_mul hs.ne' (sqrt_pos.mpr hπ).ne', log_sqrt hπ.le, div_pow,
      Normal.residual]
    rw [← Finset.sum_div, ← Finset.sum_div]
    ring
  rwa [he] at h

private theorem normalizer_mul_at_zero :
    Tendsto (fun c ↦ c * normalizer c) (𝓝[>] 0) (𝓝 2) := by
  have hf : Continuous (fun t : ℝ ↦ exp (-(t^2/2))) := by fun_prop
  have hi : Tendsto (fun c : ℝ ↦ ∫ t in (0 : ℝ)..c, exp (-(t^2/2))) (𝓝[>] 0) (𝓝 0) := by
    have h : Continuous (fun c : ℝ ↦ ∫ t in (0 : ℝ)..c, exp (-(t^2/2))) :=
      intervalIntegral.continuous_primitive (fun a b ↦ hf.intervalIntegrable a b) 0
    simpa using h.continuousAt (x := 0) |>.tendsto.mono_left nhdsWithin_le_nhds
  have h0 : Tendsto (fun c : ℝ ↦ c) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hE : Tendsto (fun c : ℝ ↦ 2*exp (-(c^2/2))) (𝓝[>] 0) (𝓝 2) := by
    have hc : Continuous (fun c : ℝ ↦ 2*exp (-(c^2/2))) := by fun_prop
    simpa using hc.continuousAt (x := (0 : ℝ)) |>.tendsto.mono_left nhdsWithin_le_nhds
  have ht := (h0.mul (hi.const_mul 2)).add hE
  simp only [mul_zero, zero_add] at ht
  refine ht.congr' ?_
  filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with c hc
  unfold normalizer
  have hc' : 0 < c := hc
  field_simp [hc'.ne']

private theorem rho_laplace_bounds {c z : ℝ} (hc : 0 ≤ c) :
    c*|z| - c^2/2 ≤ rho c z ∧ rho c z ≤ c*|z| := by
  rw [rho_eq_piecewise hc]
  split_ifs with h
  · constructor
    · nlinarith [sq_nonneg (|z|-c), sq_abs z]
    · have hp := mul_nonneg (abs_nonneg z) (sub_nonneg.mpr h)
      nlinarith [sq_abs z, sq_nonneg z]
  · constructor <;> nlinarith [sq_nonneg c]

/-- Shrinking Huber cutoff with scale proportional to cutoff gives the
matched Laplace log likelihood. -/
theorem laplace_face_path {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {b : ℝ} (hb : 0 < b) :
    Tendsto (fun c ↦ logLikelihood x c m (c*b)) (𝓝[>] 0) (𝓝 (Laplace.logLik x m b)) := by
  have h := (normalizer_mul_at_zero.const_mul b).log (by positivity : b*2 ≠ 0)
  have hlog : Tendsto (fun c ↦ -log (c*b*normalizer c)) (𝓝[>] 0) (𝓝 (-log (2*b))) := by
    rw [mul_comm 2 b]
    exact h.neg.congr fun c ↦ by rw [show b * (c * normalizer c) =
      c*b*normalizer c by ring]
  have hterm (j : Fin n) :
      Tendsto (fun c ↦ rho c ((x j-m)/(c*b))) (𝓝[>] 0) (𝓝 (|x j-m|/b)) := by
    have h0 : Tendsto (fun c : ℝ ↦ c) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
    have hlo : Tendsto (fun c ↦ |x j-m|/b-c^2/2) (𝓝[>] 0) (𝓝 (|x j-m|/b)) := by
      simpa using ((h0.pow 2).div_const 2).const_sub (|x j-m|/b)
    have hevent : ∀ᶠ c : ℝ in 𝓝[>] 0, c ∈ Ioi 0 := self_mem_nhdsWithin
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' hlo tendsto_const_nhds ?_ ?_
    · refine hevent.mono fun c hc ↦ ?_
      have hc' : 0 < c := hc
      have he : c*|((x j-m)/(c*b))| = |x j-m|/b := by
        rw [abs_div, abs_of_pos (mul_pos hc' hb)]
        field_simp [hc'.ne',hb.ne']
      exact he ▸ (rho_laplace_bounds (z := (x j-m)/(c*b)) hc'.le).1
    · refine hevent.mono fun c hc ↦ ?_
      have hc' : 0 < c := hc
      have he : c*|((x j-m)/(c*b))| = |x j-m|/b := by
        rw [abs_div, abs_of_pos (mul_pos hc' hb)]
        field_simp [hc'.ne',hb.ne']
      exact he ▸ (rho_laplace_bounds (z := (x j-m)/(c*b)) hc'.le).2
  exact tendsto_finsetSum Finset.univ (fun j _ ↦ hlog.sub (hterm j))

private theorem normalizer_hasDerivAt {c : ℝ} (hc : 0 < c) :
    HasDerivAt normalizer (-(2*exp (-(c^2/2)))/c^2) c := by
  let f (t : ℝ) := exp (-(t^2/2))
  have hf : Continuous f := by unfold f; fun_prop
  have hI := intervalIntegral.integral_hasDerivAt_right (hf.intervalIntegrable 0 c)
    hf.stronglyMeasurable.stronglyMeasurableAtFilter hf.continuousAt
  have hE : HasDerivAt f (-c*f c) c := by
    convert (((((hasDerivAt_id c).pow 2).div_const 2).neg).exp) using 1
    · rfl
    · dsimp [f]
      ring
  have hd := (hI.const_mul (2 : ℝ)).add
    ((hE.const_mul (2 : ℝ)).div (hasDerivAt_id c) hc.ne')
  convert hd using 1
  · rfl
  · dsimp [f]
    field_simp [hc.ne']
    ring

private theorem normalizer_gt_normal {c : ℝ} (hc : 0 < c) : sqrt (2*π) < normalizer c := by
  have ha : StrictAntiOn normalizer (Ioi 0) := by
    apply strictAntiOn_of_deriv_neg (convex_Ioi 0)
      (fun t ht ↦ (normalizer_hasDerivAt ht).continuousAt.continuousWithinAt)
    intro t ht
    have ht' : 0 < t := interior_subset ht
    rw [(normalizer_hasDerivAt ht').deriv]
    exact div_neg_of_neg_of_pos (neg_neg_of_pos (mul_pos (by norm_num) (exp_pos _))) (by positivity)
  have hle : sqrt (2*π) ≤ normalizer (c+1) := by
    apply le_of_tendsto normalizer_atTop
    exact (eventually_ge_atTop (c+1)).mono (fun t ht ↦
      ha.antitoneOn (show c+1 ∈ Ioi (0 : ℝ) from (lt_add_one c).trans' hc) (by
        change 0 < t
        linarith) ht)
  exact hle.trans_lt (ha hc (show c+1 ∈ Ioi (0 : ℝ) from by change 0 < c+1; positivity) (lt_add_one c))

private theorem rho_nonneg {c z : ℝ} (hc : 0 ≤ c) : 0 ≤ rho c z := by
  rw [rho_eq_piecewise hc]
  split_ifs with h
  · positivity
  · exact mul_nonneg hc (by linarith [le_of_not_ge h])

/-- At positive held scale the small-cutoff likelihood vanishes, even
when all residuals are zero. This is stated in log likelihood units. -/
theorem held_scale_small_shape {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    Tendsto (fun c ↦ logLikelihood x c m s) (𝓝[>] 0) atBot := by
  have hZ : Tendsto normalizer (𝓝[>] 0) atTop := by
    have h := normalizer_mul_at_zero.pos_mul_atTop (by norm_num) tendsto_inv_nhdsGT_zero
    refine h.congr' ?_
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with c hc
    have hc' : 0 < c := hc
    change c * normalizer c * c⁻¹ = normalizer c
    field_simp [hc'.ne']
  have hL : Tendsto (fun c ↦ -(n : ℝ)*log (s*normalizer c)) (𝓝[>] 0) atBot := by
    have hl := tendsto_log_atTop.comp (hZ.const_mul_atTop hs)
    exact hl.const_mul_atTop_of_neg (neg_neg_of_pos (show (0 : ℝ) < n by exact_mod_cast hn))
  refine tendsto_atBot_mono' _ ?_ hL
  filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with c hc
  unfold logLikelihood
  calc
    _ ≤ ∑ _ : Fin n, -log (s*normalizer c) :=
      Finset.sum_le_sum (fun j _ ↦ sub_le_self _ (rho_nonneg (le_of_lt hc)))
    _ = _ := by simp [nsmul_eq_mul]

private theorem held_scale_objective_cont {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    ContinuousOn (fun c ↦ logLikelihood x c m s) (Ioi 0) := by
  unfold logLikelihood
  apply continuousOn_finsetSum Finset.univ
  intro j _
  have hZ : ContinuousOn normalizer (Ioi 0) :=
    fun c hc ↦ (normalizer_hasDerivAt hc).continuousAt.continuousWithinAt
  have hlog := (hZ.const_mul s).log (fun c hc ↦ (mul_pos hs (normalizer_pos hc)).ne')
  have hR : Continuous (fun c : ℝ ↦ rho c ((x j-m)/s)) := by unfold rho clip; fun_prop
  exact hlog.neg.sub hR.continuousOn

private theorem held_both_finite_iff_face {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    let L := fun c ↦ logLikelihood x c m s
    let B := Normal.logLik x m s
    (∃ c ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) c) ↔ ∃ c ∈ Ioi 0, B ≤ L c := by
  let L (c : ℝ) := logLikelihood x c m s
  let B := Normal.logLik x m s
  change (∃ c ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) c) ↔ ∃ c ∈ Ioi 0, B ≤ L c
  constructor
  · rintro ⟨c,hc,hm⟩
    exact ⟨c,hc,le_of_tendsto (normal_face_path x m hs)
      ((eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ hm ht)⟩
  · rintro ⟨c,hc,hB⟩
    by_cases hbetter : ∃ t > 0, B < L t
    · obtain ⟨t,ht,htB⟩ := hbetter
      let W := L t
      have hlow : ∀ᶠ c in 𝓝[>] (0 : ℝ), L c < W :=
        (held_scale_small_shape hn x m hs).eventually_lt_atBot W
      obtain ⟨d, hd, hsmall⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp hlow
      obtain ⟨b, hb⟩ := eventually_atTop.mp
        ((normal_face_path x m hs).eventually (eventually_lt_nhds htB))
      let lo := min t d
      let hi := max t b
      have hsub : Icc lo hi ⊆ Ioi (0 : ℝ) := fun _ h ↦ (lt_min ht hd).trans_le h.1
      have htK : t ∈ Icc lo hi := ⟨min_le_left _ _,le_max_left _ _⟩
      obtain ⟨q,hq,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨t,htK⟩
        ((held_scale_objective_cont x m hs).mono hsub)
      refine ⟨q,hsub hq,fun a ha ↦ ?_⟩
      by_cases ha' : a ∈ Icc lo hi
      · exact hm ha'
      have hout : L a < W := by
        by_cases halo : a < lo
        · exact hsmall ⟨ha,halo.trans_le (min_le_right _ _)⟩
        · exact hb a ((le_max_right t b).trans (lt_of_not_ge
            (fun h ↦ ha' ⟨le_of_not_gt halo,h⟩)).le)
      exact hout.le.trans (hm htK)
    · exact ⟨c,hc,fun a ha ↦ (le_of_not_gt (fun h ↦ hbetter ⟨a,ha,h⟩)).trans hB⟩

private theorem loss_reduction {c z : ℝ} (hc : 0 ≤ c) :
    z^2/2 - rho c z = (max (|z|-c) 0)^2/2 := by
  rw [rho_eq_piecewise hc]
  by_cases h : |z| ≤ c
  · rw [ite_eq_left h,max_eq_right (sub_nonpos.mpr h)]
    ring
  · rw [ite_eq_right h,max_eq_left (sub_nonneg.mpr (le_of_not_ge h))]
    nlinarith [sq_abs z]

private theorem held_both_face_iff_inequality {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {s c : ℝ} (hs : 0 < s) (hc : 0 < c) :
    Normal.logLik x m s ≤ logLikelihood x c m s ↔
      (∑ j, (max (|(x j-m)/s|-c) 0)^2/2) ≥ n * log (normalizer c / sqrt (2*π)) := by
  have hπ : 0 < 2*π := by positivity
  have hh : Normal.logLik x m s =
      ∑ j, (-log (s*sqrt (2*π)) - ((x j-m)/s)^2/2) := by
    rw [Normal.logLik_formula x m hs]
    simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
      nsmul_eq_mul, log_mul hs.ne' (sqrt_pos.mpr hπ).ne', log_sqrt hπ.le, div_pow,
      Normal.residual]
    rw [← Finset.sum_div, ← Finset.sum_div]
    ring
  rw [hh, logLikelihood]
  have he : log (s*sqrt (2*π)) - log (s*normalizer c) =
      -log (normalizer c / sqrt (2*π)) := by
    rw [log_mul hs.ne' (sqrt_pos.mpr hπ).ne', log_mul hs.ne' (normalizer_pos hc).ne',
      log_div (normalizer_pos hc).ne' (sqrt_pos.mpr hπ).ne']
    ring
  have hsum := Finset.sum_congr (s₁ := Finset.univ) rfl (fun j (_ : j ∈ Finset.univ) ↦
    loss_reduction (z := (x j-m)/s) hc.le)
  rw [Finset.sum_sub_distrib] at hsum
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul]
  constructor <;> intro h <;> linarith only [h, hsum,
    congrArg (fun a : ℝ ↦ (n : ℝ)*a) he]

/-- At held location and scale, finite tuning attains exactly when the
loss reduction matches or exceeds the normalizer penalty at some cutoff. -/
theorem held_location_scale_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    (∃ c ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ logLikelihood x t m s) (Ioi 0) c) ↔
      ∃ c > 0, (∑ j, (max (|(x j-m)/s|-c) 0)^2/2) ≥ n * log (normalizer c / sqrt (2*π)) := by
  have h := held_both_finite_iff_face hn x m hs
  dsimp only at h
  rw [h]
  exact ⟨fun ⟨c,hc,hc'⟩ ↦ ⟨c,hc,(held_both_face_iff_inequality x m hs hc).mp hc'⟩,
    fun ⟨c,hc,hc'⟩ ↦ ⟨c,hc,(held_both_face_iff_inequality x m hs hc).mpr hc'⟩⟩

/-- With both nuisances held, any failure to attain has the matched normal
as the exact supremum, strictly above all finite cutoffs. -/
theorem held_location_scale_nonattainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    let L := fun c ↦ logLikelihood x c m s
    let B := Normal.logLik x m s
    (¬ ∃ c ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) c) ↔
      (∀ c ∈ Ioi (0 : ℝ), L c < B) ∧ IsLUB (L '' Ioi 0) B :=
  LocScaleCommon.nonattainment_iff_of_face (l := atTop) (path := id)
    (eventually_gt_atTop 0) (normal_face_path x m hs) (held_both_finite_iff_face hn x m hs)

/-- A nonzero residual gives exactly one conditional scale at each
positive cutoff with location held. -/
theorem held_location_conditional_scale {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {c : ℝ}
    (hc : 0 < c) (hx : ∃ i, x i ≠ m) :
    ∃! s, s ∈ Ioi 0 ∧ IsMaxOn (logLikelihood x c m) (Ioi 0) s := by
  have h (s : ℝ) : (s ∈ Ioi 0 ∧ IsMaxOn (logLikelihood x c m) (Ioi 0) s) ↔
      (0 < s ∧ IsMaxOn (fun t ↦ Huber.logLik c x (m,t)) (Ioi 0) s) := by
    constructor <;> rintro ⟨hs,hm⟩ <;> refine ⟨hs, fun t ht ↦ ?_⟩
    · have he := hm ht
      change logLikelihood x c m t ≤ logLikelihood x c m s at he
      change Huber.logLik c x (m,t) ≤ Huber.logLik c x (m,s)
      simpa only [normalizer_transfer x hc ht, normalizer_transfer x hc hs,
        sub_le_sub_iff_right] using he
    · change logLikelihood x c m t ≤ logLikelihood x c m s
      rw [normalizer_transfer x hc ht, normalizer_transfer x hc hs, sub_le_sub_iff_right]
      exact hm ht
  simpa only [h] using heldLocation_existsUnique hc hx

/-- At held scale and constant data every positive cutoff and location
strictly loses to the normal at the common observation. -/
theorem held_scale_constant_strict {n : ℕ} (hn : 0 < n) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∀ c > 0, ∀ u : ℝ, logLikelihood (fun _ : Fin n ↦ m) c u s <
      Normal.logLik (fun _ : Fin n ↦ m) m s := by
  intro c hc u
  have hπ : 0 < 2*π := by positivity
  have hlt : log (s*sqrt (2*π)) < log (s*normalizer c) :=
    log_lt_log (mul_pos hs (sqrt_pos.mpr hπ))
      (mul_lt_mul_of_pos_left (normalizer_gt_normal hc) hs)
  have hnorm : Normal.logLik (fun _ : Fin n ↦ m) m s = -(n : ℝ)*log (s*sqrt (2*π)) := by
    rw [Normal.logLik_formula _ _ hs, log_mul hs.ne' (sqrt_pos.mpr hπ).ne', log_sqrt hπ.le]
    simp only [Normal.residual, sub_self, zero_pow (by decide : 2 ≠ 0), Finset.sum_const_zero,
      zero_div,sub_zero]
    ring
  unfold logLikelihood
  rw [hnorm]
  let : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
  calc
    _ < ∑ _ : Fin n, -log (s*sqrt (2*π)) := by
      apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
      intro j _
      linarith [rho_nonneg (z := (m-u)/s) hc.le]
    _ = _ := by simp [nsmul_eq_mul]

/-- The symmetric sample used in the held-scale Huber counterexample. -/
def separatedPair : Fin 2 → ℝ := ![-10,10]

private theorem normalizer_upper {c : ℝ} (hc : 0 < c) :
    normalizer c ≤ 2*c + 2/c := by
  have hf : Continuous (fun t : ℝ ↦ exp (-(t^2/2))) := by fun_prop
  have hi : (∫ t in (0 : ℝ)..c, exp (-(t^2/2))) ≤ c := by
    have h1 : IntervalIntegrable (fun _ : ℝ ↦ (1 : ℝ)) volume 0 c :=
      continuous_const.intervalIntegrable _ _
    have h := intervalIntegral.integral_mono_on hc.le (hf.intervalIntegrable _ _) h1
      (fun t _ ↦ exp_le_one_iff.mpr (by nlinarith [sq_nonneg t]))
    simpa using h
  unfold normalizer
  have ht : 2 * exp (-(c^2/2)) / c ≤ 2/c := by
    apply div_le_div_of_nonneg_right _ hc.le
    exact mul_le_of_le_one_right (by norm_num) (exp_le_one_iff.mpr (by nlinarith [sq_nonneg c]))
  linarith

private theorem pair_profile {c : ℝ} (hc : 0 < c) :
    logLikelihood separatedPair c 0 1 = 2 * (-log (normalizer c) - rho c 10) := by
  have he : rho c (-10) = rho c 10 := by
    rw [rho_eq_piecewise hc.le, rho_eq_piecewise hc.le]
    norm_num
  simp [logLikelihood, separatedPair, Fin.sum_univ_two, he]
  ring

private theorem pair_small_witness :
    -10 < logLikelihood separatedPair (1/10) 0 1 := by
  have hc : (0 : ℝ) < 1/10 := by norm_num
  rw [pair_profile hc, rho_eq_piecewise hc.le]
  norm_num only [abs_of_nonneg (show (0 : ℝ) ≤ 10 by norm_num)]
  have hZ : normalizer (1/10) < exp 4 := by
    have he : (5/2 : ℝ) < exp 1 := by linarith [exp_one_gt_d9]
    have he' := pow_lt_pow_left₀ he (by norm_num : (0 : ℝ) ≤ 5/2) (by decide : 4 ≠ 0)
    rw [← exp_nat_mul] at he'
    norm_num at he'
    linarith [normalizer_upper hc]
  have hz := log_lt_log (normalizer_pos hc) hZ
  rw [log_exp] at hz
  norm_num
  linarith

private theorem normal_sqrt_gt_two : 2 < sqrt (2*π) := by
  apply (lt_sqrt (by norm_num : (0 : ℝ) ≤ 2)).mpr
  linarith [pi_gt_three]

private theorem pair_large_cutoff_loses {c : ℝ} (hc : 1/2 ≤ c) :
    logLikelihood separatedPair c 0 1 < -10 := by
  have hc' : 0 < c := (by norm_num : (0 : ℝ) < 1/2).trans_le hc
  have hz : 1/8 < log (normalizer c) := by
    rw [← log_exp (1/8)]
    apply log_lt_log (exp_pos _)
    have he := exp_bound_div_one_sub_of_interval (by norm_num : (0 : ℝ) ≤ 1/8)
      (by norm_num : (1/8 : ℝ) < 1)
    exact (he.trans_lt (by norm_num : 1/(1-1/8 : ℝ) < 2)).trans
      (normal_sqrt_gt_two.trans (normalizer_gt_normal hc'))
  have hr : 39/8 ≤ rho c 10 := by
    rw [rho_eq_piecewise hc'.le, abs_of_nonneg (by norm_num : (0 : ℝ) ≤ 10)]
    by_cases h : (10 : ℝ) ≤ c
    · rw [ite_eq_left h]
      norm_num
    · rw [ite_eq_right h]
      nlinarith only [mul_nonneg (sub_nonneg.mpr hc) (show 0 ≤ 39/2-c by linarith)]
  rw [pair_profile hc']
  linarith

private theorem pair_zero_max_location {c : ℝ} (hc : 0 < c) :
    IsMaxOn (fun m ↦ logLikelihood separatedPair c m 1) univ 0 := by
  have he : locationScore c separatedPair 1 0 = 0 := by
    simp only [locationScore, Fin.sum_univ_two, separatedPair, Matrix.cons_val_zero,
      Matrix.cons_val_one, sub_zero, div_one, clip]
    by_cases h : c ≤ 10
    · rw [min_eq_right (by linarith : -10 ≤ c), max_eq_left (by linarith : -10 ≤ -c),
        min_eq_left h, max_eq_right (by linarith : -c ≤ c)]
      ring
    · have h' : (10 : ℝ) ≤ c := le_of_lt (lt_of_not_ge h)
      rw [min_eq_right (by linarith : -10 ≤ c), max_eq_right (by linarith : -c ≤ -10),
        min_eq_right h', max_eq_right (by linarith : -c ≤ 10)]
      norm_num
  have hm : (0 : ℝ) ∈ locationMaximizers c separatedPair 1 :=
    (mem_locationMaximizers_iff hc zero_lt_one separatedPair 0).mpr he
  intro m _
  change logLikelihood separatedPair c m 1 ≤ logLikelihood separatedPair c 0 1
  rw [normalizer_transfer _ hc zero_lt_one, normalizer_transfer _ hc zero_lt_one]
  exact sub_le_sub_right (hm (mem_univ m)) _

private theorem pair_location_interval {c : ℝ} (hc : 0 < c) (hsmall : c < 1/2) (m : ℝ) :
    IsMaxOn (fun u ↦ logLikelihood separatedPair c u 1) univ m ↔
      m ∈ Icc (-10+c) (10-c) := by
  have hnorm : IsMaxOn (fun u ↦ logLikelihood separatedPair c u 1) univ m ↔
      m ∈ locationMaximizers c separatedPair 1 := by
    change IsMaxOn (fun u ↦ logLikelihood separatedPair c u 1) univ m ↔
      IsMaxOn (fun u ↦ Huber.logLik c separatedPair (u,1)) univ m
    simp only [isMaxOn_iff, normalizer_transfer _ hc zero_lt_one, sub_le_sub_iff_right]
  rw [hnorm, mem_locationMaximizers_iff hc zero_lt_one]
  simp only [locationScore, Fin.sum_univ_two, separatedPair, Matrix.cons_val_zero,
    Matrix.cons_val_one, div_one, clip]
  change max (-c) (min c (-10-m)) + max (-c) (min c (10-m)) = 0 ↔
    -10+c ≤ m ∧ m ≤ 10-c
  constructor
  · intro h
    constructor
    · by_contra! hm
      have hR : max (-c) (min c (10-m)) = c := by
        rw [min_eq_left (by linarith), max_eq_right (by linarith)]
      have hL : -c < max (-c) (min c (-10-m)) :=
        (lt_min (by linarith) (by linarith)).trans_le (le_max_right _ _)
      linarith
    · by_contra! hm
      have hL : max (-c) (min c (-10-m)) = -c := by
        exact max_eq_left ((min_le_right _ _).trans (by linarith))
      have hR : max (-c) (min c (10-m)) < c :=
        max_lt_iff.mpr ⟨by linarith, (min_le_right _ _).trans_lt (by linarith)⟩
      linarith
  · rintro ⟨hl,hr⟩
    rw [max_eq_left ((min_le_right _ _).trans (by linarith)),
      min_eq_left (by linarith), max_eq_right (by linarith)]
    ring

set_option maxHeartbeats 600000 in
/-- The held-unit-scale symmetric pair has a winning cutoff in `(0,1/2)`.
At that cutoff its entire location maximizing set is the stated interval. -/
theorem pair_winning_shape_and_interval :
    ∃ c ∈ Ioo (0 : ℝ) (1/2),
      ∀ m : ℝ,
        IsMaxOn (fun p : ℝ × ℝ ↦ logLikelihood separatedPair p.1 p.2 1) (Ioi 0 ×ˢ univ) (c,m) ↔
          m ∈ Icc (-10+c) (10-c) := by
  have hnorm : Normal.logLik separatedPair 0 1 < -10 := by
    rw [Normal.logLik_formula _ _ zero_lt_one]
    have hπ : 0 ≤ log (2*π) := log_nonneg (by linarith [pi_gt_three])
    norm_num [Normal.residual, separatedPair, Fin.sum_univ_two]
    linarith
  have h := held_both_finite_iff_face (by decide : 0 < 2) separatedPair 0 zero_lt_one
  dsimp only at h
  obtain ⟨c,hc,hm⟩ := h.mpr ⟨1/10,by norm_num,(hnorm.trans pair_small_witness).le⟩
  have hsmall : c < 1/2 := by
    by_contra! hc'
    exact (pair_large_cutoff_loses hc').not_ge
      (pair_small_witness.le.trans (hm (show 1/10 ∈ Ioi (0 : ℝ) from by norm_num)))
  refine ⟨c,⟨hc,hsmall⟩,fun m ↦ ?_⟩
  constructor
  · intro hJ
    apply (pair_location_interval hc hsmall m).mp
    intro z _
    change logLikelihood separatedPair c z 1 ≤ logLikelihood separatedPair c m 1
    exact isMaxOn_iff.mp hJ (c,z) ⟨hc,mem_univ z⟩
  · intro hI p hp
    have hcm := (pair_location_interval hc hsmall m).mpr hI
    have hpos : 0 < p.1 := hp.1
    change logLikelihood separatedPair p.1 p.2 1 ≤ logLikelihood separatedPair c m 1
    calc
      _ ≤ logLikelihood separatedPair p.1 0 1 :=
        (pair_zero_max_location hpos) (mem_univ p.2)
      _ ≤ logLikelihood separatedPair c 0 1 := hm hpos
      _ ≤ _ := hcm (mem_univ (0 : ℝ))

/-- Tail probability at a positive tuning cutoff in the standardized model. -/
noncomputable def tailShare (c : ℝ) : ℝ :=
  (2*exp (-(c^2/2))) / (c*normalizer c)

private theorem tailShare_small_bounds {c : ℝ} (hc : 0 < c) (hsmall : c < 1/2) :
    1/2 < tailShare c ∧ tailShare c ≤ 1 := by
  unfold tailShare
  have hz := normalizer_pos hc
  constructor
  · apply (lt_div_iff₀ (mul_pos hc hz)).mpr
    have hZ := mul_le_mul_of_nonneg_left (normalizer_upper hc) hc.le
    have he : c*(2*c+2/c) = 2*c^2+2 := by field_simp [hc.ne']
    rw [he] at hZ
    have ht := add_one_le_exp (-(c^2/2))
    nlinarith [mul_pos (sub_pos.mpr hsmall) (show 0 < c+1/2 by positivity)]
  · apply (div_le_iff₀ (mul_pos hc hz)).mpr
    unfold normalizer
    rw [one_mul, mul_add, mul_div_cancel₀ _ hc.ne']
    have hi : 0 ≤ ∫ t in (0 : ℝ)..c, exp (-(t^2/2)) :=
      intervalIntegral.integral_nonneg_of_forall hc.le (fun _ ↦ (exp_pos _).le)
    nlinarith [mul_nonneg hc.le hi]

private theorem tailScore_hasDerivAt {c : ℝ} (hc : 0 < c) :
    HasDerivAt (fun t ↦ tailShare t / t)
      (-(tailShare c + (2*tailShare c-(tailShare c)^2)/c^2)) c := by
  let e (t : ℝ) := exp (-(t^2/2))
  have he : HasDerivAt e (-c*e c) c := by
    convert (((((hasDerivAt_id c).pow 2).div_const 2).neg).exp) using 1
    · rfl
    · dsimp [e]
      ring
  have hd := (he.const_mul (2 : ℝ)).div
    ((((hasDerivAt_id c).pow 2)).mul (normalizer_hasDerivAt hc))
    (mul_ne_zero (pow_ne_zero 2 hc.ne') (normalizer_pos hc).ne')
  convert hd using 1
  · funext t
    change tailShare t / t = 2*e t/(t^2*normalizer t)
    unfold tailShare e
    rw [div_div, show t*normalizer t*t = t^2*normalizer t by ring]
  · unfold tailShare e
    norm_num [Pi.mul_apply, Pi.pow_apply]
    field_simp [hc.ne', (normalizer_pos hc).ne']
    ring

private theorem tailShare_pos_le_one {c : ℝ} (hc : 0 < c) :
    0 < tailShare c ∧ tailShare c ≤ 1 := by
  unfold tailShare
  refine ⟨div_pos (by positivity) (mul_pos hc (normalizer_pos hc)),
    (div_le_iff₀ (mul_pos hc (normalizer_pos hc))).2 ?_⟩
  unfold normalizer
  rw [one_mul,mul_add,mul_div_cancel₀ _ hc.ne']
  exact le_add_of_nonneg_left (mul_nonneg hc.le
    (mul_nonneg (by norm_num) (intervalIntegral.integral_nonneg_of_forall hc.le
      (fun t ↦ (exp_pos _).le))))

private theorem tailShare_hasDerivAt {c : ℝ} (hc : 0 < c) :
    HasDerivAt tailShare (-c*tailShare c - tailShare c*(1-tailShare c)/c) c := by
  have hd := (hasDerivAt_id c).mul (tailScore_hasDerivAt hc)
  have ht : (fun t ↦ t*(tailShare t/t)) =ᶠ[𝓝 c] tailShare := by
    filter_upwards [Ioi_mem_nhds hc] with t ht
    field_simp [(show 0 < t from ht).ne']
  apply HasDerivAt.congr_of_eventuallyEq _ ht.symm
  convert hd using 1
  · rfl
  · simp only [id_eq,one_mul]
    field_simp [hc.ne']
    ring

private theorem tailShare_strictAnti : StrictAntiOn tailShare (Ioi 0) := by
  refine strictAntiOn_of_deriv_neg (convex_Ioi 0)
    (fun c hc ↦ (tailShare_hasDerivAt hc).continuousAt.continuousWithinAt) ?_
  intro c hc
  have hpos : 0 < c := interior_subset hc
  rw [(tailShare_hasDerivAt hpos).deriv]
  obtain ⟨hq,hq₁⟩ := tailShare_pos_le_one hpos
  have hn := mul_neg_of_neg_of_pos (neg_neg_of_pos hpos) hq
  have hp := div_nonneg (mul_nonneg hq.le (sub_nonneg.mpr hq₁)) hpos.le
  linarith only [hn,hp]

private noncomputable def curvature (c : ℝ) : ℝ :=
  tailShare c + (2*tailShare c-(tailShare c)^2)/c^2

private theorem curvature_strictAnti : StrictAntiOn curvature (Ioi 0) := by
  intro a ha b hb hab
  have ha' : 0 < a := ha
  have hb' : 0 < b := hb
  have hh := tailShare_strictAnti ha hb hab
  obtain ⟨hqa,hqa₁⟩ := tailShare_pos_le_one ha'
  obtain ⟨hqb,hqb₁⟩ := tailShare_pos_le_one hb'
  have hn : 2*tailShare b-tailShare b^2 ≤ 2*tailShare a-tailShare a^2 := by
    nlinarith only [mul_nonneg (sub_nonneg.mpr hh.le)
      (show 0 ≤ 2-tailShare a-tailShare b by linarith only [hqa₁,hqb₁])]
  have hnon : 0 ≤ 2*tailShare a-tailShare a^2 := by
    nlinarith only [mul_nonneg hqa.le (show 0 ≤ 2-tailShare a by linarith only [hqa₁])]
  have ht := (div_le_div_of_nonneg_right hn (sq_nonneg b)).trans
    (div_le_div_of_nonneg_left hnon (sq_pos_of_pos ha')
      (by nlinarith only [mul_nonneg (sub_nonneg.mpr hab.le) (add_nonneg ha'.le hb'.le)]))
  exact add_lt_add_of_lt_of_le hh ht

private theorem finite_strictConvex_zeros {f : ℝ → ℝ} (hf : StrictConvexOn ℝ (Ioi 0) f) :
    {c ∈ Ioi 0 | f c = 0}.Finite := by
  classical
  let Z := {c ∈ Ioi 0 | f c = 0}
  by_cases h : ∃ a ∈ Z, ∃ b ∈ Z, a < b
  · obtain ⟨a,ha,b,hb,hab⟩ := h
    refine ((finite_singleton a).insert b).subset ?_
    intro c hc
    have hz {u v w : ℝ} (hu : u ∈ Z) (hv : v ∈ Z) (hw : w ∈ Z)
        (huv : u < v) (hvw : v < w) : False := by
      have he : v ∈ openSegment ℝ u w := by
        rw [openSegment_eq_Ioo (huv.trans hvw)]
        exact ⟨huv,hvw⟩
      have hlt := hf.lt_on_openSegment hu.1 hw.1 (huv.trans hvw).ne he
      rw [hu.2,hv.2,hw.2,max_self] at hlt
      exact (lt_irrefl _ hlt)
    rcases lt_trichotomy c a with hca | rfl | hac
    · exact (hz hc ha hb hca hab).elim
    · exact mem_insert_of_mem _ (mem_singleton _)
    · rcases lt_trichotomy c b with hcb | rfl | hbc
      · exact (hz ha hc hb hac hcb).elim
      · exact mem_insert _ _
      · exact (hz ha hb hc hab hbc).elim
  · by_cases hZ : Z.Nonempty
    · obtain ⟨a,ha⟩ := hZ
      refine (finite_singleton a).subset fun b hb ↦ ?_
      exact mem_singleton_iff.mpr (le_antisymm
        (le_of_not_gt (fun hab ↦ h ⟨a,ha,b,hb,hab⟩))
        (le_of_not_gt (fun hba ↦ h ⟨b,hb,a,ha,hba⟩)))
    · change Z.Finite
      rw [Set.not_nonempty_iff_eq_empty.mp hZ]
      exact finite_empty

private theorem affine_tailScore_strictConvex {N M T : ℝ} (hN : 0 < N) :
    StrictConvexOn ℝ (Ioi 0) (fun c ↦ N*(tailShare c/c)+M*c-T) := by
  have hd (c : ℝ) (hc : 0 < c) : HasDerivAt (fun t ↦ N*(tailShare t/t)+M*t-T)
      (M-N*curvature c) c := by
    convert ((((tailScore_hasDerivAt hc).const_mul N).add
      ((hasDerivAt_id c).const_mul M)).sub_const T) using 1
    · rfl
    · unfold curvature
      simp only [mul_one]
      ring
  apply StrictMonoOn.strictConvexOn_of_deriv (convex_Ioi 0)
    (fun c hc ↦ (hd c hc).continuousAt.continuousWithinAt)
  rw [isOpen_Ioi.interior_eq]
  intro a ha b hb hab
  rw [(hd a ha).deriv,(hd b hb).deriv]
  exact sub_lt_sub_left (mul_lt_mul_of_pos_left (curvature_strictAnti ha hb hab) hN) _

/-- For a nonempty sample at held location and scale, the tuning score
has finitely many roots, including any roots at observed-magnitude joins. -/
theorem held_location_scale_stationary_finite {n : ℕ} (hn : 0 < n) (z : Fin n → ℝ) :
    {c ∈ Ioi 0 | (n : ℝ)*(tailShare c/c) - ∑ i, max (|z i|-c) 0 = 0}.Finite := by
  classical
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let S (J : Finset (Fin n)) := {c ∈ Ioi 0 |
    (n : ℝ)*(tailShare c/c) + (J.card : ℝ)*c - ∑ i ∈ J, |z i| = 0}
  have hS (J : Finset (Fin n)) : (S J).Finite :=
    finite_strictConvex_zeros (affine_tailScore_strictConvex hN)
  refine (Set.finite_iUnion hS).subset ?_
  intro c hc
  let J := Finset.univ.filter fun i ↦ c < |z i|
  apply mem_iUnion.mpr
  refine ⟨J,hc.1,?_⟩
  have he : ∑ i, max (|z i|-c) 0 = ∑ i ∈ J, (|z i|-c) := by
    rw [Finset.sum_filter]
    apply Finset.sum_congr rfl
    intro i _
    by_cases h : c < |z i|
    · simp [h,max_eq_left (sub_nonneg.mpr h.le)]
    · simp [h,max_eq_right (sub_nonpos.mpr (le_of_not_gt h))]
  have hh := hc.2
  rw [he,Finset.sum_sub_distrib,Finset.sum_const,nsmul_eq_mul] at hh
  change (n : ℝ)*(tailShare c/c) + (J.card : ℝ)*c - ∑ i ∈ J, |z i| = 0
  linarith only [hh]

private theorem penalty_hasDerivAt (R c : ℝ) :
    HasDerivAt (fun t ↦ (max (R-t) 0)^2/2) (-(max (R-c) 0)) c := by
  have hd (t : ℝ) : HasDerivAt (fun q ↦ (R-q)^2/2) (-(R-t)) t := by
    convert (((hasDerivAt_id t).const_sub R).pow 2).div_const 2 using 1
    · rfl
    · simp only [id_eq]
      ring
  rcases lt_trichotomy c R with h | rfl | h
  · rw [max_eq_left (sub_nonneg.mpr h.le)]
    apply (hd c).congr_of_eventuallyEq
    filter_upwards [Iio_mem_nhds h] with t ht
    rw [max_eq_left (sub_nonneg.mpr (show t ≤ R from ht.le))]
  · rw [sub_self,max_self,neg_zero]
    have h₁ : HasDerivWithinAt (fun t ↦ (max (c-t) 0)^2/2) 0 (Iic c) c := by
      have hh : HasDerivAt (fun q ↦ (c-q)^2/2) 0 c := by simpa using hd c
      apply hh.hasDerivWithinAt.congr_of_mem _ (mem_Iic.mpr le_rfl)
      intro t ht
      rw [max_eq_left (sub_nonneg.mpr ht)]
    have h₂ : HasDerivWithinAt (fun t ↦ (max (c-t) 0)^2/2) 0 (Ici c) c := by
      apply (hasDerivAt_const c (0 : ℝ)).hasDerivWithinAt.congr_of_mem _ (mem_Ici.mpr le_rfl)
      intro t ht
      simp [max_eq_right (sub_nonpos.mpr ht)]
    exact (h₁.union h₂).hasDerivAt (by simp)
  · rw [max_eq_right (sub_nonpos.mpr h.le),neg_zero]
    apply (hasDerivAt_const c (0 : ℝ)).congr_of_eventuallyEq
    filter_upwards [Ioi_mem_nhds h] with t ht
    simp [max_eq_right (sub_nonpos.mpr (show R ≤ t from ht.le))]

/-- The normalized cutoff derivative at held location and scale,
including the joins where a residual switches between tail and core. -/
theorem held_location_scale_hasDerivAt {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {s c : ℝ} (hs : 0 < s) (hc : 0 < c) :
    HasDerivAt (fun t ↦ logLikelihood x t m s)
      ((n : ℝ)*(tailShare c/c) - ∑ i, max (|(x i-m)/s|-c) 0) c := by
  have hn : HasDerivAt (fun t ↦ -log (s*normalizer t)) (tailShare c/c) c := by
    have h := (((normalizer_hasDerivAt hc).const_mul s).log
      (mul_pos hs (normalizer_pos hc)).ne').neg
    convert h using 1
    unfold tailShare
    field_simp [hs.ne',hc.ne',(normalizer_pos hc).ne']
  let z (i : Fin n) := (x i-m)/s
  have hterm (i : Fin n) : HasDerivAt (fun t ↦ -log (s*normalizer t)-rho t (z i))
      (tailShare c/c - max (|z i|-c) 0) c := by
    apply ((hn.sub_const ((z i)^2/2)).add (penalty_hasDerivAt |z i| c)).congr_of_eventuallyEq
    filter_upwards [Ioi_mem_nhds hc] with t ht
    have h := loss_reduction (z := z i) (show 0 < t from ht).le
    change -log (s*normalizer t)-rho t (z i) =
      -log (s*normalizer t)-(z i)^2/2+(max (|z i|-t) 0)^2/2
    linarith only [h]
  have h := HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ hterm i)
  simpa [logLikelihood,z,Finset.sum_sub_distrib,nsmul_eq_mul] using h

/-- On every nonempty sample at held location and scale, the set of
stationary finite cutoffs of the normalized likelihood is finite. -/
theorem held_location_scale_finite_critical {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (m : ℝ) {s : ℝ} (hs : 0 < s) :
    {c ∈ Ioi 0 | deriv (fun t ↦ logLikelihood x t m s) c = 0}.Finite := by
  refine (held_location_scale_stationary_finite hn (fun i ↦ (x i-m)/s)).subset ?_
  intro c hc
  exact ⟨hc.1, (held_location_scale_hasDerivAt x m hs hc.1).deriv ▸ hc.2⟩

/-- At held location and scale with equal absolute residuals, any
attained Huber cutoff is unique. -/
theorem held_location_scale_equal_magnitude_unique {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (m : ℝ) {s R : ℝ} (hs : 0 < s)
    (hR : ∀ i, |(x i-m)/s| = R)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hma : IsMaxOn (fun c ↦ logLikelihood x c m s) (Ioi 0) a)
    (hmb : IsMaxOn (fun c ↦ logLikelihood x c m s) (Ioi 0) b) : a = b := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let F := fun c ↦ logLikelihood x c m s
  let G := fun c ↦ (n : ℝ)*(tailShare c/c) + (n : ℝ)*c - (n : ℝ)*R
  have hd (c : ℝ) (hc : 0 < c) :
      HasDerivAt F ((n : ℝ)*(tailShare c/c) - (n : ℝ)*max (R-c) 0) c := by
    simpa only [F,hR,Finset.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul] using
      held_location_scale_hasDerivAt x m hs hc
  have hroot (c : ℝ) (hc : 0 < c) (hm : IsMaxOn F (Ioi 0) c) : c < R ∧ G c = 0 := by
    have hzero := (hm.isLocalMax (Ioi_mem_nhds hc)).hasDerivAt_eq_zero (hd c hc)
    have hlt : c < R := by
      by_contra! hc'
      rw [max_eq_right (sub_nonpos.mpr hc'),mul_zero,sub_zero] at hzero
      exact (mul_pos hN (div_pos (tailShare_pos_le_one hc).1 hc)).ne' hzero
    rw [max_eq_left (sub_nonneg.mpr hlt.le)] at hzero
    refine ⟨hlt,?_⟩
    dsimp [G]
    linarith only [hzero]
  have hstrict {a b : ℝ} (ha : 0 < a) (hab : a < b) (hbR : b < R)
      (h₁ : G a = 0) (h₂ : G b = 0) : F b < F a := by
    have hcv : StrictConvexOn ℝ (Ioi 0) G := affine_tailScore_strictConvex hN
    have hanti : StrictAntiOn F (Icc a b) := by
      apply strictAntiOn_of_deriv_neg (convex_Icc a b)
      · intro c hc
        exact (hd c (ha.trans_le hc.1)).continuousAt.continuousWithinAt
      · intro c hc'
        rw [interior_Icc] at hc'
        have hc := ha.trans hc'.1
        have hval := hcv.lt_on_openSegment ha (ha.trans hab) hab.ne (by rwa [openSegment_eq_Ioo hab])
        rw [h₁,h₂,max_self] at hval
        rw [(hd c hc).deriv,max_eq_left (sub_nonneg.mpr (hc'.2.trans hbR).le)]
        dsimp [G] at hval
        linarith only [hval]
    exact hanti ⟨le_rfl,hab.le⟩ ⟨hab.le,le_rfl⟩ hab
  obtain ⟨haR,haz⟩ := hroot a ha hma
  obtain ⟨hbR,hbz⟩ := hroot b hb hmb
  rcases lt_trichotomy a b with h | h | h
  · exact ((hstrict ha h hbR haz hbz).not_ge (hmb ha)).elim
  · exact h
  · exact ((hstrict hb h haR hbz haz).not_ge (hma hb)).elim

private noncomputable def pairCoreProfile (c : ℝ) : ℝ :=
  2 * (-log (normalizer c) - 10*c + c^2/2)

private theorem pairCore_strictConcave :
    StrictConcaveOn ℝ (Ioo 0 (1/2)) pairCoreProfile := by
  have hd (c : ℝ) (hc : 0 < c) :
      HasDerivAt pairCoreProfile (2*(tailShare c/c - 10 + c)) c := by
    have hn := ((normalizer_hasDerivAt hc).log (normalizer_pos hc).ne').neg
    have h := ((hn.sub ((hasDerivAt_id c).const_mul (10 : ℝ))).add
      (((hasDerivAt_id c).pow 2).div_const 2)).const_mul (2 : ℝ)
    convert h using 1
    · rfl
    · unfold tailShare
      simp only [id_eq, Nat.cast_ofNat, mul_one]
      field_simp [hc.ne', (normalizer_pos hc).ne']
      ring
  apply strictConcaveOn_of_deriv2_neg' (convex_Ioo 0 (1/2))
    (fun c hc ↦ (hd c hc.1).continuousAt.continuousWithinAt)
  intro c hc
  have he : deriv pairCoreProfile =ᶠ[𝓝 c] (fun t ↦ 2*(tailShare t/t-10+t)) :=
    Filter.eventually_of_mem (isOpen_Ioi.mem_nhds hc.1) (fun t ht ↦ (hd t ht).deriv)
  change deriv (deriv pairCoreProfile) c < 0
  rw [he.deriv_eq]
  have hdd : HasDerivAt (fun t ↦ 2*(tailShare t/t - 10 + t))
      (2 * (1 - (tailShare c + (2*tailShare c-(tailShare c)^2)/c^2))) c := by
    convert (((tailScore_hasDerivAt hc.1).sub_const 10).add (hasDerivAt_id c)).const_mul 2 using 1
    · rfl
    · ring
  rw [hdd.deriv]
  obtain ⟨hl,hu⟩ := tailShare_small_bounds hc.1 hc.2
  have hq : 0 < tailShare c := by linarith
  have hnum : tailShare c ≤ 2*tailShare c-tailShare c^2 := by nlinarith [mul_nonneg hq.le (sub_nonneg.mpr hu)]
  have hrat : 1 < (2*tailShare c-tailShare c^2)/c^2 := by
    apply (lt_div_iff₀ (sq_pos_of_pos hc.1)).mpr
    have hc2 := pow_lt_pow_left₀ hc.2 hc.1.le (by decide : 2 ≠ 0)
    linarith
  linarith

private theorem pairCore_eq {c : ℝ} (hc : c ∈ Ioo (0 : ℝ) (1/2)) :
    pairCoreProfile c = logLikelihood separatedPair c 0 1 := by
  have h : ¬ (10 : ℝ) ≤ c := by linarith [hc.2]
  rw [pair_profile hc.1, rho_eq_piecewise hc.1.le, abs_of_nonneg (by norm_num : (0 : ℝ) ≤ 10),
    ite_eq_right h]
  unfold pairCoreProfile
  ring

/-- At unit scale, the exact Huber maximizer set for `(-10,10)` has one
cutoff in `(0,1/2)` and the complete closed interval of locations
`[-10+c, 10-c]` at that cutoff. -/
theorem separated_pair_full_maximizers :
    ∃ c ∈ Ioo (0 : ℝ) (1/2),
      ∀ p ∈ (Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)),
        IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood separatedPair q.1 q.2 1) (Ioi 0 ×ˢ univ) p ↔
          p.1 = c ∧ p.2 ∈ Icc (-10+c) (10-c) := by
  obtain ⟨c,hc,hm⟩ := pair_winning_shape_and_interval
  let S := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
  let L (p : ℝ × ℝ) := logLikelihood separatedPair p.1 p.2 1
  have h0 : (0 : ℝ) ∈ Icc (-10+c) (10-c) := by
    constructor <;> linarith [hc.2]
  have hmax : IsMaxOn L S (c,0) := (hm 0).mpr h0
  refine ⟨c,hc,fun p hp ↦ ?_⟩
  change IsMaxOn L S p ↔ _
  constructor
  · intro hP
    have hpc : IsMaxOn L S (p.1,0) := by
      intro q hq
      change L q ≤ L (p.1,0)
      exact ((isMaxOn_iff.mp hP) q hq).trans
        ((isMaxOn_iff.mp (pair_zero_max_location hp.1)) p.2 (mem_univ _))
    have hpSmall : p.1 < 1/2 := by
      by_contra! h
      exact (pair_large_cutoff_loses h).not_ge
        (pair_small_witness.le.trans (isMaxOn_iff.mp hpc (1/10,0) ⟨by norm_num,mem_univ _⟩))
    have hpI : p.1 ∈ Ioo (0 : ℝ) (1/2) := ⟨hp.1,hpSmall⟩
    have hcore {t : ℝ} (ht : t ∈ Ioo (0 : ℝ) (1/2)) (h : IsMaxOn L S (t,0)) :
        IsMaxOn pairCoreProfile (Ioo 0 (1/2)) t := by
      intro a ha
      change pairCoreProfile a ≤ pairCoreProfile t
      rw [pairCore_eq ha, pairCore_eq ht]
      exact isMaxOn_iff.mp h (a,0) ⟨ha.1,mem_univ _⟩
    have ht := pairCore_strictConcave.eq_of_isMaxOn (hcore hpI hpc) (hcore hc hmax) hpI hc
    have he : p = (c,p.2) := Prod.ext ht rfl
    exact ⟨ht,(hm p.2).mp (he ▸ hP)⟩
  · rintro ⟨he,hI⟩
    have hp' : p = (c,p.2) := Prod.ext he rfl
    rw [hp']
    exact (hm p.2).mpr hI

end UnivariateML.LocScaleHuber

end
