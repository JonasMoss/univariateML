module

public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Probability.Distributions.Gamma
public import Mathlib.Analysis.SpecialFunctions.Gamma.Deriv
import Mathlib.Analysis.Calculus.Deriv.Inverse
import Mathlib.Analysis.Calculus.ParametricIntegral
import Mathlib.MeasureTheory.Function.JacobianOneDim
import Mathlib.MeasureTheory.Integral.Prod
import Mathlib.Tactic

set_option linter.style.header false

/-! Held-parameter fits and zero-atom limits of the discretized Gamma family.
Cell endpoints are expressed in the unit-rate variable. -/

@[expose] public section

namespace UnivariateML.Discrete3DGamma

open Set Filter MeasureTheory ProbabilityTheory
open scoped Topology ENNReal

private theorem zeroMass_lt_one {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    cdf (gammaMeasure a 1) r < 1 := by
  let μ := gammaMeasure a 1
  let : IsProbabilityMeasure μ := isProbabilityMeasure_gammaMeasure ha one_pos
  have hp : 0 < μ (Ioi r) := by
    change 0 < (volume.withDensity (gammaPDF a 1)) (Ioi r)
    rw [withDensity_apply _ measurableSet_Ioi, setLIntegral_pos_iff (by
      unfold gammaPDF
      exact (measurable_gammaPDFReal a 1).ennreal_ofReal)]
    have hsub : Ioo r (r+1) ⊆ Function.support (gammaPDF a 1) ∩ Ioi r := by
      intro y hy
      exact ⟨(ENNReal.ofReal_pos.mpr (gammaPDFReal_pos ha one_pos (hr.trans hy.1))).ne',
        hy.1⟩
    apply lt_of_lt_of_le _ (measure_mono hsub)
    simp [Real.volume_Ioo]
  have hsum : μ (Iic r) + μ (Ioi r) = 1 := by
    rw [← measure_union (by simp [Set.disjoint_left]) measurableSet_Ioi,
      Iic_union_Ioi, measure_univ]
  have hle : μ (Iic r) ≤ 1 := calc
    _ ≤ μ univ := measure_mono (subset_univ _)
    _ = 1 := measure_univ
  have hlt : μ (Iic r) < 1 := by
    by_contra! h
    rw [le_antisymm hle h] at hsum
    exact (ne_of_gt (ENNReal.lt_add_right (by simp : (1 : ℝ≥0∞) ≠ ∞) hp.ne')) hsum
  rw [cdf_eq_real, measureReal_def]
  exact (ENNReal.toReal_lt_toReal (ne_top_of_lt hlt) (by simp)).2 hlt

/-- For all-zero data with shape held, raising the rate approaches likelihood
one, which no finite positive rate attains. -/
theorem all_zero_held_shape {a : ℝ} (ha : 0 < a) {n : ℕ} (hn : 0 < n) :
    Tendsto (fun r ↦ (cdf (gammaMeasure a 1) r)^n) atTop (𝓝 1) ∧
      ∀ r > 0, (cdf (gammaMeasure a 1) r)^n < 1 := by
  constructor
  · simpa using (tendsto_cdf_atTop (gammaMeasure a 1)).pow n
  · intro r hr
    exact pow_lt_one₀ (cdf_nonneg _ _) (zeroMass_lt_one ha hr) hn.ne'


open Real
open scoped BigOperators Interval

private noncomputable def density (a : ℝ) := gammaPDFReal a 1

private theorem density_integrable {a : ℝ} (ha : 0 < a) : Integrable (density a) := by
  refine ⟨(measurable_gammaPDFReal a 1).aestronglyMeasurable, ?_⟩
  change HasFiniteIntegral (gammaPDFReal a 1) volume
  rw [hasFiniteIntegral_iff_ofReal (ae_of_all _ (gammaPDFReal_nonneg ha one_pos))]
  change (∫⁻ y, gammaPDF a 1 y) < ∞
  rw [lintegral_gammaPDF_eq_one ha one_pos]
  simp

private theorem density_formula (a : ℝ) {y : ℝ} (hy : 0 < y) :
    density a y = y ^ (a-1) * exp (-y) / Gamma a := by
  simp [density, gammaPDFReal, hy.le]
  ring

private theorem cdf_eq_interval {a : ℝ} (ha : 0 < a) {z : ℝ} (hz : 0 ≤ z) :
    cdf (gammaMeasure a 1) z = ∫ y in (0:ℝ)..z, density a y := by
  rw [cdf_gammaMeasure_eq_integral ha one_pos, intervalIntegral.integral_of_le hz]
  change (∫ y in Iic z, density a y) = _
  have hs : Iic z = Iic (0:ℝ) ∪ Ioc 0 z := by ext y; simp; grind
  rw [hs, setIntegral_union (by rw [Set.disjoint_left]; grind) measurableSet_Ioc
    (density_integrable ha).integrableOn (density_integrable ha).integrableOn]
  have hzero : (∫ y in Iic (0:ℝ), density a y) = 0 := by
    rw [integral_Iic_eq_integral_Iio]
    apply setIntegral_eq_zero_of_forall_eq_zero
    intro y hy
    simp [density, gammaPDFReal, not_le.mpr (mem_Iio.mp hy)]
  rw [hzero, zero_add]

private theorem cdf_derivative {a z : ℝ} (ha : 0 < a) (hz : 0 < z) :
    HasDerivAt (cdf (gammaMeasure a 1)) (density a z) z := by
  have hc : ContinuousAt (density a) z := by
    have hh : ContinuousAt (fun y : ℝ ↦ y^(a-1) * exp (-y) / Gamma a) z :=
      ((continuousAt_id.rpow_const (Or.inl hz.ne')).mul
        (continuous_exp.continuousAt.comp continuousAt_id.neg)).div_const _
    apply hh.congr
    filter_upwards [Ioi_mem_nhds hz] with y hy
    exact (density_formula a hy).symm
  have hh := intervalIntegral.integral_hasDerivAt_right (a := 0)
    (density_integrable ha).intervalIntegrable
    (stronglyMeasurable_gammaPDFReal a 1).stronglyMeasurableAtFilter hc
  apply hh.congr_of_eventuallyEq
  filter_upwards [Ioi_mem_nhds hz] with y hy
  exact cdf_eq_interval ha (mem_Ioi.mp hy).le

/-- Probability of the cell with index `k`, using the scaled unit-rate Gamma variable. -/
noncomputable def cellProb (a r : ℝ) (k : ℕ) :=
  cdf (gammaMeasure a 1) ((k+1 : ℝ)*r) - cdf (gammaMeasure a 1) ((k : ℝ)*r)

/-- Likelihood of a finite sample of rounded-down counts. -/
noncomputable def likelihood {n : ℕ} (x : Fin n → ℕ) (a r : ℝ) :=
  ∏ i, cellProb a r (x i)

private theorem cell_integral {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (k : ℕ) :
    cellProb a r k = ∫ y in Ioc ((k : ℝ)*r) ((k+1 : ℝ)*r), density a y := by
  have hl : 0 ≤ (k:ℝ)*r := by positivity
  have hh : (k:ℝ)*r ≤ (k+1:ℝ)*r := by nlinarith
  rw [cellProb, cdf_eq_interval ha (hl.trans hh), cdf_eq_interval ha hl,
    ← intervalIntegral.integral_add_adjacent_intervals
      (density_integrable ha).intervalIntegrable (density_integrable ha).intervalIntegrable,
    add_sub_cancel_left, intervalIntegral.integral_of_le hh]

private theorem int_pos {g : ℝ → ℝ} {l h : ℝ} (hlh : l < h)
    (hi : IntegrableOn g (Ioc l h)) (hg : ∀ y ∈ Ioo l h, 0 < g y) :
    0 < ∫ y in Ioc l h, g y := by
  rw [integral_Ioc_eq_integral_Ioo]
  rw [setIntegral_pos_iff_support_of_nonneg_ae
    ((ae_restrict_iff' measurableSet_Ioo).2 (ae_of_all _ fun y hy ↦ (hg y hy).le))
    (hi.mono_set Ioo_subset_Ioc_self)]
  have hsub : Ioo l h ⊆ Function.support g ∩ Ioo l h := fun _ hy ↦ ⟨(hg _ hy).ne', hy⟩
  exact lt_of_lt_of_le (by simp [Real.volume_Ioo, hlh]) (measure_mono hsub)

private theorem cell_pos {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (k : ℕ) :
    0 < cellProb a r k := by
  rw [cell_integral ha hr]
  apply int_pos (by nlinarith) (density_integrable ha).integrableOn
  intro y hy
  exact gammaPDFReal_pos ha one_pos (lt_of_le_of_lt (by positivity) hy.1)

private theorem cell_le_one {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (k : ℕ) :
    cellProb a r k < 1 := by
  exact (sub_le_self _ (cdf_nonneg _ _)).trans_lt (zeroMass_lt_one ha (by positivity))

private theorem likelihood_pos {n : ℕ} (x : Fin n → ℕ) {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    0 < likelihood x a r := Finset.prod_pos fun _ _ ↦ cell_pos ha hr _

private theorem likelihood_le_cell {n : ℕ} (x : Fin n → ℕ) (i : Fin n)
    {a r : ℝ} (ha : 0 < a) (hr : 0 < r) : likelihood x a r ≤ cellProb a r (x i) := by
  unfold likelihood
  calc
    _ ≤ ∏ j ∈ ({i} : Finset (Fin n)), cellProb a r (x j) :=
      Finset.prod_mono_of_subset_of_le_one₀ (Finset.singleton_subset_iff.mpr (Finset.mem_univ i))
        (fun j _ ↦ (cell_pos ha hr (x j)).le) (fun _ _ ↦ le_rfl)
        (fun j _ _ ↦ (cell_le_one ha hr (x j)).le)
    _ = _ := Finset.prod_singleton _ _


private noncomputable def flux (a y : ℝ) := y^a * exp (-y) / Gamma a

private theorem flux_derivative {a y : ℝ} (hy : 0 < y) :
    HasDerivAt (flux a) ((a-y)*density a y) y := by
  have hp := Real.hasDerivAt_rpow_const (p := a) (x := y) (Or.inl hy.ne')
  have he := (Real.hasDerivAt_exp (-y)).comp y (hasDerivAt_id y).neg
  have hh := (hp.mul he).div_const (Gamma a)
  convert hh using 1
  · rfl
  rw [density_formula a hy]
  have hpow : y^a = y^(a-1)*y := by
    conv_lhs => rw [show a = (a-1)+1 by ring, Real.rpow_add hy, Real.rpow_one]
  rw [hpow]
  simp only [Function.comp_apply]
  ring

private theorem flux_continuousOn {a : ℝ} (ha : 0 < a) :
    ContinuousOn (flux a) (Ici 0) := by
  exact ((continuous_id.continuousOn.rpow_const fun _ _ ↦ Or.inr ha.le).mul
    (continuous_exp.comp continuous_neg).continuousOn).div_const _

private theorem flux_nonneg {a y : ℝ} (ha : 0 < a) (hy : 0 ≤ y) :
    0 ≤ flux a y := by unfold flux; positivity

private theorem flux_id {a y : ℝ} (hy : 0 < y) : flux a y = y*density a y := by
  rw [flux, density_formula a hy]
  have hh : y^a = y^(a-1)*y := by
    conv_lhs => rw [show a = (a-1)+1 by ring, Real.rpow_add hy, Real.rpow_one]
  rw [hh]
  ring

private theorem density_mul_integrable {a l h : ℝ} (ha : 0 < a) (_hlh : l ≤ h) (c d : ℝ) :
    IntegrableOn (fun y ↦ (c+d*y)*density a y) (Ioc l h) := by
  have hh : ContinuousOn (fun y : ℝ ↦ c+d*y) (Icc l h) := by fun_prop
  obtain ⟨M,hM⟩ := isCompact_Icc.exists_bound_of_continuousOn hh
  apply Integrable.bdd_mul (c := M) ((density_integrable ha).integrableOn (s := Ioc l h))
  · exact (hh.mono Ioc_subset_Icc_self).aestronglyMeasurable measurableSet_Ioc
  · exact (ae_restrict_iff' measurableSet_Ioc).2 <|
      ae_of_all _ (fun y hy ↦ hM y (Ioc_subset_Icc_self hy))

private theorem flux_difference {a l h : ℝ} (ha : 0 < a) (hl : 0 ≤ l) (hlh : l ≤ h) :
    ∫ y in Ioc l h, (a-y)*density a y = flux a h- flux a l := by
  have hi : IntegrableOn (fun y ↦ (a-y)*density a y) (Ioc l h) := by
    simpa [sub_eq_add_neg] using density_mul_integrable ha (l := l) (h := h) hlh a (-1)
  rw [← intervalIntegral.integral_of_le hlh]
  exact intervalIntegral.integral_eq_sub_of_hasDerivAt_of_le hlh
    ((flux_continuousOn ha).mono fun _ hy ↦ hl.trans hy.1)
    (fun _ hy ↦ flux_derivative (hl.trans_lt hy.1))
    ((intervalIntegrable_iff_integrableOn_Ioc_of_le hlh).2 hi)

private noncomputable def edge (m u : ℝ) := m*exp u

private theorem edge_derivative (m u : ℝ) : HasDerivAt (edge m) (edge m u) u := by
  exact (hasDerivAt_exp u).const_mul m

private theorem cdf_edge_derivative {a m : ℝ} (ha : 0 < a) (hm : 0 ≤ m) (u : ℝ) :
    HasDerivAt (fun v ↦ cdf (gammaMeasure a 1) (edge m v)) (flux a (edge m u)) u := by
  rcases hm.eq_or_lt with he | hp
  · subst m
    simpa [edge, flux, Real.zero_rpow ha.ne'] using
      (hasDerivAt_const u (cdf (gammaMeasure a 1) 0))
  · have he : 0 < edge m u := mul_pos hp (exp_pos _)
    rw [flux_id he, mul_comm]
    exact (cdf_derivative ha he).comp u (edge_derivative m u)

private theorem flux_edge_derivative {a m : ℝ} (ha : 0 < a) (hm : 0 ≤ m) (u : ℝ) :
    HasDerivAt (fun v ↦ flux a (edge m v))
      ((a-edge m u)*flux a (edge m u)) u := by
  rcases hm.eq_or_lt with he | hp
  · subst m
    simpa [edge, flux, Real.zero_rpow ha.ne'] using (hasDerivAt_const u (0:ℝ))
  · have he : 0 < edge m u := mul_pos hp (exp_pos _)
    convert (flux_derivative (a := a) he).comp u (edge_derivative m u) using 1
    · rfl
    rw [flux_id he]
    ring

private noncomputable def logRate (a : ℝ) (k : ℕ) (u : ℝ) := log (cellProb a (exp u) k)

private theorem logRate_derivative {a : ℝ} (ha : 0 < a) (k : ℕ) (u : ℝ) :
    HasDerivAt (logRate a k)
      ((flux a (edge (k+1) u) - flux a (edge k u))/cellProb a (exp u) k) u := by
  have hh := (cdf_edge_derivative (m := (k:ℝ)+1) ha (by positivity) u).sub
    (cdf_edge_derivative (m := k) ha (by positivity) u)
  exact hh.log (cell_pos ha (exp_pos u) k).ne'

private theorem logRate_strictConcave {a : ℝ} (ha : 0 < a) (k : ℕ) :
    StrictConcaveOn ℝ univ (logRate a k) := by
  apply strictConcaveOn_of_deriv2_neg' convex_univ
    (fun u _ ↦ (logRate_derivative ha k u).continuousAt.continuousWithinAt)
  intro u _
  let l := edge k u
  let h := edge (k+1) u
  let p := cellProb a (exp u) k
  let d := flux a h - flux a l
  let m := d/p
  have hp : 0 < p := cell_pos ha (exp_pos u) k
  have hl : 0 ≤ l := by dsimp [l, edge]; positivity
  have hlh : l < h := by dsimp [l,h,edge]; nlinarith [exp_pos u]
  have hmass : (∫ y in Ioc l h, density a y) = p := (cell_integral ha (exp_pos u) k).symm
  have hflux : (∫ y in Ioc l h, (a-y)*density a y) = d :=
    flux_difference ha hl hlh.le
  have bounds : a-h < m ∧ m < a-l := by
    have hi : IntegrableOn (fun y ↦ (a-y)*density a y) (Ioc l h) := by
      simpa [sub_eq_add_neg] using density_mul_integrable ha hlh.le a (-1)
    have hs (c : ℝ) : IntegrableOn (fun y ↦ c*density a y) (Ioc l h) :=
      (density_integrable ha).integrableOn.const_mul c
    have hdiff (y : ℝ) :
        (h-y)*density a y = (a-y)*density a y - (a-h)*density a y := by ring
    have hdiff' (y : ℝ) :
        (y-l)*density a y = (a-l)*density a y - (a-y)*density a y := by ring
    have hlo : 0 < ∫ y in Ioc l h, (h-y)*density a y := by
      simp_rw [hdiff]
      apply int_pos hlh (hi.sub (hs _))
      intro y hy
      simp only [Pi.sub_apply]
      rw [← hdiff]
      exact mul_pos (sub_pos.mpr hy.2) (gammaPDFReal_pos ha one_pos (hl.trans_lt hy.1))
    have hhi : 0 < ∫ y in Ioc l h, (y-l)*density a y := by
      simp_rw [hdiff']
      apply int_pos hlh ((hs _).sub hi)
      intro y hy
      simp only [Pi.sub_apply]
      rw [← hdiff']
      exact mul_pos (sub_pos.mpr hy.1) (gammaPDFReal_pos ha one_pos (hl.trans_lt hy.1))
    change 0 < ∫ y in Ioc l h, (h-y)*density a y at hlo
    simp_rw [hdiff, integral_sub hi (hs _), integral_const_mul, hmass, hflux] at hlo
    simp_rw [hdiff', integral_sub (hs _) hi, integral_const_mul, hmass, hflux] at hhi
    exact ⟨(lt_div_iff₀ hp).2 (by linarith), (div_lt_iff₀ hp).2 (by linarith)⟩
  have hdd := (flux_edge_derivative (m := (k:ℝ)+1) ha (by positivity) u).sub
    (flux_edge_derivative (m := k) ha (by positivity) u)
  have hdp := (cdf_edge_derivative (m := (k:ℝ)+1) ha (by positivity) u).sub
    (cdf_edge_derivative (m := k) ha (by positivity) u)
  have hder : HasDerivAt (deriv (logRate a k))
      ((((a-h)*flux a h-(a-l)*flux a l)*p - d*d)/p^2) u := by
    apply (hdd.div hdp hp.ne').congr_of_eventuallyEq
    exact Eventually.of_forall fun v ↦ (logRate_derivative ha k v).deriv
  change deriv (deriv (logRate a k)) u < 0
  rw [hder.deriv]
  apply div_neg_of_neg_of_pos _ (sq_pos_of_pos hp)
  have hh : 0 < flux a h := by
    unfold flux
    exact div_pos (mul_pos (rpow_pos_of_pos (hl.trans_lt hlh) _) (exp_pos _)) (Gamma_pos_of_pos ha)
  have hc : ((a-h-m)*flux a h - (a-l-m)*flux a l) < 0 :=
    sub_neg.mpr ((mul_neg_of_neg_of_pos (sub_neg.mpr bounds.1) hh).trans_le
      (mul_nonneg (sub_nonneg.mpr bounds.2.le) (flux_nonneg ha hl)))
  calc
    _ = ((a-h-m)*flux a h - (a-l-m)*flux a l)*p := by
      have hm : m*p = d := div_mul_cancel₀ _ hp.ne'
      calc
        _ = ((a-h)*flux a h-(a-l)*flux a l)*p-d*(m*p) := by rw [hm]
        _ = _ := by dsimp only [d]; ring
    _ < 0 := mul_neg_of_neg_of_pos hc hp


private theorem attained_of_tails {f : ℝ → ℝ}
    (hc : ContinuousOn f (Ioi 0)) (hp : 0 < f 1)
    (hlo : Tendsto f (𝓝[>] 0) (𝓝 0)) (hhi : Tendsto f atTop (𝓝 0)) :
    ∃ r ∈ Ioi 0, IsMaxOn f (Ioi 0) r := by
  have hb : ∀ᶠ r in 𝓝[>] (0:ℝ), f r < f 1 := hlo.eventually (gt_mem_nhds hp)
  obtain ⟨ε,hε,hb⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp hb
  obtain ⟨M,hM⟩ := eventually_atTop.1 (hhi.eventually (gt_mem_nhds hp))
  let L := min 1 (ε/2)
  let U := max 1 M
  have hL : 0 < L := lt_min one_pos (half_pos hε)
  have h1 : (1:ℝ) ∈ Icc L U := ⟨min_le_left _ _, le_max_left _ _⟩
  have hsub : Icc L U ⊆ Ioi (0:ℝ) := fun _ hy ↦ hL.trans_le hy.1
  obtain ⟨r,hr,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1,h1⟩ (hc.mono hsub)
  refine ⟨r,hsub hr,?_⟩
  intro s hs
  change f s ≤ f r
  by_cases h : s ∈ Icc L U
  · exact hm h
  · apply le_trans _ (hm h1)
    apply le_of_lt
    simp only [mem_Icc, not_and_or, not_le] at h
    rcases h with h | h
    · apply hb
      exact ⟨hs, h.trans_le ((min_le_right _ _).trans (half_le_self hε.le))⟩
    · exact hM _ ((le_max_right _ _).trans h.le)

private theorem cell_at_zero {a : ℝ} (ha : 0 < a) (k : ℕ) :
    Tendsto (fun r ↦ cellProb a r k) (𝓝[>] 0) (𝓝 0) := by
  have hz : cdf (gammaMeasure a 1) 0 = 0 := by rw [cdf_eq_interval ha le_rfl]; simp
  have hc (m : ℝ) (hm : 0 ≤ m) :
      Tendsto (fun r : ℝ ↦ cdf (gammaMeasure a 1) (m*r)) (𝓝[>] 0) (𝓝 0) := by
    have hh : Tendsto (fun r : ℝ ↦ m*r) (𝓝[>] 0) (𝓝[≥] 0) := by
      apply tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within
      · have hv : ContinuousAt (fun r : ℝ ↦ m*r) 0 := continuous_const.continuousAt.mul continuousAt_id
        simpa using hv.tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi (0:ℝ)))
      · exact (show ∀ᶠ t : ℝ in 𝓝[>] (0:ℝ), t ∈ Ioi (0:ℝ) from self_mem_nhdsWithin).mono fun r hr ↦
          mul_nonneg hm (mem_Ioi.mp hr).le
    simpa only [hz, Function.comp_def] using ((cdf (gammaMeasure a 1)).right_continuous 0).tendsto.comp hh
  simpa [cellProb] using (hc (k+1) (by positivity)).sub (hc k (by positivity))

private theorem cell_rate_atTop {a : ℝ} (k : ℕ) (hk : 0 < k) :
    Tendsto (fun r ↦ cellProb a r k) atTop (𝓝 0) := by
  have hc (m : ℝ) (hm : 0 < m) :
      Tendsto (fun r : ℝ ↦ cdf (gammaMeasure a 1) (m*r)) atTop (𝓝 1) :=
    (tendsto_cdf_atTop _).comp (tendsto_id.const_mul_atTop hm)
  simpa [cellProb] using (hc (k+1) (by positivity)).sub (hc k (by exact_mod_cast hk))

private theorem continuous_rate {n : ℕ} (x : Fin n → ℕ) {a : ℝ} (ha : 0 < a) :
    ContinuousOn (likelihood x a) (Ioi 0) := by
  apply continuousOn_finsetProd
  intro i _ r hr
  have hr' : 0 < r := hr
  have hd := (logRate_derivative ha (x i) (log r)).continuousAt
  have hl : ContinuousWithinAt log (Ioi 0) r := (continuousAt_log hr'.ne').continuousWithinAt
  have hEq : (fun s ↦ exp (logRate a (x i) (log s))) =ᶠ[𝓝[Ioi (0:ℝ)] r]
      (fun s ↦ cellProb a s (x i)) := by
    filter_upwards [self_mem_nhdsWithin] with s hs
    have hs' : 0 < s := hs
    simp [logRate, exp_log hs', exp_log (cell_pos ha hs' (x i))]
  exact ((continuous_exp.continuousAt.comp_continuousWithinAt
    (hd.comp_continuousWithinAt hl)).congr_of_eventuallyEq hEq.symm (by
      change cellProb a r (x i) = exp (logRate a (x i) (log r))
      rw [logRate, exp_log hr', exp_log (cell_pos ha hr' (x i))]))

private theorem sum_strictConcave {n : ℕ} {D : Set ℝ} {f : Fin n → ℝ → ℝ}
    (i : Fin n) (hc : ∀ j, StrictConcaveOn ℝ D (f j)) :
    StrictConcaveOn ℝ D (fun t ↦ ∑ j, f j t) := by
  refine ⟨(hc i).1, ?_⟩
  intro u hu v hv hne c d hc0 hd0 hsum
  simp only [Finset.smul_sum, ← Finset.sum_add_distrib]
  exact Finset.sum_lt_sum
    (fun j _ ↦ ((hc j).2 hu hv hne hc0 hd0 hsum).le)
    ⟨i, Finset.mem_univ _, (hc i).2 hu hv hne hc0 hd0 hsum⟩

private theorem log_likelihood {n : ℕ} (x : Fin n → ℕ) {a r : ℝ}
    (ha : 0 < a) (hr : 0 < r) :
    log (likelihood x a r) = ∑ i, log (cellProb a r (x i)) := by
  exact log_prod fun _ _ ↦ (cell_pos ha hr _).ne'

/-- For a sample containing a positive count, held positive shape gives a unique rate fit. -/
theorem held_shape {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    ∃! r, r ∈ Ioi 0 ∧ IsMaxOn (likelihood x a) (Ioi 0) r := by
  obtain ⟨i,hi⟩ := hx
  have ht : Tendsto (likelihood x a) (𝓝[>] 0) (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds (cell_at_zero ha (x i))
    · exact (show ∀ᶠ t : ℝ in 𝓝[>] (0:ℝ), t ∈ Ioi (0:ℝ) from self_mem_nhdsWithin).mono fun r hr ↦ (likelihood_pos x ha (mem_Ioi.mp hr)).le
    · exact (show ∀ᶠ t : ℝ in 𝓝[>] (0:ℝ), t ∈ Ioi (0:ℝ) from self_mem_nhdsWithin).mono fun r hr ↦ likelihood_le_cell x i ha (mem_Ioi.mp hr)
  have ht' : Tendsto (likelihood x a) atTop (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds (cell_rate_atTop (x i) hi)
    · exact (eventually_gt_atTop 0).mono fun r hr ↦ (likelihood_pos x ha hr).le
    · exact (eventually_gt_atTop 0).mono fun r hr ↦ likelihood_le_cell x i ha hr
  obtain ⟨r,hr,hm⟩ := attained_of_tails (continuous_rate x ha) (likelihood_pos x ha one_pos) ht ht'
  refine ⟨r,⟨hr,hm⟩,?_⟩
  intro s hs
  have hmax (q : ℝ) (hq : q ∈ Ioi 0) (h : IsMaxOn (likelihood x a) (Ioi 0) q) :
      IsMaxOn (fun t ↦ ∑ j, logRate a (x j) t) univ (log q) := by
    intro t _
    change (∑ j, log (cellProb a (exp t) (x j))) ≤ ∑ j, log (cellProb a (exp (log q)) (x j))
    rw [exp_log (mem_Ioi.mp hq), ← log_likelihood x ha (exp_pos t), ← log_likelihood x ha hq]
    exact log_le_log (likelihood_pos x ha (exp_pos t)) (h (mem_Ioi.mpr (exp_pos t)))
  have hlog := (sum_strictConcave i (fun j ↦ logRate_strictConcave ha (x j))).eq_of_isMaxOn
    (hmax s hs.1 hs.2) (hmax r hr hm) (mem_univ _) (mem_univ _)
  exact log_injOn_pos hs.1 hr hlog


private theorem density_moment {a : ℝ} (ha : 0 < a) :
    IntegrableOn (fun y ↦ y*density a y) (Ioi 0) ∧
      (∫ y in Ioi 0, y*density a y) = a := by
  have he (y : ℝ) (hy : 0 < y) :
      y*density a y = (exp (-y) * y^((a+1)-1))/Gamma a := by
    rw [density_formula a hy, show a+1-1 = a by ring]
    have hp : y^a = y^(a-1)*y := by
      conv_lhs => rw [show a = (a-1)+1 by ring, rpow_add hy, rpow_one]
    rw [hp]
    ring
  have hi := (GammaIntegral_convergent (s := a+1) (by linarith)).div_const (Gamma a)
  constructor
  · exact hi.congr ((ae_restrict_iff' measurableSet_Ioi).2 (ae_of_all _ fun y hy ↦ (he y hy).symm))
  · rw [setIntegral_congr_fun measurableSet_Ioi (fun y hy ↦ he y (mem_Ioi.mp hy)), integral_div,
      ← Gamma_eq_integral (show 0 < a+1 by linarith), Gamma_add_one ha.ne',
      mul_div_cancel_right₀ _ (Gamma_pos_of_pos ha).ne']

private theorem tail_bound {a z : ℝ} (ha : 0 < a) (hz : 0 < z) :
    (∫ y in Ioi z, density a y) ≤ a/z := by
  apply (le_div_iff₀ hz).2
  rw [mul_comm, ← integral_const_mul]
  have hm := density_moment ha
  have hs : Ioi z ⊆ Ioi (0:ℝ) := fun _ hy ↦ hz.trans hy
  calc
    _ ≤ ∫ y in Ioi z, y*density a y :=
      setIntegral_mono_on ((density_integrable ha).integrableOn.const_mul z)
        (hm.1.mono_set hs) measurableSet_Ioi
        (fun y hy ↦ mul_le_mul_of_nonneg_right hy.le (gammaPDFReal_nonneg ha one_pos y))
    _ ≤ ∫ y in Ioi (0:ℝ), y*density a y :=
      setIntegral_mono_set hm.1
        ((ae_restrict_iff' measurableSet_Ioi).2 (ae_of_all _ fun y hy ↦
          mul_nonneg (mem_Ioi.mp hy).le (gammaPDFReal_nonneg ha one_pos y))) (ae_of_all _ hs)
    _ = _ := hm.2

private theorem integral_density_all {a : ℝ} (ha : 0 < a) :
    (∫ y, density a y) = 1 := by
  change (∫ y, gammaPDFReal a 1 y) = 1
  rw [integral_eq_lintegral_of_nonneg_ae (ae_of_all _ (gammaPDFReal_nonneg ha one_pos))
    (density_integrable ha).aestronglyMeasurable]
  change (∫⁻ y, gammaPDF a 1 y).toReal = 1
  rw [lintegral_gammaPDF_eq_one ha one_pos, ENNReal.toReal_one]

private theorem cdf_shape_zero {r : ℝ} (hr : 0 < r) :
    Tendsto (fun a ↦ cdf (gammaMeasure a 1) r) (𝓝[>] 0) (𝓝 1) := by
  have he (a : ℝ) (ha : 0 < a) :
      cdf (gammaMeasure a 1) r + (∫ y in Ioi r, density a y) = 1 := by
    rw [cdf_gammaMeasure_eq_integral ha one_pos]
    change (∫ y in Iic r, density a y) + (∫ y in Ioi r, density a y) = 1
    rw [← setIntegral_union (by simp [Set.disjoint_left]) measurableSet_Ioi
      (density_integrable ha).integrableOn (density_integrable ha).integrableOn,
      Iic_union_Ioi, setIntegral_univ, integral_density_all ha]
  have hh : Tendsto (fun a : ℝ ↦ 1-a/r) (𝓝[>] 0) (𝓝 1) := by
    have hc : ContinuousAt (fun a : ℝ ↦ 1-a/r) 0 := by fun_prop
    simpa using hc.tendsto.mono_left nhdsWithin_le_nhds
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hh tendsto_const_nhds
  · filter_upwards [self_mem_nhdsWithin] with a ha
    change 0 < a at ha
    linarith [he a ha, tail_bound ha hr]
  · exact Eventually.of_forall fun _ ↦ cdf_le_one _ _

/-- At fixed positive rate, all-zero data approach the zero atom as shape decreases to zero. -/
theorem all_zero_held_rate {r : ℝ} (hr : 0 < r) {n : ℕ} (hn : 0 < n) :
    Tendsto (fun a ↦ (cdf (gammaMeasure a 1) r)^n) (𝓝[>] 0) (𝓝 1) ∧
      ∀ a > 0, (cdf (gammaMeasure a 1) r)^n < 1 := by
  refine ⟨by simpa using (cdf_shape_zero hr).pow n, ?_⟩
  intro a ha
  exact pow_lt_one₀ (cdf_nonneg _ _) (zeroMass_lt_one ha hr) hn.ne'

private theorem cell_shape_zero {r : ℝ} (hr : 0 < r) (k : ℕ) (hk : 0 < k) :
    Tendsto (fun a ↦ cellProb a r k) (𝓝[>] 0) (𝓝 0) := by
  have hkr : 0 < (k:ℝ)*r := mul_pos (by exact_mod_cast hk) hr
  simpa [cellProb] using
    (cdf_shape_zero (show 0 < (k+1:ℝ)*r by positivity)).sub (cdf_shape_zero hkr)

private theorem density_tilt_integrable {a : ℝ} (ha : 0 < a) :
    IntegrableOn (fun y ↦ exp (-y)*density a y) (Ioi 0) := by
  apply Integrable.bdd_mul (c := 1) ((density_integrable ha).integrableOn (s := Ioi 0))
  · exact (continuous_exp.comp continuous_neg).aestronglyMeasurable
  · filter_upwards [self_mem_ae_restrict measurableSet_Ioi] with y hy
    rw [Real.norm_eq_abs, abs_of_pos (exp_pos _), exp_le_one_iff]
    exact neg_nonpos.mpr (mem_Ioi.mp hy).le

private theorem density_tilt_integral {a : ℝ} (ha : 0 < a) :
    (∫ y in Ioi 0, exp (-y)*density a y) = (1/2:ℝ)^a := by
  have he (y : ℝ) (hy : 0 < y) :
      exp (-y)*density a y = y^(a-1)*exp (-(2*y))/Gamma a := by
    rw [density_formula a hy, show -(2*y) = -y + -y by ring, exp_add]
    ring
  rw [setIntegral_congr_fun measurableSet_Ioi (fun y hy ↦ he y (mem_Ioi.mp hy)), integral_div,
    integral_rpow_mul_exp_neg_mul_Ioi ha (by norm_num : 0 < (2:ℝ)),
    mul_div_cancel_right₀ _ (Gamma_pos_of_pos ha).ne']

private theorem cell_shape_bound {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (k : ℕ) :
    cellProb a r k ≤ exp ((k+1:ℝ)*r) * (1/2:ℝ)^a := by
  let l := (k:ℝ)*r
  let h := (k+1:ℝ)*r
  have hl : 0 ≤ l := by dsimp [l]; positivity
  have hsub : Ioc l h ⊆ Ioi (0:ℝ) := fun _ hy ↦ hl.trans_lt hy.1
  have htilt : IntegrableOn (fun y ↦ exp h * (exp (-y)*density a y)) (Ioi 0) :=
    (density_tilt_integrable ha).const_mul (exp h)
  rw [cell_integral ha hr]
  change (∫ y in Ioc l h, density a y) ≤ exp h * (1/2:ℝ)^a
  calc
    _ ≤ ∫ y in Ioc l h, exp h * (exp (-y)*density a y) :=
      setIntegral_mono_on (density_integrable ha).integrableOn (htilt.mono_set hsub)
        measurableSet_Ioc fun y hy ↦ by
          have he : 1 ≤ exp (h-y) := one_le_exp_iff.mpr (sub_nonneg.mpr hy.2)
          calc
            _ = 1 * density a y := by rw [one_mul]
            _ ≤ exp (h-y) * density a y := mul_le_mul_of_nonneg_right he
                (gammaPDFReal_nonneg ha one_pos y)
            _ = _ := by rw [sub_eq_add_neg, exp_add, mul_assoc]
    _ ≤ ∫ y in Ioi (0:ℝ), exp h * (exp (-y)*density a y) :=
      setIntegral_mono_set htilt (ae_of_all _ fun y ↦ mul_nonneg (exp_pos _).le
        (mul_nonneg (exp_pos _).le (gammaPDFReal_nonneg ha one_pos y))) (ae_of_all _ hsub)
    _ = _ := by rw [integral_const_mul, density_tilt_integral ha]

private theorem cell_shape_atTop {r : ℝ} (hr : 0 < r) (k : ℕ) :
    Tendsto (fun a ↦ cellProb a r k) atTop (𝓝 0) := by
  have hh : Tendsto (fun a : ℝ ↦ exp ((k+1:ℝ)*r) * (1/2:ℝ)^a) atTop (𝓝 0) := by
    have he : Tendsto (fun a : ℝ ↦ exp (log (1/2:ℝ)*a)) atTop (𝓝 0) :=
      tendsto_exp_atBot.comp (tendsto_id.const_mul_atTop_of_neg (log_neg (by norm_num) (by norm_num)))
    simpa only [rpow_def_of_pos (by norm_num : 0 < (1/2:ℝ)), mul_comm (log _), mul_zero]
      using he.const_mul (exp ((k+1:ℝ)*r))
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hh
  · exact (eventually_gt_atTop 0).mono fun a ha ↦ (cell_pos ha hr k).le
  · exact (eventually_gt_atTop 0).mono fun a ha ↦ cell_shape_bound ha hr k


private noncomputable def shapeKernel (a y : ℝ) := exp (-y)*y^(a-1)

private theorem continuous_shape_kernel_integral {l h a : ℝ} (hl : 0 ≤ l) (ha : 0 < a) :
    ContinuousAt (fun b ↦ ∫ y in Ioc l h, shapeKernel b y) a := by
  have hs : Ioc l h ⊆ Ioi (0:ℝ) := fun _ hy ↦ hl.trans_lt hy.1
  apply continuousAt_of_dominated (bound := fun y ↦ shapeKernel (a/2) y + shapeKernel (2*a) y)
  · exact Eventually.of_forall fun b ↦
      ((measurable_id.neg.exp).mul (measurable_id.pow_const (b-1))).aestronglyMeasurable
  · filter_upwards [Ioo_mem_nhds (show a/2 < a by linarith) (show a < 2*a by linarith)] with b hb
    filter_upwards [self_mem_ae_restrict measurableSet_Ioc] with y hy
    have hy' : 0 < y := hs hy
    rw [Real.norm_eq_abs, abs_of_pos (show 0 < shapeKernel b y from
      mul_pos (exp_pos _) (rpow_pos_of_pos hy' _))]
    unfold shapeKernel
    by_cases h1 : y ≤ 1
    · exact (mul_le_mul_of_nonneg_left (rpow_le_rpow_of_exponent_ge hy' h1
        (by linarith [hb.1])) (exp_pos _).le).trans
          (le_add_of_nonneg_right (mul_nonneg (exp_pos _).le (rpow_nonneg hy'.le _)))
    · exact (mul_le_mul_of_nonneg_left (rpow_le_rpow_of_exponent_le (le_of_not_ge h1)
        (by linarith [hb.2])) (exp_pos _).le).trans
          (le_add_of_nonneg_left (mul_nonneg (exp_pos _).le (rpow_nonneg hy'.le _)))
  · exact ((GammaIntegral_convergent (show 0 < a/2 by positivity)).mono_set hs).add
      ((GammaIntegral_convergent (show 0 < 2*a by positivity)).mono_set hs)
  · filter_upwards [self_mem_ae_restrict measurableSet_Ioc] with y hy
    exact continuousAt_const.mul (continuousAt_const_rpow (mem_Ioi.mp (hs hy)).ne' |>.comp
      (continuousAt_id.sub continuousAt_const))

private theorem cell_shape_continuous {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (k : ℕ) :
    ContinuousAt (fun b ↦ cellProb b r k) a := by
  let l := (k:ℝ)*r
  let h := (k+1:ℝ)*r
  have hl : 0 ≤ l := by dsimp [l]; positivity
  have hc := (continuous_shape_kernel_integral (h := h) hl ha).div
    (hasDerivAt_Gamma_of_pos ha).continuousAt (Gamma_pos_of_pos ha).ne'
  apply hc.congr
  filter_upwards [Ioi_mem_nhds ha] with b hb
  change (∫ y in Ioc l h, shapeKernel b y) / Gamma b = _
  rw [cell_integral (mem_Ioi.mp hb) hr, ← integral_div]
  apply setIntegral_congr_fun measurableSet_Ioc
  intro y hy
  change shapeKernel b y / Gamma b = density b y
  rw [density_formula b (hl.trans_lt hy.1), shapeKernel, mul_comm (exp _)]

/-- For a sample containing a positive count, held positive rate gives a finite shape fit. -/
theorem held_rate_attained {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) {r : ℝ} (hr : 0 < r) :
    ∃ a ∈ Ioi 0, IsMaxOn (fun b ↦ likelihood x b r) (Ioi 0) a := by
  obtain ⟨i,hi⟩ := hx
  have hc : ContinuousOn (fun b ↦ likelihood x b r) (Ioi 0) := by
    apply continuousOn_finsetProd
    exact fun j _ b hb ↦ (cell_shape_continuous hb hr (x j)).continuousWithinAt
  have ht : Tendsto (fun b ↦ likelihood x b r) (𝓝[>] 0) (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds (cell_shape_zero hr (x i) hi)
    · filter_upwards [self_mem_nhdsWithin] with b hb
      exact (likelihood_pos x (mem_Ioi.mp hb) hr).le
    · filter_upwards [self_mem_nhdsWithin] with b hb
      exact likelihood_le_cell x i (mem_Ioi.mp hb) hr
  have ht' : Tendsto (fun b ↦ likelihood x b r) atTop (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds (cell_shape_atTop hr (x i))
    · exact (eventually_gt_atTop 0).mono fun b hb ↦ (likelihood_pos x hb hr).le
    · exact (eventually_gt_atTop 0).mono fun b hb ↦ likelihood_le_cell x i hb hr
  exact attained_of_tails hc (likelihood_pos x one_pos hr) ht ht'

/-- The fixed-shape half of the held-parameter existence criterion. -/
theorem held_shape_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {a : ℝ} (ha : 0 < a) :
    (∃ r ∈ Ioi 0, IsMaxOn (likelihood x a) (Ioi 0) r) ↔ ∃ i, 0 < x i := by
  constructor
  · rintro ⟨r,hr,hm⟩
    by_contra! hx
    have he (s : ℝ) : likelihood x a s = (cdf (gammaMeasure a 1) s)^n := by
      simp [likelihood, cellProb, fun i ↦ Nat.eq_zero_of_le_zero (hx i),
        cdf_eq_interval ha (z := 0) le_rfl]
    have hb (s : ℝ) (hs : 0 < s) :
        (cdf (gammaMeasure a 1) s)^n ≤ (cdf (gammaMeasure a 1) r)^n := by
      rw [← he, ← he]
      exact hm hs
    have hf := all_zero_held_shape ha hn
    exact (not_le_of_gt (hf.2 r hr))
      (le_of_tendsto hf.1 ((eventually_gt_atTop 0).mono hb))
  · intro hx
    obtain ⟨r,hr,_⟩ := held_shape hx ha
    exact ⟨r,hr⟩

/-- The fixed-rate half of the held-parameter existence criterion. -/
theorem held_rate_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ} (hr : 0 < r) :
    (∃ a ∈ Ioi 0, IsMaxOn (fun b ↦ likelihood x b r) (Ioi 0) a) ↔ ∃ i, 0 < x i := by
  constructor
  · rintro ⟨a,ha,hm⟩
    by_contra! hx
    have he (b : ℝ) (hb : 0 < b) : likelihood x b r = (cdf (gammaMeasure b 1) r)^n := by
      simp [likelihood, cellProb, fun i ↦ Nat.eq_zero_of_le_zero (hx i),
        cdf_eq_interval hb (z := 0) le_rfl]
    have hb : ∀ᶠ b in 𝓝[>] (0:ℝ),
        (cdf (gammaMeasure b 1) r)^n ≤ (cdf (gammaMeasure a 1) r)^n := by
      filter_upwards [self_mem_nhdsWithin] with b hb
      rw [← he b (mem_Ioi.mp hb), ← he a ha]
      exact hm hb
    have hf := all_zero_held_rate hr hn
    exact (not_le_of_gt (hf.2 a ha)) (le_of_tendsto hf.1 hb)
  · exact fun hx ↦ held_rate_attained hx hr

/-- When both coefficients are held, the supplied pair is the sole maximizer. -/
theorem fully_held {n : ℕ} (x : Fin n → ℕ) (a r : ℝ) :
    ∃! p : ℝ × ℝ, p ∈ ({(a,r)} : Set (ℝ × ℝ)) ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) {(a,r)} p := by
  refine ⟨(a,r), ⟨rfl, ?_⟩, fun p hp ↦ hp.1⟩
  intro p hp
  rcases mem_singleton_iff.mp hp with rfl
  change likelihood x a r ≤ likelihood x a r
  exact le_rfl


private noncomputable def logMomentIntegrand (j : ℕ) (a y : ℝ) :=
  (log y)^j * shapeKernel a y

private theorem logMoment_measurable (j : ℕ) (a : ℝ) :
    Measurable (logMomentIntegrand j a) :=
  (measurable_log.pow_const j).mul ((measurable_id.neg.exp).mul (measurable_id.pow_const _))

private theorem logMoment_integrable (j : ℕ) {a : ℝ} (ha : 0 < a) :
    IntegrableOn (logMomentIntegrand j a) (Ioi 0) := by
  let δ := a/(2*(j+1 : ℝ))
  have hd : 0 < δ := by dsimp [δ]; positivity
  have ha' : 0 < a-δ*j := by
    have he : δ*(2*(j+1 : ℝ)) = a := by
      dsimp [δ]; exact div_mul_cancel₀ _ (by positivity)
    nlinarith [mul_nonneg hd.le (Nat.cast_nonneg (α := ℝ) j)]
  have hb : 0 < a+δ*j := by positivity
  let g := fun y ↦ δ⁻¹^j * (shapeKernel (a-δ*j) y + shapeKernel (a+δ*j) y)
  have hg : IntegrableOn g (Ioi 0) :=
    ((GammaIntegral_convergent ha').add (GammaIntegral_convergent hb)).const_mul _
  apply hg.mono' (logMoment_measurable j a).aestronglyMeasurable
  filter_upwards [self_mem_ae_restrict measurableSet_Ioi] with y hy
  have hy' : 0 < y := hy
  have help (t : ℝ) (ht : |log y| ≤ y^t/δ) :
      ‖logMomentIntegrand j a y‖ ≤ δ⁻¹^j * shapeKernel (a+t*j) y := by
    have he : (y^t/δ)^j * shapeKernel a y = δ⁻¹^j * shapeKernel (a+t*j) y := by
      unfold shapeKernel
      rw [div_eq_mul_inv, mul_pow, ← rpow_mul_natCast hy'.le]
      rw [show a+t*(j:ℝ)-1 = t*j+(a-1) by ring, rpow_add hy']
      ring
    rw [← he, Real.norm_eq_abs, logMomentIntegrand, abs_mul, abs_pow,
      abs_of_pos (show 0 < shapeKernel a y from mul_pos (exp_pos _) (rpow_pos_of_pos hy' _))]
    exact mul_le_mul_of_nonneg_right (pow_le_pow_left₀ (abs_nonneg _) ht _) (by
      unfold shapeKernel; positivity)
  change _ ≤ δ⁻¹^j * (shapeKernel (a-δ*j) y + shapeKernel (a+δ*j) y)
  by_cases h : 1 ≤ y
  · have hp := help δ (by rw [abs_of_nonneg (log_nonneg h)]; exact log_le_rpow_div hy'.le hd)
    exact hp.trans (mul_le_mul_of_nonneg_left (le_add_of_nonneg_left (by
      unfold shapeKernel; positivity)) (by positivity))
  · have hp := help (-δ) (by
      rw [abs_of_nonpos (log_nonpos hy'.le (le_of_not_ge h))]
      exact neg_le.mp (by simpa only [neg_div] using neg_rpow_div_le_log hy'.le hd))
    rw [show a + -δ * (j:ℝ) = a-δ*j by ring] at hp
    exact hp.trans (mul_le_mul_of_nonneg_left (le_add_of_nonneg_right (by
      unfold shapeKernel; positivity)) (by positivity))

private theorem logMoment_derivative {S : Set ℝ} (hS : MeasurableSet S)
    (hs : S ⊆ Ioi 0) (j : ℕ) {a : ℝ} (ha : 0 < a) :
    HasDerivAt (fun b ↦ ∫ y in S, logMomentIntegrand j b y)
      (∫ y in S, logMomentIntegrand (j+1) a y) a := by
  let bound := fun y ↦ ‖logMomentIntegrand (j+1) (a/2) y‖ +
    ‖logMomentIntegrand (j+1) (2*a) y‖
  have hhalf : 0 < a/2 := by positivity
  have hdouble : 0 < 2*a := by positivity
  have hB₀ : IntegrableOn bound (Ioi 0) :=
    (logMoment_integrable (j+1) hhalf).norm.add (logMoment_integrable (j+1) hdouble).norm
  have hB := hB₀.mono_set hs
  apply (hasDerivAt_integral_of_dominated_loc_of_deriv_le
    (Ioo_mem_nhds (show a/2 < a by linarith) (show a < 2*a by linarith))
    (Eventually.of_forall fun b ↦ (logMoment_measurable j b).aestronglyMeasurable)
    ((logMoment_integrable j ha).mono_set hs)
    ((logMoment_measurable (j+1) a).aestronglyMeasurable)
    (bound := bound) (F' := logMomentIntegrand (j+1)) ?_ hB ?_).2
  · filter_upwards [self_mem_ae_restrict hS] with y hy b hb
    have hy' : 0 < y := hs hy
    have hn (t : ℝ) : ‖logMomentIntegrand (j+1) t y‖ =
        |log y|^(j+1) * exp (-y) * y^(t-1) := by
      rw [Real.norm_eq_abs, logMomentIntegrand, shapeKernel, abs_mul, abs_mul, abs_pow,
        abs_of_pos (exp_pos _), abs_of_pos (rpow_pos_of_pos hy' _), mul_assoc]
    change _ ≤ ‖logMomentIntegrand (j+1) (a/2) y‖ + ‖logMomentIntegrand (j+1) (2*a) y‖
    simp only [hn]
    by_cases h : 1 ≤ y
    · have hp := rpow_le_rpow_of_exponent_le h (show b-1 ≤ 2*a-1 by linarith [hb.2])
      exact (mul_le_mul_of_nonneg_left hp (by positivity)).trans (le_add_of_nonneg_left (by positivity))
    · have hp := rpow_le_rpow_of_exponent_ge hy' (le_of_not_ge h)
        (show a/2-1 ≤ b-1 by linarith [hb.1])
      exact (mul_le_mul_of_nonneg_left hp (by positivity)).trans (le_add_of_nonneg_right (by positivity))
  · filter_upwards [self_mem_ae_restrict hS] with y hy b _
    have hy' : 0 < y := hs hy
    have hd := ((hasDerivAt_id b).sub_const 1).const_rpow hy'
    convert (hd.const_mul ((log y)^j*exp (-y))) using 1
    · funext t
      simp only [logMomentIntegrand, shapeKernel, id_eq]
      ring
    · simp only [logMomentIntegrand, shapeKernel, id_eq, mul_one, pow_succ]
      ring

private noncomputable def logMoment (S : Set ℝ) (j : ℕ) (a : ℝ) :=
  ∫ y in S, logMomentIntegrand j a y

private noncomputable def logGammaVariance (S : Set ℝ) (a : ℝ) :=
  logMoment S 2 a / logMoment S 0 a - (logMoment S 1 a / logMoment S 0 a)^2

private theorem logMoment_zero (S : Set ℝ) (a : ℝ) :
    logMoment S 0 a = ∫ y in S, shapeKernel a y := by
  simp [logMoment, logMomentIntegrand]

private theorem logMoment_full_zero {a : ℝ} (ha : 0 < a) :
    logMoment (Ioi 0) 0 a = Gamma a := by
  rw [logMoment_zero, Gamma_eq_integral ha]
  rfl

private theorem logMoment_cell_zero_pos {l h a : ℝ} (hl : 0 ≤ l) (hlh : l < h)
    (ha : 0 < a) : 0 < logMoment (Ioc l h) 0 a := by
  rw [logMoment_zero]
  apply int_pos hlh ((GammaIntegral_convergent ha).mono_set (fun _ hy ↦ hl.trans_lt hy.1))
  intro y hy
  exact mul_pos (exp_pos _) (rpow_pos_of_pos (hl.trans_lt hy.1) _)

private theorem logMoment_log_deriv {S : Set ℝ} (hS : MeasurableSet S) (hs : S ⊆ Ioi 0)
    {a : ℝ} (ha : 0 < a) (hZ : 0 < logMoment S 0 a) :
    HasDerivAt (fun b ↦ log (logMoment S 0 b)) (logMoment S 1 a / logMoment S 0 a) a :=
  (logMoment_derivative hS hs 0 ha).log hZ.ne'

private theorem interval_shape_curvature_eq_variances {l h a : ℝ}
    (hl : 0 ≤ l) (hlh : l < h) (ha : 0 < a) :
    deriv (deriv (fun b ↦ log ((∫ y in Ioc l h, shapeKernel b y) / Gamma b))) a =
      logGammaVariance (Ioc l h) a - logGammaVariance (Ioi 0) a := by
  let C := Ioc l h
  let G := Ioi (0:ℝ)
  have hc : C ⊆ Ioi 0 := fun _ hy ↦ hl.trans_lt hy.1
  have hC (b : ℝ) (hb : 0 < b) : 0 < logMoment C 0 b :=
    logMoment_cell_zero_pos hl hlh hb
  have hG (b : ℝ) (hb : 0 < b) : 0 < logMoment G 0 b := by
    rw [logMoment_full_zero hb]
    exact Gamma_pos_of_pos hb
  let score := fun b ↦ logMoment C 1 b / logMoment C 0 b - logMoment G 1 b / logMoment G 0 b
  have hEq :
      deriv (fun b ↦ log ((∫ y in Ioc l h, shapeKernel b y) / Gamma b)) =ᶠ[𝓝 a] score := by
    filter_upwards [Ioi_mem_nhds ha] with b hb
    have hb' : 0 < b := hb
    have hd := (logMoment_log_deriv measurableSet_Ioc hc hb' (hC b hb')).sub
      (logMoment_log_deriv measurableSet_Ioi (Subset.rfl) hb' (hG b hb'))
    apply (hd.congr_of_eventuallyEq ?_).deriv
    filter_upwards [Ioi_mem_nhds hb'] with t ht
    change 0 < t at ht
    change log ((∫ y in Ioc l h, shapeKernel t y) / Gamma t) =
      log (logMoment C 0 t) - log (logMoment G 0 t)
    rw [logMoment_full_zero ht, logMoment_zero,
      log_div ((logMoment_zero C t) ▸ (hC t ht).ne') (Gamma_pos_of_pos ht).ne']
  rw [hEq.deriv_eq]
  have hd := ((logMoment_derivative measurableSet_Ioc hc 1 ha).div
    (logMoment_derivative measurableSet_Ioc hc 0 ha) (hC a ha).ne').sub
    ((logMoment_derivative measurableSet_Ioi (Subset.rfl) 1 ha).div
    (logMoment_derivative measurableSet_Ioi (Subset.rfl) 0 ha) (hG a ha).ne')
  have hd' : HasDerivAt score _ a := hd
  rw [hd'.deriv]
  change (_ - _) = logGammaVariance C a - logGammaVariance G a
  unfold logGammaVariance
  have hC' := (hC a ha).ne'
  have hG' := (hG a ha).ne'
  change ((logMoment C 2 a * logMoment C 0 a - logMoment C 1 a * logMoment C 1 a) /
      (logMoment C 0 a)^2 -
    (logMoment G 2 a * logMoment G 0 a - logMoment G 1 a * logMoment G 1 a) /
      (logMoment G 0 a)^2) = _
  field_simp

private theorem gammaCDF_strictMono {a : ℝ} (ha : 0 < a) :
    StrictMonoOn (cdf (gammaMeasure a 1)) (Ici 0) := by
  apply strictMonoOn_of_deriv_pos (convex_Ici _)
  · intro y hy
    rcases (mem_Ici.mp hy).eq_or_lt with rfl | hy'
    · exact (cdf (gammaMeasure a 1)).right_continuous 0
    · exact (cdf_derivative ha hy').continuousAt.continuousWithinAt
  · intro y hy
    rw [interior_Ici] at hy
    rw [(cdf_derivative ha hy).deriv]
    exact gammaPDFReal_pos ha one_pos hy

private theorem gammaCDF_surj {a : ℝ} (ha : 0 < a) :
    SurjOn (cdf (gammaMeasure a 1)) (Ioi 0) (Ioo 0 1) := by
  intro p hp
  have hnear := (tendsto_cdf_atTop (gammaMeasure a 1)).eventually (Ioi_mem_nhds hp.2)
  obtain ⟨y, hpy, hy⟩ := (hnear.and (eventually_gt_atTop 0)).exists
  have hz : cdf (gammaMeasure a 1) 0 = 0 := by rw [cdf_eq_interval ha le_rfl]; simp
  have hc : ContinuousOn (cdf (gammaMeasure a 1)) (Icc 0 y) := by
    intro z hz
    rcases hz.1.eq_or_lt with rfl | hz'
    · exact ((cdf (gammaMeasure a 1)).right_continuous 0).mono Icc_subset_Ici_self
    · exact (cdf_derivative ha hz').continuousAt.continuousWithinAt
  obtain ⟨z, hz', hpz⟩ := intermediate_value_Icc (a := 0) (b := y) hy.le hc
    (show p ∈ Icc (cdf (gammaMeasure a 1) 0) (cdf (gammaMeasure a 1) y) from
      ⟨hz.symm ▸ hp.1.le, hpy.le⟩)
  refine ⟨z, ?_, hpz⟩
  refine (mem_Ioi.mpr ?_)
  by_contra! hn
  have he := le_antisymm hn hz'.1
  exact hp.1.ne' (hpz.symm.trans (he ▸ hz))

private noncomputable def gammaQuantile (a v : ℝ) :=
  if hv : 0 < a ∧ v ∈ Ioo (0:ℝ) 1 then (gammaCDF_surj hv.1 hv.2).choose else 0

private theorem gammaQuantile_spec {a v : ℝ} (ha : 0 < a) (hv : v ∈ Ioo 0 1) :
    0 < gammaQuantile a v ∧ cdf (gammaMeasure a 1) (gammaQuantile a v) = v := by
  rw [gammaQuantile, dite_eq_left ⟨ha,hv⟩]
  exact (gammaCDF_surj ha hv).choose_spec

private theorem gammaQuantile_mono {a : ℝ} (ha : 0 < a) :
    StrictMonoOn (gammaQuantile a) (Ioo 0 1) := by
  intro v hv w hw hvw
  have h₁ := gammaQuantile_spec ha hv
  have h₂ := gammaQuantile_spec ha hw
  by_contra! hn
  exact hvw.not_ge (h₂.2 ▸ h₁.2 ▸ monotone_cdf _ hn)

private theorem gammaQuantile_continuous {a v : ℝ} (ha : 0 < a) (hv : v ∈ Ioo 0 1) :
    ContinuousAt (gammaQuantile a) v := by
  rw [Metric.continuousAt_iff]
  intro ε hε
  let q := gammaQuantile a v
  have hq := gammaQuantile_spec ha hv
  let d := min (ε/2) (q/2)
  have hd : 0 < d := lt_min (half_pos hε) (half_pos hq.1)
  have hlow : 0 ≤ q-d := by
    have h := min_le_right (ε/2) (q/2)
    linarith [hq.1]
  have hl := gammaCDF_strictMono ha hlow hq.1.le (sub_lt_self q hd)
  have hh := gammaCDF_strictMono ha hq.1.le (show q+d ∈ Ici 0 from by
    exact add_nonneg hq.1.le hd.le) (lt_add_of_pos_right q hd)
  have hC : v ∈ Ioo (cdf (gammaMeasure a 1) (q-d)) (cdf (gammaMeasure a 1) (q+d)) :=
    ⟨hl.trans_eq hq.2, hq.2.symm.trans_lt hh⟩
  obtain ⟨δ,hδ,hball⟩ := Metric.mem_nhds_iff.mp
    (inter_mem (Ioo_mem_nhds hC.1 hC.2) (Ioo_mem_nhds hv.1 hv.2))
  refine ⟨δ,hδ,?_⟩
  intro w hw
  have hW := hball (Metric.mem_ball.mpr hw)
  have hqw := gammaQuantile_spec ha hW.2
  have hl' : q-d < gammaQuantile a w := by
    by_contra! hn
    exact hW.1.1.not_ge (hqw.2 ▸ monotone_cdf _ hn)
  have hh' : gammaQuantile a w < q+d := by
    by_contra! hn
    exact hW.1.2.not_ge (hqw.2 ▸ monotone_cdf _ hn)
  rw [Real.dist_eq, abs_lt]
  have hd' := min_le_left (ε/2) (q/2)
  change q-d < _ at hl'
  change _ < q+d at hh'
  change -ε < gammaQuantile a w - q ∧ gammaQuantile a w - q < ε
  constructor <;> linarith

private theorem gammaQuantile_deriv {a v : ℝ} (ha : 0 < a) (hv : v ∈ Ioo 0 1) :
    HasDerivAt (gammaQuantile a) (density a (gammaQuantile a v))⁻¹ v := by
  have hq := gammaQuantile_spec ha hv
  apply HasDerivAt.of_local_left_inverse (gammaQuantile_continuous ha hv)
    (cdf_derivative ha hq.1) (gammaPDFReal_pos ha one_pos hq.1).ne'
  filter_upwards [Ioo_mem_nhds hv.1 hv.2] with w hw
  exact (gammaQuantile_spec ha hw).2

private noncomputable def gammaQuantileFlux (a v : ℝ) := flux a (gammaQuantile a v)

private theorem gammaQuantileFlux_deriv {a v : ℝ} (ha : 0 < a) (hv : v ∈ Ioo 0 1) :
    HasDerivAt (gammaQuantileFlux a) (a - gammaQuantile a v) v := by
  have hq := gammaQuantile_spec ha hv
  have hp : density a (gammaQuantile a v) ≠ 0 := (gammaPDFReal_pos ha one_pos hq.1).ne'
  have hd := (flux_derivative (a := a) hq.1).comp v (gammaQuantile_deriv ha hv)
  change HasDerivAt (gammaQuantileFlux a)
    (((a - gammaQuantile a v) * density a (gammaQuantile a v)) *
      (density a (gammaQuantile a v))⁻¹) v at hd
  rwa [mul_inv_cancel_right₀ hp] at hd

private theorem gammaQuantileFlux_continuous {a : ℝ} (ha : 0 < a) :
    ContinuousOn (gammaQuantileFlux a) (Icc 0 1) := by
  have hfzero (v : ℝ) (hv : v ∉ Ioo (0:ℝ) 1) : gammaQuantileFlux a v = 0 := by
    simp [gammaQuantileFlux, gammaQuantile, hv, flux, Real.zero_rpow ha.ne']
  have hQ0 : Tendsto (gammaQuantile a) (𝓝[Ioo (0:ℝ) 1] 0) (𝓝 0) := by
    apply tendsto_order.2
    constructor
    · intro b hb
      filter_upwards [self_mem_nhdsWithin] with v hv
      exact hb.trans (gammaQuantile_spec ha hv).1
    · intro b hb
      have hpos := gammaCDF_strictMono ha (by simp : (0:ℝ) ∈ Ici 0) hb.le hb
      rw [cdf_eq_interval ha (z := 0) le_rfl, intervalIntegral.integral_same] at hpos
      filter_upwards [self_mem_nhdsWithin,
        mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hpos)] with v hv hv'
      have hq := (gammaQuantile_spec ha hv).2
      by_contra! hn
      exact (mem_Iio.mp hv').not_ge (hq ▸ monotone_cdf _ hn)
  have hQ1 : Tendsto (gammaQuantile a) (𝓝[Ioo (0:ℝ) 1] 1) atTop := by
    apply tendsto_atTop.2
    intro b
    let c := max 1 b
    have hc : 0 < c := one_pos.trans_le (le_max_left _ _)
    filter_upwards [self_mem_nhdsWithin, mem_nhdsWithin_of_mem_nhds
      (Ioi_mem_nhds (zeroMass_lt_one ha hc))] with v hv hv'
    have hq := (gammaQuantile_spec ha hv).2
    have hqc : c ≤ gammaQuantile a v := by
      by_contra! hn
      exact (mem_Ioi.mp hv').not_ge (hq ▸ monotone_cdf _ hn.le)
    exact (le_max_right 1 b).trans hqc
  have hF0 : Tendsto (gammaQuantileFlux a) (𝓝[Ioo (0:ℝ) 1] 0) (𝓝 0) := by
    have hf : ContinuousAt (flux a) 0 :=
      (((continuous_rpow_const ha.le).continuousAt).mul
        ((continuous_exp.comp continuous_neg).continuousAt)).div_const _
    have hf0 : flux a 0 = 0 := by simp [flux, Real.zero_rpow ha.ne']
    have hh : Tendsto (gammaQuantileFlux a) (𝓝[Ioo (0:ℝ) 1] 0) (𝓝 (flux a 0)) :=
      hf.tendsto.comp hQ0
    rwa [hf0] at hh
  have hF1 : Tendsto (gammaQuantileFlux a) (𝓝[Ioo (0:ℝ) 1] 1) (𝓝 0) := by
    have hf : Tendsto (flux a) atTop (𝓝 0) := by
      change Tendsto (fun x : ℝ ↦ x^a * exp (-x) / Gamma a) atTop (𝓝 0)
      simpa only [flux, neg_mul, one_mul, zero_div] using
        (tendsto_rpow_mul_exp_neg_mul_atTop_nhds_zero a 1 one_pos).div_const (Gamma a)
    exact hf.comp hQ1
  have hends : Icc (0:ℝ) 1 = insert 0 (insert 1 (Ioo 0 1)) := by
    ext x
    simp only [mem_Icc, mem_insert_iff, mem_Ioo]
    constructor
    · rintro ⟨h₀,h₁⟩
      rcases h₀.eq_or_lt with h | h
      · exact Or.inl h.symm
      rcases h₁.eq_or_lt with h' | h'
      · exact Or.inr (Or.inl h')
      · exact Or.inr (Or.inr ⟨h,h'⟩)
    · rintro (rfl | rfl | h)
      · norm_num
      · norm_num
      · exact ⟨h.1.le,h.2.le⟩
  intro v hv
  rcases hv.1.eq_or_lt with he | hv0
  · subst v
    have h : ContinuousWithinAt (gammaQuantileFlux a) (Ioo 0 1) 0 := by
      change Tendsto _ _ (𝓝 (gammaQuantileFlux a 0))
      rw [hfzero 0 (by simp)]
      exact hF0
    rw [hends, continuousWithinAt_insert, continuousWithinAt_insert]
    exact h
  · rcases hv.2.eq_or_lt with he | hv1
    · subst v
      have h : ContinuousWithinAt (gammaQuantileFlux a) (Ioo 0 1) 1 := by
        change Tendsto _ _ (𝓝 (gammaQuantileFlux a 1))
        rw [hfzero 1 (by simp)]
        exact hF1
      rw [hends, continuousWithinAt_insert, continuousWithinAt_insert]
      exact h
    · exact (gammaQuantileFlux_deriv ha ⟨hv0,hv1⟩).continuousAt.continuousWithinAt

private theorem gammaQuantileFlux_strictConcave {a : ℝ} (ha : 0 < a) :
    StrictConcaveOn ℝ (Icc 0 1) (gammaQuantileFlux a) := by
  apply StrictAntiOn.strictConcaveOn_of_deriv (convex_Icc _ _) (gammaQuantileFlux_continuous ha)
  rw [interior_Icc]
  intro v hv w hw hvw
  rw [(gammaQuantileFlux_deriv ha hv).deriv, (gammaQuantileFlux_deriv ha hw).deriv]
  exact sub_lt_sub_left (gammaQuantile_mono ha hv hw hvw) a

private noncomputable def logGammaQuantile (a v : ℝ) := log (gammaQuantile a v)

private theorem logGammaQuantile_deriv {a v : ℝ} (ha : 0 < a) (hv : v ∈ Ioo 0 1) :
    HasDerivAt (logGammaQuantile a) (gammaQuantileFlux a v)⁻¹ v := by
  have hq := gammaQuantile_spec ha hv
  have hd := (gammaQuantile_deriv ha hv).log hq.1.ne'
  convert hd using 1
  · rfl
  rw [gammaQuantileFlux, flux_id hq.1, mul_inv, div_eq_mul_inv, mul_comm]

private theorem logGammaQuantile_spacing_contraction {a d p v w : ℝ}
    (ha : 0 < a) (hd : 0 ≤ d) (hp : 0 < p) (hp' : p < 1) (hdp : d+p ≤ 1)
    (hv : v ∈ Ioo 0 1) (hw : w ∈ Ioo 0 1) (hvw : v < w) :
    logGammaQuantile a (d+p*w) - logGammaQuantile a (d+p*v) <
      logGammaQuantile a w - logGammaQuantile a v := by
  have harg (x : ℝ) (hx : x ∈ Ioo 0 1) : d+p*x ∈ Ioo 0 1 := by
    exact ⟨add_pos_of_nonneg_of_pos hd (mul_pos hp hx.1),
      (add_lt_add_right (mul_lt_of_lt_one_right hp hx.2) d).trans_le hdp⟩
  have hflux (x : ℝ) (hx : x ∈ Ioo 0 1) : 0 < gammaQuantileFlux a x := by
    have hqx := (gammaQuantile_spec ha hx).1
    rw [gammaQuantileFlux, flux_id hqx]
    exact mul_pos hqx (gammaPDFReal_pos ha one_pos hqx)
  have hstrict (x : ℝ) (hx : x ∈ Ioo 0 1) :
      p * gammaQuantileFlux a x < gammaQuantileFlux a (d+p*x) := by
    let q := d/(1-p)
    have hq : q ∈ Icc 0 1 := ⟨div_nonneg hd (sub_pos.mpr hp').le,
      (div_le_one (sub_pos.mpr hp')).mpr (by linarith)⟩
    have he : p*x+(1-p)*q = d+p*x := by
      dsimp [q]
      rw [mul_div_cancel₀ _ (sub_ne_zero.mpr hp'.ne')]
      ring
    by_cases heq : x = q
    · have he' : d+p*x = x := by rw [← he, ← heq]; ring
      rw [he']
      exact mul_lt_of_lt_one_left (hflux x hx) hp'
    · have hj := (gammaQuantileFlux_strictConcave ha).2 (Ioo_subset_Icc_self hx)
        hq heq hp (sub_pos.mpr hp') (by ring : p+(1-p)=1)
      have hqnonneg : 0 ≤ gammaQuantileFlux a q := by
        by_cases hmem : q ∈ Ioo (0:ℝ) 1
        · exact (hflux q hmem).le
        · simp [gammaQuantileFlux, gammaQuantile, hmem, flux, Real.zero_rpow ha.ne']
      change p * gammaQuantileFlux a x + (1-p)*gammaQuantileFlux a q <
        gammaQuantileFlux a (p*x+(1-p)*q) at hj
      rw [he] at hj
      exact (le_add_of_nonneg_right (mul_nonneg (sub_pos.mpr hp').le hqnonneg)).trans_lt hj
  let f := fun x ↦ logGammaQuantile a x - logGammaQuantile a (d+p*x)
  have hder (x : ℝ) (hx : x ∈ Ioo 0 1) :
      HasDerivAt f ((gammaQuantileFlux a x)⁻¹ -
        (gammaQuantileFlux a (d+p*x))⁻¹*p) x := by
    have haff : HasDerivAt (fun x : ℝ ↦ d+p*x) p x := by
      simpa using ((hasDerivAt_id x).const_mul p).const_add d
    have hcomp : HasDerivAt (fun t ↦ logGammaQuantile a (d+p*t))
        ((gammaQuantileFlux a (d+p*x))⁻¹*p) x :=
      by
        have he := (logGammaQuantile_deriv ha (harg x hx)).comp x haff
        exact he
    exact (logGammaQuantile_deriv ha hx).sub hcomp
  have hm : StrictMonoOn f (Ioo 0 1) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioo _ _)
      (fun x hx ↦ (hder x hx).continuousAt.continuousWithinAt)
    intro x hx
    have hx' := interior_subset hx
    rw [(hder x hx').deriv]
    apply sub_pos.mpr
    rw [mul_comm, ← div_eq_mul_inv, ← one_div]
    apply (div_lt_div_iff₀ (hflux _ (harg x hx')) (hflux x hx')).2
    simpa only [one_mul] using hstrict x hx'
  have he := hm hv hw hvw
  dsimp [f] at he
  linarith

private theorem gammaCDF_quantile {a y : ℝ} (ha : 0 < a) (hy : 0 < y)
    (h : cdf (gammaMeasure a 1) y ∈ Ioo 0 1) :
    gammaQuantile a (cdf (gammaMeasure a 1) y) = y := by
  have hq := gammaQuantile_spec ha h
  exact (gammaCDF_strictMono ha).injOn hq.1.le hy.le hq.2

private theorem logQuantile_moments {S : Set ℝ} (hS : MeasurableSet S)
    (hs : S ⊆ Ioi 0) {a d p : ℝ} (ha : 0 < a) (hp : 0 < p)
    (himage : (fun y ↦ (cdf (gammaMeasure a 1) y-d)/p) '' S = Ioo 0 1) (j : ℕ) :
    IntegrableOn (fun v ↦ logGammaQuantile a (d+p*v)^j) (Ioo 0 1) ∧
      (∫ v in Ioo 0 1, logGammaQuantile a (d+p*v)^j) = logMoment S j a / (p*Gamma a) := by
  let A := fun y ↦ (cdf (gammaMeasure a 1) y-d)/p
  let g := fun v ↦ logGammaQuantile a (d+p*v)^j
  change A '' S = Ioo 0 1 at himage
  have hA : MonotoneOn A S := fun x _ y _ h ↦
    div_le_div_of_nonneg_right (sub_le_sub_right (monotone_cdf _ h) _) hp.le
  have hder (y : ℝ) (hy : y ∈ S) :
      HasDerivWithinAt A (density a y / p) S y :=
    (((cdf_derivative ha (hs hy)).sub_const d).div_const p).hasDerivWithinAt
  have heq : EqOn (fun y ↦ (density a y / p) • g (A y))
      (fun y ↦ logMomentIntegrand j a y / (p*Gamma a)) S := by
    intro y hy
    have hAv : A y ∈ Ioo 0 1 := himage ▸ mem_image_of_mem A hy
    have hc : d+p*A y = cdf (gammaMeasure a 1) y := by
      dsimp [A]
      rw [mul_div_cancel₀ _ hp.ne']
      ring
    have hC : cdf (gammaMeasure a 1) y ∈ Ioo 0 1 := by
      refine ⟨?_, zeroMass_lt_one ha (hs hy)⟩
      have hh := gammaCDF_strictMono ha (show (0:ℝ) ∈ Ici 0 by simp)
        (mem_Ici.mpr (mem_Ioi.mp (hs hy)).le) (hs hy)
      exact (cdf_nonneg _ _).trans_lt hh
    simp only [g, hc, logGammaQuantile, gammaCDF_quantile ha (hs hy) hC, smul_eq_mul,
      density_formula a (hs hy), logMomentIntegrand, shapeKernel]
    ring
  constructor
  · rw [← himage, integrableOn_image_iff_integrableOn_deriv_smul_of_monotoneOn hS hder hA]
    have hi : IntegrableOn (fun y ↦ logMomentIntegrand j a y / (p*Gamma a)) S :=
      ((logMoment_integrable j ha).mono_set hs).div_const _
    exact hi.congr_fun heq.symm hS
  · change (∫ v in Ioo 0 1, g v) = _
    rw [← himage, integral_image_eq_integral_deriv_smul_of_monotoneOn hS hder hA,
      setIntegral_congr_fun hS heq, integral_div]
    rfl

private theorem gamma_interval_rescaled_image {a l h : ℝ} (ha : 0 < a) (hl : 0 ≤ l) (hlh : l < h) :
    let F := cdf (gammaMeasure a 1)
    let p := F h - F l
    0 < p ∧ p < 1 ∧
      (fun y ↦ (F y-F l)/p) '' Ioo l h = Ioo 0 1 := by
  dsimp only
  let F := cdf (gammaMeasure a 1)
  let p := F h - F l
  have hF := gammaCDF_strictMono ha
  have hp : 0 < p := sub_pos.mpr (hF hl (hl.trans hlh.le) hlh)
  have hp' : p < 1 := (sub_le_self _ (cdf_nonneg _ _)).trans_lt (zeroMass_lt_one ha (hl.trans_lt hlh))
  refine ⟨hp,hp',?_⟩
  change (fun y ↦ (F y-F l)/p) '' Ioo l h = Ioo 0 1
  ext v
  constructor
  · rintro ⟨y,hy,rfl⟩
    refine ⟨div_pos (sub_pos.mpr (hF hl (hl.trans hy.1.le) hy.1)) hp, (div_lt_one hp).2 ?_⟩
    exact sub_lt_sub_right (hF (hl.trans hy.1.le) (hl.trans hlh.le) hy.2) _
  · intro hv
    have hv' : F l < F l+p*v ∧ F l+p*v < F h := by
      have hm : 0 < p*v := mul_pos hp hv.1
      have hm' : p*v < p := mul_lt_of_lt_one_right hp hv.2
      dsimp [p] at hm'
      constructor <;> linarith
    have hmem : F l+p*v ∈ Ioo 0 1 :=
      ⟨(cdf_nonneg _ _).trans_lt hv'.1, hv'.2.trans (zeroMass_lt_one ha (hl.trans_lt hlh))⟩
    let q := gammaQuantile a (F l+p*v)
    have hq := gammaQuantile_spec ha hmem
    have hlq : l < q := by
      by_contra! h
      exact hv'.1.not_ge (hq.2 ▸ monotone_cdf _ h)
    have hqh : q < h := by
      by_contra! h
      exact hv'.2.not_ge (hq.2 ▸ monotone_cdf _ h)
    refine ⟨q,⟨hlq,hqh⟩,?_⟩
    change (F q-F l)/p = v
    rw [show F q = F l+p*v from hq.2, add_sub_cancel_left, mul_comm, mul_div_cancel_right₀ _ hp.ne']

private theorem gamma_full_rescaled_image {a : ℝ} (ha : 0 < a) :
    (fun y ↦ (cdf (gammaMeasure a 1) y-(0:ℝ))/1) '' Ioi 0 = Ioo 0 1 := by
  simp only [sub_zero, div_one]
  apply subset_antisymm _ (gammaCDF_surj ha)
  rintro _ ⟨y,hy,rfl⟩
  have hh := gammaCDF_strictMono ha (show (0:ℝ) ∈ Ici 0 by simp)
    (mem_Ici.mpr (mem_Ioi.mp hy).le) hy
  exact ⟨(cdf_nonneg _ _).trans_lt hh, zeroMass_lt_one ha hy⟩

private theorem squared_spacing_moment {f : ℝ → ℝ}
    (h₁ : IntegrableOn f (Ioo 0 1)) (h₂ : IntegrableOn (fun v ↦ f v ^ 2) (Ioo 0 1)) :
    let μ : Measure ℝ := volume.restrict (Ioo 0 1)
    Integrable (fun z : ℝ × ℝ ↦ (f z.2 - f z.1)^2) (μ.prod μ) ∧
      (∫ z : ℝ × ℝ, (f z.2 - f z.1)^2 ∂(μ.prod μ)) =
        2 * ((∫ v, f v^2 ∂μ) - (∫ v, f v ∂μ)^2) := by
  let μ : Measure ℝ := volume.restrict (Ioo 0 1)
  have hmass : μ univ = 1 := by simp [μ, Real.volume_Ioo]
  have hconst : (∫ _ : ℝ, (1:ℝ) ∂μ) = 1 := by rw [integral_const, measureReal_def, hmass]; simp
  have h₀ : Integrable (fun _ : ℝ ↦ (1:ℝ)) μ := integrable_const _
  let A := fun z : ℝ × ℝ ↦ f z.2^2 * (1:ℝ)
  let B := fun z : ℝ × ℝ ↦ f z.1^2 * (1:ℝ)
  let C := fun z : ℝ × ℝ ↦ f z.1*f z.2
  have hA : Integrable A (μ.prod μ) := by
    have ha := h₀.mul_prod h₂
    exact ha.congr (Eventually.of_forall fun z ↦ by simp [A])
  have hB : Integrable B (μ.prod μ) := h₂.mul_prod h₀
  have hC : Integrable C (μ.prod μ) := h₁.mul_prod h₁
  have hC' : Integrable (fun z ↦ 2*C z) (μ.prod μ) := hC.const_mul 2
  have he : (fun z : ℝ × ℝ ↦ (f z.2-f z.1)^2) = (A+B)-(fun z ↦ 2*C z) := by
    funext z
    simp only [Pi.add_apply, Pi.sub_apply, A, B, C]
    ring
  rw [he]
  refine ⟨(hA.add hB).sub hC', ?_⟩
  change (∫ z : ℝ × ℝ, (A+B) z - 2*C z ∂(μ.prod μ)) = _
  rw [integral_sub (hA.add hB) hC']
  simp only [Pi.add_apply]
  rw [integral_add hA hB, integral_const_mul]
  have hIntA : (∫ z, A z ∂(μ.prod μ)) = ∫ v, f v^2 ∂μ := by
    simp only [A, mul_one]
    simpa only [one_mul, hconst] using
      (integral_prod_mul (μ := μ) (ν := μ) (fun _ : ℝ ↦ (1:ℝ)) (fun v ↦ f v^2))
  have hIntB : (∫ z, B z ∂(μ.prod μ)) = ∫ v, f v^2 ∂μ := by
    change (∫ z : ℝ × ℝ, f z.1^2 * (1:ℝ) ∂(μ.prod μ)) = _
    simpa only [hconst, mul_one] using
      (integral_prod_mul (μ := μ) (ν := μ) (fun v ↦ f v^2) (fun _ : ℝ ↦ (1:ℝ)))
  have hIntC : (∫ z, C z ∂(μ.prod μ)) = (∫ v, f v ∂μ)^2 := by
    change (∫ z : ℝ × ℝ, f z.1 * f z.2 ∂(μ.prod μ)) = _
    rw [integral_prod_mul, pow_two]
  rw [hIntA,hIntB,hIntC]
  ring

private theorem logQuantile_variance_contraction {a d p : ℝ}
    (ha : 0 < a) (hd : 0 ≤ d) (hp : 0 < p) (hp' : p < 1) (hdp : d+p ≤ 1)
    (hf₁ : IntegrableOn (logGammaQuantile a) (Ioo 0 1))
    (hf₂ : IntegrableOn (fun v ↦ logGammaQuantile a v ^ 2) (Ioo 0 1))
    (hg₁ : IntegrableOn (fun v ↦ logGammaQuantile a (d+p*v)) (Ioo 0 1))
    (hg₂ : IntegrableOn (fun v ↦ logGammaQuantile a (d+p*v)^2) (Ioo 0 1)) :
    let μ : Measure ℝ := volume.restrict (Ioo 0 1)
    (∫ v, logGammaQuantile a (d+p*v)^2 ∂μ) -
        (∫ v, logGammaQuantile a (d+p*v) ∂μ)^2 <
      (∫ v, logGammaQuantile a v^2 ∂μ) - (∫ v, logGammaQuantile a v ∂μ)^2 := by
  let U := Ioo (0:ℝ) 1
  let μ : Measure ℝ := volume.restrict U
  let f := logGammaQuantile a
  let g := fun v ↦ f (d+p*v)
  let loss := fun z : ℝ × ℝ ↦ (f z.2-f z.1)^2 - (g z.2-g z.1)^2
  have hf := squared_spacing_moment hf₁ hf₂
  have hg := squared_spacing_moment hg₁ hg₂
  have hi : Integrable loss (μ.prod μ) := hf.1.sub hg.1
  have hgpos {v w : ℝ} (hv : v ∈ U) (hw : w ∈ U) (h : v < w) : 0 < g w - g v := by
    have harg (x : ℝ) (hx : x ∈ U) : d+p*x ∈ Ioo 0 1 := by
      refine ⟨add_pos_of_nonneg_of_pos hd (mul_pos hp hx.1), ?_⟩
      exact (add_lt_add_right (mul_lt_of_lt_one_right hp hx.2) d).trans_le hdp
    have hq := gammaQuantile_mono ha (harg v hv) (harg w hw)
      (add_lt_add_right (mul_lt_mul_of_pos_left h hp) d)
    exact sub_pos.mpr (log_lt_log (gammaQuantile_spec ha (harg v hv)).1 hq)
  have hstrict {v w : ℝ} (hv : v ∈ U) (hw : w ∈ U) (h : v < w) : 0 < loss (v,w) := by
    have hh := logGammaQuantile_spacing_contraction ha hd hp hp' hdp hv hw h
    exact sub_pos.mpr (pow_lt_pow_left₀ hh (hgpos hv hw h).le (by decide : 2 ≠ 0))
  have hnonneg : 0 ≤ᵐ[μ.prod μ] loss := by
    have hU : ∀ᵐ v ∂μ, v ∈ U := self_mem_ae_restrict measurableSet_Ioo
    have hUU : ∀ᵐ z ∂(μ.prod μ), z ∈ U ×ˢ U := by
      apply (Measure.ae_prod_mem_iff_ae_ae_mem (measurableSet_Ioo.prod measurableSet_Ioo)).2
      filter_upwards [hU] with v hv
      filter_upwards [hU] with w hw
      exact ⟨hv,hw⟩
    filter_upwards [hUU] with z hz
    rcases lt_trichotomy z.1 z.2 with h | h | h
    · exact (hstrict hz.1 hz.2 h).le
    · simp [loss, h]
    · have hrev := (hstrict hz.2 hz.1 h).le
      change 0 ≤ (f z.1-f z.2)^2 - (g z.1-g z.2)^2 at hrev
      change 0 ≤ (f z.2-f z.1)^2 - (g z.2-g z.1)^2
      convert hrev using 1
      ring
  have hpos : 0 < ∫ z, loss z ∂(μ.prod μ) := by
    rw [integral_pos_iff_support_of_nonneg_ae hnonneg hi]
    let U₁ := Ioo (0:ℝ) (1/4)
    let U₂ := Ioo (1/2:ℝ) (3/4)
    have hsub₁ : U₁ ⊆ U := fun v hv ↦ ⟨hv.1, hv.2.trans (by norm_num)⟩
    have hsub₂ : U₂ ⊆ U := fun v hv ↦ ⟨(by norm_num : (0:ℝ) < 1/2).trans hv.1,
      hv.2.trans (by norm_num)⟩
    have hsub : U₁ ×ˢ U₂ ⊆ Function.support loss := by
      intro z hz
      exact (hstrict (hsub₁ hz.1) (hsub₂ hz.2)
        (hz.1.2.trans ((by norm_num : (1/4:ℝ) < 1/2).trans hz.2.1))).ne'
    apply lt_of_lt_of_le _ (measure_mono hsub)
    rw [Measure.prod_prod, Measure.restrict_apply measurableSet_Ioo,
      Measure.restrict_apply measurableSet_Ioo, inter_eq_left.mpr hsub₁, inter_eq_left.mpr hsub₂]
    norm_num [U₁,U₂,Real.volume_Ioo]
  change 0 < ∫ z : ℝ × ℝ, (f z.2-f z.1)^2-(g z.2-g z.1)^2 ∂(μ.prod μ) at hpos
  rw [integral_sub hf.1 hg.1, hf.2, hg.2] at hpos
  linarith

/-- The shape curvature of a proper interval probability under the unit-rate Gamma law.
The required inequality is the strict decrease of log-Gamma variance after conditioning
on this interval. -/
private theorem gamma_interval_shape_curvature {l h a : ℝ}
    (hl : 0 ≤ l) (hlh : l < h) (ha : 0 < a) :
    deriv (deriv (fun b ↦ log ((∫ y in Ioc l h, shapeKernel b y) / Gamma b))) a < 0 := by
  rw [interval_shape_curvature_eq_variances hl hlh ha]
  apply sub_neg.mpr
  let F := cdf (gammaMeasure a 1)
  let d := F l
  let p := F h - F l
  let U := Ioo (0:ℝ) 1
  let C := Ioo l h
  let G := Ioi (0:ℝ)
  obtain ⟨hp, hp', himage⟩ := gamma_interval_rescaled_image ha hl hlh
  change 0 < p at hp
  change p < 1 at hp'
  have hdp : d+p ≤ 1 := by dsimp [d,p]; rw [add_sub_cancel]; exact cdf_le_one _ _
  have hd : 0 ≤ d := cdf_nonneg _ _
  have hs : C ⊆ Ioi 0 := fun _ hy ↦ hl.trans_lt hy.1
  have hC j := logQuantile_moments measurableSet_Ioo hs ha hp himage j
  have hG j := logQuantile_moments measurableSet_Ioi Subset.rfl ha one_pos
    (gamma_full_rescaled_image ha) j
  have hGC j :
      IntegrableOn (fun v ↦ logGammaQuantile a v^j) U ∧
        (∫ v in U, logGammaQuantile a v^j) = logMoment G j a / Gamma a := by
    simpa only [one_mul, zero_add] using hG j
  have hG0 : Gamma a = logMoment G 0 a := (logMoment_full_zero ha).symm
  have hC0 : p*Gamma a = logMoment C 0 a := by
    have he := (hC 0).2
    have hh : (∫ _ in U, (1:ℝ)) = 1 := by
      simp [U, measureReal_def, Real.volume_Ioo]
    simp only [pow_zero] at he
    rw [hh, eq_div_iff (mul_ne_zero hp.ne' (Gamma_pos_of_pos ha).ne'), one_mul] at he
    exact he
  have hg₁ : IntegrableOn (fun v ↦ logGammaQuantile a (d+p*v)) U := by
    simpa only [pow_one] using (hC 1).1
  have hf₁ : IntegrableOn (logGammaQuantile a) U := by
    simpa only [pow_one] using (hGC 1).1
  have hvar := logQuantile_variance_contraction ha hd hp hp' hdp hf₁ (hGC 2).1 hg₁ (hC 2).1
  have hg₁' := (hC 1).2
  have hf₁' := (hGC 1).2
  simp only [pow_one] at hg₁' hf₁'
  change (∫ v in U, logGammaQuantile a (d+p*v)^2) -
      (∫ v in U, logGammaQuantile a (d+p*v))^2 <
    (∫ v in U, logGammaQuantile a v^2) - (∫ v in U, logGammaQuantile a v)^2 at hvar
  rw [(hC 2).2, (hGC 2).2, hg₁', hf₁', hC0, hG0] at hvar
  have he (j : ℕ) : logMoment (Ioc l h) j a = logMoment C j a := by
    unfold logMoment
    rw [integral_Ioc_eq_integral_Ioo]
  unfold logGammaVariance
  rw [he 2, he 1, he 0]
  exact hvar

private theorem log_cell_shape_strictConcave {r : ℝ} (hr : 0 < r) (k : ℕ) :
    StrictConcaveOn ℝ (Ioi 0) (fun a ↦ log (cellProb a r k)) := by
  let l := (k:ℝ)*r
  let h := (k+1:ℝ)*r
  have hl : 0 ≤ l := by dsimp [l]; positivity
  have hlh : l < h := by dsimp [l,h]; nlinarith
  have he : EqOn (fun a ↦ log (cellProb a r k))
      (fun a ↦ log ((∫ y in Ioc l h, shapeKernel a y) / Gamma a)) (Ioi 0) := by
    intro a ha
    apply congrArg log
    rw [cell_integral (mem_Ioi.mp ha) hr, ← integral_div]
    apply setIntegral_congr_fun measurableSet_Ioc
    intro y hy
    change density a y = shapeKernel a y / Gamma a
    rw [density_formula a (hl.trans_lt hy.1), shapeKernel, mul_comm (exp _)]
  apply strictConcaveOn_of_deriv2_neg' (convex_Ioi 0)
  · intro a ha
    have hc : ContinuousAt (fun b ↦ log (cellProb b r k)) a :=
      (continuousAt_log (cell_pos ha hr k).ne').comp_of_eq (cell_shape_continuous ha hr k) rfl
    exact hc.continuousWithinAt
  · intro a ha
    have hEq : deriv (fun b ↦ log (cellProb b r k)) =ᶠ[𝓝 a]
          deriv (fun b ↦ log ((∫ y in Ioc l h, shapeKernel b y) / Gamma b)) := by
      filter_upwards [Ioi_mem_nhds (mem_Ioi.mp ha)] with t ht
      apply Filter.EventuallyEq.deriv_eq
      filter_upwards [Ioi_mem_nhds (mem_Ioi.mp ht)] with b hb
      exact he hb
    change deriv (deriv (fun b ↦ log (cellProb b r k))) a < 0
    rw [hEq.deriv_eq]
    exact gamma_interval_shape_curvature hl hlh ha

/-- With rate held, a positive sample has a unique shape maximizer. -/
theorem held_rate {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) {r : ℝ} (hr : 0 < r) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun b ↦ likelihood x b r) (Ioi 0) a := by
  obtain ⟨a,ha,hm⟩ := held_rate_attained hx hr
  obtain ⟨i,_⟩ := hx
  refine ⟨a,⟨ha,hm⟩,?_⟩
  intro b hb
  have hmax (q : ℝ) (hq : q ∈ Ioi 0)
      (hqmax : IsMaxOn (fun s ↦ likelihood x s r) (Ioi 0) q) :
      IsMaxOn (fun s ↦ ∑ j, log (cellProb s r (x j))) (Ioi 0) q := by
    intro t ht
    change (∑ j, log (cellProb t r (x j))) ≤ ∑ j, log (cellProb q r (x j))
    rw [← log_likelihood x ht hr, ← log_likelihood x hq hr]
    exact log_le_log (likelihood_pos x ht hr) (hqmax ht)
  exact (sum_strictConcave i (fun j ↦ log_cell_shape_strictConcave hr (x j))).eq_of_isMaxOn
    (hmax b hb.1 hb.2) (hmax a ha hm) hb.1 ha

end UnivariateML.Discrete3DGamma

end
