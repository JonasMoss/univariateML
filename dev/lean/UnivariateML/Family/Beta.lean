module

public import Mathlib.Analysis.SpecialFunctions.Gamma.Beta
public import UnivariateML.Family.Gamma
public import UnivariateML.Generic.Escape

import Mathlib.MeasureTheory.Integral.DominatedConvergence
import Mathlib.MeasureTheory.Measure.OpenPos
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Beta maximum likelihood

The beta integral supplies the joint convexity and bounds on the normalizer.
Coercivity is stated on the feasible subtype so that it includes mixed
escapes, such as one shape decreasing to zero while the other diverges.

## Deviations

The constant-sample theorem proves divergence along the dossier's ray by
dominated convergence. It does not state the Stirling growth rate. The held
shape proofs use coercivity and strict concavity in place of score limits.
The dossier's ray-score root assertion requires a restriction on the data:
the ray matching a constant sample has unbounded log likelihood.
-/

@[expose] public section

noncomputable section

open Real Set Filter MeasureTheory
open scoped Topology

namespace UnivariateML.Beta

variable {n : ℕ}

/-- The positive shape quadrant. -/
def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

/-- The beta normalizer in shape coordinates. -/
def normalizer (p : ℝ × ℝ) : ℝ :=
  Real.Gamma p.1 * Real.Gamma p.2 / Real.Gamma (p.1 + p.2)

/-- The log beta normalizer. -/
def logBeta (p : ℝ × ℝ) : ℝ := log (normalizer p)

private def logKernel (p : ℝ × ℝ) (z : ℝ) : ℝ :=
  (p.1 - 1) * log z + (p.2 - 1) * log (1 - z)

private def kernel (p : ℝ × ℝ) (z : ℝ) : ℝ :=
  exp (logKernel p z)

/-- Total beta log likelihood for observations in the open unit interval. -/
def logLik (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  (p.1 - 1) * (∑ i, log (x i)) + (p.2 - 1) * (∑ i, log (1 - x i)) -
    (n : ℝ) * logBeta p

private theorem normalizer_pos {p : ℝ × ℝ} (hp : p ∈ domain) : 0 < normalizer p :=
  div_pos (mul_pos (Real.Gamma_pos_of_pos hp.1) (Real.Gamma_pos_of_pos hp.2))
    (Real.Gamma_pos_of_pos (add_pos hp.1 hp.2))

private theorem kernel_cast (p : ℝ × ℝ) {z : ℝ} (hz : z ∈ Ioo (0 : ℝ) 1) :
    (kernel p z : ℂ) =
      (z : ℂ) ^ ((p.1 : ℂ) - 1) * (1 - (z : ℂ)) ^ ((p.2 : ℂ) - 1) := by
  have h₁ : 0 < z := hz.1
  have h₂ : 0 < 1 - z := sub_pos.mpr hz.2
  rw [kernel, logKernel, exp_add, mul_comm (p.1 - 1), mul_comm (p.2 - 1),
    ← rpow_def_of_pos h₁, ← rpow_def_of_pos h₂, Complex.ofReal_mul,
    Complex.ofReal_cpow h₁.le, Complex.ofReal_cpow h₂.le]
  push_cast
  ring

private theorem kernel_integrable {p : ℝ × ℝ} (hp : p ∈ domain) :
    IntegrableOn (kernel p) (Ioo 0 1) := by
  have hi := (Complex.betaIntegral_convergent
    (show 0 < (p.1 : ℂ).re from hp.1) (show 0 < (p.2 : ℂ).re from hp.2)).1
  have he : (fun z : ℝ ↦ (kernel p z : ℂ)) =ᵐ[volume.restrict (Ioo 0 1)]
      (fun z : ℝ ↦ (z : ℂ) ^ ((p.1 : ℂ) - 1) * (1 - (z : ℂ)) ^ ((p.2 : ℂ) - 1)) :=
    (ae_restrict_mem measurableSet_Ioo).mono fun _ hz ↦ kernel_cast p hz
  exact (Integrable.iff_ofReal (𝕜 := ℂ)).mpr
    ((hi.mono_set Ioo_subset_Ioc_self).congr he.symm)

private theorem normalizer_eq_integral {p : ℝ × ℝ} (hp : p ∈ domain) :
    normalizer p = ∫ z in Ioo (0 : ℝ) 1, kernel p z := by
  apply Complex.ofReal_injective
  rw [← integral_complex_ofReal, ← integral_Ioc_eq_integral_Ioo,
    ← intervalIntegral.integral_of_le zero_le_one]
  trans Complex.betaIntegral p.1 p.2
  · rw [Complex.betaIntegral_eq_Gamma_mul_div _ _
      (show 0 < (p.1 : ℂ).re from hp.1) (show 0 < (p.2 : ℂ).re from hp.2)]
    rw [← Complex.ofReal_add]
    simp only [normalizer, Complex.ofReal_div, Complex.ofReal_mul, Complex.Gamma_ofReal]
  · exact (intervalIntegral.integral_congr_Ioo_of_le zero_le_one
      (fun _ hz ↦ (kernel_cast p hz).symm))

private theorem logKernel_deriv (p : ℝ × ℝ) {z : ℝ} (hz : z ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt (logKernel p) ((p.1 - 1) / z - (p.2 - 1) / (1 - z)) z := by
  have h := ((Real.hasDerivAt_log (sub_pos.mpr hz.2).ne').comp z
    ((hasDerivAt_const z (1 : ℝ)).sub (hasDerivAt_id z))).const_mul (p.2 - 1)
  convert (((Real.hasDerivAt_log hz.1.ne').const_mul (p.1 - 1)).add h) using 1 <;>
    first | rfl | simp [div_eq_mul_inv, mul_comm, sub_eq_add_neg]

private theorem logKernel_separates {p q : ℝ × ℝ} (hpq : p ≠ q) (A B : ℝ) :
    ∃ z ∈ Ioo (0 : ℝ) 1, logKernel p z - A ≠ logKernel q z - B := by
  by_contra! he
  have h (z : ℝ) (hz : z ∈ Ioo (0 : ℝ) 1) :
      (p.1 - 1) / z - (p.2 - 1) / (1 - z) =
        (q.1 - 1) / z - (q.2 - 1) / (1 - z) := by
    have hev : (fun x ↦ logKernel p x - A) =ᶠ[𝓝 z]
        (fun x ↦ logKernel q x - B) :=
      (show ∀ᶠ x in 𝓝 z, x ∈ Ioo (0 : ℝ) 1 from isOpen_Ioo.mem_nhds hz).mono he
    exact ((logKernel_deriv p hz).sub_const A).unique
      (((logKernel_deriv q hz).sub_const B).congr_of_eventuallyEq hev)
  have h₁ := h (1 / 3) ⟨by norm_num, by norm_num⟩
  have h₂ := h (2 / 3) ⟨by norm_num, by norm_num⟩
  norm_num [div_eq_mul_inv] at h₁ h₂
  exact hpq (Prod.ext (by linarith) (by linarith))

private theorem strict_integral {f g : ℝ → ℝ}
    (hf : IntegrableOn f (Ioo 0 1)) (hg : IntegrableOn g (Ioo 0 1))
    (hfc : ContinuousOn f (Ioo 0 1)) (hgc : ContinuousOn g (Ioo 0 1))
    (hle : ∀ z ∈ Ioo 0 1, f z ≤ g z) (hlt : ∃ z ∈ Ioo 0 1, f z < g z) :
    (∫ z in Ioo (0 : ℝ) 1, f z) < ∫ z in Ioo (0 : ℝ) 1, g z := by
  have hle' : ∀ᵐ z ∂volume.restrict (Ioo 0 1), 0 ≤ g z - f z :=
    (ae_restrict_mem measurableSet_Ioo).mono fun z hz ↦ sub_nonneg.mpr (hle z hz)
  rw [← sub_pos, ← integral_sub hg hf]
  by_contra! h
  have he := (integral_eq_zero_iff_of_nonneg_ae hle' (hg.sub hf)).mp
    (le_antisymm h (integral_nonneg_of_ae hle'))
  have heq : f =ᵐ[volume.restrict (Ioo 0 1)] g :=
    he.mono fun z hz ↦ (sub_eq_zero.mp hz).symm
  obtain ⟨z, hz, hlt⟩ := hlt
  exact hlt.ne (Measure.eqOn_open_of_ae_eq heq isOpen_Ioo hfc hgc hz)

private theorem exp_sub_log_normalizer {p : ℝ × ℝ} (hp : p ∈ domain) (z : ℝ) :
    exp (logKernel p z - logBeta p) = kernel p z / normalizer p := by
  rw [exp_sub, logBeta, exp_log (normalizer_pos hp)]
  rfl

private theorem logKernel_continuous (p : ℝ × ℝ) :
    ContinuousOn (logKernel p) (Ioo 0 1) :=
  fun _ hz ↦ (logKernel_deriv p hz).continuousAt.continuousWithinAt

/-- The log normalizer is jointly strictly convex in the positive shapes. -/
theorem logBeta_strictConvex : StrictConvexOn ℝ domain logBeta := by
  have hd : Convex ℝ domain := (convex_Ioi _).prod (convex_Ioi _)
  refine ⟨hd, ?_⟩
  intro p hp q hq hpq a b ha hb hab
  let r := a • p + b • q
  have hr : r ∈ domain := hd hp hq ha.le hb.le hab
  let C := a * logBeta p + b * logBeta q
  let f := fun z ↦ kernel r z / exp C
  let g := fun z ↦ a * (kernel p z / normalizer p) + b * (kernel q z / normalizer q)
  have hf : IntegrableOn f (Ioo 0 1) := (kernel_integrable hr).div_const _
  have hg : IntegrableOn g (Ioo 0 1) :=
    (((kernel_integrable hp).div_const _).const_mul _).add
      (((kernel_integrable hq).div_const _).const_mul _)
  have kc (s : ℝ × ℝ) : ContinuousOn (kernel s) (Ioo 0 1) :=
    continuous_exp.comp_continuousOn (logKernel_continuous s)
  have hfc : ContinuousOn f (Ioo 0 1) := (kc r).div_const _
  have hgc : ContinuousOn g (Ioo 0 1) :=
    (continuousOn_const.mul ((kc p).div_const _)).add
      (continuousOn_const.mul ((kc q).div_const _))
  have hform (z : ℝ) :
      f z = exp (a * (logKernel p z - logBeta p) + b * (logKernel q z - logBeta q)) := by
    rw [show f z = exp (logKernel r z) / exp C from rfl, ← exp_sub]
    congr 1
    dsimp [r, C, logKernel]
    linear_combination (log z + log (1 - z)) * hab
  have hgform (z : ℝ) :
      g z = a * exp (logKernel p z - logBeta p) + b * exp (logKernel q z - logBeta q) := by
    simp only [g, exp_sub_log_normalizer hp, exp_sub_log_normalizer hq]
  have hle : ∀ z ∈ Ioo 0 1, f z ≤ g z := by
    intro z _
    rw [hform, hgform]
    exact strictConvexOn_exp.convexOn.2 (mem_univ _) (mem_univ _) ha.le hb.le hab
  obtain ⟨z, hz, hne⟩ := logKernel_separates hpq (logBeta p) (logBeta q)
  have hlt : f z < g z := by
    rw [hform, hgform]
    exact strictConvexOn_exp.2 (mem_univ _) (mem_univ _) hne ha hb hab
  have hi := strict_integral hf hg hfc hgc hle ⟨z, hz, hlt⟩
  change (∫ z in Ioo (0 : ℝ) 1, kernel r z / exp C) <
    ∫ z in Ioo (0 : ℝ) 1, a * (kernel p z / normalizer p) +
      b * (kernel q z / normalizer q) at hi
  rw [integral_add (((kernel_integrable hp).div_const _).const_mul _)
    (((kernel_integrable hq).div_const _).const_mul _)] at hi
  simp only [integral_const_mul, integral_div, ← normalizer_eq_integral hp,
    ← normalizer_eq_integral hq, ← normalizer_eq_integral hr,
    div_self (normalizer_pos hp).ne', div_self (normalizer_pos hq).ne', mul_one, hab] at hi
  have hbound : normalizer r < exp C := by
    simpa using (div_lt_iff₀ (exp_pos C)).mp hi
  change logBeta r < C
  exact (log_lt_iff_lt_exp (normalizer_pos hr)).mpr hbound

private theorem logLik_strictConcave (hn : 0 < n) (x : Fin n → ℝ) :
    StrictConcaveOn ℝ domain (logLik x) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  refine ⟨logBeta_strictConvex.1, ?_⟩
  intro p hp q hq hpq a b ha hb hab
  have h := mul_lt_mul_of_pos_left (logBeta_strictConvex.2 hp hq hpq ha hb hab) hn'
  change (n : ℝ) * logBeta (a • p + b • q) <
    n * (a * logBeta p + b * logBeta q) at h
  simp only [logLik, smul_eq_mul, Prod.fst_add, Prod.snd_add, Prod.smul_fst, Prod.smul_snd]
  nlinarith only [h, congrArg (fun v ↦ v * ((∑ i, log (x i)) + ∑ i, log (1 - x i))) hab]

private theorem interval_lower_bound {p : ℝ × ℝ} (hp : p ∈ domain)
    {l r : ℝ} (hl : 0 < l) (hlr : l < r) (hr : r < 1) :
    log (r - l) + p.1 * log l + p.2 * log (1 - r) ≤ logBeta p := by
  let d := exp (p.1 * log l + p.2 * log (1 - r))
  have hsub : Ioo l r ⊆ Ioo (0 : ℝ) 1 := fun _ hz ↦
    ⟨hl.trans hz.1, hz.2.trans hr⟩
  have hmono : (∫ _z in Ioo l r, d) ≤ ∫ z in Ioo l r, kernel p z := by
    apply integral_mono_ae (integrable_const _) ((kernel_integrable hp).mono_set hsub)
    refine (ae_restrict_mem measurableSet_Ioo).mono fun z hz ↦ ?_
    have hsz := hsub hz
    change d ≤ kernel p z
    apply exp_le_exp.mpr
    have H₁ := mul_le_mul_of_nonneg_left (log_le_log hl hz.1.le) hp.1.le
    have H₂ := mul_le_mul_of_nonneg_left
      (log_le_log (sub_pos.mpr hr) (sub_le_sub_left hz.2.le 1)) hp.2.le
    have H₃ := log_nonpos hsz.1.le hsz.2.le
    have H₄ := log_nonpos (sub_nonneg.mpr hsz.2.le) (by linarith [hsz.1])
    change p.1 * log l + p.2 * log (1 - r) ≤ logKernel p z
    unfold logKernel
    linarith
  have hbig := hmono.trans (setIntegral_mono_set (kernel_integrable hp)
    (Eventually.of_forall fun z ↦ (exp_pos _).le)
    (Eventually.of_forall fun z hz ↦ @hsub z hz))
  rw [setIntegral_const, Real.volume_real_Ioo_of_le hlr.le, smul_eq_mul,
    ← normalizer_eq_integral hp] at hbig
  have H := log_le_log (mul_pos (sub_pos.mpr hlr) (exp_pos _)) hbig
  rw [log_mul (sub_pos.mpr hlr).ne' (exp_pos _).ne', log_exp] at H
  unfold logBeta
  linarith

private def shiftedRatio (p : ℝ × ℝ) : ℝ :=
  Real.Gamma (p.1 + 1) * Real.Gamma (p.2 + 1) / Real.Gamma (p.1 + p.2 + 1)

private theorem normalizer_shift {p : ℝ × ℝ} (hp : p ∈ domain) :
    normalizer p = shiftedRatio p * (p.1⁻¹ + p.2⁻¹) := by
  have h₁ : 0 < p.1 := hp.1
  have h₂ : 0 < p.2 := hp.2
  have h₃ := add_pos h₁ h₂
  unfold normalizer shiftedRatio
  rw [Real.Gamma_add_one h₁.ne', Real.Gamma_add_one h₂.ne', Real.Gamma_add_one h₃.ne']
  field_simp [h₁.ne', h₂.ne', h₃.ne', (Real.Gamma_pos_of_pos h₃).ne']
  ring

private theorem shiftedRatio_lower {M : ℝ} (hM : 0 < M) :
    ∃ m > 0, ∀ p ∈ Icc (0 : ℝ) M ×ˢ Icc (0 : ℝ) M, m ≤ shiftedRatio p := by
  let S := Icc (0 : ℝ) M ×ˢ Icc (0 : ℝ) M
  have hpos (p : ℝ × ℝ) (hp : p ∈ S) :
      0 < p.1 + 1 ∧ 0 < p.2 + 1 ∧ 0 < p.1 + p.2 + 1 := by
    have h₁ := hp.1.1
    have h₂ := hp.2.1
    exact ⟨by linarith, by linarith, by linarith⟩
  have hc : ContinuousOn shiftedRatio S := by
    have hG : ContinuousOn Real.Gamma (Ioi 0) := Real.differentiableOn_Gamma_Ioi.continuousOn
    exact ((hG.comp (by fun_prop) (fun p hp ↦ (hpos p hp).1)).mul
      (hG.comp (by fun_prop) (fun p hp ↦ (hpos p hp).2.1))).div
      (hG.comp (by fun_prop) (fun p hp ↦ (hpos p hp).2.2))
      (fun p hp ↦ (Real.Gamma_pos_of_pos (hpos p hp).2.2).ne')
  have hS : S.Nonempty := ⟨(0, 0), ⟨⟨le_rfl, hM.le⟩, ⟨le_rfl, hM.le⟩⟩⟩
  obtain ⟨p, hp, hm⟩ := (isCompact_Icc.prod isCompact_Icc : IsCompact S).exists_isMinOn hS hc
  exact ⟨shiftedRatio p, div_pos
    (mul_pos (Real.Gamma_pos_of_pos (hpos p hp).1) (Real.Gamma_pos_of_pos (hpos p hp).2.1))
    (Real.Gamma_pos_of_pos (hpos p hp).2.2), fun _ hq ↦ hm hq⟩

private def objective (u v : ℝ) (p : ℝ × ℝ) : ℝ :=
  p.1 * u + p.2 * v - logBeta p

private theorem objective_level_box {u v : ℝ} (huv : exp u + exp v < 1) (L : ℝ) :
    ∃ δ > 0, ∃ M : ℝ, ∀ p ∈ domain, L ≤ objective u v p →
      p ∈ Icc δ M ×ˢ Icc δ M := by
  obtain ⟨l, hul, hl⟩ := exists_between (show exp u < 1 - exp v by linarith)
  obtain ⟨r, hlr, hvr⟩ := exists_between hl
  have hl₀ : 0 < l := (exp_pos _).trans hul
  have hr₁ : r < 1 := by linarith [exp_pos v]
  have h₁ : 0 < log l - u := sub_pos.mpr ((lt_log_iff_exp_lt hl₀).mpr hul)
  have h₂ : 0 < log (1 - r) - v :=
    sub_pos.mpr ((lt_log_iff_exp_lt (sub_pos.mpr hr₁)).mpr (by linarith))
  let K := -log (r - l) - L
  let M := max 1 (max (K / (log l - u)) (K / (log (1 - r) - v)))
  have hM : 0 < M := lt_of_lt_of_le zero_lt_one (le_max_left _ _)
  obtain ⟨m, hm, hmin⟩ := shiftedRatio_lower hM
  let δ := m / exp (M * (|u| + |v|) - L)
  refine ⟨δ, div_pos hm (exp_pos _), M, ?_⟩
  intro p hp hL
  have hp₁ : 0 < p.1 := hp.1
  have hp₂ : 0 < p.2 := hp.2
  have htail : p.1 * (log l - u) + p.2 * (log (1 - r) - v) ≤ K := by
    have h := interval_lower_bound hp hl₀ hlr hr₁
    unfold objective at hL
    dsimp [K]
    linarith
  have H₁ : p.1 ≤ M := by
    apply le_trans ((le_div_iff₀ h₁).mpr ?_) ((le_max_left _ _).trans (le_max_right _ _))
    linarith [mul_pos hp₂ h₂]
  have H₂ : p.2 ≤ M := by
    apply le_trans ((le_div_iff₀ h₂).mpr ?_) ((le_max_right _ _).trans (le_max_right _ _))
    linarith [mul_pos hp₁ h₁]
  have hB : normalizer p ≤ exp (M * (|u| + |v|) - L) := by
    apply (log_le_iff_le_exp (normalizer_pos hp)).mp
    have hu' : p.1 * u ≤ M * |u| :=
      (mul_le_mul_of_nonneg_left (le_abs_self u) hp₁.le).trans
        (mul_le_mul_of_nonneg_right H₁ (abs_nonneg _))
    have hv' : p.2 * v ≤ M * |v| :=
      (mul_le_mul_of_nonneg_left (le_abs_self v) hp₂.le).trans
        (mul_le_mul_of_nonneg_right H₂ (abs_nonneg _))
    unfold objective logBeta at hL
    linarith
  have hmp := hmin p ⟨⟨hp₁.le, H₁⟩, ⟨hp₂.le, H₂⟩⟩
  have hshift := normalizer_shift hp
  have lower (s t : ℝ) (hs : 0 < s) (ht : 0 < t)
      (he : normalizer p = shiftedRatio p * (s⁻¹ + t⁻¹)) : δ ≤ s := by
    have hs' : m * s⁻¹ ≤ normalizer p := calc
      _ ≤ shiftedRatio p * s⁻¹ := mul_le_mul_of_nonneg_right hmp (inv_nonneg.mpr hs.le)
      _ ≤ _ := by rw [he, mul_add]; linarith [mul_pos (hm.trans_le hmp) (inv_pos.mpr ht)]
    apply (div_le_iff₀ (exp_pos _)).mpr
    have h := (div_le_iff₀ hs).mp (show m / s ≤ _ from
      (div_eq_mul_inv m s).symm ▸ hs'.trans hB)
    simpa [mul_comm] using h
  exact ⟨⟨lower _ _ hp₁ hp₂ hshift, H₁⟩,
    ⟨lower _ _ hp₂ hp₁ (by rw [hshift, add_comm p.1⁻¹ p.2⁻¹]), H₂⟩⟩

private theorem sample_log_means {x : Fin n → ℝ} (hn : 0 < n)
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) (hne : ∃ i j, x i ≠ x j) :
    exp (Gamma.avgLog x) + exp (Gamma.avgLog (fun i ↦ 1 - x i)) < 1 := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have h (y : Fin n → ℝ) (hy : ∀ i, 0 < y i) (hne' : ∃ i j, y i ≠ y j) :
      exp (Gamma.avgLog y) < Gamma.avg y := by
    have hpos : 0 < Gamma.avg y := by
      apply div_pos _ hn'
      obtain ⟨i, _, _⟩ := hne'
      exact Finset.sum_pos' (fun j _ ↦ (hy j).le) ⟨i, Finset.mem_univ _, hy i⟩
    have hJ := Gamma.jensenGap_spec hn hy
    have hgap : 0 < Gamma.jensenGap y := lt_of_le_of_ne hJ.1 (Ne.symm (fun he ↦ by
      obtain ⟨i, j, hij⟩ := hne'
      exact hij (hJ.2.mp he i j)))
    unfold Gamma.jensenGap at hgap
    exact (lt_log_iff_exp_lt hpos).mp (sub_pos.mp hgap)
  have hsum : Gamma.avg x + Gamma.avg (fun i ↦ 1 - x i) = 1 := by
    rw [Gamma.avg, Gamma.avg, ← add_div, ← Finset.sum_add_distrib]
    simp [hn'.ne']
  have h₁ := h x (fun i ↦ (hx i).1) hne
  have h₂ := h (fun i ↦ 1 - x i) (fun i ↦ sub_pos.mpr (hx i).2) (by
    obtain ⟨i, j, hij⟩ := hne
    exact ⟨i, j, fun he ↦ hij (by linarith)⟩)
  linarith

private theorem logLik_objective {x : Fin n → ℝ} (hn : 0 < n) (p : ℝ × ℝ) :
    logLik x p = (n : ℝ) * (objective (Gamma.avgLog x)
      (Gamma.avgLog (fun i ↦ 1 - x i)) p -
      (Gamma.avgLog x + Gamma.avgLog (fun i ↦ 1 - x i))) := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  unfold logLik objective Gamma.avgLog Gamma.avg
  field_simp
  ring

/-- The log likelihood tends to minus infinity on escape from every compact
subset of the positive shape quadrant. -/
theorem logLik_coercive (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) (hne : ∃ i j, x i ≠ x j) :
    Tendsto (fun p : domain ↦ logLik x p) (cocompact domain) atBot := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  apply tendsto_atBot.mpr
  intro L
  obtain ⟨δ, hδ, M, hbox⟩ := objective_level_box (sample_log_means hn hx hne)
    (L / n + Gamma.avgLog x + Gamma.avgLog (fun i ↦ 1 - x i))
  let K := Icc δ M ×ˢ Icc δ M
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  have hs : K ⊆ domain := fun _ hp ↦ ⟨hδ.trans_le hp.1.1, hδ.trans_le hp.2.1⟩
  refine mem_cocompact.mpr ⟨Subtype.val ⁻¹' K,
    Topology.IsEmbedding.subtypeVal.isInducing.isCompact_preimage'
      hK (by simpa using hs), ?_⟩
  intro p hp
  change logLik x p ≤ L
  by_contra! he
  apply hp
  apply hbox _ p.property
  rw [logLik_objective hn] at he
  have h := (div_lt_iff₀ hn').mpr (by simpa only [mul_comm (n : ℝ)] using he)
  linarith

/-- Every nonconstant interior sample has exactly one positive shape pair
maximizing its beta log likelihood. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p :=
  Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    ⟨(1, 1), ⟨zero_lt_one, zero_lt_one⟩⟩ (isOpen_Ioi.prod isOpen_Ioi)
    (logLik_strictConcave hn x) (logLik_coercive hn hx hne)

private theorem meanLog_complement_neg {x : Fin n → ℝ} (hn : 0 < n)
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) :
    Gamma.avgLog (fun i ↦ 1 - x i) < 0 := by
  have : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  unfold Gamma.avgLog Gamma.avg
  exact div_neg_of_neg_of_pos
    (Finset.sum_neg (fun i _ ↦ log_neg (sub_pos.mpr (hx i).2) (by linarith [(hx i).1]))
      Finset.univ_nonempty)
    (by exact_mod_cast hn)

/-- Holding the first shape positive leaves a unique positive maximizer
in the second shape, including for constant samples. -/
theorem heldFirst (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) {h : ℝ} (hh : 0 < h) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun z ↦ logLik x (h, z)) (Ioi 0) b := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let u := Gamma.avgLog x
  let v := Gamma.avgLog (fun i ↦ 1 - x i)
  have hv : exp v < 1 := exp_lt_one_iff.mpr (meanLog_complement_neg hn hx)
  let w := log ((1 - exp v) / 2)
  have hw : exp w + exp v < 1 := by
    rw [show exp w = (1 - exp v) / 2 from exp_log (by positivity)]
    linarith
  have hconc : StrictConcaveOn ℝ (Ioi (0 : ℝ)) (fun z ↦ logLik x (h, z)) := by
    refine ⟨convex_Ioi _, ?_⟩
    intro p hp q hq hpq a b ha hb hab
    have H := (logLik_strictConcave hn x).2
      (show (h, p) ∈ domain from ⟨hh, hp⟩)
      (show (h, q) ∈ domain from ⟨hh, hq⟩)
      (fun he ↦ hpq (congrArg Prod.snd he)) ha hb hab
    have he : a • (h, p) + b • (h, q) = (h, a • p + b • q) := by
      apply Prod.ext
      · change a * h + b * h = h
        linear_combination h * hab
      · rfl
    rwa [he] at H
  apply Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    ⟨1, zero_lt_one⟩ isOpen_Ioi hconc
  apply tendsto_atBot.mpr
  intro L
  obtain ⟨δ, hδ, M, hbox⟩ := objective_level_box hw (L / n + u + v - h * (u - w))
  have hs : Icc δ M ⊆ Ioi (0 : ℝ) := fun _ hz ↦ hδ.trans_le hz.1
  refine mem_cocompact.mpr ⟨Subtype.val ⁻¹' Icc δ M,
    Topology.IsEmbedding.subtypeVal.isInducing.isCompact_preimage'
      isCompact_Icc (by rwa [Subtype.range_coe]), ?_⟩
  intro z hz
  change logLik x (h, z) ≤ L
  by_contra! he
  apply hz
  refine (hbox (h, z) ⟨hh, z.property⟩ ?_).2
  rw [logLik_objective hn] at he
  have H := (div_lt_iff₀ hn').mpr (by simpa only [mul_comm (n : ℝ)] using he)
  change L / n < objective u v (h, ↑z) - (u + v) at H
  dsimp [objective] at H ⊢
  linarith

private theorem logLik_swap (x : Fin n → ℝ) (a b : ℝ) :
    logLik (fun i ↦ 1 - x i) (b, a) = logLik x (a, b) := by
  have he (i : Fin n) : 1 - (1 - x i) = x i := by ring
  simp only [logLik, he, logBeta, normalizer]
  rw [mul_comm (Real.Gamma b), add_comm b a, add_comm ((b - 1) * _)]

/-- Holding the second shape positive leaves a unique positive maximizer
in the first shape, including for constant samples. -/
theorem heldSecond (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) {h : ℝ} (hh : 0 < h) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun z ↦ logLik x (z, h)) (Ioi 0) a := by
  have hc (i : Fin n) : 1 - x i ∈ Ioo (0 : ℝ) 1 :=
    ⟨sub_pos.mpr (hx i).2, by linarith [(hx i).1]⟩
  simpa only [logLik_swap] using heldFirst hn hc hh

private def ray (c t : ℝ) : ℝ × ℝ := (t * c, t * (1 - c))

private theorem ray_mem {c t : ℝ} (hc : c ∈ Ioo (0 : ℝ) 1) (ht : 0 < t) :
    ray c t ∈ domain :=
  ⟨mul_pos ht hc.1, mul_pos ht (sub_pos.mpr hc.2)⟩

private def rayGap (c z : ℝ) : ℝ :=
  c * (log z - log c) + (1 - c) * (log (1 - z) - log (1 - c))

private theorem rayGap_neg {c z : ℝ} (hc : c ∈ Ioo (0 : ℝ) 1)
    (hz : z ∈ Ioo (0 : ℝ) 1) (hne : z ≠ c) : rayGap c z < 0 := by
  have hc' := sub_pos.mpr hc.2
  have h₁ := div_pos hz.1 hc.1
  have h₂ := div_pos (sub_pos.mpr hz.2) hc'
  have hne' : z / c ≠ (1 - z) / (1 - c) := by
    intro he
    apply hne
    have hh := (div_eq_div_iff hc.1.ne' hc'.ne').mp he
    nlinarith
  have h := strictConcaveOn_log_Ioi.2 h₁ h₂ hne' hc.1 hc' (by ring)
  change c * log (z / c) + (1 - c) * log ((1 - z) / (1 - c)) <
    log (c * (z / c) + (1 - c) * ((1 - z) / (1 - c))) at h
  rw [show c * (z / c) + (1 - c) * ((1 - z) / (1 - c)) = 1 by
    field_simp [hc.1.ne', hc'.ne']; ring, log_one,
    log_div hz.1.ne' hc.1.ne', log_div (sub_pos.mpr hz.2).ne' hc'.ne'] at h
  exact h

private theorem ray_ratio_formula (c t z : ℝ) :
    kernel (ray c t) z / kernel (ray c t) c =
      exp ((t - 1) * rayGap c z) * (kernel (ray c 1) z / kernel (ray c 1) c) := by
  simp only [kernel, ← exp_sub, ← exp_add, logKernel, ray, rayGap]
  congr 1
  ring

private theorem ray_normalizer_tendsto {c : ℝ} (hc : c ∈ Ioo (0 : ℝ) 1) :
    Tendsto (fun t ↦ normalizer (ray c t) / kernel (ray c t) c) atTop (𝓝 0) := by
  let F := fun t z ↦ kernel (ray c t) z / kernel (ray c t) c
  have hfi {t : ℝ} (ht : 0 < t) : IntegrableOn (F t) (Ioo 0 1) :=
    (kernel_integrable (ray_mem hc ht)).div_const _
  have h : Tendsto (fun t ↦ ∫ z in Ioo (0 : ℝ) 1, F t z) atTop
      (𝓝 (∫ _z in Ioo (0 : ℝ) 1, (0 : ℝ))) := by
    apply tendsto_integral_filter_of_dominated_convergence (F 1)
    · exact (eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ (hfi ht).aestronglyMeasurable
    · filter_upwards [eventually_ge_atTop (1 : ℝ)] with t ht
      refine (ae_restrict_mem measurableSet_Ioo).mono fun z hz ↦ ?_
      have hg : rayGap c z ≤ 0 := by
        by_cases he : z = c
        · subst z
          simp [rayGap]
        · exact (rayGap_neg hc hz he).le
      change ‖kernel (ray c t) z / kernel (ray c t) c‖ ≤ F 1 z
      have hpos : 0 < kernel (ray c t) z / kernel (ray c t) c :=
        div_pos (exp_pos _) (exp_pos _)
      rw [Real.norm_eq_abs, abs_of_pos hpos, ray_ratio_formula]
      apply mul_le_of_le_one_left (div_nonneg (exp_pos _).le (exp_pos _).le)
      exact exp_le_one_iff.mpr (mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr ht) hg)
    · exact hfi one_pos
    · have he : ∀ᵐ z : ℝ ∂volume.restrict (Ioo 0 1), z ≠ c :=
        ae_restrict_of_ae (Measure.ae_ne volume c)
      filter_upwards [ae_restrict_mem (μ := volume) (measurableSet_Ioo (a := (0 : ℝ)) (b := 1)), he]
        with z hz hne
      have ht : Tendsto (fun t : ℝ ↦ t - 1) atTop atTop := by
        refine tendsto_atTop_atTop.mpr fun A ↦ ⟨A + 1, fun t ht ↦ ?_⟩
        linarith
      have ht := ht.atTop_mul_const_of_neg (rayGap_neg hc hz hne)
      have H := (tendsto_exp_atBot.comp ht).mul_const (F 1 z)
      simpa only [Function.comp_apply, F, ← ray_ratio_formula, zero_mul] using H
  simp only [integral_zero] at h
  refine h.congr' ((eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦ ?_)
  simp only [F, integral_div, ← normalizer_eq_integral (ray_mem hc ht)]

/-- Along the fixed-mean ray of a constant sample the total log likelihood
tends to positive infinity. -/
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : c ∈ Ioo (0 : ℝ) 1) :
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ c) (t * c, t * (1 - c))) atTop atTop := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hp : ∀ᶠ t : ℝ in atTop, 0 < normalizer (ray c t) / kernel (ray c t) c :=
    (eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦
      div_pos (normalizer_pos (ray_mem hc ht)) (exp_pos _)
  have ht := tendsto_log_nhdsGT_zero.comp
    (tendsto_nhdsWithin_iff.mpr ⟨ray_normalizer_tendsto hc, hp⟩)
  have h := ht.const_mul_atBot_of_neg (neg_neg_of_pos hn')
  refine h.congr' ((eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦ ?_)
  change -(n : ℝ) * log (normalizer (ray c t) / kernel (ray c t) c) =
    logLik (fun _ : Fin n ↦ c) (ray c t)
  have hk : 0 < kernel (ray c t) c := exp_pos _
  rw [log_div (normalizer_pos (ray_mem hc ht)).ne' hk.ne']
  simp only [kernel, logKernel, log_exp, logLik, logBeta, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  ring

/-- A constant interior sample has no maximizing positive shape pair. -/
theorem constant_noMLE (hn : 0 < n) {c : ℝ} (hc : c ∈ Ioo (0 : ℝ) 1) :
    ¬ ∃ p ∈ domain, IsMaxOn (logLik (fun _ : Fin n ↦ c)) domain p := by
  rintro ⟨p, _, hp⟩
  obtain ⟨t, ht, hlt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    ((constant_unbounded hn hc).eventually
      (eventually_gt_atTop (logLik (fun _ : Fin n ↦ c) p)))).exists
  exact (not_lt_of_ge (hp (ray_mem hc ht))) hlt

/-- A nonempty interior sample has a finite joint MLE exactly when two
observations differ, and the MLE is unique whenever it exists. -/
theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) :
    (∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p) ↔ ∃ i j, x i ≠ x j := by
  refine ⟨?_, jointMLE hn hx⟩
  intro h
  by_contra! he
  let i : Fin n := ⟨0, hn⟩
  have hx' : x = fun _ ↦ x i := funext fun j ↦ he j i
  rw [hx'] at h
  obtain ⟨p, ⟨hp, hm⟩, _⟩ := h
  exact constant_noMLE hn (hx i) ⟨p, hp, hm⟩

end UnivariateML.Beta
