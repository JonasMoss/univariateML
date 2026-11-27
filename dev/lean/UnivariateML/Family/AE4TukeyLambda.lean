module

public import UnivariateML.Family.LocScaleTukeyLambda
public import Mathlib.Analysis.Analytic.Order
public import Mathlib.Analysis.Calculus.InverseFunctionTheorem.Analytic
public import Mathlib.Analysis.Calculus.InverseFunctionTheorem.Deriv
public import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
public import Mathlib.Analysis.SpecialFunctions.Pow.Deriv

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Interior Tukey lambda quantile charts

## Deviations

The scalar inverse is local to the interior with a fixed nonzero shape.
The joint quantile continuation includes shape zero. The support-contact
charts and competing score comparisons are separate.
-/

@[expose] public section

open Set Filter Real
open scoped Topology
open UnivariateML.LocScaleTukeyLambda

namespace UnivariateML.AE4TukeyLambda

private theorem analytic_rpow_at_pos {x : ℝ} (hx : 0 < x) (a : ℝ) :
    AnalyticAt ℝ (fun p : ℝ ↦ p^a) x := by
  have h := ((analyticAt_log hx).mul (analyticAt_const (v := a))).rexp
  apply h.congr
  filter_upwards [Ioi_mem_nhds hx] with p hp
  rw [rpow_def_of_pos hp]
  rfl

/-- At a nonzero held shape, the Tukey lambda quantile is analytic in
interior probabilities. -/
theorem quantile_analytic {lam p : ℝ} (hl : lam ≠ 0) (hp : p ∈ Ioo (0 : ℝ) 1) :
    AnalyticAt ℝ (quantile lam) p := by
  have h : AnalyticAt ℝ (fun q : ℝ ↦ 1-q) p := analyticAt_const.sub analyticAt_id
  have h₁ := analytic_rpow_at_pos hp.1 lam
  have h₂ := AnalyticAt.comp (f := fun q : ℝ ↦ 1-q)
    (analytic_rpow_at_pos (sub_pos.mpr hp.2) lam) h
  exact (h₁.sub h₂).div analyticAt_const hl

private theorem quantile_derivative {lam p : ℝ} (hl : lam ≠ 0)
    (hp : p ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt (quantile lam) (p^(lam-1) + (1-p)^(lam-1)) p := by
  have h₁ := hasDerivAt_rpow_const (x := p) (p := lam) (Or.inl hp.1.ne')
  have h₂ := (hasDerivAt_rpow_const (x := 1-p) (p := lam)
    (Or.inl (sub_pos.mpr hp.2).ne')).comp p ((hasDerivAt_id p).const_sub 1)
  convert (h₁.sub h₂).div_const lam using 1
  · rfl
  · field_simp [hl]
    ring

/-- The standardized Tukey lambda CDF has a local analytic inverse-quantile
chart at every interior probability and every fixed nonzero shape. -/
theorem exists_analytic_inverse {lam p : ℝ} (hl : lam ≠ 0) (hp : p ∈ Ioo (0 : ℝ) 1) :
    ∃ G : ℝ → ℝ, AnalyticAt ℝ G (quantile lam p) ∧ G (quantile lam p) = p ∧
      (∀ᶠ z in 𝓝 (quantile lam p), quantile lam (G z) = z) := by
  have ha := quantile_analytic hl hp
  have hn : deriv (quantile lam) p ≠ 0 := by
    rw [(quantile_derivative hl hp).deriv]
    exact (add_pos (rpow_pos_of_pos hp.1 _) (rpow_pos_of_pos (sub_pos.mpr hp.2) _)).ne'
  exact ⟨ha.hasStrictDerivAt.localInverse _ _ _ hn,
    ha.analyticAt_localInverse hn, mem_of_mem_nhds (ha.hasStrictDerivAt.eventually_left_inverse hn),
    HasStrictDerivAt.eventually_right_inverse ..⟩

private noncomputable def expDiv (t : ℝ) : ℝ :=
  if t = 0 then 1 else (exp t - 1) / t

private theorem expDiv_analytic (t : ℝ) : AnalyticAt ℝ expDiv t := by
  by_cases ht : t = 0
  · subst t
    obtain ⟨F, hF, hEq⟩ := (analyticAt_rexp (x := 0)).exists_eventuallyEq_sum_add_pow_mul 2
    have hEq' : ∀ᶠ z : ℝ in 𝓝 0, exp z = 1+z+z^2*F z := by
      simpa [Finset.sum_range_succ, iteratedDeriv_succ, smul_eq_mul, Real.deriv_exp] using hEq
    have h : AnalyticAt ℝ (fun z ↦ 1+z*F z) 0 :=
      analyticAt_const.add (analyticAt_id.mul hF)
    apply h.congr
    filter_upwards [hEq'] with z hz
    by_cases hz₀ : z = 0
    · simp [expDiv, hz₀]
    · rw [expDiv, ite_eq_right hz₀, hz]
      field_simp [hz₀]
      ring
  · have h : AnalyticAt ℝ (fun z ↦ (exp z - 1)/z) t :=
      (analyticAt_rexp.sub analyticAt_const).div analyticAt_id ht
    apply h.congr
    filter_upwards [(isClosed_singleton (X := ℝ) (x := 0)).isOpen_compl.mem_nhds
      (show t ∈ ({0} : Set ℝ)ᶜ from ht)] with z hz
    exact (ite_eq_right (show z ≠ 0 from hz)).symm

/-- The Tukey lambda quantile with the logarithmic value at shape zero. -/
noncomputable def quantileExtended (lam p : ℝ) : ℝ :=
  if lam = 0 then log p - log (1-p) else quantile lam p

private theorem quantileExtended_eq (lam p : ℝ) (hp : p ∈ Ioo (0 : ℝ) 1) :
    quantileExtended lam p =
      log p * expDiv (lam * log p) - log (1-p) * expDiv (lam * log (1-p)) := by
  by_cases h : lam = 0
  · simp [h, quantileExtended, expDiv]
  · have hx (t : ℝ) :
        t*expDiv (lam*t) = (exp (lam*t)-1)/lam := by
      by_cases ht : t = 0
      · simp [ht]
      · rw [expDiv, ite_eq_right (mul_ne_zero h ht)]
        field_simp [h,ht]
    rw [quantileExtended, ite_eq_right h, hx, hx, quantile,
      rpow_def_of_pos hp.1, rpow_def_of_pos (sub_pos.mpr hp.2)]
    rw [mul_comm lam (log p), mul_comm lam (log (1-p))]
    ring

/-- The shape-zero continuation of the Tukey lambda quantile is jointly
real analytic in shape and interior probability. -/
theorem quantileExtended_analytic (lam p : ℝ) (hp : p ∈ Ioo (0 : ℝ) 1) :
    AnalyticAt ℝ (fun z : ℝ × ℝ ↦ quantileExtended z.1 z.2) (lam,p) := by
  have hp' : 0 < 1-p := sub_pos.mpr hp.2
  have hl : AnalyticAt ℝ (fun z : ℝ × ℝ ↦ log z.2) (lam,p) :=
    AnalyticAt.comp (analyticAt_log hp.1) analyticAt_snd
  have hr : AnalyticAt ℝ (fun z : ℝ × ℝ ↦ log (1-z.2)) (lam,p) :=
    AnalyticAt.comp (f := fun z : ℝ × ℝ ↦ 1-z.2) (x := (lam,p))
      (analyticAt_log hp') (analyticAt_const.sub analyticAt_snd)
  have he₁ := AnalyticAt.comp (f := fun z : ℝ × ℝ ↦ z.1*log z.2) (x := (lam,p))
    (expDiv_analytic (lam*log p)) (analyticAt_fst.mul hl)
  have he₂ := AnalyticAt.comp (f := fun z : ℝ × ℝ ↦ z.1*log (1-z.2)) (x := (lam,p))
    (expDiv_analytic (lam*log (1-p))) (analyticAt_fst.mul hr)
  apply ((hl.mul he₁).sub (hr.mul he₂)).congr
  have h : ∀ᶠ z : ℝ × ℝ in 𝓝 (lam,p), z.2 ∈ Ioo (0 : ℝ) 1 :=
    continuous_snd.continuousAt (Ioo_mem_nhds hp.1 hp.2)
  filter_upwards [h] with z hz
  exact (quantileExtended_eq z.1 z.2 hz).symm

end UnivariateML.AE4TukeyLambda

end
