module

public import UnivariateML.Generic.AE1Incidence
public import Mathlib.Analysis.Analytic.Order
import Mathlib.Tactic

set_option linter.style.header false

/-! Null projection of competing scores, stratified by the orders of their zeros. -/

@[expose] public section

open Set Filter MeasureTheory ContinuousLinearMap
open scoped ContDiff Topology

namespace UnivariateML.AE3AnalyticIncidence

private theorem partialDerivative_analytic {E : Type*} [NormedAddCommGroup E]
    [NormedSpace ℝ E] [CompleteSpace E] {f : E × ℝ → ℝ} {z : E × ℝ}
    (hf : AnalyticAt ℝ f z) :
    AnalyticAt ℝ (fun y : E × ℝ ↦ deriv (fun t ↦ f (y.1, t)) y.2) z := by
  have he : (fun y : E × ℝ ↦ deriv (fun t ↦ f (y.1, t)) y.2) =ᶠ[𝓝 z]
      (fun y ↦ fderiv ℝ f y (0, 1)) := by
    filter_upwards [hf.eventually_analyticAt] with y hy
    have hh : HasDerivAt (fun t : ℝ ↦ (y.1, t)) (0, 1) y.2 :=
      (hasDerivAt_const y.2 y.1).prodMk (hasDerivAt_id y.2)
    exact (hy.differentiableAt.hasFDerivAt.comp_hasDerivAt y.2 hh).deriv
  have hEval := ((ContinuousLinearMap.apply ℝ ℝ ((0, 1) : E × ℝ)).analyticAt
    (x := fderiv ℝ f z)).comp hf.fderiv
  exact hEval.congr he.symm

private theorem iteratedPartial_analytic {E : Type*} [NormedAddCommGroup E]
    [NormedSpace ℝ E] [CompleteSpace E] {f : E × ℝ → ℝ} {z : E × ℝ}
    (hf : AnalyticAt ℝ f z) (k : ℕ) :
    AnalyticAt ℝ (fun y : E × ℝ ↦ iteratedDeriv k (fun t ↦ f (y.1, t)) y.2) z := by
  induction k with
  | zero => simpa only [iteratedDeriv_zero, Prod.mk.eta] using hf
  | succ k ih =>
    simpa only [iteratedDeriv_succ] using partialDerivative_analytic ih

/-- If every coordinate is a zero of finite positive order of a jointly analytic
score difference, the sample projection is null when the pair chart has lower dimension. -/
theorem ae_no_analytic_incidence {d n : ℕ} (hdn : d < n)
    (f : (Fin d → ℝ) × ℝ → ℝ) (S : Set (((Fin d → ℝ) × (Fin n → ℝ))))
    (ha : ∀ z ∈ S, ∀ i, AnalyticAt ℝ f (z.1, z.2 i))
    (ho : ∀ z ∈ S, ∀ i, ∃ k : ℕ,
      iteratedDeriv k (fun t ↦ f (z.1, t)) (z.2 i) = 0 ∧
      iteratedDeriv (k + 1) (fun t ↦ f (z.1, t)) (z.2 i) ≠ 0) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p, (p, x) ∉ S := by
  let F (k : Fin n → ℕ) (z : (Fin d → ℝ) × (Fin n → ℝ)) (i : Fin n) :=
    iteratedDeriv (k i) (fun t ↦ f (z.1, t)) (z.2 i)
  let D (k : Fin n → ℕ) (z : (Fin d → ℝ) × (Fin n → ℝ)) (i : Fin n) :=
    iteratedDeriv (k i + 1) (fun t ↦ f (z.1, t)) (z.2 i)
  let strata (k : Fin n → ℕ) := {z ∈ S | (∀ i, F k z i = 0) ∧ ∀ i, D k z i ≠ 0}
  have hF (k : Fin n → ℕ) (z) (hz : z ∈ strata k) : ContDiffAt ℝ 1 (F k) z := by
    apply contDiffAt_pi.mpr
    intro i
    exact (iteratedPartial_analytic (ha z hz.1 i) (k i)).contDiffAt.comp z
      (f := fun y : (Fin d → ℝ) × (Fin n → ℝ) ↦ (y.1, y.2 i)) (by fun_prop)
  have hae := AE1Incidence.ae_no_implicit_incidence hdn strata F
    (fun _ _ hz ↦ funext hz.2.1) hF ?_
  · filter_upwards [hae] with x hx p hp
    choose k hk using ho (p, x) hp
    exact hx k p ⟨hp, fun i ↦ (hk i).1, fun i ↦ (hk i).2⟩
  · intro k z hz
    have hh (i : Fin n) : HasDerivAt
        (iteratedDeriv (k i) (fun t ↦ f (z.1, t))) (D k z i) (z.2 i) := by
      have h := (iteratedPartial_analytic (ha z hz.1 i) (k i)).comp
        (f := fun t : ℝ ↦ (z.1, t)) (by fun_prop :
          AnalyticAt ℝ (fun t : ℝ ↦ (z.1, t)) (z.2 i))
      have hdiff : DifferentiableAt ℝ (iteratedDeriv (k i) (fun t ↦ f (z.1, t))) (z.2 i) :=
        h.differentiableAt
      simp only [D, iteratedDeriv_succ]
      exact hdiff.hasDerivAt
    let A := ContinuousLinearMap.pi (fun i ↦ D k z i •
      (proj i : (Fin n → ℝ) →L[ℝ] ℝ))
    have hder : HasFDerivAt (fun x ↦ F k (z.1, x)) A z.2 := by
      apply hasFDerivAt_pi.mpr
      intro i
      exact (hh i).comp_hasFDerivAt z.2 (proj i : (Fin n → ℝ) →L[ℝ] ℝ).hasFDerivAt
    have hinc : HasFDerivAt (fun x : Fin n → ℝ ↦ (z.1, x))
        (inr ℝ (Fin d → ℝ) (Fin n → ℝ)) z.2 :=
      ((hasFDerivAt_const z.1 z.2).prodMk (hasFDerivAt_id z.2))
    rw [← ((hF k z hz).differentiableAt_one.hasFDerivAt.comp z.2 hinc).fderiv]
    change (fderiv ℝ (fun x ↦ F k (z.1, x)) z.2).IsInvertible
    rw [hder.fderiv]
    refine ⟨ContinuousLinearEquiv.ofBijective A ?_ ?_, ContinuousLinearEquiv.coe_ofBijective ..⟩
    · apply LinearMap.ker_eq_bot.mpr
      intro x y h
      funext i
      exact mul_left_cancel₀ (hz.2.2 i) (congrFun h i)
    · apply LinearMap.range_eq_top.mpr
      intro y
      refine ⟨fun i ↦ y i / D k z i, ?_⟩
      funext i
      change D k z i * (y i / D k z i) = y i
      field_simp [hz.2.2 i]

/-- A zero of an analytic function that is nonzero somewhere on its connected
domain has a vanishing derivative immediately followed by a nonvanishing one. -/
theorem exists_derivative_before_nonzero {f : ℝ → ℝ} {U : Set ℝ} {x : ℝ}
    (ha : AnalyticOnNhd ℝ f U) (hU : IsPreconnected U) (hx : x ∈ U)
    (hz : f x = 0) (hne : ∃ y ∈ U, f y ≠ 0) :
    ∃ k : ℕ, iteratedDeriv k f x = 0 ∧ iteratedDeriv (k + 1) f x ≠ 0 := by
  have htop : analyticOrderAt f x ≠ ⊤ := by
    intro h
    obtain ⟨y, hy, hne⟩ := hne
    exact hne (ha.eqOn_zero_of_preconnected_of_eventuallyEq_zero hU hx
      (analyticOrderAt_eq_top.mp h) hy)
  obtain ⟨j, hj⟩ := ENat.ne_top_iff_exists.mp htop
  have horder := (analyticOrderAt_eq_nat_iff_iteratedDeriv_eq_zero (ha x hx)).mp hj.symm
  cases j with
  | zero => exact (horder.2 (by simpa only [iteratedDeriv_zero] using hz)).elim
  | succ k => exact ⟨k, horder.1 k (Nat.lt_succ_self k), horder.2⟩

/-- Analytic score differences that are nonidentically zero on each competing
pair's common interval cannot vanish simultaneously at more levels than the
number of pair coordinates, except on a null set. -/
theorem ae_no_identified_score_ties {d n : ℕ} (hdn : d < n)
    (f : (Fin d → ℝ) × ℝ → ℝ) (S : Set (Fin d → ℝ))
    (U : (Fin d → ℝ) → Set ℝ) (hU : ∀ p ∈ S, IsPreconnected (U p))
    (ha : ∀ p ∈ S, ∀ t ∈ U p, AnalyticAt ℝ f (p, t))
    (hne : ∀ p ∈ S, ∃ t ∈ U p, f (p, t) ≠ 0) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ S, (∀ i, x i ∈ U p) →
      ¬ ∀ i, f (p, x i) = 0 := by
  let T : Set ((Fin d → ℝ) × (Fin n → ℝ)) :=
    {z | z.1 ∈ S ∧ (∀ i, z.2 i ∈ U z.1) ∧ ∀ i, f (z.1, z.2 i) = 0}
  have he := ae_no_analytic_incidence hdn f T
    (fun z hz i ↦ ha z.1 hz.1 (z.2 i) (hz.2.1 i)) (fun z hz i ↦ by
      have han : AnalyticOnNhd ℝ (fun t ↦ f (z.1, t)) (U z.1) := by
        intro t ht
        refine (ha z.1 hz.1 t ht).comp (f := fun u ↦ (z.1, u)) ?_
        exact analyticAt_const.prod analyticAt_id
      exact exists_derivative_before_nonzero han (hU z.1 hz.1) (hz.2.1 i)
        (hz.2.2 i) (hne z.1 hz.1))
  filter_upwards [he] with x hx p hp hu heq
  exact hx p ⟨hp, hu, heq⟩

end UnivariateML.AE3AnalyticIncidence

end
