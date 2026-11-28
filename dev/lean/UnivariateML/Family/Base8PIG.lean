module

public import UnivariateML.Family.Discrete4PIG

set_option linter.style.header false

@[expose] public section

open Set
open scoped BigOperators

namespace UnivariateML.Base8PIG

open Discrete4PIG

/-- On the physical nonnegative shape domain a held positive mean gives
the unique shape-zero fit on zeros. On positive-count data the compact
profile gives a unique physical shape exactly at positive variance excess. -/
theorem held_mean_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {μ : ℝ} (hμ : 0 < μ) :
    let m := (∑ i, (x i : ℝ)) / (n : ℝ)
    let v := (∑ i, ((x i : ℝ) - m)^2) / (n : ℝ)
    let D := Ioc (0 : ℝ) 1
    let W := fun lam : ℝ ↦ ∃ w ∈ D, shape μ w = lam ∧
      IsMaxOn (normalizedProfile x μ) D w
    ((∀ i, x i = 0) →
      ∀ lam ∈ Ici (0 : ℝ), IsMaxOn (HeldSweepPIG.zeroLikelihood n μ) (Ici 0) lam ↔ lam = 0) ∧
    ((∃ i, 0 < x i) →
      ((∃ lam, W lam) ↔ m < v + (m - μ)^2) ∧
      (m < v + (m - μ)^2 → ∃! lam, W lam)) := by
  intro m v D W
  refine ⟨fun _ _ hlam ↦ held_mean_all_zero hn hμ hlam, fun hx ↦ ?_⟩
  have H := held_mean_positive hn x hx hμ
  change (if m < v + (m - μ)^2 then
    ∃! w, w ∈ D ∧ IsMaxOn (normalizedProfile x μ) D w
    else IsLUB (normalizedProfile x μ '' D) 1 ∧
      ∀ w ∈ D, normalizedProfile x μ w < 1) at H
  by_cases h : m < v + (m - μ)^2
  · rw [ite_eq_left h] at H
    obtain ⟨w, ⟨hw, hM⟩, hU⟩ := H
    have hu : ∃! lam, W lam := by
      refine ⟨shape μ w, ⟨w, hw, rfl, hM⟩, ?_⟩
      rintro lam ⟨q, hq, rfl, hQ⟩
      rw [hU q ⟨hq, hQ⟩]
    exact ⟨⟨fun _ ↦ h, fun _ ↦ hu.exists⟩, fun _ ↦ hu⟩
  · rw [ite_eq_right h] at H
    refine ⟨⟨?_, fun h' ↦ (h h').elim⟩, fun h' ↦ (h h').elim⟩
    rintro ⟨lam, w, hw, _, hM⟩
    have hle : 1 ≤ normalizedProfile x μ w := H.1.2 (by
      rintro _ ⟨q, hq, rfl⟩
      exact hM hq)
    exact ((H.2 w hw).not_ge hle).elim

end UnivariateML.Base8PIG

end
