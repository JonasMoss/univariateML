module

public import Mathlib.Analysis.Convex.Continuous
public import Mathlib.Topology.Order.Compact
public import Mathlib.Topology.Instances.Real.Lemmas

set_option linter.style.header false

/-!
# Coercivity and strictly concave maximization

The cocompact filter on the feasible subtype describes escape from every
compact subset of the domain. This covers approach to excluded boundary
points as well as escape to infinity. Strict concavity supplies convexity.
-/

@[expose] public section

open Set Filter

namespace UnivariateML.Generic

/-- A continuous objective tending to minus infinity on escape from compact
subsets has a maximizer on any nonempty feasible domain. -/
theorem exists_isMaxOn_of_tendsto_cocompact {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D : Set X} (hD : D.Nonempty) (hL : ContinuousOn L D)
    (hescape : Tendsto (fun x : D ↦ L x) (cocompact D) atBot) :
    ∃ x ∈ D, IsMaxOn L D x := by
  have : Nonempty D := hD.to_subtype
  have hcont : Continuous (fun y : D ↦ L y) := hL.domRestrict
  obtain ⟨x, hx⟩ := hcont.exists_forall_ge hescape
  exact ⟨x, x.property, fun y hy ↦ hx ⟨y, hy⟩⟩

/-- A continuous strictly concave objective with coercive escape has a unique maximizer. -/
theorem existsUnique_isMaxOn_of_strictConcaveOn_of_tendsto_cocompact
    {X : Type*} [TopologicalSpace X] [AddCommMonoid X] [Module ℝ X]
    {L : X → ℝ} {D : Set X} (hD : D.Nonempty) (hL : ContinuousOn L D)
    (hconc : StrictConcaveOn ℝ D L)
    (hescape : Tendsto (fun x : D ↦ L x) (cocompact D) atBot) :
    ∃! x, x ∈ D ∧ IsMaxOn L D x := by
  obtain ⟨x, hx, hm⟩ := exists_isMaxOn_of_tendsto_cocompact hD hL hescape
  exact ⟨x, ⟨hx, hm⟩, fun y hy ↦ hconc.eq_of_isMaxOn hy.2 hm hy.1 hx⟩

/-- On a finite-dimensional open domain, concavity supplies continuity. -/
theorem existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    {X : Type*} [NormedAddCommGroup X] [NormedSpace ℝ X] [FiniteDimensional ℝ X]
    {L : X → ℝ} {D : Set X} (hD : D.Nonempty) (hopen : IsOpen D)
    (hconc : StrictConcaveOn ℝ D L)
    (hescape : Tendsto (fun x : D ↦ L x) (cocompact D) atBot) :
    ∃! x, x ∈ D ∧ IsMaxOn L D x :=
  existsUnique_isMaxOn_of_strictConcaveOn_of_tendsto_cocompact
    hD (hconc.concaveOn.continuousOn hopen) hconc hescape

end UnivariateML.Generic
