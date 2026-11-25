module

public import Mathlib.Topology.Order.OrderClosed
public import Mathlib.Topology.Instances.Real.Lemmas

set_option linter.style.header false

/-! # Bounds from feasible limit paths -/

@[expose] public section

open Set Filter Topology

namespace UnivariateML.Generic

/-- A finite limit along an eventually feasible path is below every upper bound. -/
theorem limit_le_upperBound {X ι : Type*} {L : X → ℝ} {D : Set X}
    {l : Filter ι} [l.NeBot] {p : ι → X} {c u : ℝ}
    (hp : ∀ᶠ i in l, p i ∈ D) (hc : Tendsto (fun i ↦ L (p i)) l (𝓝 c))
    (hu : u ∈ upperBounds (L '' D)) : c ≤ u :=
  le_of_tendsto hc (hp.mono fun _ hi ↦ hu (mem_image_of_mem L hi))

/-- For a bounded likelihood image, every feasible finite path limit is below its supremum. -/
theorem limit_le_sSup {X ι : Type*} {L : X → ℝ} {D : Set X}
    {l : Filter ι} [l.NeBot] {p : ι → X} {c : ℝ}
    (hp : ∀ᶠ i in l, p i ∈ D) (hc : Tendsto (fun i ↦ L (p i)) l (𝓝 c))
    (hB : BddAbove (L '' D)) : c ≤ sSup (L '' D) :=
  limit_le_upperBound hp hc (fun _ hy ↦ le_csSup hB hy)

/-- The real image is either unbounded above or its supremum bounds the path limit. -/
theorem not_bddAbove_or_limit_le_sSup {X ι : Type*} {L : X → ℝ} {D : Set X}
    {l : Filter ι} [l.NeBot] {p : ι → X} {c : ℝ}
    (hp : ∀ᶠ i in l, p i ∈ D) (hc : Tendsto (fun i ↦ L (p i)) l (𝓝 c)) :
    ¬ BddAbove (L '' D) ∨ c ≤ sSup (L '' D) := by
  by_cases hB : BddAbove (L '' D)
  · exact Or.inr (limit_le_sSup hp hc hB)
  · exact Or.inl hB

end UnivariateML.Generic
