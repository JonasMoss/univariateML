module

public import Mathlib.Topology.Order.Compact
public import Mathlib.Topology.Instances.Real.Lemmas

set_option linter.style.header false

/-!
# Attainment on a compactification

Write one function on the compact set and restrict it to the feasible set.

## Deviations

For the strict nonattainment result, the maximizing boundary point must lie
in the closure of the feasible set. A compact superset alone can contain
isolated infeasible points with larger values, even when a feasible
maximizer exists. A dense compactification supplies the closure hypothesis.
-/

@[expose] public section

open Set Filter Topology

namespace UnivariateML.Generic

/-- A feasible point tying or beating every boundary value suffices for attainment. -/
theorem exists_isMaxOn_of_boundary_le {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D K : Set X} (hDK : D ⊆ K) (hK : IsCompact K)
    (hL : ContinuousOn L K) {x : X} (hx : x ∈ D)
    (hboundary : ∀ b ∈ K \ D, L b ≤ L x) :
    ∃ y ∈ D, IsMaxOn L D y := by
  obtain ⟨y, hyK, hm⟩ := hK.exists_isMaxOn ⟨x, hDK hx⟩ hL
  by_cases hy : y ∈ D
  · exact ⟨y, hy, fun z hz ↦ hm (hDK hz)⟩
  · exact ⟨x, hx, fun z hz ↦ (hm (hDK hz)).trans (hboundary y ⟨hyK, hy⟩)⟩

/-- If a reachable boundary value strictly dominates every feasible value,
it is the supremum and no feasible point attains it. -/
theorem sSup_eq_and_not_exists_isMaxOn_of_boundary_gt {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D K : Set X} (hDK : D ⊆ K) (hL : ContinuousOn L K)
    {b : X} (hb : b ∈ K) (hcl : b ∈ closure D)
    (hboundary : ∀ x ∈ D, L x < L b) :
    sSup (L '' D) = L b ∧ ¬ ∃ x ∈ D, IsMaxOn L D x := by
  have : (𝓝[D] b).NeBot := mem_closure_iff_nhdsWithin_neBot.mp hcl
  have ht : Tendsto L (𝓝[D] b) (𝓝 (L b)) := (hL b hb).mono hDK
  have hmem : ∀ᶠ x in 𝓝[D] b, x ∈ D := self_mem_nhdsWithin
  have hupper : L b ∈ upperBounds (L '' D) := by
    rintro _ ⟨x, hx, rfl⟩
    exact (hboundary x hx).le
  have hB : BddAbove (L '' D) := ⟨L b, hupper⟩
  obtain ⟨x, hx⟩ := hmem.exists
  refine ⟨le_antisymm (csSup_le ⟨L x, mem_image_of_mem L hx⟩ hupper) ?_, ?_⟩
  · exact le_of_tendsto ht (hmem.mono fun y hy ↦ le_csSup hB (mem_image_of_mem L hy))
  · rintro ⟨y, hy, hm⟩
    exact (hboundary y hy).not_ge (le_of_tendsto ht (hmem.mono fun _ hz ↦ hm hz))

end UnivariateML.Generic
