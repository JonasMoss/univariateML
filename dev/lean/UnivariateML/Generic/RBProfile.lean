module

public import UnivariateML.Generic.Compactification
public import UnivariateML.Generic.Escape
public import Mathlib.Analysis.Calculus.Deriv.Basic

set_option linter.style.header false

/-! # Compact superlevels and stationarity at a profiled maximum -/

@[expose] public section

open Set Filter
open scoped Topology

namespace UnivariateML.RBProfile

/-- A compact set inside the feasible domain that contains the entire
superlevel of one feasible witness suffices for global attainment. -/
theorem exists_isMaxOn_of_compact_superlevel {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D K : Set X} (hK : IsCompact K) (hsub : K ⊆ D)
    (hL : ContinuousOn L D) {p : X} (hp : p ∈ D)
    (hcut : ∀ q ∈ D, L p ≤ L q → q ∈ K) :
    ∃ q ∈ D, IsMaxOn L D q := by
  let A := {q ∈ D | L p ≤ L q}
  have hAK : A ⊆ K := fun _ h ↦ hcut _ h.1 h.2
  have hpA : p ∈ A := ⟨hp,le_rfl⟩
  obtain ⟨w, hw, hm⟩ := Generic.exists_isMaxOn_of_boundary_le hAK hK (hL.mono hsub) hpA
    (fun q hq ↦ le_of_not_ge (fun h ↦ hq.2 ⟨hsub hq.1,h⟩))
  refine ⟨w, hw.1, fun q hq ↦ ?_⟩
  by_cases h : L p ≤ L q
  · exact hm (show q ∈ A from ⟨hq,h⟩)
  · exact (le_of_not_ge h).trans hw.2

/-- Compact superlevels give eventual strict loss below the level
on the cocompact filter of the feasible subtype. -/
theorem eventually_lt_cocompact_of_compact_superlevel {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D K : Set X} {J : ℝ} (hK : IsCompact K) (hsub : K ⊆ D)
    (hcut : ∀ q ∈ D, J ≤ L q → q ∈ K) :
    ∀ᶠ q : D in cocompact D, L q < J := by
  have : CompactSpace K := isCompact_iff_compactSpace.mp hK
  let incl (q : K) : D := ⟨q, hsub q.property⟩
  have hc : Continuous incl := continuous_subtype_val.subtype_mk _
  have hout : ∀ᶠ q : D in cocompact D, q ∉ range incl :=
    (isCompact_range hc).compl_mem_cocompact
  exact hout.mono fun q hq ↦ lt_of_not_ge (fun h ↦ hq
    ⟨⟨q,hcut q q.property h⟩,Subtype.ext rfl⟩)

/-- At a joint maximum the optimized profile is stationary if the
fixed-nuisance slice is stationary. The slice and the maximum height
sandwich the profile difference, even when the profile is defined by
a supremum and the nuisance argmax is set-valued. -/
theorem profile_hasDerivAt_zero {X : Type*} {D : Set X} {U : Set ℝ}
    {f : ℝ → X → ℝ} {c : ℝ} {p : X} (hp : p ∈ D) (hc : U ∈ 𝓝 c)
    (hmax : ∀ t ∈ U, ∀ q ∈ D, f t q ≤ f c p)
    (hd : HasDerivAt (fun t ↦ f t p) 0 c) :
    HasDerivAt (fun t ↦ sSup (f t '' D)) 0 c := by
  have hupper (t : ℝ) (ht : t ∈ U) : f c p ∈ upperBounds (f t '' D) := by
    rintro _ ⟨q, hq, rfl⟩
    exact hmax t ht q hq
  have hB (t : ℝ) (ht : t ∈ U) : BddAbove (f t '' D) := ⟨_, hupper t ht⟩
  have hle (t : ℝ) (ht : t ∈ U) : sSup (f t '' D) ≤ f c p :=
    csSup_le ⟨_, mem_image_of_mem _ hp⟩ (fun _ hb ↦ hupper t ht hb)
  have hlo (t : ℝ) (ht : t ∈ U) : f t p ≤ sSup (f t '' D) :=
    le_csSup (hB t ht) (mem_image_of_mem _ hp)
  have he : sSup (f c '' D) = f c p :=
    le_antisymm (hle c (mem_of_mem_nhds hc)) (hlo c (mem_of_mem_nhds hc))
  rw [hasDerivAt_iff_isLittleO, Asymptotics.isLittleO_iff]
  intro ε hε
  have hd' := Asymptotics.isLittleO_iff.mp hd.isLittleO hε
  filter_upwards [hc, hd'] with t ht ht'
  rw [he]
  simp only [smul_zero, sub_zero, Real.norm_eq_abs] at ht' ⊢
  rw [abs_of_nonpos (sub_nonpos.mpr (hle t ht))]
  rw [abs_of_nonpos (sub_nonpos.mpr (hmax t ht p hp))] at ht'
  exact (neg_le_neg (sub_le_sub_right (hlo t ht) _)).trans ht'

end UnivariateML.RBProfile

end
