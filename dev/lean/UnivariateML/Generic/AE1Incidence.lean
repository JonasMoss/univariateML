module

public import Mathlib.Analysis.Calculus.ImplicitContDiff
public import Mathlib.Topology.Compactness.Lindelof
public import Mathlib.Topology.MetricSpace.HausdorffDimension

set_option linter.style.header false

/-!
# Null sample projections from local incidence graphs

The intended pieces for analytic score differences are the strata where
each coordinate has a specified zero multiplicity. The mathematical
hypothesis supplied here is the local graph representation on each piece.
-/

@[expose] public section

open Set Filter MeasureTheory Topology
open scoped ContDiff ENNReal NNReal

namespace UnivariateML.AE1Incidence

set_option maxHeartbeats 600000 in
-- The dimension bound requires casts between natural, real and extended real dimensions.
private theorem projection_null {d n : ℕ} (hdn : d < n)
    (S : Set ((Fin d → ℝ) × (Fin n → ℝ)))
    (hgraph : ∀ z ∈ S, ∃ U ∈ 𝓝 z, ∃ V : Set (Fin d → ℝ), ∃ g : (Fin d → ℝ) → (Fin n → ℝ),
      DifferentiableOn ℝ g V ∧ ∀ y ∈ S ∩ U, y.1 ∈ V ∧ y.2 = g y.1) :
    volume (Prod.snd '' S) = 0 := by
  classical
  choose U hU V g hg he using hgraph
  obtain ⟨T, hcount, hcover⟩ :=
    (HereditarilyLindelofSpace.isLindelof S).elim_nhds_subcover' U hU
  have hnull (z : S) : volume (g z z.property '' V z z.property) = 0 := by
    have hdim := (hg z z.property).dimH_image_le.trans
      (dimH_mono (subset_univ (V z z.property)))
    rw [Real.dimH_univ_pi_fin] at hdim
    have hn : dimH (g z z.property '' V z z.property) < (n : ℝ≥0) := by
      exact hdim.trans_lt (by exact_mod_cast hdn)
    have hvol : (μH[((n : ℝ≥0) : ℝ)] : Measure (Fin n → ℝ)) = volume := by
      simpa only [Fintype.card_fin, NNReal.coe_natCast] using (hausdorffMeasure_pi_real (ι := Fin n))
    rw [← hvol]
    exact hausdorffMeasure_of_dimH_lt (d := (n : ℝ≥0)) hn
  apply measure_mono_null (t := ⋃ z ∈ T, g z z.property '' V z z.property)
  · rintro _ ⟨y, hy, rfl⟩
    obtain ⟨z, hz, hU'⟩ := mem_iUnion₂.mp (hcover hy)
    obtain ⟨hV, hy'⟩ := he z z.property y ⟨hy, hU'⟩
    exact mem_iUnion₂.mpr ⟨z, hz, y.1, hV, hy'.symm⟩
  · exact (measure_biUnion_null_iff hcount).mpr (fun z _ ↦ hnull z)

/-- A countable family of locally differentiable incidence graphs in d
parameter coordinates has a null sample projection in n > d dimensions. -/
private theorem ae_no_incidence {d n : ℕ} (hdn : d < n) {ι : Type*} [Countable ι]
    (S : ι → Set ((Fin d → ℝ) × (Fin n → ℝ)))
    (hgraph : ∀ i, ∀ z ∈ S i,
      ∃ U ∈ 𝓝 z, ∃ V : Set (Fin d → ℝ), ∃ g : (Fin d → ℝ) → (Fin n → ℝ),
        DifferentiableOn ℝ g V ∧ ∀ y ∈ S i ∩ U, y.1 ∈ V ∧ y.2 = g y.1) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ i p, (p, x) ∉ S i := by
  have h (i : ι) : ∀ᵐ x : Fin n → ℝ ∂volume, x ∉ Prod.snd '' S i := by
    rw [ae_iff]
    simpa only [not_not, ofPred_mem_eq] using projection_null hdn (S i) (hgraph i)
  filter_upwards [ae_all_iff.mpr h] with x hx i p hp
  exact hx i ⟨(p, x), hp, rfl⟩

/-- On each incidence stratum, a smooth system of equations with invertible
sample derivative gives a null sample projection if the parameter dimension
is strictly smaller than the sample dimension. -/
theorem ae_no_implicit_incidence {d n : ℕ} (hdn : d < n) {ι : Type*} [Countable ι]
    (S : ι → Set ((Fin d → ℝ) × (Fin n → ℝ)))
    (F : ι → ((Fin d → ℝ) × (Fin n → ℝ)) → (Fin n → ℝ))
    (hzero : ∀ i, ∀ z ∈ S i, F i z = 0)
    (hsmooth : ∀ i, ∀ z ∈ S i, ContDiffAt ℝ 1 (F i) z)
    (hinv : ∀ i, ∀ z ∈ S i,
      (fderiv ℝ (F i) z ∘L ContinuousLinearMap.inr ℝ (Fin d → ℝ) (Fin n → ℝ)).IsInvertible) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ i p, (p, x) ∉ S i := by
  apply ae_no_incidence hdn S
  intro i z hz
  have hs := hsmooth i z hz
  have hi := hinv i z hz
  let g := hs.implicitFunction one_ne_zero hi
  let V : Set (Fin d → ℝ) := {p | ContDiffAt ℝ 1 g p}
  have hV : V ∈ 𝓝 z.1 := (hs.contDiffAt_implicitFunction one_ne_zero hi).eventually (by simp)
  let U : Set ((Fin d → ℝ) × (Fin n → ℝ)) :=
    {y | F i y = F i z ↔ g y.1 = y.2} ∩ Prod.fst ⁻¹' V
  have hU : U ∈ 𝓝 z := inter_mem
    (hs.eventually_apply_eq_iff_implicitFunction one_ne_zero hi)
    (continuous_fst.continuousAt hV)
  refine ⟨U, hU, V, g, fun p hp ↦ hp.differentiableAt_one.differentiableWithinAt, ?_⟩
  rintro y ⟨hy, he, hmem⟩
  exact ⟨hmem, (he.mp ((hzero i y hy).trans (hzero i z hz).symm)).symm⟩

end UnivariateML.AE1Incidence

end
