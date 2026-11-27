module

public import Mathlib.Analysis.Calculus.Rademacher
public import Mathlib.Analysis.Calculus.LocalExtr.Basic
public import Mathlib.Analysis.Calculus.MeanValue
public import Mathlib.Topology.Compactness.Lindelof

import Mathlib.Analysis.SpecialFunctions.Log.Deriv
import Mathlib.MeasureTheory.Measure.Lebesgue.EqHaar
import Mathlib.Analysis.Normed.Module.Convex
import Mathlib.Analysis.Normed.Module.FiniteDimension
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Almost-everywhere agreement of sample derivatives at global winners

Rademacher's theorem is applied to compact comparison envelopes. The full
likelihood envelope need not be locally Lipschitz or even finite away from
the samples at which winners are compared.
-/

@[expose] public section

open Set Filter MeasureTheory Metric
open scoped NNReal Topology

namespace UnivariateML.UniqueAE

variable {P E : Type*} [TopologicalSpace P]
  [NormedAddCommGroup E] [NormedSpace ℝ E]

/-- The envelope of a compact parameter piece. -/
noncomputable def envelope (L : P → E → ℝ) (K : Set P) (x : E) : ℝ :=
  sSup ((fun p ↦ L p x) '' K)

omit [NormedSpace ℝ E] in
private theorem piece_max {K : Set P} {C : Set E} {L : P → E → ℝ}
    (hK : IsCompact K) (hne : K.Nonempty)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ C))
    {x : E} (hx : x ∈ C) :
    ∃ p ∈ K, L p x = envelope L K x ∧ ∀ q ∈ K, L q x ≤ envelope L K x := by
  have hc : ContinuousOn (fun p ↦ L p x) K :=
    hL.comp (continuous_id.prodMk continuous_const).continuousOn (fun _ hp ↦ ⟨hp, hx⟩)
  obtain ⟨p, hp, hm⟩ := hK.exists_isMaxOn hne hc
  have hb : BddAbove ((fun q ↦ L q x) '' K) := ⟨L p x, by
    rintro _ ⟨q, hq, rfl⟩
    exact hm hq⟩
  have he : L p x = envelope L K x :=
    le_antisymm (le_csSup hb (mem_image_of_mem _ hp))
      (csSup_le (hne.image _) (by rintro _ ⟨q, hq, rfl⟩; exact hm hq))
  exact ⟨p, hp, he, fun q hq ↦ (hm hq).trans_eq he⟩

/-- A compact parameter piece has a Lipschitz envelope on each compact convex
sample region on which the likelihood and its sample derivative are continuous. -/
theorem envelope_lipschitzOn {K : Set P} {C : Set E} {L : P → E → ℝ}
    {D : P → E → E →L[ℝ] ℝ}
    (hK : IsCompact K) (hne : K.Nonempty) (hC : IsCompact C) (hc : Convex ℝ C)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ C))
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) (K ×ˢ C))
    (hd : ∀ p ∈ K, ∀ x ∈ C, HasFDerivAt (L p) (D p x) x) :
    ∃ M : ℝ≥0, LipschitzOnWith M (envelope L K) C := by
  have hnorm : ContinuousOn (fun z : P × E ↦ ‖D z.1 z.2‖₊) (K ×ˢ C) :=
    hD.nnNorm
  obtain ⟨A, hA⟩ : BddAbove ((fun z : P × E ↦ ‖D z.1 z.2‖₊) '' (K ×ˢ C)) :=
    (hK.prod hC).bddAbove_image hnorm
  have hlip (p : P) (hp : p ∈ K) : LipschitzOnWith A (L p) C := by
    have hb (x : E) (hx : x ∈ C) : ‖D p x‖₊ ≤ A := hA ⟨(p, x), ⟨hp, hx⟩, rfl⟩
    exact Convex.lipschitzOnWith_of_nnNorm_hasFDerivWithin_le
      (fun x hx ↦ (hd p hp x hx).hasFDerivWithinAt) hb hc
  apply Exists.intro A
  apply LipschitzOnWith.of_dist_le_mul
  intro x hx y hy
  have hle (u v : E) (hu : u ∈ C) (hv : v ∈ C) :
      envelope L K u - envelope L K v ≤ A * dist u v := by
    obtain ⟨p, hp, he, _⟩ := piece_max hK hne hL hu
    obtain ⟨_, _, _, hb⟩ := piece_max hK hne hL hv
    rw [← he]
    have h := (hlip p hp).dist_le_mul u hu v hv
    rw [Real.dist_eq] at h
    linarith [hb p hp, le_abs_self (L p u - L p v)]
  rw [Real.dist_eq, abs_le]
  refine ⟨?_, hle x y hx hy⟩
  have h := hle y x hy hx
  rw [dist_comm y x] at h
  linarith

private theorem derivative_of_touch {f V : E → ℝ} {x : E} {d : E →L[ℝ] ℝ}
    (hd : HasFDerivAt f d x) (hV : DifferentiableAt ℝ V x)
    (he : f x = V x) (hle : ∀ᶠ y in 𝓝 x, f y ≤ V y) :
    d = fderiv ℝ V x := by
  have hm : IsLocalMin (fun y ↦ V y - f y) x :=
    hle.mono (fun y hy ↦ by change V x - f x ≤ V y - f y; linarith)
  exact (sub_eq_zero.mp (hm.hasFDerivAt_eq_zero (hV.hasFDerivAt.sub hd))).symm

variable [FiniteDimensional ℝ E] [MeasurableSpace E] [BorelSpace E]
  {μ : Measure E} [μ.IsAddHaarMeasure]

/-- Almost everywhere in the interior of a compact convex sample region, all
winners of the compact piece have the derivative of its envelope. -/
theorem ae_piece_derivative {K : Set P} {C : Set E} {L : P → E → ℝ}
    {D : P → E → E →L[ℝ] ℝ}
    (hK : IsCompact K) (hne : K.Nonempty) (hC : IsCompact C) (hc : Convex ℝ C)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ C))
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) (K ×ˢ C))
    (hd : ∀ p ∈ K, ∀ x ∈ C, HasFDerivAt (L p) (D p x) x) :
    ∀ᵐ x ∂μ, x ∈ interior C → ∀ p ∈ K, L p x = envelope L K x →
      D p x = fderiv ℝ (envelope L K) x := by
  obtain ⟨M, hM⟩ := envelope_lipschitzOn hK hne hC hc hL hD hd
  filter_upwards [hM.ae_differentiableWithinAt_of_mem (μ := μ)] with x hx hi p hp he
  have hn := mem_interior_iff_mem_nhds.mp hi
  exact derivative_of_touch (hd p hp x (interior_subset hi))
    ((hx (interior_subset hi)).differentiableAt hn) he
    (Filter.eventually_of_mem hn (fun y hy ↦ (piece_max hK hne hL hy).choose_spec.2.2 p hp))

omit [NormedSpace ℝ E] [FiniteDimensional ℝ E] [MeasurableSpace E] [BorelSpace E] in
private theorem closedBall_cover {U : Set E} (hU : IsOpen U) (x : U) :
    ∃ r > 0, closedBall (x : E) r ⊆ U :=
  Metric.nhds_basis_closedBall.mem_iff.mp (hU.mem_nhds x.property)

omit [MeasurableSpace E] [BorelSpace E] in
/-- The envelope of a compact piece is locally Lipschitz wherever the sample
likelihood and its derivative are jointly continuous. -/
theorem envelope_locallyLipschitzOn {K : Set P} {U : Set E} {L : P → E → ℝ}
    {D : P → E → E →L[ℝ] ℝ}
    (hK : IsCompact K) (hne : K.Nonempty) (hU : IsOpen U)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ U))
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) (K ×ˢ U))
    (hd : ∀ p ∈ K, ∀ x ∈ U, HasFDerivAt (L p) (D p x) x) :
    LocallyLipschitzOn U (envelope L K) := by
  intro x hx
  obtain ⟨r, hr, hsub⟩ := closedBall_cover hU ⟨x, hx⟩
  have hs : K ×ˢ closedBall x r ⊆ K ×ˢ U := fun _ hz ↦ ⟨hz.1, hsub hz.2⟩
  obtain ⟨M, hM⟩ := envelope_lipschitzOn hK hne (isCompact_closedBall x r)
    (convex_closedBall x r) (hL.mono hs) (hD.mono hs)
    (fun p hp y hy ↦ hd p hp y (hsub hy))
  exact ⟨M, closedBall x r, mem_nhdsWithin_of_mem_nhds (closedBall_mem_nhds x hr), hM⟩

/-- One exceptional null set suffices for the whole open sample region. -/
theorem ae_envelope_derivative {K : Set P} {U : Set E} {L : P → E → ℝ}
    {D : P → E → E →L[ℝ] ℝ}
    (hK : IsCompact K) (hne : K.Nonempty) (hU : IsOpen U)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ U))
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) (K ×ˢ U))
    (hd : ∀ p ∈ K, ∀ x ∈ U, HasFDerivAt (L p) (D p x) x) :
    ∀ᵐ x ∂μ, x ∈ U → ∀ p ∈ K, L p x = envelope L K x →
      D p x = fderiv ℝ (envelope L K) x := by
  classical
  choose r hr hsub using closedBall_cover hU
  obtain ⟨s, hs, hcov⟩ := (HereditarilyLindelofSpace.isLindelof U).elim_countable_subcover
    (fun x : U ↦ ball (x : E) (r x)) (fun _ ↦ isOpen_ball)
    (fun x hx ↦ mem_iUnion.mpr ⟨⟨x, hx⟩, mem_ball_self (hr ⟨x, hx⟩)⟩)
  let : Countable s := hs.to_subtype
  have hae (a : s) :
      ∀ᵐ x ∂μ, x ∈ interior (closedBall ((a : U) : E) (r a)) →
        ∀ p ∈ K, L p x = envelope L K x → D p x = fderiv ℝ (envelope L K) x := by
    have hprod : K ×ˢ closedBall ((a : U) : E) (r a) ⊆ K ×ˢ U :=
      fun _ hz ↦ ⟨hz.1, hsub a hz.2⟩
    exact ae_piece_derivative hK hne (isCompact_closedBall _ _) (convex_closedBall _ _)
      (hL.mono hprod) (hD.mono hprod) (fun p hp y hy ↦ hd p hp y (hsub a hy))
  filter_upwards [ae_all_iff.mpr hae] with x hx hxU
  obtain ⟨a, ha, hxa⟩ := mem_iUnion₂.mp (hcov hxU)
  exact hx ⟨a, ha⟩ (ball_subset_interior_closedBall hxa)

/-- A parameter is feasible and attains the global finite supremum at the sample. -/
def Winner (S : Set (P × E)) (L : P → E → ℝ) (x : E) (p : P) : Prop :=
  (p, x) ∈ S ∧ ∀ q : P, (q, x) ∈ S → L q x ≤ L p x

/-- A compact comparison piece in a feasible parameter–sample domain. Its
sample interior is used for the covering, and the whole compact product for
a uniform bound on the sample derivative. -/
structure Piece (S : Set (P × E)) where
  parameter : Set P
  sample : Set E
  compact_parameter : IsCompact parameter
  nonempty_parameter : parameter.Nonempty
  compact_sample : IsCompact sample
  convex_sample : Convex ℝ sample
  feasible : parameter ×ˢ sample ⊆ S

/-- For a countable compact-piece cover, almost every sample has one common
sample derivative across all parameters attaining its global supremum. -/
theorem ae_winners_derivative_of_cover {ι : Type*} [Countable ι]
    {S : Set (P × E)} {L : P → E → ℝ} {D : P → E → E →L[ℝ] ℝ}
    (B : ι → Piece S)
    (hcover : ∀ z ∈ S, ∃ j, z.1 ∈ (B j).parameter ∧ z.2 ∈ interior (B j).sample)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) S)
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) S)
    (hd : ∀ p x, (p, x) ∈ S → HasFDerivAt (L p) (D p x) x) :
    ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q → D p x = D q x := by
  let V (j : ι) := envelope L (B j).parameter
  let W (j k : ι) (x : E) := max (V j x) (V k x)
  have hLip (j : ι) : ∃ M : ℝ≥0, LipschitzOnWith M (V j) (B j).sample :=
    envelope_lipschitzOn (B j).compact_parameter (B j).nonempty_parameter
      (B j).compact_sample (B j).convex_sample (hL.mono (B j).feasible)
      (hD.mono (B j).feasible) (fun p hp x hx ↦ hd p x ((B j).feasible ⟨hp, hx⟩))
  have hae (jk : ι × ι) :
      ∀ᵐ x ∂μ, x ∈ (B jk.1).sample ∩ (B jk.2).sample →
        DifferentiableWithinAt ℝ (W jk.1 jk.2) ((B jk.1).sample ∩ (B jk.2).sample) x := by
    obtain ⟨M, hM⟩ := hLip jk.1
    obtain ⟨N, hN⟩ := hLip jk.2
    have h₁ : LipschitzOnWith M (V jk.1) ((B jk.1).sample ∩ (B jk.2).sample) :=
      fun _ hx _ hy ↦ hM hx.1 hy.1
    have h₂ : LipschitzOnWith N (V jk.2) ((B jk.1).sample ∩ (B jk.2).sample) :=
      fun _ hx _ hy ↦ hN hx.2 hy.2
    have hW : LipschitzOnWith (max M N) (W jk.1 jk.2)
        ((B jk.1).sample ∩ (B jk.2).sample) := by
      simpa only [one_mul, Function.comp_def, W] using
        lipschitzWith_max.comp_lipschitzOnWith (h₁.prodMk h₂)
    exact hW.ae_differentiableWithinAt_of_mem
  filter_upwards [ae_all_iff.mpr hae] with x hx p q hp hq
  obtain ⟨j, hpj, hxj⟩ := hcover (p, x) hp.1
  obtain ⟨k, hqk, hxk⟩ := hcover (q, x) hq.1
  have hj := mem_interior_iff_mem_nhds.mp hxj
  have hk := mem_interior_iff_mem_nhds.mp hxk
  have hdiff : DifferentiableAt ℝ (W j k) x :=
    (hx (j, k) ⟨interior_subset hxj, interior_subset hxk⟩).differentiableAt
      (inter_mem hj hk)
  have hpiece (i : ι) {y : E} (hy : y ∈ (B i).sample) :
      ∃ r ∈ (B i).parameter, L r y = V i y ∧ ∀ s ∈ (B i).parameter, L s y ≤ V i y :=
    piece_max (B i).compact_parameter (B i).nonempty_parameter (hL.mono (B i).feasible) hy
  obtain ⟨r, hr, her, _⟩ := hpiece j (interior_subset hxj)
  obtain ⟨s, hs, hes, _⟩ := hpiece k (interior_subset hxk)
  have htouch (a : P) (ha : Winner S L x a) (hle : L a x ≤ W j k x) :
      L a x = W j k x := by
    refine le_antisymm hle (max_le ?_ ?_)
    · rw [← her]
      exact ha.2 r ((B j).feasible ⟨hr, interior_subset hxj⟩)
    · rw [← hes]
      exact ha.2 s ((B k).feasible ⟨hs, interior_subset hxk⟩)
  have hp_le : ∀ᶠ y in 𝓝 x, L p y ≤ W j k y :=
    (Filter.eventually_of_mem hj fun _ hy ↦ (hpiece j hy).choose_spec.2.2 p hpj |>.trans
      (le_max_left _ _))
  have hq_le : ∀ᶠ y in 𝓝 x, L q y ≤ W j k y :=
    (Filter.eventually_of_mem hk fun _ hy ↦ (hpiece k hy).choose_spec.2.2 q hqk |>.trans
      (le_max_right _ _))
  exact (derivative_of_touch (hd p x hp.1) hdiff (htouch p hp hp_le.self_of_nhds) hp_le).trans
    (derivative_of_touch (hd q x hq.1) hdiff (htouch q hq hq_le.self_of_nhds) hq_le).symm

/-- If equal sample derivatives identify the winning class almost everywhere,
then almost every sample has at most one winning class. Use the identity map
for uniqueness of the parameter itself. -/
theorem ae_winning_class_of_cover {ι A : Type*} [Countable ι]
    {S : Set (P × E)} {L : P → E → ℝ} {D : P → E → E →L[ℝ] ℝ}
    (B : ι → Piece S)
    (hcover : ∀ z ∈ S, ∃ j, z.1 ∈ (B j).parameter ∧ z.2 ∈ interior (B j).sample)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) S)
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) S)
    (hd : ∀ p x, (p, x) ∈ S → HasFDerivAt (L p) (D p x) x)
    (cls : P → A) (hsep : ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q →
      D p x = D q x → cls p = cls q) :
    ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q → cls p = cls q := by
  filter_upwards [ae_winners_derivative_of_cover B hcover hL hD hd, hsep] with x hx hs p q hp hq
  exact hs p q hp hq (hx p q hp hq)

omit [MeasurableSpace E] [BorelSpace E] in
/-- An open candidate–sample domain in a locally compact second-countable
chart admits a countable cover by compact parameter pieces and sample balls. -/
theorem exists_countable_piece_cover [LocallyCompactSpace P] [SecondCountableTopology P]
    {S : Set (P × E)} (hS : IsOpen S) :
    ∃ b : Set (Piece S), b.Countable ∧
      ∀ z ∈ S, ∃ B ∈ b, z.1 ∈ B.parameter ∧ z.2 ∈ interior B.sample := by
  have hcover : S ⊆ ⋃ B : Piece S, interior B.parameter ×ˢ interior B.sample := by
    rintro ⟨p, x⟩ hz
    obtain ⟨u, v, hu, hp, hv, hx, huv⟩ := mem_nhds_prod_iff'.mp (hS.mem_nhds hz)
    obtain ⟨K, hK, hpK, hKu⟩ := exists_compact_subset hu hp
    obtain ⟨r, hr, hrv⟩ := closedBall_cover hv ⟨x, hx⟩
    let B : Piece S := ⟨K, closedBall x r, hK, ⟨p, interior_subset hpK⟩,
      isCompact_closedBall x r, convex_closedBall x r,
      fun _ h ↦ huv ⟨hKu h.1, hrv h.2⟩⟩
    exact mem_iUnion.mpr ⟨B, hpK, ball_subset_interior_closedBall (mem_ball_self hr)⟩
  obtain ⟨b, hb, hcov⟩ := (HereditarilyLindelofSpace.isLindelof S).elim_countable_subcover
    (fun B : Piece S ↦ interior B.parameter ×ˢ interior B.sample)
    (fun _ ↦ isOpen_interior.prod isOpen_interior) hcover
  refine ⟨b, hb, fun z hz ↦ ?_⟩
  obtain ⟨B, hB, hp, hx⟩ := mem_iUnion₂.mp (hcov hz)
  exact ⟨B, hB, interior_subset hp, hx⟩

/-- In an open candidate–sample domain over a locally compact second-countable
parameter chart, all global winners have equal sample derivatives almost
everywhere. A countable disjoint union of such charts can be used as one chart. -/
theorem ae_winners_derivative [LocallyCompactSpace P] [SecondCountableTopology P]
    {S : Set (P × E)} {L : P → E → ℝ} {D : P → E → E →L[ℝ] ℝ}
    (hS : IsOpen S)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) S)
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) S)
    (hd : ∀ p x, (p, x) ∈ S → HasFDerivAt (L p) (D p x) x) :
    ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q → D p x = D q x := by
  obtain ⟨b, hb, hcover⟩ := exists_countable_piece_cover hS
  let : Countable b := hb.to_subtype
  exact ae_winners_derivative_of_cover (fun B : b ↦ B.val)
    (fun z hz ↦ by
      obtain ⟨B, hB, hp, hx⟩ := hcover z hz
      exact ⟨⟨B, hB⟩, hp, hx⟩) hL hD hd

/-- Sample derivatives that separate the winning classes give uniqueness of
the winning class almost everywhere in an open chart. -/
theorem ae_winning_class [LocallyCompactSpace P] [SecondCountableTopology P]
    {A : Type*} {S : Set (P × E)} {L : P → E → ℝ} {D : P → E → E →L[ℝ] ℝ}
    (hS : IsOpen S)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) S)
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) S)
    (hd : ∀ p x, (p, x) ∈ S → HasFDerivAt (L p) (D p x) x)
    (cls : P → A) (hsep : ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q →
      D p x = D q x → cls p = cls q) :
    ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q → cls p = cls q := by
  filter_upwards [ae_winners_derivative hS hL hD hd, hsep] with x hx hs p q hp hq
  exact hs p q hp hq (hx p q hp hq)

section Student

variable {n : ℕ}

/-- Held-df, held-scale Student log likelihood up to a positive affine
transformation independent of the location. The denominator scale is
`a = sd * sqrt (nu - 2)` for the variance-parameterized Student family. -/
noncomputable def studentLocationLogLik (a c : ℝ) (x : Fin n → ℝ) : ℝ :=
  ∑ i, -Real.log ((x i - c) ^ 2 + a ^ 2)

private noncomputable def studentScore (a c t : ℝ) : ℝ :=
  -(2 * (t - c)) / ((t - c) ^ 2 + a ^ 2)

private noncomputable def studentSampleDeriv (a c : ℝ) (x : Fin n → ℝ) :
    (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, studentScore a c (x i) • ContinuousLinearMap.proj i

private theorem student_positive {a : ℝ} (ha : 0 < a) (c t : ℝ) :
    0 < (t - c) ^ 2 + a ^ 2 := by positivity

private theorem student_hasFDerivAt {a : ℝ} (ha : 0 < a) (c : ℝ) (x : Fin n → ℝ) :
    HasFDerivAt (studentLocationLogLik a c) (studentSampleDeriv a c x) x := by
  unfold studentLocationLogLik studentSampleDeriv
  apply HasFDerivAt.fun_sum
  intro i _
  let f : (Fin n → ℝ) →L[ℝ] ℝ := ContinuousLinearMap.proj i
  have hd := ((((hasDerivAt_id (x i)).sub_const c).pow 2).add_const (a ^ 2)).log
    (student_positive ha c (x i)).ne'
  have hterm : HasDerivAt (fun t : ℝ ↦ -Real.log ((t - c) ^ 2 + a ^ 2))
      (studentScore a c (x i)) (f x) := by
    convert hd.neg using 1 <;> first | rfl | simp [studentScore, neg_div]
  exact hterm.comp_hasFDerivAt x f.hasFDerivAt

private theorem student_deriv_continuous {a : ℝ} (ha : 0 < a) :
    Continuous (fun z : ℝ × (Fin n → ℝ) ↦ studentSampleDeriv a z.1 z.2) := by
  unfold studentSampleDeriv
  apply continuous_finsetSum Finset.univ
  intro i _
  have hc : Continuous (fun z : ℝ × (Fin n → ℝ) ↦ studentScore a z.1 (z.2 i)) := by
    unfold studentScore
    exact (by fun_prop : Continuous fun z : ℝ × (Fin n → ℝ) ↦ -(2 * (z.2 i - z.1))).div
      (by fun_prop) (fun z ↦ (student_positive ha z.1 (z.2 i)).ne')
  exact hc.smul (continuous_const (y :=
    (ContinuousLinearMap.proj i : (Fin n → ℝ) →L[ℝ] ℝ)))

private theorem student_likelihood_continuous {a : ℝ} (ha : 0 < a) :
    Continuous (fun z : ℝ × (Fin n → ℝ) ↦ studentLocationLogLik a z.1 z.2) := by
  unfold studentLocationLogLik
  apply continuous_finsetSum Finset.univ
  intro i _
  have hc : Continuous (fun z : ℝ × (Fin n → ℝ) ↦ (z.2 i - z.1) ^ 2 + a ^ 2) := by fun_prop
  exact (hc.log (fun z ↦ (student_positive ha z.1 (z.2 i)).ne')).neg

private theorem student_deriv_apply_single (a c : ℝ) (x : Fin n → ℝ) (i : Fin n) :
    studentSampleDeriv a c x (Pi.single i 1) = studentScore a c (x i) := by
  simp only [studentSampleDeriv, sum_apply, smul_apply,
    ContinuousLinearMap.proj_apply, smul_eq_mul]
  rw [Finset.sum_eq_single i]
  · simp
  · intro j _ hji
    simp [Pi.single_eq_of_ne hji]
  · simp

private theorem student_score_crossing {a c d t : ℝ} (ha : 0 < a) (hcd : c ≠ d)
    (he : studentScore a c t = studentScore a d t) : (t - c) * (t - d) = a ^ 2 := by
  simp only [studentScore] at he
  have h := (div_eq_div_iff (student_positive ha c t).ne'
    (student_positive ha d t).ne').mp he
  have hz : (c - d) * ((t - c) * (t - d) - a ^ 2) = 0 := by linear_combination -h / 2
  exact sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_left (sub_ne_zero.mpr hcd))

private theorem student_separates {a c d : ℝ} (ha : 0 < a) {x : Fin n → ℝ}
    {i j k : Fin n} (hij : x i ≠ x j) (hik : x i ≠ x k) (hjk : x j ≠ x k)
    (he : studentSampleDeriv a c x = studentSampleDeriv a d x) : c = d := by
  by_contra hcd
  have h (l : Fin n) : (x l - c) * (x l - d) = a ^ 2 := by
    apply student_score_crossing ha hcd
    rw [← student_deriv_apply_single, ← student_deriv_apply_single, he]
  have hpair (l m : Fin n) (hne : x l ≠ x m) : x l + x m = c + d := by
    have hz : (x l - x m) * (x l + x m - (c + d)) = 0 := by
      linear_combination h l - h m
    exact sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_left (sub_ne_zero.mpr hne))
  exact hjk (by linarith [hpair i j hij, hpair i k hik])

private theorem ae_distinct_coordinates {i j : Fin n} (hij : i ≠ j) :
    ∀ᵐ x : Fin n → ℝ ∂volume, x i ≠ x j := by
  classical
  let f : (Fin n → ℝ) →L[ℝ] ℝ :=
    (ContinuousLinearMap.proj i : (Fin n → ℝ) →L[ℝ] ℝ) - ContinuousLinearMap.proj j
  have hker : LinearMap.ker f.toLinearMap ≠ ⊤ := by
    intro he
    let e : Fin n → ℝ := Pi.single i 1
    have hm : e ∈ LinearMap.ker f.toLinearMap := by simp [he]
    have hz := LinearMap.mem_ker.mp hm
    change e i - e j = 0 at hz
    simp [e, Pi.single_eq_of_ne hij.symm] at hz
  rw [ae_iff]
  have hs : {x : Fin n → ℝ | ¬ x i ≠ x j} =
      (LinearMap.ker f.toLinearMap : Set (Fin n → ℝ)) := by
    ext x
    simp [f, sub_eq_zero]
  rw [hs]
  exact Measure.addHaar_submodule volume _ hker

/-- With degrees of freedom and scale held, every attained Student fit is
unique for Lebesgue-almost every sample of size at least three. -/
theorem student_held_df_scale_ae_unique {a : ℝ} (ha : 0 < a) (hn : 3 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ c d : ℝ,
      IsMaxOn (fun p ↦ studentLocationLogLik a p x) univ c →
      IsMaxOn (fun p ↦ studentLocationLogLik a p x) univ d → c = d := by
  let i : Fin n := ⟨0, by omega⟩
  let j : Fin n := ⟨1, by omega⟩
  let k : Fin n := ⟨2, by omega⟩
  have hij : i ≠ j := by simp [i, j]
  have hik : i ≠ k := by simp [i, k]
  have hjk : j ≠ k := by simp [j, k]
  have hsep : ∀ᵐ x : Fin n → ℝ ∂volume, ∀ c d : ℝ,
      Winner univ (studentLocationLogLik a) x c → Winner univ (studentLocationLogLik a) x d →
      studentSampleDeriv a c x = studentSampleDeriv a d x → id c = id d := by
    filter_upwards [ae_distinct_coordinates hij, ae_distinct_coordinates hik,
      ae_distinct_coordinates hjk] with x h₁ h₂ h₃ c d _ _ he
    exact student_separates ha h₁ h₂ h₃ he
  have hae := ae_winning_class (S := (univ : Set (ℝ × (Fin n → ℝ)))) isOpen_univ
    (student_likelihood_continuous ha).continuousOn (student_deriv_continuous ha).continuousOn
    (fun c x _ ↦ student_hasFDerivAt ha c x) id hsep
  filter_upwards [hae] with x hx c d hc hd
  exact hx c d ⟨mem_univ _, fun _ _ ↦ hc (mem_univ _)⟩
    ⟨mem_univ _, fun _ _ ↦ hd (mem_univ _)⟩

private theorem student_location_attains {a : ℝ} (ha : 0 < a) (hn : 0 < n)
    (x : Fin n → ℝ) : ∃ c : ℝ, IsMaxOn (fun p ↦ studentLocationLogLik a p x) univ c := by
  classical
  let j : Fin n := ⟨0,hn⟩
  obtain ⟨l,_,hl⟩ := Finset.exists_min_image Finset.univ x ⟨j,Finset.mem_univ _⟩
  obtain ⟨u,_,hu⟩ := Finset.exists_max_image Finset.univ x ⟨j,Finset.mem_univ _⟩
  have hlu := hl u (Finset.mem_univ u)
  have hc : Continuous (fun p : ℝ ↦ studentLocationLogLik a p x) :=
    (student_likelihood_continuous ha).comp (continuous_id.prodMk continuous_const)
  obtain ⟨c,_,hcmax⟩ := isCompact_Icc.exists_isMaxOn ⟨x l,le_rfl,hlu⟩ hc.continuousOn
  refine ⟨c,fun d _ ↦ ?_⟩
  by_cases hdl : d < x l
  · refine le_trans ?_ (hcmax ⟨le_rfl,hlu⟩)
    apply Finset.sum_le_sum
    intro i _
    have hh := hl i (Finset.mem_univ i)
    have hsq : (x i-x l)^2 ≤ (x i-d)^2 := by
      nlinarith [mul_nonneg (sub_nonneg.mpr hdl.le) (show 0 ≤ 2*x i-x l-d by linarith)]
    exact neg_le_neg (Real.log_le_log (student_positive ha (x l) (x i)) (by linarith))
  by_cases hdu : x u < d
  · refine le_trans ?_ (hcmax ⟨hlu,le_rfl⟩)
    apply Finset.sum_le_sum
    intro i _
    have hh := hu i (Finset.mem_univ i)
    have hsq : (x i-x u)^2 ≤ (x i-d)^2 := by
      nlinarith [mul_nonneg (sub_nonneg.mpr hdu.le) (show 0 ≤ d+x u-2*x i by linarith)]
    exact neg_le_neg (Real.log_le_log (student_positive ha (x u) (x i)) (by linarith))
  exact hcmax ⟨le_of_not_gt hdl,le_of_not_gt hdu⟩

/-- With degrees of freedom and scale held, the Student location MLE
exists and is unique for Lebesgue-almost every sample of size at least three. -/
theorem student_held_df_scale_ae_existsUnique {a : ℝ} (ha : 0 < a) (hn : 3 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∃! c : ℝ,
      IsMaxOn (fun p ↦ studentLocationLogLik a p x) univ c := by
  filter_upwards [student_held_df_scale_ae_unique ha hn] with x hx
  obtain ⟨c,hc⟩ := student_location_attains ha (by omega) x
  exact ⟨c,hc,fun d hd ↦ hx d c hd hc⟩

end Student

end UnivariateML.UniqueAE

end
