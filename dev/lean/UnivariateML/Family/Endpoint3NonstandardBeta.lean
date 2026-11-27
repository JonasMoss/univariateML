module

public import UnivariateML.Family.Endpoint2NonstandardBeta
public import UnivariateML.Family.Endpoint2NonstandardBetaAffine
public import UnivariateML.Family.Endpoint2NonstandardBetaContacts
public import UnivariateML.Family.Endpoint3GeneralizedBetaHeldPower

import Mathlib.Tactic

set_option linter.style.header false

/-! # Both endpoints free with fixed beta shapes -/

@[expose] public section

open Real Filter Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint3NonstandardBeta

variable {n : ℕ}

/-- With both endpoints free, a subunit shape permits a spike and a
constant sample permits concentration. The two mechanisms both give
unbounded likelihood using strictly containing endpoints. -/
theorem fixed_shapes_failure (hn : 0 < n) (x : Fin n → ℝ) {a b : ℝ}
    (h : (a < 1 ∨ b < 1) ∨ ∃ v, ∀ i, x i = v) :
    let D : Set (ℝ × ℝ) := {p | p.1 < p.2 ∧ ∀ i, x i ∈ Ioo p.1 p.2}
    let L : ℝ × ℝ → ℝ := fun p ↦ NonstandardBeta.logLik x p.1 p.2 (a, b)
    ¬ BddAbove (L '' D) := by
  dsimp only
  rcases h with h | ⟨v, hv⟩
  · exact Endpoint2NonstandardBeta.free_endpoints_unbounded hn x h.symm
  · rw [show x = fun _ ↦ v from funext hv]
    rintro ⟨B, hB⟩
    have hlim := Endpoint2NonstandardBeta.free_endpoints_constant_collapse (n := n) hn b a v
    have hpos : ∀ᶠ w : ℝ in 𝓝[>] 0, 0 < w := self_mem_nhdsWithin
    obtain ⟨w, hgt, hw⟩ := ((hlim.eventually_gt_atTop B).and hpos).exists
    exact hgt.not_ge (hB ⟨(v - w / 2, v + w / 2),
      ⟨by linarith, fun _ ↦ ⟨by linarith, by linarith⟩⟩, rfl⟩)

/-- A positive finite endpoint fit exists exactly for nonconstant data
with both shapes at least one. Endpoints may touch their sample extremes
at the corresponding unit shape. -/
theorem fixed_shapes_criterion (hn : 0 < n) {x : Fin n → ℝ} {m M a b : ℝ}
    (hx : ∀ i, x i ∈ Icc m M) (hmin : ∃ i, x i = m) (hmax : ∃ i, x i = M) :
    let D := {v ∈ Endpoint2NonstandardBetaAffine.endpointDomain m M a b | v.1 < v.2}
    let L := fun v : ℝ × ℝ ↦ NonstandardBeta.logLik x v.1 v.2 (a, b)
    (∃ v ∈ D, IsMaxOn L D v) ↔ m < M ∧ 1 ≤ a ∧ 1 ≤ b := by
  intro D L
  constructor
  · rintro ⟨v, _, hm⟩
    have hsub : {p : ℝ × ℝ | p.1 < p.2 ∧ ∀ i, x i ∈ Ioo p.1 p.2} ⊆ D := by
      intro p hp
      obtain ⟨i, hi⟩ := hmin
      obtain ⟨j, hj⟩ := hmax
      have hl : p.1 < m := hi ▸ (hp.2 i).1
      have hr : M < p.2 := hj ▸ (hp.2 j).2
      exact ⟨⟨hl.le, hr.le, Or.inr hl, Or.inr hr⟩, hp.1⟩
    have hgood : ¬ ((a < 1 ∨ b < 1) ∨ ∃ v, ∀ i, x i = v) :=
      fun h ↦ fixed_shapes_failure hn x h (BddAbove.mono (image_mono hsub) hm.bddAbove)
    have hspan : m < M := by
      by_contra! h
      exact hgood (Or.inr ⟨m, fun i ↦ le_antisymm ((hx i).2.trans h) (hx i).1⟩)
    exact ⟨hspan, le_of_not_gt fun h ↦ hgood (Or.inl (Or.inl h)),
      le_of_not_gt fun h ↦ hgood (Or.inl (Or.inr h))⟩
  · rintro ⟨hspan, ha, hb⟩
    obtain ⟨v, ⟨hv, hm⟩, _⟩ :=
      Endpoint2NonstandardBetaAffine.fixed_shapes_endpoint_fit hspan hx hmin hmax ha hb
    exact ⟨v, ⟨hv, hv.1.trans_lt (hspan.trans_le hv.2.1)⟩,
      fun _ h ↦ hm h.1⟩

/-- When one end and both shapes are held, the negative-distance and
held-contact cases are tested before finite likelihood. With positive
held-end factors, subunit free-end shape spikes at a positive maximum
distance, and zero distances at held shape one concentrate by shrinking
the width. Distances are measured inward from either held endpoint. -/
theorem fixed_width_shapes_failure (hn : 0 < n) {z : Fin n → ℝ} {a b Z : ℝ}
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) :
    let F := Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a b
    let L := Endpoint2NonstandardBeta.widthLogLik z a b
    let D := Endpoint2NonstandardBeta.finiteWidthDomain z a b
    ((∃ i, z i < 0) → ∀ W, F W = 0) ∧
    ((∀ i, 0 ≤ z i) → (∃ i, z i = 0) → ∀ W > max Z 0,
      (b < 1 → F W = ⊤) ∧ (1 < b → F W = 0) ∧
      (b = 1 → 0 < F W ∧ F W < ⊤)) ∧
    (Endpoint2NonstandardBeta.heldCompatible z b →
      ((a < 1 ∧ 0 < Z) ∨ Z = 0) → ¬ BddAbove (L '' D)) := by
  intro F L D
  refine ⟨fun h W ↦ Endpoint2NonstandardBetaContacts.incompatible_zero z h, ?_, ?_⟩
  · intro hz hcontact W hW
    exact Endpoint2NonstandardBetaContacts.contact_likelihood
      ((le_max_right Z 0).trans_lt hW)
      (fun i ↦ ⟨hz i, (hle i).trans_lt ((le_max_left Z 0).trans_lt hW)⟩) hcontact
  · intro hz hbad
    rintro ⟨B, hB⟩
    have hnear : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
    have hc (W : ℝ) (hW : 0 < W) (hZW : Z < W) : W ∈ D :=
      ⟨hW, hz, fun i ↦ (hle i).trans hZW.le, Or.inr fun i ↦ (hle i).trans_lt hZW⟩
    rcases hbad with ⟨ha, hZ⟩ | hZ
    · have hlim := Endpoint2NonstandardBeta.fixed_shapes_spike hZ hz hle hmax ha
      have hsmall : ∀ᶠ t : ℝ in 𝓝[>] 0, t < 1 :=
        mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)
      obtain ⟨t, hgt, ht, ht'⟩ := ((hlim.eventually_gt_atTop B).and
        (hnear.and hsmall)).exists
      have hW : Z < Z / (1 - t) := by
        apply (lt_div_iff₀ (sub_pos.mpr ht')).mpr
        nlinarith [mul_pos hZ ht]
      exact hgt.not_ge (hB (mem_image_of_mem L (hc _ (hZ.trans hW) hW)))
    · have hzero (i : Fin n) : z i = 0 := le_antisymm (hZ ▸ hle i) (hz.1 i)
      have hb : b = 1 := hz.2.resolve_right fun h ↦ (h ⟨0, hn⟩).ne' (hzero _)
      have hlim := Endpoint2NonstandardBeta.zero_distances_collapse hn a
      obtain ⟨W, hgt, hW⟩ := ((hlim.eventually_gt_atTop B).and hnear).exists
      have hh := hB (mem_image_of_mem L (hc W hW (hZ ▸ hW)))
      exact hgt.not_ge (by simpa only [L, funext hzero, hb] using hh)

private theorem width_genbeta_transfer {z : Fin n → ℝ} (hz : ∀ i, 0 < z i)
    {a b W : ℝ} (hW : 0 < W) :
    Endpoint2NonstandardBeta.widthLogLik z a b W =
      Endpoint2GeneralizedBeta.logLik z W b a 1 := by
  unfold Endpoint2NonstandardBeta.widthLogLik NonstandardBeta.logLik
    Endpoint2GeneralizedBeta.logLik
  apply Finset.sum_congr rfl
  intro i _
  have hr : (W - z i) / W = 1 - z i / W := by
    rw [sub_div, div_self hW.ne']
  simp only [sub_zero, Endpoint2GeneralizedBeta.unitData, rpow_one, log_one, zero_sub, hr]
  rw [log_div (hz i).ne' hW.ne']
  ring

private theorem width_genbeta_domain {z : Fin n → ℝ} {Z a : ℝ}
    (hz : ∀ i, 0 < z i) (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) :
    {v : ℝ × ℝ | 0 < v.1 ∧ v.2 ∈ Endpoint2NonstandardBeta.finiteWidthDomain z a v.1} =
      Ioi (0 : ℝ) ×ˢ (if a = 1 then Ici Z else Ioi Z) := by
  obtain ⟨j, hj⟩ := hmax
  have hZ : 0 < Z := hj ▸ hz j
  ext v
  constructor
  · intro hv
    refine ⟨hv.1, ?_⟩
    have hwidth := hv.2
    by_cases ha : a = 1
    · simpa only [ha, ite_true, mem_Ici, hj] using hwidth.2.2.1 j
    · have hs := hwidth.2.2.2.resolve_left ha
      simpa only [ha, ite_false, mem_Ioi, hj] using hs j
  · intro hv
    have hcompat : Endpoint2NonstandardBeta.heldCompatible z v.1 :=
      ⟨fun i ↦ (hz i).le, Or.inr hz⟩
    by_cases ha : a = 1
    · have hW : Z ≤ v.2 := by simpa [ha] using hv.2
      exact ⟨hv.1, hZ.trans_le hW, hcompat, fun i ↦ (hle i).trans hW, Or.inl ha⟩
    · have hW : Z < v.2 := by simpa [ha] using hv.2
      exact ⟨hv.1, hZ.trans hW, hcompat, fun i ↦ (hle i).trans hW.le,
        Or.inr fun i ↦ (hle i).trans_lt hW⟩

private theorem strict_width_finite {z : Fin n → ℝ} (hz : ∀ i, 0 < z i)
    (a b W : ℝ) : Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a b W < ⊤ := by
  classical
  unfold Endpoint2NonstandardBetaContacts.strictWidthLikelihood
  split
  · apply ENNReal.prod_lt_top
    intro i _
    simp only [Endpoint2NonstandardBetaContacts.heldFactor, (hz i).ne', ite_false]
    exact lt_top_iff_ne_top.mpr (ENNReal.mul_ne_top ENNReal.ofReal_ne_top ENNReal.ofReal_ne_top)
  · exact ENNReal.zero_lt_top

/-- With the free-end shape held, negative distances are incompatible,
held-end contact permits infinite likelihood, and a positive constant
sample or subunit free-end shape gives unbounded likelihood. The
inward-distance formulation covers either held endpoint. -/
theorem held_endpoint_fixed_free_shape_failure {z : Fin n → ℝ} {Z a : ℝ}
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) (ha : 0 < a) :
    let D := {v : ℝ × ℝ | 0 < v.1 ∧
      v.2 ∈ Endpoint2NonstandardBeta.finiteWidthDomain z a v.1}
    let L := fun v : ℝ × ℝ ↦ Endpoint2NonstandardBeta.widthLogLik z a v.1 v.2
    ((∃ i, z i < 0) → D = ∅) ∧
    ((∀ i, 0 ≤ z i) → (∃ i, z i = 0) → ∀ W > max Z 0,
      Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a (1 / 2) W = ⊤) ∧
    ((∀ i, 0 < z i) → (a < 1 ∨ ∀ i, z i = Z) → ¬ BddAbove (L '' D)) := by
  intro D L
  refine ⟨?_, ?_, ?_⟩
  · rintro ⟨i, hi⟩
    apply eq_empty_iff_forall_notMem.mpr
    intro v hv
    exact hi.not_ge (hv.2.2.1.1 i)
  · intro hz he W hW
    exact (Endpoint2NonstandardBetaContacts.contact_likelihood
      ((le_max_right Z 0).trans_lt hW)
      (fun i ↦ ⟨hz i, (hle i).trans_lt ((le_max_left Z 0).trans_lt hW)⟩) he).1 (by norm_num)
  · intro hz h
    let D' := Ioi (0 : ℝ) ×ˢ (if a = 1 then Ici Z else Ioi Z)
    let L' := fun v : ℝ × ℝ ↦ Endpoint2GeneralizedBeta.logLik z v.2 v.1 a 1
    have he : D = D' := width_genbeta_domain hz hle hmax
    have himage : L '' D = L' '' D' := by
      ext y
      constructor
      · rintro ⟨v, hv, rfl⟩
        exact ⟨v, he ▸ hv, (width_genbeta_transfer hz hv.2.1).symm⟩
      · rintro ⟨v, hv, rfl⟩
        have hw : v ∈ D := he.symm ▸ hv
        exact ⟨v, hw, width_genbeta_transfer hz hw.2.1⟩
    rw [himage]
    exact (Endpoint3GeneralizedBetaHeldPower.criterion hz hle hmax ha one_pos).2.1 h

/-- The free width and held-end shape admit a finite fit exactly when
all inward distances are positive and nonconstant and the held
free-end shape is at least one. The strict-width test excludes
infinite held-end contact before comparison on the finite domain. -/
theorem held_endpoint_fixed_free_shape_criterion {z : Fin n → ℝ} {Z a : ℝ}
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) (ha : 0 < a) :
    let D := {v : ℝ × ℝ | 0 < v.1 ∧
      v.2 ∈ Endpoint2NonstandardBeta.finiteWidthDomain z a v.1}
    let L := fun v : ℝ × ℝ ↦ Endpoint2NonstandardBeta.widthLogLik z a v.1 v.2
    let FiniteContact := ∀ b > 0, ∀ W > max Z 0,
      Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a b W < ⊤
    (FiniteContact ∧ ∃ v ∈ D, IsMaxOn L D v) ↔
      (∀ i, 0 < z i) ∧ (∃ i, z i < Z) ∧ 1 ≤ a := by
  intro D L FiniteContact
  have hiff (hz : ∀ i, 0 < z i) :
      (∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ a ∧ ∃ i, z i < Z := by
    let D' := Ioi (0 : ℝ) ×ˢ (if a = 1 then Ici Z else Ioi Z)
    let L' := fun v : ℝ × ℝ ↦ Endpoint2GeneralizedBeta.logLik z v.2 v.1 a 1
    have he : D = D' := width_genbeta_domain hz hle hmax
    have h (v : ℝ × ℝ) (hv : v ∈ D) : L v = L' v :=
      width_genbeta_transfer hz hv.2.1
    have hm (v : ℝ × ℝ) (hv : v ∈ D) : IsMaxOn L D v ↔ IsMaxOn L' D' v := by
      constructor <;> intro hmax w hw
      · have hw' : w ∈ D := he.symm ▸ hw
        have hh : L w ≤ L v := hmax hw'
        change L' w ≤ L' v
        simpa only [h v hv, h w hw'] using hh
      · have hw' : w ∈ D' := he ▸ hw
        change L w ≤ L v
        rw [h v hv, h w hw]
        exact hmax hw'
    have hg := (Endpoint3GeneralizedBetaHeldPower.criterion hz hle hmax ha one_pos).1
    constructor
    · rintro ⟨v, hv, hmax⟩
      have hv' : v ∈ D' := he ▸ hv
      exact hg.mp ⟨v, hv', (hm v hv).mp hmax⟩
    · intro hf
      obtain ⟨v, hv, hmax⟩ := hg.mpr hf
      have hv' : v ∈ D := he.symm ▸ hv
      exact ⟨v, hv', (hm v hv').mpr hmax⟩
  constructor
  · rintro ⟨hfinite, v, hv, hm⟩
    have hn : ∀ i, 0 ≤ z i := hv.2.2.1.1
    have hz : ∀ i, 0 < z i := by
      intro i
      by_contra! hi
      have hzero : z i = 0 := le_antisymm hi (hn i)
      let W := max Z 0 + 1
      have hW : max Z 0 < W := lt_add_of_pos_right _ one_pos
      have htop := (held_endpoint_fixed_free_shape_failure hle hmax ha).2.1
        hn ⟨i, hzero⟩ W hW
      exact ((hfinite (1 / 2) (by norm_num) W hW).ne htop).elim
    obtain ⟨ha₁, hne⟩ := (hiff hz).mp ⟨v, hv, hm⟩
    exact ⟨hz, hne, ha₁⟩
  · rintro ⟨hz, hne, ha₁⟩
    exact ⟨fun b _ W _ ↦ strict_width_finite hz a b W, (hiff hz).mpr ⟨ha₁, hne⟩⟩

/-- With both endpoints held around a common interior observation,
the fixed-mean two-shape ray has unbounded sample likelihood. -/
theorem held_endpoints_constant_shape_ray (hn : 0 < n) {l r x : ℝ}
    (hl : l < x) (hr : x < r) :
    let u := (x - l) / (r - l)
    Tendsto (fun t ↦ NonstandardBeta.logLik (fun _ : Fin n ↦ x) l r
      (t * u, t * (1 - u))) atTop atTop := by
  intro u
  have hlr := hl.trans hr
  have hu : u ∈ Ioo 0 1 := ⟨div_pos (sub_pos.mpr hl) (sub_pos.mpr hlr),
    (div_lt_one (sub_pos.mpr hlr)).mpr (sub_lt_sub_right hr _)⟩
  have h := Beta.constant_unbounded (n := n) hn hu
  exact (tendsto_atTop_add_const_right _ (-((n : ℝ) * log (r - l))) h).congr fun t ↦
    (NonstandardBeta.logLik_transfer (fun _ : Fin n ↦ x) hlr (t * u, t * (1 - u))).symm

/-- With both endpoints free, holding either positive shape still
leaves a subunit shape available at the other endpoint. It gives an
unbounded likelihood on every nonempty sample. -/
theorem one_shape_fixed_failure (hn : 0 < n) (x : Fin n → ℝ) (b : ℝ) :
    let D : Set (ℝ × ℝ) := {p | p.1 < p.2 ∧ ∀ i, x i ∈ Ioo p.1 p.2}
    (¬ BddAbove ((fun p : ℝ × ℝ ↦ NonstandardBeta.logLik x p.1 p.2 (b, 1 / 2)) '' D)) ∧
      (¬ BddAbove ((fun p : ℝ × ℝ ↦ NonstandardBeta.logLik x p.1 p.2 (1 / 2, b)) '' D)) := by
  exact ⟨fixed_shapes_failure hn x (Or.inl (Or.inr (by norm_num))),
    fixed_shapes_failure hn x (Or.inl (Or.inl (by norm_num)))⟩

/-- If the moving-end shape is free, one subunit choice suffices.
Negative distances give zero likelihood, a tied held endpoint has
the zero, finite, or infinite factor dictated by its shape, and
positive held-end factors permit an unbounded moving-end likelihood. -/
theorem free_end_shape_failure (hn : 0 < n) {z : Fin n → ℝ} {Z : ℝ}
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) :
    let F := fun b W ↦ Endpoint2NonstandardBetaContacts.strictWidthLikelihood z (1 / 2) b W
    ((∃ i, z i < 0) → ∀ b W, F b W = 0) ∧
    ((∀ i, 0 ≤ z i) → (∃ i, z i = 0) → ∀ b W, max Z 0 < W →
      (b < 1 → F b W = ⊤) ∧ (1 < b → F b W = 0) ∧
        (b = 1 → 0 < F b W ∧ F b W < ⊤)) ∧
    (∀ b, Endpoint2NonstandardBeta.heldCompatible z b →
      ¬ BddAbove (Endpoint2NonstandardBeta.widthLogLik z (1 / 2) b ''
        Endpoint2NonstandardBeta.finiteWidthDomain z (1 / 2) b)) := by
  intro F
  have hf (b : ℝ) := fixed_width_shapes_failure hn hle hmax (a := 1 / 2) (b := b)
  refine ⟨fun h b W ↦ (hf b).1 h W,
    fun hnzc hc b W hW ↦ (hf b).2.1 hnzc hc W hW, ?_⟩
  intro b hz
  obtain ⟨j, hj⟩ := hmax
  have hZ : 0 ≤ Z := hj ▸ hz.1 j
  apply (hf b).2.2 hz
  rcases eq_or_lt_of_le hZ with he | he
  · exact Or.inr he.symm
  · exact Or.inl ⟨by norm_num, he⟩

end UnivariateML.Endpoint3NonstandardBeta

end
