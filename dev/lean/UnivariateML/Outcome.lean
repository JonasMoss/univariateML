module

public import UnivariateML.Family.Weibull
public import UnivariateML.Family.Laplace
public import UnivariateML.Family.Lomax
public import UnivariateML.Generic.Limit

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Exact likelihood outcome claims

Estimates use feasible real maximizers. A named limit has an eventually
feasible path along a nontrivial filter, a finite supremum, and strict
nonattainment. Unknown cardinality carries the same optimality claim as
known cardinality.

## Deviations

An exact maximizer may be represented by a rounded coefficient vector in the
library. This module models the exact claim, with rounding and certification
of computed comparisons left to layer D. Finite nonattainment is a
mathematical fact shared by named limits and unnamed nonexistence stops.
Choosing a public stop requires the family's catalogue of recognized models.
-/

@[expose] public section

open Set Filter Topology

namespace UnivariateML.Outcome

/-- The additional cardinality claim carried by an estimate. -/
inductive Uniqueness (Θ : Type*) where
  | unknown
  | singleton
  | multiple (maximizers : Set Θ)

/-- A named limit model and a path witnessing approach to its value. -/
structure LimitPath (Θ : Type*) where
  model : String
  Index : Type
  filter : Filter Index
  point : Index → Θ

set_option linter.dupNamespace false in
/-- The exact mathematical claims carried by certified outcomes. -/
inductive Outcome (Θ : Type*) where
  | estimate (θ : Θ) (unique : Uniqueness Θ)
  | limit (path : LimitPath Θ) (value : ℝ)
  | unattained (value : ℝ)
  | unbounded

variable {Θ Φ : Type*} {L : Θ → ℝ} {D : Set Θ}

/-- The set of feasible global maximizers. -/
def maximizers (L : Θ → ℝ) (D : Set Θ) : Set Θ :=
  {θ ∈ D | IsMaxOn L D θ}

/-- A finite supremum strictly above every feasible value. -/
def FiniteUnattained (L : Θ → ℝ) (D : Set Θ) (v : ℝ) : Prop :=
  IsLUB (L '' D) v ∧ ∀ θ ∈ D, L θ < v

/-- The objective and feasible set satisfy the claims of the outcome. -/
def Sound (L : Θ → ℝ) (D : Set Θ) : Outcome Θ → Prop
  | .estimate θ .unknown => θ ∈ maximizers L D
  | .estimate θ .singleton => θ ∈ maximizers L D ∧ maximizers L D = {θ}
  | .estimate θ (.multiple S) => θ ∈ maximizers L D ∧ S = maximizers L D ∧ S.Nontrivial
  | .limit p v => p.filter.NeBot ∧ (∀ᶠ i in p.filter, p.point i ∈ D) ∧
      Tendsto (fun i ↦ L (p.point i)) p.filter (𝓝 v) ∧ FiniteUnattained L D v
  | .unattained v => FiniteUnattained L D v
  | .unbounded => ¬ BddAbove (L '' D)

private theorem member_of_sound_estimate {θ : Θ} {u : Uniqueness Θ}
    (h : Sound L D (.estimate θ u)) : θ ∈ maximizers L D := by
  cases u
  · exact h
  · exact h.1
  · exact h.1

/-- Any sound estimate remains sound with cardinality left unspecified. -/
theorem estimate_unknown {θ : Θ} {u : Uniqueness Θ}
    (h : Sound L D (.estimate θ u)) : Sound L D (.estimate θ .unknown) :=
  member_of_sound_estimate h

/-- A sound unique estimate determines the full maximizer set. -/
theorem maximizers_eq_singleton {θ : Θ} (h : Sound L D (.estimate θ .singleton)) :
    maximizers L D = {θ} :=
  h.2

/-- Unique existence supplies a sound singleton estimate. -/
private theorem exists_sound_singleton (h : ∃! θ, θ ∈ maximizers L D) :
    ∃ θ, Sound L D (.estimate θ .singleton) := by
  obtain ⟨θ, hθ, hu⟩ := h
  exact ⟨θ, hθ, Set.eq_singleton_iff_unique_mem.mpr ⟨hθ, hu⟩⟩

private theorem no_maximizer_of_finite_unattained {v : ℝ}
    (h : FiniteUnattained L D v) : ¬ (maximizers L D).Nonempty := by
  rintro ⟨θ, hθ, hm⟩
  have hu : L θ ∈ upperBounds (L '' D) := by
    rintro _ ⟨η, hη, rfl⟩
    exact hm hη
  exact (h.2 θ hθ).not_ge (h.1.2 hu)

/-- A limit's named path reaches the finite supremum only in the limit. -/
theorem limit_unattained {p : LimitPath Θ} {v : ℝ} (h : Sound L D (.limit p v)) :
    sSup (L '' D) = v ∧ ¬ (maximizers L D).Nonempty := by
  let : p.filter.NeBot := h.1
  have hD : D.Nonempty := by
    obtain ⟨i, hi⟩ := h.2.1.exists
    exact ⟨p.point i, hi⟩
  exact ⟨h.2.2.2.1.csSup_eq (hD.image L), no_maximizer_of_finite_unattained h.2.2.2⟩

/-- Finite nonattainment and unboundedness both preclude every estimate. -/
theorem estimate_incompatible {θ : Θ} {u : Uniqueness Θ} (h : Sound L D (.estimate θ u)) :
    (∀ p v, ¬ Sound L D (.limit p v)) ∧
    (∀ v, ¬ Sound L D (.unattained v)) ∧ ¬ Sound L D .unbounded := by
  have hm := member_of_sound_estimate h
  have hn : (maximizers L D).Nonempty := ⟨θ, hm⟩
  refine ⟨fun p v hl ↦ (limit_unattained hl).2 hn,
    fun v hu ↦ no_maximizer_of_finite_unattained hu hn, ?_⟩
  intro hu
  apply hu
  refine ⟨L θ, ?_⟩
  rintro _ ⟨η, hη, rfl⟩
  exact hm.2 hη

/-- A finite unattained bound, including a named limit, excludes unboundedness. -/
theorem finite_incompatible_unbounded {v : ℝ} (h : Sound L D (.unattained v)) :
    ¬ Sound L D .unbounded :=
  fun hu ↦ hu ⟨v, h.1.1⟩

/-- A named limit has a finite upper bound and excludes unboundedness. -/
theorem limit_incompatible_unbounded {p : LimitPath Θ} {v : ℝ}
    (h : Sound L D (.limit p v)) : ¬ Sound L D .unbounded :=
  finite_incompatible_unbounded h.2.2.2

/-- A singleton proof excludes a sound multiplicity claim for any representative. -/
theorem singleton_incompatible_multiple {θ η : Θ} {S : Set Θ}
    (h : Sound L D (.estimate θ .singleton)) : ¬ Sound L D (.estimate η (.multiple S)) := by
  intro hm
  obtain ⟨a, ha, b, hb, hab⟩ := hm.2.2
  rw [hm.2.1, maximizers_eq_singleton h, mem_singleton_iff] at ha hb
  exact hab (ha.trans hb.symm)

private theorem finite_of_path {ι : Type*} {l : Filter ι} [l.NeBot] {p : ι → Θ} {v : ℝ}
    (hp : ∀ᶠ i in l, p i ∈ D)
    (ht : Tendsto (fun i ↦ L (p i)) l (𝓝 v))
    (hlt : ∀ θ ∈ D, L θ < v) : FiniteUnattained L D v := by
  refine ⟨⟨?_, fun _ hu ↦ Generic.limit_le_upperBound hp ht hu⟩, hlt⟩
  rintro _ ⟨θ, hθ, rfl⟩
  exact (hlt θ hθ).le

/-- Pull an outcome back along a parameter equivalence and shift its finite values. -/
def transfer (φ : Φ ≃ Θ) (c : ℝ) : Outcome Θ → Outcome Φ
  | .estimate θ .unknown => .estimate (φ.symm θ) .unknown
  | .estimate θ .singleton => .estimate (φ.symm θ) .singleton
  | .estimate θ (.multiple S) => .estimate (φ.symm θ) (.multiple (φ ⁻¹' S))
  | .limit p v => .limit { p with point := fun i ↦ φ.symm (p.point i) } (v + c)
  | .unattained v => .unattained (v + c)
  | .unbounded => .unbounded

/-- Exact equality up to an additive constant on the corresponding feasible domains
preserves soundness, including full maximizer sets and eventually feasible paths. -/
theorem sound_transfer {L₂ : Φ → ℝ} (φ : Φ ≃ Θ) (c : ℝ)
    (he : ∀ q ∈ φ ⁻¹' D, L₂ q = L (φ q) + c)
    {o : Outcome Θ} (h : Sound L D o) :
    Sound L₂ (φ ⁻¹' D) (transfer φ c o) := by
  have hmem (q : Φ) : q ∈ maximizers L₂ (φ ⁻¹' D) ↔ φ q ∈ maximizers L D := by
    constructor
    · rintro ⟨hq, hm⟩
      refine ⟨hq, fun θ hθ ↦ ?_⟩
      have hq' : φ.symm θ ∈ φ ⁻¹' D := by simpa using hθ
      have hh : L₂ (φ.symm θ) ≤ L₂ q := hm hq'
      rw [he _ hq', he _ hq, φ.apply_symm_apply] at hh
      exact le_of_add_le_add_right hh
    · rintro ⟨hq, hm⟩
      refine ⟨hq, fun q' hq' ↦ ?_⟩
      change L₂ q' ≤ L₂ q
      rw [he _ hq', he _ hq]
      exact add_le_add_left (hm hq') c
  let shift : ℝ ≃o ℝ := OrderIso.addRight c
  have himage : L₂ '' (φ ⁻¹' D) = shift '' (L '' D) := by
    ext v
    constructor
    · rintro ⟨q, hq, rfl⟩
      exact ⟨L (φ q), mem_image_of_mem L hq, (he _ hq).symm⟩
    · rintro ⟨_, ⟨θ, hθ, rfl⟩, rfl⟩
      have hq : φ.symm θ ∈ φ ⁻¹' D := by simpa using hθ
      exact ⟨φ.symm θ, hq, by rw [he _ hq, φ.apply_symm_apply]; rfl⟩
  have hfinite {v : ℝ} (hv : FiniteUnattained L D v) :
      FiniteUnattained L₂ (φ ⁻¹' D) (v + c) := by
    constructor
    · change IsLUB (L₂ '' (φ ⁻¹' D)) (shift v)
      rw [himage]
      exact shift.isLUB_image'.mpr hv.1
    · intro q hq
      rw [he _ hq]
      exact add_lt_add_left (hv.2 (φ q) hq) c
  cases o with
  | estimate θ u =>
    have hθ := (hmem (φ.symm θ)).mpr (by simpa using member_of_sound_estimate h)
    cases u with
    | unknown => exact hθ
    | singleton =>
      refine ⟨hθ, ?_⟩
      ext q
      rw [hmem, maximizers_eq_singleton h, mem_singleton_iff, mem_singleton_iff]
      exact φ.eq_symm_apply.symm
    | multiple S =>
      refine ⟨hθ, ?_, ?_⟩
      · ext q
        change φ q ∈ S ↔ q ∈ maximizers L₂ (φ ⁻¹' D)
        rw [hmem, h.2.1]
      · obtain ⟨a, ha, b, hb, hab⟩ := h.2.2
        exact ⟨φ.symm a, by simpa using ha, φ.symm b, by simpa using hb,
          fun e ↦ hab (φ.symm.injective e)⟩
  | limit p v =>
    have hp : ∀ᶠ i in p.filter, φ.symm (p.point i) ∈ φ ⁻¹' D :=
      h.2.1.mono fun i hi ↦ by simpa using hi
    refine ⟨h.1, hp, ?_, hfinite h.2.2.2⟩
    exact (h.2.2.1.add tendsto_const_nhds).congr' (hp.mono fun i hi ↦ by
      change L (p.point i) + c = L₂ (φ.symm (p.point i))
      rw [he _ hi, φ.apply_symm_apply])
  | unattained v => exact hfinite h
  | unbounded =>
    change ¬ BddAbove (L₂ '' (φ ⁻¹' D))
    rw [himage, shift.bddAbove_image]
    exact h

variable {n : ℕ}

/-- Nonconstant positive Weibull samples give a sound unique estimate. -/
theorem weibull_sound (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃ p, Sound (fun q : ℝ × ℝ ↦ Weibull.logLik x q.1 q.2) Weibull.domain
      (.estimate p .singleton) :=
  exists_sound_singleton (Weibull.jointMLE hn hx hne)

/-- Distinct central values give an entire segment of joint Laplace maximizers. -/
theorem laplace_sound_multiple (hn : 0 < n) {x : Fin n → ℝ}
    (h : Laplace.lowerMedian x hn < Laplace.upperMedian x hn) :
    let d := Laplace.meanAbsDev x (Laplace.lowerMedian x hn)
    Sound (fun q : ℝ × ℝ ↦ Laplace.logLik x q.1 q.2) Laplace.domain
      (.estimate (Laplace.upperMedian x hn, d) (.multiple (Laplace.medianSet x hn ×ˢ {d}))) := by
  dsimp
  let d := Laplace.meanAbsDev x (Laplace.lowerMedian x hn)
  have hd : 0 < d := by
    apply lt_of_le_of_ne
    · unfold d Laplace.meanAbsDev
      positivity
    · intro hz
      obtain ⟨c, hc⟩ := (Laplace.minMeanAbsDev_zero_iff hn x).mp hz.symm
      simp [Laplace.lowerMedian, Laplace.upperMedian, Laplace.orderStat, hc] at h
  have he : Laplace.medianSet x hn ×ˢ {d} =
      maximizers (fun q : ℝ × ℝ ↦ Laplace.logLik x q.1 q.2) Laplace.domain :=
    (Laplace.joint hn hd).symm
  have hlo : (Laplace.lowerMedian x hn, d) ∈ Laplace.medianSet x hn ×ˢ {d} :=
    ⟨⟨le_rfl, h.le⟩, mem_singleton _⟩
  have hhi : (Laplace.upperMedian x hn, d) ∈ Laplace.medianSet x hn ×ˢ {d} :=
    ⟨⟨h.le, le_rfl⟩, mem_singleton _⟩
  exact ⟨he ▸ hhi, he, ⟨_, hlo, _, hhi, fun e ↦ h.ne (congrArg Prod.fst e)⟩⟩

/-- Zero minimum deviation in the joint Laplace fit gives an unbounded outcome. -/
theorem laplace_sound_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    (hd : Laplace.meanAbsDev x (Laplace.lowerMedian x hn) = 0) :
    Sound (fun q : ℝ × ℝ ↦ Laplace.logLik x q.1 q.2) Laplace.domain .unbounded := by
  apply not_bddAbove_iff.mpr
  intro M
  obtain ⟨p, hp, hlt⟩ := Laplace.joint_unbounded hn hd M
  exact ⟨_, mem_image_of_mem _ hp, hlt⟩

/-- Without a Lomax sign witness, the fixed-product path approaches the best exponential value. -/
theorem lomax_sound_limit (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar x), Lomax.psi x u ≤ 0) :
    Sound (fun q : ℝ × ℝ ↦ Lomax.logLik x q.1 q.2) Lomax.domain
      (.limit ⟨"exp", ℝ, atTop, fun κ ↦ (κ, (Lomax.avg x)⁻¹ / κ)⟩
        (Lomax.bestExp x)) := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  have hr : 0 < (Lomax.avg x)⁻¹ :=
    inv_pos.mpr (div_pos (Finset.sum_pos (fun i _ ↦ hx i) Finset.univ_nonempty)
      (by exact_mod_cast hn))
  have hp : ∀ᶠ κ : ℝ in atTop, (κ, (Lomax.avg x)⁻¹ / κ) ∈ Lomax.domain :=
    (eventually_gt_atTop 0).mono fun _ hk ↦ ⟨hk, div_pos hr hk⟩
  have ht := Lomax.logLik_tendsto x (fun i ↦ (hx i).le) hr
  exact ⟨atTop_neBot, hp, ht,
    finite_of_path hp ht (Lomax.exponential_sup_of_no_witness hn hx h).2⟩

end UnivariateML.Outcome
