module

public import UnivariateML.Family.Shape2GEV
public import UnivariateML.Family.JoinsGEV

set_option linter.style.header false

/-! # GEV at a held location with at least half contact

The majority and exact-half branches include the closed reverse-exponential
member of the fitted shape interval. The theorem concerns the held-location
failure row. Attainment in the negative-residual and strict-sub-half regimes
requires separate bounds.
-/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.Shape3GEV
noncomputable section

/-- For a held location at or below the minimum, majority contact is
unbounded. Exact half contact has a sharp finite supremum, attained
precisely when all strictly positive residuals are equal. -/
theorem held_location_contact_classification {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {m : ℝ} (hm : ∀ i, m ≤ x i) :
    let c := (Finset.univ.filter (fun i ↦ x i = m)).card
    let D : Set (ℝ × ℝ) := {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ (m, q.2) ∈ GEV.domain x q.1}
    let L := fun q : ℝ × ℝ ↦ GEV.logLik x q.1 (m, q.2)
    (n < 2 * c → ¬ BddAbove (L '' D)) ∧
    (n = 2 * c → IsLUB (L '' D) (Shape2GEV.halfValue x m) ∧
      ((∃ q ∈ D, IsMaxOn L D q) ↔ ∃ s > 0, ∀ i, m < x i → x i - m = s)) := by
  intro c D L
  constructor
  · intro hc
    obtain ⟨hD, hlim⟩ := HeldSweepGEV.held_minimum_majority_collapse x hm hc
    rintro ⟨b, hb⟩
    obtain ⟨s, hs, hb'⟩ := (eventually_mem_nhdsWithin.and
      (hlim.eventually (eventually_gt_atTop b))).exists
    have hp : (1, s) ∈ D := ⟨by norm_num, hD s hs⟩
    exact hb'.not_ge (hb ⟨(1, s), hp, rfl⟩)
  · exact Shape2GEV.held_minimum_half_classification hn hm

/-- At positive held scale, competitive positive superlevels are compact,
and the reverse-exponential endpoint has its own attainable location
optimum as a member of the fitted model. -/
theorem held_scale_compact_and_closed_endpoint {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    let D := {p : ℝ × ℝ | p.2 ∈ Icc (-1 : ℝ) 1 ∧ (p.1, s) ∈ GEV.domain x p.2}
    (∀ e > 0, IsCompact (((univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1) ∩
      JoinsGEV.heldScaleLikelihood x s ⁻¹' Ici e)) ∧
    (∃! m, (m, s) ∈ GEV.domain x (-1) ∧
      IsMaxOn (fun u ↦ GEV.logLik x (-1) (u, s)) {u | (u, s) ∈ GEV.domain x (-1)} m) ∧
    (∃ p ∈ D, IsMaxOn (fun q ↦ GEV.logLik x q.2 (q.1, s)) D p) := by
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ x ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  exact ⟨fun _ he ↦ JoinsGEV.held_scale_compact hn x hs he,
    HeldSweepGEV.reverse_held_scale hn j (fun i ↦ hj i (Finset.mem_univ i)) hs,
    JoinsGEV.held_scale_attainment hn x hs⟩

end
end UnivariateML.Shape3GEV
end
