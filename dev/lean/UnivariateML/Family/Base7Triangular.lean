module

public import UnivariateML.Family.LocScaleTriangular

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! Held-endpoint compatibility and all-free maximizing knots.

## Deviations
Compatibility is a predicate on positive mathematical likelihood. The
knot theorem leaves the count comparison and higher-stratum nullity open.
-/

@[expose] public section

namespace UnivariateML.Base7Triangular

open Set Triangular LocScaleTriangular

/-- For valid endpoints the mode domain is compact. A positive maximum exists
exactly when every observation is inside the closed interval and the sample
does not occupy both endpoints. Otherwise the supported product is zero. -/
theorem held_endpoint_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a b : ℝ} (hab : a < b) :
    let D := Icc a b
    let L := fun c ↦ supportLikelihood x (a, b, c)
    let compatible := (∀ i, x i ∈ D) ∧ ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b))
    IsCompact D ∧
    ((∃ c ∈ D, IsMaxOn L D c ∧ 0 < L c) ↔ compatible) ∧
    (¬ compatible → ∀ c ∈ D, L c = 0) := by
  classical
  dsimp only
  have he (hx : ∀ i, x i ∈ Icc a b) (c : ℝ) (hc : c ∈ Icc a b) :
      supportLikelihood x (a, b, c) = likelihood a b x c := by
    unfold supportLikelihood
    exact ite_eq_left (show (a, b, c) ∈ jointDomain x from ⟨hab, hc, hx⟩)
  have hz : ¬ ((∀ i, x i ∈ Icc a b) ∧ ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b))) →
      ∀ c ∈ Icc a b, supportLikelihood x (a, b, c) = 0 := by
    intro h c hc
    by_cases hx : ∀ i, x i ∈ Icc a b
    · have ⟨ha, hb⟩ := not_not.mp (fun hn' ↦ h ⟨hx, hn'⟩)
      rw [he hx c hc, likelihood_zero_of_both_endpoints hab ha hb]
    · push Not at hx
      obtain ⟨i, hi⟩ := hx
      exact held_endpoint_incompatible_zero x a b c ⟨i, by
        have hi : x i < a ∨ b < x i := by
          by_contra! h
          exact hi ⟨h.1, h.2⟩
        rcases hi with h | h
        · exact Or.inl h
        · exact Or.inr (Or.inl h)⟩
  refine ⟨isCompact_Icc, ⟨?_, ?_⟩, hz⟩
  · rintro ⟨c, hc, _, hp⟩
    exact not_not.mp (fun h ↦ hp.ne' (hz h c hc))
  · rintro ⟨hx, hsep⟩
    have witness : ∃ c ∈ Icc a b, 0 < likelihood a b x c := by
      by_cases ha : ∃ i, x i = a
      · refine ⟨a, left_mem_Icc.mpr hab.le, (likelihood_pos_iff hab hx
          (left_mem_Icc.mpr hab.le)).mpr ⟨fun _ _ ↦ rfl, ?_⟩⟩
        exact fun i hi ↦ (hsep ⟨ha, i, hi⟩).elim
      · refine ⟨b, right_mem_Icc.mpr hab.le, (likelihood_pos_iff hab hx
          (right_mem_Icc.mpr hab.le)).mpr ⟨?_, fun _ _ ↦ rfl⟩⟩
        exact fun i hi ↦ (ha ⟨i, hi⟩).elim
    obtain ⟨d, hd, hpos⟩ := witness
    obtain ⟨c, hc, hm⟩ := exists_isMaxOn hab hn hx
    refine ⟨c, hc, ?_, ?_⟩
    · intro z hz'
      change supportLikelihood x (a, b, z) ≤ supportLikelihood x (a, b, c)
      rw [he hx z hz', he hx c hc]
      exact hm hz'
    · rw [he hx c hc]
      exact hpos.trans_le (hm hd)

private theorem joint_from_widths {n : ℕ} {x : Fin n → ℝ} {c : ℝ} {w : ℝ × ℝ}
    (hw : w ∈ widthDomain x c) : (c - w.1, c + w.2, c) ∈ jointDomain x :=
  ⟨by linarith [hw.2.2.1], ⟨by linarith [hw.1], by linarith [hw.2.1]⟩, hw.2.2.2⟩

private theorem joint_to_widths {n : ℕ} {x : Fin n → ℝ} {a b c : ℝ}
    (h : (a, b, c) ∈ jointDomain x) : (c - a, b - c) ∈ widthDomain x c := by
  have hw : a < b := h.1
  have hc : c ∈ Icc a b := h.2.1
  refine ⟨sub_nonneg.mpr hc.1, sub_nonneg.mpr hc.2, by linarith, fun i ↦ ?_⟩
  have hx := h.2.2 i
  exact ⟨by linarith [hx.1], by linarith [hx.2]⟩

/-- Constants have no winner. Otherwise each observed mode has exactly
one fitted endpoint pair, and the winning triples are exactly the knot
fits at greatest height. -/
theorem all_free_knot_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) :
    let D := jointDomain x
    let L := jointLikelihood x
    let W := {p ∈ D | IsMaxOn L D p}
    ((∀ i j, x i = x j) → W = ∅) ∧
    ((∃ i j, x i ≠ x j) →
      ∃ w : Fin n → ℝ × ℝ,
        (∀ i, w i ∈ widthDomain x (x i) ∧
          IsMaxOn (widthLikelihood x (x i)) (widthDomain x (x i)) (w i) ∧
          ∀ v ∈ widthDomain x (x i),
            IsMaxOn (widthLikelihood x (x i)) (widthDomain x (x i)) v → v = w i) ∧
        W = {p | ∃ i, p = (x i - (w i).1, x i + (w i).2, x i) ∧
          ∀ j, widthLikelihood x (x j) (w j) ≤ widthLikelihood x (x i) (w i)}) := by
  dsimp only
  classical
  constructor
  · intro hc
    rw [show x = fun _ ↦ x ⟨0, hn⟩ from funext (fun i ↦ hc i ⟨0, hn⟩)]
    apply eq_empty_iff_forall_notMem.mpr
    rintro p ⟨hp, hm⟩
    exact all_free_collapse hn _ ⟨_, (hm.isLUB hp).1⟩
  · intro hnc
    have hsep (i : Fin n) : ∃ j, x j ≠ x i := by
      by_contra! h
      obtain ⟨j, k, hjk⟩ := hnc
      exact hjk ((h j).trans (h k).symm)
    choose w hw hwpos hwm using fun i ↦ held_mode_attains hn x (x i) (hsep i)
    let fit (i : Fin n) := (x i - (w i).1, x i + (w i).2, x i)
    have hf (i : Fin n) : fit i ∈ jointDomain x := joint_from_widths (hw i)
    have hp (p : ℝ × ℝ × ℝ) (hd : p ∈ jointDomain x)
        (hm : IsMaxOn (jointLikelihood x) (jointDomain x) p) : ∃ i, p = fit i := by
      obtain ⟨a, b, c⟩ := p
      have hpos : 0 < likelihood a b x c :=
        (hwpos ⟨0, hn⟩).trans_le (hm (hf ⟨0, hn⟩))
      have hcompat : ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b)) := by
        rintro ⟨ha, hb⟩
        exact hpos.ne' (likelihood_zero_of_both_endpoints hd.1 ha hb c)
      have hm' : IsMaxOn (likelihood a b x) (Icc a b) c :=
        fun q hq ↦ hm (a := (a, b, q)) ⟨hd.1, hq, hd.2.2⟩
      obtain ⟨⟨i, rfl⟩, _⟩ := (isMaxOn_iff_observation hd.1 hn hd.2.2 hcompat hd.2.1).mp hm'
      have hv := joint_to_widths hd
      have he : (x i - (x i - a), x i + (b - x i), x i) = (a, b, x i) := by
        congr 1 <;> simp
      have hval : widthLikelihood x (x i) (x i - a, b - x i) = likelihood a b x (x i) := by
        simp [widthLikelihood]
      have hvmax : IsMaxOn (widthLikelihood x (x i)) (widthDomain x (x i))
          (x i - a, b - x i) := by
        intro v hdom
        change widthLikelihood x (x i) v ≤ _
        rw [hval]
        exact hm (joint_from_widths hdom)
      have heq := held_mode_unique hn x hv (hw i) (hval ▸ hpos) (hwpos i) hvmax (hwm i)
      refine ⟨i, ?_⟩
      dsimp [fit]
      rw [← heq]
      exact he.symm
    refine ⟨w, fun i ↦ ⟨hw i, hwm i, fun v hv hvm ↦
      held_mode_unique hn x hv (hw i) ((hwpos i).trans_le (hvm (hw i))) (hwpos i) hvm (hwm i)⟩, ?_⟩
    ext p
    constructor
    · rintro ⟨hD, hm⟩
      obtain ⟨i, rfl⟩ := hp p hD hm
      exact ⟨i, rfl, fun j ↦ hm (hf j)⟩
    · rintro ⟨i, rfl, hi⟩
      obtain ⟨p, hd, _, hm⟩ := (all_free_criterion hn x).mpr hnc
      obtain ⟨j, rfl⟩ := hp p hd hm
      exact ⟨hf i, fun q hq ↦ (show jointLikelihood x q ≤ jointLikelihood x (fit j) from
        hm hq).trans (hi j)⟩

end UnivariateML.Base7Triangular

end
