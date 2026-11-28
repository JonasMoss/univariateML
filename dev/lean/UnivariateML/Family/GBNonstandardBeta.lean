module

public import UnivariateML.Family.Base9NonstandardBeta
public import UnivariateML.Family.GBHeldPower

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Nonstandard beta with one endpoint and the opposite shape held

The sample coordinates are inward distances, so the width and remaining
shape are the same in the forward and reflected formulations.

The held-power generalized-beta likelihood supplies the regular
uniqueness result after setting its power to one.
-/

@[expose] public section

open Set Real

namespace UnivariateML.GBNonstandardBeta
open Endpoint2NonstandardBeta

noncomputable section

variable {n : ℕ} {z : Fin n → ℝ} {a Z : ℝ}

private theorem width_shape_difference (z : Fin n → ℝ) (W a b : ℝ) :
    widthLogLik z a b W =
      (∑ i, ((b - 1) * log (z i / W) +
        (a - 1) * log ((W - z i) / W) - log W)) -
        (n : ℝ) * Beta.logBeta (b, a) := by
  simp only [widthLogLik, NonstandardBeta.logLik, sub_zero]
  simp_rw [show ∀ u v w c : ℝ, u + v - w - c = u + v - c - w by intros; ring]
  simp [Finset.sum_sub_distrib, nsmul_eq_mul]

/-- The conditional likelihood in the held-end shape is strictly concave. -/
theorem width_shape_strictConcave (hn : 0 < n) (z : Fin n → ℝ)
    {a : ℝ} (ha : 0 < a) (W : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (fun b ↦ widthLogLik z a b W) := by
  refine ⟨convex_Ioi _, ?_⟩
  intro x hx y hy hxy s t hs ht hst
  have h := Beta.logBeta_strictConvex.2
    (show (x, a) ∈ Beta.domain from ⟨hx, ha⟩)
    (show (y, a) ∈ Beta.domain from ⟨hy, ha⟩)
    (fun he ↦ hxy (congrArg Prod.fst he)) hs ht hst
  change Beta.logBeta (s * x + t * y, s * a + t * a) <
    s * Beta.logBeta (x, a) + t * Beta.logBeta (y, a) at h
  rw [← add_mul, hst, one_mul] at h
  simp only [smul_eq_mul, width_shape_difference]
  have hterms :
      (∑ i, (((s * x + t * y) - 1) * log (z i / W) +
        (a - 1) * log ((W - z i) / W) - log W)) =
      s * (∑ i, ((x - 1) * log (z i / W) +
        (a - 1) * log ((W - z i) / W) - log W)) +
      t * (∑ i, ((y - 1) * log (z i / W) +
        (a - 1) * log ((W - z i) / W) - log W)) := by
    simp only [Finset.mul_sum, ← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro i _
    calc
      _ = (s * x + t * y - (s + t)) * log (z i / W) +
        (s + t) * ((a - 1) * log ((W - z i) / W) - log W) := by rw [hst]; ring
      _ = _ := by ring
  rw [hterms]
  nlinarith [mul_lt_mul_of_pos_left h (Nat.cast_pos.mpr hn : (0 : ℝ) < n)]

private theorem unit_width_domain (hz : ∀ i, 0 < z i)
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) (b : ℝ) :
    finiteWidthDomain z 1 b = Ici Z := by
  obtain ⟨j, hj⟩ := hmax
  have hZ : 0 < Z := hj ▸ hz j
  ext W
  constructor
  · intro hW
    exact hj ▸ hW.2.2.1 j
  · intro hW
    exact ⟨hZ.trans_le hW, ⟨fun i ↦ (hz i).le, Or.inr hz⟩,
      fun i ↦ (hle i).trans hW, Or.inl rfl⟩

/-- At unit free-end shape, there is exactly one finite maximizing pair
when the data admit an attained fit. Its width is closed contact. -/
theorem unit_width_unique (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) :
    let D := {v : ℝ × ℝ | 0 < v.1 ∧ v.2 ∈ finiteWidthDomain z 1 v.1}
    let L := fun v : ℝ × ℝ ↦ widthLogLik z 1 v.1 v.2
    let FiniteContact := ∀ b > 0, ∀ W > max Z 0,
      Endpoint2NonstandardBetaContacts.strictWidthLikelihood z 1 b W < ⊤
    (FiniteContact ∧ (∃! v, v ∈ D ∧ IsMaxOn L D v) ∧
      ∀ v ∈ D, IsMaxOn L D v → v.2 = Z) ↔
      (∀ i, 0 < z i) ∧ (∃ i, z i < Z) := by
  intro D L FiniteContact
  have hc := Endpoint3NonstandardBeta.held_endpoint_fixed_free_shape_criterion hle hmax
    (a := 1) one_pos
  constructor
  · rintro ⟨hf, h, _⟩
    obtain ⟨v, hv, _⟩ := h
    exact ⟨(hc.mp ⟨hf, v, hv.1, hv.2⟩).1, (hc.mp ⟨hf, v, hv.1, hv.2⟩).2.1⟩
  · rintro ⟨hz, hne⟩
    obtain ⟨hf, v, hv, hm⟩ := hc.mpr ⟨hz, hne, le_rfl⟩
    obtain ⟨j, hj⟩ := hmax
    have hZ : 0 < Z := hj ▸ hz j
    have hwidth (w : ℝ × ℝ) (hw : w ∈ D) (hmw : IsMaxOn L D w) : w.2 = Z := by
      have hw₀ : 0 < w.1 := hw.1
      have hw' : w.2 ∈ Ici Z := unit_width_domain hz hle ⟨j, hj⟩ w.1 ▸ hw.2
      have h := fixed_shapes_fit (Nat.pos_of_ne_zero (fun he ↦
        Fin.elim0 (he ▸ j))) hZ ⟨fun i ↦ (hz i).le, Or.inr hz⟩ hle
        ⟨j, hj⟩ (a := 1) le_rfl hw₀
      have h' : IsMaxOn (widthLogLik z 1 w.1) (Ici Z) w.2 := by
        intro W hW
        exact hmw (show (w.1, W) ∈ D from
          ⟨hw₀, (unit_width_domain hz hle ⟨j, hj⟩ w.1).symm ▸ hW⟩)
      obtain ⟨u, _, hu⟩ := h.1
      exact (hu _ ⟨by simpa using hw', by simpa using h'⟩).trans
        (hu _ ⟨by simp, h.2.1 rfl⟩).symm
    have hshape (w : ℝ × ℝ) (hw : w ∈ D) (hmw : IsMaxOn L D w)
        (he : w.2 = Z) : IsMaxOn (fun b ↦ widthLogLik z 1 b Z) (Ioi 0) w.1 := by
      intro b hb
      have h := hmw (show (b, Z) ∈ D from
        ⟨hb, (unit_width_domain hz hle ⟨j, hj⟩ b).symm ▸ (show Z ∈ Ici Z from le_rfl)⟩)
      change widthLogLik z 1 b Z ≤ widthLogLik z 1 w.1 w.2 at h
      rwa [he] at h
    have hvZ := hwidth v hv hm
    refine ⟨hf, ⟨v, ⟨hv, hm⟩, ?_⟩, hwidth⟩
    rintro w ⟨hw, hmw⟩
    have hwZ := hwidth w hw hmw
    apply Prod.ext _ (hwZ.trans hvZ.symm)
    exact (width_shape_strictConcave (Nat.pos_of_ne_zero (fun he ↦
      Fin.elim0 (he ▸ j))) z one_pos Z).eq_of_isMaxOn
      (hshape w hw hmw hwZ) (hshape v hv hm hvZ) hw.1 hv.1

private theorem width_to_genbeta (hz : ∀ i, 0 < z i) {a b W : ℝ} (hW : 0 < W) :
    widthLogLik z a b W = Endpoint2GeneralizedBeta.logLik z W b a 1 := by
  unfold widthLogLik NonstandardBeta.logLik Endpoint2GeneralizedBeta.logLik
  apply Finset.sum_congr rfl
  intro i _
  have hr : (W - z i) / W = 1 - z i / W := by rw [sub_div, div_self hW.ne']
  simp only [sub_zero, Endpoint2GeneralizedBeta.unitData, rpow_one, log_one, zero_sub, hr]
  rw [log_div (hz i).ne' hW.ne']
  ring

private theorem width_domain (hz : ∀ i, 0 < z i)
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) (a b : ℝ) :
    finiteWidthDomain z a b = if a = 1 then Ici Z else Ioi Z := by
  by_cases he : a = 1
  · simpa [he] using unit_width_domain hz hle hmax b
  obtain ⟨j, hj⟩ := hmax
  have hZ : 0 < Z := hj ▸ hz j
  rw [ite_eq_right he]
  ext W
  constructor
  · intro h
    rcases h.2.2.2 with h' | h'
    · exact (he h').elim
    · exact hj ▸ h' j
  · intro h
    exact ⟨hZ.trans h, ⟨fun i ↦ (hz i).le, Or.inr hz⟩,
      fun i ↦ (hle i).trans h.le, Or.inr (fun i ↦ (hle i).trans_lt h)⟩

/-- In either orientation, there is a unique finite width and remaining
shape exactly on the attained branch. This includes closed unit-shape
contact and excludes incompatible, held-contact and constant samples. -/
theorem held_endpoint_fixed_free_shape_unique (hle : ∀ i, z i ≤ Z)
    (hmax : ∃ i, z i = Z) (ha : 0 < a) :
    let D := {v : ℝ × ℝ | 0 < v.1 ∧ v.2 ∈ finiteWidthDomain z a v.1}
    let L := fun v : ℝ × ℝ ↦ widthLogLik z a v.1 v.2
    let FiniteContact := ∀ b > 0, ∀ W > max Z 0,
      Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a b W < ⊤
    (FiniteContact ∧ ∃! v, v ∈ D ∧ IsMaxOn L D v) ↔
      (∀ i, 0 < z i) ∧ (∃ i, z i < Z) ∧ 1 ≤ a := by
  intro D L FiniteContact
  have hcrit := Endpoint3NonstandardBeta.held_endpoint_fixed_free_shape_criterion hle hmax ha
  constructor
  · rintro ⟨hf, v, hv, _⟩
    exact hcrit.mp ⟨hf, v, hv.1, hv.2⟩
  · intro h
    obtain ⟨hf, v, hv, hm⟩ := hcrit.mpr h
    have hn : 0 < n := by obtain ⟨j, _⟩ := hmax; exact Nat.zero_lt_of_lt j.isLt
    let D' := Ioi (0 : ℝ) ×ˢ (if a = 1 then Ici Z else Ioi Z)
    let L' := fun v : ℝ × ℝ ↦ Endpoint2GeneralizedBeta.logLik z v.2 v.1 a 1
    have hD : D = D' := by
      ext w
      change (0 < w.1 ∧ w.2 ∈ finiteWidthDomain z a w.1) ↔ w ∈ D'
      rw [width_domain h.1 hle hmax]
      rfl
    have heq (w : ℝ × ℝ) (hw : w ∈ D) : L w = L' w :=
      width_to_genbeta h.1 hw.2.1
    have htransfer (w : ℝ × ℝ) (hw : w ∈ D) (hm : IsMaxOn L D w) :
        w ∈ D' ∧ IsMaxOn L' D' w := by
      refine ⟨hD ▸ hw, ?_⟩
      intro t ht
      have ht' : t ∈ D := hD.symm ▸ ht
      have hh : L t ≤ L w := hm ht'
      change L' t ≤ L' w
      simpa only [heq t ht', heq w hw] using hh
    have H := (GBHeldPower.held_power_unique hn h.1 hle hmax ha one_pos).1.mp
      ⟨v, htransfer v hv hm⟩
    obtain ⟨u, _, hu⟩ := H
    refine ⟨hf, v, ⟨hv, hm⟩, ?_⟩
    rintro w ⟨hw, hmw⟩
    exact (hu w (htransfer w hw hmw)).trans (hu v (htransfer v hv hm)).symm

end

end UnivariateML.GBNonstandardBeta

end
