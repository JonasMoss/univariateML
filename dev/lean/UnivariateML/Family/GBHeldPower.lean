module

public import UnivariateML.Family.GBEndpointTrace
public import UnivariateML.Family.Endpoint3GeneralizedBetaHeldPower
public import UnivariateML.Generic.GBBetaCurvature

import Mathlib.Tactic

set_option linter.style.header false

/-! # Joint first-shape and endpoint uniqueness at held power -/

@[expose] public section

open Real Set
open scoped Topology

namespace UnivariateML.GBHeldPower

open Endpoint2GeneralizedBeta GBEndpointTrace GBBetaCurvature

noncomputable section

variable {n : ℕ} {x : Fin n → ℝ} {M q a : ℝ}

private theorem chart_value (hx : ∀ i, 0 < x i) (hM : 0 < M)
    {v : ℝ × ℝ} (hb : 0 < v.2) :
    logLik x v.2 v.1 q a =
      endpointObjective (fun i ↦ a * log (M / x i)) v.1 q 1 (a * log (v.2 / M)) +
        ((n : ℝ) * log a - ∑ i, log (x i)) := by
  rw [endpoint_objective_eq hx hM hb]
  have h : endpointObjective (fun i ↦ log (M / x i)) v.1 q a (log (v.2 / M)) =
      endpointObjective (fun i ↦ a * log (M / x i)) v.1 q 1 (a * log (v.2 / M)) +
        ∑ _ : Fin n, log a := by
    unfold endpointObjective
    rw [← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro i _
    rw [← mul_add]
    simp only [log_one, zero_sub, mul_one, one_mul]
    ring
  rw [h]
  simp [nsmul_eq_mul, add_sub_assoc]

private theorem endpoint_profile_eq (d : Fin n → ℝ) (q : ℝ) (v : ℝ × ℝ) :
    endpointObjective d v.1 q 1 v.2 = endpointProfile d q v := by
  simp only [endpointObjective, endpointProfile, log_one, zero_sub, mul_one, one_mul,
    Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_neg_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum]
  ring_nf

/-- For fixed positive upper shape and power, every attained finite
fit is unique. At upper shape one it is at closed endpoint contact. -/
theorem held_power_unique (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hle : ∀ i, x i ≤ M) (hmax : ∃ i, x i = M) (hq : 0 < q) (ha : 0 < a) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ logLik x v.2 v.1 q a
    ((∃ v ∈ D, IsMaxOn L D v) ↔ (∃! v, v ∈ D ∧ IsMaxOn L D v)) ∧
      (q = 1 → ∀ v ∈ D, IsMaxOn L D v →
        v = ((n : ℝ) / (a * ∑ i, log (M / x i)), M)) := by
  intro D L
  let d := fun i ↦ a * log (M / x i)
  let D' : Set (ℝ × ℝ) := Ioi 0 ×ˢ (if q = 1 then Ici 0 else Ioi 0)
  let F := fun v : ℝ × ℝ ↦ endpointObjective d v.1 q 1 v.2
  let toChart := fun v : ℝ × ℝ ↦ (v.1, a * log (v.2 / M))
  let fromChart := fun v : ℝ × ℝ ↦ (v.1, M * exp (v.2 / a))
  have hM : 0 < M := (hx ⟨0, hn⟩).trans_le (hle ⟨0, hn⟩)
  have hd (i : Fin n) : 0 ≤ d i := mul_nonneg ha.le (log_nonneg ((one_le_div (hx i)).mpr (hle i)))
  have hb (v : ℝ × ℝ) (hv : v ∈ D) : M ≤ v.2 := by
    by_cases he : q = 1
    · simpa [D, he] using hv.2
    · exact (show M < v.2 by simpa [D, he] using hv.2).le
  have hf (v : ℝ × ℝ) (hv : v ∈ D) : toChart v ∈ D' := by
    refine ⟨hv.1, ?_⟩
    by_cases he : q = 1
    · simpa [D', toChart, he] using mul_nonneg ha.le
        (log_nonneg ((one_le_div hM).mpr (hb v hv)))
    · have hb' : M < v.2 := by simpa [D, he] using hv.2
      simpa [D', toChart, he] using mul_pos ha (log_pos ((one_lt_div hM).mpr hb'))
  have hg (v : ℝ × ℝ) (hv : v ∈ D') : fromChart v ∈ D := by
    refine ⟨hv.1, ?_⟩
    by_cases he : q = 1
    · have hv₂ : 0 ≤ v.2 := by simpa [D', he] using hv.2
      have H := mul_le_mul_of_nonneg_left (one_le_exp_iff.mpr (div_nonneg hv₂ ha.le)) hM.le
      simpa [D, fromChart, he] using H
    · have hv₂ : 0 < v.2 := by simpa [D', he] using hv.2
      have H := mul_lt_mul_of_pos_left (one_lt_exp_iff.mpr (div_pos hv₂ ha)) hM
      simpa [D, fromChart, he] using H
  have hfg (v : ℝ × ℝ) : toChart (fromChart v) = v := by
    refine Prod.ext rfl ?_
    simp [toChart, fromChart, mul_div_cancel_left₀ _ hM.ne', mul_div_cancel₀ _ ha.ne']
  have hgf (v : ℝ × ℝ) (hv : v ∈ D) : fromChart (toChart v) = v := by
    refine Prod.ext rfl ?_
    simp [toChart, fromChart, mul_div_cancel_left₀ _ ha.ne',
      exp_log (div_pos (hM.trans_le (hb v hv)) hM), mul_div_cancel₀ _ hM.ne']
  have hval (v : ℝ × ℝ) (hv : v ∈ D) :
      L v = F (toChart v) + ((n : ℝ) * log a - ∑ i, log (x i)) :=
    chart_value hx hM (hM.trans_le (hb v hv))
  have hmChart (v : ℝ × ℝ) (hv : v ∈ D) (hm : IsMaxOn L D v) :
      IsMaxOn F D' (toChart v) := by
    intro w hw
    have H := hm (hg w hw)
    change L (fromChart w) ≤ L v at H
    rw [hval (fromChart w) (hg w hw), hval v hv, hfg] at H
    exact (add_le_add_iff_right _).mp H
  have hcriterion := (Endpoint3GeneralizedBetaHeldPower.criterion hx hle hmax hq ha).1
  have hpos (v : ℝ × ℝ) (hv : v ∈ D) (hm : IsMaxOn L D v) : ∃ i, 0 < d i := by
    obtain ⟨_, j, hj⟩ := hcriterion.mp ⟨v, hv, hm⟩
    exact ⟨j, mul_pos ha (log_pos ((one_lt_div (hx j)).mpr hj))⟩
  have hunit {v : ℝ × ℝ} (hv : v ∈ D) (hm : IsMaxOn L D v) (he : q = 1) :
      toChart v = ((n : ℝ) / (∑ i, d i), 0) := by
    have h := GBEndpointTrace.held_power_unit_endpoint_unique hn hd (hpos v hv hm) one_pos
    have hc := hmChart v hv hm
    have hmem := hf v hv
    simp only [D', F, he, ite_true] at hc hmem
    simpa using (h _ hmem).mp hc
  constructor
  · constructor
    · rintro ⟨v, hv, hm⟩
      refine ⟨v, ⟨hv, hm⟩, ?_⟩
      rintro w ⟨hw, hmw⟩
      have hq' := (hcriterion.mp ⟨v, hv, hm⟩).1
      have heq : toChart w = toChart v := by
        rcases eq_or_lt_of_le hq' with he | hlt
        · exact (hunit hw hmw he.symm).trans (hunit hv hm he.symm).symm
        · have hv' := hf v hv
          have hw' := hf w hw
          have hm' := hmChart v hv hm
          have hmw' := hmChart w hw hmw
          simp only [D', ite_eq_right (ne_of_gt hlt)] at hv' hw' hm' hmw'
          simp only [F, endpoint_profile_eq] at hm' hmw'
          exact endpoint_profile_at_most_one hn hd hlt _ hw' _ hv' hmw' hm'
      exact (hgf w hw).symm.trans ((congrArg fromChart heq).trans (hgf v hv))
    · rintro ⟨v, hv, _⟩
      exact ⟨v, hv⟩
  · intro he v hv hm
    have h := congrArg fromChart (hunit hv hm he)
    rw [hgf v hv] at h
    simpa [fromChart, d, Finset.mul_sum] using h

end

end UnivariateML.GBHeldPower

end
