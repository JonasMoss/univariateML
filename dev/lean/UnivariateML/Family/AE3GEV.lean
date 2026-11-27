module

public import UnivariateML.Family.GEV
public import UnivariateML.Generic.UniqueAE
import Mathlib.Tactic

set_option linter.style.header false

/-!
Reverse-exponential submodel of GEV, including endpoint contact.

## Deviations

These statements optimize the reverse-exponential branch alone. Comparisons
with other GEV shapes require an incidence argument that includes moving
endpoint contacts and the Gumbel seam.
-/

@[expose] public section

open Set Real

namespace UnivariateML.AE3GEV

noncomputable section

variable {n : ℕ}

/-- At shape minus one, the right endpoint may equal an observed level. -/
def reverseDomain (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {p | 0 < p.2 ∧ ∀ i, x i ≤ p.1 + p.2}

/-- Weighted GEV log likelihood at shape minus one, including contact. -/
def reverseLikelihood (w : Fin n → ℕ) (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (-log p.2 - 1 + (x i - p.1) / p.2)

/-- The unweighted branch agrees with the existing GEV likelihood even
when the maximum contacts its closed right endpoint. -/
theorem reverseLikelihood_eq_gev (x : Fin n → ℝ) (p : ℝ × ℝ) :
    reverseLikelihood (fun _ ↦ 1) x p = GEV.logLik x (-1) p := by
  unfold reverseLikelihood GEV.logLik
  apply Finset.sum_congr rfl
  intro i _
  simp [GEV.supportArg]
  ring

/-- Sample derivative of the reverse-exponential contact chart. -/
def contactDerivative (w : Fin n → ℕ) (i : Fin n) (s : ℝ) :
    (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ j, ((w j : ℝ) / s) •
    ((ContinuousLinearMap.proj j : (Fin n → ℝ) →L[ℝ] ℝ) - ContinuousLinearMap.proj i)

/-- On the open set where the indexed maximum stays unique, the contact
branch can be expressed with the scale as its free coordinate. This
expression differentiates even though the original domain has a closed
boundary. -/
theorem reverse_contact_hasDeriv (w : Fin n → ℕ) (i : Fin n) (s : ℝ) (x : Fin n → ℝ) :
    HasFDerivAt (fun y ↦ reverseLikelihood w y (y i - s, s)) (contactDerivative w i s) x := by
  unfold reverseLikelihood contactDerivative
  apply HasFDerivAt.fun_sum
  intro j _
  let J : (Fin n → ℝ) →L[ℝ] ℝ := ContinuousLinearMap.proj j
  let I : (Fin n → ℝ) →L[ℝ] ℝ := ContinuousLinearMap.proj i
  have h := ((((J.hasFDerivAt (x := x)).sub (I.hasFDerivAt.sub_const s)).mul_const s⁻¹).const_add
    (-log s - 1)).const_mul (w j : ℝ)
  convert h using 1
  · rfl
  · change ((w j : ℝ) / s) • (J - I) = _
    rw [smul_smul, div_eq_mul_inv]

private theorem reverseLikelihood_eq (w : Fin n → ℕ) (x : Fin n → ℝ) (p : ℝ × ℝ) :
    reverseLikelihood w x p =
      (∑ i, (w i : ℝ)) * (-log p.2 - 1) +
        ((∑ i, (w i : ℝ) * x i) - (∑ i, (w i : ℝ)) * p.1) / p.2 := by
  unfold reverseLikelihood
  calc
    _ = ∑ i, ((w i : ℝ) * (-log p.2 - 1) +
        ((w i : ℝ) * x i - (w i : ℝ) * p.1) / p.2) := by
      apply Finset.sum_congr rfl
      intro i _
      rw [mul_add, ← mul_div_assoc]
      ring
    _ = _ := by rw [Finset.sum_add_distrib, ← Finset.sum_mul, ← Finset.sum_div,
      Finset.sum_sub_distrib, ← Finset.sum_mul]

private theorem weight_pos {w : Fin n → ℕ} (hw : ∀ i, 0 < w i) (i : Fin n) :
    0 < ∑ j, (w j : ℝ) :=
  Finset.sum_pos' (fun _ _ ↦ Nat.cast_nonneg _) ⟨i, Finset.mem_univ _, by exact_mod_cast hw i⟩

private theorem at_contact {w : Fin n → ℕ} {x : Fin n → ℝ} {i : Fin n}
    (hw : ∀ i, 0 < w i) (_hmax : ∀ j, x j ≤ x i)
    {p : ℝ × ℝ} (hp : p ∈ reverseDomain x) :
    reverseLikelihood w x p ≤ reverseLikelihood w x (x i - p.2, p.2) ∧
      (reverseLikelihood w x p = reverseLikelihood w x (x i - p.2, p.2) →
        p.1 = x i - p.2) := by
  rw [reverseLikelihood_eq, reverseLikelihood_eq]
  have hW := weight_pos hw i
  have hloc : x i - p.2 ≤ p.1 := sub_le_iff_le_add.mpr (hp.2 i)
  change _ ≤ _ ∧ (_ = _ → _)
  refine ⟨add_le_add_right ((div_le_div_iff_of_pos_right hp.1).mpr ?_) _, ?_⟩
  · dsimp
    linarith [mul_le_mul_of_nonneg_left hloc hW.le]
  · intro h
    have he := (div_left_inj₀ hp.1.ne').mp (add_left_cancel h)
    dsimp at he
    exact mul_left_cancel₀ hW.ne' (by linarith only [he])

/-- At held positive scale, the reverse-exponential branch has a unique
winner, whose right endpoint is the sample maximum. -/
theorem reverse_held_sigma_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i : Fin n) (hmax : ∀ j, x j ≤ x i) {s : ℝ} (hs : 0 < s) :
    ∃! μ, (μ, s) ∈ reverseDomain x ∧
      IsMaxOn (fun m ↦ reverseLikelihood w x (m, s)) {m | (m, s) ∈ reverseDomain x} μ := by
  have hv : (x i - s, s) ∈ reverseDomain x := ⟨hs, by simpa using hmax⟩
  refine ⟨x i - s, ⟨hv, ?_⟩, ?_⟩
  · intro m hm
    exact (at_contact hw hmax hm).1
  · intro m hm
    have h := at_contact hw hmax hm.1
    exact h.2 (le_antisymm h.1 (hm.2 hv))

private theorem contact_objective_strict {D s : ℝ} (hD : 0 < D) (hs : 0 < s) (hne : s ≠ D) :
    -log s - D / s < -log D - 1 := by
  have hr := div_pos hD hs
  have hn : D / s ≠ 1 := fun h ↦ hne ((div_eq_one_iff_eq hs.ne').mp h).symm
  have h := Real.log_lt_sub_one_of_pos hr hn
  rw [log_div hD.ne' hs.ne'] at h
  linarith

/-- On nonconstant samples, the all-free reverse-exponential submodel
has a unique winner at contact, with scale equal to the weighted mean gap. -/
theorem reverse_all_free_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i j : Fin n) (hmax : ∀ k, x k ≤ x i) (hj : x j < x i) :
    ∃! p, p ∈ reverseDomain x ∧ IsMaxOn (reverseLikelihood w x) (reverseDomain x) p := by
  let W : ℝ := ∑ k, (w k : ℝ)
  let D : ℝ := (∑ k, (w k : ℝ) * (x i - x k)) / W
  have hW : 0 < W := weight_pos hw i
  have hD : 0 < D := div_pos (Finset.sum_pos' (fun k _ ↦ mul_nonneg (by positivity)
    (sub_nonneg.mpr (hmax k))) ⟨j, Finset.mem_univ _, mul_pos
      (by exact_mod_cast hw j) (sub_pos.mpr hj)⟩) hW
  have hobj (s : ℝ) (hs : 0 < s) :
      reverseLikelihood w x (x i - s, s) = W * (-log s - D / s) := by
    rw [reverseLikelihood_eq]
    have hsum : ∑ k, (w k : ℝ) * (x i - x k) = W * x i - ∑ k, (w k : ℝ) * x k := by
      simp only [mul_sub, Finset.sum_sub_distrib, ← Finset.sum_mul, W]
    dsimp only [D, Prod.fst, Prod.snd]
    rw [hsum]
    field_simp [hW.ne', hs.ne']
    ring
  let p : ℝ × ℝ := (x i - D, D)
  have hp : p ∈ reverseDomain x := ⟨hD, by simpa [p] using hmax⟩
  have hle (s : ℝ) (hs : 0 < s) :
      reverseLikelihood w x (x i - s, s) ≤ reverseLikelihood w x p := by
    rcases eq_or_ne s D with rfl | hn
    · rfl
    · rw [hobj s hs, show reverseLikelihood w x p = _ from hobj D hD,
        div_self hD.ne']
      exact (mul_lt_mul_of_pos_left (contact_objective_strict hD hs hn) hW).le
  refine ⟨p, ⟨hp, fun q hq ↦ ((at_contact hw hmax hq).1).trans (hle q.2 hq.1)⟩, ?_⟩
  intro q ⟨hq, hm⟩
  have hscale : q.2 = D := by
    by_contra hne
    have h := (at_contact hw hmax hq).1
    have ht := mul_lt_mul_of_pos_left (contact_objective_strict hD hq.1 hne) hW
    rw [← div_self hD.ne', ← hobj D hD, ← hobj q.2 hq.1] at ht
    exact (h.trans_lt ht).not_ge (hm hp)
  have hloc := (at_contact hw hmax hq).2 (le_antisymm (at_contact hw hmax hq).1
    (by rw [hscale]; exact hm hp))
  exact Prod.ext (by simpa [p, hscale] using hloc) hscale

private theorem contact_objective_anti (D : ℝ) :
    StrictAntiOn (fun s : ℝ ↦ -log s - D / s) (Ioi 0 ∩ Ici D) := by
  apply strictAntiOn_of_deriv_neg ((convex_Ioi _).inter (convex_Ici _))
  · intro s hs
    exact ((continuousAt_log hs.1.ne').neg.sub
      (continuousAt_const.div continuous_id.continuousAt hs.1.ne')).continuousWithinAt
  · intro s hs
    have hs' : 0 < s ∧ D < s := by simpa [interior_inter] using hs
    have hd := ((hasDerivAt_log hs'.1.ne').neg.sub
      ((hasDerivAt_const s D).div (hasDerivAt_id s) hs'.1.ne')).deriv
    change deriv (-log - (fun _ : ℝ ↦ D) / id) s < 0
    rw [hd]
    simp only [id_eq, zero_mul, zero_sub, mul_one, ← one_div]
    have he : -(1 / s) - (-D / s ^ 2) = (D - s) / s ^ 2 := by
      field_simp [hs'.1.ne']
      ring
    rw [he]
    exact div_neg_of_neg_of_pos (sub_neg.mpr hs'.2) (sq_pos_of_pos hs'.1)

/-- At held location, the reverse-exponential fit may be at contact or have
slack. On nonconstant data the weighted branch has exactly one winner. -/
theorem reverse_held_mu_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i j : Fin n) (hmax : ∀ k, x k ≤ x i) (hj : x j < x i) (μ : ℝ) :
    ∃! s, (μ, s) ∈ reverseDomain x ∧
      IsMaxOn (fun s ↦ reverseLikelihood w x (μ, s)) {s | (μ, s) ∈ reverseDomain x} s := by
  let W : ℝ := ∑ k, (w k : ℝ)
  let V : ℝ := ∑ k, (w k : ℝ) * x k
  let D := μ - V / W
  let B := x i - μ
  let s := max B D
  have hW : 0 < W := weight_pos hw i
  have hgap : V < W * x i := by
    unfold V W
    rw [Finset.sum_mul]
    exact Finset.sum_lt_sum
      (fun k _ ↦ mul_le_mul_of_nonneg_left (hmax k) (by positivity))
      ⟨j, Finset.mem_univ _, mul_lt_mul_of_pos_left hj (by exact_mod_cast hw j)⟩
  have hs : 0 < s := by
    by_cases hB : 0 < B
    · exact hB.trans_le (le_max_left _ _)
    · apply lt_of_lt_of_le _ (le_max_right B D)
      have hμ : x i ≤ μ := by dsimp [B] at hB; linarith
      exact sub_pos.mpr (((div_lt_iff₀ hW).mpr (by simpa [mul_comm] using hgap)).trans_le hμ)
  have hmem (t : ℝ) : (μ, t) ∈ reverseDomain x ↔ 0 < t ∧ B ≤ t := by
    refine ⟨fun h ↦ ⟨h.1, ?_⟩, fun h ↦ ⟨h.1, ?_⟩⟩
    · have he := h.2 i
      dsimp [B] at *
      linarith
    · intro k
      have he := h.2
      dsimp [B] at he
      exact (hmax k).trans (by linarith)
  have hobj (t : ℝ) (ht : 0 < t) :
      reverseLikelihood w x (μ, t) = W * (-log t - D / t) - W := by
    rw [reverseLikelihood_eq]
    change W * (-log t - 1) + (V - W * μ) / t = _
    dsimp only [D]
    field_simp [hW.ne', ht.ne']
    ring
  have hlt (t : ℝ) (ht : (μ, t) ∈ reverseDomain x) (hne : t ≠ s) :
      reverseLikelihood w x (μ, t) < reverseLikelihood w x (μ, s) := by
    rw [hobj t ht.1, hobj s hs, sub_lt_sub_iff_right]
    apply mul_lt_mul_of_pos_left _ hW
    by_cases hb : B ≤ D
    · have he : s = D := max_eq_right hb
      rw [he]
      have hD : 0 < D := he ▸ hs
      rw [div_self hD.ne']
      exact contact_objective_strict hD ht.1 (he ▸ hne)
    · have he : s = B := max_eq_left (le_of_not_ge hb)
      have hts : s < t := lt_of_le_of_ne (he.symm ▸ ((hmem t).mp ht).2) hne.symm
      exact contact_objective_anti D ⟨hs, le_max_right _ _⟩
        ⟨ht.1, (le_max_right _ _).trans hts.le⟩ hts
  have hS := (hmem s).mpr ⟨hs, le_max_left _ _⟩
  refine ⟨s, ⟨hS, ?_⟩, ?_⟩
  · intro t ht
    rcases eq_or_ne t s with rfl | hne
    · exact le_rfl
    · exact (hlt t ht hne).le
  · intro t ⟨ht, hm⟩
    by_contra hne
    exact (hlt t ht hne).not_ge (hm hS)

end

end UnivariateML.AE3GEV

end
