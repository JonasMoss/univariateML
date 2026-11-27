module

public import UnivariateML.Family.JoinsDNorm

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Discretised normal limiting laws and held-fit uniqueness

The limiting laws are stated for the Gaussian CDF cell probabilities. The
uniqueness proofs use the normalized working product in the affine
coordinates `(mean / sd, 1 / sd)`.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators

namespace UnivariateML.Discrete4DNorm

open Discrete3DNorm HeldSweepDNorm JoinsDNorm SkewLSGaussian

/-- The cell mass in the location and inverse-scale coordinates. -/
noncomputable def cell (k m B : ℝ) : ℝ :=
  Φ ((k+1-m)*B)-Φ ((k-m)*B)

/-- At fixed location strictly inside cell `j`, shrinking scale
concentrates at `j`, with every other cell probability tending to zero. -/
theorem point_path (j k : ℤ) {m : ℝ} (h₁ : (j : ℝ) < m) (h₂ : m < (j : ℝ)+1) :
    Tendsto (cell k m) atTop (𝓝 (if k = j then 1 else 0)) := by
  unfold cell
  have ht (a : ℝ) (ha : 0 < a) : Tendsto (fun B : ℝ ↦ Φ (a*B)) atTop (𝓝 1) :=
    cdf_atTop.comp (tendsto_id.const_mul_atTop ha)
  have hb (a : ℝ) (ha : a < 0) : Tendsto (fun B : ℝ ↦ Φ (a*B)) atTop (𝓝 0) :=
    cdf_atBot.comp (tendsto_id.const_mul_atTop_of_neg ha)
  rcases lt_trichotomy k j with h | rfl | h
  · have hk : (k : ℝ)+1 ≤ j := by exact_mod_cast (show k+1 ≤ j by omega)
    simpa [ne_of_lt h] using
      (hb ((k : ℝ)+1-m) (by linarith)).sub (hb ((k : ℝ)-m) (by linarith))
  · simpa using (ht ((k : ℝ)+1-m) (by linarith)).sub (hb ((k : ℝ)-m) (by linarith))
  · have hk : (j : ℝ)+1 ≤ k := by exact_mod_cast (show j+1 ≤ k by omega)
    simpa [ne_of_gt h] using
      (ht ((k : ℝ)+1-m) (by linarith)).sub (ht ((k : ℝ)-m) (by linarith))

/-- Sending inverse scale to infinity and setting the mean at the shared
boundary minus `t/B` gives masses `Φ t` and `1 - Φ t` on the adjacent cells. -/
theorem adjacent_path (j k : ℤ) (t : ℝ) :
    Tendsto (fun B : ℝ ↦ cell k ((j : ℝ)+1-t/B) B) atTop
      (𝓝 (if k = j then Φ t else if k = j+1 then 1-Φ t else 0)) := by
  have ht (a : ℝ) (ha : 0 < a) : Tendsto (fun B : ℝ ↦ Φ (a*B+t)) atTop (𝓝 1) :=
    cdf_atTop.comp (tendsto_atTop_add_const_right atTop t (tendsto_id.const_mul_atTop ha))
  have hb (a : ℝ) (ha : a < 0) : Tendsto (fun B : ℝ ↦ Φ (a*B+t)) atTop (𝓝 0) :=
    cdf_atBot.comp (tendsto_atBot_add_const_right atTop t (tendsto_id.const_mul_atTop_of_neg ha))
  have hs : (fun B : ℝ ↦ cell k ((j : ℝ)+1-t/B) B) =ᶠ[atTop]
      (fun B : ℝ ↦ Φ (((k : ℝ)-j)*B+t)-Φ (((k : ℝ)-j-1)*B+t)) := by
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with B hB
    unfold cell
    congr 1 <;> congr 1 <;> field_simp [hB.ne'] <;> ring
  apply (Filter.tendsto_congr' hs).mpr
  rcases lt_trichotomy k j with h | rfl | h
  · have h' : (k : ℝ) < j := by exact_mod_cast h
    simpa [ne_of_lt h,show k ≠ j+1 by omega] using
      (hb ((k : ℝ)-j) (by linarith)).sub (hb ((k : ℝ)-j-1) (by linarith))
  · simpa using (tendsto_const_nhds (x := Φ t)).sub (hb (-1) (by norm_num))
  · rcases eq_or_lt_of_le (show j+1 ≤ k by omega) with he | h'
    · subst k
      convert (ht 1 one_pos).sub (tendsto_const_nhds (x := Φ t)) using 1
      · ext B
        push_cast
        ring_nf
      · simp
    · have h'' : (j : ℝ)+1 < k := by exact_mod_cast h'
      simpa [ne_of_gt h,ne_of_gt h'] using
        (ht ((k : ℝ)-j) (by linarith)).sub (ht ((k : ℝ)-j-1) (by linarith))

/-- With positive scale held, either location escape makes the entire
sample likelihood tend to zero. -/
theorem held_scale_tails {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) {B : ℝ} (hB : 0 < B) :
    Tendsto (fun m ↦ heldLocationLikelihood x m B) atTop (𝓝 0) ∧
    Tendsto (fun m ↦ heldLocationLikelihood x m B) atBot (𝓝 0) := by
  have hl (c : ℝ) : Tendsto (fun m ↦ Φ ((c-m)*B)) atTop (𝓝 0) :=
    cdf_atBot.comp (by
      simpa [sub_eq_add_neg] using
        (tendsto_atBot_add_const_left atTop c tendsto_neg_atTop_atBot).atBot_mul_const hB)
  have hr (c : ℝ) : Tendsto (fun m ↦ Φ ((c-m)*B)) atBot (𝓝 1) :=
    cdf_atTop.comp (by
      simpa [sub_eq_add_neg] using
        (tendsto_atTop_add_const_left atBot c tendsto_neg_atBot_atTop).atTop_mul_const hB)
  have he : (∏ _ : Fin n, (0 : ℝ)) = 0 :=
    Finset.prod_eq_zero (Finset.mem_univ ⟨0,hn⟩) rfl
  unfold heldLocationLikelihood
  constructor
  · simpa only [sub_self,he] using tendsto_finsetProd Finset.univ
      (fun i _ ↦ (hl ((x i : ℝ)+1)).sub (hl (x i : ℝ)))
  · simpa only [sub_self,he] using tendsto_finsetProd Finset.univ
      (fun i _ ↦ (hr ((x i : ℝ)+1)).sub (hr (x i : ℝ)))

/-- Any attained held-location maximum of the normalized working
likelihood is unique. -/
theorem held_mean_atMostOne {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (m : ℝ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hma : IsMaxOn (fun B ↦ workingLikelihood x (m*B,B)) (Ioi 0) a)
    (hmb : IsMaxOn (fun B ↦ workingLikelihood x (m*B,B)) (Ioi 0) b) : a = b := by
  let D : Set (ℝ × ℝ) := {p | 0 < p.2 ∧ p.1 = m*p.2}
  have hD : D ⊆ workingDomain := fun p hp ↦ ⟨mem_univ _,hp.1⟩
  have transfer {B : ℝ} (hB : 0 < B)
      (hm : IsMaxOn (fun q ↦ workingLikelihood x (m*q,q)) (Ioi 0) B) :
      IsMaxOn (fun q ↦ workingLogLik x (m*q,q)) (Ioi 0) B := by
    have hp : (m*B,B) ∈ D := ⟨hB,rfl⟩
    have hw : IsMaxOn (workingLikelihood x) D (m*B,B) := by
      rintro ⟨c,q⟩ ⟨hq,he⟩
      change c = m*q at he
      subst c
      exact hm (show q ∈ Ioi 0 from hq)
    have hlog := (working_argmax_iff x hD hp).mp hw
    exact fun q hq ↦ hlog (show (m*q,q) ∈ D from ⟨hq,rfl⟩)
  exact held_location_atMostOne hn x m ha hb (transfer ha hma) (transfer hb hmb)

/-- Any attained held-positive-scale maximum of the normalized working
likelihood is unique. -/
theorem held_scale_atMostOne_working {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {B : ℝ} (hB : 0 < B) {a b : ℝ}
    (ha : IsMaxOn (fun A ↦ workingLikelihood x (A,B)) univ a)
    (hb : IsMaxOn (fun A ↦ workingLikelihood x (A,B)) univ b) : a = b := by
  let D : Set (ℝ × ℝ) := {p | p.2 = B}
  have hD : D ⊆ workingDomain := fun p hp ↦ ⟨mem_univ _,(show p.2 = B from hp).symm ▸ hB⟩
  have transfer {A : ℝ} (hm : IsMaxOn (fun q ↦ workingLikelihood x (q,B)) univ A) :
      IsMaxOn (fun q ↦ workingLogLik x (q,B)) univ A := by
    have hp : (A,B) ∈ D := rfl
    have hw : IsMaxOn (workingLikelihood x) D (A,B) := by
      rintro ⟨q,c⟩ hc
      change c = B at hc
      subst c
      exact hm (mem_univ q)
    have hlog := (working_argmax_iff x hD hp).mp hw
    exact fun q _ ↦ hlog (show (q,B) ∈ D from rfl)
  exact held_scale_atMostOne hn x hB (transfer ha) (transfer hb)

/-- The all-free constant or adjacent two-cell data have saturated
heights that every finite normal fit strictly misses. -/
theorem saturated_suprema {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    let Y := workingLikelihood x '' workingDomain
    ((∃ j, ∀ i, x i = j) → IsLUB Y 1 ∧
      ∀ p ∈ workingDomain, workingLikelihood x p < 1) ∧
    (∀ j, (∀ i, x i = j ∨ x i = j+1) →
      0 < cellCount x j → 0 < cellCount x (j+1) →
      let C := adjacentHeight (cellCount x j) (cellCount x (j+1))
      IsLUB Y C ∧ ∀ p ∈ workingDomain, workingLikelihood x p < C) := by
  intro Y
  refine ⟨?_,fun j hx hz ho ↦ adjacent_supremum hx hz ho⟩
  rintro ⟨j,hj⟩
  exact constant_supremum hn hj

end UnivariateML.Discrete4DNorm

end
