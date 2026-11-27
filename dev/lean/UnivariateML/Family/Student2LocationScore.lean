module

public import UnivariateML.Family.StudentLocation

import Mathlib.Tactic
public import Mathlib.Algebra.Polynomial.Roots

set_option linter.style.header false

/-!
# Fixed-shape, fixed-scale Student location

The two-level calculation uses the squared kernel width. For standard
Student this is `(ν - 2) * σ^2`. For ordinary location–scale Student it
is `ν * a^2`.
-/

@[expose] public section

open Set Real Filter

namespace UnivariateML.Student2LocationScore

/-- Two-level negative log likelihood up to a positive multiplier and
an additive constant, with levels zero and `D`. -/
noncomputable def twoLevelCost (Q D k l m : ℝ) : ℝ :=
  k * log (Q + m^2) + l * log (Q + (m-D)^2)

private theorem cost_deriv {Q : ℝ} (hQ : 0 < Q) (D k l m : ℝ) :
    HasDerivAt (twoLevelCost Q D k l)
      (2*(k*m/(Q+m^2) + l*(m-D)/(Q+(m-D)^2))) m := by
  have H₁ := (((hasDerivAt_id m).pow 2).const_add Q).log
    (show Q + m^2 ≠ 0 by positivity)
  have H₂ := ((((hasDerivAt_id m).sub_const D).pow 2).const_add Q).log
    (show Q + (m-D)^2 ≠ 0 by positivity)
  convert (H₁.const_mul k).add (H₂.const_mul l) using 1
  · ext u
    rfl
  · norm_num
    ring

private theorem cost_attains {Q D k l : ℝ} (hQ : 0 < Q) (hD : 0 ≤ D)
    (hk : 0 ≤ k) (hl : 0 ≤ l) : ∃ m, IsMinOn (twoLevelCost Q D k l) univ m := by
  have hf : Continuous (twoLevelCost Q D k l) :=
    continuous_iff_continuousAt.mpr fun m ↦ (cost_deriv hQ D k l m).continuousAt
  obtain ⟨m,hm,hbest⟩ := isCompact_Icc.exists_isMinOn ⟨0,le_rfl,hD⟩ hf.continuousOn
  have compare (u v : ℝ) (h₀ : u^2 ≤ v^2) (h₁ : (u-D)^2 ≤ (v-D)^2) :
      twoLevelCost Q D k l u ≤ twoLevelCost Q D k l v :=
    add_le_add (mul_le_mul_of_nonneg_left (log_le_log (by positivity) (by linarith)) hk)
      (mul_le_mul_of_nonneg_left (log_le_log (by positivity) (by linarith)) hl)
  refine ⟨m,fun y _ ↦ ?_⟩
  by_cases h : y < 0
  · exact (hbest (show 0 ∈ Icc 0 D from ⟨le_rfl,hD⟩)).trans
      (compare 0 y (by simpa using sq_nonneg y) (by nlinarith [mul_nonneg hD (neg_nonneg.mpr h.le), sq_nonneg y]))
  by_cases h' : D < y
  · exact (hbest (show D ∈ Icc 0 D from ⟨hD,le_rfl⟩)).trans
      (compare D y (sq_le_sq₀ hD (hD.trans h'.le) |>.mpr h'.le) (by simpa using sq_nonneg (y-D)))
  exact hbest (show y ∈ Icc 0 D from ⟨le_of_not_gt h,le_of_not_gt h'⟩)

private theorem min_score {Q : ℝ} (hQ : 0 < Q) (D k l : ℝ) {m : ℝ}
    (hm : IsMinOn (twoLevelCost Q D k l) univ m) :
    k*m/(Q+m^2) + l*(m-D)/(Q+(m-D)^2) = 0 := by
  have H := (hm.isLocalMin univ_mem).deriv_eq_zero
  rw [(cost_deriv hQ D k l m).deriv] at H
  linarith

private theorem balanced_cost {Q D k : ℝ} (hQ : 0 < Q) (hk : 0 < k) (m : ℝ) :
    IsMinOn (twoLevelCost Q D k k) univ m ↔
      if Q < (D/2)^2 then
        m = D/2 + sqrt ((D/2)^2-Q) ∨ m = D/2 - sqrt ((D/2)^2-Q)
      else m = D/2 := by
  let h := D/2
  let A := fun u ↦ ((u-h)^2 + Q-h^2)^2
  have H (u : ℝ) : twoLevelCost Q D k k u = k * log (A u + 4*h^2*Q) := by
    unfold twoLevelCost
    rw [← mul_add,← log_mul (by positivity) (by positivity)]
    congr 2
    dsimp [A,h]
    ring
  have hc (u v : ℝ) : twoLevelCost Q D k k u ≤ twoLevelCost Q D k k v ↔ A u ≤ A v := by
    rw [H,H,mul_le_mul_iff_right₀ hk,log_le_log_iff,add_le_add_iff_right]
    · dsimp [A,h]
      have hp : 0 < (Q+u^2)*(Q+(u-D)^2) := by positivity
      have he : ((u-D/2)^2+Q-(D/2)^2)^2 + 4*(D/2)^2*Q = (Q+u^2)*(Q+(u-D)^2) := by ring
      rw [he]
      exact hp
    · dsimp [A,h]
      have hp : 0 < (Q+v^2)*(Q+(v-D)^2) := by positivity
      have he : ((v-D/2)^2+Q-(D/2)^2)^2 + 4*(D/2)^2*Q = (Q+v^2)*(Q+(v-D)^2) := by ring
      rw [he]
      exact hp
  change _ ↔ if Q < h^2 then m = h + sqrt (h^2-Q) ∨ m = h - sqrt (h^2-Q) else m = h
  by_cases hq : Q < h^2
  · rw [ite_eq_left hq]
    let r := sqrt (h^2-Q)
    change _ ↔ m = h+r ∨ m = h-r
    have hr : r^2 = h^2-Q := sq_sqrt (by linarith)
    have hPlus : A (h+r) = 0 := by dsimp [A]; rw [show h+r-h=r by ring]; nlinarith only [hr]
    constructor
    · intro hm
      have H := (hc m (h+r)).mp (hm (mem_univ _))
      rw [hPlus] at H
      have hz : (m-h)^2 + Q-h^2 = 0 := sq_eq_zero_iff.mp (le_antisymm H (sq_nonneg _))
      have he : (m-h-r)*(m-h+r) = 0 := by nlinarith only [hz,hr]
      rcases mul_eq_zero.mp he with h' | h'
      · exact Or.inl (by linarith)
      · exact Or.inr (by linarith)
    · intro hm u _
      apply (hc m u).mpr
      have hm' : (m-h)^2 = h^2-Q := by
        rcases hm with h' | h' <;> rw [h'] <;> nlinarith only [hr]
      dsimp [A]
      rw [hm']
      nlinarith [sq_nonneg ((u-h)^2+Q-h^2)]
  · rw [ite_eq_right hq]
    have hq₀ : 0 ≤ Q-h^2 := sub_nonneg.mpr (le_of_not_gt hq)
    have h' : ∀ u, A h ≤ A u := by
      intro u
      change ((h-h)^2+Q-h^2)^2 ≤ ((u-h)^2+Q-h^2)^2
      apply (sq_le_sq₀ (show 0 ≤ (h-h)^2+Q-h^2 by simpa using hq₀)
        (show 0 ≤ (u-h)^2+Q-h^2 by linarith [sq_nonneg (u-h)])).mpr
      nlinarith [sq_nonneg (u-h)]
    constructor
    · intro hm
      have H := (hc m h).mp (hm (mem_univ _))
      have Hp : 0 ≤ (m-h)^2+Q-h^2 := by linarith [sq_nonneg (m-h)]
      have H' := (sq_le_sq₀ Hp (show 0 ≤ (h-h)^2+Q-h^2 by simpa using hq₀)).mp H
      have Hz : (m-h)^2 = 0 := by nlinarith only [H',sq_nonneg (m-h)]
      exact sub_eq_zero.mp (sq_eq_zero_iff.mp Hz)
    · rintro rfl u _
      exact (hc _ _).mpr (h' u)

private theorem heavier_half {Q D k l : ℝ} (hQ : 0 < Q) (hD : 0 < D)
    (hl : 0 < l) (hk : l < k) {m : ℝ}
    (hm : IsMinOn (twoLevelCost Q D k l) univ m) : 0 < m ∧ m < D/2 := by
  have hk' := hl.trans hk
  have hscore := min_score hQ D k l hm
  have hlt₀ : 0 ≤ m := by
    by_contra! h
    have h₁ : k*m/(Q+m^2) < 0 := div_neg_of_neg_of_pos (mul_neg_of_pos_of_neg hk' h) (by positivity)
    have h₂ : l*(m-D)/(Q+(m-D)^2) < 0 :=
      div_neg_of_neg_of_pos (mul_neg_of_pos_of_neg hl (by linarith)) (by positivity)
    linarith
  have hlt : m ≤ D/2 := by
    by_contra! h
    have hsq : (D-m)^2 < m^2 := by nlinarith [mul_pos hD (show 0 < 2*m-D by linarith)]
    have hlog := log_lt_log (show 0 < Q+(D-m)^2 by positivity) (by linarith : Q+(D-m)^2 < Q+m^2)
    have hc := hm (show D-m ∈ (univ : Set ℝ) from mem_univ _)
    change twoLevelCost Q D k l m ≤ twoLevelCost Q D k l (D-m) at hc
    unfold twoLevelCost at hc
    rw [show (m-D)^2 = (D-m)^2 by ring,show (D-m-D)^2 = m^2 by ring] at hc
    nlinarith [mul_pos (sub_pos.mpr hk) (sub_pos.mpr hlog)]
  refine ⟨lt_of_le_of_ne hlt₀ ?_,lt_of_le_of_ne hlt ?_⟩
  · intro h
    rw [← h] at hscore
    have H : l*(0-D)/(Q+(0-D)^2) < 0 :=
      div_neg_of_neg_of_pos (mul_neg_of_pos_of_neg hl (by linarith)) (by positivity)
    exact H.ne (show l*(0-D)/(Q+(0-D)^2) = 0 by simpa using hscore)
  · rintro rfl
    have H : (k-l)*(D/2)/(Q+(D/2)^2) = 0 := by
      rw [show D/2-D = -(D/2) by ring,neg_sq] at hscore
      rw [← add_div] at hscore
      convert hscore using 1
      ring
    exact (div_pos (mul_pos (sub_pos.mpr hk) (half_pos hD)) (by positivity)).ne' H

private theorem heavier_ratio {Q D k l m : ℝ} (hQ : 0 < Q) (hD : 0 < D)
    (hl : 0 < l) (hk : l < k) (hm : 0 < m) (hm' : m < D/2)
    (hscore : k*m/(Q+m^2) + l*(m-D)/(Q+(m-D)^2) = 0) :
    let v := m/(D-m)
    let ρ := l/k
    0 < v ∧ v < ρ ∧ D^2/Q = (1+v)^2/v * ((ρ-v)/(1-ρ*v)) := by
  dsimp only
  let v := m/(D-m)
  let ρ := l/k
  have hk' := hl.trans hk
  have hdm : 0 < D-m := by linarith
  have hv : 0 < v := div_pos hm hdm
  have hv₁ : v < 1 := (div_lt_one hdm).mpr (by linarith)
  have hρ : 0 < ρ := div_pos hl hk'
  have hρ₁ : ρ < 1 := (div_lt_one hk').mpr hk
  have hmv : m = D*v/(1+v) := by
    dsimp [v]
    field_simp [hdm.ne',hD.ne']
    ring
  have hM : m-D = -D/(1+v) := by rw [hmv]; field_simp; ring
  have he : Q*(1+v)^2*(k*v-l) + D^2*v*(k-l*v) = 0 := by
    rw [hM,hmv] at hscore
    have h₁ : 0 < Q+(D*v/(1+v))^2 := by positivity
    have h₂ : 0 < Q+(-D/(1+v))^2 := by positivity
    field_simp [h₁.ne',h₂.ne',hD.ne',show 1+v ≠ 0 by positivity] at hscore
    simp only [mul_zero] at hscore
    have H := (mul_eq_zero.mp hscore).resolve_left
      (mul_ne_zero hD.ne' (show 1+v ≠ 0 by positivity))
    linear_combination H
  have hsign : 0 < k-l*v := by nlinarith [mul_lt_mul_of_pos_left hv₁ hl]
  have hp := mul_pos (mul_pos (sq_pos_of_pos hD) hv) hsign
  have hkv : k*v-l < 0 := by
    by_contra! h
    linarith [mul_nonneg (mul_nonneg hQ.le (sq_nonneg (1+v))) h]
  have hvρ : v < ρ := (lt_div_iff₀ hk').mpr (by linarith)
  refine ⟨hv,hvρ,?_⟩
  have hden : 0 < 1-ρ*v := by nlinarith [mul_lt_mul_of_pos_left hv₁ hρ]
  change D^2/Q = (1+v)^2/v * ((ρ-v)/(1-ρ*v))
  rw [div_mul_div_comm]
  apply (div_eq_div_iff hQ.ne' (mul_ne_zero hv.ne' hden.ne')).mpr
  have hkρ : l = k*ρ := by dsimp [ρ]; field_simp
  rw [hkρ] at he
  have hpoly : k * (D^2*(v*(1-ρ*v))-Q*((1+v)^2*(ρ-v))) = 0 := by
    linear_combination he
  have H := (mul_eq_zero.mp hpoly).resolve_left hk'.ne'
  linear_combination H

private theorem ratio_strict {ρ u v : ℝ} (hρ : 0 < ρ) (hρ₁ : ρ < 1)
    (hu : 0 < u) (huv : u < v) (hv : v < ρ) :
    (1+v)^2/v * ((ρ-v)/(1-ρ*v)) < (1+u)^2/u * ((ρ-u)/(1-ρ*u)) := by
  have hv₀ := hu.trans huv
  have hv₁ := hv.trans hρ₁
  have hu₁ := huv.trans hv₁
  have huD : 0 < 1-ρ*u := by nlinarith [mul_lt_mul_of_pos_left hu₁ hρ]
  have hvD : 0 < 1-ρ*v := by nlinarith [mul_lt_mul_of_pos_left hv₁ hρ]
  have hProd : 0 < 1-u*v := by nlinarith [mul_lt_mul_of_pos_left hv₁ hu]
  have hF : (1+v)^2/v < (1+u)^2/u := by
    apply (div_lt_div_iff₀ hv₀ hu).mpr
    have H := mul_pos (sub_pos.mpr huv) hProd
    linear_combination H
  have hG : (ρ-v)/(1-ρ*v) < (ρ-u)/(1-ρ*u) := by
    apply (div_lt_div_iff₀ hvD huD).mpr
    have H := mul_pos (sub_pos.mpr huv) (show 0 < 1-ρ^2 by nlinarith)
    linear_combination H
  exact mul_lt_mul_of_pos_of_nonneg hF.le hG (div_pos (by positivity) hv₀)
    (div_nonneg (sub_nonneg.mpr (huv.trans hv).le) huD.le)

private theorem heavier_unique {Q D k l : ℝ} (hQ : 0 < Q) (hD : 0 < D)
    (hl : 0 < l) (hk : l < k) :
    ∃! m, IsMinOn (twoLevelCost Q D k l) univ m := by
  obtain ⟨m,hm⟩ := cost_attains hQ hD.le (hl.trans hk).le hl.le
  refine ⟨m,hm,fun y hy ↦ ?_⟩
  obtain ⟨hm₀,hm'⟩ := heavier_half hQ hD hl hk hm
  obtain ⟨hy₀,hy'⟩ := heavier_half hQ hD hl hk hy
  obtain ⟨h₁,h₂,h₃⟩ := heavier_ratio hQ hD hl hk hm₀ hm' (min_score hQ D k l hm)
  obtain ⟨g₁,g₂,g₃⟩ := heavier_ratio hQ hD hl hk hy₀ hy' (min_score hQ D k l hy)
  have hρ := div_pos hl (hl.trans hk)
  have hρ₁ := (div_lt_one (hl.trans hk)).mpr hk
  have hEq : y/(D-y) = m/(D-m) := by
    rcases lt_trichotomy (y/(D-y)) (m/(D-m)) with h | h | h
    · have h' := ratio_strict hρ hρ₁ g₁ h h₂
      rw [← h₃,← g₃] at h'
      exact (lt_irrefl _ h').elim
    · exact h
    · have h' := ratio_strict hρ hρ₁ h₁ h g₂
      rw [← h₃,← g₃] at h'
      exact (lt_irrefl _ h').elim
  have H := (div_eq_div_iff (show D-y ≠ 0 by linarith) (show D-m ≠ 0 by linarith)).mp hEq
  have H' : D * (y-m) = 0 := by linear_combination H
  exact sub_eq_zero.mp ((mul_eq_zero.mp H').resolve_left hD.ne')

private theorem unequal_unique {Q D k l : ℝ} (hQ : 0 < Q) (hD : 0 < D)
    (hk : 0 ≤ k) (hl : 0 ≤ l) (hne : k ≠ l) :
    ∃! m, IsMinOn (twoLevelCost Q D k l) univ m := by
  have hRef (m : ℝ) : IsMinOn (twoLevelCost Q D k l) univ m ↔
      IsMinOn (twoLevelCost Q D l k) univ (D-m) := by
    have hAlg (m : ℝ) : twoLevelCost Q D k l m = twoLevelCost Q D l k (D-m) := by
      unfold twoLevelCost
      rw [show (D-m)^2 = (m-D)^2 by ring,show (D-m-D)^2 = m^2 by ring]
      exact add_comm _ _
    constructor <;> intro h y _
    · have H : twoLevelCost Q D k l m ≤ twoLevelCost Q D k l (D-y) := h (mem_univ _)
      rw [hAlg m,hAlg (D-y),show D-(D-y) = y by ring] at H
      exact H
    · change twoLevelCost Q D k l m ≤ twoLevelCost Q D k l y
      rw [hAlg m,hAlg y]
      exact h (mem_univ _)
  by_cases h0 : k = 0 ∨ l = 0
  · obtain ⟨m,hm⟩ := cost_attains hQ hD.le hk hl
    have u (m : ℝ) (hm : IsMinOn (twoLevelCost Q D k l) univ m) :
        m = if k = 0 then D else 0 := by
      have H := min_score hQ D k l hm
      rcases h0 with rfl | rfl
      · have hne' : l ≠ 0 := Ne.symm hne
        simp only [zero_mul,zero_div,zero_add,ite_true] at H ⊢
        have hnum := (div_eq_zero_iff.mp H).resolve_right (by positivity : Q+(m-D)^2 ≠ 0)
        exact sub_eq_zero.mp ((mul_eq_zero.mp hnum).resolve_left hne')
      · simp only [zero_mul,zero_div,add_zero,ite_eq_right hne] at H ⊢
        have hnum := (div_eq_zero_iff.mp H).resolve_right (by positivity : Q+m^2 ≠ 0)
        exact (mul_eq_zero.mp hnum).resolve_left hne
    exact ⟨m,hm,fun y hy ↦ (u y hy).trans (u m hm).symm⟩
  · have hk' : 0 < k := lt_of_le_of_ne hk fun h ↦ h0 (.inl h.symm)
    have hl' : 0 < l := lt_of_le_of_ne hl fun h ↦ h0 (.inr h.symm)
    rcases lt_or_gt_of_ne hne with h | h
    · obtain ⟨m,hm,hu⟩ := heavier_unique hQ hD hk' h
      refine ⟨D-m, (hRef (D-m)).mpr ?_,fun y hy ↦ ?_⟩
      · simpa only [sub_sub_cancel] using hm
      · have H := hu (D-y) ((hRef y).mp hy)
        linarith
    · exact heavier_unique hQ hD hl' h

/-- A two-level fixed-width Student sample has exactly the reflected
pair in the balanced wide-gap regime. Every other count and gap case
has a singleton. When a count is zero, that singleton is the occupied level. -/
theorem two_level_cost_classification {Q D k l : ℝ} (hQ : 0 < Q) (hD : 0 ≤ D)
    (hk : 0 ≤ k) (hl : 0 ≤ l) (hn : 0 < k+l) :
    ((k = l ∧ Q < (D/2)^2) →
      ∀ m, IsMinOn (twoLevelCost Q D k l) univ m ↔
        m = D/2 + sqrt ((D/2)^2-Q) ∨ m = D/2 - sqrt ((D/2)^2-Q)) ∧
    (¬ (k = l ∧ Q < (D/2)^2) → ∃! m, IsMinOn (twoLevelCost Q D k l) univ m) ∧
    (k = 0 → ∀ m, IsMinOn (twoLevelCost Q D k l) univ m ↔ m = D) ∧
    (l = 0 → ∀ m, IsMinOn (twoLevelCost Q D k l) univ m ↔ m = 0) := by
  refine ⟨?_,?_,?_,?_⟩
  · rintro ⟨rfl,h⟩ m
    have hk' : 0 < k := by linarith
    simpa only [ite_eq_left h] using balanced_cost hQ hk' (D := D) m
  · intro h
    by_cases hD' : D = 0
    · subst D
      obtain ⟨m,hm⟩ := cost_attains (D := 0) hQ (by norm_num) hk hl
      have hzero (y : ℝ) (hy : IsMinOn (twoLevelCost Q 0 k l) univ y) : y = 0 := by
        have H := min_score hQ 0 k l hy
        rw [sub_zero,← add_div,← add_mul] at H
        have h' := (div_eq_zero_iff.mp H).resolve_right (by positivity : Q+y^2 ≠ 0)
        exact (mul_eq_zero.mp h').resolve_left hn.ne'
      exact ⟨m,hm,fun y hy ↦ (hzero y hy).trans (hzero m hm).symm⟩
    · by_cases hkl : k = l
      · subst l
        have hk' : 0 < k := by linarith
        have hgap : ¬ Q < (D/2)^2 := fun hh ↦ h ⟨rfl,hh⟩
        have H (m : ℝ) := balanced_cost hQ hk' (D := D) m
        exact ⟨D/2,(H _).mpr (by simp [hgap]),fun m hm ↦ by simpa [hgap] using (H m).mp hm⟩
      · exact unequal_unique hQ (lt_of_le_of_ne hD (Ne.symm hD')) hk hl hkl
  · intro h m
    subst k
    have hzero (y : ℝ) (hy : IsMinOn (twoLevelCost Q D 0 l) univ y) : y = D := by
      have H := min_score hQ D 0 l hy
      simp only [zero_mul,zero_div,zero_add] at H
      have hl' : l ≠ 0 := by linarith
      exact sub_eq_zero.mp ((mul_eq_zero.mp
        ((div_eq_zero_iff.mp H).resolve_right (by positivity))).resolve_left hl')
    constructor
    · exact hzero m
    · rintro rfl
      obtain ⟨m,hm⟩ := cost_attains (k := 0) hQ hD (by norm_num) hl
      rw [hzero m hm] at hm
      exact hm
  · intro h m
    subst l
    have hzero (y : ℝ) (hy : IsMinOn (twoLevelCost Q D k 0) univ y) : y = 0 := by
      have H := min_score hQ D k 0 hy
      simp only [zero_mul,zero_div,add_zero] at H
      have hk' : k ≠ 0 := by linarith
      exact (mul_eq_zero.mp ((div_eq_zero_iff.mp H).resolve_right (by positivity))).resolve_left hk'
    constructor
    · exact hzero m
    · rintro rfl
      obtain ⟨m,hm⟩ := cost_attains (l := 0) hQ hD hk (by norm_num)
      rw [hzero m hm] at hm
      exact hm

private theorem two_level_sum {n : ℕ} (x : Fin n → ℝ) {b c : ℝ} (hbc : b ≠ c)
    (hx : ∀ i, x i = b ∨ x i = c) (f : ℝ → ℝ) :
    (∑ i, f (x i)) = TDist.multiplicity x b * f b + TDist.multiplicity x c * f c := by
  classical
  have H := Finset.sum_filter_add_sum_filter_not (s := Finset.univ) (fun i ↦ x i = b)
    (fun i ↦ f (x i))
  have hsets : (Finset.univ.filter (fun i ↦ ¬ x i = b)) =
      Finset.univ.filter (fun i ↦ x i = c) := by
    apply Finset.filter_congr
    intro i _
    exact ⟨fun h ↦ (hx i).resolve_left h,fun h h' ↦ hbc (h'.symm.trans h)⟩
  rw [hsets] at H
  have tt (a : ℝ) : (∑ i ∈ Finset.univ.filter (fun i ↦ x i = a), f (x i)) =
      TDist.multiplicity x a * f a := by
    calc
      _ = ∑ _ ∈ Finset.univ.filter (fun i ↦ x i = a), f a :=
        Finset.sum_congr rfl fun i hi ↦ congrArg f (Finset.mem_filter.mp hi).2
      _ = _ := by simp [TDist.multiplicity]
  rw [tt b,tt c] at H
  exact H.symm

/-- With standard deviation and df held, the complete one- and two-level
cardinality classification is determined by counts and the gap.
Zero counts cover one occupied level. -/
theorem std_two_level_all_counts {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b c ν σ : ℝ} (hbc : b < c) (hν : 2 < ν) (hσ : 0 < σ)
    (hx : ∀ i, x i = b ∨ x i = c) :
    let Q := (ν-2)*σ^2
    let h := (c-b)/2
    let k := TDist.multiplicity x b
    let l := TDist.multiplicity x c
    let best := fun m ↦ IsMaxOn (fun u ↦ TDist.stdLogLik x u ν σ) univ m
    ((k = l ∧ Q < h^2) →
      ∀ m, best m ↔ m = b+h+sqrt (h^2-Q) ∨ m = b+h-sqrt (h^2-Q)) ∧
    (¬ (k = l ∧ Q < h^2) → ∃! m, best m) ∧
    (k = 0 → ∀ m, best m ↔ m = c) ∧ (l = 0 → ∀ m, best m ↔ m = b) := by
  dsimp only
  let Q := (ν-2)*σ^2
  let D := c-b
  let k : ℝ := TDist.multiplicity x b
  let l : ℝ := TDist.multiplicity x c
  let w := (ν+1)/2
  have hQ : 0 < Q := mul_pos (sub_pos.mpr hν) (sq_pos_of_pos hσ)
  have hw : 0 < w := by dsimp [w]; linarith
  have hnkl : k+l = n := by
    have H := two_level_sum x hbc.ne hx (fun _ ↦ (1 : ℝ))
    simpa [k,l,add_comm] using H.symm
  have hsum : 0 < k+l := by rw [hnkl]; exact_mod_cast hn
  have H := two_level_cost_classification (Q := Q) (D := D) (k := k) (l := l) hQ
    (sub_nonneg.mpr hbc.le) (by dsimp [k]; positivity) (by dsimp [l]; positivity) hsum
  let A := (n : ℝ) * (log (Gamma ((ν+1)/2)) - log (Gamma (ν/2)) -
    log ((ν-2)*π)/2 - log σ + w*log Q)
  have hval (m : ℝ) : TDist.stdLogLik x m ν σ = A - w*twoLevelCost Q D k l (m-b) := by
    have hlog (i : Fin n) : log (1+(x i-m)^2/Q) = log (Q+(x i-m)^2) - log Q := by
      rw [show 1+(x i-m)^2/Q = (Q+(x i-m)^2)/Q by field_simp [hQ.ne'],
        log_div (by positivity) hQ.ne']
    have harg : (b-m)^2 = (m-b)^2 := by ring
    have harg' : (c-m)^2 = (m-b-D)^2 := by dsimp [D]; ring
    unfold TDist.stdLogLik
    change (∑ i, (log (Gamma ((ν+1)/2)) - log (Gamma (ν/2)) - log ((ν-2)*π)/2 -
      log σ - w*log (1+(x i-m)^2/Q))) = _
    simp_rw [hlog]
    rw [show (∑ i, (log (Gamma ((ν+1)/2)) - log (Gamma (ν/2)) - log ((ν-2)*π)/2 -
        log σ - w*(log (Q+(x i-m)^2)-log Q))) = A - w*(∑ i, log (Q+(x i-m)^2)) by
      simp [A,Finset.sum_sub_distrib,← Finset.mul_sum,mul_sub,mul_add]
      ring]
    rw [two_level_sum x hbc.ne hx (fun y ↦ log (Q+(y-m)^2))]
    change A-w*(k*log (Q+(b-m)^2)+l*log (Q+(c-m)^2)) = _
    rw [harg,harg']
    rfl
  have hmax (m : ℝ) : IsMaxOn (fun u ↦ TDist.stdLogLik x u ν σ) univ m ↔
      IsMinOn (twoLevelCost Q D k l) univ (m-b) := by
    constructor
    · intro hm u _
      have hh : TDist.stdLogLik x (u+b) ν σ ≤ TDist.stdLogLik x m ν σ :=
        hm (mem_univ _)
      rw [hval,hval,add_sub_cancel_right] at hh
      exact le_of_mul_le_mul_left (by linarith only [hh]) hw
    · intro hm u _
      have hh : twoLevelCost Q D k l (m-b) ≤ twoLevelCost Q D k l (u-b) :=
        hm (mem_univ _)
      change TDist.stdLogLik x u ν σ ≤ TDist.stdLogLik x m ν σ
      rw [hval,hval]
      linarith only [mul_le_mul_of_nonneg_left hh hw.le]
  refine ⟨?_,?_,?_,?_⟩
  · rintro ⟨he,hGap⟩ m
    have hEq : k = l := by dsimp [k,l]; exact_mod_cast he
    rw [hmax,(H.1 ⟨hEq,hGap⟩ (m-b))]
    dsimp [D]
    constructor <;> rintro (h | h)
    · exact Or.inl (by linarith only [h])
    · exact Or.inr (by linarith only [h])
    · exact Or.inl (by linarith only [h])
    · exact Or.inr (by linarith only [h])
  · intro h
    obtain ⟨m,hm,hu⟩ := H.2.1 (fun ⟨hkl,hgap⟩ ↦ h ⟨by dsimp [k,l] at hkl; exact_mod_cast hkl,hgap⟩)
    refine ⟨m+b,(hmax _).mpr (by simpa only [add_sub_cancel_right] using hm),fun u hu' ↦ ?_⟩
    have hh := hu (u-b) ((hmax _).mp hu')
    linarith
  · intro h m
    rw [hmax,H.2.2.1 (by dsimp [k]; exact_mod_cast h) _]
    dsimp [D]
    constructor <;> intro hh <;> linarith
  · intro h m
    rw [hmax,H.2.2.2 (by dsimp [l]; exact_mod_cast h) _]
    exact sub_eq_zero

/-- The numerator of the location score after clearing the positive
Student denominators. -/
noncomputable def locationNumerator {n : ℕ} (x : Fin n → ℝ) (Q : ℝ) : Polynomial ℝ :=
  ∑ i, (Polynomial.X - Polynomial.C (x i)) *
    ∏ j ∈ Finset.univ.erase i, ((Polynomial.X - Polynomial.C (x j))^2 + Polynomial.C Q)

private theorem numerator_eval {n : ℕ} (x : Fin n → ℝ) {Q : ℝ} (hQ : 0 < Q) (m : ℝ) :
    (locationNumerator x Q).eval m =
      (∏ i, ((m-x i)^2+Q)) * ∑ i, (m-x i)/((m-x i)^2+Q) := by
  simp only [locationNumerator,Polynomial.eval_finsetSum,Polynomial.eval_mul,
    Polynomial.eval_sub,Polynomial.eval_X,Polynomial.eval_C,Polynomial.eval_prod,
    Polynomial.eval_add,Polynomial.eval_pow,Finset.mul_sum]
  apply Finset.sum_congr rfl
  intro i _
  rw [← Finset.mul_prod_erase Finset.univ (fun j ↦ (m-x j)^2+Q) (Finset.mem_univ i)]
  have h : (m-x i)^2+Q ≠ 0 := by positivity
  field_simp [h]

private theorem numerator_ne_zero {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {Q : ℝ} (hQ : 0 < Q) : locationNumerator x Q ≠ 0 := by
  let m := (∑ i, |x i|) + 1
  have hm (i : Fin n) : x i < m := by
    have h : |x i| ≤ ∑ j, |x j| :=
      Finset.single_le_sum (fun j _ ↦ abs_nonneg (x j)) (Finset.mem_univ i)
    dsimp [m]
    linarith [le_abs_self (x i)]
  have hEval : 0 < (locationNumerator x Q).eval m := by
    rw [numerator_eval x hQ]
    apply mul_pos (Finset.prod_pos fun i _ ↦ by positivity)
    apply Finset.sum_pos
    · intro i _
      exact div_pos (sub_pos.mpr (hm i)) (by positivity)
    · exact ⟨⟨0,hn⟩,Finset.mem_univ _⟩
  intro h
  rw [h,Polynomial.eval_zero] at hEval
  exact (lt_irrefl 0 hEval)

private theorem logLik_as_product {n : ℕ} (x : Fin n → ℝ) {ν s : ℝ}
    (hν : 0 < ν) (hs : 0 < s) (m : ℝ) :
    let Q := ν*s^2
    TDist.logLik x m ν s =
      (n : ℝ) * (TDist.logNormalizer ν - log s + (ν+1)/2 * log Q) -
      (ν+1)/2 * log (∏ i, ((m-x i)^2+Q)) := by
  dsimp only
  have hQ : 0 < ν*s^2 := by positivity
  have h (i : Fin n) : log (1+(x i-m)^2/(ν*s^2)) = log ((m-x i)^2+ν*s^2) - log (ν*s^2) := by
    rw [show 1+(x i-m)^2/(ν*s^2) = ((m-x i)^2+ν*s^2)/(ν*s^2) by field_simp; ring,
      log_div (by positivity) hQ.ne']
  rw [log_prod fun i _ ↦ (show 0 < (m-x i)^2+ν*s^2 by positivity).ne']
  unfold TDist.logLik
  simp_rw [h]
  simp [Finset.sum_sub_distrib,← Finset.mul_sum,mul_sub,mul_add]
  ring

private theorem logLik_location_deriv {n : ℕ} (x : Fin n → ℝ) {ν s : ℝ}
    (hν : 0 < ν) (hs : 0 < s) (m : ℝ) :
    HasDerivAt (fun u ↦ TDist.logLik x u ν s)
      (-(ν+1)*(∑ i, (m-x i)/((m-x i)^2+ν*s^2))) m := by
  let Q := ν*s^2
  have hQ : 0 < Q := by dsimp [Q]; positivity
  have h (i : Fin n) :
      HasDerivAt (fun u ↦ log ((u-x i)^2+Q)) (2*(m-x i)/((m-x i)^2+Q)) m := by
    have H := ((((hasDerivAt_id m).sub_const (x i)).pow 2).add_const Q).log
      (show (m-x i)^2+Q ≠ 0 by positivity)
    simpa only [id_eq,Nat.cast_ofNat,Nat.reduceSub,pow_one,mul_one,Pi.pow_apply] using H
  have H := ((HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ h i).const_mul ((ν+1)/2)).const_sub
    ((n : ℝ) * (TDist.logNormalizer ν-log s+(ν+1)/2*log Q))
  convert H using 1
  · ext u
    rw [logLik_as_product x hν hs]
    simp only [Q, log_prod (fun i _ ↦ (show 0 < (u-x i)^2 + ν*s^2 by positivity).ne')]
  · change -(ν+1)*(∑ i, (m-x i)/((m-x i)^2+Q)) =
      -((ν+1)/2*(∑ i, 2*(m-x i)/((m-x i)^2+Q)))
    simp_rw [mul_div_assoc,← Finset.mul_sum]
    ring

/-- At held positive shape and scale the ordinary Student location
maxima are exactly the roots minimizing the product of all kernels.
The cleared score polynomial is nonzero, and its real root set is finite
and nonempty. -/
theorem lst_held_shape_scale_finite_roots {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    let Q := ν*s^2
    let P := locationNumerator x Q
    let R := {m : ℝ | P.IsRoot m}
    let K := fun m ↦ ∏ i, ((m-x i)^2+Q)
    P ≠ 0 ∧ R.Finite ∧ R.Nonempty ∧
      ∀ m, IsMaxOn (fun u ↦ TDist.logLik x u ν s) univ m ↔
        m ∈ R ∧ ∀ u ∈ R, K m ≤ K u := by
  dsimp only
  let Q := ν*s^2
  let P := locationNumerator x Q
  let R := {m : ℝ | P.IsRoot m}
  let K := fun m ↦ ∏ i, ((m-x i)^2+Q)
  have hQ : 0 < Q := by dsimp [Q]; positivity
  have hK (m : ℝ) : 0 < K m := Finset.prod_pos fun _ _ ↦ by positivity
  have hroot (m : ℝ) (hm : IsMaxOn (fun u ↦ TDist.logLik x u ν s) univ m) : m ∈ R := by
    have H := (hm.isLocalMax univ_mem).deriv_eq_zero
    rw [(logLik_location_deriv x hν hs m).deriv] at H
    have hν' : -(ν+1) ≠ 0 := by linarith
    have hz := (mul_eq_zero.mp H).resolve_left hν'
    change P.eval m = 0
    rw [numerator_eval x hQ,hz,mul_zero]
  have hComp (m u : ℝ) : TDist.logLik x u ν s ≤ TDist.logLik x m ν s ↔ K m ≤ K u := by
    rw [logLik_as_product x hν hs u,logLik_as_product x hν hs m]
    change _ - (ν+1)/2 * log (K u) ≤ _ - (ν+1)/2 * log (K m) ↔ _
    rw [sub_le_sub_iff_left,mul_le_mul_iff_right₀ (by positivity : 0 < (ν+1)/2),
      log_le_log_iff (hK m) (hK u)]
  obtain ⟨w,hw⟩ := StudentLocation.lst_location_attainment x hn hν hs
  have hP := numerator_ne_zero x hn hQ
  refine ⟨hP,Polynomial.finite_setOfPred_isRoot hP,⟨w,hroot w hw⟩,fun m ↦ ?_⟩
  constructor
  · intro hm
    exact ⟨hroot m hm,fun u _ ↦ (hComp m u).mp (hm (mem_univ u))⟩
  · rintro ⟨_,hm⟩ u _
    exact (hw (mem_univ u)).trans ((hComp m w).mpr (hm w (hroot w hw)))

open Polynomial

/-- Spatial log-density score of a Student kernel, where a is the
exponent numerator and Q is the squared kernel width. -/
noncomputable def spatialScore (a m Q y : ℝ) : ℝ :=
  -a * (y-m) / (Q+(y-m)^2)

private noncomputable def differenceNumerator (a b m k Q R : ℝ) : ℝ[X] :=
  C (a-b)*X^3 +
    C (a*(-m-2*k)-b*(-k-2*m))*X^2 +
    C (a*(R+k^2+2*m*k)-b*(Q+m^2+2*k*m))*X +
    C (-a*m*(R+k^2)+b*k*(Q+m^2))

private theorem differenceNumerator_eval (a b m k Q R y : ℝ) :
    (differenceNumerator a b m k Q R).eval y =
      a*(y-m)*(R+(y-k)^2)-b*(y-k)*(Q+(y-m)^2) := by
  simp [differenceNumerator]
  ring

private theorem numerator_degree (a b m k Q R : ℝ) :
    (differenceNumerator a b m k Q R).natDegree ≤ (if a = b then 2 else 3) := by
  unfold differenceNumerator
  split_ifs with h
  · subst b
    simp only [sub_self, map_zero, zero_mul, zero_add]
    compute_degree
  · compute_degree

/-- Four distinct levels separate unequal Student parameters. Three
suffice at a common shape, where the cubic coefficient cancels. -/
theorem student_spatial_score_separation {a b m k Q R : ℝ}
    (ha : 0 < a) (hb : 0 < b) (hQ : 0 < Q) (hR : 0 < R)
    (s : Finset ℝ) (hs : (if a = b then 2 else 3) < s.card)
    (hag : ∀ y ∈ s, spatialScore a m Q y = spatialScore b k R y) :
    a = b ∧ m = k ∧ Q = R := by
  let p := differenceNumerator a b m k Q R
  have hz : p = 0 := by
    by_contra hne
    have hincl : s ⊆ p.roots.toFinset := by
      intro y hy
      rw [Multiset.mem_toFinset, mem_roots hne, IsRoot.def, differenceNumerator_eval]
      have hh := hag y hy
      unfold spatialScore at hh
      have h1 : 0 < Q+(y-m)^2 := by positivity
      have h2 : 0 < R+(y-k)^2 := by positivity
      have hcross := (div_eq_div_iff h1.ne' h2.ne').mp hh
      linarith
    have hc := (Finset.card_le_card hincl).trans
      (Multiset.toFinset_card_le _)
    exact (not_le_of_gt hs) (hc.trans ((card_roots' p).trans (numerator_degree ..)))
  have hall (y : ℝ) :
      a*(y-m)*(R+(y-k)^2)-b*(y-k)*(Q+(y-m)^2) = 0 := by
    rw [← differenceNumerator_eval, ← show p = differenceNumerator a b m k Q R from rfl, hz, eval_zero]
  have hm : m = k := by
    have hh := hall m
    simp only [sub_self, mul_zero, zero_mul, zero_pow two_ne_zero, add_zero, zero_sub, neg_eq_zero,
      mul_eq_zero] at hh
    exact sub_eq_zero.mp (by aesop)
  subst k
  have hab : a = b := by
    have hh := congrArg (fun p : ℝ[X] ↦ p.coeff 3) hz
    simp only [p, differenceNumerator, coeff_add, C_mul_X_pow_eq_monomial, C_mul_X_eq_monomial,
      coeff_monomial, coeff_C, coeff_zero] at hh
    norm_num at hh
    exact sub_eq_zero.mp hh
  subst b
  have hh := hall (m+1)
  simp only [add_sub_cancel_left, mul_one, one_pow] at hh
  have hq : a * (R-Q) = 0 := by linear_combination hh
  exact ⟨rfl,rfl,(sub_eq_zero.mp ((mul_eq_zero.mp hq).resolve_left ha.ne')).symm⟩

/-- At a shared center, equality at two distinct nonzero residual
magnitudes identifies both the exponent and the squared width. -/
theorem student_common_center_score_separation {a b m Q R y z : ℝ}
    (ha : 0 < a) (hQ : 0 < Q) (hR : 0 < R)
    (hy : y ≠ m) (hz : z ≠ m) (hgap : (y-m)^2 ≠ (z-m)^2)
    (hyag : spatialScore a m Q y = spatialScore b m R y)
    (hzag : spatialScore a m Q z = spatialScore b m R z) :
    a = b ∧ Q = R := by
  have cross (t : ℝ) (ht : t ≠ m) (hag : spatialScore a m Q t = spatialScore b m R t) :
      a * (R+(t-m)^2) = b * (Q+(t-m)^2) := by
    unfold spatialScore at hag
    have h₁ : 0 < Q+(t-m)^2 := by positivity
    have h₂ : 0 < R+(t-m)^2 := by positivity
    have he := (div_eq_div_iff h₁.ne' h₂.ne').mp hag
    apply mul_right_cancel₀ (sub_ne_zero.mpr ht)
    linear_combination -he
  have h1 := cross y hy hyag
  have h2 := cross z hz hzag
  have hh : (a-b)*((y-m)^2-(z-m)^2) = 0 := by linear_combination h1-h2
  have hab := sub_eq_zero.mp ((mul_eq_zero.mp hh).resolve_right (sub_ne_zero.mpr hgap))
  subst b
  have hqr : a*(Q-R) = 0 := by linear_combination -h1
  exact ⟨rfl,sub_eq_zero.mp ((mul_eq_zero.mp hqr).resolve_left ha.ne')⟩

/-- A fixed-center Gaussian score and a Student score cannot agree at
two nonzero levels of different absolute residual. -/
theorem student_normal_common_center_separation {a m Q V y z : ℝ}
    (hQ : 0 < Q) (hV : 0 < V)
    (hy : y ≠ m) (hz : z ≠ m) (hgap : (y-m)^2 ≠ (z-m)^2)
    (hyag : spatialScore a m Q y = -(y-m)/V) :
    spatialScore a m Q z ≠ -(z-m)/V := by
  have cross (t : ℝ) (ht : t ≠ m) (hag : spatialScore a m Q t = -(t-m)/V) :
      a * V = Q + (t-m)^2 := by
    unfold spatialScore at hag
    have h₁ : 0 < Q+(t-m)^2 := by positivity
    have he := (div_eq_div_iff h₁.ne' hV.ne').mp hag
    apply mul_right_cancel₀ (sub_ne_zero.mpr ht)
    linear_combination -he
  intro he
  exact hgap (by linarith [cross y hy hyag,cross z hz he])

private theorem polynomial_eq_zero_of_card_lt {p : ℝ[X]} (s : Finset ℝ)
    (hs : p.natDegree < s.card) (h : ∀ y ∈ s, p.eval y = 0) : p = 0 := by
  by_contra hp
  have hsub : s ⊆ p.roots.toFinset := fun y hy ↦
    Multiset.mem_toFinset.mpr ((mem_roots hp).mpr (h y hy))
  exact hs.not_ge ((Finset.card_le_card hsub).trans
    ((Multiset.toFinset_card_le _).trans (card_roots' p)))

/-- A finite Student spatial score cannot agree with a Gaussian score
at four distinct observation levels. -/
theorem student_normal_score_separation {a m Q k V : ℝ} (hQ : 0 < Q) (hV : 0 < V)
    (s : Finset ℝ) (hs : 3 < s.card) :
    ¬ ∀ y ∈ s, spatialScore a m Q y = -(y-k)/V := by
  intro h
  let p : ℝ[X] := X^3+C (-k-2*m)*X^2+C (Q+m^2+2*k*m-a*V)*X+C (-k*(Q+m^2)+a*m*V)
  have hdeg : p.natDegree ≤ 3 := by dsimp [p]; compute_degree
  have he (y : ℝ) : p.eval y = (y-k)*(Q+(y-m)^2)-a*V*(y-m) := by
    simp [p]; ring
  have h0 := polynomial_eq_zero_of_card_lt s (hdeg.trans_lt hs) (fun y hy ↦ by
    rw [he]
    have hh := h y hy
    unfold spatialScore at hh
    have h1 : 0 < Q+(y-m)^2 := by positivity
    have h2 := (div_eq_div_iff h1.ne' hV.ne').mp hh
    linarith)
  have h3 := congrArg (fun p : ℝ[X] ↦ p.coeff 3) h0
  simp only [p,coeff_add,C_mul_X_pow_eq_monomial,C_mul_X_eq_monomial,coeff_monomial,
    coeff_X_pow,coeff_C,coeff_zero] at h3
  norm_num at h3

/-- A positive-mass collapse score cannot agree with a finite Student
score at three distinct noncontact levels. -/
theorem student_collapse_score_separation {a m Q b k : ℝ} (hQ : 0 < Q) (hk : 0 < k)
    (s : Finset ℝ) (hs : 2 < s.card) (hoff : ∀ y ∈ s, y ≠ b) :
    ¬ ∀ y ∈ s, spatialScore a m Q y = -k/(y-b) := by
  intro h
  let p : ℝ[X] := C (k-a)*X^2+C (-2*k*m+a*(m+b))*X+C (k*(Q+m^2)-a*m*b)
  have hdeg : p.natDegree ≤ 2 := by dsimp [p]; compute_degree
  have he (y : ℝ) : p.eval y = k*(Q+(y-m)^2)-a*(y-m)*(y-b) := by simp [p]; ring
  have h0 := polynomial_eq_zero_of_card_lt s (hdeg.trans_lt hs) (fun y hy ↦ by
    rw [he]
    have hh := h y hy
    unfold spatialScore at hh
    have h1 : 0 < Q+(y-m)^2 := by positivity
    have h2 := (div_eq_div_iff h1.ne' (sub_ne_zero.mpr (hoff y hy))).mp hh
    linarith)
  have hb := he b
  rw [h0,eval_zero,sub_self,mul_zero,sub_zero] at hb
  exact (mul_pos hk (by positivity : 0 < Q+(b-m)^2)).ne' hb.symm

/-- The rational score above is the derivative with respect to the
observation of the ordinary location–scale Student log density. -/
theorem student_spatial_score_derivative {v m s : ℝ} (hv : 0 < v) (hs : 0 < s) (y : ℝ) :
    HasDerivAt (fun z ↦ log (TDist.density m v s z))
      (spatialScore (v+1) m (v*s^2) y) y := by
  let Q := v*s^2
  have hQ : 0 < Q := by dsimp [Q]; positivity
  have hD : HasDerivAt (fun z : ℝ ↦ log (Q+(z-m)^2)) (2*(y-m)/(Q+(y-m)^2)) y := by
    have h := ((((hasDerivAt_id y).sub_const m).pow 2).const_add Q).log
      (show Q+(y-m)^2 ≠ 0 by positivity)
    simpa only [id_eq,Nat.cast_ofNat,Nat.reduceSub,pow_one,mul_one,Pi.pow_apply] using h
  have h := (hD.const_mul ((v+1)/2)).const_sub
    (TDist.logNormalizer v-log s+(v+1)/2*log Q)
  convert h using 1
  · ext z
    rw [TDist.density,log_exp,show 1+(z-m)^2/(v*s^2) = (Q+(z-m)^2)/Q by
      change 1+(z-m)^2/Q = _; field_simp]
    rw [log_div (by positivity : Q+(z-m)^2 ≠ 0) hQ.ne']
    ring
  · unfold spatialScore
    change -(v+1)*(y-m)/(Q+(y-m)^2) = -((v+1)/2*(2*(y-m)/(Q+(y-m)^2)))
    ring


/-- A Gaussian/Student-width denominator separates a finite Student
score from a same-center collapse trace at two unequal nonzero magnitudes. -/
theorem student_collapse_common_center_separation {a m Q k y z : ℝ} (hQ : 0 < Q) (hk : 0 < k)
    (hy : y ≠ m) (hz : z ≠ m) (hgap : (y-m)^2 ≠ (z-m)^2)
    (hyag : spatialScore a m Q y = -k/(y-m)) :
    spatialScore a m Q z ≠ -k/(z-m) := by
  have cross (t : ℝ) (ht : t ≠ m) (hag : spatialScore a m Q t = -k/(t-m)) :
      a*(t-m)^2 = k*(Q+(t-m)^2) := by
    unfold spatialScore at hag
    have h : 0 < Q+(t-m)^2 := by positivity
    have hh := (div_eq_div_iff h.ne' (sub_ne_zero.mpr ht)).mp hag
    linear_combination -hh
  intro he
  have hy' := cross y hy hyag
  have hz' := cross z hz he
  have hh : (a-k)*((y-m)^2-(z-m)^2) = 0 := by linear_combination hy'-hz'
  have hak := sub_eq_zero.mp ((mul_eq_zero.mp hh).resolve_right (sub_ne_zero.mpr hgap))
  rw [hak] at hy'
  have hbad : k*Q = 0 := by linear_combination -hy'
  exact (mul_pos hk hQ).ne' hbad

end UnivariateML.Student2LocationScore
end
