module

public import UnivariateML.Family.NegHypergeometric
public import Mathlib.Data.Nat.Choose.Vandermonde

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Negative hypergeometric support and saturation

Observations are total draws. The stopped-draw likelihood includes the
combinatorial factor, which varies when comparing stopping counts.
-/

@[expose] public section

open Set Filter Topology
open scoped BigOperators Classical

namespace UnivariateML.Discrete4NHyper

open NegHypergeometric

/-- The zero stopping count gives the atom at zero. For positive stopping
count the black draws must fit in the unmarked portion of the urn. -/
def Compatible (N m r t : ℕ) : Prop :=
  m ≤ N ∧ r ≤ m ∧ (if r = 0 then t = 0 else r ≤ t ∧ t-r ≤ N-m)

noncomputable def totalMass (N m r t : ℕ) : ℝ := by
  classical
  exact if Compatible N m r t then mass r m (N-m) (t-r) else 0

noncomputable def totalLikelihood {h : ℕ} (x : Fin h → ℕ) (N m r : ℕ) : ℝ :=
  ∏ i, totalMass N m r (x i)

private theorem zero_mass {N m : ℕ} (hm : m ≤ N) : totalMass N m 0 0 = 1 := by
  simp [totalMass, Compatible, hm, mass, reducedMass]

private theorem stopped_upper {N m r t : ℕ} (hc : Compatible N m r t) (hr : 0 < r) :
    totalMass N m r t ≤ (r : ℝ)/t := by
  have hmt := hc.1
  have hs := hc.2.2
  rw [ite_eq_right hr.ne'] at hs
  obtain ⟨hrt, hst⟩ := hs
  have hrm := hc.2.1
  have htN : t ≤ N := by omega
  have hne : (N.choose t : ℝ) ≠ 0 := (Nat.cast_pos.mpr (Nat.choose_pos htN)).ne'
  have he : m+(N-m) = N := Nat.add_sub_of_le hmt
  have ht : r+(t-r) = t := Nat.add_sub_of_le hrt
  have hd (a b : ℕ) : (a.descFactorial b : ℝ) = b.factorial * (a.choose b : ℝ) := by
    rw [Nat.descFactorial_eq_factorial_mul_choose, Nat.cast_mul]
  have hfact : ((t-1).choose (r-1) : ℝ) * r.factorial * (t-r).factorial * t =
      r * t.factorial := by
    have H := Nat.choose_mul_factorial_mul_factorial (show r-1 ≤ t-1 by omega)
    have H' : ((t-1).choose (r-1) : ℝ) * (r-1).factorial * (t-r).factorial =
        (t-1).factorial := by
      exact_mod_cast (show (t-1).choose (r-1) * (r-1).factorial * (t-r).factorial =
        (t-1).factorial by simpa only [show t-1-(r-1) = t-r by omega] using H)
    have Hr : (r.factorial : ℝ) = r * (r-1).factorial := by
      conv_lhs => rw [show r = (r-1)+1 by omega, Nat.factorial_succ, Nat.cast_mul]
      rw [Nat.sub_add_cancel hr]
    have Ht : (t.factorial : ℝ) = t * (t-1).factorial := by
      conv_lhs => rw [show t = (t-1)+1 by omega, Nat.factorial_succ, Nat.cast_mul]
      rw [Nat.sub_add_cancel (show 1 ≤ t by omega)]
    rw [Hr, Ht]
    linear_combination ((r : ℝ) * t) * H'
  have hprod : m.choose r * (N-m).choose (t-r) ≤ N.choose t := by
    conv_rhs => rw [← he, Nat.add_choose_eq]
    exact Finset.single_le_sum
      (f := fun ij : ℕ × ℕ ↦ m.choose ij.1 * (N-m).choose ij.2)
      (fun _ _ ↦ Nat.zero_le _) (show (r,t-r) ∈ Finset.antidiagonal t from Finset.mem_antidiagonal.mpr ht)
  have hidentity : totalMass N m r t =
      ((r : ℝ)/t) * ((m.choose r : ℝ) * (N-m).choose (t-r) / N.choose t) := by
    rw [totalMass, ite_eq_left hc, mass, reducedMass, he, ht]
    rw [hd, hd, hd]
    have htR : (t : ℝ) ≠ 0 := (Nat.cast_pos.mpr (show 0 < t by omega)).ne'
    field_simp [hne, htR]
    linear_combination ((m.choose r : ℝ) * ((N-m).choose (t-r) : ℝ)) * hfact
  have hq : (m.choose r : ℝ) * (N-m).choose (t-r) / N.choose t ≤ 1 := by
    rw [div_le_iff₀ (Nat.cast_pos.mpr (Nat.choose_pos htN)), one_mul]
    exact_mod_cast hprod
  rw [hidentity]
  simpa using mul_le_mul_of_nonneg_left hq (show 0 ≤ (r : ℝ)/t by positivity)

private theorem total_mass_le_one (N m r t : ℕ) : totalMass N m r t ≤ 1 := by
  by_cases hc : Compatible N m r t
  · by_cases hr : r = 0
    · subst r
      have ht : t = 0 := by simpa using hc.2.2
      subst t
      rw [zero_mass hc.1]
    · have hpos := Nat.pos_of_ne_zero hr
      have hrt : r ≤ t := by
        have h := hc.2.2
        rw [ite_eq_right hr] at h
        exact h.1
      exact (stopped_upper hc hpos).trans
        ((div_le_one (Nat.cast_pos.mpr (hpos.trans_le hrt))).mpr (by exact_mod_cast hrt))
  · simp [totalMass, hc]

/-- A constant sample attains likelihood one at the all-white urn with
population and stopping count equal to the common observation. -/
theorem constant_attains_saturated {h : ℕ} (c : ℕ) :
    totalLikelihood (fun _ : Fin h ↦ c) c c c = 1 ∧
      ∀ N m r, totalLikelihood (fun _ : Fin h ↦ c) N m r ≤ 1 := by
  have H : totalMass c c c c = 1 := by
    by_cases hc : c = 0
    · subst c
      exact zero_mass le_rfl
    · have hs : Compatible c c c c := by simp [Compatible, hc]
      have hd : (c.descFactorial c : ℝ) ≠ 0 :=
        (Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr le_rfl)).ne'
      simp [totalMass, hs, mass, reducedMass, hd]
  refine ⟨by simp [totalLikelihood, H], fun N m r ↦ ?_⟩
  exact Finset.prod_le_one₀ (fun i _ ↦ by
    unfold totalMass
    split
    · unfold mass reducedMass
      positivity
    · exact le_rfl)
    (fun _ _ ↦ total_mass_le_one N m r c)

/-- A sample containing both zero and a positive number of draws is infeasible. -/
theorem mixed_zero_infeasible {h : ℕ} (x : Fin h → ℕ) {i j : Fin h}
    (hi : x i = 0) (hj : 0 < x j) :
    ¬ ∃ N m r, ∀ k, Compatible N m r (x k) := by
  rintro ⟨N,m,r,H⟩
  by_cases hr : r = 0
  · have := (H j).2.2
    simp [hr] at this
    omega
  · have := (H i).2.2
    simp [hr, hi] at this

private theorem total_mass_nonneg (N m r t : ℕ) : 0 ≤ totalMass N m r t := by
  unfold totalMass
  split
  · unfold mass reducedMass
    positivity
  · exact le_rfl

private theorem falling_lt {a b r : ℕ} (hr : 0 < r) (hra : r ≤ a) (hab : a < b) :
    a.descFactorial r < b.descFactorial r := by
  rw [Nat.descFactorial_eq_prod_range, Nat.descFactorial_eq_prod_range]
  apply Finset.prod_lt_prod_of_nonempty₀
  · intro i hi
    have := Finset.mem_range.mp hi
    omega
  · intro i hi
    have := Finset.mem_range.mp hi
    omega
  · exact ⟨0, Finset.mem_range.mpr hr⟩

private theorem total_mass_one_iff (N m r c : ℕ) :
    totalMass N m r c = 1 ↔ m ≤ N ∧ r ≤ m ∧ r = c ∧ (c = 0 ∨ m = N) := by
  constructor
  · intro H
    have hc : Compatible N m r c := by
      by_contra h
      simp [totalMass,h] at H
    have hm := hc.1
    have hrm := hc.2.1
    by_cases hr : r = 0
    · have hz : c = 0 := by simpa [hr] using hc.2.2
      exact ⟨hm, hrm, hr.trans hz.symm, Or.inl hz⟩
    · have hr' := Nat.pos_of_ne_zero hr
      have h₀ := hc.2.2
      rw [ite_eq_right hr] at h₀
      have hrle := h₀.1
      have he : r = c := by
        by_contra hne
        have hlt : (r : ℝ) < c := by exact_mod_cast Nat.lt_of_le_of_ne hrle hne
        have h := (stopped_upper hc hr').trans_lt
          ((div_lt_one (Nat.cast_pos.mpr (hr'.trans_le hrle))).mpr hlt)
        exact (ne_of_lt h) H
      subst c
      refine ⟨hm, hrm, rfl, Or.inr ?_⟩
      by_contra hne
      have hden : 0 < (N.descFactorial r : ℝ) :=
        Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr (hrm.trans hm))
      have hnum : (m.descFactorial r : ℝ) < N.descFactorial r := by
        exact_mod_cast falling_lt hr' hrm (Nat.lt_of_le_of_ne hm hne)
      have hEq : totalMass N m r r = (m.descFactorial r : ℝ) / N.descFactorial r := by
        simp [totalMass, hc, mass, reducedMass, Nat.add_sub_of_le hm]
      exact (ne_of_lt (hEq ▸ (div_lt_one hden).mpr hnum)) H
  · rintro ⟨hm, hrm, rfl, hz | he⟩
    · subst r
      exact zero_mass hm
    · subst N
      have hc : Compatible m m r r := by
        by_cases hz : r = 0 <;> simp [Compatible, hz, hrm]
      have hd : (m.descFactorial r : ℝ) ≠ 0 :=
        (Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hrm)).ne'
      simp [totalMass, hc, mass, reducedMass, hd]

/-- The saturated set on constants consists precisely of all zero-stopping
urns on zeros, and all-white urns with stopping count equal to the common
positive observation otherwise. -/
theorem constant_saturated_iff {h : ℕ} (hh : 0 < h) (N m r c : ℕ) :
    totalLikelihood (fun _ : Fin h ↦ c) N m r = 1 ↔
      m ≤ N ∧ r ≤ m ∧ r = c ∧ (c = 0 ∨ m = N) := by
  rw [← total_mass_one_iff]
  rw [totalLikelihood, Finset.prod_const, Finset.card_univ, Fintype.card_fin]
  constructor
  · intro H
    by_contra hne
    exact (ne_of_lt (pow_lt_one₀ (total_mass_nonneg N m r c)
      (lt_of_le_of_ne (total_mass_le_one N m r c) hne) hh.ne')) H
  · intro H
    simp [H]

private theorem finite_grid {h N : ℕ} (x : Fin h → ℕ) (H : (ℕ × ℕ) → Prop) :
    let D : Set (ℕ × ℕ) := {p | p.1 ≤ N ∧ p.2 ≤ p.1 ∧ H p ∧
      ∀ i, Compatible N p.1 p.2 (x i)}
    D.Finite ∧ (D.Nonempty ↔ ∃ p ∈ D,
      IsMaxOn (fun q ↦ totalLikelihood x N q.1 q.2) D p) := by
  dsimp
  let grid := ((Finset.range (N+1)).product (Finset.range (N+1))).filter
    fun p ↦ p.2 ≤ p.1 ∧ H p ∧ ∀ i, Compatible N p.1 p.2 (x i)
  have hg (p : ℕ × ℕ) : p ∈ grid ↔ p.1 ≤ N ∧ p.2 ≤ p.1 ∧ H p ∧
      ∀ i, Compatible N p.1 p.2 (x i) := by
    rcases p with ⟨a,b⟩
    simp [grid, and_assoc]
    exact fun ha hb _ _ ↦ hb.trans ha
  refine ⟨grid.finite_toSet.subset (fun p hp ↦ (hg p).mpr hp), ?_⟩
  constructor
  · rintro ⟨a,ha⟩
    obtain ⟨b,hb,hmax⟩ := grid.exists_max_image (fun p ↦ totalLikelihood x N p.1 p.2)
      ⟨a,(hg a).mpr ha⟩
    exact ⟨b,(hg b).mp hb, fun p hp ↦ hmax p ((hg p).mpr hp)⟩
  · rintro ⟨p,hp,_⟩
    exact ⟨p,hp⟩

/-- A fixed population admits precisely all-zero samples and samples
whose observations are all positive and at most the population. -/
theorem fixed_population_feasible_iff {h N : ℕ} (x : Fin h → ℕ) :
    (∃ m r, m ≤ N ∧ r ≤ m ∧ ∀ i, Compatible N m r (x i)) ↔
      (∀ i, x i = 0) ∨ (∀ i, 0 < x i ∧ x i ≤ N) := by
  constructor
  · rintro ⟨m,r,hm,hr,H⟩
    by_cases hz : r = 0
    · exact Or.inl (fun i ↦ by simpa [hz] using (H i).2.2)
    · refine Or.inr (fun i ↦ ?_)
      have hf := (H i).2.2
      rw [ite_eq_right hz] at hf
      omega
  · rintro (hz | hp)
    · exact ⟨0,0,Nat.zero_le _,le_rfl,fun i ↦ by simp [Compatible,hz i]⟩
    · by_cases hn : h = 0
      · exact ⟨0,0,Nat.zero_le _,le_rfl,fun i ↦ by exact Fin.elim0 (hn ▸ i)⟩
      · have hN : 1 ≤ N := by have := (hp ⟨0,Nat.pos_of_ne_zero hn⟩); omega
        exact ⟨1,1,hN,le_rfl,fun i ↦ by
          have := hp i
          simp only [Compatible, one_ne_zero, ite_false]
          omega⟩

/-- At known population the feasible composition and stopping grid is
finite. Attainment is equivalent to the explicit data compatibility test. -/
theorem fixed_population_attainment {h N : ℕ} (x : Fin h → ℕ) :
    let D : Set (ℕ × ℕ) := {p | p.1 ≤ N ∧ p.2 ≤ p.1 ∧
      ∀ i, Compatible N p.1 p.2 (x i)}
    D.Finite ∧ (((∀ i, x i = 0) ∨ (∀ i, 0 < x i ∧ x i ≤ N)) ↔
      ∃ p ∈ D, IsMaxOn (fun q ↦ totalLikelihood x N q.1 q.2) D p) := by
  dsimp
  have hg := finite_grid (N := N) x (fun _ ↦ True)
  simp only [true_and] at hg
  refine ⟨hg.1, ?_⟩
  apply Iff.trans _ hg.2
  rw [← fixed_population_feasible_iff (N := N) x]
  constructor
  · rintro ⟨m,r,hm,hr,hs⟩
    exact ⟨(m,r),hm,hr,hs⟩
  · rintro ⟨⟨m,r⟩,hm,hr,hs⟩
    exact ⟨m,r,hm,hr,hs⟩

/-- At known population and composition the finite stopping-count set
attains exactly when it is nonempty. -/
theorem fixed_population_mark_attainment {h N m : ℕ} (x : Fin h → ℕ) :
    let D : Set (ℕ × ℕ) := {p | p.1 ≤ N ∧ p.2 ≤ p.1 ∧ p.1 = m ∧
      ∀ i, Compatible N p.1 p.2 (x i)}
    D.Finite ∧ (D.Nonempty ↔ ∃ p ∈ D,
      IsMaxOn (fun q ↦ totalLikelihood x N q.1 q.2) D p) :=
  finite_grid x (fun p ↦ p.1 = m)

/-- At known population and stopping count the finite composition set
attains exactly when it is nonempty. -/
theorem fixed_population_stop_attainment {h N r : ℕ} (x : Fin h → ℕ) :
    let D : Set (ℕ × ℕ) := {p | p.1 ≤ N ∧ p.2 ≤ p.1 ∧ p.2 = r ∧
      ∀ i, Compatible N p.1 p.2 (x i)}
    D.Finite ∧ (D.Nonempty ↔ ∃ p ∈ D,
      IsMaxOn (fun q ↦ totalLikelihood x N q.1 q.2) D p) :=
  finite_grid x (fun p ↦ p.2 = r)

/-- With positive stopping count and held composition, the black count
can be optimized by the first crossing. The successor is retained exactly
when the adjacent likelihoods tie. -/
theorem held_mark_stop_positive {h m r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hr : 0 < r) (hm : r ≤ m) (hx : ∀ i, r ≤ x i) :
    let y := fun i ↦ x i-r
    let D := Ici (maxBlackDraws y)
    ∃ b ∈ D, whiteRatio r m (m+b+1) y ≤ 1 ∧
      (∀ j ∈ D, j < b → 1 < whiteRatio r m (m+j+1) y) ∧
      ∀ j, (IsMaxOn (fun n ↦ totalLikelihood x (m+n) m r) D j ∧ j ∈ D) ↔
        j = b ∨ (j = b+1 ∧ whiteRatio r m (m+b+1) y = 1) := by
  dsimp
  let y := fun i ↦ x i-r
  obtain ⟨b,hb,hbr,hpre,hmax⟩ := white_maximizers hh y hr hm
  obtain ⟨C,hC,hscale⟩ := mass_product_eq r y
  have he (j : ℕ) (hj : j ∈ Ici (maxBlackDraws y)) :
      totalLikelihood x (m+j) m r = C * likelihood r m j y := by
    rw [← hscale]
    unfold totalLikelihood
    apply Finset.prod_congr rfl
    intro i _
    have hji := (white_feasible_iff y j).mp hj i
    have he : m+j-m = j := by omega
    have hs : Compatible (m+j) m r (x i) := by
      exact ⟨by omega, hm, by rw [ite_eq_right hr.ne', he]; exact ⟨hx i,hji⟩⟩
    rw [totalMass, ite_eq_left hs, he]
  refine ⟨b,hb,hbr,hpre,fun j ↦ ?_⟩
  refine Iff.trans ?_ (hmax j)
  constructor <;> rintro ⟨hM,hJ⟩
  · refine ⟨fun k hk ↦ ?_, hJ⟩
    have he' := hM hk
    change totalLikelihood x (m+k) m r ≤ totalLikelihood x (m+j) m r at he'
    rw [he k hk, he j hJ] at he'
    exact le_of_mul_le_mul_left he' hC
  · refine ⟨fun k hk ↦ ?_, hJ⟩
    change totalLikelihood x (m+k) m r ≤ totalLikelihood x (m+j) m r
    rw [he k hk, he j hJ]
    exact mul_le_mul_of_nonneg_left (hM hk) hC.le

/-- When the held stopping count is zero, compatibility requires all
observations to be zero and every population above the held composition ties. -/
theorem held_mark_stop_zero {h m : ℕ} (x : Fin h → ℕ) :
    (∃ N ≥ m, ∀ i, Compatible N m 0 (x i)) ↔ ∀ i, x i = 0 := by
  constructor
  · rintro ⟨N,_,H⟩ i
    simpa using (H i).2.2
  · intro hz
    exact ⟨m,le_rfl,fun i ↦ by simp [Compatible,hz i]⟩

/-- On compatible zeros with the stopping count held at zero, the
likelihood equals one throughout the population ray. -/
theorem held_zero_stop_flat {h N m : ℕ} (x : Fin h → ℕ) (hm : m ≤ N)
    (hx : ∀ i, x i = 0) : totalLikelihood x N m 0 = 1 := by
  simp [totalLikelihood, hx, zero_mass hm]

private theorem held_mark_mass_limit (m r t : ℕ) (hr : 0 < r) (hm : r ≤ m) :
    Tendsto (fun n : ℕ ↦ totalMass (m+n) m r t) atTop (𝓝 0) := by
  let C : ℝ := ((t-1).choose (r-1) : ℝ) * m.descFactorial r
  have hN : Tendsto (fun n : ℕ ↦ (m : ℝ)+n) atTop atTop :=
    tendsto_atTop_add_const_left atTop (m : ℝ) tendsto_natCast_atTop_atTop
  have ht : Tendsto (fun n : ℕ ↦ C / ((m : ℝ)+n)) atTop (𝓝 0) := by
    simpa [div_eq_mul_inv] using (tendsto_inv_atTop_zero.comp hN).const_mul C
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ht
    (Filter.Eventually.of_forall fun n ↦ total_mass_nonneg (m+n) m r t) ?_
  apply Filter.Eventually.of_forall
  intro n
  have htot : 0 < m+n := by omega
  by_cases hc : Compatible (m+n) m r t
  · have hsup := hc.2.2
    rw [ite_eq_right hr.ne'] at hsup
    have hre : r+(t-r) = t := Nat.add_sub_of_le hsup.1
    have hn : (m+n)-m = n := by omega
    have htN : t ≤ m+n := by rw [hn] at hsup; omega
    have hd : ((m+n-r).descFactorial (t-r)) * ((m+n).descFactorial r) =
        (m+n).descFactorial t := Nat.descFactorial_mul_descFactorial hsup.1
    have hd' : (m+n-1).descFactorial (r-1) * (m+n) =
        (m+n).descFactorial r := by
      simpa using Nat.descFactorial_mul_descFactorial (n := m+n) (show 1 ≤ r by omega)
    have hmono : n.descFactorial (t-r) ≤ (m+n-r).descFactorial (t-r) := by
      rw [Nat.descFactorial_eq_prod_range, Nat.descFactorial_eq_prod_range]
      exact Finset.prod_le_prod fun i _ ↦ by omega
    have hD : 0 < (↑((m+n-r).descFactorial (t-r)) : ℝ) :=
      Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr (by omega))
    have hd1 : (m+n) ≤ (m+n).descFactorial r := by
      have hx : 1 ≤ (m+n-1).descFactorial (r-1) :=
        Nat.descFactorial_pos.mpr (by omega)
      rw [← hd']
      simpa using Nat.mul_le_mul_right (m+n) hx
    have hN' : 0 < ((m+n : ℕ) : ℝ) := Nat.cast_pos.mpr htot
    have hbound : (↑(n.descFactorial (t-r)) : ℝ) * (↑(m+n : ℕ) : ℝ) ≤
        (↑((m+n).descFactorial t) : ℝ) := by
      exact_mod_cast (show n.descFactorial (t-r) * (m+n) ≤ (m+n).descFactorial t by
        rw [← hd]
        exact Nat.mul_le_mul hmono hd1)
    have hD' : 0 < (↑((m+n).descFactorial t) : ℝ) :=
      Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr htN)
    rw [totalMass, ite_eq_left hc, mass, reducedMass, hn, hre]
    change ((t-1).choose (r-1) : ℝ) *
      ((m.descFactorial r : ℝ) * n.descFactorial (t-r) / (m+n).descFactorial t)
      ≤ C / ((m : ℝ)+n)
    rw [← Nat.cast_add]
    have hm' : (n.descFactorial (t-r) : ℝ) / (m+n).descFactorial t ≤
        1 / (↑(m+n : ℕ) : ℝ) := by
      rw [div_le_div_iff₀ hD' hN', one_mul]
      exact hbound
    calc
      _ = C * ((n.descFactorial (t-r) : ℝ) / (m+n).descFactorial t) := by
        unfold C
        ring
      _ ≤ C * (1 / (↑(m+n : ℕ) : ℝ)) := mul_le_mul_of_nonneg_left hm' (by unfold C; positivity)
      _ = _ := by ring
  · rw [totalMass, ite_eq_right hc]
    exact div_nonneg (by unfold C; positivity) (by positivity)

/-- With positive stopping count and composition held, the likelihood
vanishes along the population ray. -/
theorem held_mark_stop_limit {h m r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hr : 0 < r) (hm : r ≤ m) :
    Tendsto (fun b : ℕ ↦ totalLikelihood x (m+b) m r) atTop (𝓝 0) := by
  have ht := tendsto_finsetProd Finset.univ
    (fun i _ ↦ held_mark_mass_limit m r (x i) hr hm)
  have he : (∏ _ : Fin h, (0 : ℝ)) = 0 :=
    Finset.prod_eq_zero (Finset.mem_univ ⟨0,hh⟩) rfl
  simpa only [totalLikelihood, he] using ht

private theorem total_mass_pos {N m r t : ℕ} (h : Compatible N m r t) :
    0 < totalMass N m r t := by
  by_cases hr : r = 0
  · subst r
    have ht : t = 0 := by simpa using h.2.2
    subst t
    simp [zero_mass h.1]
  · have hs := h.2.2
    rw [ite_eq_right hr] at hs
    have hN := h.1
    have hm := h.2.1
    have h₁ : 0 < (m.descFactorial r : ℝ) := Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hm)
    have h₂ : 0 < ((N-m).descFactorial (t-r) : ℝ) :=
      Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hs.2)
    have h₃ : 0 < ((m+(N-m)).descFactorial (r+(t-r)) : ℝ) :=
      Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr (by omega))
    have h₄ : 0 < ((r+(t-r)-1).choose (r-1) : ℝ) := Nat.cast_pos.mpr (Nat.choose_pos (by omega))
    rw [totalMass, ite_eq_left h, mass, reducedMass]
    exact mul_pos h₄ (div_pos (mul_pos h₁ h₂) h₃)

/-- With composition held, positive data have a finite population cutoff
against any positive likelihood witness, uniformly over feasible stopping counts. -/
theorem held_mark_positive_cutoff {h m : ℕ} (x : Fin h → ℕ)
    (hx : ∃ i, 0 < x i) {C : ℝ} (hC : 0 < C) :
    ∃ B : ℕ, ∀ N ≥ B, ∀ r, (∀ i, Compatible N m r (x i)) →
      totalLikelihood x N m r < C := by
  obtain ⟨i,hi⟩ := hx
  have htail : ∀ᶠ n in atTop, ∀ r ∈ Finset.range (m+1), 0 < r →
      totalLikelihood x (m+n) m r < C := by
    apply (eventually_all_finset _).mpr
    intro r hr
    by_cases hz : r = 0
    · exact Filter.Eventually.of_forall fun _ h ↦ by omega
    · have hrm : r ≤ m := Nat.lt_succ_iff.mp (Finset.mem_range.mp hr)
      exact ((held_mark_stop_limit i.isLt.pos x (Nat.pos_of_ne_zero hz) hrm).eventually_lt_const hC)
        |>.mono (fun _ hn _ ↦ hn)
  obtain ⟨b,hb⟩ := eventually_atTop.mp htail
  refine ⟨m+b,fun N hN r hr ↦ ?_⟩
  have hrm := (hr i).2.1
  have hs := (hr i).2.2
  have hr₀ : 0 < r := by
    by_contra! hn
    have he : r = 0 := by omega
    simp [he] at hs
    omega
  have he : m+(N-m) = N := by omega
  simpa only [he] using hb (N-m) (by omega) r
    (Finset.mem_range.mpr (Nat.lt_succ_of_le hrm)) hr₀

/-- With composition held, every feasible sample attains a maximum.
The zero branch retains every compatible urn with stopping count zero. -/
theorem held_mark_attainment {h m : ℕ} (x : Fin h → ℕ)
    (hfeas : ∃ N r, m ≤ N ∧ r ≤ m ∧ ∀ i, Compatible N m r (x i)) :
    ∃ N r, m ≤ N ∧ r ≤ m ∧ (∀ i, Compatible N m r (x i)) ∧
      ∀ M s, m ≤ M → s ≤ m → (∀ i, Compatible M m s (x i)) →
        totalLikelihood x M m s ≤ totalLikelihood x N m r := by
  by_cases hx : ∀ i, x i = 0
  · refine ⟨m,0,le_rfl,Nat.zero_le _,fun i ↦ by simp [Compatible,hx i],
      fun M s _ _ _ ↦ ?_⟩
    rw [held_zero_stop_flat x le_rfl hx]
    exact Finset.prod_le_one₀ (fun i _ ↦ total_mass_nonneg M m s (x i))
      (fun i _ ↦ total_mass_le_one M m s (x i))
  · push Not at hx
    obtain ⟨i,hi⟩ := hx
    obtain ⟨N₀,r₀,hm₀,hr₀,h₀⟩ := hfeas
    have hC : 0 < totalLikelihood x N₀ m r₀ :=
      Finset.prod_pos fun i _ ↦ total_mass_pos (h₀ i)
    obtain ⟨B,hB⟩ := held_mark_positive_cutoff x ⟨i, Nat.pos_of_ne_zero hi⟩ hC
    have hN₀ : N₀ < B := by
      by_contra! ht
      exact (lt_irrefl _) (hB N₀ ht r₀ h₀)
    let grid := ((Finset.range B).product (Finset.range (m+1))).filter
      fun p ↦ m ≤ p.1 ∧ ∀ i, Compatible p.1 m p.2 (x i)
    have hg (N r : ℕ) : (N,r) ∈ grid ↔
        N < B ∧ r ≤ m ∧ m ≤ N ∧ ∀ i, Compatible N m r (x i) := by
      simp [grid,and_assoc]
    have hmem := (hg N₀ r₀).mpr ⟨hN₀,hr₀,hm₀,h₀⟩
    obtain ⟨⟨N,r⟩,hN,hmax⟩ := grid.exists_max_image (fun p ↦ totalLikelihood x p.1 m p.2)
      ⟨(N₀,r₀),hmem⟩
    obtain ⟨_,hr,hm,hs⟩ := (hg N r).mp hN
    refine ⟨N,r,hm,hr,hs,fun M s hm' hs' hMS ↦ ?_⟩
    by_cases ht : M < B
    · exact hmax (M,s) ((hg M s).mpr ⟨ht,hs',hm',hMS⟩)
    · exact (hB M (Nat.le_of_not_gt ht) s hMS).le.trans (hmax (N₀,r₀) hmem)

private theorem finite_crossing {l u : ℕ} (hlu : l ≤ u) (f R : ℕ → ℝ)
    (hf : ∀ a ∈ Icc l u, 0 < f a)
    (he : ∀ a ∈ Ico l u, f (a+1) = f a*R a)
    (hR : StrictAntiOn R (Ico l u)) :
    ∃ b ∈ Icc l u, (b < u → R b ≤ 1) ∧
      (∀ j ∈ Ico l u, j < b → 1 < R j) ∧
      ∀ j ∈ Icc l u, IsMaxOn f (Icc l u) j ↔
        j = b ∨ (j = b+1 ∧ b < u ∧ R b = 1) := by
  classical
  let P := fun j ↦ j ∈ Ico l u ∧ R j ≤ 1
  let b := if h : ∃ j, P j then Nat.find h else u
  have hb : b ∈ Icc l u := by
    by_cases h : ∃ j, P j
    · have hh := Nat.find_spec h
      simpa only [b,dite_eq_left h] using (show Nat.find h ∈ Icc l u from ⟨hh.1.1,hh.1.2.le⟩)
    · simp [b,h,hlu]
  have hr (hbu : b < u) : R b ≤ 1 := by
    by_cases h : ∃ j, P j
    · simpa only [b,dite_eq_left h] using (Nat.find_spec h).2
    · simp [b,h] at hbu
  have hl (j : ℕ) (hj : j ∈ Ico l u) (hjb : j < b) : 1 < R j := by
    by_contra! hx
    have h : ∃ j, P j := ⟨j,hj,hx⟩
    have hle : b ≤ j := by
      simpa only [b,dite_eq_left h] using Nat.find_min' h ⟨hj,hx⟩
    exact hjb.not_ge hle
  have hincr : StrictMonoOn f (Icc l b) := by
    apply strictMonoOn_of_lt_succ ordConnected_Icc
    intro a _ ha ha'
    change a+1 ∈ Icc l b at ha'
    have hab : a < b := Nat.lt_of_succ_le ha'.2
    have hAu : a ∈ Ico l u := ⟨ha.1,hab.trans_le hb.2⟩
    change f a < f (a+1)
    rw [he a hAu]
    exact lt_mul_of_one_lt_right (hf a ⟨ha.1,hAu.2.le⟩) (hl a hAu hab)
  have hdecr : StrictAntiOn f (Icc (b+1) u) := by
    apply strictAntiOn_of_succ_lt ordConnected_Icc
    intro a _ ha ha'
    change a+1 ∈ Icc (b+1) u at ha'
    have hAb : b < a := ha.1
    have hAu : a ∈ Ico l u := ⟨hb.1.trans hAb.le,Nat.lt_of_succ_le ha'.2⟩
    have hB : b ∈ Ico l u := ⟨hb.1,hAb.trans hAu.2⟩
    have hRa : R a < 1 := (hR hB hAu hAb).trans_le (hr hB.2)
    change f (a+1) < f a
    rw [he a hAu]
    exact mul_lt_of_lt_one_right (hf a ⟨hAu.1,hAu.2.le⟩) hRa
  have hbmax : IsMaxOn f (Icc l u) b := by
    intro j hj
    change f j ≤ f b
    by_cases h : j ≤ b
    · exact hincr.monotoneOn ⟨hj.1,h⟩ ⟨hb.1,le_rfl⟩ h
    · have hbj : b+1 ≤ j := Nat.lt_of_not_ge h
      have hbu : b < u := hbj.trans hj.2
      exact (hdecr.antitoneOn ⟨le_rfl,hbj.trans hj.2⟩ ⟨hbj,hj.2⟩ hbj).trans (by
        rw [he b ⟨hb.1,hbu⟩]
        exact mul_le_of_le_one_right (hf b hb).le (hr hbu))
  refine ⟨b,hb,hr,hl,fun j hj ↦ ?_⟩
  constructor
  · intro hM
    rcases lt_trichotomy j b with h | h | h
    · exact ((hincr ⟨hj.1,h.le⟩ ⟨hb.1,le_rfl⟩ h).not_ge (hM hb)).elim
    · exact Or.inl h
    · have hbu : b < u := h.trans_le hj.2
      have hb' : b+1 ∈ Icc l u := ⟨hb.1.trans (Nat.le_succ _),hbu⟩
      have hjb : j = b+1 := by
        by_contra hn
        have he' : b+1 < j := lt_of_le_of_ne h (Ne.symm hn)
        exact (hdecr ⟨le_rfl,hb'.2⟩ ⟨h,hj.2⟩ he').not_ge (hM hb')
      refine Or.inr ⟨hjb,hbu,?_⟩
      have H := le_antisymm (hbmax hj) (hM hb)
      change f j = f b at H
      rw [hjb, he b ⟨hb.1,hbu⟩] at H
      exact mul_left_cancel₀ (hf b hb).ne' (by simpa only [mul_one] using
        (show f b*R b = f b*1 by simpa only [mul_one] using H))
  · rintro (rfl | ⟨rfl,hbu,hRone⟩)
    · exact hbmax
    · intro a ha
      change f a ≤ f (b+1)
      rw [he b ⟨hb.1,hbu⟩,hRone,mul_one]
      exact hbmax ha

private theorem composition_mass_step {N m r t : ℕ} (hr : 0 < r)
    (ha : Compatible N m r t) (hb : Compatible N (m+1) r t) :
    totalMass N (m+1) r t = totalMass N m r t *
      (((m : ℝ)+1)/((m : ℝ)+1-r) *
        (((N : ℝ)-m-(t-r : ℕ))/((N : ℝ)-m))) := by
  have hm := ha.1
  have hrm := ha.2.1
  have hM := hb.1
  have hS := hb.2.2
  rw [ite_eq_right hr.ne'] at hS
  have hx : t-r ≤ N-m := by omega
  have hN : N-(m+1)+1 = N-m := by omega
  have hs (q d : ℕ) : (q.descFactorial d : ℝ) * (q+1 : ℕ) =
      ((q+1).descFactorial d : ℝ) * (q+1-d : ℕ) := by
    exact_mod_cast (show q.descFactorial d*(q+1) =
      (q+1).descFactorial d*(q+1-d) by
      rw [Nat.mul_comm (q.descFactorial d), ← Nat.succ_descFactorial_succ,
        Nat.descFactorial_succ, Nat.mul_comm])
  have h₁ := hs m r
  have h₂ := hs (N-(m+1)) (t-r)
  rw [hN] at h₂
  have hm' : 0 < (m : ℝ)+1-r := sub_pos.mpr (by exact_mod_cast (show r < m+1 by omega))
  have hn' : 0 < (N : ℝ)-m := sub_pos.mpr (by exact_mod_cast (show m < N by omega))
  have htN : r+(t-r) ≤ N := by omega
  have hd : (N.descFactorial (r+(t-r)) : ℝ) ≠ 0 :=
    (Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr htN)).ne'
  push_cast [Nat.cast_sub (show r ≤ m+1 by omega),Nat.cast_sub hx,Nat.cast_sub hm] at h₁ h₂
  rw [totalMass,ite_eq_left hb,totalMass,ite_eq_left ha,mass,mass,reducedMass,reducedMass,
    Nat.add_sub_of_le hm,Nat.add_sub_of_le hM]
  field_simp [hm'.ne',hn'.ne',hd]
  linear_combination ((r+(t-r)-1).choose (r-1) : ℝ) *
    ((N-m-1).descFactorial (t-r) : ℝ) * ((N : ℝ)-m) * (-h₁) +
    ((r+(t-r)-1).choose (r-1) : ℝ) * (m.descFactorial r : ℝ) * ((m : ℝ)+1) * h₂

/-- The exact adjacent-composition ratio at a held population and
positive stopping count. -/
noncomputable def compositionRatio {h : ℕ} (x : Fin h → ℕ) (N r m : ℕ) : ℝ :=
  ∏ i, (((m : ℝ)+1)/((m : ℝ)+1-r)) * (((N : ℝ)-m-(x i-r : ℕ))/((N : ℝ)-m))

/-- At fixed population and positive stopping count the complete set of
maximizers is the first crossing and its successor precisely on equality.
If there is no crossing, the last feasible composition is the sole fit. -/
theorem fixed_population_stop_crossing {h N r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hr : 0 < r) (hx : ∀ i, r ≤ x i) (hfeas : ∃ m, ∀ i, Compatible N m r (x i)) :
    let u := N - maxBlackDraws (fun i ↦ x i-r)
    let D := Icc r u
    ∃ b ∈ D, (b < u → compositionRatio x N r b ≤ 1) ∧
      (∀ j ∈ Ico r u, j < b → 1 < compositionRatio x N r j) ∧
      ∀ j ∈ D, IsMaxOn (fun m ↦ totalLikelihood x N m r) D j ↔
        j = b ∨ (j = b+1 ∧ b < u ∧ compositionRatio x N r b = 1) := by
  intro u D
  let d := maxBlackDraws (fun i ↦ x i-r)
  have hd : ∀ i, x i-r ≤ d := (white_feasible_iff _ d).mp (mem_Ici.mpr le_rfl)
  have hD' (m : ℕ) (hm : m ∈ D) (i : Fin h) : Compatible N m r (x i) := by
    have ha : r ≤ m := hm.1
    have hb : m ≤ N-d := hm.2
    refine ⟨hb.trans (Nat.sub_le _ _),ha,?_⟩
    rw [ite_eq_right hr.ne']
    exact ⟨hx i,(hd i).trans (by
      have hdN : d ≤ N := by
        obtain ⟨m,hm⟩ := hfeas
        apply (white_feasible_iff _ N).mpr
        intro j
        have h := (hm j).2.2
        rw [ite_eq_right hr.ne'] at h
        exact h.2.trans (Nat.sub_le _ _)
      omega)⟩
  have hru : r ≤ u := by
    obtain ⟨m,hm⟩ := hfeas
    have hrm := (hm ⟨0,hh⟩).2.1
    have hmn := (hm ⟨0,hh⟩).1
    have hdm : d ≤ N-m := (white_feasible_iff _ _).mpr fun i ↦ by
      have h := (hm i).2.2
      rw [ite_eq_right hr.ne'] at h
      exact h.2
    change r ≤ N-d
    omega
  refine finite_crossing hru _ _ (fun m hm ↦ Finset.prod_pos fun i _ ↦ total_mass_pos (hD' m hm i))
    ?_ ?_
  · intro m hm
    have h₁ : m ∈ D := ⟨hm.1,hm.2.le⟩
    have h₂ : m+1 ∈ D := ⟨by have := hm.1; omega, hm.2⟩
    rw [totalLikelihood,totalLikelihood,compositionRatio,← Finset.prod_mul_distrib]
    exact Finset.prod_congr rfl fun i _ ↦ composition_mass_step hr (hD' m h₁ i) (hD' _ h₂ i)
  · intro a ha b hb hab
    have hb' : b+1 ∈ D := ⟨by have := hb.1; omega,hb.2⟩
    have haR : (r : ℝ) ≤ a := by exact_mod_cast ha.1
    have hrR : 0 < (r : ℝ) := Nat.cast_pos.mpr hr
    have habR : (a : ℝ) < b := by exact_mod_cast hab
    unfold compositionRatio
    apply Finset.prod_lt_prod_of_nonempty₀
    · intro i _
      have H := (hD' _ hb' i).2.2
      rw [ite_eq_right hr.ne'] at H
      have hc' : (b : ℝ)+1+(x i-r : ℕ) ≤ N := by
        exact_mod_cast (show b+1+(x i-r) ≤ N by
          have := (hD' _ hb' i).1
          omega)
      exact mul_pos (div_pos (by positivity) (by linarith))
        (div_pos (by linarith) (by linarith [show (0 : ℝ) ≤ (x i-r : ℕ) from Nat.cast_nonneg _]))
    · intro i _
      let y : ℝ := (x i-r : ℕ)
      have hby : (b : ℝ)+1+y ≤ N := by
        dsimp [y]
        have H := (hD' _ hb' i).2.2
        rw [ite_eq_right hr.ne'] at H
        exact_mod_cast (show b+1+(x i-r) ≤ N by
          have := (hD' _ hb' i).1
          omega)
      have hy : 0 ≤ y := Nat.cast_nonneg _
      have hbN : 0 < (N : ℝ)-b := by linarith
      have haN : 0 < (N : ℝ)-a := by linarith
      have h₁ : ((b : ℝ)+1)/((b : ℝ)+1-r) < ((a : ℝ)+1)/((a : ℝ)+1-r) := by
        rw [div_lt_div_iff₀ (by linarith) (by linarith)]
        nlinarith only [mul_pos hrR (sub_pos.mpr habR)]
      have h₂ : (((N : ℝ)-b-y)/((N : ℝ)-b)) ≤
          (((N : ℝ)-a-y)/((N : ℝ)-a)) := by
        rw [div_le_div_iff₀ hbN haN]
        nlinarith only [mul_nonneg hy (sub_nonneg.mpr habR.le)]
      exact (mul_lt_mul_of_pos_right h₁ (div_pos (by linarith) hbN)).trans_le
        (mul_le_mul_of_nonneg_left h₂ (div_nonneg (by positivity) (by linarith)))
    · exact ⟨⟨0,hh⟩,Finset.mem_univ _⟩

private theorem stopping_mass_step {N m r t : ℕ} (hr : 0 < r)
    (ha : Compatible N m r t) (hb : Compatible N m (r+1) t) :
    totalMass N m (r+1) t = totalMass N m r t *
      (((t : ℝ)-r)*((m : ℝ)-r) / ((r : ℝ)*((N : ℝ)-m-t+r+1))) := by
  have hm := ha.1
  have hrm := hb.2.1
  have hsa := ha.2.2
  have hsb := hb.2.2
  rw [ite_eq_right hr.ne'] at hsa
  rw [ite_eq_right (Nat.succ_ne_zero r)] at hsb
  have ht : t ≤ N := by have := ha.2.1; omega
  have ht₀ : t-r = (t-(r+1))+1 := by omega
  have hr₀ : r-1+1 = r := Nat.sub_add_cancel hr
  have hc₀ : t-1-(r-1) = t-r := by omega
  have hc : ((t-1).choose r : ℝ)*r = ((t-1).choose (r-1) : ℝ)*(t-r : ℕ) := by
    exact_mod_cast (show (t-1).choose r * r = (t-1).choose (r-1)*(t-r) by
      simpa only [hr₀,hc₀] using Nat.choose_succ_right_eq (t-1) (r-1))
  have hf : (N-m).descFactorial (t-r) =
      (N-m-(t-(r+1)))*(N-m).descFactorial (t-(r+1)) := by
    conv_lhs => rw [ht₀,Nat.descFactorial_succ]
  have hr' : 0 < (r : ℝ) := Nat.cast_pos.mpr hr
  have hd : 0 < (N.descFactorial t : ℝ) := Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr ht)
  have hx : (t : ℝ) ≤ (N : ℝ)-m+r := by
    have h : t+m ≤ N+r := by omega
    have h' : (t : ℝ)+m ≤ (N : ℝ)+r := by exact_mod_cast h
    linarith
  have hD : 0 < (N : ℝ)-m-t+r+1 := by linarith
  have hT : ((N-m-(t-(r+1)) : ℕ) : ℝ) = (N : ℝ)-m-t+r+1 := by
    rw [Nat.cast_sub hsb.2,Nat.cast_sub hm,Nat.cast_sub hsb.1,Nat.cast_add,Nat.cast_one]
    ring
  have hR : r+(t-r) = t := Nat.add_sub_of_le hsa.1
  have hR' : r+1+(t-(r+1)) = t := Nat.add_sub_of_le hsb.1
  rw [totalMass,ite_eq_left hb,totalMass,ite_eq_left ha,mass,mass,reducedMass,reducedMass,
    hR,hR',Nat.add_sub_of_le hm,Nat.add_sub_cancel r 1,Nat.descFactorial_succ m r,hf,
    Nat.cast_mul,Nat.cast_mul,hT,Nat.cast_sub (show r ≤ m by omega)]
  rw [Nat.cast_sub hsa.1] at hc
  field_simp [hr'.ne',hd.ne',hD.ne']
  linear_combination ((m : ℝ)-r) * (m.descFactorial r : ℝ) *
    ((N-m).descFactorial (t-(r+1)) : ℝ) * hc

/-- The exact adjacent stopping-count ratio at held population and composition. -/
noncomputable def stoppingRatio {h : ℕ} (x : Fin h → ℕ) (N m r : ℕ) : ℝ :=
  ∏ i, (((x i : ℝ)-r)*((m : ℝ)-r) /
    ((r : ℝ)*((N : ℝ)-m-x i+r+1)))

/-- At held population and composition on positive data the first
nonpositive ratio crossing identifies every winning stopping count,
including an adjacent tie and the terminal-size case. -/
theorem fixed_population_mark_crossing {h N m : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hx : ∀ i, 0 < x i) (hfeas : ∃ r, ∀ i, Compatible N m r (x i)) :
    let l := max 1 (Finset.univ.sup x-(N-m))
    let u := min m (Finset.univ.inf' ⟨⟨0,hh⟩,Finset.mem_univ _⟩ x)
    let D := Icc l u
    ∃ b ∈ D, (b < u → stoppingRatio x N m b ≤ 1) ∧
      (∀ j ∈ Ico l u, j < b → 1 < stoppingRatio x N m j) ∧
      ∀ j ∈ D, IsMaxOn (fun r ↦ totalLikelihood x N m r) D j ↔
        j = b ∨ (j = b+1 ∧ b < u ∧ stoppingRatio x N m b = 1) := by
  intro l u D
  obtain ⟨R,hR⟩ := hfeas
  have hm := (hR ⟨0,hh⟩).1
  have hR₀ : R ≠ 0 := by
    intro h
    have hs := (hR ⟨0,hh⟩).2.2
    simp [h] at hs
    exact (hx ⟨0,hh⟩).ne' hs
  have hR' (i : Fin h) : R ≤ x i ∧ x i-R ≤ N-m := by
    simpa only [ite_eq_right hR₀] using (hR i).2.2
  have H (i : Fin h) : x i ≤ Finset.univ.sup x := Finset.le_sup (Finset.mem_univ i)
  have hl : l ≤ R := by
    dsimp [l]
    apply max_le (Nat.pos_of_ne_zero hR₀)
    have he : Finset.univ.sup x ≤ R+(N-m) :=
      Finset.sup_le fun i _ ↦ by have := (hR' i).2; omega
    omega
  have hu : R ≤ u := by
    dsimp [u]
    exact le_min ((hR ⟨0,hh⟩).2.1) (Finset.le_inf' _ _ fun i _ ↦ (hR' i).1)
  have hD (r : ℕ) (hr : r ∈ D) (i : Fin h) : Compatible N m r (x i) := by
    have hr₀ : 0 < r := (le_max_left _ _).trans hr.1
    have hrm : r ≤ m := hr.2.trans (min_le_left _ _)
    have hmin := Finset.inf'_le x (Finset.mem_univ i)
    have hrx : r ≤ x i := hr.2.trans ((min_le_right _ _).trans hmin)
    have hl' : Finset.univ.sup x-(N-m) ≤ r := (le_max_right _ _).trans hr.1
    refine ⟨hm,hrm,?_⟩
    rw [ite_eq_right hr₀.ne']
    exact ⟨hrx,by have := H i; omega⟩
  refine finite_crossing (hl.trans hu) _ _
    (fun r hr ↦ Finset.prod_pos fun i _ ↦ total_mass_pos (hD r hr i)) ?_ ?_
  · intro r hr
    have hr₀ : 0 < r := (le_max_left _ _).trans hr.1
    have hr' : r+1 ∈ D := ⟨hr.1.trans (Nat.le_succ _),hr.2⟩
    have hr'' : r ∈ D := ⟨hr.1,hr.2.le⟩
    rw [totalLikelihood,totalLikelihood,stoppingRatio,← Finset.prod_mul_distrib]
    exact Finset.prod_congr rfl fun i _ ↦ stopping_mass_step hr₀ (hD r hr'' i) (hD _ hr' i)
  · intro a ha b hb hab
    have ha₀ : 0 < (a : ℝ) := Nat.cast_pos.mpr ((le_max_left _ _).trans ha.1)
    have hab' : (a : ℝ) < b := by exact_mod_cast hab
    have hbm : (b : ℝ) < m := by exact_mod_cast
      (show b < m from hb.2.trans_le (min_le_left _ _))
    have hb' : b+1 ∈ D := ⟨hb.1.trans (Nat.le_succ _),hb.2⟩
    have ha' : a ∈ D := ⟨ha.1,ha.2.le⟩
    have hxa (i : Fin h) : (x i : ℝ) ≤ (N : ℝ)-m+a := by
      have H := (hD a ha' i).2.2
      rw [ite_eq_right (Nat.cast_pos.mp ha₀).ne'] at H
      have ht : (x i : ℝ)+m ≤ (N : ℝ)+a := by exact_mod_cast (show x i+m ≤ N+a by omega)
      linarith
    have hxb (i : Fin h) : (b : ℝ) < x i := by
      have H := (hD _ hb' i).2.2
      rw [ite_eq_right (Nat.succ_ne_zero b)] at H
      exact_mod_cast (show b < x i from H.1)
    unfold stoppingRatio
    apply Finset.prod_lt_prod_of_nonempty₀
    · intro i _
      exact div_pos (mul_pos (sub_pos.mpr (hxb i)) (sub_pos.mpr hbm))
        (mul_pos (ha₀.trans hab') (by linarith [hxa i]))
    · intro i _
      have hA : 0 < (N : ℝ)-m-x i+a+1 := by linarith [hxa i]
      have hB : 0 < (N : ℝ)-m-x i+b+1 := by linarith
      have hn : ((x i : ℝ)-b)*((m : ℝ)-b) < ((x i : ℝ)-a)*((m : ℝ)-a) :=
        mul_lt_mul_of_pos_of_nonneg (by linarith) (by linarith) (by linarith [hxb i])
          (by linarith [hbm])
      have hd : (a : ℝ)*((N : ℝ)-m-x i+a+1) ≤ (b : ℝ)*((N : ℝ)-m-x i+b+1) :=
        mul_le_mul hab'.le (by linarith) hA.le (ha₀.trans hab').le
      exact (div_lt_div_of_pos_right hn (mul_pos (ha₀.trans hab') hB)).trans_le
        (div_le_div_of_nonneg_left (mul_nonneg (sub_nonneg.mpr (hab'.trans (hxb i)).le)
          (sub_nonneg.mpr (hab'.trans hbm).le)) (mul_pos ha₀ hA) hd)
    · exact ⟨⟨0,hh⟩, Finset.mem_univ _⟩

end UnivariateML.Discrete4NHyper

end
