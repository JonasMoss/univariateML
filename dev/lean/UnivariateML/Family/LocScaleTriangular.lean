module

public import UnivariateML.Family.Triangular
public import UnivariateML.Family.LocScaleCommon

import Mathlib.Analysis.Calculus.LocalExtr.Basic
import Mathlib.Analysis.SpecialFunctions.Log.Deriv
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Triangular widths and support contacts

At a fixed mode the two nonnegative widths give a continuous likelihood
on compatible support with positive total width. A zero width on one
side is allowed when that side contains no observations.

## Deviations

The results model positive mathematical likelihood and the zero value
at incompatible contacts. Rejection by the fitting API is outside
the Lean model.
-/

@[expose] public section

open Set Filter
open scoped Topology
open UnivariateML.Triangular

namespace UnivariateML.LocScaleTriangular

variable {n : ℕ}

/-- At mode `c`, the pair gives the left and right widths. -/
def widthDomain (x : Fin n → ℝ) (c : ℝ) : Set (ℝ × ℝ) :=
  {p | 0 ≤ p.1 ∧ 0 ≤ p.2 ∧ 0 < p.1+p.2 ∧ ∀ i, c-p.1 ≤ x i ∧ x i ≤ c+p.2}

/-- Product likelihood at a fixed mode, including endpoint modes. -/
noncomputable def widthLikelihood (x : Fin n → ℝ) (c : ℝ) (p : ℝ × ℝ) : ℝ :=
  likelihood (c-p.1) (c+p.2) x c

private theorem widthFormula (x : Fin n → ℝ) (c : ℝ) (p : ℝ × ℝ) :
    widthLikelihood x c p = (2/(p.1+p.2))^n * ∏ j,
      (if x j < c then (x j-c+p.1)/p.1 else if c < x j then (c+p.2-x j)/p.2 else 1) := by
  unfold widthLikelihood likelihood factor
  simp only [show c+p.2-(c-p.1) = p.1+p.2 by ring,
    show c-(c-p.1) = p.1 by ring, show c+p.2-c = p.2 by ring,
    show ∀ j, x j-(c-p.1) = x j-c+p.1 by intro j; ring]

private theorem widthContinuous (x : Fin n → ℝ) (c : ℝ) :
    ContinuousOn (widthLikelihood x c) (widthDomain x c) := by
  simp only [show widthLikelihood x c = _ from funext (widthFormula x c)]
  apply ContinuousOn.mul
  · have hden : ContinuousOn (fun p : ℝ × ℝ ↦ p.1+p.2) (widthDomain x c) :=
      (continuous_fst.add continuous_snd).continuousOn
    exact (continuousOn_const.div hden (fun p hp ↦ (show 0 < p.1+p.2 from hp.2.2.1).ne')).pow n
  · apply continuousOn_finsetProd Finset.univ
    intro j _
    by_cases h : x j < c
    · simp only [h, ite_true]
      exact (by fun_prop : ContinuousOn (fun p : ℝ × ℝ ↦ x j-c+p.1) (widthDomain x c)).div
        continuous_fst.continuousOn (fun p hp ↦ by
          have h' := (hp.2.2.2 j).1
          exact ne_of_gt (by linarith : 0 < p.1))
    · by_cases h' : c < x j
      · simp only [h, h', ite_true, ite_false]
        exact (by fun_prop : ContinuousOn (fun p : ℝ × ℝ ↦ c+p.2-x j) (widthDomain x c)).div
          continuous_snd.continuousOn (fun p hp ↦ by
            have h'' := (hp.2.2.2 j).2
            exact ne_of_gt (by linarith : 0 < p.2))
      · simp [h,h',continuousOn_const]

private theorem widthPeak {x : Fin n → ℝ} {c : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ widthDomain x c) : widthLikelihood x c p ≤ (2/(p.1+p.2))^n := by
  rw [widthFormula]
  have hf (j : Fin n) :
      let f := if x j < c then (x j-c+p.1)/p.1 else if c < x j then (c+p.2-x j)/p.2 else 1
      0 ≤ f ∧ f ≤ 1 := by
    rcases hp.2.2.2 j with ⟨ha,hb⟩
    split_ifs with h h
    · have hw : 0 < p.1 := by linarith
      exact ⟨div_nonneg (by linarith) hw.le, (div_le_one hw).mpr (by linarith)⟩
    · have hw : 0 < p.2 := by linarith
      exact ⟨div_nonneg (by linarith) hw.le, (div_le_one hw).mpr (by linarith)⟩
    · norm_num
  have hw : 0 < p.1+p.2 := hp.2.2.1
  apply mul_le_of_le_one_right (by positivity : 0 ≤ (2/(p.1+p.2))^n)
  exact Finset.prod_le_one₀ (fun j _ ↦ (hf j).1) (fun j _ ↦ (hf j).2)

private theorem width_attainment {x : Fin n → ℝ} (hn : 0 < n) {c : ℝ} {H : Set (ℝ × ℝ)}
    (hH : IsClosed H) {δ : ℝ} (hδ : 0 < δ)
    (hbound : ∀ p ∈ widthDomain x c ∩ H, δ ≤ p.1+p.2)
    {p : ℝ × ℝ} (hp : p ∈ widthDomain x c ∩ H) (hL : 0 < widthLikelihood x c p) :
    ∃ q ∈ widthDomain x c ∩ H, 0 < widthLikelihood x c q ∧
      IsMaxOn (widthLikelihood x c) (widthDomain x c ∩ H) q := by
  let W := widthLikelihood x c p
  let R := max (p.1+p.2) (max 2 (2/W))
  let K := (Icc 0 R ×ˢ Icc 0 R) ∩ (widthDomain x c ∩ H)
  let C : Set (ℝ × ℝ) := {q | δ ≤ q.1+q.2} ∩
    (⋂ i, {q | c-q.1 ≤ x i} ∩ {q | x i ≤ c+q.2})
  have hc : IsClosed C := by
    apply IsClosed.inter (isClosed_le continuous_const (continuous_fst.add continuous_snd))
    exact isClosed_iInter fun i ↦ (isClosed_le
      (continuous_const.sub continuous_fst) continuous_const).inter
      (isClosed_le continuous_const (continuous_const.add continuous_snd))
  have he : K = (Icc 0 R ×ˢ Icc 0 R) ∩ (C ∩ H) := by
    ext q
    constructor
    · rintro ⟨hq, h⟩
      exact ⟨hq, ⟨hbound q h, mem_iInter.mpr h.1.2.2.2⟩, h.2⟩
    · rintro ⟨hq, hC, hqH⟩
      exact ⟨hq, ⟨hq.1.1,hq.2.1,hδ.trans_le hC.1,mem_iInter.mp hC.2⟩, hqH⟩
  have hK : IsCompact K := he ▸ (isCompact_Icc.prod isCompact_Icc).inter_right (hc.inter hH)
  have hpK : p ∈ K := by
    have h := le_max_left (p.1+p.2) (max 2 (2/W))
    exact ⟨⟨⟨hp.1.1, (le_add_of_nonneg_right hp.1.2.1).trans h⟩,
      ⟨hp.1.2.1,(le_add_of_nonneg_left hp.1.1).trans h⟩⟩,hp⟩
  obtain ⟨q,hq,hqm⟩ := hK.exists_isMaxOn ⟨p,hpK⟩
    ((widthContinuous x c).mono (fun _ h ↦ h.2.1))
  refine ⟨q,hq.2,hL.trans_le (hqm hpK),fun r hr ↦ ?_⟩
  by_cases hk : r ∈ K
  · exact hqm hk
  have hR : R < r.1+r.2 := by
    by_contra! h
    exact hk ⟨⟨⟨hr.1.1,(le_add_of_nonneg_right hr.1.2.1).trans h⟩,
      ⟨hr.1.2.1,(le_add_of_nonneg_left hr.1.1).trans h⟩⟩,hr⟩
  have hW : 0 < W := hL
  have hrat : 2/(r.1+r.2) ≤ W := by
    apply (div_le_iff₀ hr.1.2.2.1).mpr
    rw [mul_comm]
    exact (div_le_iff₀ hW).mp (((le_max_right 2 (2/W)).trans
      (le_max_right (p.1+p.2) _)).trans hR.le)
  have hrat' : 2/(r.1+r.2) ≤ 1 := (div_le_one hr.1.2.2.1).mpr
    (((le_max_left 2 (2/W)).trans (le_max_right (p.1+p.2) _)).trans hR.le)
  have hpw : (2/(r.1+r.2))^n ≤ 2/(r.1+r.2) := by
    have hz : 0 ≤ 2/(r.1+r.2) := div_nonneg (by norm_num) hr.1.2.2.1.le
    obtain ⟨k,rfl⟩ := Nat.exists_eq_succ_of_ne_zero hn.ne'
    rw [pow_succ]
    exact mul_le_of_le_one_left hz (pow_le_one₀ hz hrat')
  exact ((widthPeak hr.1).trans (hpw.trans hrat)).trans (hqm hpK)

/-- With the mode held, every sample with a nonzero residual has an
attained positive-likelihood fit, allowing either side width to be zero. -/
theorem held_mode_attains (hn : 0 < n) (x : Fin n → ℝ) (c : ℝ)
    (hx : ∃ i, x i ≠ c) :
    ∃ p ∈ widthDomain x c, 0 < widthLikelihood x c p ∧
      IsMaxOn (widthLikelihood x c) (widthDomain x c) p := by
  obtain ⟨i,hi⟩ := hx
  let δ := |x i-c|
  have hδ : 0 < δ := abs_pos.mpr (sub_ne_zero.mpr hi)
  let B := 1 + ∑ j, |x j-c|
  have hp (j : Fin n) : |x j-c| < B := by
    have h := Finset.single_le_sum (f := fun j ↦ |x j-c|)
      (fun j _ ↦ abs_nonneg (x j-c)) (Finset.mem_univ j)
    dsimp [B]
    linarith
  have hB : 0 < B := (abs_nonneg (x i-c)).trans_lt (hp i)
  have hd : (B,B) ∈ widthDomain x c := by
    refine ⟨hB.le,hB.le,add_pos hB hB,fun j ↦ ?_⟩
    obtain ⟨h₁,h₂⟩ := abs_lt.mp (hp j)
    exact ⟨by linarith,by linarith⟩
  have hpos : 0 < widthLikelihood x c (B,B) := by
    apply (likelihood_pos_iff (a := c-B) (b := c+B) (by linarith)
      (fun j ↦ hd.2.2.2 j) (by constructor <;> linarith)).mpr
    refine ⟨fun j h ↦ ?_,fun j h ↦ ?_⟩
    · have h₁ := (abs_lt.mp (hp j)).1
      linarith
    · have h₂ := (abs_lt.mp (hp j)).2
      linarith
  have hδb (p : ℝ × ℝ) (h : p ∈ widthDomain x c ∩ univ) : δ ≤ p.1+p.2 := by
    apply abs_le.mpr
    obtain ⟨h₁,h₂⟩ := h.1.2.2.2 i
    exact ⟨by linarith [h.1.2.1],by linarith [h.1.1]⟩
  simpa only [inter_univ] using
    width_attainment hn isClosed_univ hδ hδb ⟨hd,mem_univ _⟩ hpos

/-- A constant sample at the held mode has unbounded width collapse
through admissible endpoint modes. Both orientations are available. -/
theorem held_mode_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    (∀ s > 0, (0,s) ∈ widthDomain x m ∧ (s,0) ∈ widthDomain x m) ∧
    Tendsto (fun s ↦ widthLikelihood x m (0,s)) (𝓝[>] 0) atTop ∧
    Tendsto (fun s ↦ widthLikelihood x m (s,0)) (𝓝[>] 0) atTop := by
  have h : Tendsto (fun s : ℝ ↦ (2/s)^n) (𝓝[>] 0) atTop :=
    (show Tendsto (fun s : ℝ ↦ 2/s) (𝓝[>] 0) atTop by
      simpa only [div_eq_mul_inv] using
        tendsto_inv_nhdsGT_zero.const_mul_atTop (by norm_num : (0 : ℝ) < 2)).atTop_pow₀ hn.ne'
  refine ⟨?_,?_,?_⟩
  · intro s hs
    exact ⟨⟨le_rfl,hs.le,by simpa,fun _ ↦ by constructor <;> linarith⟩,
      ⟨hs.le,le_rfl,by simpa,fun _ ↦ by constructor <;> linarith⟩⟩
  · exact h.congr fun s ↦ by rw [widthFormula]; simp
  · exact h.congr fun s ↦ by rw [widthFormula]; simp

/-- The admissible joint chart is `(a,b,c)` with positive width and
compatible closed support. -/
def jointDomain (x : Fin n → ℝ) : Set (ℝ × (ℝ × ℝ)) :=
  {p | p.1 < p.2.1 ∧ p.2.2 ∈ Icc p.1 p.2.1 ∧ ∀ i, x i ∈ Icc p.1 p.2.1}

/-- Product likelihood in the joint chart. -/
noncomputable def jointLikelihood (x : Fin n → ℝ) (p : ℝ × (ℝ × ℝ)) : ℝ :=
  likelihood p.1 p.2.1 x p.2.2

private theorem from_widths {x : Fin n → ℝ} {c : ℝ} {q : ℝ × ℝ}
    (hq : q ∈ widthDomain x c) : (c-q.1,c+q.2,c) ∈ jointDomain x := by
  exact ⟨by linarith [hq.2.2.1], ⟨by linarith [hq.1],by linarith [hq.2.1]⟩,hq.2.2.2⟩

private theorem to_widths {x : Fin n → ℝ} {a b c : ℝ} (h : (a,b,c) ∈ jointDomain x) :
    (c-a,b-c) ∈ widthDomain x c := by
  have hw : a < b := h.1
  have hc : c ∈ Icc a b := h.2.1
  refine ⟨sub_nonneg.mpr hc.1,sub_nonneg.mpr hc.2,by linarith,fun i ↦ ?_⟩
  have he := h.2.2 i
  exact ⟨by linarith [he.1],by linarith [he.2]⟩

private theorem widths_joint_value (x : Fin n → ℝ) (a b c : ℝ) :
    widthLikelihood x c (c-a,b-c) = jointLikelihood x (a,b,c) := by
  unfold widthLikelihood jointLikelihood
  change likelihood (c-(c-a)) (c+(b-c)) x c = likelihood a b x c
  rw [show c-(c-a) = a by ring,show c+(b-c) = b by ring]

/-- Nonempty constant samples have an unbounded all-free likelihood. -/
theorem all_free_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (jointLikelihood x '' jointDomain x) := by
  let x : Fin n → ℝ := fun _ ↦ m
  change ¬ BddAbove (jointLikelihood x '' jointDomain x)
  rintro ⟨B,hB⟩
  have h := held_mode_collapse hn m
  have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨s,hs,hgt⟩ := (hs.and (h.2.1.eventually_gt_atTop B)).exists
  have hp : (m,m+s,m) ∈ jointDomain x := by
    refine ⟨by linarith [show 0 < s from hs], ⟨le_rfl,by linarith [show 0 < s from hs]⟩,?_⟩
    exact fun _ ↦ ⟨le_rfl,by linarith [show 0 < s from hs]⟩
  have hle := hB (mem_image_of_mem (jointLikelihood x) hp)
  change likelihood m (m+s) x m ≤ B at hle
  apply hgt.not_ge
  simpa only [widthLikelihood,Prod.fst,Prod.snd,sub_zero] using hle

private theorem reduce_to_knot (hn : 0 < n) {x : Fin n → ℝ} {a b c : ℝ}
    (hp : (a,b,c) ∈ jointDomain x) (hpos : 0 < likelihood a b x c) :
    ∃ k, likelihood a b x c ≤ likelihood a b x (x k) := by
  have hcompat : ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b)) := by
    rintro ⟨ha,hb⟩
    rw [likelihood_zero_of_both_endpoints hp.1 ha hb] at hpos
    exact (lt_irrefl 0) hpos
  obtain ⟨m,hm,hmax⟩ := Triangular.exists_isMaxOn hp.1 hn hp.2.2
  obtain ⟨⟨k,hk⟩,_⟩ :=
    (isMaxOn_iff_observation hp.1 hn hp.2.2 hcompat hm).mp hmax
  exact ⟨k, hk.symm ▸ hmax hp.2.1⟩

/-- The all-free triangular maximum is attained exactly for nonconstant
samples. Every positive comparison reduces to an observed mode knot. -/
theorem all_free_criterion (hn : 0 < n) (x : Fin n → ℝ) :
    (∃ p ∈ jointDomain x, 0 < jointLikelihood x p ∧ IsMaxOn (jointLikelihood x) (jointDomain x) p)
      ↔ ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨p,hp,_,hm⟩
    by_contra h
    let i : Fin n := ⟨0,hn⟩
    have he : x = (fun _ ↦ x i) := funext fun j ↦ not_not.mp (fun hj ↦ h ⟨j,i,hj⟩)
    rw [he] at hp hm
    exact all_free_collapse hn (x i) ⟨_,(hm.isLUB hp).1⟩
  · intro hx
    classical
    have hres (k : Fin n) : ∃ j, x j ≠ x k := by
      by_contra! h
      obtain ⟨i,j,hij⟩ := hx
      exact hij ((h i).trans (h j).symm)
    choose w hw hwpos hwmax using fun k ↦ held_mode_attains hn x (x k) (hres k)
    let F (k : Fin n) := widthLikelihood x (x k) (w k)
    obtain ⟨k,_,hk⟩ := Finset.exists_max_image Finset.univ F ⟨⟨0,hn⟩,Finset.mem_univ _⟩
    refine ⟨(x k-(w k).1,x k+(w k).2,x k),from_widths (hw k),hwpos k,?_⟩
    rintro ⟨a,b,c⟩ hp
    change likelihood a b x c ≤ F k
    by_cases hpL : 0 < likelihood a b x c
    · obtain ⟨j,hj⟩ := reduce_to_knot hn hp hpL
      have hp' : (a,b,x j) ∈ jointDomain x := ⟨hp.1,hp.2.2 j,hp.2.2⟩
      have hmax := hwmax j (to_widths hp')
      change widthLikelihood x (x j) (x j-a,b-x j) ≤ F j at hmax
      rw [widths_joint_value x a b] at hmax
      exact hj.trans (hmax.trans (hk j (Finset.mem_univ j)))
    · exact (le_of_not_gt hpL).trans (hwpos k).le

/-- With both endpoints held the closed mode interval always contains a
maximizer. Occupying both endpoints makes every mode likelihood zero. -/
theorem held_endpoints_failure (hn : 0 < n) (x : Fin n → ℝ) {a b : ℝ} (hab : a < b)
    (hx : ∀ j, x j ∈ Icc a b) :
    (∃ c ∈ Icc a b, IsMaxOn (likelihood a b x) (Icc a b) c) ∧
      ((∃ j, x j = a) → (∃ j, x j = b) → ∀ c ∈ Icc a b, likelihood a b x c = 0) :=
  ⟨Triangular.exists_isMaxOn hab hn hx,
    fun ha hb c _ ↦ likelihood_zero_of_both_endpoints hab ha hb c⟩

/-- The two-observation triangular counterexample. -/
def twoObservations : Fin 2 → ℝ := ![0,1]

private theorem one_side_pair_bound {u v : ℝ} (hu : 0 ≤ u) (hv : 1 ≤ v) :
    (2/(u+v))^2 * ((v-1)/v) ≤ 16/27 := by
  have hv' : 0 < v := zero_lt_one.trans_le hv
  have hw : 0 < u+v := add_pos_of_nonneg_of_pos hu hv'
  have hr : 2/(u+v) ≤ 2/v :=
    div_le_div_of_nonneg_left (by norm_num) hv' (le_add_of_nonneg_left hu)
  have hq := mul_le_mul_of_nonneg_right
    (pow_le_pow_left₀ (div_nonneg (by norm_num) hw.le) hr 2) (div_nonneg (sub_nonneg.mpr hv) hv'.le)
  refine hq.trans ?_
  have he : (2/v)^2 * ((v-1)/v) = 4*(v-1)/v^3 := by field_simp [hv'.ne']; ring
  rw [he]
  apply (div_le_iff₀ (pow_pos hv' 3)).mpr
  have hn := mul_nonneg (sq_nonneg (2*v-3)) (by linarith : 0 ≤ v+3)
  nlinarith only [hn]

private theorem pair_mode_bound {c : ℝ} (hc : c = 0 ∨ c = 1)
    {p : ℝ × ℝ} (hp : p ∈ widthDomain twoObservations c) :
    widthLikelihood twoObservations c p ≤ 16/27 := by
  rcases hc with rfl | rfl
  · have hv : 1 ≤ p.2 := by simpa [twoObservations] using (hp.2.2.2 (1 : Fin 2)).2
    rw [widthFormula]
    simp only [twoObservations, Fin.prod_univ_two, Matrix.cons_val_zero, Matrix.cons_val_one]
    norm_num
    exact one_side_pair_bound hp.1 hv
  · have hu : 1 ≤ p.1 := by
      have h := (hp.2.2.2 (0 : Fin 2)).1
      simp [twoObservations] at h
      linarith
    rw [widthFormula]
    simp only [twoObservations, Fin.prod_univ_two, Matrix.cons_val_zero, Matrix.cons_val_one]
    norm_num
    simpa only [add_comm p.2 p.1, sub_eq_add_neg, add_comm (-1 : ℝ) p.1] using
      one_side_pair_bound hp.2.1 hu

/-- On `(0,1)`, the two reflected endpoint-mode triples both maximize
the all-free likelihood and have product likelihood `16/27`. -/
theorem two_observation_reflected_tie :
    let p : ℝ × (ℝ × ℝ) := (0,3/2,0)
    let q : ℝ × (ℝ × ℝ) := (-1/2,1,1)
    p ≠ q ∧ p ∈ jointDomain twoObservations ∧ q ∈ jointDomain twoObservations ∧
      jointLikelihood twoObservations p = 16/27 ∧ jointLikelihood twoObservations q = 16/27 ∧
      IsMaxOn (jointLikelihood twoObservations) (jointDomain twoObservations) p ∧
      IsMaxOn (jointLikelihood twoObservations) (jointDomain twoObservations) q := by
  let p : ℝ × (ℝ × ℝ) := (0,3/2,0)
  let q : ℝ × (ℝ × ℝ) := (-1/2,1,1)
  have hp : p ∈ jointDomain twoObservations := by
    refine ⟨by norm_num [p],by norm_num [p],?_⟩
    intro i
    fin_cases i <;> norm_num [twoObservations,p]
  have hq : q ∈ jointDomain twoObservations := by
    refine ⟨by norm_num [q],by norm_num [q],?_⟩
    intro i
    fin_cases i <;> norm_num [twoObservations,q]
  have hLp : jointLikelihood twoObservations p = 16/27 := by
    norm_num [p,jointLikelihood,likelihood,factor,twoObservations,Fin.prod_univ_two]
  have hLq : jointLikelihood twoObservations q = 16/27 := by
    norm_num [q,jointLikelihood,likelihood,factor,twoObservations,Fin.prod_univ_two]
  have hb (r : ℝ × (ℝ × ℝ)) (hr : r ∈ jointDomain twoObservations) :
      jointLikelihood twoObservations r ≤ 16/27 := by
    rcases r with ⟨a,b,c⟩
    change likelihood a b twoObservations c ≤ _
    by_cases hpos : 0 < likelihood a b twoObservations c
    · obtain ⟨i,hi⟩ := reduce_to_knot (by decide : 0 < 2) hr hpos
      have hri : (a,b,twoObservations i) ∈ jointDomain twoObservations :=
        ⟨hr.1,hr.2.2 i,hr.2.2⟩
      have hc : twoObservations i = 0 ∨ twoObservations i = 1 := by
        fin_cases i <;> simp [twoObservations]
      have hm := pair_mode_bound hc (to_widths hri)
      rw [widths_joint_value twoObservations a b] at hm
      exact hi.trans hm
    · exact (le_of_not_gt hpos).trans (by norm_num)
  exact ⟨fun h ↦ (by norm_num [p,q] : p.1 ≠ q.1) (congrArg Prod.fst h),hp,hq,hLp,hLq,
    fun r hr ↦ (hb r hr).trans_eq hLp.symm,fun r hr ↦ (hb r hr).trans_eq hLq.symm⟩

/-- The compatible width domain with the lower endpoint and mode held. -/
def heldLowerModeDomain (x : Fin n → ℝ) (a c : ℝ) : Set (ℝ × ℝ) :=
  widthDomain x c ∩ {p | p.1 = c-a}

/-- The compatible width domain with the upper endpoint and mode held. -/
def heldUpperModeDomain (x : Fin n → ℝ) (b c : ℝ) : Set (ℝ × ℝ) :=
  widthDomain x c ∩ {p | p.2 = b-c}

/-- Fixing the lower endpoint at the common observation and mode still
allows unbounded collapse. -/
theorem held_lower_mode_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (widthLikelihood x m '' heldLowerModeDomain x m m) := by
  let x : Fin n → ℝ := fun _ ↦ m
  change ¬ BddAbove (widthLikelihood x m '' heldLowerModeDomain x m m)
  rintro ⟨B,hB⟩
  have h := held_mode_collapse hn m
  have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨s,hs,hgt⟩ := (hs.and (h.2.1.eventually_gt_atTop B)).exists
  have hp : (0,s) ∈ heldLowerModeDomain x m m := ⟨(h.1 s hs).1, by simp⟩
  exact hgt.not_ge (hB (mem_image_of_mem (widthLikelihood x m) hp))

/-- Holding the lower endpoint and mode gives finite attainment on
compatible data except a constant sample at their common value. -/
theorem held_lower_mode_criterion (hn : 0 < n) (x : Fin n → ℝ) (a c : ℝ) :
    (∃ p ∈ heldLowerModeDomain x a c, 0 < widthLikelihood x c p ∧
      IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x a c) p) ↔
    a ≤ c ∧ (∀ j, a ≤ x j) ∧ (∀ j, x j = a → c = a) ∧ (c ≠ a ∨ ∃ j, x j ≠ a) := by
  constructor
  · rintro ⟨p,hp,hL,hm⟩
    have hu : p.1 = c-a := hp.2
    have ha : a ≤ c := by linarith [hp.1.1]
    have e : c-p.1 = a := by linarith
    have hq := from_widths hp.1
    have hleft (j : Fin n) : a ≤ x j := e ▸ (hp.1.2.2.2 j).1
    have hcompat := (likelihood_pos_iff hq.1 hq.2.2 hq.2.1).mp hL
    refine ⟨ha,hleft,fun j hj ↦ e ▸ hcompat.1 j (hj.trans e.symm),?_⟩
    by_contra! h
    obtain ⟨hca,h⟩ := h
    subst c
    have hex : x = (fun _ ↦ a) := funext h
    rw [hex] at hp hm
    exact held_lower_mode_collapse hn a ⟨_,(hm.isLUB hp).1⟩
  · rintro ⟨ha,hx,hc,hne⟩
    let B := 1+∑ j, |x j-c|
    have hB (j : Fin n) : |x j-c| < B := by
      have h := Finset.single_le_sum (f := fun j ↦ |x j-c|)
        (fun j _ ↦ abs_nonneg (x j-c)) (Finset.mem_univ j)
      dsimp [B]
      linarith
    let i : Fin n := ⟨0,hn⟩
    have hBp : 0 < B := (abs_nonneg (x i-c)).trans_lt (hB i)
    have hp : (c-a,B) ∈ heldLowerModeDomain x a c := by
      refine ⟨⟨sub_nonneg.mpr ha,hBp.le,by linarith,fun j ↦ ?_⟩,rfl⟩
      obtain ⟨h₁,h₂⟩ := abs_lt.mp (hB j)
      exact ⟨by linarith [hx j],by linarith⟩
    have hpos : 0 < widthLikelihood x c (c-a,B) := by
      unfold widthLikelihood
      rw [show c-(c-a) = a by ring]
      apply (likelihood_pos_iff (by linarith : a < c+B)
        (fun j ↦ ⟨hx j,(sub_lt_iff_lt_add.mp (abs_lt.mp (hB j)).2).le.trans_eq (add_comm _ _)⟩)
        ⟨ha,by linarith⟩).mpr
      refine ⟨hc,fun j h ↦ ?_⟩
      have he := (abs_lt.mp (hB j)).2
      linarith
    have hδ : ∃ δ > 0, ∀ q ∈ heldLowerModeDomain x a c, δ ≤ q.1+q.2 := by
      by_cases h : c = a
      · obtain ⟨j,hj⟩ := hne.resolve_left (not_not_intro h)
        have hn : a < x j := (hx j).lt_of_ne hj.symm
        refine ⟨x j-a,sub_pos.mpr hn,fun q hq ↦ ?_⟩
        have he := (hq.1.2.2.2 j).2
        linarith [hq.1.1]
      · refine ⟨c-a,sub_pos.mpr (ha.lt_of_ne (Ne.symm h)),fun q hq ↦ ?_⟩
        have hu : q.1 = c-a := hq.2
        linarith [hq.1.2.1]
    obtain ⟨δ,hδ,hbound⟩ := hδ
    exact width_attainment hn (isClosed_eq continuous_fst continuous_const)
      hδ hbound hp hpos

private theorem reflect_width_domain (x : Fin n → ℝ) (c : ℝ) (p : ℝ × ℝ) :
    p.swap ∈ widthDomain (fun j ↦ -x j) (-c) ↔ p ∈ widthDomain x c := by
  simp only [widthDomain, mem_ofPred_eq, Prod.fst_swap, Prod.snd_swap]
  constructor <;> rintro ⟨h₁,h₂,h₃,h⟩
  · exact ⟨h₂,h₁,by linarith,fun j ↦ by obtain ⟨hl,hu⟩ := h j; exact ⟨by linarith,by linarith⟩⟩
  · exact ⟨h₂,h₁,by linarith,fun j ↦ by obtain ⟨hl,hu⟩ := h j; exact ⟨by linarith,by linarith⟩⟩

private theorem reflect_width_value (x : Fin n → ℝ) (c : ℝ) (p : ℝ × ℝ) :
    widthLikelihood (fun j ↦ -x j) (-c) p.swap = widthLikelihood x c p := by
  rw [widthFormula,widthFormula]
  simp only [Prod.fst_swap,Prod.snd_swap,add_comm p.2 p.1]
  congr 1
  apply Finset.prod_congr rfl
  intro j _
  rcases lt_trichotomy (x j) c with h | h | h
  · rw [ite_eq_left h,ite_eq_right (not_lt_of_ge (neg_le_neg h.le)),
      ite_eq_left (neg_lt_neg h)]
    congr 1
    ring
  · rw [h]
    simp
  · rw [ite_eq_right (not_lt_of_ge h.le),ite_eq_left h,ite_eq_left (neg_lt_neg h)]
    congr 1
    ring

private theorem reflect_upper_lower (x : Fin n → ℝ) (b c : ℝ) :
    BijOn Prod.swap (heldUpperModeDomain x b c)
      (heldLowerModeDomain (fun j ↦ -x j) (-b) (-c)) := by
  have he (p : ℝ × ℝ) :
      p.swap ∈ heldLowerModeDomain (fun j ↦ -x j) (-b) (-c) ↔
        p ∈ heldUpperModeDomain x b c := by
    change (p.swap ∈ widthDomain (fun j ↦ -x j) (-c) ∧ p.2 = -c - -b) ↔
      (p ∈ widthDomain x c ∧ p.2 = b-c)
    rw [reflect_width_domain, show -c - -b = b-c by ring]
  exact ⟨fun p hp ↦ (he p).mpr hp,
    fun p _ q _ h ↦ Prod.ext (congrArg Prod.snd h) (congrArg Prod.fst h),
    fun q hq ↦ ⟨q.swap,(he q.swap).mp (by simpa using hq),Prod.swap_swap q⟩⟩

/-- The upper-endpoint and mode criterion follows by reflecting the
lower-endpoint and mode fit, preserving its height and support contacts. -/
theorem held_upper_mode_criterion (hn : 0 < n) (x : Fin n → ℝ) (b c : ℝ) :
    (∃ p ∈ heldUpperModeDomain x b c, 0 < widthLikelihood x c p ∧
      IsMaxOn (widthLikelihood x c) (heldUpperModeDomain x b c) p) ↔
    c ≤ b ∧ (∀ j, x j ≤ b) ∧ (∀ j, x j = b → c = b) ∧ (c ≠ b ∨ ∃ j, x j ≠ b) := by
  let y (j : Fin n) := -x j
  let S := heldUpperModeDomain x b c
  let T := heldLowerModeDomain y (-b) (-c)
  have he : BijOn Prod.swap S T := reflect_upper_lower x b c
  have h (p : ℝ × ℝ) (hp : p ∈ S) :
      IsMaxOn (widthLikelihood x c) S p ↔ IsMaxOn (widthLikelihood y (-c)) T p.swap :=
    LocScaleCommon.isMaxOn_iff_of_bijOn he (fun p _ ↦ (reflect_width_value x c p).symm) hp
  have ht : (∃ p ∈ S, 0 < widthLikelihood x c p ∧ IsMaxOn (widthLikelihood x c) S p) ↔
      ∃ q ∈ T, 0 < widthLikelihood y (-c) q ∧ IsMaxOn (widthLikelihood y (-c)) T q := by
    constructor
    · rintro ⟨p,hp,hL,hm⟩
      exact ⟨p.swap,he.mapsTo hp,(reflect_width_value x c p).symm ▸ hL,(h p hp).mp hm⟩
    · rintro ⟨q,hq,hL,hm⟩
      obtain ⟨p,hp,rfl⟩ := he.surjOn hq
      exact ⟨p,hp,(reflect_width_value x c p) ▸ hL,(h p hp).mpr hm⟩
  change (∃ p ∈ S, 0 < widthLikelihood x c p ∧ IsMaxOn (widthLikelihood x c) S p) ↔ _
  rw [ht]
  exact (held_lower_mode_criterion hn y (-b) (-c)).trans (by simp [y,neg_le_neg_iff])

/-- Holding only the mode permits a positive maximum exactly when some
observation differs from that mode. -/
theorem held_mode_criterion (hn : 0 < n) (x : Fin n → ℝ) (c : ℝ) :
    (∃ p ∈ widthDomain x c, 0 < widthLikelihood x c p ∧
      IsMaxOn (widthLikelihood x c) (widthDomain x c) p) ↔ ∃ i, x i ≠ c := by
  refine ⟨?_, held_mode_attains hn x c⟩
  rintro ⟨p,hp,_,hm⟩
  by_contra! h
  have hx : x = (fun _ ↦ c) := funext h
  rw [hx] at hp hm
  have hpath := held_mode_collapse hn c
  have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨s,hs,hgt⟩ := (hs.and (hpath.2.1.eventually_gt_atTop
    (widthLikelihood (fun _ : Fin n ↦ c) c p))).exists
  exact hgt.not_ge (hm (hpath.1 s hs).1)

/-- Fixing the upper endpoint at the common observation and mode still
allows unbounded collapse. -/
theorem held_upper_mode_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (widthLikelihood x m '' heldUpperModeDomain x m m) := by
  let x : Fin n → ℝ := fun _ ↦ m
  change ¬ BddAbove (widthLikelihood x m '' heldUpperModeDomain x m m)
  rintro ⟨B,hB⟩
  have h := held_mode_collapse hn m
  have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨s,hs,hgt⟩ := (hs.and (h.2.2.eventually_gt_atTop B)).exists
  have hp : (s,0) ∈ heldUpperModeDomain x m m := ⟨(h.1 s hs).2, by simp⟩
  exact hgt.not_ge (hB (mem_image_of_mem (widthLikelihood x m) hp))

/-- With only the lower endpoint held, the mode and upper endpoint vary. -/
def heldLowerDomain (x : Fin n → ℝ) (a : ℝ) : Set (ℝ × (ℝ × ℝ)) :=
  jointDomain x ∩ {p | p.1 = a}

/-- With only the upper endpoint held, the mode and lower endpoint vary. -/
def heldUpperDomain (x : Fin n → ℝ) (b : ℝ) : Set (ℝ × (ℝ × ℝ)) :=
  jointDomain x ∩ {p | p.2.1 = b}

/-- Constant observations at a held lower endpoint have an unbounded
likelihood as the upper endpoint approaches them along the mode. -/
theorem held_lower_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (jointLikelihood x '' heldLowerDomain x m) := by
  let x : Fin n → ℝ := fun _ ↦ m
  change ¬ BddAbove (jointLikelihood x '' heldLowerDomain x m)
  rintro ⟨B,hB⟩
  have h := held_mode_collapse hn m
  have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨s,hs,hgt⟩ := (hs.and (h.2.1.eventually_gt_atTop B)).exists
  have hp' := from_widths (h.1 s hs).1
  rw [sub_zero] at hp'
  have hp : (m,m+s,m) ∈ heldLowerDomain x m := ⟨hp',rfl⟩
  have hle := hB (mem_image_of_mem (jointLikelihood x) hp)
  change likelihood m (m+s) x m ≤ B at hle
  exact hgt.not_ge (by simpa only [widthLikelihood,Prod.fst,Prod.snd,sub_zero] using hle)

/-- Holding the lower endpoint gives a positive maximum exactly when all
observations are compatible and at least one is strictly above it. -/
theorem held_lower_criterion (hn : 0 < n) (x : Fin n → ℝ) (a : ℝ) :
    (∃ p ∈ heldLowerDomain x a, 0 < jointLikelihood x p ∧
      IsMaxOn (jointLikelihood x) (heldLowerDomain x a) p) ↔
    (∀ j, a ≤ x j) ∧ ∃ j, x j ≠ a := by
  classical
  constructor
  · rintro ⟨p,hp,_,hm⟩
    have he : p.1 = a := hp.2
    refine ⟨fun j ↦ he ▸ (hp.1.2.2 j).1,?_⟩
    by_contra! h
    have hx : x = (fun _ ↦ a) := funext h
    rw [hx] at hp hm
    exact held_lower_collapse hn a ⟨_,(hm.isLUB hp).1⟩
  · rintro ⟨hx,hdiff⟩
    let K := {j : Fin n | ∀ k, x k = a → x j = a}
    have hkne : K.Nonempty := by
      by_cases h : ∃ j, x j = a
      · obtain ⟨j,hj⟩ := h
        exact ⟨j,fun _ _ ↦ hj⟩
      · exact ⟨⟨0,hn⟩,fun j hj ↦ (h ⟨j,hj⟩).elim⟩
    let : Nonempty K := hkne.to_subtype
    have hf (j : K) := (held_lower_mode_criterion hn x a (x j)).mpr
      ⟨hx j,hx,j.property,Or.inr hdiff⟩
    choose w hw hwpos hwmax using hf
    let F (j : K) := widthLikelihood x (x j) (w j)
    obtain ⟨k,_,hk⟩ := Finset.exists_max_image Finset.univ F Finset.univ_nonempty
    let p := (a,x k+(w k).2,x k)
    have huw : (w k).1 = x k-a := (hw k).2
    have hw' : x k-(w k).1 = a := by linarith
    have hp : p ∈ heldLowerDomain x a := by
      have h := from_widths (hw k).1
      rw [hw'] at h
      exact ⟨h,rfl⟩
    have hep : jointLikelihood x p = F k := by
      unfold p F jointLikelihood widthLikelihood
      rw [hw']
    refine ⟨p,hp,by rw [hep]; exact hwpos k,?_⟩
    rintro ⟨l,b,c⟩ hr
    have hl : l = a := hr.2
    subst l
    change likelihood a b x c ≤ jointLikelihood x p
    rw [hep]
    by_cases hpos : 0 < likelihood a b x c
    · obtain ⟨j,hj⟩ := reduce_to_knot hn hr.1 hpos
      have hpos' := hpos.trans_le hj
      have hcomp := (likelihood_pos_iff hr.1.1 hr.1.2.2 (hr.1.2.2 j)).mp hpos'
      let j' : K := ⟨j,hcomp.1⟩
      have hdom : (x j-a,b-x j) ∈ heldLowerModeDomain x a (x j) :=
        ⟨to_widths (show (a,b,x j) ∈ jointDomain x from ⟨hr.1.1,hr.1.2.2 j,hr.1.2.2⟩),rfl⟩
      have hm := hwmax j' hdom
      change widthLikelihood x (x j) (x j-a,b-x j) ≤ F j' at hm
      rw [widths_joint_value x a b] at hm
      exact hj.trans (hm.trans (hk j' (Finset.mem_univ j')))
    · exact (le_of_not_gt hpos).trans (hwpos k).le

/-- Any positive global winner at a held lower endpoint has observation
mode. The fixed-endpoint comparison applies to that winner's width. -/
theorem held_lower_winner_is_knot (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ}
    {p : ℝ × (ℝ × ℝ)} (hp : p ∈ heldLowerDomain x a) (hpos : 0 < jointLikelihood x p)
    (hm : IsMaxOn (jointLikelihood x) (heldLowerDomain x a) p) : ∃ j, x j = p.2.2 := by
  have hcomp : ¬ ((∃ j, x j = p.1) ∧ (∃ j, x j = p.2.1)) := by
    rintro ⟨h₁,h₂⟩
    have hz := likelihood_zero_of_both_endpoints hp.1.1 h₁ h₂ p.2.2
    exact hpos.ne' hz
  have hslice : IsMaxOn (likelihood p.1 p.2.1 x) (Icc p.1 p.2.1) p.2.2 :=
    fun c hc ↦ isMaxOn_iff.mp hm (p.1,p.2.1,c) ⟨⟨hp.1.1,hc,hp.1.2.2⟩,hp.2⟩
  exact ((isMaxOn_iff_observation hp.1.1 hn hp.1.2.2 hcomp hp.1.2.1).mp hslice).1

private def reflectJoint (p : ℝ × (ℝ × ℝ)) : ℝ × (ℝ × ℝ) :=
  (-p.2.1,-p.1,-p.2.2)

private theorem reflectJoint_twice (p : ℝ × (ℝ × ℝ)) :
    reflectJoint (reflectJoint p) = p := by
  rcases p with ⟨a,b,c⟩
  simp [reflectJoint]

private theorem reflectJoint_domain (x : Fin n → ℝ) (b : ℝ) (p : ℝ × (ℝ × ℝ)) :
    reflectJoint p ∈ heldLowerDomain (fun j ↦ -x j) (-b) ↔ p ∈ heldUpperDomain x b := by
  rcases p with ⟨a,d,c⟩
  change ((-d < -a ∧ (-c ∈ Icc (-d) (-a)) ∧ ∀ j, -x j ∈ Icc (-d) (-a)) ∧ -d = -b) ↔
    ((a < d ∧ c ∈ Icc a d ∧ ∀ j, x j ∈ Icc a d) ∧ d = b)
  simp only [neg_lt_neg_iff, mem_Icc, neg_le_neg_iff, neg_inj]
  constructor <;> rintro ⟨⟨h₁,h₂,h₃⟩,h₄⟩ <;>
    exact ⟨⟨h₁,h₂.symm,fun j ↦ (h₃ j).symm⟩,h₄⟩

private theorem reflectJoint_value (x : Fin n → ℝ) (p : ℝ × (ℝ × ℝ)) :
    jointLikelihood (fun j ↦ -x j) (reflectJoint p) = jointLikelihood x p := by
  rcases p with ⟨a,b,c⟩
  have h := reflect_width_value x c (c-a,b-c)
  simp only [Prod.swap] at h
  change jointLikelihood (fun j ↦ -x j) (-b,-a,-c) = jointLikelihood x (a,b,c)
  rw [← widths_joint_value, ← widths_joint_value, ← h]
  rw [show -c - -b = b-c by ring, show -a - -c = c-a by ring]

private theorem reflectJoint_bij (x : Fin n → ℝ) (b : ℝ) :
    BijOn reflectJoint (heldUpperDomain x b) (heldLowerDomain (fun j ↦ -x j) (-b)) := by
  refine ⟨fun p hp ↦ (reflectJoint_domain x b p).mpr hp, ?_, ?_⟩
  · intro p _ q _ he
    simpa only [reflectJoint_twice] using congrArg reflectJoint he
  · intro q hq
    refine ⟨reflectJoint q,(reflectJoint_domain x b (reflectJoint q)).mp ?_,reflectJoint_twice q⟩
    rwa [reflectJoint_twice]

/-- Holding the upper endpoint gives a positive maximum exactly when all
observations are compatible and at least one lies strictly below it. -/
theorem held_upper_criterion (hn : 0 < n) (x : Fin n → ℝ) (b : ℝ) :
    (∃ p ∈ heldUpperDomain x b, 0 < jointLikelihood x p ∧
      IsMaxOn (jointLikelihood x) (heldUpperDomain x b) p) ↔
    (∀ j, x j ≤ b) ∧ ∃ j, x j ≠ b := by
  let y (j : Fin n) := -x j
  let S := heldUpperDomain x b
  let T := heldLowerDomain y (-b)
  have he : BijOn reflectJoint S T := reflectJoint_bij x b
  have h (p : ℝ × (ℝ × ℝ)) (hp : p ∈ S) :
      IsMaxOn (jointLikelihood x) S p ↔ IsMaxOn (jointLikelihood y) T (reflectJoint p) :=
    LocScaleCommon.isMaxOn_iff_of_bijOn he (fun p _ ↦ (reflectJoint_value x p).symm) hp
  have ht : (∃ p ∈ S, 0 < jointLikelihood x p ∧ IsMaxOn (jointLikelihood x) S p) ↔
      ∃ q ∈ T, 0 < jointLikelihood y q ∧ IsMaxOn (jointLikelihood y) T q := by
    constructor
    · rintro ⟨p,hp,hL,hm⟩
      exact ⟨reflectJoint p,he.mapsTo hp,(reflectJoint_value x p).symm ▸ hL,(h p hp).mp hm⟩
    · rintro ⟨q,hq,hL,hm⟩
      obtain ⟨p,hp,rfl⟩ := he.surjOn hq
      exact ⟨p,hp,(reflectJoint_value x p) ▸ hL,(h p hp).mpr hm⟩
  change (∃ p ∈ S, 0 < jointLikelihood x p ∧ IsMaxOn (jointLikelihood x) S p) ↔ _
  rw [ht]
  exact (held_lower_criterion hn y (-b)).trans (by simp [y,neg_le_neg_iff])

/-- The joint constant-sample collapse is also feasible with its upper
endpoint held at the common observation. -/
theorem held_upper_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (jointLikelihood x '' heldUpperDomain x m) := by
  let x : Fin n → ℝ := fun _ ↦ m
  let y : Fin n → ℝ := fun _ ↦ -m
  change ¬ BddAbove (jointLikelihood x '' heldUpperDomain x m)
  have he : BijOn reflectJoint (heldUpperDomain x m) (heldLowerDomain y (-m)) :=
    reflectJoint_bij x m
  have hsub : jointLikelihood y '' heldLowerDomain y (-m) ⊆
      jointLikelihood x '' heldUpperDomain x m := by
    rintro _ ⟨q,hq,rfl⟩
    obtain ⟨p,hp,rfl⟩ := he.surjOn hq
    exact ⟨p,hp,(reflectJoint_value x p).symm⟩
  exact fun h ↦ held_lower_collapse hn (-m) (h.mono hsub)

/-- Any positive global winner at a held upper endpoint has observation mode. -/
theorem held_upper_winner_is_knot (hn : 0 < n) (x : Fin n → ℝ) {b : ℝ}
    {p : ℝ × (ℝ × ℝ)} (hp : p ∈ heldUpperDomain x b) (hpos : 0 < jointLikelihood x p)
    (hm : IsMaxOn (jointLikelihood x) (heldUpperDomain x b) p) : ∃ j, x j = p.2.2 := by
  let y (j : Fin n) := -x j
  have he : BijOn reflectJoint (heldUpperDomain x b) (heldLowerDomain y (-b)) := reflectJoint_bij x b
  have hval := reflectJoint_value x p
  have hmax := (LocScaleCommon.isMaxOn_iff_of_bijOn he
    (fun q _ ↦ (reflectJoint_value x q).symm) hp).mp hm
  obtain ⟨j,hj⟩ := held_lower_winner_is_knot hn y (he.mapsTo hp) (hval.symm ▸ hpos) hmax
  exact ⟨j,neg_injective hj⟩

private theorem positive_right_slack {x : Fin n → ℝ} {c : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ widthDomain x c) (hL : 0 < widthLikelihood x c p) {j : Fin n} (hj : c < x j) :
    x j-c < p.2 := by
  have h := (hp.2.2.2 j).2
  by_contra! hn
  have he : x j = c+p.2 := by linarith
  have hb := from_widths hp
  have hh := (likelihood_pos_iff hb.1 hb.2.2 hb.2.1).mp hL
  exact hj.ne' (he.trans (hh.2 j he).symm)

private noncomputable def rightTerms (x : Fin n → ℝ) (c v : ℝ) : ℝ :=
  ∑ j, if c < x j then (x j-c)/(v-(x j-c)) else 0

private theorem right_log_derivative (x : Fin n → ℝ) (c u : ℝ) {v : ℝ}
    (hu : 0 ≤ u) (hv : 0 < v) (hslack : ∀ j, c < x j → x j-c < v) :
    HasDerivAt (fun t ↦ Triangular.logLik (c-u) (c+t) x c)
      (-(n : ℝ)/(u+v) + rightTerms x c v / v) v := by
  let U (t : ℝ) := (n : ℝ)*(Real.log 2 - Real.log (u+t))
  let F (j : Fin n) (t : ℝ) :=
    if x j < c then Real.log ((x j-c+u)/u) else if c < x j then
      Real.log (t-(x j-c)) - Real.log t else 0
  have hU : HasDerivAt U (-(n : ℝ)/(u+v)) v := by
    have h := ((((hasDerivAt_id v).const_add u).log
      (add_pos_of_nonneg_of_pos hu hv).ne').const_sub (Real.log 2)).const_mul (n : ℝ)
    convert h using 1
    · rfl
    · simp only [id_eq]
      ring
  have hF (j : Fin n) :
      HasDerivAt (F j) ((if c < x j then (x j-c)/(v-(x j-c)) else 0)/v) v := by
    unfold F
    by_cases h : c < x j
    · simp only [h,ite_true,not_lt_of_ge h.le,ite_false]
      have h' := (((hasDerivAt_id v).sub_const (x j-c)).log
        (sub_pos.mpr (hslack j h)).ne').sub ((hasDerivAt_id v).log hv.ne')
      convert h' using 1
      · rfl
      · simp only [id_eq]
        field_simp [hv.ne',(sub_pos.mpr (hslack j h)).ne']
        ring
    · by_cases h' : x j < c
      · simpa only [h,h',ite_true,ite_false,zero_div] using
          (hasDerivAt_const v (Real.log ((x j-c+u)/u)))
      · simpa only [h,h',ite_false,zero_div] using (hasDerivAt_const v (0 : ℝ))
  have hsum := HasDerivAt.fun_sum (u := Finset.univ) (fun j _ ↦ hF j)
  rw [← Finset.sum_div] at hsum
  have h := hU.add hsum
  apply HasDerivAt.congr_of_eventuallyEq h
  have hvl : ∀ᶠ t in 𝓝 v, 0 < t := eventually_gt_nhds hv
  have hright : ∀ᶠ t in 𝓝 v, ∀ j, c < x j → x j-c < t := by
    rw [eventually_all]
    intro j
    by_cases hj : c < x j
    · exact (eventually_gt_nhds (hslack j hj)).mono (fun _ ht _ ↦ ht)
    · exact Eventually.of_forall (fun _ h ↦ (hj h).elim)
  filter_upwards [hvl,hright] with t ht hright
  unfold U Triangular.logLik
  rw [show c+t-(c-u) = u+t by ring,
    Real.log_div (by norm_num) (add_pos_of_nonneg_of_pos hu ht).ne']
  congr 1
  apply Finset.sum_congr rfl
  intro j _
  unfold F factor
  by_cases hl : x j < c
  · simp only [hl,ite_true]
    rw [show x j-(c-u) = x j-c+u by ring,show c-(c-u) = u by ring]
  · by_cases hr : c < x j
    · simp only [hl,hr,ite_true,ite_false]
      rw [show c+t-x j = t-(x j-c) by ring,show c+t-c = t by ring,
        Real.log_div (sub_pos.mpr (hright j hr)).ne' ht.ne']
    · simp [hl,hr]

private theorem right_score_at_winner {x : Fin n → ℝ} {a c : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ heldLowerModeDomain x a c) (hL : 0 < widthLikelihood x c p)
    (hm : IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x a c) p)
    (hv : 0 < p.2) : rightTerms x c p.2 = (n : ℝ)*p.2/(p.1+p.2) := by
  have hslack : ∀ j, c < x j → x j-c < p.2 := fun _ hj ↦ positive_right_slack hp.1 hL hj
  have hmain : IsLocalMax (fun t ↦ Triangular.logLik (c-p.1) (c+t) x c) p.2 := by
    have hright : ∀ᶠ t in 𝓝 p.2, ∀ j, c < x j → x j-c < t := by
      rw [eventually_all]
      intro j
      by_cases hj : c < x j
      · exact (eventually_gt_nhds (hslack j hj)).mono (fun _ ht _ ↦ ht)
      · exact Eventually.of_forall (fun _ h ↦ (hj h).elim)
    filter_upwards [eventually_gt_nhds hv,hright] with t ht hts
    have hB : (p.1,t) ∈ heldLowerModeDomain x a c := by
      refine ⟨⟨hp.1.1,ht.le,add_pos_of_nonneg_of_pos hp.1.1 ht,fun j ↦ ?_⟩,hp.2⟩
      refine ⟨(hp.1.2.2.2 j).1,?_⟩
      by_cases hj : c < x j
      · linarith [hts j hj]
      · linarith [le_of_not_gt hj]
    have hp' := from_widths hp.1
    have hB' := from_widths hB.1
    have hcontact := (likelihood_pos_iff hp'.1 hp'.2.2 hp'.2.1).mp hL
    have hL' : 0 < widthLikelihood x c (p.1,t) :=
      (likelihood_pos_iff hB'.1 hB'.2.2 hB'.2.1).mpr ⟨hcontact.1,fun j hj ↦ by
        have hj' : c < x j := by change x j = c+t at hj; linarith
        have h := hts j hj'
        change x j = c+t at hj
        linarith⟩
    have hlog (q : ℝ × ℝ) (hq : q ∈ widthDomain x c) (hval : 0 < widthLikelihood x c q) :
        Real.log (widthLikelihood x c q) = Triangular.logLik (c-q.1) (c+q.2) x c := by
      have hb : 0 < 2/(c+q.2-(c-q.1)) := div_pos (by norm_num) (by linarith [hq.2.2.1])
      have hne (j : Fin n) : factor (c-q.1) (c+q.2) c (x j) ≠ 0 := by
        intro he
        apply hval.ne'
        unfold widthLikelihood likelihood
        rw [Finset.prod_eq_zero (Finset.mem_univ j) he,mul_zero]
      unfold widthLikelihood likelihood Triangular.logLik
      rw [Real.log_mul (pow_ne_zero _ hb.ne') (Finset.prod_ne_zero_iff.mpr (fun j _ ↦ hne j)),
        Real.log_pow, Real.log_prod (fun j _ ↦ hne j)]
    have h := Real.log_le_log hL' (hm hB)
    change Real.log (widthLikelihood x c (p.1,t)) ≤ Real.log (widthLikelihood x c p) at h
    rwa [hlog _ hB.1 hL',hlog _ hp.1 hL] at h
  have hder := (right_log_derivative x c p.1 hp.1.1 hv hslack).deriv
  have hzero := hmain.deriv_eq_zero
  rw [hder] at hzero
  field_simp [hv.ne', (show 0 < p.1+p.2 from hp.1.2.2.1).ne'] at hzero ⊢
  linarith

private theorem rightTerms_decrease {x : Fin n → ℝ} {c v w : ℝ}
    (hw : v < w) (hslack : ∀ j, c < x j → x j-c < v) (hside : ∃ j, c < x j) :
    rightTerms x c w < rightTerms x c v := by
  obtain ⟨i,hi⟩ := hside
  unfold rightTerms
  have h (j : Fin n) (hj : c < x j) :
      (x j-c)/(w-(x j-c)) < (x j-c)/(v-(x j-c)) :=
    div_lt_div_of_pos_left (sub_pos.mpr hj) (sub_pos.mpr (hslack j hj))
      (sub_lt_sub_right hw _)
  refine Finset.sum_lt_sum (fun j _ ↦ ?_) ⟨i,Finset.mem_univ i,?_⟩
  · by_cases hj : c < x j
    · simpa only [hj,ite_true] using (h j hj).le
    · simp [hj]
  · simpa only [hi,ite_true] using h i hi

private theorem unused_right_zero (hn : 0 < n) {x : Fin n → ℝ} {a c : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ heldLowerModeDomain x a c) (hL : 0 < widthLikelihood x c p)
    (hm : IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x a c) p)
    (hside : ∀ j, x j ≤ c) : p.2 = 0 := by
  by_contra hn'
  have hv : 0 < p.2 := hp.1.2.1.lt_of_ne (Ne.symm hn')
  have h' : (p.1,p.2/2) ∈ heldLowerModeDomain x a c := by
    refine ⟨⟨hp.1.1,(half_pos hv).le,add_pos_of_nonneg_of_pos hp.1.1 (half_pos hv),?_⟩,hp.2⟩
    exact fun j ↦ ⟨(hp.1.2.2.2 j).1,(hside j).trans (le_add_of_nonneg_right (half_pos hv).le)⟩
  have hphys := widthFormula x c p
  have hphys' := widthFormula x c (p.1,p.2/2)
  let C := ∏ j, (if x j < c then (x j-c+p.1)/p.1 else 1)
  have he : widthLikelihood x c p = (2/(p.1+p.2))^n * C := by
    simpa only [C,not_lt_of_ge (hside _),ite_false] using hphys
  have he' : widthLikelihood x c (p.1,p.2/2) = (2/(p.1+p.2/2))^n * C := by
    simpa only [C,Prod.fst,Prod.snd,not_lt_of_ge (hside _),ite_false] using hphys'
  have hpos (t : ℝ) (ht : 0 < t) : 0 < (2/(p.1+t))^n :=
    pow_pos (div_pos (by norm_num) (add_pos_of_nonneg_of_pos hp.1.1 ht)) _
  have hC : 0 < C := by rwa [he,mul_pos_iff_of_pos_left (hpos _ hv)] at hL
  have hlt : widthLikelihood x c p < widthLikelihood x c (p.1,p.2/2) := by
    rw [he,he']
    apply mul_lt_mul_of_pos_right _ hC
    apply pow_lt_pow_left₀ _ (div_nonneg (by norm_num) (add_nonneg hp.1.1 hv.le)) hn.ne'
    exact div_lt_div_of_pos_left (by norm_num) (add_pos_of_nonneg_of_pos hp.1.1 (half_pos hv))
      (by linarith [half_lt_self hv])
  exact hlt.not_ge (hm h')

/-- A positive maximizer with lower endpoint and mode held has a unique
upper width, including a zero right width if no residual is on the right. -/
theorem held_lower_mode_unique (hn : 0 < n) (x : Fin n → ℝ) {a c : ℝ}
    {p q : ℝ × ℝ} (hp : p ∈ heldLowerModeDomain x a c) (hq : q ∈ heldLowerModeDomain x a c)
    (hLp : 0 < widthLikelihood x c p) (hLq : 0 < widthLikelihood x c q)
    (hpm : IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x a c) p)
    (hqm : IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x a c) q) : p = q := by
  have hu : p.1 = q.1 := hp.2.trans hq.2.symm
  apply Prod.ext hu
  by_cases hside : ∃ i, c < x i
  · obtain ⟨i,hi⟩ := hside
    have hpv : 0 < p.2 := (sub_pos.mpr hi).trans (positive_right_slack hp.1 hLp hi)
    have hqv : 0 < q.2 := (sub_pos.mpr hi).trans (positive_right_slack hq.1 hLq hi)
    have he := right_score_at_winner hp hLp hpm hpv
    have he' := right_score_at_winner hq hLq hqm hqv
    rw [← hu] at he'
    have hmono : MonotoneOn (fun t : ℝ ↦ (n : ℝ)*t/(p.1+t)) (Ioi 0) := by
      intro b hb r hr hbr
      apply (div_le_div_iff₀ (add_pos_of_nonneg_of_pos hp.1.1 hb)
        (add_pos_of_nonneg_of_pos hp.1.1 hr)).mpr
      have hn' : 0 ≤ (n : ℝ) := Nat.cast_nonneg _
      nlinarith only [mul_nonneg (mul_nonneg hn' hp.1.1) (sub_nonneg.mpr hbr)]
    rcases lt_trichotomy p.2 q.2 with h | h | h
    · have hlt := rightTerms_decrease (x := x) h (fun _ hj ↦ positive_right_slack hp.1 hLp hj) ⟨i,hi⟩
      rw [he,he'] at hlt
      exact False.elim (hlt.not_ge (hmono hpv hqv h.le))
    · exact h
    · have hlt := rightTerms_decrease (x := x) h (fun _ hj ↦ positive_right_slack hq.1 hLq hj) ⟨i,hi⟩
      rw [he,he'] at hlt
      exact False.elim (hlt.not_ge (hmono hqv hpv h.le))
  · have hs (j : Fin n) : x j ≤ c := le_of_not_gt (fun h ↦ hside ⟨j,h⟩)
    exact (unused_right_zero hn hp hLp hpm hs).trans (unused_right_zero hn hq hLq hqm hs).symm

/-- A positive maximizer with upper endpoint and mode held has a unique
lower width. -/
theorem held_upper_mode_unique (hn : 0 < n) (x : Fin n → ℝ) {b c : ℝ}
    {p q : ℝ × ℝ} (hp : p ∈ heldUpperModeDomain x b c) (hq : q ∈ heldUpperModeDomain x b c)
    (hLp : 0 < widthLikelihood x c p) (hLq : 0 < widthLikelihood x c q)
    (hpm : IsMaxOn (widthLikelihood x c) (heldUpperModeDomain x b c) p)
    (hqm : IsMaxOn (widthLikelihood x c) (heldUpperModeDomain x b c) q) : p = q := by
  let y (i : Fin n) := -x i
  have hb := reflect_upper_lower x b c
  have hmax (r : ℝ × ℝ) (hr : r ∈ heldUpperModeDomain x b c) :=
    LocScaleCommon.isMaxOn_iff_of_bijOn hb (fun s _ ↦ (reflect_width_value x c s).symm) hr
  have he := held_lower_mode_unique hn y (hb.mapsTo hp) (hb.mapsTo hq)
    ((reflect_width_value x c p).symm ▸ hLp) ((reflect_width_value x c q).symm ▸ hLq)
    ((hmax p hp).mp hpm) ((hmax q hq).mp hqm)
  exact hb.injOn hp hq he

private theorem width_slice_right {x : Fin n → ℝ} {c : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ widthDomain x c) (hm : IsMaxOn (widthLikelihood x c) (widthDomain x c) p) :
    p ∈ heldLowerModeDomain x (c-p.1) c ∧
      IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x (c-p.1) c) p :=
  ⟨⟨hp,by change p.1 = c-(c-p.1); ring⟩,fun _ ht ↦ hm ht.1⟩

private theorem reflect_width_max {x : Fin n → ℝ} {c : ℝ} {p : ℝ × ℝ}
    (hm : IsMaxOn (widthLikelihood x c) (widthDomain x c) p) :
    IsMaxOn (widthLikelihood (fun j ↦ -x j) (-c))
      (widthDomain (fun j ↦ -x j) (-c)) p.swap := by
  intro q hq
  have hd := (reflect_width_domain x c q.swap).mp (by simpa only [Prod.swap_swap] using hq)
  have h := hm hd
  change widthLikelihood x c q.swap ≤ widthLikelihood x c p at h
  rw [← reflect_width_value x c q.swap, ← reflect_width_value x c p, Prod.swap_swap] at h
  exact h

/-- At held mode, every positive global maximizer has the same two
widths. One-sided samples include the zero width on the unused side. -/
theorem held_mode_unique (hn : 0 < n) (x : Fin n → ℝ) {c : ℝ}
    {p q : ℝ × ℝ} (hp : p ∈ widthDomain x c) (hq : q ∈ widthDomain x c)
    (hLp : 0 < widthLikelihood x c p) (hLq : 0 < widthLikelihood x c q)
    (hpm : IsMaxOn (widthLikelihood x c) (widthDomain x c) p)
    (hqm : IsMaxOn (widthLikelihood x c) (widthDomain x c) q) : p = q := by
  let y (j : Fin n) := -x j
  have eq_right {r t : ℝ × ℝ} (hr : r ∈ widthDomain x c) (ht : t ∈ widthDomain x c)
      (hLr : 0 < widthLikelihood x c r) (hLt : 0 < widthLikelihood x c t)
      (hrm : IsMaxOn (widthLikelihood x c) (widthDomain x c) r)
      (htm : IsMaxOn (widthLikelihood x c) (widthDomain x c) t)
      (he : r.1 = t.1) : r = t := by
    obtain ⟨hr',hmr'⟩ := width_slice_right hr hrm
    obtain ⟨ht',hmt'⟩ := width_slice_right ht htm
    rw [← he] at ht' hmt'
    exact held_lower_mode_unique hn x hr' ht' hLr hLt hmr' hmt'
  have hpr := (reflect_width_domain x c p).mpr hp
  have hqr := (reflect_width_domain x c q).mpr hq
  have hLpr : 0 < widthLikelihood y (-c) p.swap := (reflect_width_value x c p).symm ▸ hLp
  have hLqr : 0 < widthLikelihood y (-c) q.swap := (reflect_width_value x c q).symm ▸ hLq
  have hpmr := reflect_width_max hpm
  have hqmr := reflect_width_max hqm
  by_cases hl : ∃ j, x j < c
  · by_cases hr : ∃ j, c < x j
    · have hls : ∃ j, -c < y j := by
        obtain ⟨j,hj⟩ := hl
        exact ⟨j,neg_lt_neg hj⟩
      have hscore {r : ℝ × ℝ} (hs : r ∈ widthDomain x c)
          (hL : 0 < widthLikelihood x c r)
          (hm : IsMaxOn (widthLikelihood x c) (widthDomain x c) r) :
          rightTerms y (-c) r.1 = (n : ℝ)*r.1/(r.1+r.2) ∧
            rightTerms x c r.2 = (n : ℝ)*r.2/(r.1+r.2) := by
        obtain ⟨i,hi⟩ := hls
        obtain ⟨j,hj⟩ := hr
        have hs' := (reflect_width_domain x c r).mpr hs
        have hL' : 0 < widthLikelihood y (-c) r.swap := (reflect_width_value x c r).symm ▸ hL
        obtain ⟨h₁,h₁m⟩ := width_slice_right hs' (reflect_width_max hm)
        obtain ⟨h₂,h₂m⟩ := width_slice_right hs hm
        have hv := (sub_pos.mpr hj).trans (positive_right_slack hs hL hj)
        have hu := (sub_pos.mpr hi).trans (positive_right_slack hs' hL' hi)
        exact ⟨by simpa only [Prod.fst_swap,Prod.snd_swap,add_comm r.2 r.1] using
          right_score_at_winner h₁ hL' h₁m hu, right_score_at_winner h₂ hL h₂m hv⟩
      have hlt {r t : ℝ × ℝ} (hr' : r ∈ widthDomain x c) (ht : t ∈ widthDomain x c)
          (hLr : 0 < widthLikelihood x c r) (hLt : 0 < widthLikelihood x c t)
          (hrm : IsMaxOn (widthLikelihood x c) (widthDomain x c) r)
          (htm : IsMaxOn (widthLikelihood x c) (widthDomain x c) t)
          (he : r.1 < t.1) : False := by
        have hrr := (reflect_width_domain x c r).mpr hr'
        have hLrr : 0 < widthLikelihood y (-c) r.swap := (reflect_width_value x c r).symm ▸ hLr
        have hR := hscore hr' hLr hrm
        have hT := hscore ht hLt htm
        have hU := rightTerms_decrease (x := y) he
          (fun _ hj ↦ positive_right_slack hrr hLrr hj) hls
        rw [hR.1,hT.1] at hU
        have hsumr : 0 < r.1+r.2 := hr'.2.2.1
        have hsumt : 0 < t.1+t.2 := ht.2.2.1
        have hn' : (0 : ℝ) < n := by exact_mod_cast hn
        have hmul := (div_lt_div_iff₀ hsumt hsumr).mp hU
        have hcross : t.1*r.2 < r.1*t.2 := by
          have hh : (n : ℝ)*(t.1*r.2) < (n : ℝ)*(r.1*t.2) := by linarith only [hmul]
          exact lt_of_mul_lt_mul_left hh hn'.le
        have hv : r.2 < t.2 := by
          by_contra! h
          have h₁ := mul_le_mul_of_nonneg_left h hr'.1
          have h₂ := mul_le_mul_of_nonneg_right he.le hr'.2.1
          linarith
        have hV := rightTerms_decrease (x := x) hv
          (fun _ hj ↦ positive_right_slack hr' hLr hj) hr
        rw [hR.2,hT.2] at hV
        have hv' := (div_lt_div_iff₀ hsumt hsumr).mp hV
        linarith only [hmul,hv']
      rcases lt_trichotomy p.1 q.1 with h | h | h
      · exact (hlt hp hq hLp hLq hpm hqm h).elim
      · exact eq_right hp hq hLp hLq hpm hqm h
      · exact (hlt hq hp hLq hLp hqm hpm h).elim
    · have hs (j : Fin n) : x j ≤ c := le_of_not_gt (fun h ↦ hr ⟨j,h⟩)
      obtain ⟨hp',hpm'⟩ := width_slice_right hp hpm
      obtain ⟨hq',hqm'⟩ := width_slice_right hq hqm
      have he := unused_right_zero hn hp' hLp hpm' hs
      have he' := unused_right_zero hn hq' hLq hqm' hs
      have hd₁ : p ∈ heldUpperModeDomain x c c := ⟨hp,by simpa using he⟩
      have hd₂ : q ∈ heldUpperModeDomain x c c := ⟨hq,by simpa using he'⟩
      exact held_upper_mode_unique hn x hd₁ hd₂ hLp hLq
        (fun _ hr ↦ hpm hr.1) (fun _ hr ↦ hqm hr.1)
  · have hs (j : Fin n) : y j ≤ -c := neg_le_neg (le_of_not_gt (fun h ↦ hl ⟨j,h⟩))
    obtain ⟨hp',hm₁⟩ := width_slice_right hpr hpmr
    obtain ⟨hq',hm₂⟩ := width_slice_right hqr hqmr
    have he := unused_right_zero hn hp' hLpr hm₁ hs
    have he' := unused_right_zero hn hq' hLqr hm₂ hs
    exact eq_right hp hq hLp hLq hpm hqm (he.trans he'.symm)

private theorem knot_argmax_iff (hn : 0 < n) (x : Fin n → ℝ)
    (E : ℝ → ℝ → Prop) {p : ℝ × (ℝ × ℝ)}
    (hp : p ∈ jointDomain x ∧ E p.1 p.2.1) (hpos : 0 < jointLikelihood x p) :
    IsMaxOn (jointLikelihood x) {q | q ∈ jointDomain x ∧ E q.1 q.2.1} p ↔
      (∃ j, x j = p.2.2) ∧
      ∀ q, q ∈ jointDomain x → E q.1 q.2.1 → (∃ j, x j = q.2.2) →
        jointLikelihood x q ≤ jointLikelihood x p := by
  have hcomp : ¬ ((∃ j, x j = p.1) ∧ (∃ j, x j = p.2.1)) := by
    rintro ⟨h₁,h₂⟩
    exact hpos.ne' (likelihood_zero_of_both_endpoints hp.1.1 h₁ h₂ _)
  constructor
  · intro hm
    have hslice : IsMaxOn (likelihood p.1 p.2.1 x) (Icc p.1 p.2.1) p.2.2 :=
      fun c hc ↦ isMaxOn_iff.mp hm (p.1,p.2.1,c) ⟨⟨hp.1.1,hc,hp.1.2.2⟩,hp.2⟩
    exact ⟨((isMaxOn_iff_observation hp.1.1 hn hp.1.2.2 hcomp hp.1.2.1).mp hslice).1,
      fun q hq hE _ ↦ isMaxOn_iff.mp hm q ⟨hq,hE⟩⟩
  · rintro ⟨_,hm⟩ ⟨a,b,c⟩ ⟨hq,hE⟩
    change likelihood a b x c ≤ jointLikelihood x p
    by_cases h : 0 < likelihood a b x c
    · obtain ⟨j,hj⟩ := reduce_to_knot hn hq h
      exact hj.trans (hm (a,b,x j) ⟨hq.1,hq.2.2 j,hq.2.2⟩ hE ⟨j,rfl⟩)
    · exact (le_of_not_gt h).trans hpos.le

/-- For a held lower endpoint, positive maxima are exactly the observed
knot fits whose heights dominate all feasible widths at all knots.
Equality retains every winning knot. The width at each winning knot is
unique by `held_lower_mode_unique`. -/
theorem held_lower_knot_maximizers (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ}
    {p : ℝ × (ℝ × ℝ)} (hp : p ∈ heldLowerDomain x a) (hpos : 0 < jointLikelihood x p) :
    IsMaxOn (jointLikelihood x) (heldLowerDomain x a) p ↔
      (∃ j, x j = p.2.2) ∧
      ∀ q ∈ heldLowerDomain x a, (∃ j, x j = q.2.2) → jointLikelihood x q ≤ jointLikelihood x p := by
  have h := knot_argmax_iff hn x (fun l _ ↦ l = a) hp hpos
  exact h.trans ⟨fun ⟨hk,hm⟩ ↦ ⟨hk, fun q hq ↦ hm q hq.1 hq.2⟩,
    fun ⟨hk,hm⟩ ↦ ⟨hk, fun q hq hE ↦ hm q ⟨hq,hE⟩⟩⟩

/-- For a held upper endpoint, positive maxima likewise retain exactly
the equal winning knots. The width at each winning knot is unique by
`held_upper_mode_unique`. -/
theorem held_upper_knot_maximizers (hn : 0 < n) (x : Fin n → ℝ) {b : ℝ}
    {p : ℝ × (ℝ × ℝ)} (hp : p ∈ heldUpperDomain x b) (hpos : 0 < jointLikelihood x p) :
    IsMaxOn (jointLikelihood x) (heldUpperDomain x b) p ↔
      (∃ j, x j = p.2.2) ∧
      ∀ q ∈ heldUpperDomain x b, (∃ j, x j = q.2.2) → jointLikelihood x q ≤ jointLikelihood x p := by
  have h := knot_argmax_iff hn x (fun _ u ↦ u = b) hp hpos
  exact h.trans ⟨fun ⟨hk,hm⟩ ↦ ⟨hk, fun q hq ↦ hm q hq.1 hq.2⟩,
    fun ⟨hk,hm⟩ ↦ ⟨hk, fun q hq hE ↦ hm q ⟨hq,hE⟩⟩⟩

/-- The product extended by zero when any observation is outside support
or the proposed coefficients are invalid. -/
noncomputable def supportLikelihood (x : Fin n → ℝ) (p : ℝ × (ℝ × ℝ)) : ℝ := by
  classical
  exact if p ∈ jointDomain x then jointLikelihood x p else 0

/-- An excluded observation or an observed endpoint different from the
mode makes the entire likelihood zero. -/
theorem held_endpoint_incompatible_zero (x : Fin n → ℝ) (a b c : ℝ)
    (hx : ∃ j, x j < a ∨ b < x j ∨ (x j = a ∧ c ≠ a) ∨ (x j = b ∧ c ≠ b)) :
    supportLikelihood x (a,b,c) = 0 := by
  classical
  unfold supportLikelihood
  by_cases h : (a,b,c) ∈ jointDomain x
  · rw [ite_eq_left h]
    obtain ⟨j,hj⟩ := hx
    have hs := h.2.2 j
    change a ≤ x j ∧ x j ≤ b at hs
    rcases hj with hj | hj | ⟨hj,hc⟩ | ⟨hj,hc⟩
    · exact (hj.not_ge hs.1).elim
    · exact (hj.not_ge hs.2).elim
    · have hc' : a < c := h.2.1.1.lt_of_ne (Ne.symm hc)
      unfold jointLikelihood likelihood
      change (2/(b-a))^n * (∏ i, factor a b c (x i)) = 0
      rw [Finset.prod_eq_zero (Finset.mem_univ j) (by simp [factor,hj,hc']),mul_zero]
    · have hc' : c < b := h.2.1.2.lt_of_ne hc
      unfold jointLikelihood likelihood
      change (2/(b-a))^n * (∏ i, factor a b c (x i)) = 0
      rw [Finset.prod_eq_zero (Finset.mem_univ j) (by simp [factor,hj,hc',hc'.not_gt]),mul_zero]
  · rw [ite_eq_right h]

end UnivariateML.LocScaleTriangular

end
