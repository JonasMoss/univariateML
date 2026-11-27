module

public import UnivariateML.Family.FoldedNormal

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Joint attainment for the folded normal

Profile the variance on a compact interval uniformly over the relevant
location range. A pair of distinct observations bounds the residual
moment away from zero.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.LocScaleFoldedNormal

open FoldedNormal

variable {n : ℕ}

private theorem location_cap (x : Fin n → ℝ) {M μ t : ℝ} (hx : ∀ i, 0 ≤ x i)
    (hM : ∀ i, x i ≤ M) (hμ : M ≤ μ) (ht : 0 < t) :
    logLik x μ t ≤ logLik x M t := by
  have hD : D x M ≤ D x μ := by
    unfold D avg
    exact div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ by
      have h := hM i
      nlinarith [mul_nonneg (sub_nonneg.mpr hμ) (show 0 ≤ μ + M - 2*x i by linarith)])
      (Nat.cast_nonneg _)
  have hS : avg (fun i ↦ log (1 + exp (-(2 * μ * x i) / t))) ≤
      avg (fun i ↦ log (1 + exp (-(2 * M * x i) / t))) := by
    unfold avg
    refine div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ ?_) (Nat.cast_nonneg _)
    have he : -(2 * μ * x i)/t ≤ -(2 * M * x i)/t :=
      div_le_div_of_nonneg_right (by nlinarith [mul_nonneg (sub_nonneg.mpr hμ) (hx i)]) ht.le
    exact log_le_log (by positivity) (add_le_add_right (exp_le_exp.mpr he) _)
  unfold logLik
  linarith [div_le_div_of_nonneg_right hD (show 0 ≤ 2*t by positivity)]

private theorem residual_lower (x : Fin n → ℝ) {i j : Fin n} (hneq : x i ≠ x j)
    (μ : ℝ) : (x i-x j)^2/(2*n) ≤ D x μ := by
  have hij : i ≠ j := fun h ↦ hneq (congrArg x h)
  have hsum : (x i-μ)^2+(x j-μ)^2 ≤ ∑ k, (x k-μ)^2 := by
    have h := Finset.sum_le_sum_of_subset_of_nonneg
      (show ({i,j} : Finset (Fin n)) ⊆ Finset.univ from Finset.subset_univ _) (f := fun k ↦ (x k-μ)^2)
      (fun k _ _ ↦ sq_nonneg (x k-μ))
    simpa [Finset.sum_insert, hij] using h
  have hhalf : (x i-x j)^2 / 2 ≤ ∑ k, (x k-μ)^2 := by nlinarith [sq_nonneg (x i+x j-2*μ)]
  unfold D avg
  rw [← div_div]
  exact div_le_div_of_nonneg_right hhalf (Nat.cast_nonneg _)

private theorem joint_attains (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃ μ t, 0 ≤ μ ∧ 0 < t ∧ ∀ a b, 0 ≤ a → 0 < b → logLik x a b ≤ logLik x μ t := by
  obtain ⟨i,j,hij⟩ := hne
  obtain ⟨u,_,hu⟩ := Finset.exists_max_image Finset.univ x ⟨i,Finset.mem_univ i⟩
  let M := x u
  let δ := (x i-x j)^2/(2*n)
  let B := m₂ x + M^2
  let K := Icc 0 M ×ˢ Icc δ B
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  have hδ : 0 < δ := div_pos (sq_pos_of_ne_zero (sub_ne_zero.mpr hij)) (by positivity)
  have hprofile (a : ℝ) (ha : 0 ≤ a) (hM : a ≤ M) :
      ∃ b, (a,b) ∈ K ∧ ∀ t > 0, logLik x a t ≤ logLik x a b := by
    have hD := residual_lower x hij a
    obtain ⟨b,hb,_,_,hbmax⟩ := heldLocation hn hx ha (hδ.trans_le hD)
    refine ⟨b,⟨⟨ha,hM⟩,⟨hD.trans hb.1,hb.2.trans ?_⟩⟩,fun t ht ↦ ?_⟩
    · dsimp [B]
      nlinarith [mul_nonneg (sub_nonneg.mpr hM) (add_nonneg ha (hx u))]
    · by_cases h : t = b
      · simp [h]
      · exact (hbmax t ht h).le
  obtain ⟨r,hr,_⟩ := hprofile 0 le_rfl (hx u)
  have hcont : ContinuousOn (fun p : ℝ × ℝ ↦ logLik x p.1 p.2) K := by
    intro p hp
    have hpos : 0 < p.2 := hδ.trans_le hp.2.1
    unfold logLik D avg
    have hlog : ContinuousAt (fun a : ℝ × ℝ ↦ log a.2) p :=
      continuous_snd.continuousAt.log hpos.ne'
    have hD : ContinuousAt (fun a : ℝ × ℝ ↦ (∑ i, (x i-a.1)^2) / n / (2*a.2)) p :=
      ContinuousAt.div (by fun_prop) (by fun_prop) (by positivity)
    have hsum : ContinuousAt (fun a : ℝ × ℝ ↦ (∑ i, log (1 + exp (-(2*a.1*x i)/a.2)))/n) p := by
      apply ContinuousAt.div_const
      let f (l : Fin n) (a : ℝ × ℝ) := log (1 + exp (-(2*a.1*x l)/a.2))
      have hf (l : Fin n) : ContinuousAt (f l) p := by
        apply ContinuousAt.log
        · apply continuousAt_const.add
          apply Real.continuous_exp.continuousAt.comp
          exact ContinuousAt.div (by fun_prop) continuous_snd.continuousAt hpos.ne'
        · exact (show 0 < 1 + exp (-(2*p.1*x l)/p.2) by positivity).ne'
      have hs (J : Finset (Fin n)) : ContinuousAt (fun a ↦ ∑ l ∈ J, f l a) p := by
        classical
        induction J using Finset.induction_on with
        | empty => simpa only [Finset.sum_empty] using
          (continuousAt_const : ContinuousAt (fun _ : ℝ × ℝ ↦ (0 : ℝ)) p)
        | insert j J hj hJ =>
          have he : (fun a ↦ ∑ l ∈ insert j J, f l a) =
              f j + (fun a ↦ ∑ l ∈ J, f l a) := funext fun a ↦ Finset.sum_insert hj
          rw [he]
          exact (hf j).add hJ
      exact hs Finset.univ
    exact ((hlog.neg.div_const 2).sub hD |>.add hsum).continuousWithinAt
  obtain ⟨p,hp,hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨(0,r),hr⟩ hcont
  refine ⟨p.1,p.2,hp.1.1,hδ.trans_le hp.2.1,fun a b ha hb ↦ ?_⟩
  let q := min a M
  obtain ⟨t,ht,htm⟩ := hprofile q (le_min ha (hx u)) (min_le_right _ _)
  have hv : logLik x a b ≤ logLik x q b := by
    rcases le_total a M with h | h
    · simp [q,min_eq_left h]
    · rw [show q = M from min_eq_right h]
      exact location_cap x hx (fun k ↦ hu k (Finset.mem_univ k)) h hb
  exact hv.trans ((htm b hb).trans (hm ht))

/-- The all-free folded normal has a finite MLE exactly on nonconstant
nonempty nonnegative samples. -/
theorem joint_criterion (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) :
    (∃ μ t, 0 ≤ μ ∧ 0 < t ∧ ∀ a b, 0 ≤ a → 0 < b → logLik x a b ≤ logLik x μ t) ↔
    ∃ i j, x i ≠ x j := by
  refine ⟨?_,joint_attains hn x hx⟩
  rintro ⟨μ,t,hμ,ht,hm⟩
  by_contra! h
  let i : Fin n := ⟨0,hn⟩
  have hD : D x (x i) = 0 := by simp [D,avg,fun j ↦ h j i]
  obtain ⟨b,hb,hv⟩ := ((eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))).and
    ((heldLocation_unbounded hD).eventually_gt_atTop (logLik x μ t))).exists
  exact hv.not_ge (hm (x i) b (hx i) hb)

/-- At held location the collapse detector is exactly the event that
every observation equals that location. -/
theorem held_location_collapse_detector (hn : 0 < n) (x : Fin n → ℝ)
    (hx : ∀ i, 0 ≤ x i) {μ : ℝ} (hμ : 0 ≤ μ) :
    (¬ ∃ t > 0, ∀ b > 0, logLik x μ b ≤ logLik x μ t) ↔ ∀ i, x i = μ := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  constructor
  · intro hm i
    by_contra h
    have hD : 0 < D x μ := div_pos
      (lt_of_lt_of_le (sq_pos_of_ne_zero (sub_ne_zero.mpr h))
        (Finset.single_le_sum (f := fun j ↦ (x j-μ)^2)
          (fun j _ ↦ sq_nonneg _) (Finset.mem_univ i))) hN
    obtain ⟨t,_,ht,_,hmax⟩ := heldLocation hn hx hμ hD
    apply hm
    refine ⟨t,ht,fun b hb ↦ ?_⟩
    by_cases he : b = t
    · simp [he]
    · exact (hmax _ hb he).le
  · rintro h ⟨t,_,hm⟩
    have hD : D x μ = 0 := by simp [D,avg,h]
    obtain ⟨b,hb,hv⟩ := ((eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))).and
      ((heldLocation_unbounded hD).eventually_gt_atTop (logLik x μ t))).exists
    exact hv.not_ge (hm b hb)

end UnivariateML.LocScaleFoldedNormal

end
