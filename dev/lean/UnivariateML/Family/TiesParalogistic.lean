module

public import UnivariateML.Family.Paralogistic
public import UnivariateML.Family.Weibull

import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Reciprocation of paralogistic maximizing sets

The two free coefficients are shape and scale. At held scale the same
transport acts as the identity on feasible shapes.
-/

@[expose] public section

namespace UnivariateML.TiesParalogistic

open Set Real

noncomputable section

/-- The inverse model's entire maximizing set maps bijectively to the
paralogistic maximizing set on reciprocal data. The map fixes shape and
inverts scale. -/
theorem reciprocal_joint_maximizers {n : ℕ} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) :
    BijOn (fun p : ℝ × ℝ ↦ (p.1, p.2⁻¹))
      {p ∈ Weibull.domain | IsMaxOn
        (fun q : ℝ × ℝ ↦ InvParalogistic.logLik x q.1 q.2) Weibull.domain p}
      {p ∈ Weibull.domain | IsMaxOn
        (fun q : ℝ × ℝ ↦ Paralogistic.logLik (Transfer.reciprocal x) q.1 q.2)
        Weibull.domain p} := by
  let f (p : ℝ × ℝ) : ℝ × ℝ := (p.1, p.2⁻¹)
  have hInv (p : ℝ × ℝ) : f (f p) = p := by simp [f]
  have hDom (p : ℝ × ℝ) (hp : p ∈ Weibull.domain) : f p ∈ Weibull.domain :=
    ⟨hp.1, inv_pos.mpr hp.2⟩
  have hBij : BijOn f Weibull.domain Weibull.domain := by
    refine ⟨hDom, ?_, ?_⟩
    · intro p _ q _ h
      simpa only [hInv] using congrArg f h
    · exact fun p hp ↦ ⟨f p, hDom p hp, hInv p⟩
  exact Transfer.bijOn_maximizers hBij
    (fun p _ ↦ InvParalogistic.logLik_reciprocal hx p.1 p.2)

/-- Holding a positive inverse scale fixes the reciprocal scale in the
parent model and preserves the complete set of maximizing shapes. -/
theorem reciprocal_held_scale_maximizers {n : ℕ} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {s : ℝ} (_hs : 0 < s) :
    {a ∈ Ioi 0 | IsMaxOn (fun b ↦ InvParalogistic.logLik x b s) (Ioi 0) a} =
      {a ∈ Ioi 0 | IsMaxOn
        (fun b ↦ Paralogistic.logLik (Transfer.reciprocal x) b s⁻¹) (Ioi 0) a} := by
  ext a
  simp only [mem_ofPred_eq]
  constructor
  · rintro ⟨ha, hm⟩
    exact ⟨ha, (Transfer.isMaxOn_iff (bijOn_id (Ioi 0))
      (fun b _ ↦ InvParalogistic.logLik_reciprocal hx b s) ha).mp hm⟩
  · rintro ⟨ha, hm⟩
    exact ⟨ha, (Transfer.isMaxOn_iff (bijOn_id (Ioi 0))
      (fun b _ ↦ InvParalogistic.logLik_reciprocal hx b s) ha).mpr hm⟩


open Paralogistic (shapeLik shapeScore tieLogs sigmoid)
open BurrHeld (softplus avg)

private theorem scaled_log_bound (k : ℕ) {t q : ℝ} (ht : |t| < 1)
    (hq : q = 2 ^ k * (1 - t)) :
    |log q - ((k : ℝ) * log 2 -
      ∑ i ∈ Finset.range 12, t ^ (i + 1) / (i + 1))| ≤ |t| ^ 13 / (1 - |t|) := by
  rw [hq, log_mul (by positivity) (sub_pos.mpr ((abs_lt.mp ht).2)).ne', log_pow]
  convert abs_log_sub_add_sum_range_le ht 12 using 2
  ring

private theorem exp_one_lower : (36/100 : ℝ) ≤ exp (-(1 : ℝ)) := by
  apply (log_le_log_iff (by norm_num) (exp_pos _)).mp
  rw [log_exp, show (36/100 : ℝ) = (100/36 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 2 (t := 44/144) (q := 100/36)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem exp_mid_lower : (4/100 : ℝ) ≤ exp (-(16/5 : ℝ)) := by
  apply (log_le_log_iff (by norm_num) (exp_pos _)).mp
  rw [log_exp, show (4/100 : ℝ) = (100/4 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 5 (t := 28/128) (q := 100/4)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem exp_edge_lower : (76/10000 : ℝ) ≤ exp (-(4875/1000 : ℝ)) := by
  apply (log_le_log_iff (by norm_num) (exp_pos _)).mp
  rw [log_exp, show (76/10000 : ℝ) = (10000/76 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 8 (t := 9456/19456) (q := 10000/76)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem exp_edge_upper : exp (-(4875/1000 : ℝ)) ≤ (77/10000 : ℝ) := by
  apply (log_le_log_iff (exp_pos _) (by norm_num)).mp
  rw [log_exp, show (77/10000 : ℝ) = (10000/77 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 8 (t := 9712/19712) (q := 10000/77)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private theorem exp_5688_lower : (337/100000 : ℝ) ≤ exp (-(5688/1000 : ℝ)) := by
  apply (log_le_log_iff (by norm_num) (exp_pos _)).mp
  rw [log_exp, show (337/100000 : ℝ) = (100000/337 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 9 (t := 72544/172544) (q := 100000/337)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem exp_5688_upper : exp (-(5688/1000 : ℝ)) ≤ (34/10000 : ℝ) := by
  apply (log_le_log_iff (exp_pos _) (by norm_num)).mp
  rw [log_exp, show (34/10000 : ℝ) = (10000/34 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 9 (t := 7408/17408) (q := 10000/34)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private theorem exp_6094_lower : (221/100000 : ℝ) ≤ exp (-(6094/1000 : ℝ)) := by
  apply (log_le_log_iff (by norm_num) (exp_pos _)).mp
  rw [log_exp, show (221/100000 : ℝ) = (100000/221 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 9 (t := 13152/113152) (q := 100000/221)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem exp_6094_upper : exp (-(6094/1000 : ℝ)) ≤ (23/10000 : ℝ) := by
  apply (log_le_log_iff (exp_pos _) (by norm_num)).mp
  rw [log_exp, show (23/10000 : ℝ) = (10000/23 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 9 (t := 1776/11776) (q := 10000/23)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private theorem exp_6500_lower : (1495/1000000 : ℝ) ≤ exp (-(6500/1000 : ℝ)) := by
  apply (log_le_log_iff (by norm_num) (exp_pos _)).mp
  rw [log_exp, show (1495/1000000 : ℝ) = (1000000/1495 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 10 (t := 530880/1530880) (q := 1000000/1495)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem exp_10_upper : exp (-(10 : ℝ)) ≤ (1/20000 : ℝ) := by
  apply (log_le_log_iff (exp_pos _) (by norm_num)).mp
  rw [log_exp, show (1/20000 : ℝ) = (20000/1 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 15 (t := 12768/32768) (q := 20000/1)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private theorem exp_14_upper : exp (-(14 : ℝ)) ≤ (1/1000000 : ℝ) := by
  apply (log_le_log_iff (exp_pos _) (by norm_num)).mp
  rw [log_exp, show (1/1000000 : ℝ) = (1000000/1 : ℝ)⁻¹ by norm_num, log_inv]
  have h := (abs_le.mp (scaled_log_bound 20 (t := 48576/1048576) (q := 1000000/1)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private theorem softplus_pos (t : ℝ) : 0 < softplus t :=
  log_pos (lt_add_of_pos_right _ (exp_pos _))

private theorem softplus_upper (t : ℝ) : softplus t ≤ exp t := by
  unfold softplus
  linarith only [log_le_sub_one_of_pos (show 0 < 1 + exp t by positivity)]

private theorem sigmoid_pos (t : ℝ) : 0 < sigmoid t := by
  unfold sigmoid
  positivity

private theorem sigmoid_lower {c t : ℝ} (hc : 0 ≤ c) (ht : c ≤ exp t) :
    c / (1 + c) ≤ sigmoid t := by
  unfold sigmoid
  apply (div_le_div_iff₀ (by positivity) (by positivity)).mpr
  linarith only [ht]

private theorem sigmoid_le_softplus (t : ℝ) : sigmoid t ≤ softplus t := by
  unfold sigmoid softplus
  have hp : 0 < 1 + exp t := by positivity
  have h := log_le_sub_one_of_pos (inv_pos.mpr hp)
  rw [log_inv] at h
  have he : (1 + exp t)⁻¹ - 1 = -(exp t / (1 + exp t)) := by
    field_simp [hp.ne']
    ring
  linarith only [h, he]

private theorem sigmoid_le_exp (t : ℝ) : sigmoid t ≤ exp t :=
  (sigmoid_le_softplus t).trans (softplus_upper t)

private theorem softplus_le_log_two {t : ℝ} (ht : t ≤ 0) : softplus t ≤ log 2 := by
  unfold softplus
  exact log_le_log (by positivity) (by linarith only [exp_le_one_iff.mpr ht])

private theorem sigmoid_le_half {t : ℝ} (ht : t ≤ 0) : sigmoid t ≤ 1 / 2 := by
  unfold sigmoid
  apply (div_le_iff₀ (by positivity)).mpr
  linarith only [exp_le_one_iff.mpr ht]

private theorem softplus_tangent (t : ℝ) : log 2 + t / 2 ≤ softplus t := by
  have h : 2 * exp (t / 2) ≤ 1 + exp t := by
    have he : exp t = exp (t / 2) * exp (t / 2) := by
      rw [← exp_add, show t / 2 + t / 2 = t by ring]
    rw [he]
    nlinarith only [sq_nonneg (exp (t / 2) - 1)]
  rw [← log_exp (t / 2), ← log_mul (by norm_num : (2 : ℝ) ≠ 0) (exp_pos _).ne']
  exact log_le_log (by positivity) h

private theorem held_score (a r : ℝ) :
    shapeScore (tieLogs r) a =
      2 / a - (1 / 1000 + r) / 2 -
        (softplus (-(a / 1000)) + softplus (-(a * r))) / 2 +
        (a + 1) * ((1 / 1000) * sigmoid (-(a / 1000)) + r * sigmoid (-(a * r))) / 2 := by
  simp only [shapeScore, avg, tieLogs, Fin.sum_univ_two, Matrix.cons_val_zero,
    Matrix.cons_val_one, Nat.cast_ofNat]
  rw [show a * (-1 / 1000) = -(a / 1000) by ring, mul_neg a r]
  ring

private theorem held_value (a r : ℝ) :
    shapeLik (tieLogs r) a = 2 * log a - a * (1 / 1000 + r) / 2 -
      (a + 1) * (softplus (-(a / 1000)) + softplus (-(a * r))) / 2 := by
  have hshift (t : ℝ) : softplus t = t + softplus (-t) := by
    unfold softplus
    have h : exp t * (1 + exp (-t)) = 1 + exp t := by
      rw [exp_neg, mul_add, mul_one, mul_inv_cancel₀ (exp_pos _).ne', add_comm]
    rw [← h, log_mul (exp_pos _).ne' (by positivity), log_exp]
  simp only [shapeLik, avg, tieLogs, Fin.sum_univ_two, Matrix.cons_val_zero,
    Matrix.cons_val_one, Nat.cast_ofNat]
  rw [show a * (-1 / 1000) = -(a / 1000) by ring, mul_neg a r,
    neg_neg, neg_neg, hshift (a / 1000), hshift (a * r)]
  ring

private theorem witness_gt {r : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000)) :
    -1 < shapeLik (tieLogs r) 3 := by
  rw [held_value]
  have h₁ := softplus_le_log_two (t := -((3 : ℝ) / 1000)) (by norm_num)
  have h₂ := softplus_le_log_two (t := -(3 * r)) (by linarith only [hr.1])
  linarith only [hr.2, h₁, h₂, log_two_lt_d9, log_three_gt_d9]

private theorem early_positive {a r : ℝ} (ha : a ∈ Icc (1 : ℝ) 2)
    (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000)) : 0 < shapeScore (tieLogs r) a := by
  have hap : 0 < a := by linarith only [ha.1]
  have hrp : 0 ≤ r := by linarith only [hr.1]
  have hd : 0 ≤ a * r := mul_nonneg hap.le hrp
  have h₁ := softplus_le_log_two (t := -(a / 1000)) (by linarith)
  have h₂ := softplus_le_log_two (neg_nonpos.mpr hd)
  have ha' : 1 ≤ 2 / a := (le_div_iff₀ hap).mpr (by linarith only [ha.2])
  have h₃ : 0 ≤ (a + 1) * ((1 / 1000) * sigmoid (-(a / 1000)) +
      r * sigmoid (-(a * r))) / 2 := by
    have := sigmoid_pos (-(a / 1000))
    have := sigmoid_pos (-(a * r))
    positivity
  rw [held_score]
  linarith only [h₁, h₂, ha', h₃, hr.2, log_two_lt_d9]

private theorem middle_negative {a r l u : ℝ} (ha : a ∈ Icc l u) (hl : 0 < l)
    (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000))
    (hcheck : 2 / l - 693 / 1000 + (3 / 2000) * u + (3 / 2000) * (u + 1) < 0) :
    shapeScore (tieLogs r) a < 0 := by
  have hap := hl.trans_le ha.1
  have hrp : 0 < r := by linarith only [hr.1]
  have hpr : a * r ≤ u * (5 / 1000) :=
    mul_le_mul ha.2 hr.2 hrp.le (hap.le.trans ha.2)
  have h₁ := softplus_tangent (-(a / 1000))
  have h₂ := softplus_tangent (-(a * r))
  have h₃ := sigmoid_le_half (t := -(a / 1000)) (by linarith)
  have h₄ := sigmoid_le_half (neg_nonpos.mpr (mul_nonneg hap.le hrp.le))
  have hd : (1 / 1000) * sigmoid (-(a / 1000)) + r * sigmoid (-(a * r)) ≤
      3 / 1000 := by
    have hz := mul_le_mul_of_nonneg_left h₄ hrp.le
    linarith only [h₃, hz, hr.2]
  have h₅ : (a + 1) * ((1 / 1000) * sigmoid (-(a / 1000)) +
      r * sigmoid (-(a * r))) ≤ (u + 1) * (3 / 1000) :=
    mul_le_mul (by linarith only [ha.2]) hd (by
      have := sigmoid_pos (-(a / 1000))
      have := sigmoid_pos (-(a * r))
      positivity) (by linarith [hap.le.trans ha.2])
  have h₆ := div_le_div_of_nonneg_left (show (0 : ℝ) ≤ 2 by norm_num) hl ha.1
  rw [held_score]
  linarith only [hcheck, h₁, h₂, h₅, h₆, hpr, ha.2, hr.1, log_two_gt_d9]

private theorem log_5000_lt : log (5000 : ℝ) < 171 / 20 := by
  rw [show (5000 : ℝ) = 2 ^ 3 * 5 ^ 4 by norm_num, log_mul (by positivity)
    (by positivity), log_pow, log_pow]
  norm_num
  linarith only [log_two_lt_d9, log_five_lt_d9]

private theorem valley_loses {a r l u c : ℝ} (ha : a ∈ Icc l u) (hl : 0 < l)
    (hu : u ≤ 5000) (hc : 0 ≤ c) (hexp : c ≤ exp (-(u / 1000)))
    (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000))
    (hcheck : (171 / 10 : ℝ) - l / 500 - (l + 1) * (c / (1 + c)) / 2 < -1) :
    shapeLik (tieLogs r) a < shapeLik (tieLogs r) 3 := by
  have hap := hl.trans_le ha.1
  have hlog := (log_le_log hap (ha.2.trans hu)).trans_lt log_5000_lt
  have h₁ := (sigmoid_lower hc (hexp.trans (exp_le_exp.mpr
    (by linarith only [ha.2] : -(u / 1000) ≤ -(a / 1000))))).trans
    (sigmoid_le_softplus (-(a / 1000)))
  have h₂ := (softplus_pos (-(a * r))).le
  have h₃ := mul_le_mul (show l ≤ a from ha.1)
    (show (1 / 250 : ℝ) ≤ 1 / 1000 + r by linarith only [hr.1]) (by norm_num) hap.le
  have h₄ := mul_le_mul (show l + 1 ≤ a + 1 by linarith only [ha.1])
    (show c / (1 + c) ≤ softplus (-(a / 1000)) + softplus (-(a * r)) by linarith only [h₁, h₂])
    (by positivity : 0 ≤ c / (1 + c)) (by positivity : 0 ≤ a + 1)
  rw [held_value a]
  exact (show 2 * log a - a * (1 / 1000 + r) / 2 -
    (a + 1) * (softplus (-(a / 1000)) + softplus (-(a * r))) / 2 < -1 by
      linarith only [hcheck, hlog, h₃, h₄]).trans (witness_gt hr)

private theorem later_positive {a r l u c d : ℝ} (ha : a ∈ Icc l u) (hl : 4875 ≤ l)
    (hc : 0 ≤ c) (hupper : exp (-(l / 1000)) ≤ d) (hlower : c ≤ exp (-(u / 1000)))
    (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000))
    (hcheck : -(3 / 1000 : ℝ) - (d + 1 / 1000000) / 2 +
      (l + 1) * (c / (1 + c)) / 2000 > 0) :
    0 < shapeScore (tieLogs r) a := by
  have hap : 0 < a := by linarith only [hl, ha.1]
  have hrp : 0 ≤ r := by linarith only [hr.1]
  have hpr : (14 : ℝ) ≤ a * r := by
    have hh := mul_le_mul (hl.trans ha.1) hr.1 (by norm_num : (0 : ℝ) ≤ 3 / 1000) hap.le
    linarith only [hh]
  have h₁ := (softplus_upper _).trans ((exp_le_exp.mpr
    (show -(a / 1000) ≤ -(l / 1000) by linarith only [ha.1])).trans hupper)
  have h₂ := (softplus_upper _).trans ((exp_le_exp.mpr (neg_le_neg hpr)).trans exp_14_upper)
  have h₃ := sigmoid_lower hc (hlower.trans
    (exp_le_exp.mpr (show -(u / 1000) ≤ -(a / 1000) by linarith only [ha.2])))
  have h₄ := mul_le_mul (show l + 1 ≤ a + 1 by linarith only [ha.1])
    (show (1 / 1000) * (c / (1 + c)) ≤
      (1 / 1000) * sigmoid (-(a / 1000)) + r * sigmoid (-(a * r)) by
        have := mul_nonneg hrp (sigmoid_pos (-(a * r))).le
        linarith only [h₃, this])
    (by positivity : 0 ≤ (1 / 1000) * (c / (1 + c))) (by positivity : 0 ≤ a + 1)
  rw [held_score]
  linarith only [hcheck, h₁, h₂, h₄, hr.2, div_pos (show (0 : ℝ) < 2 by norm_num) hap]

private theorem latest_negative {a r : ℝ} (ha : a ∈ Icc (10000 : ℝ) 20000)
    (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000)) : shapeScore (tieLogs r) a < 0 := by
  have hap : 0 < a := by linarith only [ha.1]
  have h₁ := (sigmoid_le_exp (-(a / 1000))).trans
    ((exp_le_exp.mpr (show -(a / 1000) ≤ -10 by linarith only [ha.1])).trans exp_10_upper)
  have hpr : 14 ≤ a * r := by
    have hh := mul_le_mul ha.1 hr.1 (by norm_num : (0 : ℝ) ≤ 3 / 1000) hap.le
    linarith only [hh]
  have h₂ := (sigmoid_le_exp (-(a * r))).trans
    ((exp_le_exp.mpr (neg_le_neg hpr)).trans exp_14_upper)
  have h₃ := mul_le_mul hr.2 h₂ (sigmoid_pos (-(a * r))).le
    (by norm_num : (0 : ℝ) ≤ 5 / 1000)
  have h₄ := mul_le_mul (show a + 1 ≤ 20001 by linarith only [ha.2])
    (show (1 / 1000) * sigmoid (-(a / 1000)) + r * sigmoid (-(a * r)) ≤
      (1 / 1000) / 20000 + (5 / 1000) / 1000000 by linarith only [h₁, h₃])
    (by
      have := sigmoid_pos (-(a / 1000))
      have := sigmoid_pos (-(a * r))
      have : 0 ≤ r := by linarith only [hr.1]
      positivity) (by norm_num : (0 : ℝ) ≤ 20001)
  have h₅ := div_le_div_of_nonneg_left (show (0 : ℝ) ≤ 2 by norm_num)
    (show (0 : ℝ) < 10000 by norm_num) ha.1
  rw [held_score]
  linarith only [h₄, h₅, hr.1, softplus_pos (-(a / 1000)), softplus_pos (-(a * r))]

private theorem held_cover {r : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000))
    {a : ℝ} (ha : a ∈ Icc (1 : ℝ) 20000) (hlo : a ∉ Icc (2 : ℝ) 4)
    (hhi : a ∉ Icc (6500 : ℝ) 10000) (hz : shapeScore (tieLogs r) a = 0) :
    shapeLik (tieLogs r) a < shapeLik (tieLogs r) 3 := by
  by_cases h₂ : a ≤ 2
  · exact ((early_positive ⟨ha.1, h₂⟩ hr).ne' hz).elim
  have h₄ : 4 ≤ a := le_of_not_ge fun h ↦ hlo ⟨le_of_not_ge h₂, h⟩
  by_cases h₅₀ : a ≤ 50
  · exact ((middle_negative ⟨h₄, h₅₀⟩ (by norm_num) hr (by norm_num)).ne hz).elim
  by_cases h₁₀₀ : a ≤ 100
  · exact ((middle_negative ⟨le_of_not_ge h₅₀, h₁₀₀⟩ (by norm_num) hr
      (by norm_num)).ne hz).elim
  by_cases h₂₀₀ : a ≤ 200
  · exact ((middle_negative ⟨le_of_not_ge h₁₀₀, h₂₀₀⟩ (by norm_num) hr
      (by norm_num)).ne hz).elim
  by_cases h₁₀₀₀ : a ≤ 1000
  · exact valley_loses (c := 36 / 100) ⟨le_of_not_ge h₂₀₀, h₁₀₀₀⟩ (by norm_num) (by norm_num)
      (by norm_num) (by convert exp_one_lower using 2; norm_num) hr (by norm_num)
  by_cases h₃₂₀₀ : a ≤ 3200
  · exact valley_loses (c := 4 / 100) ⟨le_of_not_ge h₁₀₀₀, h₃₂₀₀⟩ (by norm_num) (by norm_num)
      (by norm_num) (by convert exp_mid_lower using 2; norm_num) hr (by norm_num)
  by_cases h₄₈₇₅ : a ≤ 4875
  · exact valley_loses ⟨le_of_not_ge h₃₂₀₀, h₄₈₇₅⟩ (by norm_num) (by norm_num)
      (by norm_num) exp_edge_lower hr (by norm_num)
  by_cases h₅₆₈₈ : a ≤ 5688
  · exact ((later_positive ⟨le_of_not_ge h₄₈₇₅, h₅₆₈₈⟩ (by norm_num) (by norm_num)
      exp_edge_upper exp_5688_lower hr (by norm_num)).ne' hz).elim
  by_cases h₆₀₉₄ : a ≤ 6094
  · exact ((later_positive ⟨le_of_not_ge h₅₆₈₈, h₆₀₉₄⟩ (by norm_num) (by norm_num)
      exp_5688_upper exp_6094_lower hr (by norm_num)).ne' hz).elim
  by_cases h₆₅₀₀ : a ≤ 6500
  · exact ((later_positive ⟨le_of_not_ge h₆₀₉₄, h₆₅₀₀⟩ (by norm_num) (by norm_num)
      exp_6094_upper exp_6500_lower hr (by norm_num)).ne' hz).elim
  have h₁₀₀₀₀ : 10000 ≤ a := le_of_not_ge fun h ↦ hhi ⟨le_of_not_ge h₆₅₀₀, h⟩
  exact ((latest_negative ⟨h₁₀₀₀₀, ha.2⟩ hr).ne hz).elim

private def heldPair (f : ℝ → ℝ) : Prop :=
  ∃ a ∈ Icc (2 : ℝ) 4, ∃ b ∈ Icc (6500 : ℝ) 10000,
    {k ∈ Ioi 0 | IsMaxOn f (Ioi 0) k} = {a, b}

private def held (r a : ℝ) := Paralogistic.logLik (fun i ↦ exp (tieLogs r i)) a 1
private def inverseHeld (r a : ℝ) := InvParalogistic.logLik (fun i ↦ exp (-(tieLogs r i))) a 1

private theorem held_identity (r a : ℝ) :
    held r a = shapeLik (tieLogs r) a - avg (tieLogs r) := by
  have he : BurrHeld.logRatios (fun i ↦ exp (tieLogs r i)) 1 = tieLogs r := by
    funext i
    simp [BurrHeld.logRatios]
  unfold held Paralogistic.logLik
  rw [BurrHeld.logLik_eq_scale (by decide), he]
  simp only [shapeLik, BurrHeld.scaleLik, BurrHeld.G, BurrHeld.R, avg, log_exp,
    Finset.sum_add_distrib, ← Finset.mul_sum, add_div]
  ring

private theorem tied_band_heights {r : ℝ} (h : heldPair (held r)) :
    Paralogistic.bandHeight (Icc 2 4) r = Paralogistic.bandHeight (Icc 6500 10000) r := by
  obtain ⟨a, ha, b, hb, he⟩ := h
  have ham : a ∈ {k ∈ Ioi 0 | IsMaxOn (held r) (Ioi 0) k} := by rw [he]; simp
  have hbm : b ∈ {k ∈ Ioi 0 | IsMaxOn (held r) (Ioi 0) k} := by rw [he]; simp
  have hbest {q : ℝ} (hq : q ∈ {k ∈ Ioi 0 | IsMaxOn (held r) (Ioi 0) k})
      {t : ℝ} (ht : 0 < t) : shapeLik (tieLogs r) t ≤ shapeLik (tieLogs r) q := by
    have hh : held r t ≤ held r q := hq.2 (show t ∈ Ioi 0 from ht)
    rwa [held_identity, held_identity, sub_le_sub_iff_right] at hh
  have hheight {u l v : ℝ} (hlo : 0 < l) (hu : u ∈ Icc l v)
      (hum : u ∈ {k ∈ Ioi 0 | IsMaxOn (held r) (Ioi 0) k}) :
      Paralogistic.bandHeight (Icc l v) r = shapeLik (tieLogs r) u := by
    unfold Paralogistic.bandHeight
    apply IsGreatest.csSup_eq
    refine ⟨mem_image_of_mem _ hu, ?_⟩
    rintro _ ⟨t, ht, rfl⟩
    exact hbest hum (hlo.trans_le ht.1)
  rw [hheight (by norm_num) ha ham, hheight (by norm_num) hb hbm]
  exact le_antisymm (hbest hbm ham.1) (hbest ham hbm.1)

/-- The held-scale construction has one real crossing parameter and
exactly two global maximizers there, one in each specified shape band. -/
theorem held_scale_global_tie :
    ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧
      ∃ a ∈ Icc (2 : ℝ) 4, ∃ b ∈ Icc (6500 : ℝ) 10000,
        {k ∈ Ioi 0 | IsMaxOn (fun t ↦ Paralogistic.logLik
          (fun i ↦ exp (tieLogs r i)) t 1) (Ioi 0) k} = {a, b} := by
  change ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧ heldPair (held r)
  obtain ⟨r, hr, hpair⟩ :=
    Paralogistic.heldScale_tie_of_band_bounds (fun _ hr _ ha ↦ held_cover hr ha)
  obtain ⟨r₀, _, hu⟩ := Paralogistic.heldScale_band_crossing
  exact ⟨r, ⟨hr, hpair⟩, fun t ht ↦
    (hu t ⟨ht.1, tied_band_heights ht.2⟩).trans (hu r ⟨hr, tied_band_heights hpair⟩).symm⟩

private theorem held_inverse_iff (r : ℝ) : heldPair (inverseHeld r) ↔ heldPair (held r) := by
  have hrec : Transfer.reciprocal (fun i ↦ exp (-(tieLogs r i))) =
      fun i ↦ exp (tieLogs r i) := by
    ext i
    simp [Transfer.reciprocal, exp_neg]
  have h := reciprocal_held_scale_maximizers (fun i ↦ exp_pos (-(tieLogs r i))) zero_lt_one
  rw [hrec, inv_one] at h
  unfold heldPair held inverseHeld
  simp only [h]

/-- The inverse held-scale construction has the same unique crossing and
the same two maximizing shapes, on the reciprocal sample. -/
theorem inverse_held_scale_global_tie :
    ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧
      ∃ a ∈ Icc (2 : ℝ) 4, ∃ b ∈ Icc (6500 : ℝ) 10000,
        {k ∈ Ioi 0 | IsMaxOn (fun t ↦ InvParalogistic.logLik
          (fun i ↦ exp (-(tieLogs r i))) t 1) (Ioi 0) k} = {a, b} := by
  change ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧ heldPair (inverseHeld r)
  have he := held_scale_global_tie
  change ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧ heldPair (held r) at he
  obtain ⟨r, hr, hu⟩ := he
  exact ⟨r, ⟨hr.1, (held_inverse_iff r).mpr hr.2⟩,
    fun t ht ↦ hu t ⟨ht.1, (held_inverse_iff t).mp ht.2⟩⟩

end
end UnivariateML.TiesParalogistic

end
