module

public import UnivariateML.Family.BurrFree
public import UnivariateML.Family.TiesParalogistic

import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Tactic

set_option linter.style.header false

/-!
# All-free paralogistic ties

The underlying likelihood is the written two-level construction in its
two intercept charts. The written hard-profile bound restricts the
possible maximizing shapes before checking intercept and curvature.
-/

@[expose] public section

namespace UnivariateML.TiesParalogisticJoint

open Real Set
open BurrHeld (softplus avg)
open Paralogistic (sigmoid)

noncomputable section

private abbrev e : ℝ := 1 / 10000
private abbrev w : ℝ := 9999 / 10000
private def A (t : ℝ) : ℝ := 1 + e * t
private def shift (K t : ℝ) : ℝ := 10000 * K * t
private def low (K t : ℝ) := 2 * log t - K * w * t ^ 2
private def high (K t : ℝ) := 2 * log t - K * t
private def objective (K : ℝ) (p : ℝ × ℝ) : ℝ :=
  2 * log p.1 -
    (e * (softplus (-p.2) + e * p.1 * softplus p.2) +
      w * (softplus (-(p.2 + shift K p.1)) + e * p.1 * softplus (p.2 + shift K p.1)))
private def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

private theorem softplus_shift (z : ℝ) : softplus z = z + softplus (-z) := by
  have h : exp z * (1 + exp (-z)) = 1 + exp z := by
    rw [exp_neg, mul_add, mul_one, mul_inv_cancel₀ (exp_pos _).ne', add_comm]
  unfold softplus
  rw [← h, log_mul (exp_pos _).ne' (by positivity), log_exp]

private theorem softplus_pos (z : ℝ) : 0 < softplus z :=
  log_pos (lt_add_of_pos_right _ (exp_pos _))

private theorem softplus_ge (z : ℝ) : z ≤ softplus z := by
  rw [softplus_shift z]
  exact le_add_of_nonneg_right (softplus_pos _).le

private theorem softplus_le_exp (z : ℝ) : softplus z ≤ exp z := by
  unfold softplus
  linarith only [log_le_sub_one_of_pos (show 0 < 1 + exp z by positivity)]

private theorem tiny_exp {z : ℝ} (hz : z ≤ -40) : exp z ≤ 1 / 10 ^ 12 := by
  have h : (10 : ℝ) ^ 12 ≤ exp 40 := by
    rw [show (40 : ℝ) = (40 : ℕ) * 1 by norm_num, exp_nat_mul]
    exact (by norm_num : (10 : ℝ) ^ 12 ≤ 2 ^ 40).trans
      (pow_le_pow_left₀ (by norm_num) exp_one_gt_two.le _)
  calc
    _ ≤ exp (-40) := exp_le_exp.mpr hz
    _ = (exp 40)⁻¹ := exp_neg 40
    _ ≤ 1 / 10 ^ 12 := by rw [one_div]; exact inv_anti₀ (by positivity) h

private theorem small_exp_ten : exp (-10) ≤ 1 / 20000 := by
  have h : (20000 : ℝ) ≤ exp 10 := by
    rw [show (10 : ℝ) = (10 : ℕ) * 1 by norm_num, exp_nat_mul]
    exact (by norm_num : (20000 : ℝ) ≤ (27 / 10) ^ 10).trans
      (pow_le_pow_left₀ (by norm_num) (by linarith only [exp_one_gt_d9]) _)
  rw [exp_neg, one_div]
  exact inv_anti₀ (by norm_num) h

private theorem low_chart (K t v : ℝ) :
    objective K (t, v) =
      low K t + e * ((1 - w * t) * v - A t * softplus v) -
        A t * w * softplus (-(v + shift K t)) := by
  unfold objective low A shift
  simp only
  rw [show softplus (-v) = softplus v - v by linarith only [softplus_shift v],
    softplus_shift (v + 10000 * K * t)]
  unfold e w
  ring

private theorem high_chart (K t v : ℝ) :
    objective K (t, v - shift K t) =
      high K t + v - A t * w * softplus v -
        A t * e * softplus (v - shift K t) := by
  unfold objective high A
  simp only [sub_add_cancel]
  rw [show softplus (-(v - shift K t)) = softplus (v - shift K t) - (v - shift K t) by
      linarith only [softplus_shift (v - shift K t)],
    show softplus (-v) = softplus v - v by linarith only [softplus_shift v]]
  unfold shift e w
  ring

private theorem hard_bound {K t b : ℝ} (ht : 0 < t) :
    objective K (t, b) ≤ max (low K t) (high K t) := by
  let m : ℝ := min e (w * (e * t))
  have he : (0 : ℝ) ≤ e := by norm_num [e]
  have hw : (0 : ℝ) ≤ w := by norm_num [w]
  have hm : 0 ≤ m := le_min he (mul_nonneg hw (mul_nonneg he ht.le))
  have h₁ : m * (-b) ≤ e * softplus (-b) :=
    (mul_le_mul_of_nonneg_left (softplus_ge _) hm).trans
      (mul_le_mul_of_nonneg_right (min_le_left _ _) (softplus_pos _).le)
  have h₂ : m * (b + shift K t) ≤ w * (e * t) * softplus (b + shift K t) :=
    (mul_le_mul_of_nonneg_left (softplus_ge _) hm).trans
      (mul_le_mul_of_nonneg_right (min_le_right _ _) (softplus_pos _).le)
  have h₃ : 0 ≤ e * (e * t * softplus b) := by
    have := softplus_pos b
    unfold e
    positivity
  have h₄ : 0 ≤ w * softplus (-(b + shift K t)) := by
    have := softplus_pos (-(b + shift K t))
    unfold w
    positivity
  have hb : objective K (t,b) ≤ 2 * log t - m * shift K t := by
    unfold objective
    dsimp only
    nlinarith only [h₁, h₂, h₃, h₄]
  rcases le_total e (w * (e * t)) with h | h
  · have hb' : objective K (t, b) ≤ high K t := by
      dsimp [m] at hb
      rw [min_eq_left h] at hb
      unfold high shift e at *
      nlinarith only [hb]
    exact hb'.trans (le_max_right _ _)
  · have hb' : objective K (t, b) ≤ low K t := by
      dsimp [m] at hb
      rw [min_eq_right h] at hb
      have hprod : w * (e * t) * shift K t = K * w * t ^ 2 := by
        unfold shift e
        ring
      rw [hprod] at hb
      exact hb
    exact hb'.trans (le_max_left _ _)

private def lowPeak (K : ℝ) : ℝ := sqrt (1 / (K * w))
private def highPeak (K : ℝ) : ℝ := 2 / K

private theorem lowPeak_spec {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    lowPeak K ∈ Icc (79 / 100 : ℝ) (85 / 100) ∧ K * w * lowPeak K ^ 2 = 1 := by
  have hklo : (139 / 100 : ℝ) ≤ K * w := by have h := hk.1; norm_num [w] at *; linarith
  have hkhi : K * w ≤ 8 / 5 := by have h := hk.2; norm_num [w] at *; linarith
  have hp : 0 < K * w := (by norm_num : (0 : ℝ) < 139 / 100).trans_le hklo
  have hsq : lowPeak K ^ 2 = 1 / (K * w) := sq_sqrt (by positivity)
  have hc₁ : (79 / 100 : ℝ) ^ 2 ≤ 1 / (K * w) := by
    rw [le_div_iff₀ hp]
    linarith only [hkhi]
  have hc₂ : 1 / (K * w) ≤ (85 / 100 : ℝ) ^ 2 := by
    rw [div_le_iff₀ hp]
    linarith only [hklo]
  refine ⟨⟨?_, ?_⟩, ?_⟩
  · unfold lowPeak
    exact Real.le_sqrt (by norm_num) (by positivity) |>.mpr hc₁
  · unfold lowPeak
    exact Real.sqrt_le_iff.mpr ⟨by norm_num, hc₂⟩
  · rw [hsq, mul_one_div_cancel hp.ne']

private theorem highPeak_bounds {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    highPeak K ∈ Icc (5 / 4 : ℝ) (10 / 7) := by
  have hp : 0 < K := (by norm_num : (0 : ℝ) < 7 / 5).trans_le hk.1
  unfold highPeak
  exact ⟨(le_div_iff₀ hp).mpr (by linarith only [hk.2]),
    (div_le_iff₀ hp).mpr (by linarith only [hk.1])⟩

private theorem low_witness {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    low K (lowPeak K) - 1 / 1000 < objective K (lowPeak K, 0) := by
  let t := lowPeak K
  have ht := (lowPeak_spec hk).1
  have hD : 40 ≤ shift K t := by
    have h := mul_le_mul hk.1 ht.1 (by norm_num : (0 : ℝ) ≤ 79 / 100)
      ((by norm_num : (0 : ℝ) ≤ 7 / 5).trans hk.1)
    unfold shift
    linarith only [h]
  have hA : A t ≤ 2 := by unfold A e; linarith only [ht.2]
  have hsp : softplus (-shift K t) ≤ 1 / 10 ^ 12 :=
    (softplus_le_exp _).trans (tiny_exp (neg_le_neg hD))
  have hpen := mul_le_mul hA hsp (softplus_pos _).le (by norm_num : (0 : ℝ) ≤ 2)
  have hsp0 : softplus 0 ≤ 1 := by
    unfold softplus
    rw [exp_zero, show (1 : ℝ) + 1 = 2 by norm_num]
    linarith only [log_two_lt_d9]
  have hpen0 := mul_le_mul hA hsp0 (softplus_pos _).le (by norm_num : (0 : ℝ) ≤ 2)
  change low K t - 1 / 1000 < objective K (t, 0)
  rw [low_chart, mul_zero, zero_add, zero_sub]
  unfold e w
  linarith only [hpen, hpen0]

private theorem high_witness {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    high K (highPeak K) - 1 / 1000 <
      objective K (highPeak K, 10 - shift K (highPeak K)) := by
  let t := highPeak K
  have ht := highPeak_bounds hk
  have hD : 50 ≤ shift K t := by
    have h := mul_le_mul hk.1 ht.1 (by norm_num : (0 : ℝ) ≤ 5 / 4)
      ((by norm_num : (0 : ℝ) ≤ 7 / 5).trans hk.1)
    unfold shift
    linarith only [h]
  have hA : A t ≤ 1 + 3 / 20000 := by unfold A e; linarith only [ht.2]
  have hsp : softplus (10 - shift K t) ≤ 1 / 10 ^ 12 :=
    (softplus_le_exp _).trans (tiny_exp (by linarith only [hD]))
  have hpen := mul_le_mul hA hsp (softplus_pos _).le (by norm_num : (0 : ℝ) ≤ 1 + 3 / 20000)
  have hsp10 : softplus 10 ≤ 10 + 1 / 20000 := by
    rw [softplus_shift 10]
    exact add_le_add_right ((softplus_le_exp _).trans small_exp_ten) _
  have hpen10 := mul_le_mul hA hsp10 (softplus_pos _).le (by norm_num : (0 : ℝ) ≤ 1 + 3 / 20000)
  change high K t - 1 / 1000 < objective K (t, 10 - shift K t)
  rw [high_chart]
  unfold e w
  linarith only [hpen, hpen10]

private theorem low_band_loss {K t : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    (ht : 0 < t) (ht' : t ∉ Icc (3 / 4 : ℝ) (9 / 10)) :
    low K t < low K (lowPeak K) - 1 / 1000 := by
  let u := lowPeak K
  have hs := lowPeak_spec hk
  have hu : 0 < u := (by norm_num : (0 : ℝ) < 79 / 100).trans_le hs.1.1
  have hlog := log_le_sub_one_of_pos (div_pos ht hu)
  rw [log_div ht.ne' hu.ne'] at hlog
  have hku : K * w * u ^ 2 = 1 := hs.2
  have he : t / u = K * w * (t * u) := by
    apply (div_eq_iff hu.ne').mpr
    linear_combination -t * hku
  rw [he] at hlog
  have hdist : (4 / 100 : ℝ) ≤ |t - u| := by
    by_cases h : t < 3 / 4
    · exact (le_abs.mpr (Or.inr (by linarith only [h, hs.1.1])))
    · have hh : 9 / 10 < t := lt_of_not_ge fun hh ↦ ht' ⟨le_of_not_gt h, hh⟩
      exact (le_abs.mpr (Or.inl (by linarith only [hh, hs.1.2])))
  have hsquare := pow_le_pow_left₀ (by norm_num : (0 : ℝ) ≤ 4 / 100) hdist 2
  rw [sq_abs] at hsquare
  have hKw : (139 / 100 : ℝ) ≤ K * w := by have h := hk.1; norm_num [w] at *; linarith
  have hprod := mul_le_mul hKw hsquare (by norm_num : 0 ≤ (4 / 100 : ℝ) ^ 2)
    ((by norm_num : (0 : ℝ) ≤ 139 / 100).trans hKw)
  have hdiff : low K t ≤ low K u - (K * w) * (t - u) ^ 2 := by
    unfold low
    linear_combination 2 * hlog + 2 * hku
  exact hdiff.trans_lt (sub_lt_sub_left
    ((by norm_num : (1 / 1000 : ℝ) < 139 / 100 * (4 / 100) ^ 2).trans_le hprod) _)

private theorem high_band_loss {K t : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    (ht : 0 < t) (ht' : t ∉ Icc (11 / 10 : ℝ) (17 / 10)) :
    high K t < high K (highPeak K) - 1 / 1000 := by
  let u := highPeak K
  have hu := highPeak_bounds hk
  have hu₀ : 0 < u := (by norm_num : (0 : ℝ) < 5 / 4).trans_le hu.1
  have hk₀ : 0 < K := (by norm_num : (0 : ℝ) < 7 / 5).trans_le hk.1
  let z := t / u
  have hz : 0 < z := div_pos ht hu₀
  have he : K * t = 2 * z := by
    dsimp [z, u, highPeak]
    field_simp [hk₀.ne']
  have hku : K * u = 2 := by dsimp [u, highPeak]; field_simp [hk₀.ne']
  have hzlog : log z = log t - log u := log_div ht.ne' hu₀.ne'
  have hbound : 2 * (log z - z + 1) < -1 / 1000 := by
    by_cases hlo : t < 11 / 10
    · have hz' : z ≤ 9 / 10 := by
        apply (div_le_iff₀ hu₀).mpr
        linarith only [hlo, hu.1]
      have hlog : log (9 / 10 : ℝ) ≤ -(39 / 380 : ℝ) := by
        rw [show (9 / 10 : ℝ) = (19 / 20) * (18 / 19) by norm_num,
          log_mul (by norm_num) (by norm_num)]
        linarith only [log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 19 / 20),
          log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 18 / 19)]
      have h := log_le_sub_one_of_pos (div_pos hz (by norm_num : (0 : ℝ) < 9 / 10))
      rw [log_div hz.ne' (by norm_num)] at h
      linarith only [h, hlog, hz']
    · have hhi : 17 / 10 < t := lt_of_not_ge fun h ↦ ht' ⟨le_of_not_gt hlo, h⟩
      have hz' : 11 / 10 ≤ z := by
        apply (le_div_iff₀ hu₀).mpr
        linarith only [hhi, hu.2]
      have hlog : log (11 / 10 : ℝ) ≤ (41 / 420 : ℝ) := by
        rw [show (11 / 10 : ℝ) = (21 / 20) * (22 / 21) by norm_num,
          log_mul (by norm_num) (by norm_num)]
        linarith only [log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 21 / 20),
          log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 22 / 21)]
      have h := log_le_sub_one_of_pos (div_pos hz (by norm_num : (0 : ℝ) < 11 / 10))
      rw [log_div hz.ne' (by norm_num)] at h
      linarith only [h, hlog, hz']
  change high K t < high K u - 1 / 1000
  unfold high
  linarith only [he, hku, hzlog, hbound]

private theorem maximizer_in_bands {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    {p : ℝ × ℝ} (hp : p ∈ domain) (hm : IsMaxOn (objective K) domain p) :
    p.1 ∈ Icc (3 / 4 : ℝ) (9 / 10) ∨ p.1 ∈ Icc (11 / 10 : ℝ) (17 / 10) := by
  by_contra! h
  have ht : 0 < p.1 := hp.1
  have hl : (lowPeak K, 0) ∈ domain := ⟨(by norm_num : (0 : ℝ) < 79 / 100).trans_le
    (lowPeak_spec hk).1.1, mem_univ _⟩
  have hh : (highPeak K, 10 - shift K (highPeak K)) ∈ domain :=
    ⟨(by norm_num : (0 : ℝ) < 5 / 4).trans_le (highPeak_bounds hk).1, mem_univ _⟩
  have hl' := (low_band_loss hk ht h.1).trans (low_witness hk) |>.trans_le (hm hl)
  have hh' := (high_band_loss hk ht h.2).trans (high_witness hk) |>.trans_le (hm hh)
  exact (max_lt hl' hh').not_ge (hard_bound ht)

private def lowBox : Set (ℝ × ℝ) := Icc (3 / 4) (9 / 10) ×ˢ Icc (-3) 0
private def highBox : Set (ℝ × ℝ) := Icc (11 / 10) (17 / 10) ×ˢ Icc 8 12
private def highObjective (K : ℝ) (p : ℝ × ℝ) := objective K (p.1, p.2 - shift K p.1)

private theorem sigmoid_pos (z : ℝ) : 0 < sigmoid z := by unfold sigmoid; positivity

private theorem sigmoid_lt_one (z : ℝ) : sigmoid z < 1 := by
  unfold sigmoid
  exact (div_lt_one (by positivity)).mpr (by linarith)

private theorem sigmoid_le_exp (z : ℝ) : sigmoid z ≤ exp z := by
  unfold sigmoid
  exact div_le_self (exp_pos _).le (le_add_of_nonneg_right (exp_pos _).le)

private theorem sigmoid_neg (z : ℝ) : sigmoid (-z) = (1 + exp z)⁻¹ := by
  unfold sigmoid
  rw [exp_neg]
  field_simp [(exp_pos z).ne', (show 0 < 1 + exp z by positivity).ne',
    (show 0 < 1 + (exp z)⁻¹ by positivity).ne']
  ring

private theorem sigmoid_complement (z : ℝ) : 1 - sigmoid z = sigmoid (-z) := by
  rw [sigmoid_neg]
  unfold sigmoid
  field_simp [(show 0 < 1 + exp z by positivity).ne']
  ring

private theorem sigmoid_mono : Monotone sigmoid := by
  intro u v huv
  unfold sigmoid
  apply (div_le_div_iff₀ (by positivity) (by positivity)).mpr
  linarith only [exp_le_exp.mpr huv]

private theorem sigmoid_zero : sigmoid 0 = 1 / 2 := by norm_num [sigmoid]

private theorem exp_nat_upper (n : ℕ) : exp (n : ℝ) ≤ (68 / 25) ^ n := by
  rw [show (n : ℝ) = n * 1 by ring, exp_nat_mul]
  exact pow_le_pow_left₀ (exp_pos _).le (by linarith only [exp_one_lt_d9]) _

private theorem exp_nat_lower (n : ℕ) : (27 / 10 : ℝ) ^ n ≤ exp (n : ℝ) := by
  rw [show (n : ℝ) = n * 1 by ring, exp_nat_mul]
  exact pow_le_pow_left₀ (by norm_num) (by linarith only [exp_one_gt_d9]) _

private theorem sigmoid_neg_three_bounds :
    1 / 25 ≤ sigmoid (-3) ∧ sigmoid (-3) ≤ 1 / 20 := by
  rw [sigmoid_neg, ← one_div]
  constructor
  · apply (le_div_iff₀ (by positivity)).mpr
    have h := exp_nat_upper 3
    norm_num at h
    linarith only [h]
  · apply (div_le_iff₀ (by positivity)).mpr
    have h := exp_nat_lower 3
    norm_num at h
    linarith only [h]

private theorem sigmoid_neg_eight_bound : 1 / 4000 ≤ sigmoid (-8) := by
  rw [sigmoid_neg, ← one_div]
  apply (le_div_iff₀ (by positivity)).mpr
  have h := exp_nat_upper 8
  norm_num at h
  linarith only [h]

private theorem sigmoid_neg_twelve_bounds :
    1 / 200000 ≤ sigmoid (-12) ∧ sigmoid (-12) ≤ 1 / 125000 := by
  rw [sigmoid_neg, ← one_div]
  constructor
  · apply (le_div_iff₀ (by positivity)).mpr
    have h := exp_nat_upper 12
    norm_num at h
    linarith only [h]
  · apply (div_le_iff₀ (by positivity)).mpr
    have h := exp_nat_lower 12
    norm_num at h
    linarith only [h]

private theorem softplus_deriv (z : ℝ) : HasDerivAt softplus (sigmoid z) z := by
  exact ((hasDerivAt_exp z).const_add 1).log (by positivity)

private def q (z : ℝ) := sigmoid z * (1 - sigmoid z)

private theorem q_pos (z : ℝ) : 0 < q z :=
  mul_pos (sigmoid_pos _) (sub_pos.mpr (sigmoid_lt_one _))

private theorem sigmoid_deriv (z : ℝ) : HasDerivAt sigmoid (q z) z := by
  have h := (hasDerivAt_exp z).div ((hasDerivAt_exp z).const_add 1) (by positivity)
  convert h using 1
  · rfl
  · unfold q sigmoid
    field_simp [(show 0 < 1 + exp z by positivity).ne']

private theorem A_bounds {t : ℝ} (ht₀ : 0 ≤ t) (ht : t ≤ 2) : 1 ≤ A t ∧ A t ≤ 3 / 2 := by
  unfold A e
  constructor <;> linarith only [ht₀, ht]

private theorem shift_large {K t : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    (ht : 3 / 4 ≤ t) : 100 ≤ shift K t := by
  have hp := mul_le_mul hk.1 ht (by norm_num : (0 : ℝ) ≤ 3 / 4)
    ((by norm_num : (0 : ℝ) ≤ 7 / 5).trans hk.1)
  unfold shift
  linarith only [hp]

private theorem low_intercept_deriv (K t v : ℝ) :
    HasDerivAt (fun b ↦ objective K (t, b))
      (e * (1 - w * t - A t * sigmoid v) + A t * w * sigmoid (-(v + shift K t))) v := by
  have h₁ := ((hasDerivAt_id v).const_mul (1 - w * t)).sub
    ((softplus_deriv v).const_mul (A t))
  have hz := ((hasDerivAt_id v).add_const (shift K t)).neg
  have h₂ := (softplus_deriv (-(v + shift K t))).comp v hz
  convert ((h₁.const_mul e).const_add (low K t)).sub (h₂.const_mul (A t * w)) using 1
  · funext b
    exact low_chart K t b
  · ring

private theorem high_intercept_deriv (K t v : ℝ) :
    HasDerivAt (fun b ↦ highObjective K (t, b))
      (1 - A t * w * sigmoid v - A t * e * sigmoid (v - shift K t)) v := by
  have h₁ := ((hasDerivAt_id v).const_add (high K t)).sub
    ((softplus_deriv v).const_mul (A t * w))
  have hz := (hasDerivAt_id v).sub_const (shift K t)
  have h₂ := (softplus_deriv (v - shift K t)).comp v hz
  convert h₁.sub (h₂.const_mul (A t * e)) using 1
  · funext b
    exact high_chart K t b
  · ring

private theorem low_anchor {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    {p : ℝ × ℝ} (hp : p.1 ∈ Icc (3 / 4 : ℝ) (9 / 10))
    (hm : IsMaxOn (objective K) domain p) : p ∈ lowBox := by
  rcases p with ⟨t, v⟩
  have htp : 0 < t := (by norm_num : (0 : ℝ) < 3 / 4).trans_le hp.1
  have ha := A_bounds htp.le (hp.2.trans (by norm_num))
  have hder : deriv (fun b ↦ objective K (t,b)) v = 0 := by
    apply (show IsMaxOn (fun b ↦ objective K (t,b)) univ v from
      fun b _ ↦ hm (show (t,b) ∈ domain from ⟨htp, mem_univ _⟩)).isLocalMax Filter.univ_mem
      |>.deriv_eq_zero
  rw [(low_intercept_deriv K t v).deriv] at hder
  refine ⟨hp, ⟨?_, ?_⟩⟩
  · by_contra! hlt
    have h : sigmoid v ≤ 1 / 20 :=
      (sigmoid_mono hlt.le).trans sigmoid_neg_three_bounds.2
    have h₁ := mul_le_mul ha.2 h (sigmoid_pos _).le (by norm_num : (0 : ℝ) ≤ 3 / 2)
    have h₂ : 0 ≤ A t * w * sigmoid (-(v + shift K t)) := by
      have := sigmoid_pos (-(v + shift K t))
      have := ha.1
      unfold w
      positivity
    have ht := hp.2
    unfold e w at hder h₂
    linarith only [ht, h₁, h₂, hder]
  · by_contra! hlt
    have h : 1 / 2 ≤ sigmoid v := sigmoid_zero ▸ sigmoid_mono hlt.le
    have h₁ := mul_le_mul ha.1 h (by norm_num : (0 : ℝ) ≤ 1 / 2)
      ((by norm_num : (0 : ℝ) ≤ 1).trans ha.1)
    have hz : -(v + shift K t) ≤ -40 := by linarith only [shift_large hk hp.1, hlt]
    have h₂ := mul_le_mul ha.2 ((sigmoid_le_exp _).trans (tiny_exp hz))
      (sigmoid_pos _).le (by norm_num : (0 : ℝ) ≤ 3 / 2)
    have ht := hp.1
    unfold e w at hder
    linarith only [ht, h₁, h₂, hder]

private theorem high_anchor {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    {p : ℝ × ℝ} (hp : p.1 ∈ Icc (11 / 10 : ℝ) (17 / 10))
    (hm : IsMaxOn (objective K) domain p) : (p.1, p.2 + shift K p.1) ∈ highBox := by
  rcases p with ⟨t, b⟩
  let v := b + shift K t
  have ht : 0 < t := (by norm_num : (0 : ℝ) < 11 / 10).trans_le hp.1
  have hv : v - shift K t = b := by dsimp [v]; ring
  have hder : deriv (fun x ↦ highObjective K (t,x)) v = 0 := by
    have hmax : IsMaxOn (fun x ↦ highObjective K (t,x)) univ v := by
      intro x _
      change objective K (t, x - shift K t) ≤ objective K (t, v - shift K t)
      rw [hv]
      exact hm (show (t, x - shift K t) ∈ domain from ⟨ht, mem_univ _⟩)
    exact (hmax.isLocalMax Filter.univ_mem).deriv_eq_zero
  rw [(high_intercept_deriv K t v).deriv] at hder
  have hAw : 1 + 9 / 1000000 ≤ A t * w ∧ A t * w ≤ 1 + 7 / 100000 := by
    have hlo := hp.1
    have hhi := hp.2
    unfold A e w
    constructor <;> linarith only [hlo, hhi]
  refine ⟨hp, ⟨?_, ?_⟩⟩
  · change 8 ≤ v
    by_contra! hlt
    have hσ := (sigmoid_mono (neg_le_neg hlt.le)).trans_eq rfl
    rw [← sigmoid_complement 8, ← sigmoid_complement v] at hσ
    have hs : sigmoid v ≤ 1 - 1 / 4000 := by
      have h := sigmoid_neg_eight_bound
      rw [← sigmoid_complement] at h
      linarith only [h, hσ]
    have h₁ := mul_le_mul hAw.2 hs (sigmoid_pos _).le
      (by norm_num : (0 : ℝ) ≤ 1 + 7 / 100000)
    have hz : v - shift K t ≤ -40 := by
      have h := shift_large hk (hp.1.trans' (by norm_num : (3 / 4 : ℝ) ≤ 11 / 10))
      linarith only [h, hlt]
    have ha := A_bounds ht.le (hp.2.trans (by norm_num))
    have h₂ := mul_le_mul ha.2 ((sigmoid_le_exp _).trans (tiny_exp hz))
      (sigmoid_pos _).le (by norm_num : (0 : ℝ) ≤ 3 / 2)
    unfold e at hder
    nlinarith only [hder, h₁, h₂]
  · change v ≤ 12
    by_contra! hlt
    have hs : 1 - 1 / 125000 ≤ sigmoid v := by
      have h := (sigmoid_mono (neg_le_neg hlt.le)).trans sigmoid_neg_twelve_bounds.2
      rw [← sigmoid_complement] at h
      linarith only [h]
    have h₁ := mul_le_mul hAw.1 hs (by norm_num : (0 : ℝ) ≤ 1 - 1 / 125000)
      ((by norm_num : (0 : ℝ) ≤ 1 + 9 / 1000000).trans hAw.1)
    have h₂ : 0 ≤ A t * e * sigmoid (v - shift K t) := by
      have := (A_bounds ht.le (hp.2.trans (by norm_num))).1
      have := sigmoid_pos (v - shift K t)
      unfold e
      positivity
    have hn : 1 < A t * w * sigmoid v :=
      (by norm_num : (1 : ℝ) < (1 + 9 / 1000000) * (1 - 1 / 125000)).trans_le h₁
    exact (sub_neg_of_lt ((sub_neg_of_lt hn).trans_le h₂)).ne hder


private theorem softplus_component_deriv {T Z : ℝ → ℝ} {t' z' s : ℝ}
    (ht : HasDerivAt T t' s) (hz : HasDerivAt Z z' s) :
    HasDerivAt (fun u ↦ A (T u) * softplus (Z u))
      (e * t' * softplus (Z s) + A (T s) * sigmoid (Z s) * z') s := by
  have ha := (ht.const_mul e).const_add 1
  convert ha.mul ((softplus_deriv (Z s)).comp s hz) using 1
  · funext u
    simp only [Pi.mul_apply, Function.comp_apply, A]
  · simp only [Function.comp_apply, A]
    ring

private theorem softplus_component_second {T Z : ℝ → ℝ} {t' z' s : ℝ}
    (ht : HasDerivAt T t' s) (hz : HasDerivAt Z z' s) :
    HasDerivAt (fun u ↦ e * t' * softplus (Z u) + A (T u) * sigmoid (Z u) * z')
      (2 * e * t' * z' * sigmoid (Z s) + A (T s) * q (Z s) * z' ^ 2) s := by
  have ha := (ht.const_mul e).const_add 1
  have hs := (softplus_deriv (Z s)).comp s hz
  have hp := (sigmoid_deriv (Z s)).comp s hz
  convert (hs.const_mul (e * t')).add ((ha.mul hp).mul_const z') using 1
  · funext u
    simp only [Pi.mul_apply, Pi.add_apply, Function.comp_apply, A]
  · simp only [Function.comp_apply, A]
    ring

private def criterion (C L B J c d : ℝ) (T V Z : ℝ → ℝ) (s : ℝ) : ℝ :=
  2 * log (T s) - C * (T s) ^ 2 + L * T s + B * V s + J * (T s * V s) -
    c * (A (T s) * softplus (V s)) - d * (A (T s) * softplus (Z s))

private def second (C J c d t v z X Y Z : ℝ) : ℝ :=
  -2 * X ^ 2 / t ^ 2 - 2 * C * X ^ 2 + 2 * J * X * Y -
    c * (2 * e * X * Y * sigmoid v + A t * q v * Y ^ 2) -
    d * (2 * e * X * Z * sigmoid z + A t * q z * Z ^ 2)

private theorem criterion_deriv {C L B J c d X Y W : ℝ} {T V Z : ℝ → ℝ}
    (ht : ∀ s, HasDerivAt T X s) (hv : ∀ s, HasDerivAt V Y s)
    (hz : ∀ s, HasDerivAt Z W s) {s : ℝ} (hs : 0 < T s) :
    HasDerivAt (criterion C L B J c d T V Z)
      (2 * X / T s - 2 * C * T s * X + L * X + B * Y + J * (X * V s + T s * Y) -
        c * (e * X * softplus (V s) + A (T s) * sigmoid (V s) * Y) -
        d * (e * X * softplus (Z s) + A (T s) * sigmoid (Z s) * W)) s := by
  have hlog := ((ht s).log hs.ne').const_mul 2
  have ht₂ := ((ht s).pow 2).const_mul C
  have htv := ((ht s).mul (hv s)).const_mul J
  have h₁ := (softplus_component_deriv (ht s) (hv s)).const_mul c
  have h₂ := (softplus_component_deriv (ht s) (hz s)).const_mul d
  convert (((((hlog.sub ht₂).add ((ht s).const_mul L)).add ((hv s).const_mul B)).add
    htv).sub h₁).sub h₂ using 1
  · rfl
  · simp only [Nat.cast_ofNat]
    ring

private theorem criterion_second {C L B J c d X Y W : ℝ} {T V Z : ℝ → ℝ}
    (ht : ∀ s, HasDerivAt T X s) (hv : ∀ s, HasDerivAt V Y s)
    (hz : ∀ s, HasDerivAt Z W s) {s : ℝ} (hs : 0 < T s) :
    deriv^[2] (criterion C L B J c d T V Z) s =
      second C J c d (T s) (V s) (Z s) X Y W := by
  let G (u : ℝ) := 2 * X / T u - 2 * C * T u * X + L * X + B * Y +
    J * (X * V u + T u * Y) -
    c * (e * X * softplus (V u) + A (T u) * sigmoid (V u) * Y) -
    d * (e * X * softplus (Z u) + A (T u) * sigmoid (Z u) * W)
  have he : deriv (criterion C L B J c d T V Z) =ᶠ[nhds s] G := by
    filter_upwards [(ht s).continuousAt.preimage_mem_nhds (Ioi_mem_nhds hs)] with u hu
    exact (criterion_deriv ht hv hz (show 0 < T u from hu)).deriv
  change deriv (deriv (criterion C L B J c d T V Z)) s = _
  rw [he.deriv_eq]
  have hlog := (hasDerivAt_const s (2 * X)).div (ht s) hs.ne'
  have hquad := ((ht s).const_mul (2 * C)).mul_const X
  have hlin := (((hv s).const_mul X).add ((ht s).mul_const Y)).const_mul J
  have h₁ := (softplus_component_second (ht s) (hv s)).const_mul c
  have h₂ := (softplus_component_second (ht s) (hz s)).const_mul d
  have hD : HasDerivAt G (second C J c d (T s) (V s) (Z s) X Y W) s := by
    convert (((((hlog.sub hquad).add_const (L * X)).add_const (B * Y)).add
      hlin).sub h₁).sub h₂ using 1
    unfold second
    ring
  exact hD.deriv

private theorem curvature_negative {C J c d t v z X Y W : ℝ}
    (hC : 0 ≤ C) (hJ : -1 / 10000 ≤ J ∧ J ≤ 0)
    (hc : c ∈ Icc (0 : ℝ) 1) (hd : d ∈ Icc (0 : ℝ) 1)
    (ht : 0 < t) (ht' : t ≤ 2) (hcurv : 1 / 500000 ≤ c * (A t * q v))
    (hz : z ≤ -40) (hW : |W| ≤ |Y| + 16000 * |X|)
    (hXY : X ≠ 0 ∨ Y ≠ 0) :
    second C J c d t v z X Y W < 0 := by
  have hA := (A_bounds ht.le ht').1
  have hsigma : 0 ≤ sigmoid v := (sigmoid_pos _).le
  have hraw : 0 ≤ c * sigmoid v ∧ c * sigmoid v ≤ 1 :=
    ⟨mul_nonneg hc.1 hsigma, (mul_le_mul hc.2 (sigmoid_lt_one _).le hsigma (by norm_num)).trans_eq
      (by ring)⟩
  let U := |X|
  let V := |Y|
  have hU : 0 ≤ U := abs_nonneg _
  have hV : 0 ≤ V := abs_nonneg _
  have hX : U ^ 2 = X ^ 2 := sq_abs _
  have hY : V ^ 2 = Y ^ 2 := sq_abs _
  have hXY' : -(X * Y) ≤ U * V := by
    dsimp [U, V]
    rw [← abs_mul]
    exact neg_le_abs _
  have hmain : 2 * J * X * Y - c * (2 * e * X * Y * sigmoid v) ≤
      (1 / 2500) * (U * V) := by
    let k := -J + e * (c * sigmoid v)
    have hk₀ : 0 ≤ k := by dsimp [k]; unfold e; linarith only [hJ.2, hraw.1]
    have hk₁ : 2 * k ≤ 1 / 2500 := by dsimp [k]; unfold e; linarith only [hJ.1, hraw.2]
    have ha := mul_le_mul_of_nonneg_left hXY' (show 0 ≤ 2 * k by positivity)
    have hb := mul_le_mul_of_nonneg_right hk₁ (show 0 ≤ U * V by positivity)
    dsimp [k] at ha
    linear_combination ha + hb
  have htailP : 0 ≤ d * sigmoid z ∧ d * sigmoid z ≤ 1 / 10 ^ 12 := by
    exact ⟨mul_nonneg hd.1 (sigmoid_pos _).le,
      (mul_le_mul hd.2 ((sigmoid_le_exp _).trans (tiny_exp hz)) (sigmoid_pos _).le
        (by norm_num : (0 : ℝ) ≤ 1)).trans_eq (by ring)⟩
  have htailXY : -(X * W) ≤ U * (V + 16000 * U) := by
    exact ((neg_le_abs _).trans_eq (abs_mul X W)).trans
      (mul_le_mul_of_nonneg_left hW hU)
  have htail : -d * (2 * e * X * W * sigmoid z) ≤
      1 / 10 ^ 12 * (U * (V + 16000 * U)) := by
    have hcoeff : 0 ≤ 2 * e * (d * sigmoid z) :=
      mul_nonneg (by norm_num [e]) htailP.1
    have hcoeff' : 2 * e * (d * sigmoid z) ≤ 1 / 10 ^ 12 := by
      unfold e
      linarith only [htailP.2]
    have h₁ := mul_le_mul_of_nonneg_left htailXY hcoeff
    have h₂ := mul_le_mul_of_nonneg_right hcoeff' (show 0 ≤ U * (V + 16000 * U) by positivity)
    linear_combination h₁ + h₂
  have hlog : X ^ 2 / 2 ≤ 2 * X ^ 2 / t ^ 2 := by
    have ht2 : t ^ 2 ≤ 4 := by
      have h := pow_le_pow_left₀ ht.le ht' 2
      linarith only [h]
    apply (le_div_iff₀ (pow_pos ht 2)).mpr
    have h := mul_le_mul_of_nonneg_left ht2 (show 0 ≤ X ^ 2 / 2 by positivity)
    linarith only [h]
  have hneg₁ : 0 ≤ 2 * C * X ^ 2 := by positivity
  have hneg₂ : 0 ≤ d * (A t * q z * W ^ 2) := by
    have := (q_pos z).le
    have : 0 ≤ A t := (by norm_num : (0 : ℝ) ≤ 1).trans hA
    have : 0 ≤ d := hd.1
    positivity
  have hmcurv := mul_le_mul_of_nonneg_right hcurv (sq_nonneg Y)
  have huv : 0 ≤ U * V := mul_nonneg hU hV
  have hcross : (1 / 2000) * (U * V) ≤ X ^ 2 / 4 + Y ^ 2 / 2000 ^ 2 := by
    have h := sq_nonneg (U / 2 - V / 2000)
    linear_combination h + hX / 4 + hY / 2000 ^ 2
  have hstrict : 0 < X ^ 2 ∨ 0 < Y ^ 2 := hXY.imp (fun h ↦ sq_pos_of_ne_zero h)
    (fun h ↦ sq_pos_of_ne_zero h)
  have hbound : second C J c d t v z X Y W ≤
      -X ^ 2 / 2 + (1 / 2500 + 1 / 10 ^ 12) * (U * V) +
        (16000 / 10 ^ 12) * X ^ 2 - Y ^ 2 / 500000 := by
    unfold second
    linear_combination hlog + hneg₁ + hmain + hmcurv + htail + hneg₂ + (16000 / 10 ^ 12) * hX
  have hbetter : second C J c d t v z X Y W ≤
      -X ^ 2 / 3 + (1 / 2000) * (U * V) - Y ^ 2 / 500000 := by
    exact hbound.trans (by linarith only [huv, sq_nonneg X])
  rcases hstrict with h | h
  · linarith only [hbetter, hcross, sq_nonneg Y, h]
  · linarith only [hbetter, hcross, sq_nonneg X, h]


private def line (p r : ℝ × ℝ) (s : ℝ) : ℝ × ℝ := p + s • (r - p)

private theorem line_first_deriv (p r : ℝ × ℝ) (s : ℝ) :
    HasDerivAt (fun u ↦ (line p r u).1) (r.1 - p.1) s := by
  change HasDerivAt (fun u ↦ p.1 + u * (r.1 - p.1)) (r.1 - p.1) s
  simpa only [id_eq, one_mul] using ((hasDerivAt_id s).mul_const (r.1 - p.1)).const_add p.1

private theorem line_second_deriv (p r : ℝ × ℝ) (s : ℝ) :
    HasDerivAt (fun u ↦ (line p r u).2) (r.2 - p.2) s := by
  change HasDerivAt (fun u ↦ p.2 + u * (r.2 - p.2)) (r.2 - p.2) s
  simpa only [id_eq, one_mul] using ((hasDerivAt_id s).mul_const (r.2 - p.2)).const_add p.2

private theorem line_distinct {p r : ℝ × ℝ} (h : p ≠ r) : r.1 - p.1 ≠ 0 ∨ r.2 - p.2 ≠ 0 := by
  by_contra! h'
  exact h (Prod.ext (sub_eq_zero.mp h'.1).symm (sub_eq_zero.mp h'.2).symm)

private theorem strict_from_line {S : Set (ℝ × ℝ)} {f : ℝ × ℝ → ℝ} (hc : Convex ℝ S)
    (hline : ∀ p ∈ S, ∀ r ∈ S, p ≠ r →
      StrictConcaveOn ℝ (Icc (0 : ℝ) 1) (fun s ↦ f (line p r s))) :
    StrictConcaveOn ℝ S f := by
  refine ⟨hc, ?_⟩
  intro p hp r hr hpr a b ha hb hab
  have h := (hline p hp r hr hpr).2
    (show (0 : ℝ) ∈ Icc 0 1 by norm_num)
    (show (1 : ℝ) ∈ Icc 0 1 by norm_num) (by norm_num) ha hb hab
  have he : line p r b = a • p + b • r := by
    ext <;> simp only [line, Prod.fst_add, Prod.snd_add, Prod.smul_fst, Prod.smul_snd,
      Prod.fst_sub, Prod.snd_sub, smul_eq_mul] <;> first
      | linear_combination -p.1 * hab
      | linear_combination -p.2 * hab
  simp only [smul_eq_mul, mul_zero, mul_one, zero_add] at h
  rw [he] at h
  simpa only [line, zero_smul, one_smul, add_zero, add_sub_cancel, smul_eq_mul] using h

private theorem low_q {v : ℝ} (hv : v ∈ Icc (-3 : ℝ) 0) : 1 / 50 ≤ q v := by
  have h₁ := sigmoid_neg_three_bounds.1.trans (sigmoid_mono hv.1)
  have h₂ : 1 / 2 ≤ 1 - sigmoid v := by
    have h := sigmoid_mono hv.2
    rw [sigmoid_zero] at h
    linarith only [h]
  unfold q
  exact (by norm_num : (1 / 50 : ℝ) = (1 / 25) * (1 / 2)) ▸
    mul_le_mul h₁ h₂ (by norm_num) ((by norm_num : (0 : ℝ) ≤ 1 / 25).trans h₁)

private theorem high_q {v : ℝ} (hv : v ∈ Icc (8 : ℝ) 12) : 1 / 400000 ≤ q v := by
  have h₁ : 1 / 2 ≤ sigmoid v := sigmoid_zero ▸
    sigmoid_mono ((by norm_num : (0 : ℝ) ≤ 8).trans hv.1)
  have h₂ : 1 / 200000 ≤ 1 - sigmoid v := by
    rw [sigmoid_complement]
    exact sigmoid_neg_twelve_bounds.1.trans (sigmoid_mono (neg_le_neg hv.2))
  unfold q
  exact (by norm_num : (1 / 400000 : ℝ) = (1 / 2) * (1 / 200000)) ▸
    mul_le_mul h₁ h₂ (by norm_num) ((by norm_num : (0 : ℝ) ≤ 1 / 2).trans h₁)

private theorem shift_derivative_bound {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    (X Y : ℝ) : |Y + 10000 * K * X| ≤ |Y| + 16000 * |X| ∧
      |Y - 10000 * K * X| ≤ |Y| + 16000 * |X| := by
  have hK₀ : 0 ≤ 10000 * K := by linarith only [hk.1]
  have hK₁ : 10000 * K ≤ 16000 := by linarith only [hk.2]
  have h : |10000 * K * X| ≤ 16000 * |X| := by
    rw [abs_mul, abs_of_nonneg hK₀]
    exact mul_le_mul_of_nonneg_right hK₁ (abs_nonneg _)
  constructor
  · exact (abs_add_le _ _).trans (add_le_add_right h _)
  · rw [sub_eq_add_neg]
    exact (abs_add_le _ _).trans
      (add_le_add_right (show |-(10000 * K * X)| ≤ 16000 * |X| by simpa only [abs_neg] using h) _)

private theorem low_box_strict {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    StrictConcaveOn ℝ lowBox (objective K) := by
  have hconv : Convex ℝ lowBox := (convex_Icc ..).prod (convex_Icc ..)
  apply strict_from_line hconv
  intro p hp r hr hpr
  let T (s : ℝ) := (line p r s).1
  let V (s : ℝ) := (line p r s).2
  let Z (s : ℝ) := -(V s + shift K (T s))
  let X := r.1 - p.1
  let Y := r.2 - p.2
  have hT (s) : HasDerivAt T X s := line_first_deriv p r s
  have hV (s) : HasDerivAt V Y s := line_second_deriv p r s
  have hZ (s) : HasDerivAt Z (-(Y + 10000 * K * X)) s :=
    ((hV s).add ((hT s).const_mul (10000 * K))).neg
  have he : (fun s ↦ objective K (line p r s)) =
      criterion (K * w) 0 e (-e * w) e w T V Z := by
    funext s
    change objective K (T s, V s) = _
    rw [low_chart]
    unfold criterion low Z
    ring
  rw [he]
  have hmem (s : ℝ) (hs : s ∈ Icc (0 : ℝ) 1) : line p r s ∈ lowBox :=
    hconv.add_smul_sub_mem hp hr hs
  have hpos (s) (hs : s ∈ Icc (0 : ℝ) 1) : 0 < T s :=
    (by norm_num : (0 : ℝ) < 3 / 4).trans_le (hmem s hs).1.1
  apply strictConcaveOn_of_deriv2_neg' (convex_Icc ..)
  · exact fun s hs ↦ (criterion_deriv hT hV hZ (hpos s hs)).continuousAt.continuousWithinAt
  · intro s hs
    rw [criterion_second hT hV hZ (hpos s hs)]
    have hs' := hmem s hs
    have ht₂ : T s ≤ 2 := hs'.1.2.trans (by norm_num)
    have hA := (A_bounds (hpos s hs).le ht₂).1
    have hcv := mul_le_mul hA (low_q hs'.2) (by norm_num : (0 : ℝ) ≤ 1 / 50)
      ((by norm_num : (0 : ℝ) ≤ 1).trans hA)
    have hcv' : 1 / 500000 ≤ e * (A (T s) * q (V s)) :=
      (by norm_num [e] : (1 / 500000 : ℝ) ≤ e * (1 * (1 / 50))).trans
        (mul_le_mul_of_nonneg_left hcv (by norm_num [e]))
    have hz : Z s ≤ -40 := by
      have hv : -3 ≤ V s := hs'.2.1
      have hd := shift_large hk (show 3 / 4 ≤ T s from hs'.1.1)
      unfold Z
      linarith only [hv, hd]
    have hW : |-(Y + 10000 * K * X)| ≤ |Y| + 16000 * |X| := by
      rw [abs_neg]
      exact (shift_derivative_bound hk X Y).1
    exact curvature_negative (by have h := hk.1; unfold w; positivity)
      (by norm_num [e, w]) (by constructor <;> norm_num [e])
      (by constructor <;> norm_num [w]) (hpos s hs) ht₂ hcv' hz hW (line_distinct hpr)

private theorem high_box_strict {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    StrictConcaveOn ℝ highBox (highObjective K) := by
  have hconv : Convex ℝ highBox := (convex_Icc ..).prod (convex_Icc ..)
  apply strict_from_line hconv
  intro p hp r hr hpr
  let T (s : ℝ) := (line p r s).1
  let V (s : ℝ) := (line p r s).2
  let Z (s : ℝ) := V s - shift K (T s)
  let X := r.1 - p.1
  let Y := r.2 - p.2
  have hT (s) : HasDerivAt T X s := line_first_deriv p r s
  have hV (s) : HasDerivAt V Y s := line_second_deriv p r s
  have hZ (s) : HasDerivAt Z (Y - 10000 * K * X) s :=
    (hV s).sub ((hT s).const_mul (10000 * K))
  have he : (fun s ↦ highObjective K (line p r s)) =
      criterion 0 (-K) 1 0 w e T V Z := by
    funext s
    change objective K (T s, V s - shift K (T s)) = _
    rw [high_chart]
    unfold criterion high Z
    ring
  rw [he]
  have hmem (s : ℝ) (hs : s ∈ Icc (0 : ℝ) 1) : line p r s ∈ highBox :=
    hconv.add_smul_sub_mem hp hr hs
  have hpos (s) (hs : s ∈ Icc (0 : ℝ) 1) : 0 < T s :=
    (by norm_num : (0 : ℝ) < 11 / 10).trans_le (hmem s hs).1.1
  apply strictConcaveOn_of_deriv2_neg' (convex_Icc ..)
  · exact fun s hs ↦ (criterion_deriv hT hV hZ (hpos s hs)).continuousAt.continuousWithinAt
  · intro s hs
    rw [criterion_second hT hV hZ (hpos s hs)]
    have hs' := hmem s hs
    have ht₂ : T s ≤ 2 := hs'.1.2.trans (by norm_num)
    have hA := (A_bounds (hpos s hs).le ht₂).1
    have hcv := mul_le_mul hA (high_q hs'.2) (by norm_num : (0 : ℝ) ≤ 1 / 400000)
      ((by norm_num : (0 : ℝ) ≤ 1).trans hA)
    have hcv' : 1 / 500000 ≤ w * (A (T s) * q (V s)) :=
      (by norm_num [w] : (1 / 500000 : ℝ) ≤ w * (1 * (1 / 400000))).trans
        (mul_le_mul_of_nonneg_left hcv (by norm_num [w]))
    have hz : Z s ≤ -40 := by
      have hv : V s ≤ 12 := hs'.2.2
      have htt : 3 / 4 ≤ T s := (by norm_num : (3 / 4 : ℝ) ≤ 11 / 10).trans hs'.1.1
      have hd := shift_large hk htt
      unfold Z
      linarith only [hv, hd]
    exact curvature_negative (by norm_num) (by norm_num)
      (by constructor <;> norm_num [w]) (by constructor <;> norm_num [e])
      (hpos s hs) ht₂ hcv' hz (shift_derivative_bound hk X Y).2 (line_distinct hpr)


private theorem low_K_deriv (t v K : ℝ) :
    HasDerivAt (fun k ↦ objective k (t, v) + (9 / 10) * k)
      (9 / 10 - w * t ^ 2 + A t * w * sigmoid (-(v + shift K t)) * (10000 * t)) K := by
  have h₁ := (((hasDerivAt_id K).mul_const (w * t ^ 2)).const_sub (2 * log t)).add_const
    (e * ((1 - w * t) * v - A t * softplus v))
  have hz : HasDerivAt (fun k ↦ -(v + shift k t)) (-(10000 * t)) K := by
    have h := (((hasDerivAt_id K).const_mul 10000).mul_const t).const_add v |>.neg
    convert h using 1
    · funext k
      rfl
    · simp
  have h₂ := (softplus_deriv (-(v + shift K t))).comp K hz
  convert (h₁.sub (h₂.const_mul (A t * w))).add
    ((hasDerivAt_id K).const_mul (9 / 10)) using 1
  · funext k
    simp only [Pi.add_apply, Pi.sub_apply, Function.comp_apply, id_eq, low_chart, low]
    ring
  · ring

private theorem high_K_deriv (t v K : ℝ) :
    HasDerivAt (fun k ↦ highObjective k (t, v) + (9 / 10) * k)
      (9 / 10 - t + A t * e * sigmoid (v - shift K t) * (10000 * t)) K := by
  have h₁ := (((hasDerivAt_id K).mul_const t).const_sub (2 * log t)).add_const
    (v - A t * w * softplus v)
  have hz : HasDerivAt (fun k ↦ v - shift k t) (-(10000 * t)) K := by
    have h := (((hasDerivAt_id K).const_mul 10000).mul_const t).const_sub v
    simpa only [id_eq, mul_one, shift] using h
  have h₂ := (softplus_deriv (v - shift K t)).comp K hz
  convert (h₁.sub (h₂.const_mul (A t * e))).add
    ((hasDerivAt_id K).const_mul (9 / 10)) using 1
  · funext k
    simp only [Pi.add_apply, Pi.sub_apply, Function.comp_apply, id_eq, highObjective, high_chart,
      high]
    ring
  · ring

private theorem low_K_increases {p : ℝ × ℝ} (hp : p ∈ lowBox) :
    StrictMonoOn (fun K ↦ objective K p + (9 / 10) * K) (Icc (7 / 5 : ℝ) (8 / 5)) := by
  rcases p with ⟨t,v⟩
  have ht : 0 < t := (by norm_num : (0 : ℝ) < 3 / 4).trans_le hp.1.1
  have ha := (A_bounds ht.le (hp.1.2.trans (by norm_num))).1
  apply strictMonoOn_of_deriv_pos (convex_Icc ..)
    (fun K _ ↦ (low_K_deriv t v K).continuousAt.continuousWithinAt)
  intro K _
  rw [(low_K_deriv t v K).deriv]
  have h := pow_le_pow_left₀ ht.le hp.1.2 2
  have hh := mul_le_mul_of_nonneg_left h (by norm_num [w] : 0 ≤ w)
  have hz : 0 ≤ A t * w * sigmoid (-(v + shift K t)) * (10000 * t) := by
    have := sigmoid_pos (-(v + shift K t))
    unfold w
    positivity
  unfold w at hh hz ⊢
  linarith only [hh, hz]

private theorem high_K_decreases {p : ℝ × ℝ} (hp : p ∈ highBox) :
    StrictAntiOn (fun K ↦ highObjective K p + (9 / 10) * K) (Icc (7 / 5 : ℝ) (8 / 5)) := by
  rcases p with ⟨t,v⟩
  have ht : 0 < t := (by norm_num : (0 : ℝ) < 11 / 10).trans_le hp.1.1
  have ht₂ : t ≤ 2 := hp.1.2.trans (by norm_num)
  apply strictAntiOn_of_deriv_neg (convex_Icc ..)
    (fun K _ ↦ (high_K_deriv t v K).continuousAt.continuousWithinAt)
  intro K hK
  rw [(high_K_deriv t v K).deriv]
  have hD := shift_large (interior_subset hK)
    ((by norm_num : (3 / 4 : ℝ) ≤ 11 / 10).trans hp.1.1)
  have hv := hp.2.2
  have hz : v - shift K t ≤ -40 := by linarith only [hD, hv]
  have hσ := (sigmoid_le_exp _).trans (tiny_exp hz)
  have hA := (A_bounds ht.le ht₂).2
  have hprod := mul_le_mul hA hσ (sigmoid_pos _).le (by norm_num : (0 : ℝ) ≤ 3 / 2)
  have hbound := mul_le_mul hprod ht₂ ht.le (by positivity : (0 : ℝ) ≤ (3 / 2) * (1 / 10 ^ 12))
  have hfactor : A t * e * sigmoid (v - shift K t) * (10000 * t) =
      (A t * sigmoid (v - shift K t)) * t := by unfold e; ring
  rw [hfactor]
  linarith only [hp.1.1, hbound]

private def win (K : ℝ) (p : ℝ × ℝ) : Prop :=
  p ∈ domain ∧ IsMaxOn (objective K) domain p

private def chartTie (K : ℝ) : Prop :=
  ∃ p q : ℝ × ℝ, win K p ∧ win K q ∧ p.1 < 1 ∧ 1 < q.1

private def anchor (K : ℝ) (p : ℝ × ℝ) : ℝ × ℝ :=
  (p.1, p.2 + shift K p.1)

private theorem anchor_value (K : ℝ) (p : ℝ × ℝ) :
    highObjective K (anchor K p) = objective K p := by
  simp [highObjective, anchor]

private theorem lowBox_subset : lowBox ⊆ domain := by
  intro p hp
  exact ⟨(by norm_num : (0 : ℝ) < 3 / 4).trans_le hp.1.1, mem_univ _⟩

private theorem highBox_feasible {K : ℝ} {p : ℝ × ℝ} (hp : p ∈ highBox) :
    (p.1, p.2 - shift K p.1) ∈ domain :=
  ⟨(by norm_num : (0 : ℝ) < 11 / 10).trans_le hp.1.1, mem_univ _⟩

private theorem win_below {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) {p : ℝ × ℝ}
    (hp : win K p) (hs : p.1 < 1) : p ∈ lowBox := by
  rcases maximizer_in_bands hk hp.1 hp.2 with hl | hh
  · exact low_anchor hk hl hp.2
  · exact False.elim (by linarith only [hh.1, hs])

private theorem win_above {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) {p : ℝ × ℝ}
    (hp : win K p) (hs : 1 < p.1) : anchor K p ∈ highBox := by
  rcases maximizer_in_bands hk hp.1 hp.2 with hl | hh
  · exact False.elim (by linarith only [hl.2, hs])
  · exact high_anchor hk hh hp.2

private theorem exactly_two_chart {K : ℝ} (hk : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    {p q : ℝ × ℝ} (hp : win K p) (hq : win K q) (hps : p.1 < 1) (hqs : 1 < q.1)
    {r : ℝ × ℝ} (hr : win K r) : r = p ∨ r = q := by
  rcases maximizer_in_bands hk hr.1 hr.2 with hl | hh
  · exact Or.inl ((low_box_strict hk).eq_of_isMaxOn
      (fun _ hr' ↦ hr.2 (lowBox_subset hr')) (fun _ hr' ↦ hp.2 (lowBox_subset hr'))
      (low_anchor hk hl hr.2) (win_below hk hp hps))
  · have hmax (s : ℝ × ℝ) (hs : win K s) :
        IsMaxOn (highObjective K) highBox (anchor K s) := by
      intro v hv
      change objective K (v.1, v.2 - shift K v.1) ≤ highObjective K (anchor K s)
      rw [anchor_value]
      exact hs.2 (highBox_feasible hv)
    have he := (high_box_strict hk).eq_of_isMaxOn (hmax r hr) (hmax q hq)
      (high_anchor hk hh hr.2) (win_above hk hq hqs)
    apply Or.inr
    simpa only [anchor, add_sub_cancel_right] using
      congrArg (fun v : ℝ × ℝ ↦ (v.1, v.2 - shift K v.1)) he

private theorem chart_ties_cannot_increase {K₁ K₂ : ℝ}
    (hK₁ : K₁ ∈ Icc (7 / 5 : ℝ) (8 / 5)) (hK₂ : K₂ ∈ Icc (7 / 5 : ℝ) (8 / 5))
    (h₁ : chartTie K₁) (h₂ : chartTie K₂) (hlt : K₁ < K₂) : False := by
  obtain ⟨p, q, hp, hq, hps, _⟩ := h₁
  obtain ⟨r, s, hr, hs, _, hss⟩ := h₂
  have hpq : objective K₁ p = objective K₁ q := le_antisymm (hq.2 hp.1) (hp.2 hq.1)
  have hrs : objective K₂ r = objective K₂ s := le_antisymm (hs.2 hr.1) (hr.2 hs.1)
  have hlo := low_K_increases (win_below hK₁ hp hps) hK₁ hK₂ hlt
  have hhi := high_K_decreases (win_above hK₂ hs hss) hK₁ hK₂ hlt
  change highObjective K₂ (anchor K₂ s) + 9 / 10 * K₂ <
    highObjective K₁ (anchor K₂ s) + 9 / 10 * K₁ at hhi
  rw [anchor_value, ← hrs] at hhi
  have hol : objective K₂ p ≤ objective K₂ r := hr.2 hp.1
  have hoh : highObjective K₁ (anchor K₂ s) ≤ objective K₁ q := by
    exact hq.2 (show ((anchor K₂ s).1, (anchor K₂ s).2 - shift K₁ (anchor K₂ s).1) ∈ domain
      from ⟨hs.1.1, mem_univ _⟩)
  change objective K₁ p + 9 / 10 * K₁ < objective K₂ p + 9 / 10 * K₂ at hlo
  linarith only [hpq, hlo, hhi, hol, hoh]

private theorem chart_unique {K₁ K₂ : ℝ}
    (hK₁ : K₁ ∈ Ioo (7 / 5 : ℝ) (8 / 5)) (hK₂ : K₂ ∈ Ioo (7 / 5 : ℝ) (8 / 5))
    (h₁ : chartTie K₁) (h₂ : chartTie K₂) : K₁ = K₂ := by
  rcases lt_trichotomy K₁ K₂ with h | h | h
  · exact (chart_ties_cannot_increase (Ioo_subset_Icc_self hK₁) (Ioo_subset_Icc_self hK₂)
      h₁ h₂ h).elim
  · exact h
  · exact (chart_ties_cannot_increase (Ioo_subset_Icc_self hK₂) (Ioo_subset_Icc_self hK₁)
      h₂ h₁ h).elim


private def data (K : ℝ) (i : Fin 10000) : ℝ :=
  if i = 0 then 1 else exp (100000000 * K)

private theorem data_pos (K : ℝ) (i : Fin 10000) : 0 < data K i := by
  unfold data
  split <;> positivity

private theorem log_avg_levels (K : ℝ) (g : ℝ → ℝ) :
    avg (fun i ↦ g (log (data K i))) = e * g 0 + w * g (100000000 * K) := by
  unfold avg
  rw [Fin.sum_univ_succ]
  simp [data, e, w]
  ring

private def fromChart (p : ℝ × ℝ) : ℝ × ℝ :=
  (e * p.1, exp (-p.2 / (e * p.1)))

private def toChart (p : ℝ × ℝ) : ℝ × ℝ :=
  (10000 * p.1, -(p.1 * log p.2))

private theorem fromChart_toChart {p : ℝ × ℝ} (hp : p ∈ Weibull.domain) :
    fromChart (toChart p) = p := by
  have hp₁ : 0 < p.1 := hp.1
  have hs : e * (10000 * p.1) = p.1 := by unfold e; ring
  unfold fromChart toChart
  simp only [hs, neg_neg]
  rw [mul_comm p.1, mul_div_cancel_right₀ _ hp₁.ne', exp_log hp.2]

private theorem toChart_fromChart {p : ℝ × ℝ} (hp : p ∈ domain) :
    toChart (fromChart p) = p := by
  have ha : 0 < e * p.1 := mul_pos (by norm_num [e]) hp.1
  have ht : 10000 * (e * p.1) = p.1 := by unfold e; ring
  unfold fromChart toChart
  simp only [ht, log_exp]
  have hb : -(e * p.1 * (-p.2 / (e * p.1))) = p.2 := by
    have hp' : 0 < p.1 := hp.1
    field_simp [hp'.ne']
  rw [hb]

private theorem chart_bij : BijOn fromChart domain Weibull.domain := by
  refine ⟨?_, ?_, ?_⟩
  · intro p hp
    exact ⟨mul_pos (by norm_num [e]) hp.1, exp_pos _⟩
  · intro p hp q hq he
    simpa only [toChart_fromChart hp, toChart_fromChart hq] using congrArg toChart he
  · intro p hp
    exact ⟨toChart p, ⟨mul_pos (by norm_num) hp.1, mem_univ _⟩, fromChart_toChart hp⟩

private theorem chart_likelihood (K : ℝ) {p : ℝ × ℝ} (hp : p ∈ domain) :
    objective K p =
      Paralogistic.logLik (data K) (fromChart p).1 (fromChart p).2 +
        (avg (fun i ↦ log (data K i)) - 2 * log e) := by
  rcases p with ⟨t,b⟩
  have ht : 0 < t := hp.1
  have ha : 0 < e * t := mul_pos (by norm_num [e]) ht
  unfold Paralogistic.logLik fromChart
  simp only
  rw [BurrHeld.logLik_eq_first (by decide) (data_pos K) (exp_pos _), log_exp]
  have hb : -(e * t * (-b / (e * t))) = b := by field_simp [ha.ne']
  rw [hb]
  unfold BurrHeld.firstLik
  simp only
  rw [log_avg_levels K (fun y ↦
    softplus (-(e * t * y + b)) + e * t * softplus (e * t * y + b))]
  rw [log_mul (by norm_num [e]) ht.ne', mul_zero, zero_add]
  have hD : e * t * (100000000 * K) + b = b + shift K t := by unfold e shift; ring
  rw [hD]
  unfold objective
  simp only
  ring

private def physicalWinners (f : (ℝ × ℝ) → ℝ) : Set (ℝ × ℝ) :=
  {p ∈ Weibull.domain | IsMaxOn f Weibull.domain p}

private def physical (K : ℝ) (p : ℝ × ℝ) : ℝ := Paralogistic.logLik (data K) p.1 p.2

private theorem chart_winners_bij (K : ℝ) :
    BijOn fromChart {p | win K p} (physicalWinners (physical K)) :=
  Transfer.bijOn_maximizers chart_bij (fun _ hp ↦ chart_likelihood K hp)

private theorem tie_from_two_physical {K : ℝ} {p q : ℝ × ℝ}
    (hp : p ∈ physicalWinners (physical K)) (hq : q ∈ physicalWinners (physical K))
    (hps : p.1 < e) (hqs : e < q.1) : chartTie K := by
  obtain ⟨p', hp', hpp⟩ := (chart_winners_bij K).surjOn hp
  obtain ⟨q', hq', hqq⟩ := (chart_winners_bij K).surjOn hq
  have h₁ : p'.1 < 1 := by
    have h := congrArg Prod.fst hpp
    change e * p'.1 = p.1 at h
    unfold e at hps h
    linarith only [hps, h]
  have h₂ : 1 < q'.1 := by
    have h := congrArg Prod.fst hqq
    change e * q'.1 = q.1 at h
    unfold e at hqs h
    linarith only [hqs, h]
  exact ⟨p', q', hp', hq', h₁, h₂⟩

private theorem chart_tie_exists :
    ∃ K ∈ Ioo (7 / 5 : ℝ) (8 / 5), chartTie K := by
  obtain ⟨K, hK, p, hp, q, hq, hps, hqs, hpm, hqm⟩ :=
    BurrFree.paralogistic_two_level_tie
  exact ⟨K, hK, tie_from_two_physical ⟨hp, hpm⟩ ⟨hq, hqm⟩ hps hqs⟩

private def separatedPair (f : (ℝ × ℝ) → ℝ) : Prop :=
  ∃ p q : ℝ × ℝ, p.1 < e ∧ e < q.1 ∧ physicalWinners f = {p, q}

private theorem physical_tie_iff {K : ℝ} (hK : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    separatedPair (physical K) ↔ chartTie K := by
  constructor
  · rintro ⟨p, q, hp, hq, he⟩
    have hm₁ : p ∈ physicalWinners (physical K) := by rw [he]; simp
    have hm₂ : q ∈ physicalWinners (physical K) := by rw [he]; simp
    exact tie_from_two_physical hm₁ hm₂ hp hq
  · rintro ⟨p, q, hp, hq, hps, hqs⟩
    have hchart : {v | win K v} = {p, q} := by
      ext r
      simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
      exact ⟨exactly_two_chart hK hp hq hps hqs, fun h ↦ h.elim (· ▸ hp) (· ▸ hq)⟩
    have him := (chart_winners_bij K).image_eq
    rw [hchart, image_pair] at him
    have he : 0 < e := by norm_num [e]
    exact ⟨fromChart p, fromChart q, by simpa [fromChart] using mul_lt_mul_of_pos_left hps he,
      by simpa [fromChart] using mul_lt_mul_of_pos_left hqs he, him.symm⟩

/-- The all-free two-level paralogistic sample has a unique crossing
parameter, with exactly two global maximizers at the crossing. -/
theorem joint_global_tie :
    ∃! K, K ∈ Ioo (7 / 5 : ℝ) (8 / 5) ∧
      let x : Fin 10000 → ℝ := fun i ↦ if i = 0 then 1 else exp (100000000 * K)
      ∃ p q : ℝ × ℝ, p.1 < 1 / 10000 ∧ 1 / 10000 < q.1 ∧
        {v ∈ Weibull.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Paralogistic.logLik x b.1 b.2) Weibull.domain v} = {p, q} := by
  change ∃! K, K ∈ Ioo (7 / 5 : ℝ) (8 / 5) ∧ separatedPair (physical K)
  obtain ⟨K, hK, ht⟩ := chart_tie_exists
  refine ⟨K, ⟨hK, (physical_tie_iff (Ioo_subset_Icc_self hK)).mpr ht⟩, ?_⟩
  intro L hL
  exact chart_unique hL.1 hK ((physical_tie_iff (Ioo_subset_Icc_self hL.1)).mp hL.2) ht


private def inverseData (K : ℝ) (i : Fin 10000) : ℝ :=
  if i = 0 then 1 else exp (-(100000000 * K))

private def inversePhysical (K : ℝ) (p : ℝ × ℝ) : ℝ :=
  InvParalogistic.logLik (inverseData K) p.1 p.2

private def invertScale (p : ℝ × ℝ) : ℝ × ℝ := (p.1, p.2⁻¹)

private theorem inverse_tie_iff (K : ℝ) :
    separatedPair (inversePhysical K) ↔ separatedPair (physical K) := by
  have hx (i : Fin 10000) : 0 < inverseData K i := by unfold inverseData; split <;> positivity
  have hrec : Transfer.reciprocal (inverseData K) = data K := by
    ext i
    by_cases hi : i = 0 <;> simp [Transfer.reciprocal, inverseData, data, hi, exp_neg]
  have hbij : BijOn invertScale (physicalWinners (inversePhysical K))
      (physicalWinners (physical K)) := by
    have h := TiesParalogistic.reciprocal_joint_maximizers hx
    rw [hrec] at h
    exact h
  constructor
  · rintro ⟨p, q, hp, hq, h⟩
    have him := hbij.image_eq
    rw [h, image_pair] at him
    exact ⟨invertScale p, invertScale q, hp, hq, him.symm⟩
  · rintro ⟨p, q, hp, hq, h⟩
    have he : invertScale ∘ invertScale = id := by
      funext p
      simp [invertScale]
    have him := congrArg (fun S : Set (ℝ × ℝ) ↦ invertScale '' S) hbij.image_eq
    change invertScale '' (invertScale '' physicalWinners (inversePhysical K)) =
      invertScale '' physicalWinners (physical K) at him
    rw [image_image, ← Function.comp_def, he, image_id, h, image_pair] at him
    exact ⟨invertScale p, invertScale q, hp, hq, him⟩

/-- The inverse all-free construction has its exact two-way tie at the
same unique crossing after reciprocating the observations and scales. -/
theorem inverse_joint_global_tie :
    ∃! K, K ∈ Ioo (7 / 5 : ℝ) (8 / 5) ∧
      let x : Fin 10000 → ℝ := fun i ↦ if i = 0 then 1 else exp (-(100000000 * K))
      ∃ p q : ℝ × ℝ, p.1 < 1 / 10000 ∧ 1 / 10000 < q.1 ∧
        {v ∈ Weibull.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          InvParalogistic.logLik x b.1 b.2) Weibull.domain v} = {p, q} := by
  change ∃! K, K ∈ Ioo (7 / 5 : ℝ) (8 / 5) ∧ separatedPair (inversePhysical K)
  have ht := joint_global_tie
  change ∃! K, K ∈ Ioo (7 / 5 : ℝ) (8 / 5) ∧ separatedPair (physical K) at ht
  obtain ⟨K, hK, hu⟩ := ht
  exact ⟨K, ⟨hK.1, (inverse_tie_iff K).mpr hK.2⟩,
    fun L hL ↦ hu L ⟨hL.1, (inverse_tie_iff L).mp hL.2⟩⟩


end
end UnivariateML.TiesParalogisticJoint

end
