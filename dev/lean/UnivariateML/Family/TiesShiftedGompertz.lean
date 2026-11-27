module

public import UnivariateML.Family.ShapeFacesShiftedGompertz

import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Tactic

set_option linter.style.header false

/-!
# A shifted Gompertz tie with the exponential member

## Deviations

The crossing uses the larger outlier bracket (8.1, 128) and a uniform
separation from the zero-shape optimum. The theorem proves two distinct
global maximizers. It leaves the number of positive-shape winners, the
narrower bracket, and the uniqueness of the crossing parameter open.
-/

@[expose] public section

namespace UnivariateML.TiesShiftedGompertz

open Set Real
open ShapeFacesShiftedGompertz (logLik survivalSum exponentialRate jointDomain)

noncomputable section

private def sample (m : ℝ) (i : Fin 10) := if i = 0 then m else 1

private theorem sample_sum (m : ℝ) (g : ℝ → ℝ) :
    (∑ i, g (sample m i)) = 9 * g 1 + g m := by
  classical
  simp [sample, Fin.sum_univ_succ]
  ring

private theorem sample_total (m : ℝ) : (∑ i, sample m i) = 9 + m := by
  simpa using sample_sum m id

private theorem sample_nonneg {m : ℝ} (hm : 0 ≤ m) (i : Fin 10) :
    0 ≤ sample m i := by
  unfold sample
  split <;> positivity

private theorem survival_eq (m b : ℝ) :
    survivalSum (sample m) b = 9 * exp (-b) + exp (-b * m) := by
  unfold survivalSum
  rw [show (∑ i, exp (-b * sample m i)) = _ from sample_sum m (fun x ↦ exp (-b * x))]
  simp only [mul_one]

private def face (m : ℝ) := 10 * log (10 / (9 + m)) - 10
private def sep : ℝ := 117 / 200
private def shapeCut : ℝ := exp 110

private theorem exp_rate (m : ℝ) : exponentialRate (sample m) = 10 / (9 + m) := by
  rw [exponentialRate, sample_total]
  norm_num

private theorem face_eq {m : ℝ} (hm : 0 < m) :
    logLik (sample m) (exponentialRate (sample m)) 0 = face m := by
  simp only [logLik, zero_mul, sub_zero, add_zero, log_one, Finset.sum_const_zero,
    Nat.cast_ofNat, exp_rate, sample_total, face]
  rw [div_mul_cancel₀ _ (by positivity : (9 + m) ≠ 0)]

private theorem exponential_best {m b : ℝ} (hm : 0 < m) (hb : 0 < b) :
    logLik (sample m) b 0 ≤ face m := by
  rcases eq_or_ne b (exponentialRate (sample m)) with he | he
  · rw [he, face_eq hm]
  · rw [← face_eq hm]
    exact (ShapeFacesShiftedGompertz.exponential_strict (by decide)
      (by rw [sample_total]; positivity) hb he).le

private theorem rate_of_face_lt_sep {m : ℝ} (hm : 81 / 10 ≤ m) :
    exponentialRate (sample m) < sep := by
  rw [exp_rate, div_lt_iff₀ (by linarith only [hm] : 0 < 9 + m), sep]
  linarith only [hm]

private theorem survival_below_sep (m : ℝ) {b : ℝ} (hbs : b ≤ sep) :
    (10 : ℝ) ≤ 2 * survivalSum (sample m) b := by
  have hlog : sep < log (9 / 5 : ℝ) := by
    rw [log_div (by norm_num) (by norm_num), show (9 : ℝ) = 3 ^ 2 by norm_num, log_pow]
    norm_num [sep]
    linarith only [log_three_gt_d9, log_five_lt_d9]
  have he : (5 / 9 : ℝ) < exp (-sep) := by
    apply (log_lt_log_iff (by norm_num) (exp_pos _)).mp
    rw [log_exp, show (5 / 9 : ℝ) = (9 / 5 : ℝ)⁻¹ by norm_num, log_inv]
    linarith only [hlog]
  rw [survival_eq]
  have he' := he.trans_le (exp_le_exp.mpr (neg_le_neg hbs))
  linarith only [he', exp_pos (-b * m)]

private theorem below_sep_rate {m b η : ℝ} (hm : 0 ≤ m) (hb : 0 < b) (hbs : b ≤ sep)
    (hη : 0 ≤ η) : logLik (sample m) b η ≤ logLik (sample m) b 0 := by
  rcases hη.eq_or_lt with heη | hη
  · rw [← heη]
  · exact (ShapeFacesShiftedGompertz.held_rate_zero_strict (sample_nonneg hm) hb
      (survival_below_sep m hbs) hη (by decide)).le

private theorem upper_log_at {x c : ℝ} (hx : 0 < x) (hc : 0 < c) :
    log x ≤ log c + x / c - 1 := by
  have h := log_le_sub_one_of_pos (div_pos hx hc)
  rw [log_div hx.ne' hc.ne'] at h
  linarith only [h]

private theorem logLik_upper {m b η : ℝ} (hm : 0 ≤ m) (hb : 0 < b) (hη : 0 ≤ η) :
    logLik (sample m) b η ≤
      10 * log b - b * (9 + m) - η * survivalSum (sample m) b + 10 * log (1 + η) := by
  have h (i : Fin 10) :
      log (1 + η * (1 - exp (-b * sample m i))) ≤ log (1 + η) := by
    have hx := sample_nonneg hm i
    have h₀ : -b * sample m i ≤ 0 := mul_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr hb.le) hx
    have huv := exp_le_one_iff.mpr h₀
    have h₁ := mul_nonneg hη (sub_nonneg.mpr huv)
    have h₂ := mul_nonneg hη (exp_pos (-b * sample m i)).le
    exact log_le_log (by linarith only [h₁]) (by linarith only [h₂,
      show η * (1 - exp (-b * sample m i)) = η - η * exp (-b * sample m i) by ring])
  have hh := Finset.sum_le_sum fun i (_ : i ∈ Finset.univ) ↦ h i
  rw [logLik, sample_total]
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
    Nat.cast_ofNat] at hh ⊢
  linarith only [hh]

private theorem rate_upper {m b η : ℝ} (hm : 0 ≤ m) (hb : 0 < b) (hη : 0 ≤ η) :
    logLik (sample m) b η ≤ 10 * log b - b * (m - 1) + 2 := by
  let c := exp b * (10 / 9)
  have hc : 0 < c := by dsimp [c]; positivity
  have h := upper_log_at (show 0 < 1 + η by positivity) hc
  have hcl : log c ≤ b + 1 / 9 := by
    dsimp [c]
    rw [log_mul (exp_pos _).ne' (by norm_num), log_exp]
    have h' := log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 10 / 9)
    linarith only [h']
  have hfrac : 10 * ((1 + η) / c) = 9 * exp (-b) + η * (9 * exp (-b)) := by
    dsimp [c]
    rw [exp_neg]
    field_simp [(exp_pos b).ne']
  have hh := logLik_upper hm hb hη
  have hV : η * (9 * exp (-b)) ≤ η * survivalSum (sample m) b := by
    rw [survival_eq]
    exact mul_le_mul_of_nonneg_left (le_add_of_nonneg_right (exp_pos _).le) hη
  have he := exp_le_one_iff.mpr (neg_nonpos.mpr hb.le)
  linarith only [h, hcl, hfrac, hh, hV, he]

private theorem face_gt_neg40 {m : ℝ} (hm : m ∈ Icc (81 / 10 : ℝ) 128) :
    -40 < face m := by
  have h : (1 / 16 : ℝ) ≤ 10 / (9 + m) := by
    rw [le_div_iff₀ (by linarith only [hm.1] : 0 < 9 + m)]
    linarith only [hm.2]
  have h' := log_le_log (by norm_num : (0 : ℝ) < 1 / 16) h
  rw [show (1 / 16 : ℝ) = ((2 : ℝ) ^ 4)⁻¹ by norm_num, log_inv, log_pow] at h'
  unfold face
  norm_num at h'
  linarith only [h', log_two_lt_d9]

private theorem large_rate_loses {m b η : ℝ} (hm : m ∈ Icc (81 / 10 : ℝ) 128)
    (hb : 100 ≤ b) (hη : 0 ≤ η) : logLik (sample m) b η < face m := by
  have hbp : 0 < b := by linarith only [hb]
  have hm₀ : 0 ≤ m := by linarith only [hm.1]
  have hlog := upper_log_at hbp (by norm_num : (0 : ℝ) < 4)
  rw [log_four_eq] at hlog
  have hM := mul_le_mul_of_nonneg_left
    (show (71 / 10 : ℝ) ≤ m - 1 by linarith only [hm.1]) hbp.le
  exact (show logLik (sample m) b η < -40 by
    linarith only [hlog, hM, rate_upper hm₀ hbp hη, hb, log_two_lt_d9]).trans (face_gt_neg40 hm)

private theorem large_shape_loses {m b η : ℝ} (hm : m ∈ Icc (81 / 10 : ℝ) 128)
    (hb : 0 < b) (hb' : b ≤ 100) (hη : shapeCut ≤ η) :
    logLik (sample m) b η < face m := by
  have hη₀ := (exp_pos 110).le.trans hη
  have hm₀ : 0 ≤ m := by linarith only [hm.1]
  have hV : 9 * exp (-100) ≤ survivalSum (sample m) b := by
    rw [survival_eq]
    have hh := exp_le_exp.mpr (neg_le_neg hb')
    linarith only [hh, exp_pos (-b * m)]
  have hexp : 10 * exp (-110) ≤ exp (-100) := by
    have h := mul_le_mul_of_nonneg_right (show (10 : ℝ) ≤ exp 10 by
      linarith only [add_one_le_exp (10 : ℝ)]) (exp_pos (-110)).le
    rwa [← exp_add, show (10 : ℝ) + -110 = -100 by norm_num] at h
  have hcancel : exp (-100) * exp 110 = exp 10 := by
    rw [← exp_add, show (-100 : ℝ) + 110 = 10 by norm_num]
  have hlog := upper_log_at (show 0 < 1 + η by positivity) (exp_pos 110)
  rw [log_exp, div_eq_mul_inv, ← exp_neg] at hlog
  have hslope : 10 * ((1 + η) * exp (-110)) ≤ exp (-100) + η * exp (-100) := by
    have h := mul_le_mul_of_nonneg_right hexp (show 0 ≤ 1 + η by positivity)
    linear_combination h
  have hηV := mul_le_mul_of_nonneg_left hV hη₀
  have hlb : log b ≤ 5 := by
    have h := log_le_log hb hb'
    rw [show (100 : ℝ) = 10 ^ 2 by norm_num, log_pow, log_ten_eq] at h
    norm_num at h
    linarith only [h, log_two_lt_d9, log_five_lt_d9]
  have hηtail : 150 ≤ η * exp (-100) := by
    have h := mul_le_mul_of_nonneg_right hη (exp_pos (-100)).le
    rw [shapeCut, mul_comm (exp 110), hcancel] at h
    have ht : (150 : ℝ) ≤ exp 10 := by
      rw [show (10 : ℝ) = (10 : ℕ) * 1 by norm_num, exp_nat_mul]
      exact (by norm_num : (150 : ℝ) ≤ 2 ^ 10).trans
        (pow_le_pow_left₀ (by norm_num) exp_one_gt_two.le _)
    exact ht.trans h
  have ht := logLik_upper hm₀ hb hη₀
  have hp : 0 ≤ b * (9 + m) := by positivity
  have he := exp_le_one_iff.mpr (by norm_num : (-100 : ℝ) ≤ 0)
  exact (show logLik (sample m) b η < -40 by
    linarith only [hlog, hslope, hηV, hlb, hηtail, ht, hp, he]).trans (face_gt_neg40 hm)

private theorem scaled_log_bound (k : ℕ) {t q : ℝ} (ht : |t| < 1)
    (hq : q = 2 ^ k * (1 - t)) :
    |log q - ((k : ℝ) * log 2 -
      ∑ i ∈ Finset.range 12, t ^ (i + 1) / (i + 1))| ≤ |t| ^ 13 / (1 - |t|) := by
  rw [hq, log_mul (by positivity) (sub_pos.mpr ((abs_lt.mp ht).2)).ne', log_pow]
  convert abs_log_sub_add_sum_range_le ht 12 using 2
  ring

private theorem log_arg_gt : (489 / 1000 : ℝ) < log (204 / 125) := by
  have h := (abs_le.mp (scaled_log_bound 1 (t := 23/125) (q := 204/125)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem log_ratio_gt : (536 / 1000 : ℝ) < log (171 / 100) := by
  have h := (abs_le.mp (scaled_log_bound 1 (t := 29/200) (q := 171/100)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem left_wins : face (81 / 10) < logLik (sample (81 / 10)) 1 1 := by
  have he : exp (-(81 / 10 : ℝ)) ≤ 1 / 1000 := by
    calc
      _ ≤ exp (-(8 : ℝ)) := exp_le_exp.mpr (by norm_num)
      _ = exp (-1) ^ 8 := by
        rw [← exp_nat_mul]
        congr 1
        norm_num
      _ ≤ (2 / 5 : ℝ) ^ 8 := pow_le_pow_left₀ (exp_pos _).le
          (exp_neg_one_lt_d9.le.trans (by norm_num)) _
      _ ≤ _ := by norm_num
  have hl₁ := log_arg_gt.trans_le (log_le_log (by norm_num : (0 : ℝ) < 204 / 125)
    (show (204 / 125 : ℝ) ≤ 2 - exp (-1) by linarith only [exp_neg_one_lt_d9]))
  have hl₂ : (692 / 1000 : ℝ) < log (2 - exp (-(81 / 10 : ℝ))) := by
    have h := upper_log_at (by norm_num : (0 : ℝ) < 2)
      (by norm_num : (0 : ℝ) < 1999 / 1000)
    have h' := log_le_log (by norm_num : (0 : ℝ) < 1999 / 1000)
      (show (1999 / 1000 : ℝ) ≤ 2 - exp (-(81 / 10 : ℝ)) by linarith only [he])
    linarith only [h, h', log_two_gt_d9]
  unfold face
  rw [show (10 / (9 + 81 / 10) : ℝ) = (171 / 100 : ℝ)⁻¹ by norm_num, log_inv]
  rw [logLik, sample_total, survival_eq]
  rw [show (∑ i, log (1 + 1 * (1 - exp (-1 * sample (81 / 10) i)))) = _ from
    sample_sum (81 / 10) (fun x ↦ log (1 + 1 * (1 - exp (-1 * x))))]
  norm_num
  rw [show 1 + (1 - exp (-1 : ℝ)) = 2 - exp (-1) by ring,
    show 1 + (1 - exp (-(81 / 10 : ℝ))) = 2 - exp (-(81 / 10 : ℝ)) by ring]
  linarith only [log_ratio_gt, hl₁, hl₂, he, exp_neg_one_lt_d9]

private theorem right_loses {b η : ℝ} (hb : sep ≤ b) (hη : 0 ≤ η) :
    logLik (sample 128) b η < face 128 := by
  have hbp : 0 < b := by linarith only [show (0 : ℝ) < sep by norm_num [sep], hb]
  have hlog := log_le_sub_one_of_pos hbp
  have hl := rate_upper (m := 128) (by norm_num) hbp hη
  exact (show logLik (sample 128) b η < -40 by
    unfold sep at hb
    linarith only [hlog, hl, hb]).trans (face_gt_neg40 (by constructor <;> norm_num))

private def box : Set (ℝ × ℝ) := Icc sep 100 ×ˢ Icc 0 shapeCut

private theorem witness_mem_box : (1, 1) ∈ box := by
  refine ⟨⟨by norm_num [sep], by norm_num⟩, ⟨by norm_num, ?_⟩⟩
  exact one_le_exp_iff.mpr (by norm_num)

private def gap (m : ℝ) (p : ℝ × ℝ) : ℝ :=
  let m' := max (81 / 10) m
  logLik (sample m') (max sep p.1) (max 0 p.2) - face m'

private theorem gap_eq {m : ℝ} {p : ℝ × ℝ} (hm : 81 / 10 ≤ m) (hp : p ∈ box) :
    gap m p = logLik (sample m) p.1 p.2 - face m := by
  unfold gap
  rw [max_eq_right hm, max_eq_right hp.1.1, max_eq_right hp.2.1]

private theorem continuous_gap : Continuous (Function.uncurry gap) := by
  let M (p : ℝ × (ℝ × ℝ)) := max (81 / 10) p.1
  let B (p : ℝ × (ℝ × ℝ)) := max sep p.2.1
  let E (p : ℝ × (ℝ × ℝ)) := max 0 p.2.2
  have hM : Continuous M := continuous_const.max continuous_fst
  have hB : Continuous B := continuous_const.max (continuous_fst.comp continuous_snd)
  have hE : Continuous E := continuous_const.max (continuous_snd.comp continuous_snd)
  have hMp (p) : 0 ≤ M p :=
    (by norm_num : (0 : ℝ) ≤ 81 / 10).trans (le_max_left _ _)
  have hBp (p) : 0 < B p :=
    (by norm_num [sep] : 0 < sep).trans_le (le_max_left _ _)
  have hEp (p) : 0 ≤ E p := le_max_left _ _
  have hS (i : Fin 10) : Continuous (fun p ↦ sample (M p) i) := by
    by_cases hi : i = 0
    · simpa only [sample, ite_eq_left hi] using hM
    · simpa only [sample, ite_eq_right hi] using
        (continuous_const : Continuous (fun _ : ℝ × (ℝ × ℝ) ↦ (1 : ℝ)))
  have hsurv (i : Fin 10) : Continuous (fun p ↦ exp (-B p * sample (M p) i)) := by
    fun_prop
  have hterm (i : Fin 10) : Continuous (fun p ↦
      log (1 + E p * (1 - exp (-B p * sample (M p) i)))) := by
    have h₁ : Continuous (fun p ↦ 1 + E p * (1 - exp (-B p * sample (M p) i))) := by
      fun_prop
    refine h₁.log fun p ↦ (show 0 < 1 + E p * (1 - exp (-B p * sample (M p) i)) from ?_).ne'
    have hs := exp_le_one_iff.mpr (mul_nonpos_of_nonpos_of_nonneg
      (neg_nonpos.mpr (hBp p).le) (sample_nonneg (hMp p) i))
    linarith only [mul_nonneg (hEp p) (sub_nonneg.mpr hs)]
  have hbl : Continuous (fun p ↦ log (B p)) := hB.log (fun p ↦ (hBp p).ne')
  have hratio : Continuous (fun p ↦ 10 / (9 + M p)) :=
    continuous_const.div (continuous_const.add hM) (fun p ↦ (by have := hMp p; positivity))
  have hm' : Continuous (fun p ↦ face (M p)) := by
    have h := hratio.log fun p ↦ (show 0 < 10 / (9 + M p) by have := hMp p; positivity).ne'
    unfold face
    fun_prop
  change Continuous (fun p ↦ logLik (sample (M p)) (B p) (E p) - face (M p))
  unfold logLik survivalSum
  exact ((((hbl.const_mul _).sub (hB.mul (continuous_finsetSum _ fun i _ ↦ hS i))).sub
    (hE.mul (continuous_finsetSum _ fun i _ ↦ hsurv i))).add
    (continuous_finsetSum _ fun i _ ↦ hterm i)).sub hm'

set_option maxHeartbeats 1000000 in
-- The compact crossing elaborates the sample-specific likelihood at both winners.
/-- Nine ones and one real outlier admit an exact global tie between the
exponential fit and a strictly positive shape at a separated rate. -/
theorem exponential_positive_shape_tie :
    ∃ m ∈ Ioo (81 / 10 : ℝ) 128,
      let x : Fin 10 → ℝ := fun i ↦ if i = 0 then m else 1
      let f := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
      let p₀ := (exponentialRate x, 0)
      p₀ ∈ jointDomain ∧ IsMaxOn f jointDomain p₀ ∧
        ∃ b > (117 / 200 : ℝ), ∃ η > (0 : ℝ),
          (b, η) ∈ jointDomain ∧ IsMaxOn f jointDomain (b, η) := by
  let H (m : ℝ) := sSup (gap m '' box)
  have hc : IsCompact box := isCompact_Icc.prod isCompact_Icc
  have hH : Continuous H := hc.continuous_sSup continuous_gap
  have hcont (m : ℝ) : Continuous (gap m) := continuous_gap.comp
    ((continuous_const : Continuous (fun _ : ℝ × ℝ ↦ m)).prodMk continuous_id)
  have hmax (m : ℝ) := hc.exists_sSup_image_eq_and_ge (show box.Nonempty from ⟨_, witness_mem_box⟩)
    (hcont m).continuousOn
  have hleft : 0 < H (81 / 10) := by
    obtain ⟨p, _, hp, hm⟩ := hmax (81 / 10)
    change H (81 / 10) = gap (81 / 10) p at hp
    rw [hp]
    apply lt_of_lt_of_le _ (hm _ witness_mem_box)
    rw [gap_eq le_rfl witness_mem_box]
    exact sub_pos.mpr left_wins
  have hright : H 128 < 0 := by
    obtain ⟨p, hp, hval, _⟩ := hmax 128
    change H 128 = gap 128 p at hval
    rw [hval, gap_eq (by norm_num) hp]
    exact sub_neg.mpr (right_loses hp.1.1 hp.2.1)
  obtain ⟨m, hm, hz⟩ := intermediate_value_Icc (by norm_num : (81 / 10 : ℝ) ≤ 128)
    hH.neg.continuousOn (show 0 ∈ Icc (-H (81 / 10)) (-H 128) from
      ⟨(neg_nonpos.mpr hleft.le), (neg_nonneg.mpr hright.le)⟩)
  have he : H m = 0 := neg_eq_zero.mp hz
  have hmi : m ∈ Ioo (81 / 10 : ℝ) 128 :=
    ⟨hm.1.lt_of_ne (fun h ↦ hleft.ne' (h ▸ he)), hm.2.lt_of_ne (fun h ↦ hright.ne (h ▸ he))⟩
  have hm₀ : 0 < m := by linarith only [hm.1]
  obtain ⟨p, hp, hpval, hpmax⟩ := hmax m
  change H m = gap m p at hpval
  rw [he, gap_eq hm.1 hp] at hpval
  have hpface : logLik (sample m) p.1 p.2 = face m := by linarith only [hpval]
  have hb : 0 < p.1 := (by norm_num [sep] : 0 < sep).trans_le hp.1.1
  have hη : 0 ≤ p.2 := hp.2.1
  have hb₀ : 0 < exponentialRate (sample m) := by rw [exp_rate]; positivity
  have hbst : sep < p.1 := by
    by_contra! h'
    have hn : p.1 ≠ exponentialRate (sample m) :=
      ((rate_of_face_lt_sep hm.1).trans_le hp.1.1).ne'
    have hlt := ShapeFacesShiftedGompertz.exponential_strict (by decide)
      (by rw [sample_total]; positivity) hb hn
    rw [face_eq hm₀] at hlt
    exact ((below_sep_rate hm₀.le hb h' hη).trans_lt hlt).ne hpface
  have hηp : 0 < p.2 := hη.lt_of_ne fun h' ↦ by
    have hn : p.1 ≠ exponentialRate (sample m) :=
      ((rate_of_face_lt_sep hm.1).trans hbst).ne'
    have hlt := ShapeFacesShiftedGompertz.exponential_strict (by decide)
      (by rw [sample_total]; positivity) hb hn
    rw [face_eq hm₀, h'] at hlt
    exact hlt.ne hpface
  have hall (q : ℝ × ℝ) (hq : q ∈ jointDomain) :
      logLik (sample m) q.1 q.2 ≤ face m := by
    by_cases hq₁ : q.1 ≤ sep
    · exact (below_sep_rate hm₀.le hq.1 hq₁ hq.2).trans (exponential_best hm₀ hq.1)
    by_cases hq₂ : 100 ≤ q.1
    · exact (large_rate_loses hm hq₂ hq.2).le
    by_cases hq₃ : shapeCut ≤ q.2
    · exact (large_shape_loses hm hq.1 (le_of_not_ge hq₂) hq₃).le
    have hbox : q ∈ box := ⟨⟨le_of_not_ge hq₁, le_of_not_ge hq₂⟩, ⟨hq.2, le_of_not_ge hq₃⟩⟩
    have h := hpmax q hbox
    change gap m q ≤ gap m p at h
    rw [gap_eq hm.1 hbox, gap_eq hm.1 hp, hpface, sub_self] at h
    exact sub_nonpos.mp h
  refine ⟨m, hmi, ⟨hb₀, le_rfl⟩, ?_, p.1, hbst, p.2, hηp, ⟨hb, hη⟩, ?_⟩
  · intro q hq
    exact (hall q hq).trans_eq (face_eq hm₀).symm
  · intro q hq
    exact (hall q hq).trans_eq hpface.symm

end
end UnivariateML.TiesShiftedGompertz

end
