module

public import UnivariateML.Family.LomaxTwoPoint
public import UnivariateML.Family.Transfers

import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Algebra.Polynomial.RuleOfSigns
import Mathlib.Analysis.Calculus.LocalExtr.Rolle
import Mathlib.Data.List.GetD
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Separated Lomax maxima

The compact crossing constructs the two separated winners. Descartes
and Rolle exclude any additional winner. A fixed-rate sensitivity comparison
proves uniqueness of the sample parameter.
-/

@[expose] public section

namespace UnivariateML.TiesLomax

open Set Real
noncomputable section
open Lomax (avg shape bestExp)
open LomaxTwoPoint (Ar F)

private theorem compact_crossing {f : ℝ → ℝ → ℝ}
    {a b l s u w : ℝ} (hab : a ≤ b) (hls : l ≤ s) (hsu : s ≤ u)
    (hw : w ∈ Icc l s)
    (hf : Continuous (Function.uncurry f))
    (ha : ∃ t ∈ Icc s u, ∀ r ∈ Icc l s, f a r < f a t)
    (hb : ∀ r ∈ Icc s u, f b r < f b w)
    (hsep : ∀ t ∈ Icc a b, f t s < f t w) :
    ∃ t ∈ Ioo a b, ∃ p ∈ Ico l s, ∃ q ∈ Ioc s u,
      IsMaxOn (f t) (Icc l u) p ∧ IsMaxOn (f t) (Icc l u) q := by
  let L (t : ℝ) := sSup (f t '' Icc l s)
  let R (t : ℝ) := sSup (f t '' Icc s u)
  have hL : Continuous L := isCompact_Icc.continuous_sSup hf
  have hR : Continuous R := isCompact_Icc.continuous_sSup hf
  have hc (t : ℝ) : Continuous (f t) := hf.comp (continuous_const.prodMk continuous_id)
  have hlmax (t : ℝ) := isCompact_Icc.exists_sSup_image_eq_and_ge
    (nonempty_Icc.mpr hls) (hc t).continuousOn
  have hrmax (t : ℝ) := isCompact_Icc.exists_sSup_image_eq_and_ge
    (nonempty_Icc.mpr hsu) (hc t).continuousOn
  have h₁ : L a < R a := by
    obtain ⟨p, hp, hpval, _⟩ := hlmax a
    obtain ⟨q, _, hqval, hqmax⟩ := hrmax a
    obtain ⟨r, hr, h⟩ := ha
    exact hpval.trans_lt ((h p hp).trans_le ((hqmax r hr).trans_eq hqval.symm))
  have h₂ : R b < L b := by
    obtain ⟨p, _, hpval, hpmax⟩ := hlmax b
    obtain ⟨q, hq, hqval, _⟩ := hrmax b
    exact hqval.trans_lt ((hb q hq).trans_le ((hpmax w hw).trans_eq hpval.symm))
  obtain ⟨t, ht, hz⟩ := intermediate_value_Icc hab (hL.sub hR).continuousOn
    (show 0 ∈ Icc (L a - R a) (L b - R b) from
      ⟨(sub_neg.mpr h₁).le, (sub_pos.mpr h₂).le⟩)
  have he : L t = R t := sub_eq_zero.mp hz
  have hti : t ∈ Ioo a b :=
    ⟨ht.1.lt_of_ne (fun h ↦ h₁.ne (h ▸ he)),
      ht.2.lt_of_ne (fun h ↦ h₂.ne (h ▸ he.symm))⟩
  obtain ⟨p, hp, hpval, hpmax⟩ := hlmax t
  obtain ⟨q, hq, hqval, hqmax⟩ := hrmax t
  have hpq : f t p = f t q := hpval.symm.trans (he.trans hqval)
  have hwle := hpmax w hw
  have hps : p < s := hp.2.lt_of_ne fun h ↦ (hsep t ht).not_ge (h ▸ hwle)
  have hqs : s < q := hq.1.lt_of_ne fun h ↦
    (hsep t ht).not_ge (h ▸ (hwle.trans_eq hpq))
  have hg : IsMaxOn (f t) (Icc l u) p := by
    intro r hr
    rcases le_total r s with hrs | hsr
    · exact hpmax r ⟨hr.1, hrs⟩
    · exact (hqmax r ⟨hsr, hr.2⟩).trans_eq hpq.symm
  exact ⟨t, hti, p, ⟨hp.1, hps⟩, q, ⟨hqs, hq.2⟩, hg,
    fun _ hr ↦ (hg hr).trans_eq hpq⟩

private theorem profile_gap {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {r : ℝ} (hr : 0 < r) :
    Lomax.logLik x (shape x r) r - bestExp x = n * F x r := by
  have hm : 0 < avg x := div_pos (Finset.sum_pos (fun i _ ↦ hx i)
    (Finset.univ_nonempty_iff.mpr ⟨⟨0, hn⟩⟩)) (Nat.cast_pos.mpr hn)
  have he : Lomax.A x (r * avg x) = Ar x r := by
    unfold Lomax.A Ar
    congr 1
    ext i
    unfold Lomax.y
    congr 2
    field_simp
  simpa only [Lomax.gap, he, F] using Lomax.profile_gap hn hx hr

private theorem lift_profile {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p)
    (hm : IsMaxOn (F x) (Ioi 0) p) :
    (shape x p, p) ∈ Lomax.domain ∧
      IsMaxOn (fun b : ℝ × ℝ ↦ Lomax.logLik x b.1 b.2) Lomax.domain (shape x p, p) := by
  refine ⟨⟨(Lomax.shape_profile hn hx hp).1, hp⟩, ?_⟩
  rintro ⟨k, r⟩ ⟨hk, hr⟩
  change 0 < r at hr
  change 0 < k at hk
  have h₁ := profile_gap hn hx hp
  have h₂ := profile_gap hn hx hr
  have h₃ := mul_le_mul_of_nonneg_left (hm hr) (Nat.cast_nonneg n)
  change Lomax.logLik x k r ≤ Lomax.logLik x (shape x p) p
  have h₄ : Lomax.logLik x k r ≤ Lomax.logLik x (shape x r) r := by
    rcases eq_or_ne k (shape x r) with he | he
    · rw [he]
    · exact ((Lomax.shape_profile hn hx hr).2 k hk he).le
  linarith only [h₁, h₂, h₃, h₄]

private def sample (a : ℝ) (i : Fin 21) : ℝ :=
  if i.val = 0 then a else if i.val ≤ 10 then 1 else 100

private theorem sample_positive {a : ℝ} (ha : 0 < a) (i : Fin 21) :
    0 < sample a i := by
  unfold sample
  split <;> first | exact ha | split <;> norm_num

private theorem sample_sum (a : ℝ) (g : ℝ → ℝ) :
    (∑ i, g (sample a i)) = g a + 10 * g 1 + 10 * g 100 := by
  simp [Fin.sum_univ_succ, sample]
  ring

private def mean (a : ℝ) : ℝ := (1010 + a) / 21
private def A (a r : ℝ) : ℝ :=
  (log (1 + r * a) + 10 * log (1 + r) + 10 * log (1 + 100 * r)) / 21
private def gap (a r : ℝ) : ℝ := log (r * mean a) - log (A a r) - A a r

private theorem gap_eq (a r : ℝ) : gap a r = F (sample a) r := by
  have hm : avg (sample a) = mean a := by
    unfold avg mean
    rw [show (∑ i, sample a i) = _ from sample_sum a id]
    norm_num
    ring
  have hA : Ar (sample a) r = A a r := by
    unfold Ar avg A
    rw [show (∑ i, log (1 + r * sample a i)) = _ from
      sample_sum a (fun t ↦ log (1 + r * t))]
    simp only [Nat.cast_ofNat, mul_one]
    ring_nf
  rw [F, hm, hA, gap]

private theorem A_pos {a r : ℝ} (ha : 0 < a) (hr : 0 < r) : 0 < A a r := by
  have hp : 0 < log (1 + r * a) := log_pos (by linarith [mul_pos hr ha])
  have hq : 0 < log (1 + r) := log_pos (by linarith)
  have hs : 0 < log (1 + 100 * r) := log_pos (by linarith)
  unfold A
  positivity

private theorem scaled_log_bound (k : ℕ) {t q : ℝ} (ht : |t| < 1)
    (hq : q = 2 ^ k * (1 - t)) :
    |log q - ((k : ℝ) * log 2 -
      ∑ i ∈ Finset.range 12, t ^ (i + 1) / (i + 1))| ≤ |t| ^ 13 / (1 - |t|) := by
  rw [hq, log_mul (by positivity) (sub_pos.mpr ((abs_lt.mp ht).2)).ne', log_pow]
  convert abs_log_sub_add_sum_range_le ht 12 using 2
  ring

private theorem left_witness {a : ℝ} (ha : 0 < a) (ha₁ : a ≤ 1) :
    1 / 10 < gap a (1 / 10) := by
  have h11 : log 11 < (12 / 5 : ℝ) := by
    have h := (abs_le.mp (scaled_log_bound 4 (t := 5/16) (q := 11)
      (by norm_num) (by norm_num))).2
    norm_num [Finset.sum_range_succ] at h
    linarith only [h, log_two_lt_d9]
  have hmean : (3 / 2 : ℝ) < log (101 / 21) := by
    have h := (abs_le.mp (scaled_log_bound 3 (t := 67/168) (q := 101/21)
      (by norm_num) (by norm_num))).1
    norm_num [Finset.sum_range_succ] at h
    linarith only [h, log_two_gt_d9]
  have hA : A a (1 / 10) ≤ 6 / 5 := by
    have h₁ : log (1 + (1 / 10) * a) ≤ (1 / 10 : ℝ) := by
      have h := log_le_sub_one_of_pos (show 0 < 1 + (1 / 10) * a by positivity)
      linarith
    have h₂ : log (1 + (1 / 10) : ℝ) ≤ 1 / 10 := by
      have h := log_le_sub_one_of_pos (show (0 : ℝ) < 1 + 1 / 10 by norm_num)
      linarith
    unfold A
    norm_num at *
    linarith only [h₁, h₂, h11]
  have hlA := log_le_sub_one_of_pos (A_pos ha (by norm_num : (0 : ℝ) < 1 / 10))
  have hlm : log (101 / 21 : ℝ) ≤ log ((1 / 10) * mean a) := by
    apply log_le_log (by norm_num)
    unfold mean
    linarith
  unfold gap
  linarith only [hmean, hA, hlA, hlm]


private theorem log_mean_upper : log (1011 / 21) < (39 / 10 : ℝ) := by
  have h := (abs_le.mp (scaled_log_bound 6 (t := 333/1344) (q := 1011/21)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private theorem log_ten_lower : (23 / 10 : ℝ) < log 10 := by
  have h := (abs_le.mp (scaled_log_bound 4 (t := 3/8) (q := 10)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem log_low_lower : (21 / 10 : ℝ) < log (87 / 10) := by
  have h := (abs_le.mp (scaled_log_bound 4 (t := 73/160) (q := 87/10)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem log_high_lower : (31 / 10 : ℝ) < log 23 := by
  have h := (abs_le.mp (scaled_log_bound 5 (t := 9/32) (q := 23)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private noncomputable def base (z : ℝ) : ℝ := (20 / 21) * (z + log 10)
private noncomputable def envelope (z : ℝ) : ℝ := z + 39 / 10 - log (base z) - base z

private theorem base_le_A {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    base (log r) ≤ A a r := by
  have h₁ := log_le_log hr (show r ≤ 1 + r by linarith)
  have h₂ := log_le_log (show 0 < 100 * r by positivity)
    (show 100 * r ≤ 1 + 100 * r by linarith)
  rw [log_mul (by norm_num) hr.ne', show (100 : ℝ) = 10 ^ 2 by norm_num,
    log_pow] at h₂
  have h₃ := (log_pos (show 1 < 1 + r * a by linarith [mul_pos hr ha])).le
  unfold base A
  norm_num at h₂
  linarith only [h₁, h₂, h₃]

private theorem gap_le_envelope {a r : ℝ} (ha : 0 < a) (ha₁ : a ≤ 1)
    (hr : 0 < r) (hb : 0 < base (log r)) : gap a r ≤ envelope (log r) := by
  have hm : 0 < mean a := by unfold mean; positivity
  have hl := (log_le_log hm (show mean a ≤ 1011 / 21 by unfold mean; linarith)).trans
    log_mean_upper.le
  have hA := base_le_A ha hr
  have hlA := log_le_log hb hA
  unfold gap envelope
  rw [log_mul hr.ne' hm.ne']
  linarith only [hl, hA, hlA]

private theorem envelope_left : envelope (10 * log 2) < 0 := by
  have hA : (35 / 4 : ℝ) ≤ base (10 * log 2) := by
    unfold base
    linarith only [log_two_gt_d9, log_ten_lower]
  have hl := log_low_lower.trans_le (log_le_log (by norm_num)
    (show (87 / 10 : ℝ) ≤ base (10 * log 2) by linarith))
  unfold envelope
  linarith only [hA, hl, log_two_lt_d9]

private theorem envelope_right : envelope (32 * log 2) < 0 := by
  have hA : (23 : ℝ) ≤ base (32 * log 2) := by
    unfold base
    linarith only [log_two_gt_d9, log_ten_lower]
  have hl := log_high_lower.trans_le (log_le_log (by norm_num) hA)
  unfold envelope
  linarith only [hl, log_two_lt_d9, log_two_gt_d9, log_ten_lower, show
    base (32 * log 2) = (20 / 21) * (32 * log 2 + log 10) by rfl]

private theorem envelope_neg {z : ℝ} (hz : z ∈ Icc (10 * log 2) (32 * log 2)) :
    envelope z < 0 := by
  have hconc : ConcaveOn ℝ (Icc (10 * log 2) (32 * log 2))
      (fun z ↦ log (base z)) := by
    refine ⟨convex_Icc .., ?_⟩
    intro x hx y hy p q hp hq hpq
    have hpos (t : ℝ) (ht : t ∈ Icc (10 * log 2) (32 * log 2)) : base t ∈ Ioi 0 := by
      change 0 < base t
      unfold base
      linarith only [ht.1, log_two_gt_d9, log_ten_lower]
    have h := strictConcaveOn_log_Ioi.concaveOn.2 (hpos x hx) (hpos y hy) hp hq hpq
    simp only [smul_eq_mul] at h ⊢
    convert h using 2
    unfold base
    linear_combination -(20 / 21) * log 10 * hpq
  have hconv : ConvexOn ℝ (Icc (10 * log 2) (32 * log 2)) envelope := by
    refine ⟨convex_Icc .., ?_⟩
    intro x hx y hy p q hp hq hpq
    have h := hconc.2 hx hy hp hq hpq
    simp only [smul_eq_mul] at h ⊢
    unfold envelope base at *
    linear_combination h - (39 / 10 - 20 / 21 * log 10) * hpq
  have hends := hz.1.trans hz.2
  exact (hconv.le_max_of_mem_Icc (left_mem_Icc.mpr hends) (right_mem_Icc.mpr hends) hz).trans_lt
    (max_lt envelope_left envelope_right)

private theorem log_ten_upper : log 10 < (231 / 100 : ℝ) := by
  have h := (abs_le.mp (scaled_log_bound 4 (t := 3/8) (q := 10)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private theorem log_mean_lower : (19 / 5 : ℝ) < log (1010 / 21) := by
  have h := (abs_le.mp (scaled_log_bound 6 (t := 334/1344) (q := 1010/21)
    (by norm_num) (by norm_num))).1
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_gt_d9]

private theorem log_witness_upper : log 350 < (6 : ℝ) := by
  have h := (abs_le.mp (scaled_log_bound 9 (t := 162/512) (q := 350)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private theorem log_left_upper : log 52000 < (11 : ℝ) := by
  have h := (abs_le.mp (scaled_log_bound 16 (t := 13536/65536) (q := 52000)
    (by norm_num) (by norm_num))).2
  norm_num [Finset.sum_range_succ] at h
  linarith only [h, log_two_lt_d9]

private noncomputable def aMin : ℝ := (2 ^ 512)⁻¹
private noncomputable def aMax : ℝ := (2 ^ 32)⁻¹

private theorem aMin_pos : 0 < aMin := by unfold aMin; positivity
private theorem aMax_pos : 0 < aMax := by unfold aMax; positivity
private theorem aMax_le_one : aMax ≤ 1 := by norm_num [aMax]

private theorem mean_pos {a : ℝ} (ha : 0 < a) : 0 < mean a := by
  unfold mean
  positivity

private theorem seam {a : ℝ} (ha : 0 < a) (ha₁ : a ≤ 1) : gap a 1024 < 0 := by
  have hl : log 1024 = 10 * log 2 := by
    rw [show (1024 : ℝ) = 2 ^ 10 by norm_num, log_pow]
    norm_num
  apply lt_of_le_of_lt (gap_le_envelope ha ha₁ (by norm_num) ?_)
  · simpa only [hl] using envelope_left
  · rw [hl]
    unfold base
    linarith only [log_two_gt_d9, log_ten_lower]

private theorem A_extra {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    log r + (20 * log 10 + log a) / 21 ≤ A a r := by
  have h₁ := log_le_log hr (show r ≤ 1 + r by linarith)
  have h₂ := log_le_log (show 0 < 100 * r by positivity)
    (show 100 * r ≤ 1 + 100 * r by linarith)
  rw [log_mul (by norm_num) hr.ne', show (100 : ℝ) = 10 ^ 2 by norm_num,
    log_pow] at h₂
  norm_num at h₂
  have h₃ := log_le_log (mul_pos hr ha) (show r * a ≤ 1 + r * a by linarith)
  rw [log_mul hr.ne' ha.ne'] at h₃
  unfold A
  linarith only [h₁, h₂, h₃]

private theorem right_loses {r : ℝ} (hr : 1024 ≤ r) : gap aMax r < 0 := by
  have hrp : 0 < r := lt_of_lt_of_le (by norm_num) hr
  have hz : 10 * log 2 ≤ log r := by
    have h := log_le_log (show (0 : ℝ) < 2 ^ 10 by positivity)
      (show (2 : ℝ) ^ 10 ≤ r by norm_num; exact hr)
    simpa [log_pow] using h
  by_cases ht : log r ≤ 32 * log 2
  · have hb : 0 < base (log r) := by
      unfold base
      linarith only [hz, log_ten_lower, log_two_gt_d9]
    exact (gap_le_envelope aMax_pos aMax_le_one hrp hb).trans_lt
      (envelope_neg ⟨hz, ht⟩)
  · have hel : log aMax = -(32 * log 2) := by rw [aMax, log_inv, log_pow]; norm_num
    have hA : base (32 * log 2) + (log r - 32 * log 2) ≤ A aMax r := by
      have h := A_extra aMax_pos hrp
      rw [hel] at h
      unfold base
      linarith only [h]
    have hbp : 0 < base (32 * log 2) := by
      unfold base
      linarith only [log_ten_lower, log_two_gt_d9]
    have hbl : base (32 * log 2) ≤ A aMax r :=
      (le_add_of_nonneg_right (sub_nonneg.mpr (le_of_not_ge ht))).trans hA
    have hlogA := log_le_log hbp hbl
    have hlogm := (log_le_log (mean_pos aMax_pos)
      (show mean aMax ≤ 1011 / 21 by unfold mean; linarith [aMax_le_one])).trans
      log_mean_upper.le
    unfold gap
    rw [log_mul hrp.ne' (mean_pos aMax_pos).ne']
    have he := envelope_right
    unfold envelope at he
    linarith only [hA, hlogA, hlogm, he]

private theorem log_one_add_lower {t : ℝ} (ht : 0 ≤ t) :
    t / (1 + t) ≤ log (1 + t) := by
  have hp : 0 < 1 + t := by linarith
  have h := log_le_sub_one_of_pos (inv_pos.mpr hp)
  rw [log_inv] at h
  have he : (1 + t)⁻¹ - 1 = -(t / (1 + t)) := by
    field_simp
    ring
  rw [he] at h
  linarith

private theorem left_upper {a r : ℝ} (ha : 0 < a) (ha₁ : a ≤ 1)
    (hr : 0 < r) (hr₀ : r ≤ 1024) : gap a r < 11 := by
  have hp : 0 < (20 / 21) * (r / (1 + r)) := by positivity
  have hA : (20 / 21) * (r / (1 + r)) ≤ A a r := by
    have h := log_one_add_lower hr.le
    have h₁ : log (1 + r) ≤ log (1 + 100 * r) :=
      log_le_log (by positivity) (by linarith)
    have h₂ := (log_pos (show 1 < 1 + r * a by linarith [mul_pos hr ha])).le
    unfold A
    linarith only [h, h₁, h₂]
  have hm : mean a ≤ 1011 / 21 := by unfold mean; linarith
  have hx : r * mean a / A a r ≤ 52000 := by
    apply (div_le_iff₀ (A_pos ha hr)).mpr
    calc
      r * mean a ≤ r * (1011 / 21) := by gcongr
      _ ≤ 52000 * ((20 / 21) * (r / (1 + r))) := by
        rw [← mul_div_assoc (20 / 21 : ℝ), ← mul_div_assoc (52000 : ℝ),
          le_div_iff₀ (show 0 < 1 + r by positivity)]
        calc
          r * (1011 / 21) * (1 + r) = r * ((1011 / 21) * (1 + r)) := by ring
          _ ≤ r * (52000 * (20 / 21)) := mul_le_mul_of_nonneg_left (by linarith) hr.le
          _ = 52000 * (20 / 21 * r) := by ring
      _ ≤ 52000 * A a r := by gcongr
  have hl := log_le_log (div_pos (mul_pos hr (mean_pos ha)) (A_pos ha hr)) hx
  rw [log_div (mul_pos hr (mean_pos ha)).ne' (A_pos ha hr).ne'] at hl
  unfold gap
  linarith only [hl, log_left_upper, A_pos ha hr]

private theorem right_witness : 11 < gap aMin (2 ^ 512) := by
  let r : ℝ := 2 ^ 512
  have hr : 0 < r := by dsimp [r]; positivity
  have hlr : log r = 512 * log 2 := by norm_num [r, log_pow]
  have hra : r * aMin = 1 := mul_inv_cancel₀ hr.ne'
  have hr₁ : 1 ≤ r := one_le_pow₀ (by norm_num)
  have h₁ := log_le_log (show 0 < 1 + r by positivity)
    (show 1 + r ≤ 2 * r by rw [two_mul]; exact add_le_add_left hr₁ r)
  rw [log_mul (by norm_num) hr.ne', hlr] at h₁
  have h₂ := log_le_log (show 0 < 1 + 100 * r by positivity)
    (show 1 + 100 * r ≤ 2 * (100 * r) by
      rw [two_mul]
      exact add_le_add_left (one_le_mul_of_one_le_of_one_le (by norm_num : (1 : ℝ) ≤ 100) hr₁) _)
  rw [log_mul (by norm_num) (by positivity : 100 * r ≠ 0),
    log_mul (by norm_num) hr.ne', hlr, show (100 : ℝ) = 10 ^ 2 by norm_num, log_pow] at h₂
  norm_num at h₂
  have hA : A aMin r ≤ (20 / 21) * (512 * log 2 + log 10) + log 2 := by
    unfold A
    rw [hra, show (1 : ℝ) + 1 = 2 by norm_num]
    linarith only [h₁, h₂]
  have hA' : A aMin r ≤ 350 := by
    linarith only [hA, log_two_lt_d9, log_ten_upper]
  have hlA := (log_le_log (A_pos aMin_pos hr) hA').trans_lt log_witness_upper
  have hlm := log_mean_lower.trans_le (log_le_log (by norm_num)
    (show (1010 / 21 : ℝ) ≤ mean aMin by unfold mean; linarith only [aMin_pos]))
  change 11 < gap aMin r
  unfold gap
  rw [log_mul hr.ne' (mean_pos aMin_pos).ne', hlr]
  linarith only [hA, hlA, hlm, log_two_gt_d9, log_two_lt_d9, log_ten_upper]

private theorem small_gap {a r : ℝ} (ha : 0 < a) (ha₁ : a ≤ 1)
    (hr : 0 < r) (hr₁ : r ≤ 1 / 1000) : gap a r ≤ 1 / 10 := by
  have hden : 0 < 1 + 100 * r := by positivity
  have hterm (x : ℝ) (hx : 0 ≤ x) (hx₁ : x ≤ 100) :
      r * x / (1 + 100 * r) ≤ log (1 + r * x) :=
    (div_le_div_of_nonneg_left (mul_nonneg hr.le hx) (by positivity)
      (add_le_add_right (mul_le_mul_of_nonneg_left hx₁ hr.le) 1 |>.trans_eq (by ring))).trans
      (log_one_add_lower (mul_nonneg hr.le hx))
  have ha' := hterm a ha.le (by linarith)
  have h₁ := hterm 1 (by norm_num) (by norm_num)
  have h₂ := hterm 100 (by norm_num) le_rfl
  rw [mul_comm r 100] at h₂
  simp only [mul_one] at h₁
  have hA : r * mean a / (1 + 100 * r) ≤ A a r := by
    unfold mean A
    linear_combination (ha' + 10 * h₁ + 10 * h₂) / 21
  have hquot : r * mean a / A a r ≤ 1 + 100 * r := by
    rw [div_le_iff₀ (A_pos ha hr), mul_comm (1 + 100 * r)]
    exact (div_le_iff₀ hden).mp hA
  have hlog := log_le_log (div_pos (mul_pos hr (mean_pos ha)) (A_pos ha hr)) hquot
  rw [log_div (mul_pos hr (mean_pos ha)).ne' (A_pos ha hr).ne'] at hlog
  have ht := log_le_sub_one_of_pos hden
  unfold gap
  linarith only [hlog, ht, hr₁, A_pos ha hr]

private noncomputable def cutoff : ℝ := exp (512 + exp 516)

private theorem witness_le_cutoff : (2 : ℝ) ^ 512 ≤ cutoff := by
  apply (log_le_log_iff (by positivity) (exp_pos _)).mp
  rw [log_pow, log_exp]
  norm_num
  linarith only [log_two_lt_d9, exp_pos (516 : ℝ)]

private theorem large_gap {a r : ℝ} (ha : aMin ≤ a) (ha₁ : a ≤ 1)
    (hr : cutoff ≤ r) : gap a r ≤ 0 := by
  have hap := aMin_pos.trans_le ha
  have hrp := (exp_pos _ : 0 < cutoff).trans_le hr
  have hla := log_le_log aMin_pos ha
  have hla' : log aMin = -(512 * log 2) := by rw [aMin, log_inv, log_pow]; norm_num
  rw [hla'] at hla
  have hlr := log_le_log (exp_pos _ : 0 < cutoff) hr
  change log (exp (512 + exp 516)) ≤ log r at hlr
  rw [log_exp] at hlr
  have hA := A_extra hap hrp
  have hAl : log r - 512 ≤ A a r := by
    linarith only [hA, hla, log_two_lt_d9, log_ten_lower]
  have h := log_le_log (exp_pos 516) (show exp 516 ≤ A a r by linarith only [hlr, hAl])
  rw [log_exp] at h
  have hm : mean a ≤ 1011 / 21 := by unfold mean; linarith
  have hlm := (log_le_log (mean_pos hap) hm).trans log_mean_upper.le
  unfold gap
  rw [log_mul hrp.ne' (mean_pos hap).ne']
  linarith only [hAl, h, hlm]

private noncomputable def clippedGap (a r : ℝ) : ℝ :=
  gap (max aMin a) (max (1 / 1000) r)

private theorem clippedGap_eq {a r : ℝ} (ha : aMin ≤ a) (hr : 1 / 1000 ≤ r) :
    clippedGap a r = gap a r := by
  unfold clippedGap
  rw [max_eq_right ha, max_eq_right hr]

private theorem clippedGap_continuous : Continuous (Function.uncurry clippedGap) := by
  have ha : Continuous (fun p : ℝ × ℝ ↦ max aMin p.1) := continuous_const.max continuous_fst
  have hr : Continuous (fun p : ℝ × ℝ ↦ max (1 / 1000) p.2) :=
    continuous_const.max continuous_snd
  have hap (p : ℝ × ℝ) : 0 < max aMin p.1 := aMin_pos.trans_le (le_max_left _ _)
  have hrp (p : ℝ × ℝ) : 0 < max (1 / 1000 : ℝ) p.2 :=
    (by norm_num : (0 : ℝ) < 1 / 1000).trans_le (le_max_left _ _)
  have hm : Continuous (fun p : ℝ × ℝ ↦ mean (max aMin p.1)) := by
    unfold mean
    fun_prop
  have hA : Continuous (fun p : ℝ × ℝ ↦ A (max aMin p.1) (max (1 / 1000) p.2)) := by
    have h₁ : Continuous (fun p : ℝ × ℝ ↦ 1 + max (1 / 1000 : ℝ) p.2 * max aMin p.1) := by
      fun_prop
    have h₂ : Continuous (fun p : ℝ × ℝ ↦ 1 + max (1 / 1000 : ℝ) p.2) := by fun_prop
    have h₃ : Continuous (fun p : ℝ × ℝ ↦ 1 + 100 * max (1 / 1000 : ℝ) p.2) := by fun_prop
    have h₁' := h₁.log (fun p ↦ (add_pos (zero_lt_one : (0 : ℝ) < 1) (mul_pos (hrp p) (hap p))).ne')
    have h₂' := h₂.log (fun p ↦ (add_pos one_pos (hrp p)).ne')
    have h₃' := h₃.log (fun p ↦ (add_pos one_pos (mul_pos (by norm_num) (hrp p))).ne')
    unfold A
    fun_prop
  exact (((hr.mul hm).log (fun p ↦ (mul_pos (hrp p) (mean_pos (hap p))).ne')).sub
    (hA.log (fun p ↦ (A_pos (hap p) (hrp p)).ne'))).sub hA

private def D (a r : ℝ) : ℝ := (1 + r) * (1 + 100 * r) * (1 + a * r)
private def N (a r : ℝ) : ℝ := 21 + (1111 + 20 * a) * r + (100 + 1010 * a) * r ^ 2
private def dD (a r : ℝ) : ℝ := 101 + a + 2 * (100 + 101 * a) * r + 300 * a * r ^ 2
private def dN (a r : ℝ) : ℝ := 1111 + 20 * a + 2 * (100 + 1010 * a) * r
private def score (a r : ℝ) : ℝ := A a r + 1 - 21 * D a r / N a r

private theorem D_pos {a r : ℝ} (ha : 0 ≤ a) (hr : 0 ≤ r) : 0 < D a r := by
  unfold D
  positivity

private theorem N_pos {a r : ℝ} (ha : 0 ≤ a) (hr : 0 ≤ r) : 0 < N a r := by
  unfold N
  positivity

private theorem A_deriv {a r : ℝ} (ha : 0 ≤ a) (hr : 0 ≤ r) :
    HasDerivAt (A a) ((a / (1 + r * a) + 10 / (1 + r) + 1000 / (1 + 100 * r)) / 21) r := by
  have h₁ := (((hasDerivAt_id r).mul_const a).const_add 1).log
    (by simp only [id_eq]; positivity)
  have h₂ := ((hasDerivAt_id r).const_add 1).log (by simp only [id_eq]; positivity)
  have h₃ := (((hasDerivAt_id r).const_mul 100).const_add 1).log
    (by simp only [id_eq]; positivity)
  convert ((h₁.add (h₂.const_mul 10)).add (h₃.const_mul 10)).div_const 21 using 1
  · rfl
  · simp only [id_eq]
    ring

private theorem A_deriv_rat {a r : ℝ} (ha : 0 ≤ a) (hr : 0 < r) :
    HasDerivAt (A a) ((21 * D a r - N a r) / (21 * D a r * r)) r := by
  convert A_deriv ha hr.le using 1
  unfold D N
  have h₁ : 0 < 1 + r * a := by positivity
  have h₂ : 0 < 1 + r := by positivity
  have h₃ : 0 < 1 + 100 * r := by positivity
  have h₄ : 0 < 1 + a * r := by positivity
  field_simp [h₁.ne', h₂.ne', h₃.ne', h₄.ne', hr.ne']
  ring

private theorem D_deriv (a r : ℝ) : HasDerivAt (D a) (dD a r) r := by
  have h := (((hasDerivAt_id r).const_add 1).mul
    (((hasDerivAt_id r).const_mul 100).const_add 1)).mul
    (((hasDerivAt_id r).const_mul a).const_add 1)
  convert h using 1
  · rfl
  · simp only [id_eq]
    simp only [Pi.mul_apply]
    unfold dD
    ring

private theorem N_deriv (a r : ℝ) : HasDerivAt (N a) (dN a r) r := by
  have h := ((((hasDerivAt_id r).const_mul (1111 + 20 * a)).const_add 21).add
    (((hasDerivAt_id r).pow 2).const_mul (100 + 1010 * a)))
  convert h using 1
  · rfl
  · simp only [id_eq, Nat.cast_ofNat]
    unfold dN
    ring

private theorem score_cont {a r : ℝ} (ha : 0 ≤ a) (hr : 0 ≤ r) :
    ContinuousAt (score a) r := by
  exact ((A_deriv ha hr).continuousAt.add continuousAt_const).sub
    (((D_deriv a r).continuousAt.const_mul 21).div (N_deriv a r).continuousAt
      (N_pos ha hr).ne')

private theorem gap_deriv {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    HasDerivAt (gap a) (N a r * score a r / (21 * D a r * r * A a r)) r := by
  have hA := A_deriv_rat ha.le hr
  have hm := mean_pos ha
  have h := (((hasDerivAt_id r).mul_const (mean a)).log
    (by simpa only [id_eq] using (mul_pos hr hm).ne')).sub
    (hA.log (A_pos ha hr).ne') |>.sub hA
  convert h using 1
  · rfl
  · simp only [id_eq]
    unfold score
    field_simp [(N_pos ha.le hr.le).ne', (D_pos ha.le hr.le).ne', hr.ne',
      (A_pos ha hr).ne', hm.ne']
    ring

private theorem gap_stationary {a r : ℝ} (ha : 0 < a) (hr : 0 < r)
    (h : deriv (gap a) r = 0) : score a r = 0 := by
  rw [(gap_deriv ha hr).deriv] at h
  exact (mul_eq_zero.mp ((div_eq_zero_iff.mp h).resolve_right
    (by have := D_pos ha.le hr.le; have := A_pos ha hr; positivity))).resolve_left
    (N_pos ha.le hr.le).ne'

open Polynomial in
private def scorePoly (a : ℝ) : ℝ[X] := ofCoeffList [
  0,
  0,
  399 * a ^ 2 - 84840 * a + 1260210,
  400 * a ^ 3 - 45753 * a ^ 2 - 10464750 * a + 1076984210,
  39390 * a ^ 3 - 11761773 * a ^ 2 + 2635806090 * a - 1973577000,
  -5910 * a ^ 3 + 2508656079 * a ^ 2 - 7839717000 * a - 3999600000,
  950925100 * a ^ 3 - 8402061300 * a ^ 2 - 13816800000 * a + 20000000,
  -21000 * a * (120611 * a ^ 2 + 679730 * a + 20000),
  -44100000 * a ^ 2 * (101 * a + 10)]

private theorem eval_scorePoly (a r : ℝ) :
    (scorePoly a).eval r = (21 * D a r - N a r) * N a r ^ 2 -
      21 ^ 2 * r * D a r * (N a r * dD a r - dN a r * D a r) := by
  simp only [scorePoly, Polynomial.ofCoeffList_cons, Polynomial.ofCoeffList_nil,
    Polynomial.eval_add, Polynomial.eval_mul, Polynomial.eval_C, Polynomial.eval_X,
    Polynomial.eval_zero]
  unfold D N dD dN
  ring

private theorem score_deriv_root {a r : ℝ} (ha : 0 ≤ a) (hr : 0 < r)
    (hz : deriv (score a) r = 0) : (scorePoly a).IsRoot r := by
  have hd := (((A_deriv_rat ha hr).add_const 1).sub
    (((D_deriv a r).const_mul 21).div (N_deriv a r) (N_pos ha hr.le).ne')).deriv
  change deriv (score a) r = _ at hd
  rw [hz] at hd
  change (scorePoly a).eval r = 0
  rw [eval_scorePoly]
  field_simp [(D_pos ha hr.le).ne', (N_pos ha hr.le).ne', hr.ne'] at hd
  linear_combination -hd

private theorem scorePoly_bound {a : ℝ} (ha : 0 < a) (ha' : a ≤ 1 / 1000) :
    (scorePoly a).roots.countP (0 < ·) ≤ 3 ∧ scorePoly a ≠ 0 := by
  classical
  let P := scorePoly a
  have ha₁ : a ≤ 1 := by linarith
  have hsq : a ^ 2 ≤ a := by nlinarith only [mul_nonneg ha.le (sub_nonneg.mpr ha₁)]
  have hcube : a ^ 3 ≤ a := by
    calc
      a ^ 3 = a ^ 2 * a := by ring
      _ ≤ a * a := mul_le_mul_of_nonneg_right hsq ha.le
      _ ≤ a := by nlinarith only [hsq]
  have h₂ : 0 < 399 * a ^ 2 - 84840 * a + 1260210 := by
    nlinarith only [ha', sq_nonneg a]
  have h₃ : 0 < 400 * a ^ 3 - 45753 * a ^ 2 - 10464750 * a + 1076984210 := by
    nlinarith only [ha', hsq, pow_nonneg ha.le 3]
  have h₄ : 39390 * a ^ 3 - 11761773 * a ^ 2 + 2635806090 * a - 1973577000 < 0 := by
    nlinarith only [ha', hcube, sq_nonneg a]
  have h₅ : -5910 * a ^ 3 + 2508656079 * a ^ 2 - 7839717000 * a - 3999600000 < 0 := by
    nlinarith only [ha', hsq, ha, pow_nonneg ha.le 3]
  have h₆ : 0 < 950925100 * a ^ 3 - 8402061300 * a ^ 2 - 13816800000 * a + 20000000 := by
    nlinarith only [ha', hsq, pow_nonneg ha.le 3]
  have h₇ : -21000 * a * (120611 * a ^ 2 + 679730 * a + 20000) < 0 := by
    exact mul_neg_of_neg_of_pos (mul_neg_of_neg_of_pos (by norm_num) ha) (by positivity)
  have h₈ : -44100000 * a ^ 2 * (101 * a + 10) < 0 := by
    exact mul_neg_of_neg_of_pos (mul_neg_of_neg_of_pos (by norm_num) (pow_pos ha _))
      (by positivity)
  have hc₈ : P.coeff 8 ≠ 0 := by
    simpa [P, scorePoly] using h₈.ne
  have hP : P ≠ 0 := fun h ↦ hc₈ (by simp [h])
  have hdeg : P.natDegree = 8 := by
    refine le_antisymm (Polynomial.natDegree_le_iff_coeff_eq_zero.mpr ?_)
      (Polynomial.le_natDegree_of_ne_zero hc₈)
    intro n hn
    simp only [P, scorePoly, Polynomial.coeff_ofCoeffList]
    apply List.getD_eq_default
    exact Nat.succ_le_of_lt hn
  refine ⟨P.roots_countP_pos_le_signVariations.trans ?_, hP⟩
  rw [Polynomial.signVariations, Polynomial.coeffList_of_ne_zero hP, hdeg]
  rw [show (List.range (8 + 1)).reverse = [8, 7, 6, 5, 4, 3, 2, 1, 0] by decide]
  simp only [List.map_cons, List.map_nil, P, scorePoly, Polynomial.coeff_ofCoeffList,
    List.getD_cons_succ, List.getD_cons_zero]
  simp only [List.signVariations, List.map_cons, List.map_nil, sign_neg h₈, sign_neg h₇,
    sign_pos h₆, sign_neg h₅, sign_neg h₄, sign_pos h₃, sign_pos h₂, sign_zero]
  decide

private theorem no_four_score_zeros {a p q r s : ℝ} (ha : 0 < a) (ha' : a ≤ 1 / 1000)
    (hp : 0 < p) (hpq : p < q) (hqr : q < r) (hrs : r < s)
    (h₁ : score a p = 0) (h₂ : score a q = 0) (h₃ : score a r = 0)
    (h₄ : score a s = 0) : False := by
  classical
  have hz : score a 0 = 0 := by norm_num [score, A, D, N]
  have cont (u v : ℝ) (hu : 0 ≤ u) : ContinuousOn (score a) (Icc u v) :=
    fun t ht ↦ (score_cont ha.le (hu.trans ht.1)).continuousWithinAt
  obtain ⟨u, hu, hu'⟩ := exists_deriv_eq_zero hp (cont 0 p le_rfl) (hz.trans h₁.symm)
  obtain ⟨v, hv, hv'⟩ := exists_deriv_eq_zero hpq (cont p q hp.le) (h₁.trans h₂.symm)
  obtain ⟨w, hw, hw'⟩ := exists_deriv_eq_zero hqr (cont q r (hp.trans hpq).le) (h₂.trans h₃.symm)
  obtain ⟨y, hy, hy'⟩ := exists_deriv_eq_zero hrs
    (cont r s (hp.trans (hpq.trans hqr)).le) (h₃.trans h₄.symm)
  have huv := hu.2.trans hv.1
  have hvw := hv.2.trans hw.1
  have hwy := hw.2.trans hy.1
  let P := scorePoly a
  have hb := scorePoly_bound ha ha'
  change P.roots.countP (0 < ·) ≤ 3 ∧ P ≠ 0 at hb
  let roots := (P.roots.filter (0 < ·)).toFinset
  have hmem (t : ℝ) (ht : 0 < t) (ht' : deriv (score a) t = 0) : t ∈ roots := by
    simp only [roots, Multiset.mem_toFinset, Multiset.mem_filter, Polynomial.mem_roots hb.2]
    exact ⟨score_deriv_root ha.le ht ht', ht⟩
  have hs : ({u, v, w, y} : Finset ℝ) ⊆ roots := by
    intro t ht
    simp only [Finset.mem_insert, Finset.mem_singleton] at ht
    rcases ht with rfl | rfl | rfl | rfl
    · exact hmem _ hu.1 hu'
    · exact hmem _ (hu.1.trans huv) hv'
    · exact hmem _ (hu.1.trans (huv.trans hvw)) hw'
    · exact hmem _ (hu.1.trans (huv.trans (hvw.trans hwy))) hy'
  have hc : ({u, v, w, y} : Finset ℝ).card = 4 := by
    rw [Finset.card_insert_of_notMem, Finset.card_insert_of_notMem, Finset.card_insert_of_notMem]
    all_goals simp [huv.ne, hvw.ne, hwy.ne, (huv.trans hvw).ne,
      (hvw.trans hwy).ne, (huv.trans (hvw.trans hwy)).ne]
  have hcount := (Finset.card_le_card hs).trans
    (Multiset.toFinset_card_le (P.roots.filter (0 < ·)))
  rw [hc, ← Multiset.countP_eq_card_filter] at hcount
  omega

private theorem no_three_profile_maxima {a p q r : ℝ} (ha : 0 < a) (ha' : a ≤ 1 / 1000)
    (hp : 0 < p) (hpq : p < q) (hqr : q < r)
    (h₁ : IsMaxOn (F (sample a)) (Ioi 0) p)
    (h₂ : IsMaxOn (F (sample a)) (Ioi 0) q)
    (h₃ : IsMaxOn (F (sample a)) (Ioi 0) r) : False := by
  have hq := hp.trans hpq
  have hr := hq.trans hqr
  have he : F (sample a) = gap a := funext fun _ ↦ (gap_eq _ _).symm
  rw [he] at h₁ h₂ h₃
  have hcont (u v : ℝ) (hu : 0 < u) : ContinuousOn (gap a) (Icc u v) :=
    fun t ht ↦ (gap_deriv ha (hu.trans_le ht.1)).continuousAt.continuousWithinAt
  obtain ⟨u, hu, hdu⟩ := exists_deriv_eq_zero hpq (hcont p q hp)
    (le_antisymm (h₂ hp) (h₁ hq))
  obtain ⟨v, hv, hdv⟩ := exists_deriv_eq_zero hqr (hcont q r hq)
    (le_antisymm (h₃ hq) (h₂ hr))
  exact no_four_score_zeros ha ha' hp hu.1 hu.2 hv.1
    (gap_stationary ha hp (h₁.isLocalMax (Ioi_mem_nhds hp)).deriv_eq_zero)
    (gap_stationary ha (hp.trans hu.1) hdu)
    (gap_stationary ha hq (h₂.isLocalMax (Ioi_mem_nhds hq)).deriv_eq_zero)
    (gap_stationary ha (hq.trans hv.1) hdv)

private theorem maximize_profiles {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b : ℝ × ℝ} (hb : b ∈ Lomax.domain)
    (hm : IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b) :
    b.1 = shape x b.2 ∧ IsMaxOn (F x) (Ioi 0) b.2 := by
  have hr : 0 < b.2 := hb.2
  have hs := Lomax.shape_profile hn hx hr
  have he : b.1 = shape x b.2 := by
    by_contra h
    exact (hs.2 _ hb.1 h).not_ge (hm (show (shape x b.2, b.2) ∈ Lomax.domain
      from ⟨hs.1, hr⟩))
  refine ⟨he, ?_⟩
  intro r hr'
  change 0 < r at hr'
  have h := hm (show (shape x r, r) ∈ Lomax.domain from
    ⟨(Lomax.shape_profile hn hx hr').1, hr'⟩)
  change Lomax.logLik x (shape x r) r ≤ Lomax.logLik x b.1 b.2 at h
  rw [he] at h
  have h' := sub_le_sub_right h (bestExp x)
  rw [profile_gap hn hx hr', profile_gap hn hx hr] at h'
  exact le_of_mul_le_mul_left h' (Nat.cast_pos.mpr hn)

private def A_rate (a r : ℝ) : ℝ :=
  (a / (1 + r * a) + 10 / (1 + r) + 1000 / (1 + 100 * r)) / 21

private theorem A_rate_strict {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    r * A_rate a r < A a r := by
  have hit {u : ℝ} (hu : 0 < u) : u / (1 + u) < log (1 + u) := by
    apply lt_of_le_of_lt _ (lt_log_one_add_of_pos hu)
    apply (div_le_div_iff₀ (by positivity) (by positivity)).mpr
    nlinarith only [sq_nonneg u]
  have h₁ := hit (mul_pos hr ha)
  have h₂ := hit hr
  have h₃ := hit (show 0 < 100 * r by positivity)
  unfold A_rate A
  linear_combination (h₁ + 10 * h₂ + 10 * h₃) / 21

private theorem A_over_rate_antitone {a : ℝ} (ha : 0 < a) :
    AntitoneOn (fun r ↦ A a r / r) (Ioi 0) := by
  have hd {r : ℝ} (hr : 0 < r) :
      HasDerivAt (fun t ↦ A a t / t) ((A_rate a r * r - A a r) / r ^ 2) r := by
    convert (A_deriv ha.le hr.le).div (hasDerivAt_id r) hr.ne' using 1
    · rfl
    · simp only [id_eq, mul_one, A_rate]
  apply antitoneOn_of_deriv_nonpos (convex_Ioi _)
    (fun _ hr ↦ (hd hr).continuousAt.continuousWithinAt)
    (fun _ hr ↦ (hd (interior_subset hr)).differentiableAt.differentiableWithinAt)
  intro r hr
  have hr' : 0 < r := interior_subset hr
  rw [(hd hr').deriv]
  exact div_nonpos_of_nonpos_of_nonneg
    (sub_nonpos.mpr (by rw [mul_comm]; exact (A_rate_strict ha hr').le)) (sq_nonneg _)

private theorem A_observation_deriv {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    HasDerivAt (fun t ↦ A t r) ((r / (1 + r * a)) / 21) a := by
  have h := (((hasDerivAt_id a).const_mul r).const_add 1).log
    (by simp only [id_eq]; positivity)
  convert ((h.add_const (10 * log (1 + r))).add_const (10 * log (1 + 100 * r))).div_const
    21 using 1
  · funext t
    unfold A
    rw [mul_comm 100 r]
    simp only [id_eq]
  · simp

private theorem A_minus_product_antitone {r : ℝ} (hr : 0 < r) :
    AntitoneOn (fun a ↦ A a r - a * r) (Ioi 0) := by
  have hd {a : ℝ} (ha : 0 < a) :
      HasDerivAt (fun t ↦ A t r - t * r) (r / (1 + r * a) / 21 - r) a := by
    convert (A_observation_deriv ha hr).sub ((hasDerivAt_id a).mul_const r) using 1
    · rfl
    · simp
  apply antitoneOn_of_deriv_nonpos (convex_Ioi _)
    (fun _ ha ↦ (hd ha).continuousAt.continuousWithinAt)
    (fun _ ha ↦ (hd (interior_subset ha)).differentiableAt.differentiableWithinAt)
  intro a ha
  have ha' : 0 < a := interior_subset ha
  rw [(hd ha').deriv]
  have h := div_le_self hr.le (le_add_of_nonneg_right (mul_nonneg hr.le ha'.le))
  linarith only [h, hr]

private theorem score_root_bounds {a r : ℝ} (ha : 0 < a) (ha' : a ≤ 1)
    (hr : 0 < r) (h : score a r = 0) : a * r ≤ A a r := by
  have hD := D_pos ha.le hr.le
  have hN := N_pos ha.le hr.le
  have h₁ : 0 < 1 + r * a := by positivity
  have h₂ : 0 < 1 + r := by positivity
  have h₃ : 0 < 1 + 100 * r := by positivity
  have h₄ : 0 < 1 + a * r := by positivity
  have hU : N a r / (21 * D a r) =
      (1 / (1 + r * a) + 10 / (1 + r) + 10 / (1 + 100 * r)) / 21 := by
    unfold N D
    field_simp [h₁.ne', h₂.ne', h₃.ne', h₄.ne']
    ring
  have hlow : 1 / (1 + r) ≤ 1 / (1 + r * a) :=
    div_le_div_of_nonneg_left (by norm_num) h₁ (by
      linarith only [mul_le_mul_of_nonneg_left ha' hr.le])
  have hhi : 1 / (1 + 100 * r) ≤ 1 / (1 + r * a) :=
    div_le_div_of_nonneg_left (by norm_num) h₁ (by
      have h := mul_le_mul_of_nonneg_left (show a ≤ 100 by linarith only [ha']) hr.le
      nlinarith only [h])
  have he : N a r / (21 * D a r) ≤ (1 + r * a)⁻¹ := by
    rw [hU, ← one_div]
    linear_combination (10 * hlow + 10 * hhi) / 21
  have hInv := inv_anti₀ (show 0 < N a r / (21 * D a r) by positivity) he
  rw [inv_inv, inv_div] at hInv
  unfold score at h
  nlinarith only [hInv, h]

private def sensitivity (a r : ℝ) : ℝ := r / (1 + a * r) * (1 + (A a r)⁻¹)

private theorem sensitivity_deriv {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    HasDerivAt (sensitivity a)
      ((A a r * (A a r + 1) - (1 + a * r) * (r * A_rate a r)) /
        ((1 + a * r) ^ 2 * (A a r) ^ 2)) r := by
  have hden : 0 < 1 + a * r := by positivity
  have h₁ := (hasDerivAt_id r).div (((hasDerivAt_id r).const_mul a).const_add 1)
    (by simpa only [id_eq] using hden.ne')
  have hA : HasDerivAt (A a) (A_rate a r) r := A_deriv ha.le hr.le
  have h₂ := (hA.inv (A_pos ha hr).ne').const_add 1
  convert h₁.mul h₂ using 1
  · rfl
  · simp only [id_eq, Pi.div_apply, Pi.inv_apply]
    field_simp [hden.ne', (A_pos ha hr).ne']
    ring

private theorem sensitivity_increases {a p q : ℝ} (ha : 0 < a) (hp : 0 < p)
    (hAq : a * q ≤ A a q) :
    StrictMonoOn (sensitivity a) (Icc p q) := by
  apply strictMonoOn_of_deriv_pos (convex_Icc ..)
    (fun r hr ↦ (sensitivity_deriv ha (hp.trans_le hr.1)).continuousAt.continuousWithinAt)
  intro r hr
  have hr' : r ∈ Icc p q := interior_subset hr
  have hrp := hp.trans_le hr'.1
  have hqp := hrp.trans_le hr'.2
  have hAr : a * r ≤ A a r :=
    (le_div_iff₀ hrp).mp
      (((le_div_iff₀ hqp).mpr hAq).trans (A_over_rate_antitone ha hrp hqp hr'.2))
  have hneg := A_rate_strict ha hrp
  have hden : 0 < 1 + a * r := by positivity
  have hnum : (1 + a * r) * (r * A_rate a r) < A a r * (A a r + 1) := by
    calc
      _ < (1 + a * r) * A a r := mul_lt_mul_of_pos_left hneg hden
      _ ≤ A a r * (A a r + 1) := by
        rw [mul_comm (1 + a * r)]
        exact mul_le_mul_of_nonneg_left (by linarith only [hAr]) (A_pos ha hrp).le
  rw [(sensitivity_deriv ha hrp).deriv]
  exact div_pos (sub_pos.mpr hnum) (mul_pos (pow_pos hden _) (pow_pos (A_pos ha hrp) _))

private theorem gap_observation_deriv {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    HasDerivAt (fun t ↦ gap t r)
      (1 / (21 * mean a) - sensitivity a r / 21) a := by
  have hm : HasDerivAt mean (1 / 21) a :=
    ((hasDerivAt_id a).const_add 1010).div_const 21
  have h₁ := (hm.const_mul r).log (mul_pos hr (mean_pos ha)).ne'
  have hA := A_observation_deriv ha hr
  convert (h₁.sub (hA.log (A_pos ha hr).ne')).sub hA using 1
  · rfl
  · unfold sensitivity
    have h₁' : 0 < 1 + r * a := by positivity
    have h₂' : 0 < 1 + a * r := by positivity
    field_simp [hr.ne', (mean_pos ha).ne', (A_pos ha hr).ne', h₁'.ne', h₂'.ne']
    ring

private theorem gap_difference_decreases {a b p q : ℝ}
    (ha : 0 < a) (hp : 0 < p) (hpq : p < q)
    (hA : b * q ≤ A b q) :
    StrictAntiOn (fun u ↦ gap u q - gap u p) (Icc a b) := by
  have hq := hp.trans hpq
  have hd {u : ℝ} (hu : 0 < u) :=
    (gap_observation_deriv hu hq).sub (gap_observation_deriv hu hp)
  apply strictAntiOn_of_deriv_neg (convex_Icc ..)
    (fun _ hu ↦ (hd (ha.trans_le hu.1)).continuousAt.continuousWithinAt)
  intro u hu
  have hu' : u ∈ Icc a b := interior_subset hu
  have hup := ha.trans_le hu'.1
  have hbp := hup.trans_le hu'.2
  have h : A b q - b * q ≤ A u q - u * q := A_minus_product_antitone hq hup hbp hu'.2
  have hA' : u * q ≤ A u q := by linarith only [hA, h]
  have hs := sensitivity_increases hup hp hA' (left_mem_Icc.mpr hpq.le)
    (right_mem_Icc.mpr hpq.le) hpq
  rw [(hd hup).deriv]
  linarith only [hs]

private theorem separated_ties_cannot_increase {a b : ℝ} (ha : 0 < a) (hab : a < b)
    (hb₁ : b ≤ 1) {p q : ℝ × ℝ} (hp : p ∈ Lomax.domain) (hq : q ∈ Lomax.domain)
    (hpq : p.2 < q.2)
    (hm₁ : IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik (sample a) v.1 v.2) Lomax.domain p)
    (hm₂ : IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik (sample b) v.1 v.2) Lomax.domain q) :
    False := by
  have hb := ha.trans hab
  have h₁ := (maximize_profiles (by decide) (sample_positive ha) hp hm₁).2
  have h₂ := (maximize_profiles (by decide) (sample_positive hb) hq hm₂).2
  have hfun₁ : F (sample a) = gap a := funext fun _ ↦ (gap_eq _ _).symm
  have hfun₂ : F (sample b) = gap b := funext fun _ ↦ (gap_eq _ _).symm
  rw [hfun₁] at h₁
  rw [hfun₂] at h₂
  have hq₀ : 0 < q.2 := hq.2
  have hp₀ : 0 < p.2 := hp.2
  have hz := (h₂.isLocalMax (Ioi_mem_nhds hq₀)).deriv_eq_zero
  have hA := score_root_bounds hb hb₁ hq₀ (gap_stationary hb hq₀ hz)
  have ht : gap b q.2 - gap b p.2 < gap a q.2 - gap a p.2 :=
    gap_difference_decreases ha hp₀ hpq hA (left_mem_Icc.mpr hab.le)
      (right_mem_Icc.mpr hab.le) hab
  have hqa : gap a q.2 ≤ gap a p.2 := h₁ hq₀
  have hpb : gap b p.2 ≤ gap b q.2 := h₂ hp₀
  linarith only [ht, hqa, hpb]

private theorem three_level_exists :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p ∈ Lomax.domain, ∃ q ∈ Lomax.domain,
        p.2 < 1024 ∧ 1024 < q.2 ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain p ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain q ∧
        (∀ b ∈ Lomax.domain,
          IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b → b = p ∨ b = q) ∧
        bestExp x < Lomax.logLik x p.1 p.2 := by
  have hab : aMin ≤ aMax := by
    unfold aMin aMax
    exact inv_anti₀ (by positivity) (pow_le_pow_right₀ (by norm_num) (by norm_num))
  have hls : (1 / 1000 : ℝ) ≤ 1024 := by norm_num
  have hsw : (1024 : ℝ) ≤ 2 ^ 512 := by
    rw [show (1024 : ℝ) = 2 ^ 10 by norm_num]
    exact pow_le_pow_right₀ (by norm_num) (by norm_num)
  have hsu := hsw.trans witness_le_cutoff
  have hw : (1 / 10 : ℝ) ∈ Icc (1 / 1000 : ℝ) 1024 := by constructor <;> norm_num
  have ha : ∃ t ∈ Icc (1024 : ℝ) cutoff,
      ∀ r ∈ Icc (1 / 1000 : ℝ) 1024, clippedGap aMin r < clippedGap aMin t := by
    refine ⟨2 ^ 512, ⟨hsw, witness_le_cutoff⟩, fun r hr ↦ ?_⟩
    rw [clippedGap_eq le_rfl hr.1, clippedGap_eq le_rfl (hls.trans hsw)]
    exact (left_upper aMin_pos (hab.trans aMax_le_one)
      ((by norm_num : (0 : ℝ) < 1 / 1000).trans_le hr.1) hr.2).trans right_witness
  have hb (r : ℝ) (hr : r ∈ Icc (1024 : ℝ) cutoff) :
      clippedGap aMax r < clippedGap aMax (1 / 10) := by
    rw [clippedGap_eq hab (hls.trans hr.1), clippedGap_eq hab hw.1]
    exact (right_loses hr.1).trans ((by norm_num : (0 : ℝ) < 1 / 10).trans
      (left_witness aMax_pos aMax_le_one))
  have hsep (a : ℝ) (ha : a ∈ Icc aMin aMax) :
      clippedGap a 1024 < clippedGap a (1 / 10) := by
    rw [clippedGap_eq ha.1 hls, clippedGap_eq ha.1 hw.1]
    have hp := aMin_pos.trans_le ha.1
    have hu := ha.2.trans aMax_le_one
    exact (seam hp hu).trans ((by norm_num : (0 : ℝ) < 1 / 10).trans (left_witness hp hu))
  obtain ⟨a, ha, p, hp, q, hq, hpm, hqm⟩ :=
    compact_crossing hab hls hsu hw clippedGap_continuous ha hb hsep
  have hap := aMin_pos.trans ha.1
  have hau := ha.2.le.trans aMax_le_one
  have hp₀ := (by norm_num : (0 : ℝ) < 1 / 1000).trans_le hp.1
  have hq₀ := (by norm_num : (0 : ℝ) < 1024).trans hq.1
  have hwc : (1 / 10 : ℝ) ∈ Icc (1 / 1000 : ℝ) cutoff := ⟨hw.1, hw.2.trans hsu⟩
  have global (u : ℝ) (hu : 1 / 1000 ≤ u)
      (hm : IsMaxOn (clippedGap a) (Icc (1 / 1000) cutoff) u) :
      IsMaxOn (F (sample a)) (Ioi 0) u ∧ 1 / 10 < F (sample a) u := by
    have hwle := hm hwc
    change clippedGap a (1 / 10) ≤ clippedGap a u at hwle
    rw [clippedGap_eq ha.1.le hw.1, clippedGap_eq ha.1.le hu] at hwle
    have huw := (left_witness hap hau).trans_le hwle
    rw [← gap_eq]
    refine ⟨?_, huw⟩
    intro r hr
    change 0 < r at hr
    change F (sample a) r ≤ F (sample a) u
    rw [← gap_eq, ← gap_eq]
    by_cases hr₁ : 1 / 1000 ≤ r
    · by_cases hr₂ : r ≤ cutoff
      · have h := hm (show r ∈ Icc (1 / 1000) cutoff from ⟨hr₁, hr₂⟩)
        change clippedGap a r ≤ clippedGap a u at h
        rwa [clippedGap_eq ha.1.le hr₁, clippedGap_eq ha.1.le hu] at h
      · exact (large_gap ha.1.le hau (le_of_not_ge hr₂)).trans
          ((by norm_num : (0 : ℝ) ≤ 1 / 10).trans huw.le)
    · exact (small_gap hap hau hr (le_of_not_ge hr₁)).trans huw.le
  have hpg := global p hp.1 hpm
  have hqg := global q (hls.trans hq.1.le) hqm
  have hx := sample_positive hap
  have hp' := lift_profile (by decide) hx hp₀ hpg.1
  have hq' := lift_profile (by decide) hx hq₀ hqg.1
  refine ⟨a, ha, (shape (sample a) p, p), hp'.1, (shape (sample a) q, q), hq'.1,
    hp.2, hq.1, hp'.2, hq'.2, ?_, ?_⟩
  · intro b hb hbm
    change IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik (sample a) v.1 v.2) Lomax.domain b at hbm
    have hb' := maximize_profiles (by decide) hx hb hbm
    have hb₀ : 0 < b.2 := hb.2
    have hpq := hp.2.trans hq.1
    have hat : a ≤ 1 / 1000 := ha.2.le.trans (by norm_num [aMax])
    rcases lt_trichotomy b.2 p with h | h | h
    · exact (no_three_profile_maxima hap hat hb₀ h hpq hb'.2 hpg.1 hqg.1).elim
    · exact Or.inl (Prod.ext (hb'.1.trans (congrArg (shape (sample a)) h)) h)
    · rcases lt_trichotomy b.2 q with h' | h' | h'
      · exact (no_three_profile_maxima hap hat hp₀ h h' hpg.1 hb'.2 hqg.1).elim
      · exact Or.inr (Prod.ext (hb'.1.trans (congrArg (shape (sample a)) h')) h')
      · exact (no_three_profile_maxima hap hat hp₀ hpq h' hpg.1 hqg.1 hb'.2).elim
  · have hg := profile_gap (by decide) hx hp₀
    norm_num at hg
    change bestExp (sample a) < Lomax.logLik (sample a) (shape (sample a) p) p
    linarith only [hg, hpg.2]

private def exactly_two_separated (a : ℝ) : Prop :=
  let x := sample a
  let f := fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2
  ∃ p ∈ Lomax.domain, ∃ q ∈ Lomax.domain,
    p.2 < 1024 ∧ 1024 < q.2 ∧
    IsMaxOn f Lomax.domain p ∧ IsMaxOn f Lomax.domain q ∧
    (∀ b ∈ Lomax.domain, IsMaxOn f Lomax.domain b → b = p ∨ b = q) ∧
    bestExp x < f p

private theorem cannot_have_two_parameters {a b : ℝ}
    (ha : a ∈ Ioo aMin aMax) (hb : b ∈ Ioo aMin aMax) (hab : a < b)
    (ht₁ : exactly_two_separated a) (ht₂ : exactly_two_separated b) : False := by
  obtain ⟨p, hp, _, _, hps, _, hpm, _, _, _⟩ := ht₁
  obtain ⟨_, _, q, hq, _, hqs, _, hqm, _, _⟩ := ht₂
  exact separated_ties_cannot_increase (aMin_pos.trans ha.1) hab (hb.2.le.trans aMax_le_one)
    hp hq (hps.trans hqs) hpm hqm

/-- Ten copies each of 1 and 100 and one uniquely specified observation in
the dyadic interval give exactly two finite global Lomax maximizers above
the exponential limit. -/
theorem three_level_global_tie :
    ∃! a, a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹ ∧
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p ∈ Lomax.domain, ∃ q ∈ Lomax.domain,
        p.2 < 1024 ∧ 1024 < q.2 ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain p ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain q ∧
        (∀ b ∈ Lomax.domain,
          IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b → b = p ∨ b = q) ∧
        bestExp x < Lomax.logLik x p.1 p.2 := by
  change ∃! a, a ∈ Ioo aMin aMax ∧ exactly_two_separated a
  obtain ⟨a, ha, ht⟩ := three_level_exists
  refine ⟨a, ⟨ha, ht⟩, ?_⟩
  intro b hb
  rcases lt_trichotomy b a with h | h | h
  · exact (cannot_have_two_parameters hb.1 ha h hb.2 ht).elim
  · exact h
  · exact (cannot_have_two_parameters ha hb.1 h ht hb.2).elim

/-- Taking reciprocals preserves both tied parameter pairs in the
inverse Pareto parameterization. -/
theorem inverse_pareto_global_tie :
    ∃ x : Fin 21 → ℝ, (∀ i, 0 < x i) ∧
      ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.InvPareto.logLik x b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.InvPareto.bestInvExp x < Transfer.InvPareto.logLik x p.1 p.2 := by
  obtain ⟨a, ha, p, hp, q, hq, hps, hqs, hpm, hqm, hall, hexp⟩ := three_level_exists
  let x := sample a
  let y := Transfer.reciprocal x
  have hx := sample_positive (aMin_pos.trans ha.1)
  have hy (i : Fin 21) : 0 < y i := inv_pos.mpr (hx i)
  have hrec : Transfer.reciprocal y = x := by
    ext i
    simp [y, Transfer.reciprocal]
  have hlik (b : ℝ × ℝ) :
      Transfer.InvPareto.logLik y b.1 b.2 =
        Lomax.logLik x b.1 b.2 + Transfer.reciprocalJac y := by
    rw [Transfer.InvPareto.logLik_transfer, hrec]
  have hmap (b : ℝ × ℝ) (hb : b ∈ Lomax.domain) :
      IsMaxOn (fun v : ℝ × ℝ ↦ Transfer.InvPareto.logLik y v.1 v.2) Lomax.domain b ↔
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b :=
    Transfer.isMaxOn_iff (bijOn_id Lomax.domain) (fun v _ ↦ hlik v) hb
  refine ⟨y, hy, p, q, hps.trans hqs, ?_, ?_⟩
  · ext v
    simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
    constructor
    · rintro ⟨hv, hm⟩
      exact hall v hv ((hmap v hv).mp hm)
    · rintro (rfl | rfl)
      · exact ⟨hp, (hmap _ hp).mpr hpm⟩
      · exact ⟨hq, (hmap _ hq).mpr hqm⟩
  · rw [Transfer.InvPareto.bestInvExp, hrec, hlik p]
    exact add_lt_add_left hexp _

/-- For every held threshold, translating the Lomax sample gives two and
only two Pareto II maximizers, with positive excesses throughout. -/
theorem pareto_ii_global_tie (μ : ℝ) :
    ∃ x : Fin 21 → ℝ, (∀ i, μ < x i) ∧
      ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.ParetoII.logLik x μ b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.ParetoII.bestShiftedExp x μ < Transfer.ParetoII.logLik x μ q.1 q.2 := by
  obtain ⟨a, ha, p, hp, q, hq, hps, hqs, hpm, hqm, hall, hexp⟩ := three_level_exists
  let x := sample a
  change IsMaxOn (fun b : ℝ × ℝ ↦ Lomax.logLik x b.1 b.2) Lomax.domain p at hpm
  change IsMaxOn (fun b : ℝ × ℝ ↦ Lomax.logLik x b.1 b.2) Lomax.domain q at hqm
  let y (i : Fin 21) := x i + μ
  have hx := sample_positive (aMin_pos.trans ha.1)
  have hy (i : Fin 21) : μ < y i := by dsimp only [y]; linarith only [hx i]
  have hexc : Transfer.ParetoII.excess y μ = x := by
    ext i
    simp [y, Transfer.ParetoII.excess]
  let φ (v : ℝ × ℝ) : ℝ × ℝ := (v.1, v.2⁻¹)
  have hinv (v : ℝ × ℝ) : φ (φ v) = v := by simp [φ]
  have hdom (v : ℝ × ℝ) (hv : v ∈ Lomax.domain) : φ v ∈ Lomax.domain :=
    ⟨hv.1, inv_pos.mpr hv.2⟩
  have hφ : BijOn φ Lomax.domain Lomax.domain := by
    refine ⟨hdom, ?_, ?_⟩
    · intro v _ w _ h
      simpa only [hinv] using congrArg φ h
    · exact fun v hv ↦ ⟨φ v, hdom v hv, hinv v⟩
  have hlik (v : ℝ × ℝ) :
      Transfer.ParetoII.logLik y μ v.1 v.2 = Lomax.logLik x (φ v).1 (φ v).2 := by
    rw [Transfer.ParetoII.logLik_transfer, hexc]
  have hmap (v : ℝ × ℝ) (hv : v ∈ Lomax.domain) :
      IsMaxOn (fun b : ℝ × ℝ ↦ Transfer.ParetoII.logLik y μ b.1 b.2) Lomax.domain v ↔
        IsMaxOn (fun b : ℝ × ℝ ↦ Lomax.logLik x b.1 b.2) Lomax.domain (φ v) :=
    Transfer.isMaxOn_iff (c := 0) hφ (by intro b _; simp only [add_zero, hlik]) hv
  refine ⟨y, hy, φ q, φ p, (inv_lt_inv₀ hq.2 hp.2).mpr (hps.trans hqs), ?_, ?_⟩
  · ext v
    simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
    constructor
    · rintro ⟨hv, hm⟩
      rcases hall (φ v) (hdom v hv) ((hmap v hv).mp hm) with h | h
      · exact Or.inr (by simpa only [hinv] using congrArg φ h)
      · exact Or.inl (by simpa only [hinv] using congrArg φ h)
    · rintro (rfl | rfl)
      · exact ⟨hdom q hq, (hmap _ (hdom q hq)).mpr (by simpa only [hinv] using hqm)⟩
      · exact ⟨hdom p hp, (hmap _ (hdom p hp)).mpr (by simpa only [hinv] using hpm)⟩
  · rw [Transfer.ParetoII.bestShiftedExp, hexc, hlik (φ p), hinv]
    exact hexp

end
end UnivariateML.TiesLomax

end
