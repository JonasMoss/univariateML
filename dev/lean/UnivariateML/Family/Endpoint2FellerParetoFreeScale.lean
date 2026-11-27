module

public import UnivariateML.Family.Endpoint2FellerParetoRegular
public import UnivariateML.Generic.Limit

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Fixed regular shape coefficients with threshold and scale free

A scale-zero expansion separates observations at the sample minimum
from positive gaps. The signed exponent decides divergence, a strict
finite upper envelope, or attainment in a compact level set.
-/

@[expose] public section

open Filter Real Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2FellerParetoFreeScale

noncomputable section

variable {n : ℕ}

/-- The exponent E in the scale-collapse expansion. -/
def exponent (d : Fin n → ℝ) (b : ℝ) : ℝ :=
  ∑ i, if d i = 0 then -1 else b

/-- The scale and threshold gap domain, including finite positive
closed contact at lower power one. -/
def domain (a : ℝ) : Set (ℝ × ℝ) := Ioi 0 ×ˢ (if a = 1 then Ici 0 else Ioi 0)

private def kernel (a b c r : ℝ) : ℝ :=
  (a - 1) * log r - ((a + b) / c) * log (1 + r ^ c)

/-- Threshold log likelihood with free scale. The constant includes the
shape normalizer and the power term. -/
def logLik (d : Fin n → ℝ) (a b c K : ℝ) (v : ℝ × ℝ) : ℝ :=
  Endpoint2FellerPareto.logLik d a b c v.1 (K - log v.1) v.2

private theorem log_form (d : Fin n → ℝ) (a b c K : ℝ) (v : ℝ × ℝ) :
    logLik d a b c K v = ∑ i, (K - log v.1 + kernel a b c ((v.2 + d i) / v.1)) := by
  unfold logLik Endpoint2FellerPareto.logLik kernel
  apply Finset.sum_congr rfl
  intro i _
  ring

private theorem domain_nonneg {a : ℝ} {v : ℝ × ℝ} (hv : v ∈ domain a) : 0 ≤ v.2 := by
  have ht := hv.2
  by_cases he : a = 1
  · simpa only [he, ite_true, mem_Ici] using ht
  · exact (show 0 < v.2 by simpa only [he, ite_false, mem_Ioi] using ht).le

private theorem domain_pos {a : ℝ} {v : ℝ × ℝ} (ha : a ≠ 1) (hv : v ∈ domain a) : 0 < v.2 :=
  by simpa only [ha, ite_false, mem_Ioi] using hv.2

private theorem tail_identity {r c : ℝ} (hr : 0 < r) :
    log (1 + r ^ c) = c * log r + log (1 + r⁻¹ ^ c) := by
  have hp := rpow_pos_of_pos hr c
  rw [inv_rpow hr.le, ← log_rpow hr c, ← log_mul hp.ne'
    (add_pos one_pos (inv_pos.mpr hp)).ne']
  congr 1
  rw [mul_add, mul_one, mul_inv_cancel₀ hp.ne', add_comm]

private theorem tail_form {a b c r : ℝ} (hc : 0 < c) (hr : 0 < r) :
    kernel a b c r = -(b + 1) * log r - ((a + b) / c) * log (1 + r⁻¹ ^ c) := by
  unfold kernel
  rw [tail_identity hr]
  field_simp [hc.ne']
  ring

private theorem kernel_nonpos {a b c r : ℝ} (ha : 1 ≤ a) (hb : 0 < b) (hc : 0 < c)
    (hr : 0 ≤ r) : kernel a b c r ≤ 0 := by
  have hab : 0 ≤ (a + b) / c := by positivity
  by_cases h : r ≤ 1
  · unfold kernel
    exact sub_nonpos.mpr (le_trans
      (mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr ha) (log_nonpos hr h))
      (mul_nonneg hab (log_nonneg (by linarith [rpow_nonneg hr c]))))
  · have hr' : 0 < r := zero_lt_one.trans (lt_of_not_ge h)
    rw [tail_form hc hr']
    exact sub_nonpos.mpr (le_trans
      (mul_nonpos_of_nonpos_of_nonneg (by linarith : -(b+1) ≤ 0) (log_nonneg (le_of_not_ge h)))
      (mul_nonneg hab (log_nonneg (by linarith [rpow_nonneg (inv_nonneg.mpr hr) c]))))

private theorem log_kernel_nonpos {a b c r : ℝ} (ha : 1 ≤ a) (hb : 0 < b) (hc : 0 < c)
    (hr : 0 < r) : log r + kernel a b c r ≤ 0 := by
  have hab : 0 ≤ (a + b) / c := by positivity
  by_cases h : r ≤ 1
  · unfold kernel
    linarith [mul_nonpos_of_nonneg_of_nonpos (show 0 ≤ a by linarith) (log_nonpos hr.le h),
      mul_nonneg hab (log_nonneg (show 1 ≤ 1 + r ^ c by linarith [rpow_nonneg hr.le c]))]
  · rw [tail_form hc hr]
    linarith [mul_nonneg hb.le (log_nonneg (le_of_not_ge h)),
      mul_nonneg hab (log_nonneg (show 1 ≤ 1 + r⁻¹ ^ c by
        linarith [rpow_nonneg (inv_nonneg.mpr hr.le) c]))]

private def contactValue (d : Fin n → ℝ) (a b c K r : ℝ) : ℝ :=
  ∑ i, if d i = 0 then K + kernel a b c r else K - (b+1)*log (d i)

private def remainder (d : Fin n → ℝ) (a b c K r s : ℝ) : ℝ :=
  ∑ i, if d i = 0 then K + kernel a b c r
    else K - (b+1)*log (r*s+d i) - ((a+b)/c)*log (1 + (s/(r*s+d i)) ^ c)

private theorem ray_expansion {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c K r s : ℝ} (hc : 0 < c) (hr : 0 ≤ r) (hs : 0 < s) :
    logLik d a b c K (s,r*s) = exponent d b * log s + remainder d a b c K r s := by
  classical
  rw [log_form]
  unfold exponent remainder
  rw [Finset.sum_mul, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  by_cases hi : d i = 0
  · simp only [hi, add_zero, ite_true, mul_div_cancel_right₀ _ hs.ne']
    ring
  · have hdi : 0 < d i := lt_of_le_of_ne (hd i) (Ne.symm hi)
    have hy : 0 < r*s + d i := add_pos_of_nonneg_of_pos (mul_nonneg hr hs.le) hdi
    simp only [hi, ite_false]
    rw [tail_form hc (div_pos hy hs), log_div hy.ne' hs.ne', inv_div]
    ring

private theorem remainder_limit {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c K r : ℝ} (hc : 0 < c) :
    Tendsto (remainder d a b c K r) (𝓝[>] 0) (𝓝 (contactValue d a b c K r)) := by
  classical
  apply tendsto_finsetSum Finset.univ
  intro i _
  by_cases hi : d i = 0
  · simp [hi]
  · have hdi : 0 < d i := lt_of_le_of_ne (hd i) (Ne.symm hi)
    simp only [hi, ite_false]
    have hy : ContinuousAt (fun s : ℝ ↦ r*s+d i) 0 := by fun_prop
    have hratio := continuousAt_id.div hy (show r*0+d i ≠ 0 by simpa using hdi.ne')
    have hp : ContinuousAt (fun s : ℝ ↦ (s/(r*s+d i)) ^ c) 0 :=
      (continuousAt_rpow_const _ c (Or.inr hc.le)).comp' hratio
    have hlog := ((continuousAt_const (y := (1 : ℝ))).add hp).log
      (show 1 + (0 / (r*0+d i)) ^ c ≠ 0 by simp [zero_rpow hc.ne'])
    have hfirst : ContinuousAt (fun s : ℝ ↦ K - (b+1)*log (r*s+d i)) 0 :=
      continuousAt_const.sub (continuousAt_const.mul (hy.log (by simpa using hdi.ne')))
    have hR : ContinuousAt (fun s : ℝ ↦ K - (b+1)*log (r*s+d i) -
          ((a+b)/c)*log (1 + (s/(r*s+d i)) ^ c)) 0 :=
      hfirst.sub (continuousAt_const.mul hlog)
    simpa [ContinuousWithinAt, zero_rpow hc.ne'] using
      hR.continuousWithinAt (s := Ioi 0)

private theorem remainder_upper {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c K r s : ℝ} (hab : 0 < a + b) (hb : 0 < b) (hc : 0 < c) (hr : 0 ≤ r) (hs : 0 < s) :
    remainder d a b c K r s ≤ contactValue d a b c K r ∧
      ((∃ i, 0 < d i) → remainder d a b c K r s < contactValue d a b c K r) := by
  classical
  have hterm (i : Fin n) (hi : d i ≠ 0) :
      K - (b+1)*log (r*s+d i) - ((a+b)/c)*log (1+(s/(r*s+d i)) ^ c) <
        K - (b+1)*log (d i) := by
    have hdi : 0 < d i := lt_of_le_of_ne (hd i) (Ne.symm hi)
    have hy : 0 < r*s+d i := add_pos_of_nonneg_of_pos (mul_nonneg hr hs.le) hdi
    have hlog := log_le_log hdi (show d i ≤ r*s+d i by linarith [mul_nonneg hr hs.le])
    have hp := log_pos (show 1 < 1 + (s/(r*s+d i)) ^ c by
      linarith [rpow_pos_of_pos (div_pos hs hy) c])
    linarith [mul_le_mul_of_nonneg_left hlog (show 0 ≤ b+1 by positivity),
      mul_pos (div_pos hab hc) hp]
  have h (i : Fin n) :
      (if d i = 0 then K + kernel a b c r
        else K - (b+1)*log (r*s+d i) - ((a+b)/c)*log (1+(s/(r*s+d i)) ^ c)) ≤
      (if d i = 0 then K + kernel a b c r else K - (b+1)*log (d i)) := by
    by_cases hi : d i = 0
    · simp [hi]
    · simpa only [ite_eq_right hi] using (hterm i hi).le
  refine ⟨Finset.sum_le_sum (fun i _ ↦ h i), ?_⟩
  rintro ⟨i,hi⟩
  exact Finset.sum_lt_sum (fun i _ ↦ h i)
    ⟨i,Finset.mem_univ _,by simpa only [ite_eq_right hi.ne'] using hterm i hi.ne'⟩

private theorem has_positive_gap (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {b : ℝ} (h : 0 ≤ exponent d b) : ∃ i, 0 < d i := by
  by_contra! he
  have hz (i : Fin n) : d i = 0 := le_antisymm (he i) (hd i)
  have hnR : (0 : ℝ) < n := Nat.cast_pos.mpr hn
  simp [exponent,hz,nsmul_eq_mul] at h
  linarith

/-- At a zero scale-collapse exponent, the supremum equals the
sharp contact envelope and is strictly above all finite points. -/
theorem zero_exponent_envelope {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (ha : 1 ≤ a) (hb : 0 < b) (hc : 0 < c)
    (hE : exponent d b = 0) :
    ∃ C : ℝ, IsLUB (logLik d a b c K '' domain a) C ∧
      ∀ v ∈ domain a, logLik d a b c K v < C := by
  have hn : 0 < n := by obtain ⟨i,_⟩ := hz; exact i.isLt.pos
  have hab : 0 < a+b := by linarith
  let S := if a=1 then Ici (0 : ℝ) else Ioi 0
  have hf := (Endpoint2FellerParetoRegular.fixed_scale_shapes_fit
    (d := fun _ : Fin 1 ↦ 0) (fun _ ↦ le_rfl) ⟨0,rfl⟩
    (a := a) (b := b) (c := c) (K := 0) (s := 1) hb hc one_pos).1.mpr ha
  have hker : Endpoint2FellerPareto.logLik (fun _ : Fin 1 ↦ 0) a b c 1 0 = kernel a b c := by
    funext r
    simp [Endpoint2FellerPareto.logLik, kernel]
  rw [hker] at hf
  obtain ⟨r,hr,hm⟩ := hf
  have hn0 : 0 ≤ r := domain_nonneg (a := a) (v := (1,r)) ⟨one_pos,hr⟩
  have hpath_dom : ∀ᶠ s : ℝ in 𝓝[>] 0, (s,r*s) ∈ domain a := by
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
    refine ⟨hs, ?_⟩
    by_cases he : a = 1
    · simpa only [he, ite_true, mem_Ici] using mul_nonneg hn0 (show 0 < s from hs).le
    · have hp : 0 < r := by simpa only [he, ite_false, mem_Ioi] using hr
      simpa only [he, ite_false, mem_Ioi] using mul_pos hp (show 0 < s from hs)
  have hlim : Tendsto (fun s ↦ logLik d a b c K (s,r*s)) (𝓝[>] 0)
      (𝓝 (contactValue d a b c K r)) := by
    apply (remainder_limit hd (a := a) (b := b) (K := K) (r := r) hc).congr'
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
    rw [ray_expansion hd hc hn0 hs, hE, zero_mul, zero_add]
  have hlt (v : ℝ × ℝ) (hv : v ∈ domain a) : logLik d a b c K v < contactValue d a b c K r := by
    let u := v.2 / v.1
    have hs : 0 < v.1 := hv.1
    have hu0 : 0 ≤ u := div_nonneg (domain_nonneg hv) hs.le
    have huS : u ∈ S := by
      by_cases he : a = 1
      · simpa [S,he] using hu0
      · simpa [S,he] using div_pos (domain_pos he hv) hs
    have he : u * v.1 = v.2 := div_mul_cancel₀ _ hs.ne'
    have H := ray_expansion (a := a) (b := b) (K := K) hd hc hu0 hs
    rw [he,hE,zero_mul,zero_add] at H
    apply (H.trans_lt ((remainder_upper hd hab hb hc hu0 hs).2
      (has_positive_gap hn hd hE.ge))).trans_le
    unfold contactValue
    apply Finset.sum_le_sum
    intro i _
    by_cases hi : d i = 0
    · simp only [hi, ite_true]
      have h : kernel a b c u ≤ kernel a b c r := hm huS
      linarith
    · simp [hi]
  exact ⟨contactValue d a b c K r,
    ⟨by rintro _ ⟨v,hv,rfl⟩; exact (hlt v hv).le,
      fun _ h ↦ Generic.limit_le_upperBound hpath_dom hlim h⟩, hlt⟩

/-- A negative collapse exponent gives unbounded likelihood in the
positive finite domain, including constant samples. -/
theorem negative_exponent_unbounded {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c K : ℝ} (hc : 0 < c) (hE : exponent d b < 0) :
    ¬ BddAbove (logLik d a b c K '' domain a) := by
  rintro ⟨M,hM⟩
  have h := Endpoint2FellerPareto.scale_collapse (a := a) (K := K) hd hc hE
  have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
  obtain ⟨s,hgt,hs'⟩ := ((h.eventually_gt_atTop M).and hs).exists
  have hD : (s,s) ∈ domain a := ⟨hs', by
    by_cases he : a = 1
    · simpa [he] using hs'.le
    · simpa [he] using hs'⟩
  exact hgt.not_ge (hM (mem_image_of_mem (logLik d a b c K) hD))

private theorem positive_exponent_attainment {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (ha : 1 ≤ a) (hb : 0 < b) (hc : 0 < c)
    (hE : 0 < exponent d b) :
    ∃ v ∈ domain a, IsMaxOn (logLik d a b c K) (domain a) v := by
  obtain ⟨j,hj⟩ := hz
  let L := logLik d a b c K
  let N : ℝ := n
  let E := exponent d b
  let C := L (1,1)
  let M := contactValue d a b c K 0
  have hN : 0 < N := Nat.cast_pos.mpr j.isLt.pos
  have hab : 0 < a+b := by linarith
  have hSlarge (v : ℝ × ℝ) (hv : v ∈ domain a) : L v ≤ N * (K - log v.1) := by
    unfold L
    rw [log_form]
    calc
      _ ≤ ∑ _ : Fin n, (K - log v.1) := by
        apply Finset.sum_le_sum
        intro i _
        have hj := kernel_nonpos ha hb hc
          (div_nonneg (add_nonneg (domain_nonneg hv) (hd i)) hv.1.le)
        linarith
      _ = _ := by simp [N, nsmul_eq_mul, mul_sub]
  have hTlarge (v : ℝ × ℝ) (hv : v ∈ domain a) (ht : 0 < v.2) :
      L v ≤ N * (K - log v.2) := by
    unfold L
    rw [log_form]
    calc
      _ ≤ ∑ _ : Fin n, (K - log v.2) := by
        apply Finset.sum_le_sum
        intro i _
        have hy := add_pos_of_pos_of_nonneg ht (hd i)
        have H := log_kernel_nonpos ha hb hc (div_pos hy hv.1)
        rw [log_div hy.ne' hv.1.ne'] at H
        linarith [log_le_log ht (show v.2 ≤ v.2 + d i by linarith [hd i])]
      _ = _ := by simp [N, nsmul_eq_mul, mul_sub]
  have hSsmall (v : ℝ × ℝ) (hv : v ∈ domain a) : L v ≤ E * log v.1 + M := by
    let r := v.2 / v.1
    have hs : 0 < v.1 := hv.1
    have hr : 0 ≤ r := div_nonneg (domain_nonneg hv) hs.le
    have he : r * v.1 = v.2 := div_mul_cancel₀ _ hs.ne'
    have H := ray_expansion (a := a) (b := b) (K := K) hd hc hr hs
    rw [he] at H
    unfold L
    rw [H]
    apply add_le_add_right
    apply (remainder_upper hd hab hb hc hr hs).1.trans
    unfold contactValue M
    apply Finset.sum_le_sum
    intro i _
    by_cases hi : d i = 0
    · have h0 : kernel a b c 0 = 0 := by simp [kernel,zero_rpow hc.ne']
      simp only [hi, ite_true, h0, add_zero]
      linarith [kernel_nonpos ha hb hc hr]
    · simp [hi]
  let A := exp ((C - 1 - M) / E)
  let B := exp (K - (C - 1) / N)
  let T := if a = 1 then 0 else A * exp ((C - 1 - N * (K - log A)) / (a - 1))
  have hA : 0 < A := exp_pos _
  have hB : 0 < B := exp_pos _
  have hK (v : ℝ × ℝ) (hv : v ∈ domain a) (h : C ≤ L v) :
      v ∈ Icc A B ×ˢ Icc T B := by
    have hSA : A ≤ v.1 := by
      by_contra! he
      have hs := log_lt_log hv.1 he
      change log v.1 < log (exp ((C-1-M)/E)) at hs
      rw [log_exp, lt_div_iff₀ hE] at hs
      linarith [hSsmall v hv]
    have hSB : v.1 ≤ B := by
      by_contra! he
      have hs := log_lt_log hB he
      change log (exp (K-(C-1)/N)) < log v.1 at hs
      rw [log_exp] at hs
      have H' := mul_lt_mul_of_pos_left hs hN
      rw [mul_sub, mul_div_cancel₀ _ hN.ne'] at H'
      linarith [hSlarge v hv]
    have hTB : v.2 ≤ B := by
      by_contra! he
      have ho := log_lt_log hB he
      change log (exp (K-(C-1)/N)) < log v.2 at ho
      rw [log_exp] at ho
      have H := mul_lt_mul_of_pos_left ho hN
      rw [mul_sub, mul_div_cancel₀ _ hN.ne'] at H
      linarith [hTlarge v hv (hB.trans he)]
    have hTA : T ≤ v.2 := by
      by_cases he : a = 1
      · simpa [T,he] using domain_nonneg hv
      · have ht := domain_pos he hv
        have hsa : 0 < a-1 := sub_pos.mpr (lt_of_le_of_ne ha (Ne.symm he))
        have hbase := div_pos ht hA
        have hbound : L v ≤ N * (K - log A) + (a-1) * log (v.2/A) := by
          have hs : 0 < v.1 := hv.1
          have hl := log_le_log hA hSA
          have hrat := div_le_div_of_nonneg_left ht.le hA hSA
          have hsum : (∑ i, kernel a b c ((v.2+d i)/v.1)) ≤
              kernel a b c (v.2/v.1) := by
            classical
            have H (i : Fin n) : kernel a b c ((v.2+d i)/v.1) ≤
                if i=j then kernel a b c (v.2/v.1) else 0 := by
              by_cases hi : i=j
              · simp [hi,hj]
              · simp only [hi, ite_false]
                exact kernel_nonpos ha hb hc (div_nonneg (add_nonneg ht.le (hd i)) hs.le)
            simpa using Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ H i)
          have hk : kernel a b c (v.2/v.1) ≤ (a-1)*log (v.2/A) := by
            have hlog := log_le_log (div_pos ht hs) hrat
            have hnon := log_nonneg (show 1 ≤ 1 + (v.2/v.1) ^ c by
              linarith [rpow_nonneg (div_nonneg ht.le hs.le) c])
            unfold kernel
            linarith [mul_le_mul_of_nonneg_left hlog hsa.le, mul_nonneg (div_pos hab hc).le hnon]
          unfold L
          rw [log_form, Finset.sum_add_distrib]
          simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
          change N * (K - log v.1) + _ ≤ _
          linarith [hsum.trans hk, mul_le_mul_of_nonneg_left hl hN.le]
        by_contra! he'
        have hrat : v.2 / A < exp ((C-1-N*(K-log A))/(a-1)) :=
          (div_lt_iff₀ hA).mpr (by simpa [T,he,mul_comm] using he')
        have H := log_lt_log hbase hrat
        rw [log_exp, lt_div_iff₀ hsa] at H
        linarith
    exact ⟨⟨hSA,hSB⟩,⟨hTA,hTB⟩⟩
  let Q := Icc A B ×ˢ Icc T B
  have hbase : (1,1) ∈ domain a := ⟨one_pos, by by_cases he : a=1 <;> simp [he]⟩
  have hQ : Q ⊆ domain a := by
    intro v hv
    refine ⟨hA.trans_le hv.1.1,?_⟩
    by_cases he : a=1
    · have h : 0 ≤ v.2 := by simpa [T,he] using hv.2.1
      simpa [he] using h
    · have hT : 0 < T := by simp only [T,he,ite_false]; positivity
      simpa [he] using hT.trans_le hv.2.1
  have hcont : ContinuousOn L Q := by
    intro v hv
    apply ContinuousAt.continuousWithinAt
    have hD := hQ hv
    have hs : 0 < v.1 := hD.1
    rw [show L = (fun w : ℝ × ℝ ↦
      ∑ i, (K - log w.1 + kernel a b c ((w.2 + d i) / w.1))) from funext (log_form d a b c K)]
    apply tendsto_finsetSum Finset.univ
    intro i _
    have hr : ContinuousAt (fun v : ℝ × ℝ ↦ (v.2 + d i) / v.1) v :=
      (continuous_snd.continuousAt.add continuousAt_const).div continuous_fst.continuousAt hs.ne'
    have hnon := div_nonneg (add_nonneg (domain_nonneg hD) (hd i)) hs.le
    have hp := (continuousAt_rpow_const _ c (Or.inr hc.le)).comp' hr
    have ht : ContinuousAt (fun v : ℝ × ℝ ↦ log (1 + ((v.2+d i)/v.1)^c)) v :=
      (continuousAt_const.add hp).log (show 1 + ((v.2 + d i) / v.1) ^ c ≠ 0 from
        (add_pos_of_pos_of_nonneg one_pos (rpow_nonneg hnon c)).ne')
    have hhead : ContinuousAt (fun v : ℝ × ℝ ↦ K - log v.1) v :=
      continuousAt_const.sub (continuous_fst.continuousAt.log hs.ne')
    have hbody : ContinuousAt (fun v : ℝ × ℝ ↦ (a-1)*log ((v.2+d i)/v.1)) v := by
      by_cases he : a=1
      · simp [he,continuousAt_const]
      · exact continuousAt_const.mul (hr.log
          (div_pos (add_pos_of_pos_of_nonneg (domain_pos he hD) (hd i)) hs).ne')
    exact (hhead.add (hbody.sub (continuousAt_const.mul ht))).tendsto
  have hq : (1,1) ∈ Q := hK _ hbase le_rfl
  obtain ⟨v,hv,hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨_,hq⟩ hcont
  refine ⟨v,hQ hv,fun w hw ↦ ?_⟩
  by_cases he : w ∈ Q
  · exact hm he
  · exact (lt_of_not_ge (fun hh ↦ he (hK w hw hh))).le.trans (hm hq)

/-- A finite joint threshold and scale maximum exists exactly for
lower power at least one and positive scale-collapse exponent. -/
theorem fixed_shapes_criterion {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (hb : 0 < b) (hc : 0 < c) :
    (∃ v ∈ domain a, IsMaxOn (logLik d a b c K) (domain a) v) ↔
      1 ≤ a ∧ 0 < exponent d b := by
  constructor
  · rintro ⟨v,hv,hm⟩
    have ha : 1 ≤ a := by
      by_contra! he
      have hpath := Endpoint2FellerPareto.threshold_spike hd hz (K := K) (b := b) he hc one_pos
      have hpos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
      obtain ⟨t,hgt,ht⟩ := ((hpath.eventually_gt_atTop (logLik d a b c K v)).and hpos).exists
      have htD : (1,t) ∈ domain a := ⟨one_pos,by simpa [he.ne] using ht⟩
      have hle : logLik d a b c K (1,t) ≤ logLik d a b c K v := hm htD
      exact hgt.not_ge (by simpa [logLik] using hle)
    refine ⟨ha,?_⟩
    rcases lt_trichotomy (exponent d b) 0 with h | h | h
    · exact (negative_exponent_unbounded (a := a) (K := K) hd hc h hm.bddAbove).elim
    · obtain ⟨C,hC,hlt⟩ := zero_exponent_envelope (K := K) hd hz ha hb hc h
      exact ((hlt v hv).not_ge (hC.2 (by
        rintro _ ⟨u,hu,rfl⟩
        exact hm hu))).elim
    · exact h
  · rintro ⟨ha,he⟩
    exact positive_exponent_attainment hd hz ha hb hc he

end

end UnivariateML.Endpoint2FellerParetoFreeScale

end
