module

public import Mathlib.Analysis.SpecialFunctions.Gamma.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Continuity
public import Mathlib.Topology.Order.LeftRightNhds

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Feller–Pareto threshold spike

The interior log likelihood is expressed in gaps from the sample minimum.
The fixed log normalizer can be any finite real constant. This also applies
to the log-logistic and Burr specializations.
-/

@[expose] public section

open Filter Real Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2FellerPareto

noncomputable section

variable {n : ℕ}

/-- Log likelihood at threshold gap `t`, using lower and upper powers
`a` and `b`, transformation power `c`, and scale `s`. The constant `K`
is the per-observation log normalizer including the scale factor. -/
def logLik (d : Fin n → ℝ) (a b c s K t : ℝ) : ℝ :=
  ∑ i, (K + (a - 1) * log ((t + d i) / s) -
    ((a + b) / c) * log (1 + ((t + d i) / s) ^ c))

/-- A subunit lower power gives divergence as the threshold approaches the
minimum from below, with all ties at that minimum included. -/
theorem threshold_spike {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s K : ℝ} (ha : a < 1) (hc : 0 < c)
    (hs : 0 < s) :
    Tendsto (logLik d a b c s K) (𝓝[>] 0) atTop := by
  classical
  let A (i : Fin n) : ℝ := if d i = 0 then a - 1 else 0
  let R (i : Fin n) (t : ℝ) : ℝ :=
    K + (if d i = 0 then -(a - 1) * log s else (a - 1) * log ((t + d i) / s)) -
      ((a + b) / c) * log (1 + ((t + d i) / s) ^ c)
  have hA : (∑ i, A i) < 0 := by
    obtain ⟨j, hj⟩ := hz
    apply Finset.sum_neg'
    · intro i _
      by_cases hi : d i = 0
      · simpa only [A, ite_eq_left hi] using sub_nonpos.mpr ha.le
      · simp [A, hi]
    · exact ⟨j, Finset.mem_univ _, by simpa [A, hj] using sub_neg.mpr ha⟩
  have hR (i : Fin n) : ContinuousAt (R i) 0 := by
    have harg : ContinuousAt (fun t : ℝ ↦ (t + d i) / s) 0 :=
      (continuousAt_id.add continuousAt_const).div_const _
    have hpow : ContinuousAt (fun t : ℝ ↦ ((t + d i) / s) ^ c) 0 :=
      (continuousAt_rpow_const _ _ (Or.inr hc.le)).comp harg
    have htail := (continuousAt_const.add hpow).log
      (show 1 + ((0 + d i) / s) ^ c ≠ 0 by
        have := rpow_nonneg (div_nonneg (hd i) hs.le) c
        simp only [zero_add]
        linarith)
    have hbody : ContinuousAt (fun t : ℝ ↦
        if d i = 0 then -(a - 1) * log s else (a - 1) * log ((t + d i) / s)) 0 := by
      by_cases hi : d i = 0
      · simpa only [ite_eq_left hi] using
          (continuousAt_const (y := -(a - 1) * log s) (x := (0 : ℝ)))
      · simp only [ite_eq_right hi]
        exact continuousAt_const.mul
          (harg.log (by simpa using (div_pos (lt_of_le_of_ne (hd i) (Ne.symm hi)) hs).ne'))
    exact (continuousAt_const.add hbody).sub (continuousAt_const.mul htail)
  have hsum : Tendsto (fun t ↦ ∑ i, R i t) (𝓝[>] 0) (𝓝 (∑ i, R i 0)) :=
    tendsto_finsetSum Finset.univ fun i _ ↦ (hR i).continuousWithinAt
  apply Tendsto.congr' _ ((tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg hA).atTop_add hsum)
  filter_upwards [self_mem_nhdsWithin] with t ht
  have ht' : 0 < t := ht
  unfold logLik
  rw [Finset.sum_mul, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  by_cases hi : d i = 0
  · simp only [A, R, hi, ite_eq_left, add_zero, log_div ht'.ne' hs.ne']
    ring
  · simp [A, R, hi]

/-- Every model admitting the full subunit-power threshold path has an
unbounded objective, hence no finite maximizer. The ambient model may include
closed threshold contact and additional free parameters. -/
theorem unbounded_of_threshold_path {P : Type*} {D : Set P} {L : P → ℝ}
    {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    {a b c s K : ℝ} (ha : a < 1) (hc : 0 < c) (hs : 0 < s) (path : ℝ → P)
    (hpath : ∀ t > 0, path t ∈ D ∧ L (path t) = logLik d a b c s K t) :
    ¬ BddAbove (L '' D) ∧ ¬ ∃ p ∈ D, IsMaxOn L D p := by
  have hunbounded : ¬ BddAbove (L '' D) := by
    rintro ⟨M, hM⟩
    have hlarge := (threshold_spike hd hz (K := K) (b := b) ha hc hs).eventually_gt_atTop M
    have hpos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
    obtain ⟨t, hgt, ht⟩ := (hlarge.and hpos).exists
    obtain ⟨hD, hL⟩ := hpath t ht
    have hle : L (path t) ≤ M := hM (mem_image_of_mem L hD)
    exact hgt.not_ge (hL ▸ hle)
  refine ⟨hunbounded, ?_⟩
  rintro ⟨p, _, hp⟩
  apply hunbounded
  refine ⟨L p, ?_⟩
  rintro _ ⟨q, hq, rfl⟩
  exact hp hq

/-- At lower power one, closed contact uniquely maximizes over the threshold
gap for any fixed positive scale, power, and upper tail power. -/
theorem threshold_contact_unique (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {b c s K : ℝ} (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    ∃! t, t ∈ Ici (0 : ℝ) ∧ IsMaxOn (logLik d 1 b c s K) (Ici 0) t := by
  have hstrict : StrictAntiOn (logLik d 1 b c s K) (Ici 0) := by
    intro t ht u _ htu
    let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
    apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
    intro i _
    have hnonneg := div_nonneg (add_nonneg ht (hd i)) hs.le
    have hlt := (strictMonoOn_rpow_Ici_of_exponent_pos hc) hnonneg
      (div_nonneg (add_nonneg (ht.trans htu.le) (hd i)) hs.le)
      (div_lt_div_of_pos_right (by linarith) hs)
    change ((t + d i) / s) ^ c < ((u + d i) / s) ^ c at hlt
    have hlog : log (1 + ((t + d i) / s) ^ c) < log (1 + ((u + d i) / s) ^ c) :=
      log_lt_log (by linarith [rpow_nonneg hnonneg c]) (by linarith)
    have hcoef : 0 < (1 + b) / c := by positivity
    simp only [sub_self, zero_mul, add_zero]
    linarith [mul_lt_mul_of_pos_left hlog hcoef]
  refine ⟨0, ⟨le_rfl, ?_⟩, ?_⟩
  · intro q hq
    rcases eq_or_lt_of_le hq with rfl | hlt
    · exact le_rfl
    · exact (hstrict (show (0 : ℝ) ∈ Ici 0 from le_rfl) hq hlt).le
  · intro q hq
    by_contra! hne
    exact (hstrict (show (0 : ℝ) ∈ Ici 0 from le_rfl) hq.1
      (lt_of_le_of_ne hq.1 (Ne.symm hne))).not_ge (hq.2 le_rfl)

/-- Interior log likelihood in the dossier's shape parameters, with `p`
the lower beta shape and `q` the upper beta shape. -/
def interiorLogLik (d : Fin n → ℝ) (p q c s t : ℝ) : ℝ :=
  logLik d (c * p) (c * q) c s
    (log c - log s - (log (Gamma p) + log (Gamma q) - log (Gamma (p + q)))) t

/-- If either the lower beta shape or the power is free, the finite interior
log likelihood is unbounded with any valid holds on scale and upper shape.
The parameter `t` is the positive gap between threshold and sample minimum. -/
theorem free_lower_shape_or_power {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    (P C : Option ℝ) (hfree : P = none ∨ C = none)
    (hP : 0 < P.getD 1) (hC : 0 < C.getD 1) {q s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    ∀ M : ℝ, ∃ p q' c s' t : ℝ,
      0 < p ∧ P.getD p = p ∧ 0 < q' ∧ q' = q ∧ 0 < c ∧ C.getD c = c ∧
        s' = s ∧ 0 < t ∧ M < interiorLogLik d p q' c s' t := by
  have hchoose : ∃ p c : ℝ, 0 < p ∧ P.getD p = p ∧ 0 < c ∧ C.getD c = c ∧ c * p < 1 := by
    rcases hfree with h | h
    · subst P
      refine ⟨1 / (2 * C.getD 1), C.getD 1, by positivity, rfl, hC, ?_, ?_⟩
      · cases C <;> rfl
      · field_simp [hC.ne']
        linarith
    · subst C
      refine ⟨P.getD 1, 1 / (2 * P.getD 1), hP, ?_, by positivity, rfl, ?_⟩
      · cases P <;> rfl
      · field_simp [hP.ne']
        linarith
  obtain ⟨p, c, hp, hPp, hc, hCc, hsub⟩ := hchoose
  intro M
  have hlarge := (threshold_spike hd hz (b := c * q)
    (K := log c - log s - (log (Gamma p) + log (Gamma q) - log (Gamma (p + q))))
    hsub hc hs).eventually_gt_atTop M
  have hpos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
  obtain ⟨t, hM, ht⟩ := (hlarge.and hpos).exists
  exact ⟨p, q, c, s, t, hp, hPp, hq, rfl, hc, hCc, rfl, ht, hM⟩

/-- For a constant positive excess, keep its scale and both beta shapes fixed.
The ratio of every excess to the scale is one, so increasing the power adds
exactly `n * log c` to the log likelihood. -/
theorem constant_excess_power_path (hn : 0 < n) {s : ℝ} (hs : 0 < s)
    (p q B : ℝ) :
    Tendsto
      (fun c ↦ logLik (fun _ : Fin n ↦ s) (c * p) (c * q) c s
        (log c - log s - B) 0)
      atTop atTop := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  have hlim := (tendsto_log_atTop.const_mul_atTop hnR).atTop_add
    (tendsto_const_nhds (x := (n : ℝ) * (-log s - B - (p + q) * log 2)))
  apply Tendsto.congr' _ hlim
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
  have hratio : (c * p + c * q) / c = p + q := by
    rw [← mul_add, mul_comm c, mul_div_cancel_right₀ _ hc.ne']
  simp only [logLik, zero_add, div_self hs.ne', log_one, mul_zero, add_zero, one_rpow,
    hratio, one_add_one_eq_two, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul]
  ring

private theorem log_one_add_rpow {r c : ℝ} (hr : 0 < r) :
    log (1 + r ^ c) = c * log r + log (1 + r⁻¹ ^ c) := by
  have hp := rpow_pos_of_pos hr c
  rw [inv_rpow hr.le, ← log_rpow hr c, ← log_mul hp.ne'
    (add_pos zero_lt_one (inv_pos.mpr hp)).ne']
  congr 1
  rw [mul_add, mul_one, mul_inv_cancel₀ hp.ne', add_comm]

/-- Along the joint scale and threshold ray `t = s`, a minimum contributes
coefficient `-1` and a larger observation contributes `b` to the leading
`log s` term. A negative sum gives divergence, including constant samples. -/
theorem scale_collapse {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c K : ℝ} (hc : 0 < c)
    (hE : (∑ i, if d i = 0 then (-1 : ℝ) else b) < 0) :
    Tendsto (fun s ↦ logLik d a b c s (K - log s) s) (𝓝[>] 0) atTop := by
  classical
  let E (i : Fin n) : ℝ := if d i = 0 then -1 else b
  let R (i : Fin n) (s : ℝ) : ℝ :=
    if d i = 0 then K - ((a + b) / c) * log 2
    else K - (b + 1) * log (s + d i) -
      ((a + b) / c) * log (1 + (s / (s + d i)) ^ c)
  have hR (i : Fin n) : ContinuousAt (R i) 0 := by
    by_cases hi : d i = 0
    · simpa only [R, ite_eq_left hi] using
        (continuousAt_const (y := K - ((a + b) / c) * log 2) (x := (0 : ℝ)))
    · have hd' : 0 < d i := lt_of_le_of_ne (hd i) (Ne.symm hi)
      have hy : ContinuousAt (fun s : ℝ ↦ s + d i) 0 :=
        continuousAt_id.add continuousAt_const
      have hr := continuousAt_id.div hy (show 0 + d i ≠ 0 by simpa using hd'.ne')
      have hp := (continuousAt_rpow_const _ c (Or.inr hc.le)).comp hr
      have hl := (continuousAt_const.add hp).log
        (show 1 + (0 / (0 + d i)) ^ c ≠ 0 by simp [zero_rpow hc.ne'])
      simp only [R, ite_eq_right hi]
      exact (continuousAt_const.sub
        (continuousAt_const.mul (hy.log (by simpa using hd'.ne')))).sub
        (continuousAt_const.mul hl)
  have hsum : Tendsto (fun t ↦ ∑ i, R i t) (𝓝[>] 0) (𝓝 (∑ i, R i 0)) :=
    tendsto_finsetSum Finset.univ fun i _ ↦ (hR i).continuousWithinAt
  apply Tendsto.congr' _
    ((tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg (show (∑ i, E i) < 0 from hE)).atTop_add hsum)
  filter_upwards [self_mem_nhdsWithin] with s hs
  have hs' : 0 < s := hs
  unfold logLik
  rw [Finset.sum_mul, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  by_cases hi : d i = 0
  · simp only [E, R, hi, ite_eq_left, add_zero, div_self hs'.ne', log_one, mul_zero, one_rpow,
      one_add_one_eq_two]
    ring
  · have hy := add_pos_of_pos_of_nonneg hs' (hd i)
    rw [log_one_add_rpow (div_pos hy hs'), inv_div, log_div hy.ne' hs'.ne']
    simp only [E, R, ite_eq_right hi]
    field_simp [hc.ne']
    ring

private theorem continuousAt_logGamma {z : ℝ} (hz : 0 < z) :
    ContinuousAt (fun v ↦ log (Gamma v)) z :=
  (differentiableOn_Gamma_Ioi.continuousOn.continuousAt (Ioi_mem_nhds hz)).log
    (Gamma_pos_of_pos hz).ne'

/-- With scale and upper beta shape free, the simultaneous path
`q = 1/T`, `s = t = exp(-T)` diverges with arbitrary fixed positive
lower beta shape and power. Ties at the minimum contribute individually. -/
theorem vanishing_scale_upper_shape {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    {p c : ℝ} (hp : 0 < p) (hc : 0 < c) :
    Tendsto (fun T ↦ interiorLogLik d p T⁻¹ c (exp (-T)) (exp (-T))) atTop atTop := by
  classical
  let I (i : Fin n) : ℝ := if d i = 0 then 1 else 0
  let k : ℝ := ∑ i, I i
  have hk : 0 < k := by
    obtain ⟨i, hi⟩ := hz
    apply Finset.sum_pos'
    · intro j _
      dsimp [I]
      split <;> positivity
    · exact ⟨i, Finset.mem_univ _, by simp [I, hi]⟩
  let H (T : ℝ) : ℝ :=
    log c - log (Gamma p) - log (Gamma (T⁻¹ + 1)) + log (Gamma (p + T⁻¹))
  let R (i : Fin n) (T : ℝ) : ℝ :=
    H T - if d i = 0 then (p + T⁻¹) * log 2
      else c + (c * T⁻¹ + 1) * log (exp (-T) + d i) +
        (p + T⁻¹) * log (1 + (exp (-T) / (exp (-T) + d i)) ^ c)
  have hH : Tendsto H atTop (𝓝 (log c)) := by
    have h₁ := (continuousAt_logGamma (z := 1) zero_lt_one).tendsto.comp
      (show Tendsto (fun T : ℝ ↦ T⁻¹ + 1) atTop (𝓝 1) by
        simpa using tendsto_inv_atTop_zero.add (tendsto_const_nhds (x := (1 : ℝ))))
    have h₂ := (continuousAt_logGamma hp).tendsto.comp
      (show Tendsto (fun T : ℝ ↦ p + T⁻¹) atTop (𝓝 p) by
        simpa using (tendsto_const_nhds (x := p)).add tendsto_inv_atTop_zero)
    simp only [Gamma_one, log_one] at h₁
    simpa [H, Function.comp_def] using
      ((tendsto_const_nhds (x := log c - log (Gamma p))).sub h₁).add h₂
  let R₀ (i : Fin n) : ℝ :=
    log c - if d i = 0 then p * log 2 else c + log (d i)
  have hR (i : Fin n) : Tendsto (R i) atTop (𝓝 (R₀ i)) := by
    have hq : Tendsto (fun T : ℝ ↦ T⁻¹) atTop (𝓝 0) := tendsto_inv_atTop_zero
    by_cases hi : d i = 0
    · simpa [R, R₀, hi] using hH.sub
        (((tendsto_const_nhds (x := p)).add hq).mul_const (log 2))
    · have hdi : 0 < d i := lt_of_le_of_ne (hd i) (Ne.symm hi)
      have hsum := tendsto_exp_neg_atTop_nhds_zero.add (tendsto_const_nhds (x := d i))
      rw [zero_add] at hsum
      have hratio := tendsto_exp_neg_atTop_nhds_zero.div hsum hdi.ne'
      rw [zero_div] at hratio
      have hpow := (continuousAt_rpow_const 0 c (Or.inr hc.le)).tendsto.comp hratio
      rw [zero_rpow hc.ne'] at hpow
      have htail := (continuousAt_log (one_ne_zero : (1 : ℝ) ≠ 0)).tendsto.comp
        (show Tendsto (fun T : ℝ ↦ 1 + (exp (-T) / (exp (-T) + d i)) ^ c)
          atTop (𝓝 1) by
          simpa [Function.comp_def] using (tendsto_const_nhds (x := (1 : ℝ))).add hpow)
      have hbody := (continuousAt_log hdi.ne').tendsto.comp hsum
      simpa [R, R₀, hi] using hH.sub
        (((tendsto_const_nhds (x := c)).add
          (((hq.const_mul c).add (tendsto_const_nhds (x := (1 : ℝ)))).mul hbody)).add
          (((tendsto_const_nhds (x := p)).add hq).mul htail))
  have hlog : Tendsto (fun T : ℝ ↦ log T / T) atTop (𝓝 0) := by
    simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero
  have hlead : Tendsto (fun T : ℝ ↦ k * T - (n : ℝ) * log T) atTop atTop := by
    have hsmall : ∀ᶠ T in atTop, (n : ℝ) * (log T / T) < k / 2 :=
      (hlog.const_mul (n : ℝ)).eventually (Iio_mem_nhds (by simpa using half_pos hk))
    apply tendsto_atTop_mono' _ _ (tendsto_id.const_mul_atTop (half_pos hk))
    filter_upwards [hsmall, eventually_gt_atTop (0 : ℝ)] with T hT hpos
    have hcross := mul_lt_mul_of_pos_right hT hpos
    rw [mul_assoc, div_mul_cancel₀ _ hpos.ne'] at hcross
    change k / 2 * T ≤ k * T - (n : ℝ) * log T
    linarith
  apply Tendsto.congr' _ (hlead.atTop_add (tendsto_finsetSum Finset.univ fun i _ ↦ hR i))
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with T hT
  have hG : log (Gamma T⁻¹) = log (Gamma (T⁻¹ + 1)) + log T := by
    rw [Gamma_add_one (inv_ne_zero hT.ne'),
      log_mul (inv_ne_zero hT.ne') (Gamma_pos_of_pos (inv_pos.mpr hT)).ne', log_inv]
    ring
  have hcs : (c * p + c * T⁻¹) / c = p + T⁻¹ := by
    rw [← mul_add, mul_comm c, mul_div_cancel_right₀ _ hc.ne']
  have hden : (exp (-T)) ≠ 0 := (exp_pos _).ne'
  unfold interiorLogLik logLik
  rw [show k * T - (n : ℝ) * log T + (∑ i, R i T) =
      ∑ i, (I i * T - log T + R i T) by
    simp [k, Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_mul, nsmul_eq_mul]]
  apply Finset.sum_congr rfl
  intro i _
  rw [hcs, hG, log_exp]
  by_cases hi : d i = 0
  · simp [I, R, H, hi, hden]
    ring_nf
  · have hy := add_pos_of_pos_of_nonneg (exp_pos (-T)) (hd i)
    rw [log_one_add_rpow (div_pos hy (exp_pos _)), inv_div,
      log_div hy.ne' hden, log_exp]
    simp only [I, R, H, ite_eq_right hi, zero_mul]
    field_simp [hT.ne']
    ring

/-- Feller–Pareto coefficients, replacing threshold by the gap from the
sample minimum. -/
structure ThresholdParameters where
  gap : ℝ
  lowerShape : ℝ
  upperShape : ℝ
  power : ℝ
  scale : ℝ

/-- The positive finite likelihood domain with optional regular-parameter
holds. The threshold can touch the minimum at lower power one. -/
def thresholdDomain (P Q C S : Option ℝ) : Set ThresholdParameters :=
  {v | (0 < v.gap ∨ (v.gap = 0 ∧ v.power * v.lowerShape = 1)) ∧
    0 < v.lowerShape ∧ P.getD v.lowerShape = v.lowerShape ∧
    0 < v.upperShape ∧ Q.getD v.upperShape = v.upperShape ∧
    0 < v.power ∧ C.getD v.power = v.power ∧
    0 < v.scale ∧ S.getD v.scale = v.scale}

/-- All structural free-threshold masks have unbounded likelihood and
an empty finite maximizing set. This uses either a subunit lower power
or the simultaneous vanishing upper-shape and scale path. -/
theorem structural_unbounded {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) (P Q C S : Option ℝ)
    (hP : 0 < P.getD 1) (hQ : 0 < Q.getD 1) (hC : 0 < C.getD 1) (hS : 0 < S.getD 1)
    (hfree : (P = none ∨ C = none) ∨ (Q = none ∧ S = none)) :
    let L := fun v : ThresholdParameters ↦
      interiorLogLik d v.lowerShape v.upperShape v.power v.scale v.gap
    ¬ BddAbove (L '' thresholdDomain P Q C S) ∧
      ¬ ∃ v ∈ thresholdDomain P Q C S, IsMaxOn L (thresholdDomain P Q C S) v := by
  intro L
  have hu : ¬ BddAbove (L '' thresholdDomain P Q C S) := by
    rintro ⟨M, hM⟩
    rcases hfree with h | ⟨hQ', hS'⟩
    · obtain ⟨p,q,c,s,t,hp,hp',hq,hq',hc,hc',hs',ht,hgt⟩ :=
        free_lower_shape_or_power hd hz P C h hP hC hQ hS M
      have hq'' : Q.getD q = q := by rw [hq']; cases Q <;> rfl
      have hs'' : S.getD s = s := by rw [hs']; cases S <;> rfl
      have hdom : ⟨t,p,q,c,s⟩ ∈ thresholdDomain P Q C S :=
        ⟨Or.inl ht, hp, hp', hq, hq'', hc, hc', hs' ▸ hS, hs''⟩
      exact hgt.not_ge (hM (mem_image_of_mem L hdom))
    · subst Q
      subst S
      have hlarge := (vanishing_scale_upper_shape hd hz hP hC).eventually_gt_atTop M
      obtain ⟨T, hgt, hT⟩ := (hlarge.and (eventually_gt_atTop (0 : ℝ))).exists
      have hp : P.getD (P.getD 1) = P.getD 1 := by cases P <;> rfl
      have hc : C.getD (C.getD 1) = C.getD 1 := by cases C <;> rfl
      have hdom : ⟨exp (-T),P.getD 1,T⁻¹,C.getD 1,exp (-T)⟩ ∈ thresholdDomain P none C none :=
        ⟨Or.inl (exp_pos _), hP, hp, inv_pos.mpr hT, rfl, hC, hc, exp_pos _, rfl⟩
      have hbound : L ⟨exp (-T),P.getD 1,T⁻¹,C.getD 1,exp (-T)⟩ ≤ M :=
        hM (mem_image_of_mem L hdom)
      change M < L ⟨exp (-T),P.getD 1,T⁻¹,C.getD 1,exp (-T)⟩ at hgt
      exact hgt.not_ge hbound
  exact ⟨hu, by rintro ⟨v, _, hm⟩; exact hu hm.bddAbove⟩

end

end UnivariateML.Endpoint2FellerPareto

end
