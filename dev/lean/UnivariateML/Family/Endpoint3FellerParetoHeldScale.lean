module

public import UnivariateML.Family.Endpoint3FellerPareto
public import UnivariateML.Family.Beta

import Mathlib.Tactic

set_option linter.style.header false

/-! # Threshold and upper shape at fixed scale and lower shape -/

@[expose] public section

open Real Filter Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint3FellerParetoHeldScale

noncomputable section

private theorem continuousAt_logGamma {x : ℝ} (hx : 0 < x) :
    ContinuousAt (fun y ↦ log (Gamma y)) x :=
  ((Real.differentiableAt_Gamma (fun m ↦ by
    have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
    linarith)).continuousAt).log (Gamma_pos_of_pos hx).ne'

private theorem upper_shape_tails {p A : ℝ} (hp : 0 < p) (hA : 0 < A) :
    let F := fun q : ℝ ↦ log (Gamma (p + q)) - log (Gamma q) - q * A
    Tendsto F (𝓝[>] 0) atBot ∧ Tendsto F atTop atBot := by
  intro F
  constructor
  · have h0 : Tendsto (fun q : ℝ ↦ q) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
    have h := (Endpoint3GammaRatio.log_gamma_ratio_at_zero hp).add_const (log (Gamma p))
    apply (tendsto_log_nhdsGT_zero.atBot_add (h.sub (h0.mul_const A))).congr
    intro q
    dsimp [F]
    ring
  · have hlog : Tendsto (fun q : ℝ ↦ log q / q) atTop (𝓝 0) := by
      simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero
    have he : ∀ᶠ q in atTop, p * log q ≤ (A / 2) * q := by
      filter_upwards [((hlog.const_mul p).eventually
        (Iio_mem_nhds (show p * 0 < A / 2 by simpa using half_pos hA))),
        eventually_gt_atTop (0 : ℝ)] with q hq hq₀
      change p * (log q / q) < A / 2 at hq
      rw [← mul_div_assoc] at hq
      exact ((div_lt_iff₀ hq₀).mp hq).le
    have hbound : ∀ᶠ q in atTop, F q ≤ -(A / 2) * q + 1 := by
      filter_upwards [he, (Endpoint3GammaRatio.log_gamma_ratio p hp).eventually
        (Iio_mem_nhds (by norm_num : (0 : ℝ) < 1))] with q hq hb
      change log (Gamma (q + p)) - log (Gamma q) - p * log q < 1 at hb
      dsimp [F]
      rw [add_comm p q]
      linarith
    exact tendsto_atBot_mono' _ hbound
      (tendsto_atBot_add_const_right _ _ (tendsto_id.const_mul_atTop_of_neg
        (neg_neg_of_pos (half_pos hA))))

private theorem upper_shape_max {p A : ℝ} (hp : 0 < p) (hA : 0 < A) :
    let F := fun q : ℝ ↦ log (Gamma (p + q)) - log (Gamma q) - q * A
    ∃ q ∈ Ioi (0 : ℝ), IsMaxOn F (Ioi 0) q := by
  intro F
  have h0 := (upper_shape_tails hp hA).1
  have htop := (upper_shape_tails hp hA).2
  obtain ⟨ε, hε, hlo⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp
    (h0.eventually_lt_atBot (F 1))
  obtain ⟨R, hhi⟩ := eventually_atTop.mp (htop.eventually_lt_atBot (F 1))
  let Q := Icc (min ε 1) (max R 1)
  have h1 : 1 ∈ Q := ⟨min_le_right _ _, le_max_right _ _⟩
  have hq₀ (q : ℝ) (hq : q ∈ Q) : 0 < q := (lt_min hε one_pos).trans_le hq.1
  have hc : ContinuousOn F Q := by
    intro q hq
    apply ContinuousAt.continuousWithinAt
    have h := hq₀ q hq
    exact (((continuousAt_logGamma (add_pos hp h)).comp'
      (continuousAt_const.add continuousAt_id)).sub (continuousAt_logGamma h)).sub
        (continuousAt_id.mul continuousAt_const)
  obtain ⟨q, hq, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1, h1⟩ hc
  refine ⟨q, hq₀ q hq, fun u hu ↦ ?_⟩
  by_cases hul : u < min ε 1
  · exact (hlo ⟨hu, hul.trans_le (min_le_left _ _)⟩).le.trans (hm h1)
  by_cases hur : max R 1 < u
  · exact (hhi u ((le_max_left _ _).trans hur.le)).le.trans (hm h1)
  exact hm ⟨le_of_not_gt hul, le_of_not_gt hur⟩

variable {n : ℕ}

/-- If the sample is nonconstant and the lower power is at least one,
the threshold and upper shape have a finite maximizer. The unit lower
power allows closed threshold contact. -/
theorem regular_fit {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    (hne : ∃ i, 0 < d i) {p c s : ℝ} (hp : 0 < p) (hc : 0 < c) (hs : 0 < s)
    (ha : 1 ≤ c * p) :
    let D := Ioi (0 : ℝ) ×ˢ (if c * p = 1 then Ici (0 : ℝ) else Ioi 0)
    let L := fun v : ℝ × ℝ ↦ Endpoint2FellerPareto.interiorLogLik d p v.1 c s v.2
    ∃ v ∈ D, IsMaxOn L D v := by
  intro D L
  obtain ⟨j, hj⟩ := hz
  obtain ⟨k, hk⟩ := hne
  let N : ℝ := n
  let a := c * p
  let H := log c - log s - log (Gamma p)
  let R := fun y : ℝ ↦ log (1 + (y / s) ^ c)
  let A := (∑ i, R (d i)) / N
  let F := fun q : ℝ ↦ log (Gamma (p + q)) - log (Gamma q) - q * A
  let g := fun y : ℝ ↦ (a - 1) * log (y / s) - p * R y
  have hN : 0 < N := Nat.cast_pos.mpr j.isLt.pos
  have hR (y : ℝ) (hy : 0 ≤ y) : 0 ≤ R y :=
    log_nonneg (le_add_of_nonneg_right (rpow_nonneg (div_nonneg hy hs.le) _))
  have hRmono : MonotoneOn R (Ici (0 : ℝ)) := by
    intro y hy z hz hyz
    apply log_le_log (add_pos_of_pos_of_nonneg one_pos
      (rpow_nonneg (div_nonneg hy hs.le) c))
    exact add_le_add_right (rpow_le_rpow (div_nonneg hy hs.le)
      (div_le_div_of_nonneg_right hyz hs.le) hc.le) 1
  have hA : 0 < A := div_pos (Finset.sum_pos' (fun i _ ↦ hR _ (hd i))
    ⟨k, Finset.mem_univ _, log_pos (lt_add_of_pos_right _
      (rpow_pos_of_pos (div_pos hk hs) _))⟩) hN
  obtain ⟨qstar, _, hstar⟩ := upper_shape_max hp hA
  have hgap (v : ℝ × ℝ) (hv : v ∈ D) : 0 ≤ v.2 := by
    have h := hv.2
    by_cases h' : a = 1
    · simpa [a, h'] using h
    · exact (show 0 < v.2 by simpa [a, h'] using h).le
  have hgap' (v : ℝ × ℝ) (hv : v ∈ D) (h : a ≠ 1) : 0 < v.2 := by
    simpa [a, h] using hv.2
  have hform (v : ℝ × ℝ) :
      L v = N * (H + F v.1) + ∑ i, g (v.2 + d i) -
        v.1 * (∑ i, (R (v.2 + d i) - R (d i))) := by
    have hcancel : N * A = ∑ i, R (d i) := by
      dsimp [A]
      rw [mul_div_cancel₀ _ hN.ne']
    have he : (c * p + c * v.1) / c = p + v.1 := by
      rw [← mul_add, mul_div_cancel_left₀ _ hc.ne']
    simp only [L, Endpoint2FellerPareto.interiorLogLik, Endpoint2FellerPareto.logLik, he,
      H, F, g, R, a, Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.mul_sum,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    change _ = N * (_ + (_ - v.1 * A)) + _ - _
    linear_combination v.1 * hcancel
  have hg₁ (y : ℝ) (hy : 0 < y) : g y ≤ -log (y / s) := by
    have hpow := rpow_pos_of_pos (div_pos hy hs) c
    have h := log_le_log hpow (le_add_of_nonneg_left zero_le_one)
    rw [log_rpow (div_pos hy hs)] at h
    dsimp [g, R, a]
    nlinarith [mul_le_mul_of_nonneg_left h hp.le]
  have hg (y : ℝ) (hy : 0 ≤ y) : g y ≤ 0 := by
    by_cases h : y ≤ s
    · have hl := log_nonpos (div_nonneg hy hs.le) ((div_le_one hs).mpr h)
      exact sub_nonpos.mpr ((mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr ha) hl).trans
        (mul_nonneg hp.le (hR y hy)))
    · exact (hg₁ y (hs.trans (lt_of_not_ge h))).trans
        (neg_nonpos.mpr (log_nonneg ((one_le_div hs).mpr (le_of_not_ge h))))
  have hgSum (v : ℝ × ℝ) (hv : v ∈ D) (i : Fin n) :
      (∑ l, g (v.2 + d l)) ≤ g (v.2 + d i) := by
    rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i)]
    have hsum : (∑ l ∈ Finset.univ.erase i, g (v.2 + d l)) ≤ 0 :=
      Finset.sum_nonpos fun l _ ↦ hg _ (add_nonneg (hgap v hv) (hd l))
    linarith
  have hbound (v : ℝ × ℝ) (hv : v ∈ D) :
      L v ≤ N * (H + F v.1) + ∑ i, g (v.2 + d i) := by
    rw [hform]
    have hsum : 0 ≤ ∑ i, (R (v.2 + d i) - R (d i)) :=
      Finset.sum_nonneg fun i _ ↦ sub_nonneg.mpr
        (hRmono (hd i) (add_nonneg (hgap v hv) (hd i)) (le_add_of_nonneg_left (hgap v hv)))
    exact sub_le_self _ (mul_nonneg (show 0 ≤ v.1 from (show 0 < v.1 from hv.1).le) hsum)
  let C := L (1, 1)
  let U := N * (H + F qstar)
  have hupper (v : ℝ × ℝ) (hv : v ∈ D) (i : Fin n) :
      L v ≤ U + g (v.2 + d i) :=
    (hbound v hv).trans (add_le_add
      (mul_le_mul_of_nonneg_left (add_le_add_right (hstar hv.1) H) hN.le) (hgSum v hv i))
  have hlevel : ∀ v ∈ D, C ≤ L v → (C / N - H) ≤ F v.1 := by
    intro v hv h
    have hh := (hbound v hv).trans (add_le_of_nonpos_right
      (Finset.sum_nonpos fun i _ ↦ hg _ (add_nonneg (hgap v hv) (hd i))))
    exact sub_le_iff_le_add.mpr (by
      rw [mul_comm N (H + F v.1)] at hh
      have h' := (div_le_iff₀ hN).mpr (h.trans hh)
      linarith)
  obtain ⟨ε, hε, hsmall⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp
    ((upper_shape_tails hp hA).1.eventually_lt_atBot (C / N - H))
  obtain ⟨B, hB⟩ := eventually_atTop.mp
    ((upper_shape_tails hp hA).2.eventually_lt_atBot (C / N - H))
  let T := s * exp (U - C + 1)
  let δ := if a = 1 then 0 else s * exp ((C - U - 1) / (a - 1))
  let Q : Set (ℝ × ℝ) := Icc ε B ×ˢ Icc δ T
  have hQ : Q ⊆ D := by
    intro v hv
    refine ⟨hε.trans_le hv.1.1, ?_⟩
    by_cases he : a = 1
    · simpa [δ, a, he] using hv.2.1
    · have hδ : 0 < δ := by simp only [δ, he, ite_false]; positivity
      simpa [a, he] using hδ.trans_le hv.2.1
  have hkeep (v : ℝ × ℝ) (hv : v ∈ D) (h : C ≤ L v) : v ∈ Q := by
    have hF := hlevel v hv h
    refine ⟨⟨le_of_not_gt (fun h' ↦ (hsmall ⟨hv.1, h'⟩).not_ge hF),
      le_of_not_gt (fun h' ↦ (hB _ h'.le).not_ge hF)⟩, ?_, ?_⟩
    · by_cases he : a = 1
      · simpa [δ, he] using hgap v hv
      · have hat : 0 < a - 1 := by linarith [lt_of_le_of_ne ha (Ne.symm he)]
        have ht := hgap' v hv he
        have hb := hupper v hv j
        rw [hj, add_zero] at hb
        have hb' : g v.2 ≤ (a - 1) * log (v.2 / s) :=
          sub_le_self _ (mul_nonneg hp.le (hR _ ht.le))
        have hlog : (C - U - 1) / (a - 1) ≤ log (v.2 / s) := by
          apply (div_le_iff₀ hat).mpr
          linarith [h.trans (hb.trans (add_le_add_right hb' _))]
        simp only [δ, he, ite_false]
        simpa only [mul_comm s] using (le_div_iff₀ hs).mp (by
          simpa only [exp_log (div_pos ht hs)] using exp_le_exp.mpr hlog :
            exp ((C - U - 1) / (a - 1)) ≤ v.2 / s)
    · by_contra! hT
      have ht : 0 < v.2 := (mul_pos hs (exp_pos _)).trans hT
      have hlog : U - C + 1 < log (v.2 / s) := by
        apply exp_lt_exp.mp
        rw [exp_log (div_pos ht hs)]
        exact (lt_div_iff₀ hs).mpr (by simpa [T, mul_comm] using hT)
      have hb := hupper v hv j
      rw [hj, add_zero] at hb
      linarith [hb.trans (add_le_add_right (hg₁ _ ht) _)]
  have h1 : (1, 1) ∈ D := by
    refine ⟨one_pos, ?_⟩
    by_cases he : a = 1 <;> simp [a, he]
  have hcont : ContinuousOn L Q := by
    intro v hv
    apply ContinuousAt.continuousWithinAt
    have hvD := hQ hv
    have hq : 0 < v.1 := hvD.1
    unfold L Endpoint2FellerPareto.interiorLogLik Endpoint2FellerPareto.logLik
    apply tendsto_finsetSum Finset.univ
    intro i _
    have hqΓ := (continuousAt_logGamma hq).comp' continuous_fst.continuousAt
    have hpqArg : ContinuousAt (fun w : ℝ × ℝ ↦ p + w.1) v :=
      continuousAt_const.add continuous_fst.continuousAt
    have hpqΓ := (continuousAt_logGamma (add_pos hp hq)).comp'
      (f := fun w : ℝ × ℝ ↦ p + w.1) (x := v) hpqArg
    have hhead := (continuousAt_const (y := log c - log s)).sub
      ((continuousAt_const (y := log (Gamma p))).add hqΓ |>.sub hpqΓ)
    have hr : ContinuousAt (fun w : ℝ × ℝ ↦ (w.2 + d i) / s) v :=
      (continuous_snd.continuousAt.add continuousAt_const).div_const s
    have hn₀ := div_nonneg (add_nonneg (hgap v hvD) (hd i)) hs.le
    have hpow := (continuousAt_rpow_const _ c (Or.inr hc.le)).comp' hr
    have htail := (continuousAt_const.add hpow).log
      (add_pos_of_pos_of_nonneg one_pos (rpow_nonneg hn₀ c)).ne'
    have hcoef : ContinuousAt (fun w : ℝ × ℝ ↦ (c * p + c * w.1) / c) v :=
      (continuousAt_const.add (continuousAt_const.mul continuous_fst.continuousAt)).div_const c
    have hbody : ContinuousAt (fun w : ℝ × ℝ ↦ (a - 1) * log ((w.2 + d i) / s)) v := by
      by_cases he : a = 1
      · simp [he, continuousAt_const]
      · exact continuousAt_const.mul
          (hr.log (div_pos (add_pos_of_pos_of_nonneg (hgap' v hvD he) (hd i)) hs).ne')
    exact ((hhead.add hbody).sub (hcoef.mul htail)).tendsto
  have h1Q := hkeep (1, 1) h1 le_rfl
  obtain ⟨v, hv, hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨_, h1Q⟩ hcont
  refine ⟨v, hQ hv, fun w hw ↦ ?_⟩
  by_cases h : C ≤ L w
  · exact hm (hkeep w hw h)
  · exact (le_of_not_ge h).trans (hm h1Q)

set_option maxHeartbeats 600000 in
-- The criterion elaborates the joined failure and attainment branches.
/-- At held scale, lower shape and power, attainment holds exactly at
regular lower power on a nonconstant sample. -/
theorem criterion {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    {p c s : ℝ} (hp : 0 < p) (hc : 0 < c) (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (if c * p = 1 then Ici (0 : ℝ) else Ioi 0)
    let L := fun v : ℝ × ℝ ↦ Endpoint2FellerPareto.interiorLogLik d p v.1 c s v.2
    (∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ c * p ∧ ∃ i, 0 < d i := by
  intro D L
  constructor
  · rintro ⟨v, _, hm⟩
    by_contra! h
    have hbad : c * p < 1 ∨ ∀ i, d i = 0 := by
      by_cases hreg : 1 ≤ c * p
      · exact Or.inr fun i ↦ le_antisymm (h hreg i) (hd i)
      · exact Or.inl (lt_of_not_ge hreg)
    obtain ⟨t, ht, q, hq, hlt⟩ := Endpoint3FellerPareto.held_scale_lower_shape_power_failure
      hd hz hp hc hs hbad (L v)
    have hD : (q, t) ∈ D := by
      refine ⟨hq, ?_⟩
      by_cases he : c * p = 1 <;> simp [he, ht, ht.le]
    exact hlt.not_ge (hm hD)
  · rintro ⟨ha, hne⟩
    exact regular_fit hd hz hne hp hc hs ha

/-- When the held lower power is one, every nonconstant sample has
one maximizing upper shape and threshold. The threshold is closed
contact at the sample minimum. -/
theorem unit_lower_power_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) (hne : ∃ i, 0 < d i) {p c s : ℝ}
    (hp : 0 < p) (hc : 0 < c) (hs : 0 < s) (hunit : c * p = 1) :
    let D := Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ)
    let L := fun v : ℝ × ℝ ↦ Endpoint2FellerPareto.interiorLogLik d p v.1 c s v.2
    ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ v.2 = 0 := by
  intro D L
  have hN : 0 < (n : ℝ) := by
    obtain ⟨j, _⟩ := hz
    exact Nat.cast_pos.mpr j.isLt.pos
  have hp0 (q : ℝ) (hq : 0 < q) : ∀ t ∈ Ici (0 : ℝ),
      IsMaxOn (fun u ↦ L (q, u)) (Ici 0) t → t = 0 :=
    (Endpoint3FellerPareto.fixed_regular_threshold_uniqueness hd hz
      (a := c * p) (b := c * q) (s := s)
      (K := log c - log s - (log (Gamma p) + log (Gamma q) - log (Gamma (p + q))))
      (mul_pos hc hq) hc hs).1 hunit |>.2
  have hcontact (v : ℝ × ℝ) (hv : v ∈ D) (hvmax : IsMaxOn L D v) : v.2 = 0 :=
    hp0 v.1 hv.1 v.2 hv.2 (fun _ ht ↦ hvmax (show (_, _) ∈ D from ⟨hv.1, ht⟩))
  let T := ∑ i, log (1 + (d i / s) ^ c)
  have hform (q : ℝ) (hq : 0 < q) :
      L (q, 0) = (n : ℝ) * (log c - log s - Beta.logBeta (p, q)) - (p + q) * T := by
    have hβ : Beta.logBeta (p, q) =
        log (Gamma p) + log (Gamma q) - log (Gamma (p + q)) := by
      unfold Beta.logBeta Beta.normalizer
      dsimp only [Prod.fst, Prod.snd]
      rw [log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hq)).ne'
        (Gamma_pos_of_pos (add_pos hp hq)).ne',
        log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne']
    have he : (c * p + c * q) / c = p + q := by
      rw [← mul_add, mul_div_cancel_left₀ _ hc.ne']
    simp only [L, Endpoint2FellerPareto.interiorLogLik, Endpoint2FellerPareto.logLik,
      hunit, sub_self, zero_mul, add_zero, zero_add, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum, hβ, T]
    rw [← hunit, he]
    ring
  let f := fun q ↦ L (q, 0)
  have hconc : StrictConcaveOn ℝ (Ioi 0) f := by
    refine ⟨convex_Ioi 0, ?_⟩
    intro u hu v hv hne a b ha hb hab
    have hpos : 0 < a * u + b * v := add_pos (mul_pos ha hu) (mul_pos hb hv)
    have hshape := Beta.logBeta_strictConvex.2
      (show (p, u) ∈ Beta.domain from ⟨hp, hu⟩)
      (show (p, v) ∈ Beta.domain from ⟨hp, hv⟩)
      (fun h ↦ hne (congrArg Prod.snd h)) ha hb hab
    have he : a • (p,u) + b • (p,v) = (p, a*u+b*v) := by
      ext
      · change a*p+b*p = p
        linarith [congrArg (fun t : ℝ ↦ t*p) hab]
      · rfl
    rw [he] at hshape
    simp only [smul_eq_mul] at hshape
    simp only [smul_eq_mul, f, hform _ hu, hform _ hv, hform _ hpos]
    nlinarith [mul_lt_mul_of_pos_left hshape hN,
      congrArg (fun t : ℝ ↦ t * ((n : ℝ) * (log c - log s) - p * T)) hab]
  obtain ⟨w, hw, hm⟩ := (show ∃ v ∈ D, IsMaxOn L D v from by
    simpa only [hunit, ite_true] using regular_fit hd hz hne hp hc hs (by simp [hunit]))
  have hw0 := hcontact w hw hm
  refine ⟨w, ⟨hw, hm, hw0⟩, ?_⟩
  rintro v ⟨hv, hvm, hv0⟩
  have hmax (u : ℝ × ℝ) (hu : u ∈ D) (hm : IsMaxOn L D u) (h0 : u.2 = 0) :
      IsMaxOn f (Ioi 0) u.1 := by
    intro t ht
    change L (t, 0) ≤ L (u.1, 0)
    have hh : L (t, 0) ≤ L u := hm (show (t, 0) ∈ D from ⟨ht, le_rfl⟩)
    rw [show (u.1, 0) = u from Prod.ext rfl h0.symm]
    exact hh
  exact Prod.ext (hconc.eq_of_isMaxOn (hmax v hv hvm hv0) (hmax w hw hm hw0) hv.1 hw.1)
    (hv0.trans hw0.symm)

end

end UnivariateML.Endpoint3FellerParetoHeldScale

end
