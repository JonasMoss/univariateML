module

public import UnivariateML.Generic.UniqueAE
public import Mathlib.Analysis.Calculus.LocalExtr.Rolle
public import Mathlib.Analysis.SpecialFunctions.Pow.Deriv
public import Mathlib.Analysis.SpecialFunctions.Gamma.Deriv

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Superunit two-piece generalized-error scores

## Deviations

These results compare the smooth, strictly superunit chart. Shape-one
median classes and the moving-knot charts for subunit parameters are
separate.
-/

@[expose] public section

open Set Filter MeasureTheory Real ContinuousLinearMap
open scoped Topology

namespace UnivariateML.AE4SGED

noncomputable section

private def comparison (u v d k r : ℝ) := k + u * log r - v * log (r+d)

private theorem comparison_deriv {u v d k r : ℝ} (hr : 0 < r) (hd : 0 < d) :
    HasDerivAt (comparison u v d k) ((u*d+(u-v)*r)/(r*(r+d))) r := by
  have hrd : 0 < r+d := by positivity
  have h := (((hasDerivAt_log hr.ne').const_mul u).const_add k).sub
    (((hasDerivAt_log hrd.ne').comp r ((hasDerivAt_id r).add_const d)).const_mul v)
  convert h using 1
  · rfl
  · field_simp [hr.ne', hrd.ne']
    ring

private theorem comparison_critical {u v d k a b : ℝ} (hd : 0 < d)
    (ha : 0 < a) (hab : a < b)
    (h : comparison u v d k a = comparison u v d k b) :
    ∃ r ∈ Ioo a b, u*d+(u-v)*r = 0 := by
  have hc : ContinuousOn (comparison u v d k) (Icc a b) := by
    intro r hr
    exact (comparison_deriv (ha.trans_le hr.1) hd).continuousAt.continuousWithinAt
  obtain ⟨r,hr,hzero⟩ := exists_deriv_eq_zero hab hc h
  have hpos := ha.trans hr.1
  rw [(comparison_deriv hpos hd).deriv] at hzero
  exact ⟨r,hr,(div_eq_zero_iff.mp hzero).resolve_right (by positivity)⟩

private theorem one_exterior {u v d k a b : ℝ} (hu : 0 < u) (huv : v ≤ u)
    (hd : 0 < d) (ha : 0 < a) (hab : a < b)
    (h : comparison u v d k a = comparison u v d k b) : False := by
  obtain ⟨r,hr,hzero⟩ := comparison_critical hd ha hab h
  have h₁ : 0 < u*d := by positivity
  have h₂ : 0 ≤ (u-v)*r := mul_nonneg (sub_nonneg.mpr huv) (ha.trans hr.1).le
  linarith

private theorem two_exterior {u v d k a b c : ℝ} (hu : 0 < u) (hd : 0 < d)
    (ha : 0 < a) (hab : a < b) (hbc : b < c)
    (h₁ : comparison u v d k a = comparison u v d k b)
    (h₂ : comparison u v d k b = comparison u v d k c) : False := by
  obtain ⟨r,hr,er⟩ := comparison_critical hd ha hab h₁
  obtain ⟨s,hs,es⟩ := comparison_critical hd (ha.trans hab) hbc h₂
  have hp : (u-v)*(r-s) = 0 := by linear_combination er-es
  have huv := (mul_eq_zero.mp hp).resolve_right (sub_ne_zero.mpr (hr.2.trans hs.1).ne)
  have hpos : 0 < u*d := by positivity
  rw [huv, zero_mul, add_zero] at er
  exact hpos.ne' er

/-- Shape, join, left kernel width and right kernel width. -/
abbrev Parameter := ℝ × (ℝ × ℝ × ℝ)

private def rightMagnitude (p a r : ℝ) := p * r ^ (p-1) / a ^ p

/-- Spatial derivative of the log density on the strictly superunit chart. -/
def spatialScore (p : Parameter) (t : ℝ) : ℝ :=
  p.1 * (max (p.2.1-t) 0) ^ (p.1-1) / p.2.2.1 ^ p.1 -
    p.1 * (max (t-p.2.1) 0) ^ (p.1-1) / p.2.2.2 ^ p.1

private theorem mag_pos {p a r : ℝ} (hp : 1 < p) (ha : 0 < a) (hr : 0 < r) :
    0 < rightMagnitude p a r := by
  unfold rightMagnitude
  have : 0 < p := zero_lt_one.trans hp
  positivity

private theorem mag_zero {p a : ℝ} (hp : 1 < p) :
    rightMagnitude p a 0 = 0 := by
  simp [rightMagnitude, zero_rpow (sub_ne_zero.mpr hp.ne')]

private theorem left_score {p : Parameter} (hp : 1 < p.1) {t : ℝ} (ht : t ≤ p.2.1) :
    spatialScore p t = rightMagnitude p.1 p.2.2.1 (p.2.1-t) := by
  change rightMagnitude p.1 p.2.2.1 (max (p.2.1-t) 0) -
    rightMagnitude p.1 p.2.2.2 (max (t-p.2.1) 0) = _
  simp [max_eq_left (sub_nonneg.mpr ht), max_eq_right (sub_nonpos.mpr ht),
    mag_zero hp]

private theorem right_score {p : Parameter} (hp : 1 < p.1) {t : ℝ} (ht : p.2.1 ≤ t) :
    spatialScore p t = -rightMagnitude p.1 p.2.2.2 (t-p.2.1) := by
  change rightMagnitude p.1 p.2.2.1 (max (p.2.1-t) 0) -
    rightMagnitude p.1 p.2.2.2 (max (t-p.2.1) 0) = _
  simp [max_eq_left (sub_nonneg.mpr ht), max_eq_right (sub_nonpos.mpr ht),
    mag_zero hp]

private theorem magnitude_comparison {u v a b d r : ℝ}
    (hu : 1 < u) (hv : 1 < v) (ha : 0 < a) (hb : 0 < b)
    (hd : 0 < d) (hr : 0 < r)
    (he : rightMagnitude u a r = rightMagnitude v b (r + d)) :
    comparison (u-1) (v-1) d (log u-u*log a-(log v-v*log b)) r = 0 := by
  have hu' := zero_lt_one.trans hu
  have hv' := zero_lt_one.trans hv
  have hrd : 0 < r+d := by positivity
  have h := congrArg log he
  unfold comparison rightMagnitude at *
  rw [log_div (mul_pos hu' (rpow_pos_of_pos hr _)).ne' (rpow_pos_of_pos ha _).ne',
    log_div (mul_pos hv' (rpow_pos_of_pos hrd _)).ne' (rpow_pos_of_pos hb _).ne',
    log_mul hu'.ne' (rpow_pos_of_pos hr _).ne', log_mul hv'.ne' (rpow_pos_of_pos hrd _).ne',
    log_rpow hr, log_rpow hrd, log_rpow ha, log_rpow hb] at h
  linarith

private theorem exterior_side {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2)
    (hjoin : p.2.1 < q.2.1) {t : ℝ} (he : spatialScore p t = spatialScore q t) :
    t < p.2.1 ∨ q.2.1 < t := by
  by_contra! h
  rw [right_score hp.1 h.1, left_score hq.1 h.2] at he
  rcases eq_or_lt_of_le h.1 with h₁ | h₁
  · have h₂ : 0 < q.2.1-t := by linarith
    have hpos := mag_pos hq.1 hq.2.1 h₂
    rw [← h₁, sub_self, mag_zero hp.1, neg_zero] at he
    exact hpos.ne' (h₁ ▸ he.symm)
  · have hpos := mag_pos hp.1 hp.2.2 (sub_pos.mpr h₁)
    have hnon : 0 ≤ rightMagnitude q.1 q.2.2.1 (q.2.1-t) := by
      unfold rightMagnitude
      exact div_nonneg
        (mul_nonneg (zero_lt_one.trans hq.1).le (rpow_nonneg (sub_nonneg.mpr h.2) _))
        (rpow_nonneg hq.2.1.le _)
    linarith

private theorem distinct_joins {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2)
    (hjoin : p.2.1 < q.2.1) {a b c d : ℝ} (hab : a < b) (hbc : b < c) (hcd : c < d)
    (ea : spatialScore p a = spatialScore q a) (eb : spatialScore p b = spatialScore q b)
    (ec : spatialScore p c = spatialScore q c) (ed : spatialScore p d = spatialScore q d) :
    False := by
  let D := q.2.1-p.2.1
  have hD : 0 < D := sub_pos.mpr hjoin
  have left (t : ℝ) (ht : t < p.2.1) (e : spatialScore p t = spatialScore q t) :
      comparison (p.1-1) (q.1-1) D
        (log p.1-p.1*log p.2.2.1-(log q.1-q.1*log q.2.2.1)) (p.2.1-t) = 0 := by
    apply magnitude_comparison hp.1 hq.1 hp.2.1 hq.2.1 hD (sub_pos.mpr ht)
    rw [show p.2.1-t+D=q.2.1-t by dsimp [D]; ring]
    rwa [left_score hp.1 ht.le, left_score hq.1 (ht.trans hjoin).le] at e
  have right (t : ℝ) (ht : q.2.1 < t) (e : spatialScore p t = spatialScore q t) :
      comparison (q.1-1) (p.1-1) D
        (log q.1-q.1*log q.2.2.2-(log p.1-p.1*log p.2.2.2)) (t-q.2.1) = 0 := by
    apply magnitude_comparison hq.1 hp.1 hq.2.2 hp.2.2 hD (sub_pos.mpr ht)
    rw [show t-q.2.1+D=t-p.2.1 by dsimp [D]; ring]
    rw [right_score hp.1 (hjoin.trans ht).le, right_score hq.1 ht.le] at e
    exact neg_inj.mp e.symm
  rcases le_or_gt q.1 p.1 with h | h
  · rcases exterior_side hp hq hjoin eb with hl | hr
    · exact one_exterior (sub_pos.mpr hp.1) (by linarith) hD (sub_pos.mpr hl)
        (by linarith) ((left b hl eb).trans (left a (hab.trans hl) ea).symm)
    · have hc := hr.trans hbc
      exact two_exterior (sub_pos.mpr hq.1) hD (sub_pos.mpr hr) (by linarith) (by linarith)
        ((right b hr eb).trans (right c hc ec).symm)
        ((right c hc ec).trans (right d (hc.trans hcd) ed).symm)
  · rcases exterior_side hp hq hjoin ec with hl | hr
    · have hb := hbc.trans hl
      exact two_exterior (sub_pos.mpr hp.1) hD (sub_pos.mpr hl) (by linarith) (by linarith)
        ((left c hl ec).trans (left b hb eb).symm)
        ((left b hb eb).trans (left a (hab.trans hb) ea).symm)
    · exact one_exterior (sub_pos.mpr hq.1) (by linarith) hD (sub_pos.mpr hr)
        (by linarith) ((right c hr ec).trans (right d (hr.trans hcd) ed).symm)

/-- At four ordered distinct levels, two positive-width superunit
two-piece generalized-error laws with equal scores have the same join. -/
theorem join_eq_of_four_scores {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2)
    {a b c d : ℝ} (hab : a < b) (hbc : b < c) (hcd : c < d)
    (ea : spatialScore p a = spatialScore q a) (eb : spatialScore p b = spatialScore q b)
    (ec : spatialScore p c = spatialScore q c) (ed : spatialScore p d = spatialScore q d) :
    p.2.1 = q.2.1 := by
  rcases lt_trichotomy p.2.1 q.2.1 with h | h | h
  · exact False.elim (distinct_joins hp hq h hab hbc hcd ea eb ec ed)
  · exact h
  · exact False.elim (distinct_joins hq hp h hab hbc hcd ea.symm eb.symm ec.symm ed.symm)


private theorem log_mag {p a r : ℝ} (hp : 1 < p) (ha : 0 < a) (hr : 0 < r) :
    log (rightMagnitude p a r) = log p + (p-1)*log r - p*log a := by
  have hp' := zero_lt_one.trans hp
  unfold rightMagnitude
  rw [log_div (mul_pos hp' (rpow_pos_of_pos hr _)).ne' (rpow_pos_of_pos ha _).ne',
    log_mul hp'.ne' (rpow_pos_of_pos hr _).ne', log_rpow hr, log_rpow ha]

private theorem shape_of_magnitudes {p q a b r s : ℝ}
    (hp : 1 < p) (hq : 1 < q) (ha : 0 < a) (hb : 0 < b)
    (hr : 0 < r) (hs : 0 < s) (hrs : r ≠ s)
    (er : rightMagnitude p a r = rightMagnitude q b r)
    (es : rightMagnitude p a s = rightMagnitude q b s) : p = q := by
  have h₁ := congrArg log er
  have h₂ := congrArg log es
  rw [log_mag hp ha hr, log_mag hq hb hr] at h₁
  rw [log_mag hp ha hs, log_mag hq hb hs] at h₂
  have h : (p-q)*(log r-log s) = 0 := by linear_combination h₁-h₂
  exact sub_eq_zero.mp ((mul_eq_zero.mp h).resolve_right (sub_ne_zero.mpr (fun e ↦
    hrs (strictMonoOn_log.injOn hr hs e))))

private theorem width_of_magnitude {p a b r : ℝ} (hp : 1 < p) (ha : 0 < a) (hb : 0 < b)
    (hr : 0 < r) (e : rightMagnitude p a r = rightMagnitude p b r) : a = b := by
  have h := congrArg log e
  rw [log_mag hp ha hr, log_mag hp hb hr] at h
  have hz : p*(log a-log b) = 0 := by linear_combination -h
  exact strictMonoOn_log.injOn ha hb (sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_left
    (zero_lt_one.trans hp).ne'))

private theorem same_shape_of_four_scores {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2)
    {a b c d : ℝ} (hab : a < b) (hbc : b < c) (hcd : c < d)
    (ea : spatialScore p a = spatialScore q a) (eb : spatialScore p b = spatialScore q b)
    (ec : spatialScore p c = spatialScore q c) (ed : spatialScore p d = spatialScore q d) :
    p.1 = q.1 ∧ p.2.1 = q.2.1 := by
  have hj := join_eq_of_four_scores hp hq hab hbc hcd ea eb ec ed
  refine ⟨?_,hj⟩
  by_cases hb : b < p.2.1
  · have ha := hab.trans hb
    rw [left_score hp.1 ha.le, left_score hq.1 (hj ▸ ha.le), ← hj] at ea
    rw [left_score hp.1 hb.le, left_score hq.1 (hj ▸ hb.le), ← hj] at eb
    exact shape_of_magnitudes hp.1 hq.1 hp.2.1 hq.2.1 (sub_pos.mpr ha) (sub_pos.mpr hb)
      (by intro h; linarith) ea eb
  · have hc := (le_of_not_gt hb).trans_lt hbc
    have hd := hc.trans hcd
    rw [right_score hp.1 hc.le, right_score hq.1 (hj ▸ hc.le), ← hj] at ec
    rw [right_score hp.1 hd.le, right_score hq.1 (hj ▸ hd.le), ← hj] at ed
    exact shape_of_magnitudes hp.1 hq.1 hp.2.2 hq.2.2 (sub_pos.mpr hc) (sub_pos.mpr hd)
      (by intro h; linarith) (neg_inj.mp ec) (neg_inj.mp ed)

/-- Weighted normalized log likelihood in the shape, join and width chart. -/
def logLik {n : ℕ} (w : Fin n → ℕ) (p : Parameter) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (-log (p.2.2.1+p.2.2.2) - log (Gamma (1+1/p.1)) -
    ((max (p.2.1-x i) 0 / p.2.2.1) ^ p.1 + (max (x i-p.2.1) 0 / p.2.2.2) ^ p.1))

private theorem identify_widths {n : ℕ} {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 2 ≤ n)
    {x : Fin n → ℝ} (hx : Function.Injective x) (hs : p.1 = q.1) (hj : p.2.1 = q.2.1)
    (he : ∀ i, spatialScore p (x i) = spatialScore q (x i))
    (hv : logLik w p x = logLik w q x) : p = q := by
  have hl (i : Fin n) (hi : x i < p.2.1) : p.2.2.1 = q.2.2.1 := by
    have h := he i
    rw [left_score hp.1 hi.le, left_score hq.1 (hj ▸ hi.le), ← hs, ← hj] at h
    exact width_of_magnitude hp.1 hp.2.1 hq.2.1 (sub_pos.mpr hi) h
  have hr (i : Fin n) (hi : p.2.1 < x i) : p.2.2.2 = q.2.2.2 := by
    have h := he i
    rw [right_score hp.1 hi.le, right_score hq.1 (hj ▸ hi.le), ← hs, ← hj] at h
    exact width_of_magnitude hp.1 hp.2.2 hq.2.2 (sub_pos.mpr hi) (neg_inj.mp h)
  have left (i : Fin n) :
      max (p.2.1 - x i) 0 / p.2.2.1 = max (q.2.1 - x i) 0 / q.2.2.1 := by
    rw [← hj]
    by_cases h : x i < p.2.1
    · rw [hl i h]
    · simp [max_eq_right (sub_nonpos.mpr (le_of_not_gt h))]
  have right (i : Fin n) :
      max (x i - p.2.1) 0 / p.2.2.2 = max (x i - q.2.1) 0 / q.2.2.2 := by
    rw [← hj]
    by_cases h : p.2.1 < x i
    · rw [hr i h]
    · simp [max_eq_right (sub_nonpos.mpr (le_of_not_gt h))]
  have hwpos : 0 < ∑ i, (w i : ℝ) := by
    let _ : Nonempty (Fin n) := ⟨⟨0, by omega⟩⟩
    exact Finset.sum_pos (fun i _ ↦ by exact_mod_cast hw i) Finset.univ_nonempty
  unfold logLik at hv
  simp only [← hs, mul_sub, Finset.sum_sub_distrib, ← Finset.sum_mul, left, right] at hv
  have hlogs := neg_inj.mp (mul_left_cancel₀ hwpos.ne'
    (sub_left_injective (sub_left_injective hv)))
  have hsum := strictMonoOn_log.injOn (add_pos hp.2.1 hp.2.2) (add_pos hq.2.1 hq.2.2) hlogs
  have hsome : ∃ i, x i ≠ p.2.1 := by
    by_contra! h
    let i : Fin n := ⟨0, by omega⟩
    let j : Fin n := ⟨1, by omega⟩
    have hij : i ≠ j := by simp [i, j]
    exact hij (hx ((h i).trans (h j).symm))
  obtain ⟨i, hi⟩ := hsome
  rcases hi.lt_or_gt with hi | hi
  · exact Prod.ext hs (Prod.ext hj (Prod.ext (hl i hi) (by linarith [hl i hi])))
  · exact Prod.ext hs (Prod.ext hj (Prod.ext (by linarith [hr i hi]) (hr i hi)))


private theorem separates {n : ℕ} {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 4 ≤ n)
    {x : Fin n → ℝ} (hx : Function.Injective x)
    (he : ∀ i, spatialScore p (x i) = spatialScore q (x i))
    (hv : logLik w p x = logLik w q x) : p = q := by
  classical
  let S := Finset.univ.image x
  have hS : 4 ≤ S.card := by simpa [S, Finset.card_image_of_injective _ hx] using hn
  let f := S.orderEmbOfCardLe hS
  have ef (i : Fin 4) : spatialScore p (f i) = spatialScore q (f i) := by
    have h := Finset.orderEmbOfCardLe_mem S hS i
    obtain ⟨j,hj⟩ : f i ∈ Set.range x := by simpa [S,f] using h
    exact hj ▸ he j
  obtain ⟨hs,hj⟩ := same_shape_of_four_scores hp hq
    (f.strictMono (show (0 : Fin 4) < 1 by decide))
    (f.strictMono (show (1 : Fin 4) < 2 by decide))
    (f.strictMono (show (2 : Fin 4) < 3 by decide)) (ef 0) (ef 1) (ef 2) (ef 3)
  exact identify_widths hp hq w hw (by omega) hx hs hj he hv

private theorem fixed_shape_join {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2) (hshape : p.1 = q.1)
    (hjoin : p.2.1 < q.2.1) {a b c : ℝ} (hab : a < b) (hbc : b < c)
    (ea : spatialScore p a = spatialScore q a) (eb : spatialScore p b = spatialScore q b)
    (ec : spatialScore p c = spatialScore q c) : False := by
  let D := q.2.1-p.2.1
  have hD : 0 < D := sub_pos.mpr hjoin
  have left (t : ℝ) (ht : t < p.2.1) (e : spatialScore p t = spatialScore q t) :
      comparison (p.1-1) (q.1-1) D
        (log p.1-p.1*log p.2.2.1-(log q.1-q.1*log q.2.2.1)) (p.2.1-t) = 0 := by
    apply magnitude_comparison hp.1 hq.1 hp.2.1 hq.2.1 hD (sub_pos.mpr ht)
    rw [show p.2.1-t+D=q.2.1-t by dsimp [D]; ring]
    rwa [left_score hp.1 ht.le, left_score hq.1 (ht.trans hjoin).le] at e
  have right (t : ℝ) (ht : q.2.1 < t) (e : spatialScore p t = spatialScore q t) :
      comparison (q.1-1) (p.1-1) D
        (log q.1-q.1*log q.2.2.2-(log p.1-p.1*log p.2.2.2)) (t-q.2.1) = 0 := by
    apply magnitude_comparison hq.1 hp.1 hq.2.2 hp.2.2 hD (sub_pos.mpr ht)
    rw [show t-q.2.1+D=t-p.2.1 by dsimp [D]; ring]
    rw [right_score hp.1 (hjoin.trans ht).le, right_score hq.1 ht.le] at e
    exact neg_inj.mp e.symm
  rcases exterior_side hp hq hjoin eb with hl | hr
  · exact one_exterior (sub_pos.mpr hp.1) (by linarith) hD (sub_pos.mpr hl)
      (by linarith) ((left b hl eb).trans (left a (hab.trans hl) ea).symm)
  · exact one_exterior (sub_pos.mpr hq.1) (by linarith) hD (sub_pos.mpr hr)
      (by linarith) ((right b hr eb).trans (right c (hr.trans hbc) ec).symm)

private theorem fixed_shape_separates {n : ℕ} {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2) (hs : p.1 = q.1)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 3 ≤ n)
    {x : Fin n → ℝ} (hx : Function.Injective x)
    (he : ∀ i, spatialScore p (x i) = spatialScore q (x i))
    (hv : logLik w p x = logLik w q x) : p = q := by
  classical
  let S := Finset.univ.image x
  have hS : 3 ≤ S.card := by simpa [S, Finset.card_image_of_injective _ hx] using hn
  let f := S.orderEmbOfCardLe hS
  have ef (i : Fin 3) : spatialScore p (f i) = spatialScore q (f i) := by
    have h := Finset.orderEmbOfCardLe_mem S hS i
    obtain ⟨j,hj⟩ : f i ∈ Set.range x := by simpa [S,f] using h
    exact hj ▸ he j
  have h₁ := f.strictMono (show (0 : Fin 3) < 1 by decide)
  have h₂ := f.strictMono (show (1 : Fin 3) < 2 by decide)
  have hj : p.2.1 = q.2.1 := by
    rcases lt_trichotomy p.2.1 q.2.1 with h | h | h
    · exact False.elim (fixed_shape_join hp hq hs h h₁ h₂ (ef 0) (ef 1) (ef 2))
    · exact h
    · exact False.elim (fixed_shape_join hq hp hs.symm h h₁ h₂
        (ef 0).symm (ef 1).symm (ef 2).symm)
  exact identify_widths hp hq w hw (by omega) hx hs hj he hv

private theorem max_rpow_deriv {p : ℝ} (hp : 1 < p) (x : ℝ) :
    HasDerivAt (fun t : ℝ ↦ (max t 0)^p) (p*(max x 0)^(p-1)) x := by
  have hz : (0 : ℝ)^(p-1) = 0 := zero_rpow (sub_ne_zero.mpr hp.ne')
  have hz' : (0 : ℝ)^p = 0 := zero_rpow (zero_lt_one.trans hp).ne'
  have h (t : ℝ) := hasDerivAt_rpow_const (x := t) (Or.inr hp.le)
  rcases lt_trichotomy x 0 with hx | rfl | hx
  · rw [max_eq_right hx.le, hz, mul_zero]
    apply (hasDerivAt_const x (0 : ℝ)).congr_of_eventuallyEq
    filter_upwards [Iio_mem_nhds hx] with t ht
    simp [max_eq_right ht.le,hz']
  · simp only [max_self, hz, mul_zero]
    have h₀ : HasDerivWithinAt (fun t : ℝ ↦ (max t 0)^p) 0 (Ici 0) 0 := by
      have h' : HasDerivAt (fun t : ℝ ↦ t^p) 0 0 := by simpa only [hz, mul_zero] using h 0
      apply h'.hasDerivWithinAt.congr_of_mem _ (by simp)
      intro t ht
      rw [max_eq_left ht]
    have h₁ : HasDerivWithinAt (fun t : ℝ ↦ (max t 0)^p) 0 (Iic 0) 0 := by
      apply (hasDerivAt_const 0 (0 : ℝ)).hasDerivWithinAt.congr_of_mem _ (by simp)
      intro t ht
      simp [max_eq_right ht,hz']
    exact (h₀.union h₁).hasDerivAt (by simp)
  · rw [max_eq_left hx.le]
    apply (h x).congr_of_eventuallyEq
    filter_upwards [Ioi_mem_nhds hx] with t ht
    rw [max_eq_left ht.le]

private theorem term_deriv {p : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2) (t : ℝ) :
    HasDerivAt (fun y ↦ -log (p.2.2.1+p.2.2.2) - log (Gamma (1+1/p.1)) -
      ((max (p.2.1-y) 0 / p.2.2.1) ^ p.1 + (max (y-p.2.1) 0 / p.2.2.2) ^ p.1))
      (spatialScore p t) t := by
  have hl := ((max_rpow_deriv hp.1 (p.2.1-t)).div_const (p.2.2.1 ^ p.1)).comp t
    ((hasDerivAt_id t).const_sub p.2.1)
  have hr := ((max_rpow_deriv hp.1 (t-p.2.1)).div_const (p.2.2.2 ^ p.1)).comp t
    ((hasDerivAt_id t).sub_const p.2.1)
  convert (hl.add hr).const_sub (-log (p.2.2.1+p.2.2.2) - log (Gamma (1+1/p.1))) using 1
  · ext y
    simp only [Function.comp_def, div_rpow (le_max_right _ _) hp.2.1.le,
      div_rpow (le_max_right _ _) hp.2.2.le]
    rfl
  · unfold spatialScore
    simp only [mul_neg, mul_one, neg_add_rev, neg_neg]
    ring

private theorem chart {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (A : Set Parameter) (hA : IsLocallyClosed A)
    (hpar : ∀ p ∈ A, 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hsep : ∀ p ∈ A, ∀ q ∈ A, ∀ x : Fin n → ℝ, Function.Injective x →
      (∀ i, spatialScore p (x i) = spatialScore q (x i)) → logLik w p x = logLik w q x →
      p = q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ logLik w r x) A p →
        IsMaxOn (fun r ↦ logLik w r x) A q → p = q := by
  let _ : LocallyCompactSpace A := hA.locallyCompactSpace
  let L (p : A) := logLik w p.val
  let D (p : A) (x : Fin n → ℝ) : (Fin n → ℝ) →L[ℝ] ℝ :=
    ∑ i, ((w i : ℝ) * spatialScore p.val (x i)) • proj i
  let E := A × (Fin n → ℝ)
  let P (z : E) := z.1.val.1
  let a (z : E) := z.1.val.2.2.1
  let b (z : E) := z.1.val.2.2.2
  let U (i : Fin n) (z : E) := max (z.1.val.2.1-z.2 i) 0
  let V (i : Fin n) (z : E) := max (z.2 i-z.1.val.2.1) 0
  have hp (z : E) := (hpar _ z.1.property).1
  have ha (z : E) := (hpar _ z.1.property).2.1
  have hb (z : E) := (hpar _ z.1.property).2.2
  have hP : Continuous P := by unfold P E; fun_prop
  have hac : Continuous a := by unfold a E; fun_prop
  have hbc : Continuous b := by unfold b E; fun_prop
  have hU (i) : Continuous (U i) := by unfold U E; fun_prop
  have hV (i) : Continuous (V i) := by unfold V E; fun_prop
  have hL : Continuous (fun z : E ↦ L z.1 z.2) := by
    unfold L logLik
    apply continuous_finsetSum Finset.univ
    intro i _
    have hd := ((hac.add hbc).log (fun z ↦ (add_pos (ha z) (hb z)).ne')).neg
    let G (z : E) := 1+1/P z
    have hG : Continuous G := continuous_const.add (continuous_const.div hP
      (fun z ↦ (zero_lt_one.trans (hp z)).ne'))
    have hpos (z : E) : 0 < G z := by
      unfold G
      exact add_pos one_pos (one_div_pos.mpr (zero_lt_one.trans (hp z)))
    have hγ : Continuous (fun z ↦ Gamma (G z)) := by
      rw [continuous_iff_continuousAt]
      intro z
      exact (differentiableAt_Gamma (fun m ↦ by
        have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
        linarith [hpos z])).continuousAt.comp hG.continuousAt
    have hg := hγ.log (fun z ↦ (Gamma_pos_of_pos (hpos z)).ne')
    have hl := ((hU i).div hac (fun z ↦ (ha z).ne')).rpow hP
      (fun z ↦ Or.inr (zero_lt_one.trans (hp z)))
    have hr := ((hV i).div hbc (fun z ↦ (hb z).ne')).rpow hP
      (fun z ↦ Or.inr (zero_lt_one.trans (hp z)))
    exact ((hd.sub hg).sub (hl.add hr)).const_mul (w i : ℝ)
  have hD : Continuous (fun z : E ↦ D z.1 z.2) := by
    unfold D spatialScore
    apply continuous_finsetSum Finset.univ
    intro i _
    have hPL := hP.sub (continuous_const (y := (1 : ℝ)))
    have hl := (hP.mul ((hU i).rpow hPL (fun z ↦ Or.inr (sub_pos.mpr (hp z))))).div
      (hac.rpow hP (fun z ↦ Or.inl (ha z).ne')) (fun z ↦ (rpow_pos_of_pos (ha z) _).ne')
    have hr := (hP.mul ((hV i).rpow hPL (fun z ↦ Or.inr (sub_pos.mpr (hp z))))).div
      (hbc.rpow hP (fun z ↦ Or.inl (hb z).ne')) (fun z ↦ (rpow_pos_of_pos (hb z) _).ne')
    exact ((hl.sub hr).const_mul (w i : ℝ)).smul continuous_const
  have hd (p : A) (x : Fin n → ℝ) : HasFDerivAt (L p) (D p x) x := by
    unfold L D logLik
    apply HasFDerivAt.fun_sum
    intro i _
    let ev : (Fin n → ℝ) →L[ℝ] ℝ := proj i
    have h := ((term_deriv (hpar _ p.property) (x i)).const_mul (w i : ℝ)).comp_hasFDerivAt x
      (ev.hasFDerivAt (x := x))
    exact h
  have hae := UniqueAE.ae_winners_derivative (μ := volume) (S := (univ : Set E))
    isOpen_univ hL.continuousOn hD.continuousOn (fun p x _ ↦ hd p x)
  filter_upwards [hae] with x hx hi p hp q hq hmp hmq
  have he := hx ⟨p, hp⟩ ⟨q, hq⟩ ⟨mem_univ _, fun r _ ↦ hmp r.property⟩
     ⟨mem_univ _, fun r _ ↦ hmq r.property⟩
  apply hsep p hp q hq x hi _ (le_antisymm (hmq hp) (hmp hq))
  intro i
  have eval (r : A) : D r x (Pi.single i 1) = (w i : ℝ) * spatialScore r.val (x i) := by
    simp only [D, sum_apply, smul_apply, proj_apply, smul_eq_mul]
    rw [Finset.sum_eq_single i]
    · simp
    · intro j _ hj
      simp [Pi.single_eq_of_ne hj]
    · simp
  have h := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single i 1)) he
  rw [eval, eval] at h
  exact mul_left_cancel₀ (by exact_mod_cast (hw i).ne') h


/-- On the superunit chart, any attained finite fit is almost everywhere
unique with four distinct levels and fixed positive counts. The chart may
include any locally closed hold restrictions with strictly positive widths. -/
theorem superunit_ae_unique {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 4 ≤ n) (A : Set Parameter) (hA : IsLocallyClosed A)
    (hpar : ∀ p ∈ A, 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ logLik w r x) A p →
        IsMaxOn (fun r ↦ logLik w r x) A q → p = q := by
  exact chart w hw A hA hpar (fun p hp q hq _ hx he hv ↦
    separates (hpar p hp) (hpar q hq) w hw hn hx he hv)

/-- At a fixed superunit shape, three distinct levels suffice for
almost-everywhere uniqueness under any locally closed hold restrictions. -/
theorem held_nu_superunit_ae_unique {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 3 ≤ n) (A : Set Parameter) (hA : IsLocallyClosed A) {shape : ℝ} (hs : 1 < shape)
    (hpar : ∀ p ∈ A, p.1 = shape ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ logLik w r x) A p →
        IsMaxOn (fun r ↦ logLik w r x) A q → p = q := by
  have hsuper (p) (hp : p ∈ A) : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2 :=
    ⟨(hpar p hp).1.symm ▸ hs, (hpar p hp).2⟩
  exact chart w hw A hA hsuper (fun p hp q hq _ hx he hv ↦
    fixed_shape_separates (hsuper p hp) (hsuper q hq)
      ((hpar p hp).1.trans (hpar q hq).1.symm) w hw hn hx he hv)

/-- The superunit SGED chart with closed restrictions on shape, population
mean, variance and squared skew. -/
def momentDomain (S M V K : Set ℝ) : Set Parameter :=
  {p | 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2 ∧ p.1 ∈ S ∧
    p.2.1 + (Gamma (2/p.1)/Gamma (1/p.1))*(p.2.2.2-p.2.2.1) ∈ M ∧
    (Gamma (3/p.1)/Gamma (1/p.1))*(p.2.2.1^2-p.2.2.1*p.2.2.2+p.2.2.2^2) -
      ((Gamma (2/p.1)/Gamma (1/p.1))*(p.2.2.2-p.2.2.1))^2 ∈ V ∧
    p.2.2.2/p.2.2.1 ∈ K}

private theorem momentDomain_locallyClosed (S M V K : Set ℝ)
    (hS : IsClosed S) (hM : IsClosed M) (hV : IsClosed V) (hK : IsClosed K) :
    IsLocallyClosed (momentDomain S M V K) := by
  let O : Set Parameter := Ioi 1 ×ˢ (univ ×ˢ Ioi 0 ×ˢ Ioi 0)
  have hO : IsOpen O := isOpen_Ioi.prod (isOpen_univ.prod (isOpen_Ioi.prod isOpen_Ioi))
  have hp (p : O) : 0 < p.val.1 := zero_lt_one.trans p.property.1
  have hleft (p : O) : 0 < p.val.2.2.1 := p.property.2.2.1
  have hpc : Continuous (fun p : O ↦ p.val.1) := by fun_prop
  have hγ (c : ℝ) (hc : 0 < c) : Continuous (fun p : O ↦ Gamma (c/p.val.1)) := by
    have hh := (continuous_const (y := c)).div hpc (fun p ↦ (hp p).ne')
    rw [continuous_iff_continuousAt]
    intro p
    exact (differentiableAt_Gamma (fun m ↦ by
      have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
      have hpos := div_pos hc (hp p)
      linarith)).continuousAt.comp hh.continuousAt
  let A (p : O) := Gamma (2/p.val.1)/Gamma (1/p.val.1)
  let B (p : O) := Gamma (3/p.val.1)/Gamma (1/p.val.1)
  have hden (p : O) : Gamma (1/p.val.1) ≠ 0 :=
    (Gamma_pos_of_pos (one_div_pos.mpr (hp p))).ne'
  have hA : Continuous A := (hγ 2 (by norm_num)).div (hγ 1 one_pos) hden
  have hB : Continuous B := (hγ 3 (by norm_num)).div (hγ 1 one_pos) hden
  let m (p : O) := p.val.2.1 + A p*(p.val.2.2.2-p.val.2.2.1)
  let v (p : O) := B p*(p.val.2.2.1^2-p.val.2.2.1*p.val.2.2.2+p.val.2.2.2^2) -
    (A p*(p.val.2.2.2-p.val.2.2.1))^2
  let k (p : O) := p.val.2.2.2/p.val.2.2.1
  have hm : Continuous m := by unfold m; fun_prop
  have hv : Continuous v := by unfold v; fun_prop
  have hk : Continuous k := Continuous.div (by fun_prop) (by fun_prop) (fun p ↦ (hleft p).ne')
  let C : Set O := {p | p.val.1 ∈ S ∧ m p ∈ M ∧ v p ∈ V ∧ k p ∈ K}
  have hC : IsClosed C :=
    (hS.preimage hpc).inter ((hM.preimage hm).inter ((hV.preimage hv).inter (hK.preimage hk)))
  obtain ⟨F,hF,hEq⟩ := isClosed_induced_iff.mp hC
  have he : momentDomain S M V K = O ∩ F := by
    ext p
    constructor
    · intro h
      have ho : p ∈ O := ⟨h.1,mem_univ _,h.2.1,h.2.2.1⟩
      have hc : (⟨p,ho⟩ : O) ∈ C := h.2.2.2
      rw [← hEq] at hc
      exact ⟨ho,hc⟩
    · rintro ⟨ho,hf⟩
      have hc : (⟨p,ho⟩ : O) ∈ Subtype.val ⁻¹' F := hf
      rw [hEq] at hc
      exact ⟨ho.1,ho.2.2.1,ho.2.2.2,hc⟩
  exact he ▸ hO.isLocallyClosed.inter hF.isLocallyClosed

/-- Four-level a.e. uniqueness among superunit SGED fits with optional
held moments or held skew. Use a singleton for a hold and `univ` for a
free coefficient. -/
theorem superunit_ae_unique_with_holds {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 4 ≤ n) (M V K : Set ℝ) (hM : IsClosed M) (hV : IsClosed V) (hK : IsClosed K) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ momentDomain univ M V K, ∀ q ∈ momentDomain univ M V K,
        IsMaxOn (fun r ↦ logLik w r x) (momentDomain univ M V K) p →
        IsMaxOn (fun r ↦ logLik w r x) (momentDomain univ M V K) q → p = q := by
  exact superunit_ae_unique w hw hn _ (momentDomain_locallyClosed _ _ _ _ isClosed_univ hM hV hK)
    (fun _ h ↦ ⟨h.1,h.2.1,h.2.2.1⟩)

/-- Three-level a.e. uniqueness at a held superunit shape with optional
held moments or held skew. -/
theorem held_nu_superunit_ae_unique_with_holds {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 3 ≤ n) {shape : ℝ} (hs : 1 < shape)
    (M V K : Set ℝ) (hM : IsClosed M) (hV : IsClosed V) (hK : IsClosed K) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ momentDomain {shape} M V K, ∀ q ∈ momentDomain {shape} M V K,
        IsMaxOn (fun r ↦ logLik w r x) (momentDomain {shape} M V K) p →
        IsMaxOn (fun r ↦ logLik w r x) (momentDomain {shape} M V K) q → p = q := by
  exact held_nu_superunit_ae_unique w hw hn _
    (momentDomain_locallyClosed _ _ _ _ isClosed_singleton hM hV hK) hs
    (fun _ h ↦ ⟨h.2.2.2.1,h.2.1,h.2.2.1⟩)

end

end UnivariateML.AE4SGED

end
