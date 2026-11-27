module

public import UnivariateML.Family.Endpoint2FellerPareto
public import UnivariateML.Family.TiesLomax

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Pareto II threshold and contact branches

The log likelihood on the compatible closed domain uses the Lomax density,
including zero excesses. A positive number of zero excesses gives a
scale-collapse route for sufficiently small fixed shape.
-/

@[expose] public section

open Real Set Filter
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2ParetoII

noncomputable section

variable {n : ℕ}

private theorem small_shape {d : Fin n → ℝ} (hz : ∃ i, d i = 0) :
    ∃ q : ℝ, 0 < q ∧ (∑ i, if d i = 0 then (-1 : ℝ) else q) < 0 := by
  classical
  obtain ⟨j, hj⟩ := hz
  let q : ℝ := 1 / (2 * ((n : ℝ) + 1))
  have hq : 0 < q := by dsimp [q]; positivity
  have hbound := Finset.sum_le_sum (s := Finset.univ)
    (f := fun i ↦ if d i = 0 then (-1 : ℝ) else q)
    (g := fun i ↦ q - if i = j then 1 else 0) (fun i _ ↦ by
      by_cases he : i = j
      · subst i
        simp [hj]
        linarith
      · by_cases hi : d i = 0 <;> simp [hi, he]
        linarith)
  rw [Finset.sum_sub_distrib, Finset.sum_ite_eq', ite_eq_left (Finset.mem_univ j),
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at hbound
  refine ⟨q, hq, hbound.trans_lt ?_⟩
  dsimp [q]
  have hn : 0 < 2 * ((n : ℝ) + 1) := by positivity
  rw [mul_one_div, sub_neg]
  exact (div_lt_iff₀ hn).mpr (by linarith)

/-- Scale-collapse exponent, summing one contribution per observation. -/
def exponent (d : Fin n → ℝ) (q : ℝ) : ℝ :=
  ∑ i, if d i = 0 then -1 else q

private def rayRemainder (d : Fin n → ℝ) (a q s : ℝ) : ℝ :=
  ∑ i, (log q - (q + 1) *
    (if d i = 0 then log (1 + a) else log ((1 + a) * s + d i)))

private theorem continuousAt_rayRemainder {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (a q : ℝ) : ContinuousAt (rayRemainder d a q) 0 := by
  apply tendsto_finsetSum Finset.univ
  intro i _
  by_cases hi : d i = 0
  · simpa only [ite_eq_left hi] using
      (continuousAt_const (y := log q - (q + 1) * log (1 + a)) (x := (0 : ℝ))).tendsto
  · have hd' : 0 < d i := lt_of_le_of_ne (hd i) (Ne.symm hi)
    simp only [ite_eq_right hi]
    have harg : ContinuousAt (fun s : ℝ ↦ (1 + a) * s + d i) 0 := by fun_prop
    exact (continuousAt_const.sub
      (continuousAt_const.mul (harg.log (by simpa using hd'.ne')))).tendsto

private theorem ray_expansion {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a q s : ℝ} (ha : 0 < 1 + a) (hs : 0 < s) :
    Transfer.ParetoII.logLik d (-a * s) q s =
      exponent d q * log s + rayRemainder d a q s := by
  classical
  unfold Transfer.ParetoII.logLik exponent rayRemainder
  rw [Finset.sum_mul, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  unfold Transfer.ParetoII.logDensity
  by_cases hi : d i = 0
  · have h : 1 + (d i - -a * s) / s = 1 + a := by
      rw [hi]
      field_simp [hs.ne']
      ring
    rw [h]
    simp only [ite_eq_left hi]
    ring
  · have h : 1 + (d i - -a * s) / s = ((1 + a) * s + d i) / s := by
      field_simp [hs.ne']
      ring
    rw [h, log_div (add_pos_of_pos_of_nonneg (mul_pos ha hs) (hd i)).ne' hs.ne']
    simp only [ite_eq_right hi]
    ring

private theorem paretoII_scale_ray {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a q : ℝ} (ha : 0 < 1 + a) (hE : exponent d q < 0) :
    Tendsto (fun s ↦ Transfer.ParetoII.logLik d (-a * s) q s) (𝓝[>] 0) atTop := by
  apply Tendsto.congr' _ ((tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg hE).atTop_add
    (continuousAt_rayRemainder hd a q).continuousWithinAt)
  filter_upwards [self_mem_nhdsWithin] with _ hs
  exact (ray_expansion hd ha hs).symm

/-- Any nonempty sample, centered at a sample minimum, has a fixed positive
shape for which the free-threshold Pareto II likelihood diverges. -/
theorem free_threshold_diverges {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) :
    ∃ q > 0, Tendsto (fun s ↦ Transfer.ParetoII.logLik d (-s) q s) (𝓝[>] 0) atTop := by
  obtain ⟨q, hq, hE⟩ := small_shape hz
  refine ⟨q, hq, ?_⟩
  simpa using paretoII_scale_ray hd (a := 1) (by norm_num) hE

/-- The unrestricted threshold, shape and scale domain has unbounded
likelihood and no finite maximizer, with all sample ties included. -/
theorem free_threshold_unbounded {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) :
    let D : Set (ℝ × ℝ × ℝ) := Ici 0 ×ˢ Lomax.domain
    let L : ℝ × ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) p.2.1 p.2.2
    ¬ BddAbove (L '' D) ∧ ¬ ∃ p ∈ D, IsMaxOn L D p := by
  intro D L
  have hb : ¬ BddAbove (L '' D) := by
    rintro ⟨M,hM⟩
    obtain ⟨q,hq,hpath⟩ := free_threshold_diverges hd hz
    have hpos : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
    obtain ⟨s,hgt,hs⟩ := ((hpath.eventually_gt_atTop M).and hpos).exists
    exact hgt.not_ge (hM ⟨(s,q,s), ⟨hs.le,hq,hs⟩,rfl⟩)
  exact ⟨hb, by rintro ⟨u,_,hm⟩; exact hb hm.bddAbove⟩

/-- Exact held-threshold existence criterion, including compatible
observations equal to the threshold. -/
theorem held_threshold_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ ≤ x i) :
    (∃ p ∈ Lomax.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ Transfer.ParetoII.logLik x μ q.1 q.2) Lomax.domain p) ↔
    (∀ i, μ < x i) ∧
      ∃ u ∈ Ioo 0 (Lomax.uBar (Transfer.ParetoII.excess x μ)),
        Lomax.psi (Transfer.ParetoII.excess x μ) u ≤ 0 := by
  by_cases h : ∀ i, μ < x i
  · simpa [h] using Transfer.ParetoII.exists_max_iff hn h
  · simp only [h, false_and, iff_false]
    rintro ⟨p, _, hmax⟩
    push Not at h
    obtain ⟨i, hi⟩ := h
    let d := Transfer.ParetoII.excess x μ
    have hz : ∃ i, d i = 0 := ⟨i, sub_eq_zero.mpr (le_antisymm hi (hx i))⟩
    obtain ⟨q, hq, hE⟩ := small_shape hz
    have hL : ∀ s, Transfer.ParetoII.logLik d (-0 * s) q s =
        Transfer.ParetoII.logLik x μ q s := by
      intro s
      simp [Transfer.ParetoII.logLik, Transfer.ParetoII.logDensity, d, Transfer.ParetoII.excess]
    have hlarge := (paretoII_scale_ray (d := d) (fun i ↦ sub_nonneg.mpr (hx i))
      (a := 0) (by norm_num) hE).eventually_gt_atTop (Transfer.ParetoII.logLik x μ p.1 p.2)
    have hpos : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
    obtain ⟨s, hgt, hs⟩ := (hlarge.and hpos).exists
    change Transfer.ParetoII.logLik x μ p.1 p.2 <
      Transfer.ParetoII.logLik d (-0 * s) q s at hgt
    rw [hL] at hgt
    exact hgt.not_ge (hmax (show (q, s) ∈ Lomax.domain from ⟨hq, hs⟩))

/-- With fixed positive scale and shape the closed sample minimum is
the unique maximizing threshold. -/
theorem held_scale_shape_threshold (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {q s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    ∃! t, t ∈ Ici (0 : ℝ) ∧
      IsMaxOn (fun u ↦ Transfer.ParetoII.logLik d (-u) q s) (Ici 0) t := by
  have heq : (fun u ↦ Transfer.ParetoII.logLik d (-u) q s) =
      Endpoint2FellerPareto.logLik d 1 q 1 s (log q - log s) := by
    funext u
    unfold Transfer.ParetoII.logLik Endpoint2FellerPareto.logLik
    apply Finset.sum_congr rfl
    intro i _
    simp [Transfer.ParetoII.logDensity, add_comm]
  rw [heq]
  exact Endpoint2FellerPareto.threshold_contact_unique hn hd hq one_pos hs

/-- At fixed scale, a sample with at least two different observations has a
unique finite threshold and shape fit. With all gaps zero the likelihood
instead grows without bound at closed contact. -/
theorem held_scale_fit (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {s : ℝ} (hs : 0 < s) :
    let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
    let L : ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) p.2 s
    ((∃ i, 0 < d i) → ∃! p, p ∈ D ∧ IsMaxOn L D p) ∧
      ((∀ i, d i = 0) → ¬ BddAbove (L '' D)) := by
  dsimp only
  let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
  let L : ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) p.2 s
  change ((∃ i, 0 < d i) → ∃! p, p ∈ D ∧ IsMaxOn L D p) ∧
    ((∀ i, d i = 0) → ¬ BddAbove (L '' D))
  let A : ℝ := ∑ i, log (1 + d i / s)
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  have hform (q : ℝ) : L (0, q) = (n : ℝ) * (log q - log s) - (q + 1) * A := by
    simp [L, A, Transfer.ParetoII.logLik, Transfer.ParetoII.logDensity,
      Finset.sum_sub_distrib, Finset.mul_sum, nsmul_eq_mul, mul_sub]
  constructor
  · rintro ⟨i, hi⟩
    have hA : 0 < A := by
      apply Finset.sum_pos'
      · exact fun j _ ↦ log_nonneg (by linarith [div_nonneg (hd j) hs.le])
      · exact ⟨i, Finset.mem_univ _, log_pos (by linarith [div_pos hi hs])⟩
    let q₀ : ℝ := n / A
    have hq₀ : 0 < q₀ := div_pos hnR hA
    have hbest (q : ℝ) (hq : 0 < q) (hne : q ≠ q₀) : L (0, q) < L (0, q₀) := by
      have ha : q₀ * A = n := div_mul_cancel₀ _ hA.ne'
      have hb : q / q₀ ≠ 1 := by
        intro he
        exact hne ((div_eq_iff hq₀.ne').mp he |>.trans (one_mul _))
      have hc := mul_lt_mul_of_pos_left
        (log_lt_sub_one_of_pos (div_pos hq hq₀) hb) hnR
      rw [log_div hq.ne' hq₀.ne'] at hc
      have hd' : (n : ℝ) * (q / q₀ - 1) = q * A - n := by
        rw [← ha]
        field_simp [hq₀.ne']
      rw [hd'] at hc
      rw [hform, hform]
      linarith
    have hcontact (t q : ℝ) (ht : 0 < t) (hq : 0 < q) : L (t, q) < L (0, q) := by
      let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
      apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
      intro j _
      have hlt : log (1 + d j / s) < log (1 + (d j - -t) / s) := by
        apply log_lt_log (by linarith [div_nonneg (hd j) hs.le])
        have := div_lt_div_of_pos_right (show d j < d j - -t by linarith) hs
        linarith
      change Transfer.ParetoII.logDensity (-t) q s (d j) <
        Transfer.ParetoII.logDensity (-0) q s (d j)
      simp only [Transfer.ParetoII.logDensity, neg_zero, sub_zero]
      linarith [mul_lt_mul_of_pos_left hlt (show 0 < q + 1 by linarith)]
    have hp₀ : (0, q₀) ∈ D := ⟨le_rfl, hq₀⟩
    have hstrict (p : ℝ × ℝ) (hp : p ∈ D) (hne : p ≠ (0, q₀)) : L p < L (0, q₀) := by
      rcases p with ⟨t, q⟩
      rcases eq_or_lt_of_le (show 0 ≤ t from hp.1) with rfl | ht
      · exact hbest q hp.2 (fun he ↦ hne (by rw [he]))
      · apply (hcontact t q ht hp.2).trans_le
        by_cases he : q = q₀
        · simp [he]
        · exact (hbest q hp.2 he).le
    refine ⟨(0, q₀), ⟨hp₀, ?_⟩, ?_⟩
    · intro p hp
      by_cases he : p = (0, q₀)
      · simp [he]
      · exact (hstrict p hp he).le
    · intro p hp
      by_contra! he
      exact (hstrict p hp.1 he).not_ge (hp.2 hp₀)
  · intro hz
    have hA : A = 0 := by simp [A, hz]
    rintro ⟨M, hM⟩
    let q : ℝ := exp ((M + 1) / n + log s)
    have hq : 0 < q := exp_pos _
    have hb : L (0, q) ≤ M := hM (mem_image_of_mem L (show (0, q) ∈ D from ⟨le_rfl, hq⟩))
    rw [hform, hA, mul_zero, sub_zero] at hb
    dsimp [q] at hb
    rw [log_exp, add_sub_cancel_right,
      mul_div_cancel₀ _ hnR.ne'] at hb
    linarith

private theorem contact_strictConcave {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hpos : ∃ i, 0 < d i) {q : ℝ} (hq : 0 < q) :
    StrictConcaveOn ℝ univ (fun z ↦ Transfer.ParetoII.logLik d 0 q (exp (-z))) := by
  let f : ℝ → ℝ := fun z ↦ Transfer.ParetoII.logLik d 0 q (exp (-z))
  let G : ℝ → ℝ := fun z ↦ (n : ℝ) -
    (q + 1) * ∑ i, (1 - (1 + exp z * d i)⁻¹)
  have hderiv (z : ℝ) : HasDerivAt f (G z) z := by
    have hterm (i : Fin n) : HasDerivAt (fun w ↦ log (1 + exp w * d i))
        (1 - (1 + exp z * d i)⁻¹) z := by
      have h : 0 < 1 + exp z * d i := by
        linarith [mul_nonneg (exp_pos z).le (hd i)]
      convert (((hasDerivAt_exp z).mul_const (d i)).const_add 1).log h.ne' using 1
      field_simp [h.ne']
      ring
    have hsum := HasDerivAt.sum (u := Finset.univ) (fun i _ ↦ hterm i)
    have hlin := ((hasDerivAt_id z).const_add (log q)).const_mul (n : ℝ)
    convert hlin.sub (hsum.const_mul (q + 1)) using 1
    · ext w
      simp only [f, Transfer.ParetoII.logLik, Transfer.ParetoII.logDensity, sub_zero,
        log_exp, sub_neg_eq_add, div_eq_mul_inv, ← exp_neg, neg_neg,
        Finset.sum_apply, Finset.sum_sub_distrib, ← Finset.mul_sum,
        Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
      simp only [Function.id_def, Pi.sub_apply, mul_comm (exp w)]
    · simp [G]
  apply StrictAnti.strictConcaveOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun z ↦ (hderiv z).continuousAt)
  intro x y hxy
  have hterm (i : Fin n) : 1 - (1 + exp x * d i)⁻¹ ≤ 1 - (1 + exp y * d i)⁻¹ := by
    apply sub_le_sub_left
    exact inv_le_inv₀
      (by linarith [mul_nonneg (exp_pos y).le (hd i)])
      (by linarith [mul_nonneg (exp_pos x).le (hd i)]) |>.mpr
      (by linarith [mul_le_mul_of_nonneg_right (exp_le_exp.mpr hxy.le) (hd i)])
  obtain ⟨i, hi⟩ := hpos
  have hlt : 1 - (1 + exp x * d i)⁻¹ < 1 - (1 + exp y * d i)⁻¹ := by
    apply sub_lt_sub_left
    exact inv_lt_inv₀
      (by linarith [mul_nonneg (exp_pos y).le hi.le])
      (by linarith [mul_nonneg (exp_pos x).le hi.le]) |>.mpr
      (by linarith [mul_lt_mul_of_pos_right (exp_lt_exp.mpr hxy) hi])
  have H := Finset.sum_lt_sum (s := Finset.univ) (fun i _ ↦ hterm i)
    ⟨i, Finset.mem_univ _, hlt⟩
  rw [(hderiv x).deriv, (hderiv y).deriv]
  exact sub_lt_sub_left (mul_lt_mul_of_pos_left H (by linarith)) _

/-- The fixed-shape threshold/scale fit is unique exactly when the
scale-collapse exponent is positive. Its threshold is closed contact.
For negative exponent the likelihood diverges, and at zero exponent the
finite contact limit strictly exceeds every feasible finite value. -/
theorem held_shape_fit (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {q : ℝ} (hq : 0 < q) :
    let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
    let L : ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) q p.2
    (0 < exponent d q → ∃ s > 0, IsMaxOn L D (0, s) ∧
      ∀ p ∈ D, IsMaxOn L D p → p = (0, s)) ∧
      (exponent d q < 0 → ¬ BddAbove (L '' D)) ∧
      (exponent d q = 0 → ∃ C : ℝ,
        Tendsto (fun s ↦ L (0, s)) (𝓝[>] 0) (𝓝 C) ∧ ∀ p ∈ D, L p < C) := by
  dsimp only
  let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
  let L : ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) q p.2
  have hstrict {t s : ℝ} (ht : 0 < t) (hs : 0 < s) : L (t, s) < L (0, s) := by
    let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
    apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
    intro i _
    have hlog : log (1 + d i / s) < log (1 + (d i - -t) / s) := by
      apply log_lt_log (by linarith [div_nonneg (hd i) hs.le])
      have := div_lt_div_of_pos_right (show d i < d i - -t by linarith) hs
      linarith
    change Transfer.ParetoII.logDensity (-t) q s (d i) <
      Transfer.ParetoII.logDensity (-0) q s (d i)
    simp only [Transfer.ParetoII.logDensity, neg_zero, sub_zero]
    linarith [mul_lt_mul_of_pos_left hlog (show 0 < q + 1 by linarith)]
  have hcontact {t s : ℝ} (ht : 0 ≤ t) (hs : 0 < s) : L (t, s) ≤ L (0, s) := by
    rcases eq_or_lt_of_le ht with rfl | h
    · exact le_rfl
    · exact (hstrict h hs).le
  have hpos (he : 0 ≤ exponent d q) : ∃ i, 0 < d i := by
    by_contra! h
    have hz (i : Fin n) : d i = 0 := le_antisymm (h i) (hd i)
    have hnR : (0 : ℝ) < n := by exact_mod_cast hn
    simp [exponent, hz, nsmul_eq_mul] at he
    linarith
  have heq {s : ℝ} (hs : 0 < s) :
      L (0, s) = exponent d q * log s + rayRemainder d 0 q s := by
    simpa [L] using ray_expansion hd (a := 0) (q := q) (by norm_num) hs
  have hlim := (continuousAt_rayRemainder hd 0 q).continuousWithinAt
    (s := Ioi 0)
  constructor
  · intro hE
    let f : ℝ → ℝ := fun z ↦ L (0, exp (-z))
    have hcv : StrictConcaveOn ℝ univ f := by
      simpa [f, L] using contact_strictConcave hd (hpos hE.le) hq
    have hright : Tendsto f atTop atBot := by
      have hs : Tendsto (fun z : ℝ ↦ exp (-z)) atTop (𝓝[>] 0) :=
        tendsto_nhdsWithin_iff.mpr ⟨tendsto_exp_neg_atTop_nhds_zero,
          Eventually.of_forall fun t ↦ exp_pos (-t)⟩
      apply Tendsto.congr' _ (((tendsto_log_nhdsGT_zero.const_mul_atBot hE).atBot_add
        hlim).comp hs)
      exact Eventually.of_forall fun z ↦ (heq (exp_pos (-z))).symm
    have hleft : Tendsto f atBot atBot := by
      have hnR : (0 : ℝ) < n := by exact_mod_cast hn
      apply tendsto_atBot_mono (g := f) (f := fun z ↦ (n : ℝ) * z + n * log q)
      · intro z
        change (∑ i, Transfer.ParetoII.logDensity (-0) q (exp (-z)) (d i)) ≤ _
        have hrhs : (n : ℝ) * z + n * log q =
            ∑ _ : Fin n, (log q + z) := by
          simp [nsmul_eq_mul]
          ring
        rw [hrhs]
        apply Finset.sum_le_sum
        intro i _
        have hh := log_nonneg (show 1 ≤ 1 + d i / exp (-z) by
          linarith [div_nonneg (hd i) (exp_pos (-z)).le])
        simp only [Transfer.ParetoII.logDensity, neg_zero, sub_zero, log_exp, sub_neg_eq_add]
        linarith [mul_nonneg (show 0 ≤ q + 1 by linarith) hh]
      · exact (tendsto_id.const_mul_atBot hnR).atBot_add tendsto_const_nhds
    have hcont : Continuous f := by
      refine continuous_finsetSum _ fun i _ ↦ ?_
      have harg : Continuous (fun z : ℝ ↦ 1 + d i / exp (-z)) := by
        exact continuous_const.add (continuous_const.div₀
          (continuous_exp.comp continuous_neg) (fun z ↦ (exp_pos (-z)).ne'))
      have hlog := harg.log (fun z ↦ (add_pos_of_pos_of_nonneg zero_lt_one
        (div_nonneg (hd i) (exp_pos (-z)).le)).ne')
      have hc : Continuous (fun z : ℝ ↦ log q + z - (q + 1) * log (1 + d i / exp (-z))) :=
        (continuous_const.add continuous_id).sub (continuous_const.mul hlog)
      simpa [L, Transfer.ParetoII.logDensity] using hc
    obtain ⟨a, ha⟩ := eventually_atBot.mp (hleft.eventually_lt_atBot (f 0))
    obtain ⟨b, hb⟩ := eventually_atTop.mp (hright.eventually_lt_atBot (f 0))
    let K := Icc (min 0 a) (max 0 b)
    have h0 : (0 : ℝ) ∈ K := ⟨min_le_left _ _, le_max_left _ _⟩
    obtain ⟨z, hz, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨0, h0⟩ hcont.continuousOn
    have hall : IsMaxOn f univ z := by
      intro w _
      by_cases hw : w ∈ K
      · exact hmax hw
      · have hlt : f w < f 0 := by
          by_cases hwa : min 0 a ≤ w
          · have hwb : max 0 b < w := lt_of_not_ge fun h ↦ hw ⟨hwa, h⟩
            exact hb w ((le_max_right _ _).trans hwb.le)
          · exact ha w ((lt_of_not_ge hwa).le.trans (min_le_right _ _))
        exact hlt.le.trans (hmax h0)
    let s := exp (-z)
    have hsp : 0 < s := exp_pos _
    have hMS : IsMaxOn L D (0, s) := by
      intro p hp
      apply (hcontact hp.1 hp.2).trans
      simpa [f, exp_log hp.2] using hall (mem_univ (-log p.2))
    refine ⟨s, hsp, hMS, ?_⟩
    rintro ⟨t, r⟩ hr hm
    have ht : t = 0 := by
      by_contra! ht'
      have hp := lt_of_le_of_ne hr.1 (Ne.symm ht')
      exact (hstrict hp hr.2).not_ge (hm (show (0, r) ∈ D from ⟨le_rfl, hr.2⟩))
    subst t
    have hmr : IsMaxOn f univ (-log r) := by
      intro u _
      simpa [f, L, exp_log hr.2] using hm (show (0, exp (-u)) ∈ D from ⟨le_rfl, exp_pos _⟩)
    have he := hcv.eq_of_isMaxOn hmr hall (mem_univ _) (mem_univ _)
    have hr' := congrArg (fun u : ℝ ↦ exp (-u)) he
    exact Prod.ext rfl (by simpa [s, exp_log hr.2] using hr')
  · constructor
    · intro hE
      rintro ⟨M, hM⟩
      have hh := (paretoII_scale_ray hd (a := 0) (by norm_num) hE).eventually_gt_atTop M
      have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
      obtain ⟨s, hlt, hs⟩ := (hh.and hs).exists
      have hle : L (0, s) ≤ M := hM (mem_image_of_mem L (show (0, s) ∈ D from ⟨le_rfl, hs⟩))
      exact (show M < L (0, s) by simpa [L] using hlt).not_ge hle
    · intro hE
      refine ⟨rayRemainder d 0 q 0, ?_, ?_⟩
      · apply Tendsto.congr' _ hlim
        filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
        change rayRemainder d 0 q s = L (0, s)
        rw [heq hs, hE, zero_mul, zero_add]
      · intro p hp
        apply (hcontact hp.1 hp.2).trans_lt
        rw [heq hp.2, hE, zero_mul, zero_add]
        obtain ⟨j, hj⟩ := hpos hE.ge
        have ht (i : Fin n) :
            log q - (q + 1) * (if d i = 0 then log (1 + 0) else log ((1 + 0) * p.2 + d i))
              ≤ log q - (q + 1) * (if d i = 0 then log (1 + 0) else log ((1 + 0) * 0 + d i)) := by
          by_cases hi : d i = 0
          · simp [hi]
          · have hp' : 0 < p.2 := hp.2
            have hh := log_le_log (lt_of_le_of_ne (hd i) (Ne.symm hi))
              (show d i ≤ p.2 + d i by linarith)
            simp only [hi, ite_false, add_zero, one_mul, mul_zero, zero_add]
            linarith [mul_le_mul_of_nonneg_left hh (show 0 ≤ q + 1 by linarith)]
        apply Finset.sum_lt_sum (fun i _ ↦ ht i)
        refine ⟨j, Finset.mem_univ _, ?_⟩
        simp only [hj.ne', ite_false, add_zero, one_mul, mul_zero, zero_add]
        have hp' : 0 < p.2 := hp.2
        linarith [mul_lt_mul_of_pos_left (log_lt_log hj (show d j < p.2 + d j by linarith))
          (show 0 < q + 1 by linarith)]


/-- At equality in the scale-collapse exponent the contact limit is
the exact, strictly unattained supremum. -/
theorem held_shape_zero_exponent_sup (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {q : ℝ} (hq : 0 < q) (hE : exponent d q = 0) :
    let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
    let L : ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) q p.2
    ∃ C : ℝ, IsLUB (L '' D) C ∧ ∀ p ∈ D, L p < C := by
  intro D L
  obtain ⟨C, hpath, hlt⟩ := (held_shape_fit hn hd hq).2.2 hE
  refine ⟨C, ⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p,hp,rfl⟩
    exact (hlt p hp).le
  · intro B hB
    exact Generic.limit_le_upperBound
      (p := fun s : ℝ ↦ (0,s))
      ((show ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s from self_mem_nhdsWithin).mono
        (fun s hs ↦ show (0,s) ∈ D from ⟨le_rfl,hs⟩)) hpath hB

/-- The exact three-level Lomax crossing transfers to a held Pareto II
threshold of zero. Pareto II uses reciprocal scale coordinates. -/
theorem held_threshold_three_level_tie :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p q : ℝ × ℝ, p ≠ q ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.ParetoII.logLik x 0 b.1 b.2) Lomax.domain v} = {p, q} := by
  rcases TiesLomax.three_level_global_tie with
    ⟨a, ⟨ha, p, hp, q, hq, hps, hqs, hpm, hqm, hall, _⟩, _⟩
  let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
  let φ (v : ℝ × ℝ) : ℝ × ℝ := (v.1, v.2⁻¹)
  have hinv (v : ℝ × ℝ) : φ (φ v) = v := by simp [φ]
  have hdom (v : ℝ × ℝ) (hv : v ∈ Lomax.domain) : φ v ∈ Lomax.domain :=
    ⟨hv.1, inv_pos.mpr hv.2⟩
  have hφ : BijOn φ Lomax.domain Lomax.domain := by
    refine ⟨hdom, ?_, ?_⟩
    · intro v _ w _ h
      simpa only [hinv] using congrArg φ h
    · exact fun v hv ↦ ⟨φ v, hdom v hv, hinv v⟩
  have hmap (v : ℝ × ℝ) (hv : v ∈ Lomax.domain) :
      IsMaxOn (fun b : ℝ × ℝ ↦ Transfer.ParetoII.logLik x 0 b.1 b.2) Lomax.domain v ↔
        IsMaxOn (fun b : ℝ × ℝ ↦ Lomax.logLik x b.1 b.2) Lomax.domain (φ v) := by
    apply Transfer.isMaxOn_iff (c := 0) hφ _ hv
    intro b _
    have he : Transfer.ParetoII.excess x 0 = x := funext fun i ↦
      show x i - 0 = x i from sub_zero _
    simp [Transfer.ParetoII.logLik_transfer, he, φ]
  refine ⟨a, ha, φ p, φ q, ?_, ?_⟩
  · intro he
    have hpq : p = q := hφ.injOn hp hq he
    exact (hps.trans hqs).ne (congrArg Prod.snd hpq)
  · ext v
    simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
    constructor
    · rintro ⟨hv, hm⟩
      rcases hall (φ v) (hdom v hv) ((hmap v hv).mp hm) with he | he
      · exact Or.inl (by simpa only [hinv] using congrArg φ he)
      · exact Or.inr (by simpa only [hinv] using congrArg φ he)
    · rintro (rfl | rfl)
      · exact ⟨hdom p hp, (hmap _ (hdom p hp)).mpr (by simpa only [hinv] using hpm)⟩
      · exact ⟨hdom q hq, (hmap _ (hdom q hq)).mpr (by simpa only [hinv] using hqm)⟩

end

end UnivariateML.Endpoint2ParetoII

end
