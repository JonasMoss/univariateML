module

public import UnivariateML.Family.Gamma
public import UnivariateML.Family.Beta

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Digamma differences for beta profiles

Comparison with the corresponding logarithmic ratio follows by
telescoping the Gamma score gap. This gives a finite-difference form of
the trigamma lower bound without choosing a series representation for
the trigamma function.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.GBBetaCurvature

private theorem logGamma_deriv {x : ℝ} (hx : 0 < x) :
    HasDerivAt (log ∘ Real.Gamma) (Gamma.digamma x) x :=
  (((hasDerivAt_Gamma_of_pos hx).log (Gamma_pos_of_pos hx).ne').differentiableAt).hasDerivAt

private theorem digamma_step {x : ℝ} (hx : 0 < x) :
    Gamma.digamma (x + 1) = x⁻¹ + Gamma.digamma x := by
  have h := (logGamma_deriv (show 0 < x + 1 by positivity)).comp x
    ((hasDerivAt_id x).add_const 1)
  have h' := (hasDerivAt_log hx.ne').add (logGamma_deriv hx)
  have he : (fun t ↦ log t + (log ∘ Real.Gamma) t) =ᶠ[𝓝 x]
      (log ∘ Real.Gamma) ∘ (fun t ↦ t + 1) := by
    filter_upwards [Ioi_mem_nhds hx] with t ht
    change log t + log (Real.Gamma t) = log (Real.Gamma (t + 1))
    rw [Gamma_add_one ht.ne', log_mul ht.ne' (Gamma_pos_of_pos ht).ne']
  simpa using h.unique (h'.congr_of_eventuallyEq he.symm)

private noncomputable def step (x : ℝ) : ℝ := x⁻¹ - (log (x + 1) - log x)

private theorem score_gap_step {x : ℝ} (hx : 0 < x) :
    Gamma.scoreGap x = Gamma.scoreGap (x + 1) + step x := by
  rw [Gamma.scoreGap, Gamma.scoreGap, digamma_step hx, step]
  ring

private theorem step_deriv {x : ℝ} (hx : 0 < x) :
    HasDerivAt step (-(1 / (x ^ 2 * (x + 1)))) x := by
  have h := (hasDerivAt_inv hx.ne').sub
    ((((hasDerivAt_id x).add_const 1).log (show x + 1 ≠ 0 by positivity)).sub
      (hasDerivAt_log hx.ne'))
  convert h using 1
  · rfl
  · simp only [Function.id_def]
    field_simp
    ring

private theorem step_diff_antitone {a : ℝ} (ha : 0 < a) :
    StrictAntiOn (fun x ↦ step x - step (x + a)) (Ioi 0) := by
  have hd (x : ℝ) (hx : 0 < x) :
      HasDerivAt (fun x ↦ step x - step (x + a))
        (-(1 / (x ^ 2 * (x + 1))) + 1 / ((x + a) ^ 2 * (x + a + 1))) x := by
    simpa [Pi.sub_def, Function.comp_def, sub_neg_eq_add] using (step_deriv hx).sub
      ((step_deriv (add_pos hx ha)).comp x ((hasDerivAt_id x).add_const a))
  apply strictAntiOn_of_deriv_neg (convex_Ioi _)
    (fun x hx ↦ (hd x hx).continuousAt.continuousWithinAt)
  rw [interior_Ioi]
  intro x hx
  rw [(hd x hx).deriv]
  have hx₀ : 0 < x := hx
  have hlo : 0 < x ^ 2 * (x + 1) := by positivity
  have hlt : x ^ 2 * (x + 1) < (x + a) ^ 2 * (x + a + 1) := by
    have h₁ : x ^ 2 < (x + a) ^ 2 := by nlinarith
    exact mul_lt_mul_of_pos_of_nonneg' h₁ (by linarith) (by positivity) (sq_nonneg _)
  linarith [one_div_lt_one_div_of_lt hlo hlt]

private theorem gap_diff_antitone {a : ℝ} (ha : 0 < a) :
    StrictAntiOn (fun x ↦ Gamma.scoreGap x - Gamma.scoreGap (x + a)) (Ioi 0) := by
  intro x hx y hy hxy
  let G := fun z ↦ Gamma.scoreGap z - Gamma.scoreGap (z + a)
  let d : ℕ → ℝ := fun m ↦ G (x + m) - G (y + m)
  have hs (m : ℕ) : d (m + 1) < d m := by
    have hx' : 0 < x + (m : ℝ) := add_pos_of_pos_of_nonneg hx (Nat.cast_nonneg m)
    have hy' : 0 < y + (m : ℝ) := add_pos_of_pos_of_nonneg hy (Nat.cast_nonneg m)
    have h := step_diff_antitone ha hx' hy' (add_lt_add_left hxy _)
    dsimp [d, G] at *
    push_cast
    rw [← add_assoc x, ← add_assoc y,
      add_right_comm (x + (m : ℝ)) 1 a, add_right_comm (y + (m : ℝ)) 1 a]
    linarith [score_gap_step hx', score_gap_step hy',
      score_gap_step (add_pos hx' ha), score_gap_step (add_pos hy' ha)]
  have hg (z : ℝ) : Tendsto (fun m : ℕ ↦ Gamma.scoreGap (z + m)) atTop (𝓝 0) :=
    Gamma.scoreGap_tendsto_atTop.comp
      (tendsto_atTop_add_const_left atTop z tendsto_natCast_atTop_atTop)
  have hg' (z : ℝ) : Tendsto (fun m : ℕ ↦ G (z + m)) atTop (𝓝 0) := by
    have h := (hg z).sub (hg (z + a))
    simpa [G, add_right_comm z a] using h
  have hd : Tendsto d atTop (𝓝 0) := by simpa [d] using (hg' x).sub (hg' y)
  have hanti : Antitone d := antitone_nat_of_succ_le fun m ↦ (hs m).le
  have hle : 0 ≤ d 1 := le_of_tendsto hd (by
    filter_upwards [eventually_ge_atTop (1 : ℕ)] with m hm using hanti hm)
  have hlt : 0 < d 0 := hle.trans_lt (hs 0)
  simpa [d, G, sub_pos] using hlt

/-- The digamma difference decreases strictly faster than the logarithm
of the corresponding positive shape ratio. -/
theorem digamma_difference_strict {a x y : ℝ} (ha : 0 < a) (hx : 0 < x) (hxy : x < y) :
    (log (x + a) - log x) - (log (y + a) - log y) <
      (Gamma.digamma (x + a) - Gamma.digamma x) -
      (Gamma.digamma (y + a) - Gamma.digamma y) := by
  have h := gap_diff_antitone ha hx (hx.trans hxy) hxy
  dsimp [Gamma.scoreGap] at h
  linarith

noncomputable section

variable {n : ℕ}

/-- The beta likelihood in a positive endpoint slack and a free first
shape, after dropping terms constant in those two coordinates. -/
def endpointProfile (d : Fin n → ℝ) (q : ℝ) (v : ℝ × ℝ) : ℝ :=
  -(n : ℝ) * Beta.logBeta (v.1, q) - v.1 * ((n : ℝ) * v.2 + ∑ i, d i) +
    (q - 1) * ∑ i, log (1 - exp (-(v.2 + d i)))

private def odds (t : ℝ) : ℝ := 1 / (exp t - 1)

private theorem odds_pos {t : ℝ} (ht : 0 < t) : 0 < odds t :=
  div_pos one_pos (sub_pos.mpr (one_lt_exp_iff.mpr ht))

private theorem odds_deriv {t : ℝ} (ht : 0 < t) :
    HasDerivAt odds (-(odds t * (1 + odds t))) t := by
  have h := (hasDerivAt_const t (1 : ℝ)).div ((hasDerivAt_exp t).sub_const 1)
    (sub_pos.mpr (one_lt_exp_iff.mpr ht)).ne'
  convert h using 1
  · rfl
  · unfold odds
    field_simp [ne_of_gt (sub_pos.mpr (one_lt_exp_iff.mpr ht))]
    ring

private theorem log_gap_deriv {t : ℝ} (ht : 0 < t) :
    HasDerivAt (fun u ↦ log (1 - exp (-u))) (odds t) t := by
  have hgap : 0 < 1 - exp (-t) := sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos ht))
  have h := ((((hasDerivAt_id t).neg.exp).const_sub 1).log hgap.ne')
  convert h using 1
  · rfl
  · simp only [Function.id_def, mul_neg, mul_one, neg_neg]
    change 1 / (exp t - 1) = exp (-t) / (1 - exp (-t))
    rw [exp_neg]
    field_simp [exp_ne_zero, ne_of_gt (sub_pos.mpr (one_lt_exp_iff.mpr ht))]

private def rate (d : Fin n → ℝ) (q c : ℝ) : ℝ :=
  (q - 1) * (∑ i, odds (c + d i)) / n

private theorem rate_pos (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {q c : ℝ} (hq : 1 < q) (hc : 0 < c) : 0 < rate d q c := by
  apply div_pos (mul_pos (sub_pos.mpr hq) _) (Nat.cast_pos.mpr hn)
  exact Finset.sum_pos (fun i _ ↦ odds_pos (add_pos_of_pos_of_nonneg hc (hd i)))
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩

private theorem rate_deriv {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {q c : ℝ} (hc : 0 < c) :
    HasDerivAt (rate d q) (-rate d q c -
      (q - 1) * (∑ i, (odds (c + d i)) ^ 2) / n) c := by
  have h (i : Fin n) := (odds_deriv (add_pos_of_pos_of_nonneg hc (hd i))).comp c
    ((hasDerivAt_id c).add_const (d i))
  have H := ((HasDerivAt.sum (u := Finset.univ) (fun i _ ↦ h i)).const_mul (q - 1)).div_const
    (n : ℝ)
  convert H using 1
  · ext t
    simp [rate, Finset.sum_apply, Function.comp_def]
  · simp only [rate, mul_one, mul_add, Finset.sum_neg_distrib, Finset.sum_add_distrib,
      pow_two]
    ring

private theorem rate_strictAnti (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {q : ℝ} (hq : 1 < q) : StrictAntiOn (rate d q) (Ioi 0) := by
  apply strictAntiOn_of_deriv_neg (convex_Ioi _)
    (fun c hc ↦ (rate_deriv (q := q) hd hc).continuousAt.continuousWithinAt)
  rw [interior_Ioi]
  intro c hc
  rw [(rate_deriv hd hc).deriv]
  have h := rate_pos hn hd hq hc
  have hq' := sub_pos.mpr hq
  have hs : 0 ≤ ∑ i, odds (c + d i) ^ 2 := Finset.sum_nonneg (fun i _ ↦ sq_nonneg _)
  linarith [div_nonneg (mul_nonneg hq'.le hs) (Nat.cast_nonneg n)]

private theorem rate_odds_mono (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {q : ℝ} (hq : 1 < q) :
    MonotoneOn (fun c ↦ log (1 + (q - 1) / rate d q c) - c) (Ioi 0) := by
  let k := q - 1
  have hk : 0 < k := sub_pos.mpr hq
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  let V := fun c ↦ k * (∑ i, (odds (c + d i)) ^ 2) / n
  have hder (c : ℝ) (hc : 0 < c) :
      HasDerivAt (fun c ↦ log (1 + k / rate d q c) - c)
        (k * (rate d q c + V c) / (rate d q c * (rate d q c + k)) - 1) c := by
    have hr := rate_pos hn hd hq hc
    have h := ((hasDerivAt_const c k).div (rate_deriv (q := q) hd hc) hr.ne').const_add 1
    have harg : 0 < 1 + k / rate d q c := by positivity
    convert (h.log harg.ne').sub (hasDerivAt_id c) using 1
    · rfl
    · dsimp [V, k] at *
      field_simp [hr.ne', ne_of_gt (add_pos hr hk), harg.ne']
      ring
  apply monotoneOn_of_deriv_nonneg (convex_Ioi _)
    (fun c hc ↦ (hder c hc).continuousAt.continuousWithinAt)
    (fun c hc ↦ (hder c (by simpa using hc)).differentiableAt.differentiableWithinAt)
  rw [interior_Ioi]
  intro c hc
  rw [(hder c hc).deriv, sub_nonneg, le_div_iff₀
    (mul_pos (rate_pos hn hd hq hc) (add_pos (rate_pos hn hd hq hc) hk))]
  have hsq := Finset.sum_mul_sq_le_sq_mul_sq (Finset.univ : Finset (Fin n))
    (fun i ↦ odds (c + d i)) (fun _ ↦ (1 : ℝ))
  simp only [mul_one, one_pow, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul] at hsq
  have H : (rate d q c) ^ 2 ≤ k * V c := by
    change (k * (∑ i, odds (c + d i)) / n) ^ 2 ≤
      k * (k * (∑ i, (odds (c + d i)) ^ 2) / n)
    rw [div_pow]
    apply (div_le_iff₀ (pow_pos hN 2)).mpr
    rw [mul_pow]
    have he : k * (k * (∑ i, (odds (c + d i)) ^ 2) / n) * (n : ℝ) ^ 2 =
        k ^ 2 * ((∑ i, (odds (c + d i)) ^ 2) * n) := by
      field_simp
    rw [he]
    exact mul_le_mul_of_nonneg_left hsq (sq_nonneg _)
  nlinarith

private theorem score_at_max (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {q : ℝ} (hq : 1 < q) {v : ℝ × ℝ} (hv : v ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ))
    (hm : IsMaxOn (endpointProfile d q) (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) v) :
    v.1 = rate d q v.2 ∧
      Gamma.digamma (v.1 + q) - Gamma.digamma v.1 = v.2 + (∑ i, d i) / n := by
  have hp : 0 < v.1 := hv.1
  have hc : 0 < v.2 := hv.2
  have hq' := zero_lt_one.trans hq
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have h₁ : IsLocalMax (fun c ↦ endpointProfile d q (v.1, c)) v.2 := by
    filter_upwards [Ioi_mem_nhds hc] with c hc
    exact hm (show (v.1, c) ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) from ⟨hp, hc⟩)
  have h₂ : IsLocalMax (fun p ↦ endpointProfile d q (p, v.2)) v.1 := by
    filter_upwards [Ioi_mem_nhds hp] with p hp
    exact hm (show (p, v.2) ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) from ⟨hp, hc⟩)
  have hd₁ : HasDerivAt (fun c ↦ endpointProfile d q (v.1, c))
      (-(v.1 * n) + (q - 1) * ∑ i, odds (v.2 + d i)) v.2 := by
    have h (i : Fin n) := (log_gap_deriv (add_pos_of_pos_of_nonneg hc (hd i))).comp v.2
      ((hasDerivAt_id v.2).add_const (d i))
    have ht := ((hasDerivAt_id v.2).const_mul (n : ℝ)).add_const (∑ i, d i)
    have hleft := (ht.const_mul (-v.1)).const_add (-(n : ℝ) * Beta.logBeta (v.1, q))
    convert hleft.add ((HasDerivAt.sum (u := Finset.univ) (fun i _ ↦ h i)).const_mul (q - 1))
      using 1
    · ext c
      simp [endpointProfile, Function.comp_def, Finset.sum_apply]
      ring
    · simp
  have hd₂ : HasDerivAt (fun p ↦ endpointProfile d q (p, v.2))
      ((n : ℝ) * (Gamma.digamma (v.1 + q) - Gamma.digamma v.1) -
        ((n : ℝ) * v.2 + ∑ i, d i)) v.1 := by
    have hG := ((logGamma_deriv (add_pos hp hq')).comp v.1
      ((hasDerivAt_id v.1).add_const q)).sub (logGamma_deriv hp)
    have ht := (((hG.sub_const (log (Real.Gamma q))).const_mul (n : ℝ)).sub
      ((hasDerivAt_id v.1).mul_const ((n : ℝ) * v.2 + ∑ i, d i))).add_const
      ((q - 1) * ∑ i, log (1 - exp (-(v.2 + d i))))
    simp only [mul_one, one_mul] at ht
    apply ht.congr_of_eventuallyEq
    filter_upwards [Ioi_mem_nhds hp] with p hp'
    have hβ : -Beta.logBeta (p, q) =
        log (Real.Gamma (p + q)) - log (Real.Gamma p) - log (Real.Gamma q) := by
      unfold Beta.logBeta Beta.normalizer
      dsimp
      rw [log_div (mul_ne_zero (Gamma_pos_of_pos hp').ne' (Gamma_pos_of_pos hq').ne')
        (Gamma_pos_of_pos (add_pos hp' hq')).ne',
        log_mul (Gamma_pos_of_pos hp').ne' (Gamma_pos_of_pos hq').ne']
      ring
    dsimp [endpointProfile, Function.comp_def]
    rw [← hβ]
    ring
  have hz₁ := h₁.hasDerivAt_eq_zero hd₁
  have hz₂ := h₂.hasDerivAt_eq_zero hd₂
  constructor
  · dsimp [rate]
    exact (eq_div_iff hN.ne').mpr (by linarith)
  · have he : ((n : ℝ) * v.2 + ∑ i, d i) / n = v.2 + (∑ i, d i) / n := by
      rw [add_div, mul_comm, mul_div_cancel_right₀ _ hN.ne']
    rw [← he, eq_div_iff hN.ne']
    linarith

/-- A regular free endpoint and free first beta shape have at most one
maximizer. The proof compares finite score differences at the would-be
maximizers, so it does not differentiate the conditional shape profile. -/
theorem endpoint_profile_at_most_one (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {q : ℝ} (hq : 1 < q) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    ∀ v ∈ D, ∀ w ∈ D, IsMaxOn (endpointProfile d q) D v →
      IsMaxOn (endpointProfile d q) D w → v = w := by
  intro D v hv w hw hm hm'
  obtain ⟨hvr, hvs⟩ := score_at_max hn hd hq hv hm
  obtain ⟨hwr, hws⟩ := score_at_max hn hd hq hw hm'
  have hcontra (v w : ℝ × ℝ) (hv : v ∈ D) (hw : w ∈ D)
      (hvr : v.1 = rate d q v.2) (hwr : w.1 = rate d q w.2)
      (hvs : Gamma.digamma (v.1 + q) - Gamma.digamma v.1 = v.2 + (∑ i, d i) / n)
      (hws : Gamma.digamma (w.1 + q) - Gamma.digamma w.1 = w.2 + (∑ i, d i) / n)
      (hvw : v.2 < w.2) : False := by
    have hb : w.1 < v.1 := by
      rw [hwr, hvr]
      exact rate_strictAnti hn hd hq hv.2 hw.2 hvw
    have hp : 0 < w.1 := hw.1
    have hp' : 0 < v.1 := hv.1
    have hmono := rate_odds_mono hn hd hq hv.2 hw.2 hvw.le
    dsimp only at hmono
    rw [← hwr, ← hvr] at hmono
    have hδ (b : ℝ) (hb : 0 < b) (u : ℝ) (hu : 0 < u) :
        log (1 + u / b) = log (b + u) - log b := by
      rw [← log_div (add_pos hb hu).ne' hb.ne']
      congr 1
      field_simp
    have hg := digamma_difference_strict (zero_lt_one.trans hq) hp hb
    rw [hws, hvs] at hg
    rw [← hδ _ hp _ (zero_lt_one.trans hq), ← hδ _ hp' _ (zero_lt_one.trans hq)] at hg
    have hcmp : log (1 + (q - 1) / w.1) - log (1 + (q - 1) / v.1) ≤
        log (1 + q / w.1) - log (1 + q / v.1) := by
      rw [hδ _ hp _ (sub_pos.mpr hq), hδ _ hp' _ (sub_pos.mpr hq),
        hδ _ hp _ (zero_lt_one.trans hq), hδ _ hp' _ (zero_lt_one.trans hq)]
      have h := log_le_log (div_pos (add_pos hp' (zero_lt_one.trans hq))
          (add_pos hp' (sub_pos.mpr hq)))
        (show (v.1 + q) / (v.1 + (q - 1)) ≤
            (w.1 + q) / (w.1 + (q - 1)) by
          apply (div_le_div_iff₀ (add_pos hp' (sub_pos.mpr hq))
            (add_pos hp (sub_pos.mpr hq))).mpr
          nlinarith)
      rw [log_div (add_pos hp (zero_lt_one.trans hq)).ne' (add_pos hp (sub_pos.mpr hq)).ne',
        log_div (add_pos hp' (zero_lt_one.trans hq)).ne' (add_pos hp' (sub_pos.mpr hq)).ne'] at h
      linarith
    linarith
  have hc : v.2 = w.2 := le_antisymm
    (le_of_not_gt (hcontra w v hw hv hwr hvr hws hvs))
    (le_of_not_gt (hcontra v w hv hw hvr hwr hvs hws))
  exact Prod.ext (by rw [hvr, hwr, hc]) hc

end

end UnivariateML.GBBetaCurvature

end
