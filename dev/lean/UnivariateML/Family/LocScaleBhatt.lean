module

public import UnivariateML.Family.HeldSweepBhatt

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Bhattacharjee location–scale fits

The zero-width member is included. Uniform traces at positive held width
retain the contact factors when the data touch one or both endpoints.

## Deviations

For free width and scale, the constant branch and the moving uniform path
are proved here. The finite stationary-width comparison and the exact
crossing samples require additional profile results. The positive
held-width free-scale uniqueness cases and the variance detector at
held scale are also outside these theorems.
-/

@[expose] public section

open Set Filter Real
open scoped Topology
open UnivariateML.SkewLSBhatt UnivariateML.SkewLSGaussian UnivariateML.HeldSweepBhatt

namespace UnivariateML.LocScaleBhatt

/-- Width zero includes the normal collapse on every nonempty constant
sample, so the fully free likelihood is unbounded. -/
theorem all_free_constant_failure {n : ℕ} (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    let S := Ici (0 : ℝ) ×ˢ Normal.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    ¬ BddAbove (L '' S) := by
  let x : Fin n → ℝ := fun _ ↦ m
  let S := Ici (0 : ℝ) ×ˢ Normal.domain
  let L (p : ℝ × (ℝ × ℝ)) := likelihood x p.1 p.2.1 p.2.2
  have hsub : likelihood x 0 m '' Ioi 0 ⊆ L '' S := by
    rintro _ ⟨s,hs,rfl⟩
    exact ⟨(0,m,s), ⟨le_rfl, mem_univ _, hs⟩, rfl⟩
  exact fun h ↦ constant_normal_seam hn m (h.mono hsub)

/-- For nonnegative held width, attainment at zero width is precisely the
ordinary normal criterion. At positive width the range must exceed twice
the held width. -/
theorem held_width_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ} (ha : 0 ≤ a) :
    (∃ p ∈ Normal.domain,
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) Normal.domain p) ↔
        if a = 0 then (∃ i j, x i ≠ x j) else (∃ i j, 2*a < x j - x i) := by
  rcases ha.eq_or_lt with h | h
  · subst a
    simp only [ite_true]
    constructor
    · rintro ⟨p,hp,hm⟩
      by_contra hx
      exact Normal.constant_unbounded hn hx ⟨_, ((normal_member_argmax x hp).mp hm).isLUB hp |>.1⟩
    · intro hx
      obtain ⟨p,⟨hp,hm⟩,_⟩ := (Normal.existsUnique_mle_iff hn x).mpr hx
      exact ⟨p,hp,(normal_member_argmax x hp).mpr hm⟩
  · rw [ite_eq_right h.ne']
    exact positive_width_iff hn x h

/-- When the held positive width strictly covers the sample range, the
unpenalized uniform likelihood is the exact supremum and every finite fit
loses strictly. -/
theorem held_width_strict_cover_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 < a) {i j : Fin n} (hlo : ∀ k, x i ≤ x k) (hhi : ∀ k, x k ≤ x j)
    (hspan : x j - x i < 2*a) :
    let L := fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2
    let B := (1 / (2*a))^n
    IsLUB (L '' Normal.domain) B ∧ ∀ p ∈ Normal.domain, L p < B := by
  let L (p : ℝ × ℝ) := likelihood x a p.1 p.2
  let B := (1 / (2*a))^n
  change IsLUB (L '' Normal.domain) B ∧ ∀ p ∈ Normal.domain, L p < B
  let c := (x i + x j)/2
  have hx (k : Fin n) : |x k-c| < a := abs_lt.mpr (by
    dsimp [c]
    constructor <;> linarith [hlo k, hhi k])
  obtain ⟨hB, hlt⟩ := strict_cover_uniform hn ha hx
  refine ⟨⟨?_, ?_⟩, fun p hp ↦ hlt p.1 p.2 hp.2⟩
  · rintro _ ⟨p,hp,rfl⟩
    exact (hlt p.1 p.2 hp.2).le
  · intro z h
    apply hB.2
    rintro _ ⟨s,hs,rfl⟩
    have hcs : (c,s) ∈ Normal.domain := ⟨mem_univ _,hs⟩
    change L (c,s) ≤ z
    exact h (mem_image_of_mem L hcs)

/-- At held width zero and held centre, a finite scale fit exists exactly
when at least one residual is nonzero, and the fit is unique. -/
theorem held_zero_width_centre_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    (∃! s, s ∈ Ioi 0 ∧ IsMaxOn (likelihood x 0 m) (Ioi 0) s) ↔ ∃ j, x j ≠ m := by
  have he (s : ℝ) : (s ∈ Ioi 0 ∧ IsMaxOn (likelihood x 0 m) (Ioi 0) s) ↔
      (s ∈ Ioi 0 ∧ IsMaxOn (Normal.logLik x m) (Ioi 0) s) := by
    constructor <;> rintro ⟨hs,hm⟩ <;> refine ⟨hs, fun t ht ↦ ?_⟩
    · change Normal.logLik x m t ≤ Normal.logLik x m s
      rw [← (normal_member_log x m ht).2, ← (normal_member_log x m hs).2]
      exact log_le_log (normal_member_log x m ht).1 (hm ht)
    · change likelihood x 0 m t ≤ likelihood x 0 m s
      apply (log_le_log_iff (normal_member_log x m ht).1 (normal_member_log x m hs).1).mp
      rw [(normal_member_log x m ht).2, (normal_member_log x m hs).2]
      exact hm ht
  simp only [he, Normal.existsUnique_heldMean_iff hn x m]

private theorem outside_zero_limit {n : ℕ} (x : Fin n → ℝ) {a m : ℝ}
    (ha : 0 < a) {i : Fin n} (hi : a < |x i - m|) :
    Tendsto (likelihood x a m) (𝓝[>] 0) (𝓝 0) := by
  have hf (s : ℝ) : likelihood x a m s =
      ∏ j, (Φ ((x j - m + a) / s) - Φ ((x j - m - a) / s)) / (2*a) := by
    simp only [likelihood, ha.ne', ite_false]
  rw [funext hf]
  have hterm (b : ℝ) :
      Tendsto (fun s : ℝ ↦ Φ (b/s)) (𝓝[>] 0) (𝓝 (if 0 < b then 1 else if b < 0 then 0 else Φ 0)) := by
    rcases lt_trichotomy 0 b with hb | hb | hb
    · simpa only [hb, ite_true, div_eq_mul_inv, Function.comp_def] using
        cdf_atTop.comp (tendsto_inv_nhdsGT_zero.const_mul_atTop hb)
    · subst b
      simp
    · simpa only [hb, not_lt_of_ge hb.le, ite_false, ite_true, div_eq_mul_inv, Function.comp_def] using
        cdf_atBot.comp (tendsto_inv_nhdsGT_zero.const_mul_atTop_of_neg hb)
  let F (b : ℝ) := if 0 < b then (1 : ℝ) else if b < 0 then 0 else Φ 0
  have hlim := tendsto_finsetProd (s := Finset.univ) fun j (_ : j ∈ Finset.univ) ↦
    ((hterm (x j-m+a)).sub (hterm (x j-m-a))).div_const (2*a)
  have hz : (∏ j, (F (x j-m+a) - F (x j-m-a)) / (2*a)) = 0 := by
    apply Finset.prod_eq_zero (Finset.mem_univ i)
    rcases lt_or_ge (x i-m) 0 with h | h
    · rw [abs_of_neg h] at hi
      have h₁ : x i-m+a < 0 := by linarith
      have h₂ : x i-m-a < 0 := by linarith
      simp [F, h₁, h₂, not_lt_of_ge h₁.le, not_lt_of_ge h₂.le]
    · rw [abs_of_nonneg h] at hi
      have h₁ : 0 < x i-m+a := by linarith
      have h₂ : 0 < x i-m-a := by linarith
      simp [F, h₁, h₂]
  change Tendsto _ _ (𝓝 (∏ j, (F (x j-m+a) - F (x j-m-a)) / (2*a))) at hlim
  rwa [hz] at hlim

/-- At positive held width and any fixed centre, the zero-scale trace
includes one half at every endpoint contact and vanishes if any
observation is outside the closed support. -/
theorem held_positive_width_uniform_trace {n : ℕ} (x : Fin n → ℝ) {a m : ℝ} (ha : 0 < a) :
    Tendsto (likelihood x a m) (𝓝[>] 0) (𝓝
      (if ∀ i, |x i-m| ≤ a then ∏ i, (if |x i-m| = a then (1 : ℝ)/2 else 1)/(2*a) else 0)) := by
  by_cases hx : ∀ i, |x i-m| ≤ a
  · simpa only [ite_eq_left hx] using held_centre_uniform_path ha hx
  · rw [ite_eq_right hx]
    push Not at hx
    obtain ⟨i,hi⟩ := hx
    exact outside_zero_limit x ha hi

private theorem held_width_centre_outside {n : ℕ} (x : Fin n → ℝ) {a m : ℝ}
    (ha : 0 < a) {i : Fin n} (hi : a < |x i - m|) :
    ∃ s ∈ Ioi 0, IsMaxOn (likelihood x a m) (Ioi 0) s := by
  let L := likelihood x a m
  have hf : L = fun s ↦
      ∏ j, (Φ ((x j - m + a) / s) - Φ ((x j - m - a) / s)) / (2*a) := by
    funext s
    simp only [L, likelihood, ha.ne', ite_false]
  have hcont : ContinuousOn L (Ioi 0) := by
    rw [hf]
    apply continuousOn_finsetProd Finset.univ
    intro j _
    have ht (b : ℝ) : ContinuousOn (fun s : ℝ ↦ Φ (b/s)) (Ioi 0) :=
      cdf_continuous.comp_continuousOn
        (continuousOn_const.div continuous_id.continuousOn (fun _ hs ↦ ne_of_gt hs))
    exact ((ht _).sub (ht _)).div_const _
  have L₁ : 0 < L 1 := by
    rw [hf]
    apply Finset.prod_pos
    intro j _
    exact div_pos (sub_pos.mpr (cdf_strictMono (by linarith))) (by positivity)
  have htop : Tendsto L atTop (𝓝 0) := by
    rw [hf]
    have ht (b : ℝ) : Tendsto (fun s : ℝ ↦ Φ (b/s)) atTop (𝓝 (Φ 0)) :=
      cdf_continuous.continuousAt.tendsto.comp
        (show Tendsto (fun s : ℝ ↦ b/s) atTop (𝓝 0) by
          simpa only [div_eq_mul_inv, mul_zero] using
            (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul b)
    have hterm (j : Fin n) :
        Tendsto (fun s ↦ (Φ ((x j-m+a)/s) - Φ ((x j-m-a)/s))/(2*a)) atTop (𝓝 0) := by
      simpa only [sub_self, zero_div] using
        ((ht (x j-m+a)).sub (ht (x j-m-a))).div_const (2*a)
    simpa [zero_pow i.isLt.pos.ne'] using tendsto_finsetProd Finset.univ (fun j _ ↦ hterm j)
  have hlo := (outside_zero_limit x ha hi).eventually (eventually_lt_nhds L₁)
  obtain ⟨d, hd, hsmall⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp hlo
  obtain ⟨b, hb⟩ := eventually_atTop.mp (htop.eventually (eventually_lt_nhds L₁))
  let low := min 1 d
  let high := max 1 b
  have hs : Icc low high ⊆ Ioi (0 : ℝ) :=
    fun _ h ↦ (lt_min zero_lt_one hd).trans_le h.1
  have h1 : (1 : ℝ) ∈ Icc low high := ⟨min_le_left _ _, le_max_left _ _⟩
  obtain ⟨s, hsc, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1,h1⟩ (hcont.mono hs)
  refine ⟨s, hs hsc, fun t ht ↦ ?_⟩
  by_cases htc : t ∈ Icc low high
  · exact hm htc
  have hh : L t < L 1 := by
    by_cases htd : t < low
    · exact hsmall ⟨ht, htd.trans_le (min_le_right _ _)⟩
    · exact hb t ((le_max_right 1 b).trans (lt_of_not_ge
        (fun h ↦ htc ⟨le_of_not_gt htd,h⟩)).le)
  exact hh.le.trans (hm h1)

/-- At positive held physical width and held centre, an attained scale
exists exactly when some residual magnitude exceeds the width. -/
theorem held_positive_width_centre_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a m : ℝ} (ha : 0 < a) :
    (∃ s ∈ Ioi 0, IsMaxOn (likelihood x a m) (Ioi 0) s) ↔ ∃ j, a < |x j - m| := by
  constructor
  · rintro ⟨s,hs,hm⟩
    by_contra! h
    have hh := held_centre_supremum hn ha h
    exact (hh.2 s hs).not_ge (hh.1.2 (hm.isLUB hs).1)
  · rintro ⟨i,hi⟩
    exact held_width_centre_outside x ha hi

/-- Fixed positive Gaussian scale and nonnegative width yield an attained
centre for every nonempty sample. -/
theorem held_width_scale_attains {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    ∃ m, IsMaxOn (fun t ↦ likelihood x a t s) univ m := by
  rcases ha.eq_or_lt with ha | ha
  · subst a
    refine ⟨Normal.mean x, fun m _ ↦ ?_⟩
    change likelihood x 0 m s ≤ likelihood x 0 (Normal.mean x) s
    have h := normal_member_log x m hs
    have h' := normal_member_log x (Normal.mean x) hs
    apply (log_le_log_iff h.1 h'.1).mp
    rw [h.2,h'.2]
    exact ((Normal.heldSd hn x hs (Normal.mean x)).mpr rfl) (mem_univ m)
  let L (m : ℝ) := likelihood x a m s
  have hf : L = fun m ↦
      ∏ j, (Φ ((x j - m + a) / s) - Φ ((x j - m - a) / s)) / (2*a) := by
    funext m
    simp only [L, likelihood, ha.ne', ite_false]
  have hc : Continuous L := by
    rw [hf]
    apply continuous_finsetProd Finset.univ
    intro j _
    exact ((cdf_continuous.comp (by fun_prop)).sub (cdf_continuous.comp (by fun_prop))).div_const _
  have hL : 0 < L 0 := by
    rw [hf]
    apply Finset.prod_pos
    intro j _
    exact div_pos (sub_pos.mpr (cdf_strictMono ((div_lt_div_iff_of_pos_right hs).mpr
      (by linarith)))) (by positivity)
  have hlarge : Tendsto L atTop (𝓝 0) := by
    rw [hf]
    have harg (b : ℝ) : Tendsto (fun m : ℝ ↦ (b-m)/s) atTop atBot := by
      have h : Tendsto (fun m : ℝ ↦ -m+b) atTop atBot :=
        tendsto_atBot_add_const_right _ b tendsto_neg_atTop_atBot
      simpa only [sub_eq_neg_add] using h.atBot_div_const hs
    have ht (j : Fin n) :
        Tendsto (fun m ↦ (Φ ((x j-m+a)/s) - Φ ((x j-m-a)/s))/(2*a)) atTop (𝓝 0) := by
      have h := ((cdf_atBot.comp (harg (x j+a))).sub
        (cdf_atBot.comp (harg (x j-a)))).div_const (2*a)
      convert h using 1
      · ext m
        simp only [Function.comp_def]
        congr 3 <;> ring
      · simp
    simpa [zero_pow hn.ne'] using tendsto_finsetProd Finset.univ (fun j _ ↦ ht j)
  have hsmall : Tendsto L atBot (𝓝 0) := by
    rw [hf]
    have harg (b : ℝ) : Tendsto (fun m : ℝ ↦ (b-m)/s) atBot atTop := by
      have h : Tendsto (fun m : ℝ ↦ -m+b) atBot atTop :=
        tendsto_atTop_add_const_right _ b tendsto_neg_atBot_atTop
      simpa only [sub_eq_neg_add] using h.atTop_div_const hs
    have ht (j : Fin n) :
        Tendsto (fun m ↦ (Φ ((x j-m+a)/s) - Φ ((x j-m-a)/s))/(2*a)) atBot (𝓝 0) := by
      have h := ((cdf_atTop.comp (harg (x j+a))).sub
        (cdf_atTop.comp (harg (x j-a)))).div_const (2*a)
      convert h using 1
      · ext m
        simp only [Function.comp_def]
        congr 3 <;> ring
      · simp
    simpa [zero_pow hn.ne'] using tendsto_finsetProd Finset.univ (fun j _ ↦ ht j)
  obtain ⟨b,hb⟩ := eventually_atTop.mp (hlarge.eventually (eventually_lt_nhds hL))
  obtain ⟨c,hc'⟩ := eventually_atBot.mp (hsmall.eventually (eventually_lt_nhds hL))
  have h0 : (0 : ℝ) ∈ Icc (min 0 c) (max 0 b) := ⟨min_le_left _ _, le_max_left _ _⟩
  obtain ⟨m,_,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨0,h0⟩ hc.continuousOn
  refine ⟨m,fun q _ ↦ ?_⟩
  change L q ≤ L m
  by_cases hq : q ∈ Icc (min 0 c) (max 0 b)
  · exact hm hq
  have h : L q < L 0 := by
    by_cases hq' : q < min 0 c
    · exact hc' q (hq'.le.trans (min_le_right _ _))
    · exact hb q ((le_max_right 0 b).trans (lt_of_not_ge
        (fun h ↦ hq ⟨le_of_not_gt hq',h⟩)).le)
  exact h.le.trans (hm h0)

private theorem gaussian_interval_positive_side {l u : ℝ} (hl : 0 ≤ l) (hlu : l < u) :
    (Φ u - Φ l) * (-u * φ u + l * φ l) - (φ u - φ l)^2 < 0 := by
  have hd (t : ℝ) : HasDerivAt (fun v ↦ φ v + l * Φ v) ((l-t) * φ t) t := by
    convert (phi_hasDerivAt t).add ((cdf_hasDerivAt t).const_mul l) using 1
    ring
  have hanti : AntitoneOn (fun v ↦ φ v + l * Φ v) (Ici l) := by
    apply antitoneOn_of_deriv_nonpos (convex_Ici l)
      ((continuous_iff_continuousAt.mpr (fun t ↦ (hd t).continuousAt)).continuousOn)
      (fun t _ ↦ (hd t).differentiableAt.differentiableWithinAt)
    intro t ht
    rw [(hd t).deriv]
    have ht' : l ≤ t := interior_subset ht
    exact mul_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr ht') (phi_pos t).le
  have h : l * (Φ u - Φ l) ≤ φ l - φ u := by
    have he := hanti (show l ∈ Ici l from le_rfl) (show u ∈ Ici l from hlu.le) hlu.le
    linarith
  have hmass : 0 < Φ u - Φ l := sub_pos.mpr (cdf_strictMono hlu)
  have hdiff : 0 ≤ φ l - φ u := (mul_nonneg hl hmass.le).trans h
  have hmul := mul_le_mul_of_nonneg_right h hdiff
  have hstrict := mul_pos hmass (mul_pos (sub_pos.mpr hlu) (phi_pos u))
  nlinarith only [hmul,hstrict]

private theorem gaussian_interval_numerator {l u : ℝ} (hlu : l < u) :
    (Φ u - Φ l) * (-u * φ u + l * φ l) - (φ u - φ l)^2 < 0 := by
  by_cases hl : 0 ≤ l
  · exact gaussian_interval_positive_side hl hlu
  by_cases hu : u ≤ 0
  · have hf (z : ℝ) : φ (-z) = φ z := by simp [phi_formula]
    have h := gaussian_interval_positive_side (neg_nonneg.mpr hu) (neg_lt_neg hlu)
    rw [cdf_reflect l, cdf_reflect u, hf l, hf u] at h
    nlinarith only [h]
  · have hmass := sub_pos.mpr (cdf_strictMono hlu)
    have hn := add_neg_of_neg_of_nonpos
      (mul_neg_of_neg_of_pos (neg_neg_of_pos (lt_of_not_ge hu)) (phi_pos u))
      (mul_nonpos_of_nonpos_of_nonneg (le_of_not_ge hl) (phi_pos l).le)
    exact (sub_le_self _ (sq_nonneg _)).trans_lt (mul_neg_of_pos_of_neg hmass hn)

private theorem gaussian_interval_strictLogConcave {k : ℝ} (hk : 0 < k) :
    StrictConcaveOn ℝ univ (fun z ↦ log (Φ (z+k) - Φ (z-k))) := by
  let D (z : ℝ) := Φ (z+k) - Φ (z-k)
  let N (z : ℝ) := φ (z+k) - φ (z-k)
  have hD (z : ℝ) : HasDerivAt D (N z) z := by
    dsimp [D,N]
    have h₁ := (cdf_hasDerivAt (z+k)).comp z ((hasDerivAt_id z).add_const k)
    have h₂ := (cdf_hasDerivAt (z-k)).comp z ((hasDerivAt_id z).sub_const k)
    convert h₁.sub h₂ using 1
    · rfl
    · simp
  have hN (z : ℝ) : HasDerivAt N (-(z+k) * φ (z+k) + (z-k) * φ (z-k)) z := by
    dsimp [N]
    have h₁ := (phi_hasDerivAt (z+k)).comp z ((hasDerivAt_id z).add_const k)
    have h₂ := (phi_hasDerivAt (z-k)).comp z ((hasDerivAt_id z).sub_const k)
    convert h₁.sub h₂ using 1
    · rfl
    · simp only [mul_one]
      ring
  have hp (z : ℝ) : 0 < D z := sub_pos.mpr (cdf_strictMono (by linarith))
  have hd (z : ℝ) : HasDerivAt (fun t ↦ log (D t)) (N z / D z) z :=
    (hD z).log (hp z).ne'
  apply strictConcaveOn_univ_of_deriv2_neg
    (continuous_iff_continuousAt.mpr (fun z ↦ (hd z).continuousAt))
  intro z
  change deriv (deriv (fun t ↦ log (D t))) z < 0
  rw [show deriv (fun t ↦ log (D t)) = (fun t ↦ N t / D t) from funext fun t ↦ (hd t).deriv]
  change deriv (N / D) z < 0
  rw [((hN z).div (hD z) (hp z).ne').deriv]
  apply div_neg_of_neg_of_pos _ (sq_pos_of_pos (hp z))
  have h := gaussian_interval_numerator (l := z-k) (u := z+k) (by linarith)
  dsimp [D,N]
  nlinarith only [h]

/-- Fixed width and positive Gaussian scale give a unique centre.
The positive-width proof uses strict log concavity of Gaussian interval mass. -/
theorem held_width_scale_existsUnique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    ∃! m, IsMaxOn (fun t ↦ likelihood x a t s) univ m := by
  obtain ⟨m,hm⟩ := held_width_scale_attains hn x ha hs
  refine ⟨m,hm,fun q hq ↦ ?_⟩
  rcases ha.eq_or_lt with ha | ha
  · subst a
    have hmax (p : ℝ) (hp : IsMaxOn (fun t ↦ likelihood x 0 t s) univ p) :
        p = Normal.mean x := by
      apply (Normal.heldSd hn x hs p).mp
      intro y _
      change Normal.logLik x y s ≤ Normal.logLik x p s
      rw [← (normal_member_log x y hs).2, ← (normal_member_log x p hs).2]
      exact log_le_log (normal_member_log x y hs).1 (hp (mem_univ y))
    exact (hmax q hq).trans (hmax m hm).symm
  let g (z : ℝ) := log (Φ (z+a/s) - Φ (z-a/s))
  let G (t : ℝ) := ∑ j, g ((x j-t)/s)
  have hpos (t : ℝ) (j : Fin n) :
      0 < (Φ ((x j-t+a)/s) - Φ ((x j-t-a)/s))/(2*a) := by
    exact div_pos (sub_pos.mpr (cdf_strictMono
      ((div_lt_div_iff_of_pos_right hs).mpr (by linarith)))) (by positivity)
  have hLp (t : ℝ) : 0 < likelihood x a t s := by
    simp only [likelihood,ha.ne',ite_false]
    exact Finset.prod_pos (fun j _ ↦ hpos t j)
  have hLog (t : ℝ) : log (likelihood x a t s) = G t - n*log (2*a) := by
    simp only [likelihood,ha.ne',ite_false]
    rw [log_prod (fun j _ ↦ (hpos t j).ne')]
    have he (j : Fin n) : log ((Φ ((x j-t+a)/s) - Φ ((x j-t-a)/s))/(2*a)) =
        g ((x j-t)/s) - log (2*a) := by
      rw [log_div (sub_pos.mpr (cdf_strictMono
        ((div_lt_div_iff_of_pos_right hs).mpr (by linarith)))).ne' (by positivity : (2*a : ℝ) ≠ 0)]
      simp only [g,add_div,sub_div]
    rw [Finset.sum_congr rfl (fun j _ ↦ he j), Finset.sum_sub_distrib]
    simp [G,nsmul_eq_mul]
  have hconc : StrictConcaveOn ℝ univ G := by
    refine ⟨convex_univ, ?_⟩
    intro p _ t _ hpt u v hu hv huv
    simp only [G,smul_eq_mul,Finset.mul_sum,← Finset.sum_add_distrib]
    let : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
    apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
    intro j _
    have hmix : (x j-(u*p+v*t))/s = u*((x j-p)/s)+v*((x j-t)/s) := by
      rw [show v = 1-u by linarith]
      ring
    rw [hmix]
    have hne : (x j-p)/s ≠ (x j-t)/s := by
      intro h
      have he := (div_left_inj₀ hs.ne').mp h
      exact hpt (by linarith)
    exact (gaussian_interval_strictLogConcave (div_pos ha hs)).2
      (mem_univ _) (mem_univ _) hne hu hv huv
  have hmax (p : ℝ) (hp : IsMaxOn (fun t ↦ likelihood x a t s) univ p) : IsMaxOn G univ p := by
    intro t ht
    have he := log_le_log (hLp t) (hp ht)
    change log (likelihood x a t s) ≤ log (likelihood x a p s) at he
    rw [hLog t,hLog p,sub_le_sub_iff_right] at he
    exact he
  exact hconc.eq_of_isMaxOn (hmax q hq) (hmax m hm) (mem_univ _) (mem_univ _)

/-- A positive-radius covering interval can reach the full uniform height
at all observations, including tied endpoints, when the width can move. -/
theorem moving_width_uniform_path {n : ℕ} (x : Fin n → ℝ) {c r : ℝ}
    (hr : 0 < r) (hx : ∀ j, |x j - c| ≤ r) :
    Tendsto (fun t : ℝ ↦ likelihood x (r+t⁻¹) c ((t⁻¹)^2)) atTop
      (𝓝 ((1/(2*r))^n)) := by
  have hshape : Tendsto (fun t : ℝ ↦ 2*(r+t⁻¹)) atTop (𝓝 (2*r)) := by
    simpa only [add_zero] using ((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_add r).const_mul 2
  have hlim (b : ℝ) (hb : 0 ≤ b) :
      Tendsto (fun t : ℝ ↦ (b+t⁻¹)/(t⁻¹)^2) atTop atTop := by
    refine tendsto_atTop_mono' _ ?_ tendsto_id
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    have he : (b+t⁻¹)/(t⁻¹)^2 = b*t^2+t := by
      field_simp [ht.ne']
    change t ≤ _
    rw [he]
    exact le_add_of_nonneg_left (mul_nonneg hb (sq_nonneg t))
  have hterm (j : Fin n) :
      Tendsto (fun t : ℝ ↦
        (Φ ((x j-c+(r+t⁻¹))/(t⁻¹)^2) - Φ ((x j-c-(r+t⁻¹))/(t⁻¹)^2))/(2*(r+t⁻¹)))
        atTop (𝓝 (1/(2*r))) := by
    have h := abs_le.mp (hx j)
    have hp := hlim (x j-c+r) (by linarith)
    have hm := tendsto_neg_atTop_atBot.comp (hlim (r-(x j-c)) (by linarith))
    have H := ((cdf_atTop.comp hp).sub (cdf_atBot.comp hm)).div hshape (by positivity)
    rw [sub_zero] at H
    refine H.congr fun t ↦ ?_
    dsimp only [Function.comp_apply]
    rw [show x j-c+(r+t⁻¹) = x j-c+r+t⁻¹ by ring,
      show x j-c-(r+t⁻¹) = -(r-(x j-c)+t⁻¹) by ring, neg_div]
    rfl
  have h := tendsto_finsetProd Finset.univ (fun j _ ↦ hterm j)
  rw [Finset.prod_const, Finset.card_univ, Fintype.card_fin] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  have ha : 0 < r+t⁻¹ := add_pos hr (inv_pos.mpr ht)
  simp [likelihood, ha.ne']

/-- On the zero-width normal member, a sample constant at the held
centre collapses unboundedly as the normal scale vanishes. -/
theorem held_zero_width_centre_collapse {n : ℕ} (hn : 0 < n) (m : ℝ) :
    Tendsto (likelihood (fun _ : Fin n ↦ m) 0 m) (𝓝[>] 0) atTop := by
  have ht : Tendsto (fun s : ℝ ↦ φ 0/s) (𝓝[>] 0) atTop := by
    simpa only [div_eq_mul_inv] using tendsto_inv_nhdsGT_zero.const_mul_atTop (phi_pos 0)
  change Tendsto (fun s ↦ likelihood (fun _ : Fin n ↦ m) 0 m s) (𝓝[>] 0) atTop
  simpa only [likelihood,ite_true,sub_self,zero_div,Finset.prod_const,
    Finset.card_univ,Fintype.card_fin] using ht.atTop_pow₀ hn.ne'

/-- The full held-width-and-centre failure classification, including the
zero-width normal collapse and the covered uniform with contact penalties. -/
theorem held_width_centre_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a m : ℝ} (ha : 0 ≤ a) :
    (¬ ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (likelihood x a m) (Ioi 0) s) ↔
      (a = 0 ∧ ∀ i, x i = m) ∨ (0 < a ∧ ∀ i, |x i-m| ≤ a) := by
  rcases ha.eq_or_lt with he | hp
  · subst a
    simp only [true_and,lt_irrefl,false_and,or_false]
    constructor
    · intro h i
      by_contra hxi
      exact h ((held_zero_width_centre_criterion hn x m).mpr ⟨i,hxi⟩).exists
    · rintro hx ⟨s,_,hm⟩
      have hE : x = fun _ ↦ m := funext hx
      rw [hE] at hm
      obtain ⟨t,ht,hlt⟩ := ((eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))).and
        ((held_zero_width_centre_collapse hn m).eventually_gt_atTop
          (likelihood (fun _ : Fin n ↦ m) 0 m s))).exists
      exact hlt.not_ge (hm ht)
  · simp only [hp.ne',false_and,false_or,true_and,hp]
    rw [held_positive_width_centre_criterion hn x hp]
    push Not
    rfl

end UnivariateML.LocScaleBhatt

end
