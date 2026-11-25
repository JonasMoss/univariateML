module

public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
public import Mathlib.Topology.Order.IntermediateValue

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Weibull and inverse Weibull likelihoods

The coefficients are positive shape `k` and scale `λ`. The score of the
shape profile is expressed per observation. The inverse transform inverts
both the observations and the scale.

## Deviations

The results concern exact real arithmetic for nonempty, strictly positive,
uncensored samples. The dossier also covers the floating-point Gumbel delegation.
The inverse Weibull dossier states the all-free constant-sample escape in its
existence section and lists only the held-scale escape in its limit metadata.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Weibull

noncomputable section

variable {n : ℕ}

def logDensity (k lam x : ℝ) : ℝ :=
  log k - log lam + (k - 1) * log (x / lam) - (x / lam) ^ k

def logLik (x : Fin n → ℝ) (k lam : ℝ) : ℝ :=
  ∑ i, logDensity k lam (x i)

def avg (z : Fin n → ℝ) : ℝ := (∑ i, z i) / n

def logSample (x : Fin n → ℝ) : Fin n → ℝ := fun i ↦ log (x i)

def weightSum (z : Fin n → ℝ) (k : ℝ) : ℝ := ∑ i, exp (k * z i)

def weightedMean (z : Fin n → ℝ) (k : ℝ) : ℝ :=
  (∑ i, exp (k * z i) * z i) / weightSum z k

def scale (x : Fin n → ℝ) (k : ℝ) : ℝ :=
  (weightSum (logSample x) k / n) ^ k⁻¹

def profile (x : Fin n → ℝ) (k : ℝ) : ℝ := logLik x k (scale x k)

def score (x : Fin n → ℝ) (k : ℝ) : ℝ :=
  k⁻¹ + avg (logSample x) - weightedMean (logSample x) k

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

private theorem weightSum_pos (hn : 0 < n) (z : Fin n → ℝ) (k : ℝ) :
    0 < weightSum z k := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact Finset.sum_pos (fun _ _ ↦ exp_pos _) Finset.univ_nonempty

private theorem scale_pos (hn : 0 < n) (x : Fin n → ℝ) (k : ℝ) : 0 < scale x k :=
  rpow_pos_of_pos (div_pos (weightSum_pos hn _ _) (by exact_mod_cast hn)) _

private theorem logDensity_eq (k : ℝ) {lam x : ℝ} (hl : 0 < lam) (hx : 0 < x) :
    logDensity k lam x = log k - log x + k * (log x - log lam) -
      exp (k * (log x - log lam)) := by
  rw [logDensity, rpow_def_of_pos (div_pos hx hl), log_div hx.ne' hl.ne']
  rw [mul_comm (log x - log lam) k]
  ring

private theorem logLik_eq {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k : ℝ) {lam : ℝ} (hl : 0 < lam) :
    logLik x k lam =
      n * (log k - k * log lam) + (k - 1) * (∑ i, log (x i)) -
      exp (-k * log lam) * weightSum (logSample x) k := by
  unfold logLik weightSum logSample
  have h (i : Fin n) := logDensity_eq k hl (hx i)
  simp_rw [h,
    show ∀ a : ℝ, k * (a - log lam) = -k * log lam + k * a by intro a; ring,
    exp_add]
  simp only [Finset.sum_sub_distrib, Finset.sum_add_distrib, ← Finset.mul_sum,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  ring

private theorem scale_log (hn : 0 < n) (x : Fin n → ℝ) (k : ℝ) :
    log (scale x k) = log (weightSum (logSample x) k / n) * k⁻¹ := by
  rw [scale, rpow_def_of_pos (div_pos (weightSum_pos hn _ _) (by exact_mod_cast hn)),
    log_exp]

/-- For held positive shape, the power mean is the unique maximizing scale. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    0 < scale x k ∧ (scale x k) ^ k = avg (fun i ↦ (x i) ^ k) ∧
      ∀ lam > 0, lam ≠ scale x k → logLik x k lam < profile x k := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let s := weightSum (logSample x) k / n
  have hs : 0 < s := div_pos (weightSum_pos hn _ _) hN
  have hs' : weightSum (logSample x) k = n * s := by
    dsimp [s]
    field_simp
  have hopt : k * log (scale x k) = log s := by
    rw [scale_log hn]
    change k * (log s * k⁻¹) = log s
    field_simp
  refine ⟨scale_pos hn x k, ?_, ?_⟩
  · rw [rpow_def_of_pos (scale_pos hn x k), mul_comm, hopt, exp_log hs]
    unfold s avg weightSum logSample
    congr 1
    apply Finset.sum_congr rfl
    intro i _
    change exp (k * log (x i)) = x i ^ k
    rw [rpow_def_of_pos (hx i), mul_comm]
  · intro lam hl hne
    let t := exp (-k * log lam) * s
    have ht : 0 < t := mul_pos (exp_pos _) hs
    have htlog : log t = -k * log lam + log s := by
      dsimp [t]
      rw [log_mul (exp_pos _).ne' hs.ne', log_exp]
    have htne : t ≠ 1 := by
      intro he
      apply hne
      have hh : log t = 0 := by rw [he, log_one]
      have heq : log lam = log (scale x k) := by
        apply (mul_left_cancel₀ hk.ne')
        linarith only [hh, htlog, hopt]
      exact log_injOn_pos hl (scale_pos hn x k) heq
    have H := mul_lt_mul_of_pos_left (log_lt_sub_one_of_pos ht htne) hN
    have he : exp (-k * log (scale x k)) * s = 1 := by
      rw [neg_mul, hopt, exp_neg, exp_log hs, inv_mul_cancel₀ hs.ne']
    rw [profile, logLik_eq hx k hl, logLik_eq hx k (scale_pos hn x k), hs']
    rw [show exp (-k * log lam) * (n * s) = n * t by dsimp [t]; ring,
      show exp (-k * log (scale x k)) * (n * s) = n by rw [← mul_left_comm, he]; ring]
    rw [htlog] at H
    rw [hopt]
    nlinarith only [H]

private theorem hasDerivAt_weightSum (z : Fin n → ℝ) (k : ℝ) :
    HasDerivAt (weightSum z) (∑ i, exp (k * z i) * z i) k := by
  apply HasDerivAt.fun_sum
  intro i _
  simpa [mul_comm] using ((hasDerivAt_id k).mul_const (z i)).exp

private theorem hasDerivAt_weightedMean (hn : 0 < n) (z : Fin n → ℝ) (k : ℝ) :
    HasDerivAt (weightedMean z)
      ((∑ i, exp (k * z i) * (z i - weightedMean z k) ^ 2) / weightSum z k) k := by
  let s := weightSum z k
  let m := weightedMean z k
  let b := ∑ i, exp (k * z i) * z i
  let c := ∑ i, exp (k * z i) * z i ^ 2
  have hs : s ≠ 0 := (weightSum_pos hn z k).ne'
  have hb : m * s = b := div_mul_cancel₀ b hs
  have hc : HasDerivAt (fun t : ℝ ↦ ∑ i, exp (t * z i) * z i) c k := by
    apply HasDerivAt.fun_sum
    intro i _
    simpa [pow_two, mul_assoc] using (((hasDerivAt_id k).mul_const (z i)).exp).mul_const (z i)
  have H := hc.div (hasDerivAt_weightSum z k) hs
  simp only [Pi.div_def] at H
  have H' : HasDerivAt (weightedMean z) ((c * s - b * b) / s ^ 2) k := H
  convert H' using 1
  change (∑ i, exp (k * z i) * (z i - m) ^ 2) / s = (c * s - b * b) / s ^ 2
  -- Center the quotient-rule numerator to expose the nonnegative variance.
  have E : (∑ i, exp (k * z i) * (z i - m) ^ 2) = c - (2 * m) * b + m ^ 2 * s := by
    simp only [c, b, s, weightSum, Finset.mul_sum, ← Finset.sum_sub_distrib,
      ← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro i _
    ring
  rw [E, ← hb]
  field_simp [hs]
  ring

/-- The per-observation shape profile score strictly decreases on positive shapes. -/
theorem score_strictAnti (hn : 0 < n) (x : Fin n → ℝ) :
    StrictAntiOn (score x) (Ioi 0) := by
  have hm := monotone_of_hasDerivAt_nonneg (hasDerivAt_weightedMean hn (logSample x))
    (fun k ↦ div_nonneg (Finset.sum_nonneg fun i _ ↦
      mul_nonneg (exp_pos _).le (sq_nonneg _)) (weightSum_pos hn _ k).le)
  intro a ha b hb hab
  have hi : b⁻¹ < a⁻¹ := (inv_lt_inv₀ hb ha).2 hab
  unfold score
  linarith only [hi, hm hab.le]

/-- The derivative of the profiled log-likelihood is the sample size times the score. -/
theorem hasDerivAt_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) : HasDerivAt (profile x) (n * score x k) k := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let z := logSample x
  let f := fun t : ℝ ↦ n * (log t - log (weightSum z t / n) - 1) + (t - 1) * ∑ i, z i
  have hEq (t : ℝ) (ht : 0 < t) : profile x t = f t := by
    let s := weightSum z t / n
    have hs : 0 < s := div_pos (weightSum_pos hn _ _) hN
    have hlog : t * log (scale x t) = log s := by
      rw [scale_log hn]
      change t * (log s * t⁻¹) = log s
      field_simp
    rw [profile, logLik_eq hx t (scale_pos hn x t), neg_mul, hlog, exp_neg, exp_log hs]
    have E : s⁻¹ * weightSum z t = n := by
      dsimp [s]
      field_simp [(weightSum_pos hn z t).ne']
    change n * (log t - log s) + (t - 1) * (∑ i, z i) -
      s⁻¹ * weightSum z t = f t
    rw [E]
    dsimp [f, s]
    ring
  have H := (((hasDerivAt_log hk.ne').sub
    (((hasDerivAt_weightSum z k).div_const (n : ℝ)).log
      (div_pos (weightSum_pos hn _ _) hN).ne')).sub_const 1).const_mul (n : ℝ)
  have HF := H.add (((hasDerivAt_id k).sub_const 1).mul_const (∑ i, z i))
  have HF' : HasDerivAt f (n * score x k) k := by
    simp only [Pi.add_def, Pi.sub_def, id_eq, one_mul] at HF
    convert HF using 1
    simp only [score, avg, weightedMean, z]
    field_simp [hN.ne', (weightSum_pos hn (logSample x) k).ne']
    ring
  apply HF'.congr_of_eventuallyEq
  filter_upwards [Ioi_mem_nhds hk] with t ht
  exact hEq t ht

private theorem tendsto_add_finite_atTop {ι : Type*} {l : Filter ι}
    {f g : ι → ℝ} {c : ℝ} (hf : Tendsto f l atTop) (hg : Tendsto g l (𝓝 c)) :
    Tendsto (fun t ↦ f t + g t) l atTop := by
  apply tendsto_atTop.2
  intro b
  filter_upwards [(tendsto_atTop.1 hf) (b - (c - 1)),
    hg.eventually (Ioi_mem_nhds (show c - 1 < c by linarith))] with t ht hg'
  change c - 1 < g t at hg'
  linarith

/-- The shape profile score diverges to positive infinity at zero from the right. -/
theorem score_tendsto_zero (hn : 0 < n) (x : Fin n → ℝ) :
    Tendsto (score x) (𝓝[>] 0) atTop := by
  have hm := (hasDerivAt_weightedMean hn (logSample x) 0).continuousAt
  have H : ContinuousAt (fun k ↦ avg (logSample x) - weightedMean (logSample x) k) 0 :=
    continuousAt_const.sub hm
  refine (tendsto_add_finite_atTop tendsto_inv_nhdsGT_zero
    H.continuousWithinAt.tendsto).congr fun k ↦ ?_
  exact (add_sub_assoc _ _ _).symm

private theorem weightedMean_tendsto {z : Fin n → ℝ} {j : Fin n}
    (hj : ∀ i, z i ≤ z j) :
    Tendsto (weightedMean z) atTop (𝓝 (z j)) := by
  -- After factoring out the largest weight, each tied maximum retains weight one.
  let w := fun (i : Fin n) (k : ℝ) ↦ exp (k * (z i - z j))
  let a := fun i : Fin n ↦ if z i = z j then (1 : ℝ) else 0
  have hw (i : Fin n) : Tendsto (w i) atTop (𝓝 (a i)) := by
    by_cases hi : z i = z j
    · simp [w, a, hi]
    · simp only [a, ite_eq_right hi]
      exact tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg (sub_neg.mpr
        (lt_of_le_of_ne (hj i) hi)))
  have ha : 0 < ∑ i, a i := by
    apply Finset.sum_pos' (fun i _ ↦ by dsimp [a]; split <;> norm_num)
    exact ⟨j, Finset.mem_univ _, by simp [a]⟩
  have hb : (∑ i, a i * z i) = z j * ∑ i, a i := by
    rw [Finset.mul_sum]
    apply Finset.sum_congr rfl
    intro i _
    by_cases hi : z i = z j <;> simp [a, hi]
  have H := (tendsto_finsetSum Finset.univ (fun i _ ↦ (hw i).mul_const (z i))).div
    (tendsto_finsetSum Finset.univ (fun i _ ↦ hw i)) ha.ne'
  rw [hb, mul_div_cancel_right₀ _ ha.ne'] at H
  convert H using 1
  funext k
  have E (i : Fin n) : w i k = exp (-k * z j) * exp (k * z i) := by
    rw [← exp_add]
    dsimp [w]
    ring_nf
  simp only [Pi.div_apply, weightedMean, weightSum, E, mul_assoc, ← Finset.mul_sum,
    mul_div_mul_left _ _ (exp_pos _).ne']

/-- The large-shape score limit is the mean log observation minus the largest log observation. -/
theorem score_tendsto_atTop (x : Fin n → ℝ) {j : Fin n}
    (hj : ∀ i, x i ≤ x j) (hx : ∀ i, 0 < x i) :
    Tendsto (score x) atTop (𝓝 (avg (logSample x) - log (x j))) := by
  have H := (tendsto_inv_atTop_zero.add
    (tendsto_const_nhds (x := avg (logSample x)))).sub
    (weightedMean_tendsto (z := logSample x) (j := j) (fun i ↦ log_le_log (hx i) (hj i)))
  simp only [zero_add] at H
  exact H

/-- The large-shape limit is negative for nonconstant positive observations. -/
theorem score_limit_neg (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {j : Fin n} (hj : ∀ i, x i ≤ x j) (hne : ∃ a b, x a ≠ x b) :
    avg (logSample x) - log (x j) < 0 := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  obtain ⟨a, b, hab⟩ := hne
  have hlt : ∃ i, x i < x j := by
    by_cases ha : x a = x j
    · exact ⟨b, lt_of_le_of_ne (hj b) (fun hb ↦ hab (ha.trans hb.symm))⟩
    · exact ⟨a, lt_of_le_of_ne (hj a) ha⟩
  obtain ⟨i, hi⟩ := hlt
  have H : (∑ i, log (x i)) < ∑ _ : Fin n, log (x j) :=
    Finset.sum_lt_sum (fun i _ ↦ log_le_log (hx i) (hj i))
      ⟨i, Finset.mem_univ _, log_lt_log (hx i) hi⟩
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at H
  exact sub_neg.mpr ((div_lt_iff₀ hN).mpr (by simpa [logSample, mul_comm] using H))

private theorem strictMaximum_of_score {f g : ℝ → ℝ}
    (hd : ∀ k > 0, HasDerivAt f (g k) k) (hg : ContinuousOn g (Ioi 0))
    (ha : StrictAntiOn g (Ioi 0)) (h0 : Tendsto g (𝓝[>] 0) atTop)
    (hneg : ∃ b > 0, g b < 0) :
    ∃ k > 0, g k = 0 ∧ ∀ t > 0, t ≠ k → f t < f k := by
  obtain ⟨b, hb, hgb⟩ := hneg
  have hev : ∀ᶠ t : ℝ in 𝓝[>] 0, t ∈ Ioo 0 b ∧ 0 < g t := by
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
      mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hb), h0.eventually (eventually_gt_atTop 0)]
      with t ht htb hgt
    exact ⟨⟨ht, htb⟩, hgt⟩
  obtain ⟨a, ⟨ha0, hab⟩, hga⟩ := hev.exists
  have hsub : Icc a b ⊆ Ioi (0 : ℝ) := fun _ ht ↦ ha0.trans_le ht.1
  obtain ⟨k, hk, hgk⟩ := intermediate_value_Icc' hab.le (hg.mono hsub)
    (show (0 : ℝ) ∈ Icc (g b) (g a) from ⟨hgb.le, hga.le⟩)
  have hk0 := hsub hk
  have hf : ContinuousOn f (Ioi 0) := fun t ht ↦ (hd t ht).continuousAt.continuousWithinAt
  have hconc : StrictConcaveOn ℝ (Ioi 0) f := by
    apply StrictAntiOn.strictConcaveOn_of_deriv (convex_Ioi _) hf
    rw [interior_Ioi]
    intro s hs t ht hst
    rw [(hd s hs).deriv, (hd t ht).deriv]
    exact ha hs ht hst
  refine ⟨k, hk0, hgk, fun t ht hne ↦ ?_⟩
  have hk' : deriv f k = 0 := (hd k hk0).deriv.trans hgk
  rcases lt_or_gt_of_ne hne with h | h
  · have H := hconc.deriv_lt_slope ht hk0 h (hd k hk0).differentiableAt
    rw [hk', slope_def_field] at H
    exact sub_pos.mp ((div_pos_iff_of_pos_right (sub_pos.mpr h)).mp H)
  · have H := hconc.slope_lt_deriv hk0 ht h (hd k hk0).differentiableAt
    rw [hk', slope_def_field] at H
    exact sub_neg.mp (neg_of_div_neg_left H (sub_nonneg.mpr h.le))

/-- Nonconstant positive observations have a unique joint maximum over positive coefficients. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ a b, x a ≠ x b) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ x Finset.univ_nonempty
  have hj' (i : Fin n) := hj i (Finset.mem_univ i)
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hcont : ContinuousOn (score x) (Ioi 0) := by
    intro t ht
    exact (((continuousAt_id.inv₀ ht.ne').add continuousAt_const).sub
      (hasDerivAt_weightedMean hn (logSample x) t).continuousAt).continuousWithinAt
  have hneg : ∃ b > 0, n * score x b < 0 := by
    obtain ⟨b, hb, hgb⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      ((score_tendsto_atTop x hj' hx).eventually
        (Iio_mem_nhds (score_limit_neg hn hx hj' hne)))).exists
    exact ⟨b, hb, mul_neg_of_pos_of_neg hN hgb⟩
  obtain ⟨k, hk, _, hmax⟩ := strictMaximum_of_score
    (fun _ ht ↦ hasDerivAt_profile hn hx ht) (continuousOn_const.mul hcont)
    (by
      intro a ha b hb hab
      exact mul_lt_mul_of_pos_left (score_strictAnti hn x ha hb hab) hN)
    ((score_tendsto_zero hn x).const_mul_atTop hN) hneg
  have hp : (k, scale x k) ∈ domain := ⟨hk, scale_pos hn x k⟩
  have H (q : ℝ × ℝ) (hq : q ∈ domain) (hqne : q ≠ (k, scale x k)) :
      logLik x q.1 q.2 < logLik x k (scale x k) := by
    have hqk : 0 < q.1 := hq.1
    have hql : 0 < q.2 := hq.2
    by_cases he : q.1 = k
    · have hs : q.2 ≠ scale x k := fun hs ↦ hqne (Prod.ext he hs)
      simpa only [he, profile] using (heldShape hn hx hk).2.2 q.2 hql hs
    · have hle : logLik x q.1 q.2 ≤ profile x q.1 := by
        by_cases hs : q.2 = scale x q.1
        · simp [hs, profile]
        · exact ((heldShape hn hx hqk).2.2 _ hql hs).le
      exact hle.trans_lt (hmax _ hqk he)
  refine ⟨(k, scale x k), ⟨hp, ?_⟩, ?_⟩
  · intro q hq
    by_cases he : q = (k, scale x k)
    · simp [he]
    · exact (H q hq he).le
  · intro q hq
    by_contra hne'
    exact (not_le_of_gt (H q hq.1 hne')) (hq.2 hp)

/-- Constant data give a diverging log-likelihood at their common scale. -/
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k c) atTop atTop := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have H := tendsto_add_finite_atTop (tendsto_log_atTop.const_mul_atTop hN)
    (tendsto_const_nhds (x := (n : ℝ) * (-log c - 1)))
  refine H.congr fun k ↦ ?_
  simp [logLik, logDensity, div_self hc.ne']
  ring

private def heldScore (r : Fin n → ℝ) (k : ℝ) : ℝ :=
  n * k⁻¹ + ∑ i, r i * (1 - exp (k * r i))

private theorem hasDerivAt_held (x : Fin n → ℝ) (hx : ∀ i, 0 < x i)
    {lam k : ℝ} (hl : 0 < lam) (hk : 0 < k) :
    HasDerivAt (fun t ↦ logLik x t lam)
      (heldScore (fun i ↦ log (x i) - log lam) k) k := by
  let r := fun i ↦ log (x i) - log lam
  have hd (i : Fin n) :
      HasDerivAt (fun t ↦ logDensity t lam (x i))
        (k⁻¹ + r i * (1 - exp (k * r i))) k := by
    simp_rw [logDensity_eq _ hl (hx i)]
    change HasDerivAt (fun t ↦ log t - log (x i) + t * r i - exp (t * r i)) _ k
    have H := (((hasDerivAt_log hk.ne').sub_const (log (x i))).add
      ((hasDerivAt_id k).mul_const (r i))).sub (((hasDerivAt_id k).mul_const (r i)).exp)
    simp only [Pi.add_def, Pi.sub_def, id_eq, one_mul] at H
    convert H using 1
    ring
  unfold logLik
  have H := HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ hd i)
  simpa [heldScore, r, Finset.sum_add_distrib] using H

private theorem heldTerm_anti (r : ℝ) : Antitone (fun k : ℝ ↦ r * (1 - exp (k * r))) := by
  have hd (k : ℝ) : HasDerivAt (fun t : ℝ ↦ -(r * (1 - exp (t * r))))
      (r ^ 2 * exp (k * r)) k := by
    convert ((((hasDerivAt_id k).mul_const r).exp.const_sub 1).const_mul r).neg using 1
    · funext t
      rfl
    · simp only [id_eq, one_mul]
      ring
  have hm := monotone_of_hasDerivAt_nonneg hd
    (fun k ↦ mul_nonneg (sq_nonneg r) (exp_pos (k * r)).le)
  intro a b hab
  exact neg_le_neg_iff.mp (hm hab)

private theorem heldScore_strictAnti (hn : 0 < n) (r : Fin n → ℝ) :
    StrictAntiOn (heldScore r) (Ioi 0) := by
  intro a ha b hb hab
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  exact add_lt_add_of_lt_of_le (mul_lt_mul_of_pos_left ((inv_lt_inv₀ hb ha).2 hab) hN)
    (Finset.sum_le_sum fun i _ ↦ heldTerm_anti (r i) hab.le)

private theorem heldScore_continuous (r : Fin n → ℝ) :
    ContinuousOn (heldScore r) (Ioi 0) := by
  unfold heldScore
  have H : Continuous (fun k : ℝ ↦ ∑ i, r i * (1 - exp (k * r i))) := by fun_prop
  have hi : ContinuousOn (fun k : ℝ ↦ k⁻¹) (Ioi 0) := by
    intro k hk
    exact (continuousAt_id.inv₀ (mem_Ioi.mp hk).ne').continuousWithinAt
  exact (continuousOn_const.mul hi).add H.continuousOn

/-- At a held positive scale, the log-likelihood is strictly concave in positive shape. -/
theorem heldScale_strictConcave (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) :
    StrictConcaveOn ℝ (Ioi 0) (fun k ↦ logLik x k lam) := by
  apply StrictAntiOn.strictConcaveOn_of_deriv (convex_Ioi _)
    (fun _ ht ↦ (hasDerivAt_held x hx hl ht).continuousAt.continuousWithinAt)
  rw [interior_Ioi]
  intro a ha b hb hab
  rw [(hasDerivAt_held x hx hl ha).deriv, (hasDerivAt_held x hx hl hb).deriv]
  exact heldScore_strictAnti hn _ ha hb hab

private theorem heldScore_neg (hn : 0 < n) {r : Fin n → ℝ} {j : Fin n} (hj : r j ≠ 0) :
    ∃ b > 0, heldScore r b < 0 := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  -- For all shapes at least one, the j-th summand is bounded above by this
  -- negative constant, and every other summand is nonpositive.
  let c := r j * (1 - exp (1 * r j))
  have hc : c < 0 := by
    dsimp [c]
    rw [one_mul]
    rcases lt_or_gt_of_ne hj with h | h
    · exact mul_neg_of_neg_of_pos h (sub_pos.mpr (exp_lt_one_iff.mpr h))
    · exact mul_neg_of_pos_of_neg h (sub_neg.mpr (one_lt_exp_iff.mpr h))
  have H : Tendsto (fun k : ℝ ↦ n * k⁻¹) atTop (𝓝 0) := by
    simpa using tendsto_inv_atTop_zero.const_mul (n : ℝ)
  obtain ⟨b, hb, hbc⟩ := ((eventually_ge_atTop (1 : ℝ)).and
    (H.eventually (Iio_mem_nhds (neg_pos.mpr hc)))).exists
  have hs : (∑ i, r i * (1 - exp (b * r i))) ≤ c := by
    have H₁ := Finset.single_le_sum (f := fun i : Fin n ↦ -(r i * (1 - exp (b * r i))))
      (s := Finset.univ) (fun i _ ↦ by
        have H₂ := heldTerm_anti (r i) (show 0 ≤ b by linarith)
        simpa using H₂) (Finset.mem_univ j)
    rw [Finset.sum_neg_distrib] at H₁
    exact (neg_le_neg_iff.mp H₁).trans (heldTerm_anti (r j) hb)
  refine ⟨b, by linarith, ?_⟩
  change n * b⁻¹ < -c at hbc
  unfold heldScore
  linarith only [hs, hbc]

/-- A held scale has a unique maximizing shape if at least one observation differs from it. -/
theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) (hne : ∃ i, x i ≠ lam) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t lam) (Ioi 0) k := by
  let r := fun i ↦ log (x i) - log lam
  obtain ⟨j, hj⟩ := hne
  have hr : r j ≠ 0 := fun h ↦ hj (log_injOn_pos (hx j) hl (sub_eq_zero.mp h))
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hc : Continuous (fun k : ℝ ↦ ∑ i, r i * (1 - exp (k * r i))) := by fun_prop
  have h0 : Tendsto (heldScore r) (𝓝[>] 0) atTop :=
    tendsto_add_finite_atTop (tendsto_inv_nhdsGT_zero.const_mul_atTop hN)
      hc.continuousAt.continuousWithinAt.tendsto
  obtain ⟨k, hk, _, hmax⟩ := strictMaximum_of_score
    (fun _ ht ↦ hasDerivAt_held x hx hl ht) (heldScore_continuous r)
    (heldScore_strictAnti hn r) h0 (heldScore_neg hn hr)
  refine ⟨k, ⟨hk, ?_⟩, ?_⟩
  · intro t ht
    by_cases h : t = k
    · simp [h]
    · exact (hmax t ht h).le
  · intro t ht
    by_contra h
    exact (not_le_of_gt (hmax t ht.1 h)) (ht.2 hk)

/-- At a held scale equal to every observation, the log-likelihood diverges with shape. -/
theorem heldScale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {lam : ℝ} (hl : 0 < lam)
    (he : ∀ i, x i = lam) :
    Tendsto (fun k ↦ logLik x k lam) atTop atTop := by
  simpa only [show x = fun _ ↦ lam from funext he] using constant_unbounded hn hl

namespace Inverse

def logDensity (k lam x : ℝ) : ℝ :=
  log k + k * log lam - (k + 1) * log x - (x / lam) ^ (-k)

def logLik (x : Fin n → ℝ) (k lam : ℝ) : ℝ := ∑ i, logDensity k lam (x i)

def reciprocal (x : Fin n → ℝ) : Fin n → ℝ := fun i ↦ (x i)⁻¹

def scale (x : Fin n → ℝ) (k : ℝ) : ℝ := (Weibull.scale (reciprocal x) k)⁻¹

def profile (x : Fin n → ℝ) (k : ℝ) : ℝ := logLik x k (scale x k)

def score (x : Fin n → ℝ) (k : ℝ) : ℝ := Weibull.score (reciprocal x) k

/-- The reciprocal change of variables adds the sample log Jacobian. -/
theorem logLik_reciprocal {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k : ℝ) {lam : ℝ} (hl : 0 < lam) :
    logLik x k lam = Weibull.logLik (reciprocal x) k lam⁻¹ - 2 * ∑ i, log (x i) := by
  unfold logLik Weibull.logLik
  rw [Finset.mul_sum, ← Finset.sum_sub_distrib]
  apply Finset.sum_congr rfl
  intro i _
  rw [reciprocal, logDensity, Weibull.logDensity_eq k (inv_pos.mpr hl) (inv_pos.mpr (hx i)),
    log_inv, log_inv, rpow_def_of_pos (div_pos (hx i) hl), log_div (hx i).ne' hl.ne']
  rw [show (log (x i) - log lam) * -k = k * (-log (x i) - -log lam) by ring]
  ring

private theorem scale_pos (hn : 0 < n) (x : Fin n → ℝ) (k : ℝ) : 0 < scale x k :=
  inv_pos.mpr (Weibull.scale_pos hn _ _)

private theorem profile_eq (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (k : ℝ) :
    profile x k = Weibull.profile (reciprocal x) k - 2 * ∑ i, log (x i) := by
  rw [profile, logLik_reciprocal hx k (scale_pos hn x k), scale, inv_inv, Weibull.profile]

/-- With shape held, the inverse Weibull scale is the reciprocal power mean. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    0 < scale x k ∧ (scale x k) ^ (-k) = avg (fun i ↦ (x i) ^ (-k)) ∧
      ∀ lam > 0, lam ≠ scale x k → logLik x k lam < profile x k := by
  obtain ⟨hs, hp, hmax⟩ :=
    Weibull.heldShape hn (x := reciprocal x) (fun i ↦ inv_pos.mpr (hx i)) hk
  refine ⟨scale_pos hn x k, ?_, ?_⟩
  · rw [scale, rpow_neg (inv_nonneg.mpr hs.le), inv_rpow hs.le, inv_inv, hp]
    unfold avg
    congr 1
    apply Finset.sum_congr rfl
    intro i _
    change (x i)⁻¹ ^ k = x i ^ (-k)
    rw [inv_rpow (hx i).le, rpow_neg (hx i).le]
  · intro lam hl hne
    rw [logLik_reciprocal hx k hl, profile_eq hn hx]
    exact sub_lt_sub_right (hmax lam⁻¹ (inv_pos.mpr hl)
      (fun he ↦ hne (by rw [← inv_inv lam, he, scale]))) _

/-- The inverse Weibull profile has the same derivative as the reciprocal-data Weibull profile. -/
theorem hasDerivAt_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    HasDerivAt (profile x) (n * score x k) k := by
  have H := (Weibull.hasDerivAt_profile hn (x := reciprocal x)
    (fun i ↦ inv_pos.mpr (hx i)) hk).sub_const (2 * ∑ i, log (x i))
  exact H.congr_of_eventuallyEq (Eventually.of_forall fun t ↦ profile_eq hn hx t)

/-- The inverse Weibull profile score strictly decreases on positive shapes. -/
theorem score_strictAnti (hn : 0 < n) (x : Fin n → ℝ) :
    StrictAntiOn (score x) (Ioi 0) :=
  Weibull.score_strictAnti hn (reciprocal x)

/-- At zero from the right, the inverse Weibull profile score diverges to positive infinity. -/
theorem score_tendsto_zero (hn : 0 < n) (x : Fin n → ℝ) :
    Tendsto (score x) (𝓝[>] 0) atTop :=
  Weibull.score_tendsto_zero hn (reciprocal x)

/-- The large-shape limit is the smallest log observation minus the mean log observation. -/
theorem score_tendsto_atTop (x : Fin n → ℝ) {j : Fin n}
    (hj : ∀ i, x j ≤ x i) (hx : ∀ i, 0 < x i) :
    Tendsto (score x) atTop (𝓝 (log (x j) - avg (logSample x))) := by
  have H := Weibull.score_tendsto_atTop (reciprocal x) (j := j)
    (fun i ↦ inv_anti₀ (hx j) (hj i)) (fun i ↦ inv_pos.mpr (hx i))
  have E : avg (logSample (reciprocal x)) - log ((reciprocal x) j) =
      log (x j) - avg (logSample x) := by
    simp only [avg, logSample, reciprocal, log_inv, Finset.sum_neg_distrib, neg_div]
    ring
  rwa [E] at H

/-- The inverse Weibull large-shape limit is negative for nonconstant positive data. -/
theorem score_limit_neg (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {j : Fin n} (hj : ∀ i, x j ≤ x i) (hne : ∃ a b, x a ≠ x b) :
    log (x j) - avg (logSample x) < 0 := by
  obtain ⟨a, b, hab⟩ := hne
  have H := Weibull.score_limit_neg hn (x := reciprocal x) (j := j)
    (fun i ↦ inv_pos.mpr (hx i)) (fun i ↦ inv_anti₀ (hx j) (hj i))
    ⟨a, b, fun he ↦ hab (by simpa [reciprocal] using congrArg (fun t : ℝ ↦ t⁻¹) he)⟩
  simp only [avg, logSample, reciprocal, log_inv, Finset.sum_neg_distrib, neg_div] at H ⊢
  linarith only [H]

private def invertScale (p : ℝ × ℝ) : ℝ × ℝ := (p.1, p.2⁻¹)

private theorem invertScale_involutive (p : ℝ × ℝ) : invertScale (invertScale p) = p := by
  simp [invertScale]

private theorem invertScale_mem {p : ℝ × ℝ} (hp : p ∈ domain) : invertScale p ∈ domain :=
  ⟨hp.1, inv_pos.mpr hp.2⟩

private theorem max_iff {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ × ℝ} (hp : p ∈ domain) :
    IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p ↔
    IsMaxOn (fun q ↦ Weibull.logLik (reciprocal x) q.1 q.2) domain (invertScale p) := by
  constructor
  · intro h q hq
    have H := h (invertScale_mem hq)
    change logLik x q.1 q.2⁻¹ ≤ logLik x p.1 p.2 at H
    rw [logLik_reciprocal hx q.1 (inv_pos.mpr hq.2), inv_inv,
      logLik_reciprocal hx p.1 hp.2] at H
    exact (sub_le_sub_iff_right _).mp H
  · intro h q hq
    change logLik x q.1 q.2 ≤ logLik x p.1 p.2
    rw [logLik_reciprocal hx q.1 hq.2, logLik_reciprocal hx p.1 hp.2]
    exact sub_le_sub_right (h (invertScale_mem hq)) _

/-- Reciprocal scale transfers the unique joint Weibull MLE to inverse Weibull. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ a b, x a ≠ x b) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p := by
  obtain ⟨a, b, hab⟩ := hne
  obtain ⟨p, hp, hu⟩ := Weibull.jointMLE hn (x := reciprocal x)
    (fun i ↦ inv_pos.mpr (hx i)) ⟨a, b, fun he ↦ hab (by simpa [reciprocal] using congrArg (fun t : ℝ ↦ t⁻¹) he)⟩
  refine ⟨invertScale p, ⟨invertScale_mem hp.1, ?_⟩, ?_⟩
  · apply (max_iff hx (invertScale_mem hp.1)).mpr
    simpa only [invertScale_involutive] using hp.2
  · intro q hq
    have H := hu (invertScale q) ⟨invertScale_mem hq.1, (max_iff hx hq.1).mp hq.2⟩
    simpa only [invertScale_involutive] using congrArg invertScale H

/-- Constant inverse Weibull observations have diverging log-likelihood at their common scale. -/
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k c) atTop atTop := by
  have H := tendsto_add_finite_atTop (Weibull.constant_unbounded hn (inv_pos.mpr hc))
    (tendsto_const_nhds (x := -(2 * ∑ _ : Fin n, log c)))
  exact H.congr' (Eventually.of_forall fun k ↦ by
    change _ = logLik (fun _ : Fin n ↦ c) k c
    rw [logLik_reciprocal (fun _ ↦ hc) k hc, sub_eq_add_neg]
    rfl)

/-- At held scale, the inverse Weibull log-likelihood is strictly concave in positive shape. -/
theorem heldScale_strictConcave (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) :
    StrictConcaveOn ℝ (Ioi 0) (fun k ↦ logLik x k lam) := by
  have H := Weibull.heldScale_strictConcave hn
    (x := reciprocal x) (fun i ↦ inv_pos.mpr (hx i)) (inv_pos.mpr hl)
  refine ⟨H.1, ?_⟩
  intro a ha b hb hab s t hs ht hst
  have h := H.2 ha hb hab hs ht hst
  simp only [smul_eq_mul, logLik_reciprocal hx _ hl] at h ⊢
  nlinarith only [h, show (s + t) * (2 * ∑ i, log (x i)) = 2 * ∑ i, log (x i) by rw [hst, one_mul]]

/-- A held inverse Weibull scale yields a unique shape if some observation differs from it. -/
theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) (hne : ∃ i, x i ≠ lam) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t lam) (Ioi 0) k := by
  obtain ⟨j, hj⟩ := hne
  have H := Weibull.heldScale hn (x := reciprocal x)
    (fun i ↦ inv_pos.mpr (hx i)) (inv_pos.mpr hl) ⟨j, fun he ↦ hj (by simpa [reciprocal] using congrArg (fun t : ℝ ↦ t⁻¹) he)⟩
  have E (k : ℝ) :
      IsMaxOn (fun t ↦ logLik x t lam) (Ioi 0) k ↔
      IsMaxOn (fun t ↦ Weibull.logLik (reciprocal x) t lam⁻¹) (Ioi 0) k := by
    change (∀ t ∈ Ioi (0 : ℝ), logLik x t lam ≤ logLik x k lam) ↔ _
    simp_rw [logLik_reciprocal hx _ hl, sub_le_sub_iff_right]
    rfl
  simpa only [E] using H

/-- A held scale equal to all observations permits unbounded inverse Weibull likelihood. -/
theorem heldScale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {lam : ℝ} (hl : 0 < lam)
    (he : ∀ i, x i = lam) :
    Tendsto (fun k ↦ logLik x k lam) atTop atTop := by
  simpa only [show x = fun _ ↦ lam from funext he] using constant_unbounded hn hl

end Inverse

end

end UnivariateML.Weibull
