import UnivariateML.Family.Transfers3
import UnivariateML.Family.Transfers
import UnivariateML.Family.Gamma

/-! Feller–Pareto with held threshold and power.

## Deviations

The powered-excess transfer requires observations strictly above the threshold.
The narrow-span moment inequality excludes finite stationary points and
hence finite MLEs. The comparison and attainment of the boundary
supremum require a separate face compactness argument.
-/

open Real Set
namespace UnivariateML.ShapeFacesFellerPareto
noncomputable section

variable {n : ℕ}

def poweredExcess (x : Fin n → ℝ) (μ a : ℝ) : Fin n → ℝ :=
  fun i ↦ (x i - μ) ^ a

def logLikelihood (x : Fin n → ℝ) (μ a b : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, (log a - log (x i - μ) - Beta.logBeta p +
    p.1 * a * log ((x i - μ) / b) -
      (p.1 + p.2) * log (1 + ((x i - μ) / b) ^ a))

def powerJacobian (x : Fin n → ℝ) (μ a : ℝ) : ℝ :=
  ∑ i, (log a + (a - 1) * log (x i - μ))

theorem logLik_powered_excess {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    (a : ℝ) {b : ℝ} (hb : 0 < b) (p : ℝ × ℝ) :
    logLikelihood x μ a b p =
      BetaPrime.logLik (poweredExcess x μ a) (b ^ a) p + powerJacobian x μ a := by
  unfold logLikelihood BetaPrime.logLik powerJacobian
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  have hi := sub_pos.mpr (hx i)
  have hr := div_pos hi hb
  dsimp [poweredExcess]
  rw [← div_rpow hi.le hb.le, log_rpow hr, log_rpow hb, log_div hi.ne' hb.ne']
  ring

theorem held_power_scale_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (∃! p, p ∈ Beta.domain ∧ IsMaxOn (logLikelihood x μ a b) Beta.domain p) ↔
      ∃ i j, x i ≠ x j := by
  have hpos (i : Fin n) : 0 < poweredExcess x μ a i :=
    rpow_pos_of_pos (sub_pos.mpr (hx i)) _
  rw [Transfer.existsUnique_max_iff (bijOn_id Beta.domain)
    (fun p _ ↦ logLik_powered_excess hx a hb p)]
  rw [BetaPrime.jointMLE_iff hn (rpow_pos_of_pos hb a) hpos]
  constructor
  · rintro ⟨i, j, h⟩
    exact ⟨i, j, fun he ↦ h (by simp [poweredExcess, he])⟩
  · rintro ⟨i, j, h⟩
    refine ⟨i, j, fun he ↦ h ?_⟩
    have hinj := (strictMonoOn_rpow_Ici_of_exponent_pos ha).injOn
    have hxij := hinj (show x i - μ ∈ Ici 0 from sub_nonneg.mpr (hx i).le)
      (show x j - μ ∈ Ici 0 from sub_nonneg.mpr (hx j).le) he
    linarith

theorem narrow_span_moment_bound {D h μ₃ Δ T₃ : ℝ}
    (hD : 0 < D) (hh : 0 < h) (hhD : 1 / (8 * D ^ 2) ≤ h)
    (hcubic : |T₃ - μ₃| ≤ 21 * D ^ 4)
    (hsample : |Δ + T₃ / 3| ≤ 16 * D ^ 4 / 3)
    (hshape : |Δ| ≤ 1 / (6 * h ^ 2)) :
    |μ₃| ≤ 69 * D ^ 4 := by
  have hp : 0 < 8 * D ^ 2 := by positivity
  have hmul : 1 ≤ h * (8 * D ^ 2) := (div_le_iff₀ hp).mp hhD
  have hsq : 1 ≤ (h * (8 * D ^ 2)) ^ 2 := by nlinarith
  have hs : 1 / (6 * h ^ 2) ≤ 64 * D ^ 4 / 6 := by
    apply (div_le_iff₀ (show 0 < 6 * h ^ 2 by positivity)).2
    nlinarith only [hsq]
  have hd : |Δ| ≤ 64 * D ^ 4 / 6 := hshape.trans hs
  rw [abs_le] at hcubic hsample hd ⊢
  constructor <;> linarith


open Filter
open scoped Topology

private theorem logGamma_derivative {z : ℝ} (hz : 0 < z) :
    HasDerivAt (log ∘ Real.Gamma) (Gamma.digamma z) z :=
  ((hasDerivAt_Gamma_of_pos hz).log (Gamma_pos_of_pos hz).ne').differentiableAt.hasDerivAt

private theorem digamma_step {z : ℝ} (hz : 0 < z) :
    Gamma.digamma (z + 1) = z⁻¹ + Gamma.digamma z := by
  have h := (logGamma_derivative (show 0 < z + 1 by positivity)).comp z
    ((hasDerivAt_id z).add_const 1)
  have h' := (hasDerivAt_log hz.ne').add (logGamma_derivative hz)
  have he : (fun t ↦ log t + (log ∘ Real.Gamma) t) =ᶠ[𝓝 z]
      (log ∘ Real.Gamma) ∘ (fun t ↦ t + 1) := by
    filter_upwards [Ioi_mem_nhds hz] with t ht
    change log t + log (Real.Gamma t) = log (Real.Gamma (t + 1))
    rw [Real.Gamma_add_one ht.ne', log_mul ht.ne' (Gamma_pos_of_pos ht).ne']
  simpa only [mul_one] using h.unique (h'.congr_of_eventuallyEq he.symm)

private def halfTerm (z : ℝ) : ℝ := z⁻¹ / 2

private def correction (z : ℝ) : ℝ := z⁻¹ ^ 2 / 12

private def remainder (z : ℝ) : ℝ := Gamma.scoreGap z - halfTerm z

private def remainderStep (z : ℝ) : ℝ :=
  (z⁻¹ + (z + 1)⁻¹) / 2 - (log (z + 1) - log z)

private def correctionStep (z : ℝ) : ℝ := correction z - correction (z + 1)

private theorem remainder_step {z : ℝ} (hz : 0 < z) :
    remainder z = remainder (z + 1) + remainderStep z := by
  unfold remainder Gamma.scoreGap halfTerm remainderStep
  rw [digamma_step hz]
  ring

private theorem remainderStep_deriv {z : ℝ} (hz : 0 < z) :
    HasDerivAt remainderStep (-(1 / (2 * z ^ 2 * (z + 1) ^ 2))) z := by
  have hz' : 0 < z + 1 := by positivity
  have hi := ((hasDerivAt_id (z + 1)).inv hz'.ne').comp z ((hasDerivAt_id z).add_const 1)
  have hv := ((hasDerivAt_id z).inv hz.ne').add hi
  have hl := ((hasDerivAt_log hz'.ne').comp z ((hasDerivAt_id z).add_const 1)).sub
    (hasDerivAt_log hz.ne')
  convert (hv.div_const 2).sub hl using 1
  · rfl
  · simp only [id_eq, mul_one, one_div]
    field_simp
    ring

private theorem correction_deriv {z : ℝ} (hz : 0 < z) :
    HasDerivAt correction (-(1 / (6 * z ^ 3))) z := by
  have h := (((hasDerivAt_id z).inv hz.ne').pow 2).div_const 12
  convert h using 1
  · rfl
  · simp only [Pi.inv_apply, id_eq, one_div]
    field_simp [hz.ne']
    ring_nf
    rw [mul_inv_cancel₀ hz.ne']
    norm_num

private theorem correctionStep_sub_deriv {z : ℝ} (hz : 0 < z) :
    HasDerivAt (fun t ↦ correctionStep t - remainderStep t)
      (-(1 / (6 * z ^ 3 * (z + 1) ^ 3))) z := by
  have dnext := (correction_deriv (show 0 < z + 1 by positivity)).comp z
    ((hasDerivAt_id z).add_const 1)
  convert ((correction_deriv hz).sub dnext).sub (remainderStep_deriv hz) using 1
  · rfl
  · have hz' : z + 1 ≠ 0 := by positivity
    simp only [mul_one]
    field_simp
    ring

private theorem remainderStep_anti : AntitoneOn remainderStep (Ioi 0) := by
  apply StrictAntiOn.antitoneOn
  apply strictAntiOn_of_deriv_neg (convex_Ioi _)
    (fun z hz ↦ (remainderStep_deriv hz).continuousAt.continuousWithinAt)
  rw [interior_Ioi]
  intro z hz
  rw [(remainderStep_deriv hz).deriv]
  have hz' : 0 < z := hz
  exact neg_lt_zero.mpr (by positivity)

private theorem correctionStep_sub_anti :
    AntitoneOn (fun z ↦ correctionStep z - remainderStep z) (Ioi 0) := by
  apply StrictAntiOn.antitoneOn
  apply strictAntiOn_of_deriv_neg (convex_Ioi _)
    (fun z hz ↦ (correctionStep_sub_deriv hz).continuousAt.continuousWithinAt)
  rw [interior_Ioi]
  intro z hz
  rw [(correctionStep_sub_deriv hz).deriv]
  have hz' : 0 < z := hz
  exact neg_lt_zero.mpr (by positivity)

private theorem antitone_of_step_and_limit {f δ : ℝ → ℝ}
    (hδ : AntitoneOn δ (Ioi 0)) (hstep : ∀ z > 0, f z = f (z + 1) + δ z)
    (hlim : Tendsto f atTop (𝓝 0)) : AntitoneOn f (Ioi 0) := by
  intro a ha b hb hab
  let d : ℕ → ℝ := fun j ↦ f (a + j) - f (b + j)
  have hd : Antitone d := by
    apply antitone_nat_of_succ_le
    intro j
    have hA : 0 < a + (j : ℝ) := add_pos_of_pos_of_nonneg ha (Nat.cast_nonneg _)
    have hB : 0 < b + (j : ℝ) := add_pos_of_pos_of_nonneg hb (Nat.cast_nonneg _)
    have hδ' := hδ (show a + (j : ℝ) ∈ Ioi 0 from hA)
      (show b + (j : ℝ) ∈ Ioi 0 from hB) (add_le_add_left hab _)
    have hA' := hstep _ hA
    have hB' := hstep _ hB
    dsimp [d]
    push_cast
    rw [← add_assoc a, ← add_assoc b]
    linarith
  have ht (z : ℝ) : Tendsto (fun j : ℕ ↦ f (z + j)) atTop (𝓝 0) :=
    hlim.comp (tendsto_atTop_add_const_left atTop z tendsto_natCast_atTop_atTop)
  have ht' : Tendsto d atTop (𝓝 0) := by simpa [d] using (ht a).sub (ht b)
  have h0 : 0 ≤ d 0 := le_of_tendsto ht' (Eventually.of_forall fun j ↦ hd (Nat.zero_le j))
  simpa [d, sub_nonneg] using h0

private theorem remainder_tendsto : Tendsto remainder atTop (𝓝 0) := by
  have hhalf : Tendsto halfTerm atTop (𝓝 0) := by
    unfold halfTerm
    simpa only [zero_div] using
      (tendsto_inv_atTop_zero : Tendsto (fun z : ℝ ↦ z⁻¹) atTop (𝓝 0)).div_const 2
  unfold remainder
  simpa only [sub_zero] using Gamma.scoreGap_tendsto_atTop.sub hhalf

private theorem correction_tendsto : Tendsto correction atTop (𝓝 0) := by
  unfold correction
  simpa only [zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div] using
    ((tendsto_inv_atTop_zero : Tendsto (fun z : ℝ ↦ z⁻¹) atTop (𝓝 0)).pow 2).div_const 12

private theorem correction_remainder_step (z : ℝ) (hz : 0 < z) :
    correction z - remainder z =
      (correction (z + 1) - remainder (z + 1)) + (correctionStep z - remainderStep z) := by
  unfold correctionStep
  linarith only [remainder_step hz]

private theorem remainder_anti : AntitoneOn remainder (Ioi 0) :=
  antitone_of_step_and_limit remainderStep_anti (fun _ hz ↦ remainder_step hz) remainder_tendsto

private theorem correction_remainder_anti :
    AntitoneOn (fun z ↦ correction z - remainder z) (Ioi 0) :=
  antitone_of_step_and_limit correctionStep_sub_anti correction_remainder_step
    (by simpa only [sub_zero] using correction_tendsto.sub remainder_tendsto)

/-- The two-shape difference bound for the Gamma score gap, obtained from
the recurrence and the limit at infinity. -/
theorem digamma_difference_bounds {z N : ℝ} (hz : 0 < z) (hN : z ≤ N) :
    0 ≤ Gamma.scoreGap z - Gamma.scoreGap N - (1 / (2 * z) - 1 / (2 * N)) ∧
    Gamma.scoreGap z - Gamma.scoreGap N - (1 / (2 * z) - 1 / (2 * N)) ≤
      1 / (12 * z ^ 2) - 1 / (12 * N ^ 2) := by
  have hzI : z ∈ Ioi (0 : ℝ) := hz
  have hNI : N ∈ Ioi (0 : ℝ) := hz.trans_le hN
  have hlo : remainder N ≤ remainder z := remainder_anti hzI hNI hN
  have hhi : correction N - remainder N ≤ correction z - remainder z :=
    correction_remainder_anti hzI hNI hN
  unfold remainder at hlo hhi
  have he (t : ℝ) (ht : 0 < t) : halfTerm t = 1 / (2 * t) := by
    unfold halfTerm
    field_simp
  have hc (t : ℝ) (ht : 0 < t) : correction t = 1 / (12 * t ^ 2) := by
    unfold correction
    field_simp
  rw [he z hz, he N (hz.trans_le hN)] at hlo hhi
  rw [hc z hz, hc N (hz.trans_le hN)] at hhi
  constructor <;> linarith


private theorem log_cubic_error {u : ℝ} (hu : |u| ≤ 1 / 50) :
    |-log (1 + u) + u - u ^ 2 / 2 + u ^ 3 / 3| ≤ |u| ^ 4 / 3 := by
  have h : |-u| < 1 := by rw [abs_neg]; linarith
  have hseries := abs_log_sub_add_sum_range_le h 4
  norm_num [Finset.sum_range_succ, abs_neg] at hseries
  let E := -log (1 + u) + u - u ^ 2 / 2 + u ^ 3 / 3
  have hs : |u ^ 4 / 4 - E| ≤ |u| ^ 5 / (1 - |u|) := by
    convert hseries using 2; ring
  have htail : |u| ^ 5 / (1 - |u|) ≤ |u| ^ 4 / 12 := by
    apply (div_le_iff₀ (show 0 < 1 - |u| by linarith)).2
    have h' := mul_le_mul_of_nonneg_right (show 13 * |u| ≤ 1 by linarith) (pow_nonneg (abs_nonneg u) 4)
    nlinarith only [h']
  have habs : |u ^ 4 / 4| = |u| ^ 4 / 4 := by
    rw [abs_div, abs_pow, abs_of_nonneg (by norm_num : (0 : ℝ) ≤ 4)]
  have htri := abs_sub (u ^ 4 / 4) (u ^ 4 / 4 - E)
  rw [sub_sub_cancel, habs] at htri
  change |E| ≤ _
  linarith only [htri, hs.trans htail]

private theorem log_linear_error {u : ℝ} (hu : |u| ≤ 1 / 50) :
    |-log (1 + u) + u| ≤ |u| ^ 2 := by
  have h : |-u| < 1 := by rw [abs_neg]; linarith
  have hseries := abs_log_sub_add_sum_range_le h 2
  norm_num [Finset.sum_range_succ, abs_neg] at hseries
  let E := -log (1 + u) + u
  have hs : |u ^ 2 / 2 - E| ≤ |u| ^ 3 / (1 - |u|) := by
    convert hseries using 2; ring
  have htail : |u| ^ 3 / (1 - |u|) ≤ |u| ^ 2 / 2 := by
    apply (div_le_iff₀ (show 0 < 1 - |u| by linarith)).2
    have h' := mul_le_mul_of_nonneg_right (show 3 * |u| ≤ 1 by linarith) (sq_nonneg |u|)
    nlinarith only [h']
  have habs : |u ^ 2 / 2| = |u| ^ 2 / 2 := by
    rw [abs_div, abs_pow, abs_of_nonneg (by norm_num : (0 : ℝ) ≤ 2)]
  have htri := abs_sub (u ^ 2 / 2) (u ^ 2 / 2 - E)
  rw [sub_sub_cancel, habs] at htri
  change |E| ≤ _
  linarith only [htri, hs.trans htail]

private def avg (x : Fin n → ℝ) : ℝ := (∑ i, x i) / n

private theorem avg_abs_le (hn : 0 < n) {f : Fin n → ℝ} {M : ℝ}
    (h : ∀ i, |f i| ≤ M) : |avg f| ≤ M := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  unfold avg
  rw [abs_div, abs_of_pos hN]
  apply (div_le_iff₀ hN).2
  calc
    _ ≤ ∑ i, |f i| := Finset.abs_sum_le_sum_abs _ _
    _ ≤ ∑ _ : Fin n, M := Finset.sum_le_sum (fun i _ ↦ h i)
    _ = _ := by simp [nsmul_eq_mul, mul_comm]

def leftGap (T : Fin n → ℝ) (m : ℝ) : ℝ := avg (fun i ↦ -log (1 + (1 - m) * T i))

def rightGap (T : Fin n → ℝ) (m : ℝ) : ℝ := avg (fun i ↦ -log (1 - m * T i))

private theorem sample_small {D u w : ℝ} (hD : 0 < D) (hD' : D ≤ 1 / 100)
    (hw₀ : 0 ≤ w) (hw₁ : w ≤ 1) (hu : |u| ≤ 2 * D) : |w * u| ≤ 1 / 50 := by
  rw [abs_mul, abs_of_nonneg hw₀]
  linarith [mul_le_mul_of_nonneg_left hu hw₀, mul_le_mul_of_nonneg_right hw₁ (show 0 ≤ 2 * D by linarith)]

private theorem left_empirical_bound (hn : 0 < n) {D m : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hm₀ : 0 < m) (hm₁ : m < 1)
    (T : Fin n → ℝ) (hT : ∀ i, |T i| ≤ 2 * D) (hcenter : ∑ i, T i = 0) :
    leftGap T m / (1 - m) ^ 2 ≤ 4 * D ^ 2 := by
  let w := 1 - m
  have hw : 0 < w := sub_pos.mpr hm₁
  have hw₁ : w ≤ 1 := by dsimp [w]; linarith
  let f := fun i ↦ -log (1 + w * T i) + w * T i
  have hf (i : Fin n) : |f i| ≤ w ^ 2 * (4 * D ^ 2) := by
    have h := log_linear_error (sample_small hD hD' hw.le hw₁ (hT i))
    change |f i| ≤ _ at h
    rw [abs_mul, abs_of_pos hw, mul_pow] at h
    have hs : |T i| ^ 2 ≤ 4 * D ^ 2 := by
      nlinarith [mul_nonneg (sub_nonneg.mpr (hT i)) (show 0 ≤ 2 * D + |T i| by positivity)]
    exact h.trans (mul_le_mul_of_nonneg_left hs (sq_nonneg w))
  have hF := (le_abs_self (avg f)).trans (avg_abs_le hn hf)
  have he : avg f = leftGap T m := by
    simp only [avg, f, leftGap, Finset.sum_add_distrib, ← Finset.mul_sum, hcenter, mul_zero, add_zero]
    rfl
  rw [he] at hF
  change leftGap T m / w ^ 2 ≤ _
  exact (div_le_iff₀ (sq_pos_of_pos hw)).2 (by linarith only [hF, mul_comm (w ^ 2) (4 * D ^ 2)])

theorem normalized_sample_cubic_bound (hn : 0 < n) {D m : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hm₀ : 0 < m) (hm₁ : m < 1)
    (T : Fin n → ℝ) (hT : ∀ i, |T i| ≤ 2 * D) (hcenter : ∑ i, T i = 0) :
    |leftGap T m / (1 - m) ^ 2 - rightGap T m / m ^ 2 +
      (∑ i, T i ^ 3) / n / 3| ≤ 16 * D ^ 4 / 3 := by
  let w := 1 - m
  have hw : 0 < w := sub_pos.mpr hm₁
  have hw₁ : w ≤ 1 := by dsimp [w]; linarith
  let E := fun u : ℝ ↦ -log (1 + u) + u - u ^ 2 / 2 + u ^ 3 / 3
  let f := fun i ↦ E (w * T i) / w ^ 2 - E (-m * T i) / m ^ 2
  have hdiv {u a : ℝ} (ha : 0 < a) (ha' : a ≤ 1) (hu : |u| ≤ 2 * D) :
      |E (a * u) / a ^ 2| ≤ a ^ 2 * (16 * D ^ 4 / 3) := by
    have h := log_cubic_error (sample_small hD hD' ha.le ha' hu)
    change |E (a * u)| ≤ _ at h
    rw [abs_mul, abs_of_pos ha, mul_pow] at h
    rw [abs_div, abs_of_pos (sq_pos_of_pos ha)]
    apply (div_le_iff₀ (sq_pos_of_pos ha)).2
    have ht : |u| ^ 4 ≤ (2 * D) ^ 4 := pow_le_pow_left₀ (abs_nonneg _) hu 4
    have hh := mul_le_mul_of_nonneg_left ht (pow_nonneg ha.le 4)
    nlinarith only [h, hh]
  have hf (i : Fin n) : |f i| ≤ 16 * D ^ 4 / 3 := by
    have h₁ := hdiv hw hw₁ (hT i)
    have h₂ := hdiv hm₀ hm₁.le (by simpa only [abs_neg] using hT i : |-(T i)| ≤ 2 * D)
    rw [mul_neg, ← neg_mul] at h₂
    have ht := (abs_sub (E (w * T i) / w ^ 2) (E (-m * T i) / m ^ 2)).trans (add_le_add h₁ h₂)
    have hwm : w ^ 2 + m ^ 2 ≤ 1 := by
      dsimp [w]
      nlinarith [mul_nonneg hm₀.le (sub_nonneg.mpr hm₁.le)]
    have hpow : 0 ≤ 16 * D ^ 4 / 3 := by positivity
    change |f i| ≤ _
    exact ht.trans (by nlinarith only [mul_le_mul_of_nonneg_right hwm hpow])
  have hpoint (u : ℝ) :
      E (w * u) / w ^ 2 - E (-m * u) / m ^ 2 =
        -log (1 + w * u) / w ^ 2 - (-log (1 - m * u) / m ^ 2) +
          (1 / w + 1 / m) * u + u ^ 3 / 3 := by
    dsimp [E]
    rw [neg_mul, ← sub_eq_add_neg]
    field_simp [hw.ne', hm₀.ne']
    have hwm : w + m = 1 := by dsimp [w]; ring
    linear_combination (w ^ 2 * m ^ 2 * u ^ 3) * hwm
  have he : avg f = leftGap T m / w ^ 2 - rightGap T m / m ^ 2 + avg (fun i ↦ T i ^ 3) / 3 := by
    unfold avg leftGap rightGap f
    simp only [hpoint, Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_div,
      ← Finset.mul_sum, hcenter, mul_zero, sub_div, add_div, div_right_comm, avg, w, add_zero]
  have h := avg_abs_le hn hf
  rw [he] at h
  exact h


private theorem normalized_shape_bounds {m N : ℝ} (hm₀ : 0 < m) (hm₁ : m < 1) (hN : 0 < N) :
    let h := m * (1 - m) * N
    let A := (Gamma.scoreGap (m * N) - Gamma.scoreGap N) / (1 - m) ^ 2
    let B := (Gamma.scoreGap ((1 - m) * N) - Gamma.scoreGap N) / m ^ 2
    1 / (2 * h) ≤ A ∧ |A - B| ≤ 1 / (6 * h ^ 2) := by
  dsimp only
  let w := 1 - m
  let h := m * w * N
  have hw : 0 < w := sub_pos.mpr hm₁
  have hh : 0 < h := mul_pos (mul_pos hm₀ hw) hN
  have hp := digamma_difference_bounds (mul_pos hm₀ hN)
    (mul_le_of_le_one_left hN.le hm₁.le)
  have hq := digamma_difference_bounds (mul_pos hw hN)
    (mul_le_of_le_one_left hN.le (by dsimp [w]; linarith))
  have hp₀ : (1 / (2 * (m * N)) - 1 / (2 * N)) / w ^ 2 = 1 / (2 * h) := by
    dsimp [h, w]
    field_simp
  have hq₀ : (1 / (2 * (w * N)) - 1 / (2 * N)) / m ^ 2 = 1 / (2 * h) := by
    dsimp [h, w]
    field_simp
    ring
  have hp₁ : (1 / (12 * (m * N) ^ 2) - 1 / (12 * N ^ 2)) / w ^ 2 =
      (1 - m ^ 2) / (12 * h ^ 2) := by
    dsimp [h]
    field_simp
  have hq₁ : (1 / (12 * (w * N) ^ 2) - 1 / (12 * N ^ 2)) / m ^ 2 =
      (1 - w ^ 2) / (12 * h ^ 2) := by
    dsimp [h]
    field_simp
  have hP := div_le_div_of_nonneg_right hp.2 (sq_nonneg w)
  have hQ := div_le_div_of_nonneg_right hq.2 (sq_nonneg m)
  rw [sub_div, hp₀, hp₁] at hP
  rw [sub_div, hq₀, hq₁] at hQ
  have hPl := div_le_div_of_nonneg_right hp.1 (sq_nonneg w)
  have hQl := div_le_div_of_nonneg_right hq.1 (sq_nonneg m)
  rw [zero_div, sub_div, hp₀] at hPl
  rw [zero_div, sub_div, hq₀] at hQl
  have hPA : (1 - m ^ 2) / (12 * h ^ 2) ≤ 1 / (6 * h ^ 2) := by
    rw [show 1 / (6 * h ^ 2) = 2 / (12 * h ^ 2) by field_simp; ring]
    exact div_le_div_of_nonneg_right (by linarith [sq_nonneg m]) (by positivity)
  have hQB : (1 - w ^ 2) / (12 * h ^ 2) ≤ 1 / (6 * h ^ 2) := by
    rw [show 1 / (6 * h ^ 2) = 2 / (12 * h ^ 2) by field_simp; ring]
    exact div_le_div_of_nonneg_right (by linarith [sq_nonneg w]) (by positivity)
  change 1 / (2 * h) ≤ (Gamma.scoreGap (m * N) - Gamma.scoreGap N) / w ^ 2 ∧
    |(Gamma.scoreGap (m * N) - Gamma.scoreGap N) / w ^ 2 -
      (Gamma.scoreGap (w * N) - Gamma.scoreGap N) / m ^ 2| ≤ 1 / (6 * h ^ 2)
  refine ⟨by linarith only [hPl], ?_⟩
  rw [abs_le]
  constructor <;> linarith only [hP, hQ, hPl, hQl, hPA, hQB]

private theorem cubic_distortion (hn : 0 < n) {D : ℝ} (hD : 0 < D)
    (T v : Fin n → ℝ) (hT : ∀ i, |T i| ≤ 2 * D) (hv : ∀ i, |v i| ≤ D)
    (hdiff : ∀ i, |T i - v i| ≤ 3 * D ^ 2) :
    |avg (fun i ↦ T i ^ 3) - avg (fun i ↦ v i ^ 3)| ≤ 21 * D ^ 4 := by
  have h (i : Fin n) : |T i ^ 3 - v i ^ 3| ≤ 21 * D ^ 4 := by
    have h₁ : |T i| ^ 2 ≤ (2 * D) ^ 2 := pow_le_pow_left₀ (abs_nonneg _) (hT i) 2
    have h₂ : |v i| ^ 2 ≤ D ^ 2 := pow_le_pow_left₀ (abs_nonneg _) (hv i) 2
    have h₃ : |T i| * |v i| ≤ (2 * D) * D :=
      mul_le_mul (hT i) (hv i) (abs_nonneg _) (by positivity)
    have hA : |T i ^ 2 + T i * v i + v i ^ 2| ≤ 7 * D ^ 2 := by
      have hb := (abs_add_le (T i ^ 2 + T i * v i) (v i ^ 2)).trans
        (add_le_add_left (abs_add_le (T i ^ 2) (T i * v i)) _)
      rw [abs_pow, abs_mul, abs_pow] at hb
      linarith only [hb, h₁, h₂, h₃]
    rw [show T i ^ 3 - v i ^ 3 = (T i - v i) * (T i ^ 2 + T i * v i + v i ^ 2) by ring, abs_mul]
    have hh := mul_le_mul (hdiff i) hA (abs_nonneg _) (by positivity : 0 ≤ 3 * D ^ 2)
    nlinarith only [hh]
  have he : avg (fun i ↦ T i ^ 3) - avg (fun i ↦ v i ^ 3) =
      avg (fun i ↦ T i ^ 3 - v i ^ 3) := by
    simp only [avg, Finset.sum_sub_distrib, sub_div]
  rw [he]
  exact avg_abs_le hn h

/-- Once the logistic standardization bounds have been checked, the two
normalized shape equations force the third central log moment bound. -/
theorem normalized_score_moment_bound (hn : 0 < n) {D m N : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hm₀ : 0 < m) (hm₁ : m < 1) (hN : 0 < N)
    (T v : Fin n → ℝ) (hT : ∀ i, |T i| ≤ 2 * D) (hcenter : ∑ i, T i = 0)
    (hv : ∀ i, |v i| ≤ D) (hdiff : ∀ i, |T i - v i| ≤ 3 * D ^ 2)
    (hleft : leftGap T m = Gamma.scoreGap (m * N) - Gamma.scoreGap N)
    (hright : rightGap T m = Gamma.scoreGap ((1 - m) * N) - Gamma.scoreGap N) :
    |(∑ i, v i ^ 3) / n| ≤ 69 * D ^ 4 := by
  let h := m * (1 - m) * N
  let Δ := leftGap T m / (1 - m) ^ 2 - rightGap T m / m ^ 2
  have hh : 0 < h := mul_pos (mul_pos hm₀ (sub_pos.mpr hm₁)) hN
  have hs := normalized_shape_bounds hm₀ hm₁ hN
  dsimp only at hs
  rw [← hleft, ← hright] at hs
  have hsmall := left_empirical_bound hn hD hD' hm₀ hm₁ T hT hcenter
  have hl : 1 / (2 * h) ≤ 4 * D ^ 2 := hs.1.trans hsmall
  have hlow : 1 / (8 * D ^ 2) ≤ h := by
    apply (div_le_iff₀ (show 0 < 8 * D ^ 2 by positivity)).2
    have hh' := (div_le_iff₀ (show 0 < 2 * h by positivity)).1 hl
    nlinarith only [hh']
  exact narrow_span_moment_bound hD hh hlow (cubic_distortion hn hD T v hT hv hdiff)
    (normalized_sample_cubic_bound hn hD hD' hm₀ hm₁ T hT hcenter) hs.2


def logistic (s y : ℝ) : ℝ := (1 + exp (s - y))⁻¹

private theorem logistic_mem (s y : ℝ) : logistic s y ∈ Ioo (0 : ℝ) 1 := by
  unfold logistic
  have h : 1 < 1 + exp (s - y) := by linarith [exp_pos (s - y)]
  exact ⟨inv_pos.mpr (zero_lt_one.trans h), inv_lt_one_of_one_lt₀ h⟩

private theorem logistic_deriv (s y : ℝ) :
    HasDerivAt (logistic s) (logistic s y * (1 - logistic s y)) y := by
  have hd := (((hasDerivAt_id y).const_sub s).exp.const_add 1)
  have hden : 0 < 1 + exp (s - y) := by positivity
  convert hd.inv hden.ne' using 1
  · rfl
  · unfold logistic
    simp only [id_eq]
    field_simp
    ring

private def logWeight (s y : ℝ) : ℝ := log (logistic s y * (1 - logistic s y))

private theorem logWeight_deriv (s y : ℝ) :
    HasDerivAt (logWeight s) (1 - 2 * logistic s y) y := by
  have hp := logistic_mem s y
  have h := mul_pos hp.1 (sub_pos.mpr hp.2)
  have hd := logistic_deriv s y
  convert (hd.mul (hd.const_sub 1)).log h.ne' using 1
  · rfl
  · apply (eq_div_iff h.ne').2
    ring

private theorem logWeight_lipschitz (s y z : ℝ) :
    |logWeight s y - logWeight s z| ≤ |y - z| := by
  have h := (convex_univ (𝕜 := ℝ) (E := ℝ)).norm_image_sub_le_of_norm_deriv_le
    (f := logWeight s) (C := 1) (fun y _ ↦ (logWeight_deriv s y).differentiableAt)
    (fun t _ ↦ ?_) (mem_univ z) (mem_univ y)
  · simpa only [Real.norm_eq_abs, one_mul] using h
  · rw [(logWeight_deriv s t).deriv, Real.norm_eq_abs, abs_le]
    have h := logistic_mem s t
    exact ⟨by linarith [h.2], by linarith [h.1]⟩

private theorem derivative_ratio_close {D : ℝ} (hD : 0 < D) (hD' : D ≤ 1 / 100)
    (s y z : ℝ) (hyz : |y - z| ≤ D) :
    |(logistic s y * (1 - logistic s y)) / (logistic s z * (1 - logistic s z)) - 1| ≤ 3 * D := by
  let u := logistic s y * (1 - logistic s y)
  let v := logistic s z * (1 - logistic s z)
  have hu : 0 < u := mul_pos (logistic_mem s y).1 (sub_pos.mpr (logistic_mem s y).2)
  have hv : 0 < v := mul_pos (logistic_mem s z).1 (sub_pos.mpr (logistic_mem s z).2)
  have h := abs_le.mp ((logWeight_lipschitz s y z).trans hyz)
  change -D ≤ log u - log v ∧ log u - log v ≤ D at h
  rw [← log_div hu.ne' hv.ne'] at h
  have hlo := exp_le_exp.mpr h.1
  have hhi := exp_le_exp.mpr h.2
  rw [exp_log (div_pos hu hv)] at hlo hhi
  have hExp := abs_le.mp (abs_exp_sub_one_sub_id_le
    (show |D| ≤ 1 by rw [abs_of_pos hD]; linarith))
  change |u / v - 1| ≤ 3 * D
  rw [abs_le]
  have H : D ^ 2 ≤ D := by nlinarith [mul_nonneg hD.le (show 0 ≤ 1 - D by linarith)]
  exact ⟨by linarith [one_sub_le_exp_neg D], by linarith [hExp.2]⟩

private theorem avg_const (hn : 0 < n) (v : ℝ) : avg (fun _ : Fin n ↦ v) = v := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp [avg, nsmul_eq_mul, mul_comm, hN]

private theorem avg_mono {x y : Fin n → ℝ} (h : ∀ i, x i ≤ y i) : avg x ≤ avg y :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ h i) (Nat.cast_nonneg _)

private theorem avg_sub (x y : Fin n → ℝ) :
    avg (fun i ↦ x i - y i) = avg x - avg y := by simp [avg, Finset.sum_sub_distrib, sub_div]

private theorem avg_div (x : Fin n → ℝ) (w : ℝ) :
    avg (fun i ↦ x i / w) = avg x / w := by
  simp only [avg, Finset.sum_div, div_right_comm]

private theorem avg_logistic_mem (hn : 0 < n) (s : ℝ) (y : Fin n → ℝ) :
    avg (fun i ↦ logistic s (y i)) ∈ Ioo (0 : ℝ) 1 := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let i : Fin n := ⟨0, hn⟩
  constructor
  · exact div_pos (Finset.sum_pos (fun j _ ↦ (logistic_mem s (y j)).1)
      ⟨i, Finset.mem_univ _⟩) hN
  · apply (div_lt_one hN).2
    have h := Finset.sum_lt_sum (g := fun _ : Fin n ↦ (1 : ℝ))
      (fun j _ ↦ (logistic_mem s (y j)).2.le) ⟨i, Finset.mem_univ _, (logistic_mem s (y i)).2⟩
    simpa [nsmul_eq_mul] using h

private theorem logistic_mono (s : ℝ) : Monotone (logistic s) := by
  intro y z hyz
  unfold logistic
  apply inv_anti₀ (show 0 < 1 + exp (s - z) by positivity)
  exact add_le_add_right (exp_le_exp.mpr (sub_le_sub_left hyz s)) _

/-- Uniform log-interval control of the standardized logistic observations.
The bound is independent of the log scale. -/
theorem logistic_standardization (hn : 0 < n) {D L U : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hspan : U - L ≤ D)
    (s : ℝ) (y : Fin n → ℝ) (hy : ∀ i, y i ∈ Icc L U) :
    let m := avg (fun i ↦ logistic s (y i))
    let v := fun i ↦ y i - avg y
    let T := fun i ↦ (logistic s (y i) - m) / (m * (1 - m))
    0 < m ∧ m < 1 ∧ (∑ i, T i = 0) ∧
      (∀ i, |v i| ≤ D) ∧ (∀ i, |T i - v i| ≤ 3 * D ^ 2) ∧ ∀ i, |T i| ≤ 2 * D := by
  dsimp only
  let u := fun i ↦ logistic s (y i)
  let m := avg u
  let v := fun i ↦ y i - avg y
  let w := m * (1 - m)
  let T := fun i ↦ (u i - m) / w
  have hm₀ : 0 < m := (avg_logistic_mem hn s y).1
  have hm₁ : m < 1 := (avg_logistic_mem hn s y).2
  have hw : 0 < w := mul_pos hm₀ (sub_pos.mpr hm₁)
  have hmL : logistic s L ≤ m := by
    rw [← avg_const (n := n) hn (logistic s L)]
    exact avg_mono (fun i ↦ logistic_mono s (hy i).1)
  have hmU : m ≤ logistic s U := by
    rw [← avg_const (n := n) hn (logistic s U)]
    exact avg_mono (fun i ↦ logistic_mono s (hy i).2)
  have hLU : L ≤ U := ((hy ⟨0, hn⟩).1).trans (hy ⟨0, hn⟩).2
  have hcont : Continuous (logistic s) := continuous_iff_continuousAt.mpr fun t ↦ (logistic_deriv s t).continuousAt
  obtain ⟨z, hz, hzm⟩ := intermediate_value_Icc hLU hcont.continuousOn ⟨hmL, hmU⟩
  have hmz : logistic s z = m := hzm
  have hdist {a b : ℝ} (ha : a ∈ Icc L U) (hb : b ∈ Icc L U) : |a - b| ≤ D := by
    rw [abs_le]
    constructor <;> linarith [ha.1, ha.2, hb.1, hb.2]
  let f := fun t ↦ logistic s t / w - t
  have hd (t : ℝ) : HasDerivAt f (logistic s t * (1 - logistic s t) / w - 1) t :=
    ((logistic_deriv s t).div_const w).sub (hasDerivAt_id t)
  have hder (t : ℝ) (ht : t ∈ Icc L U) : ‖deriv f t‖ ≤ 3 * D := by
    rw [(hd t).deriv, Real.norm_eq_abs]
    have h := derivative_ratio_close hD hD' s t z (hdist ht hz)
    rw [hmz] at h
    exact h
  have hpair (i j : Fin n) :
      |(u i - u j) / w - (y i - y j)| ≤ 3 * D ^ 2 := by
    have h := (convex_Icc L U).norm_image_sub_le_of_norm_deriv_le
      (f := f) (fun t _ ↦ (hd t).differentiableAt) hder (hy j) (hy i)
    rw [Real.norm_eq_abs, Real.norm_eq_abs] at h
    have hh : f (y i) - f (y j) = (u i - u j) / w - (y i - y j) := by
      dsimp [f, u]
      rw [sub_div]
      ring
    rw [hh] at h
    have hb := mul_le_mul_of_nonneg_left (hdist (hy i) (hy j)) (show 0 ≤ 3 * D by positivity)
    nlinarith only [h, hb]
  have havg_id (i : Fin n) :
      avg (fun j ↦ (u i - u j) / w - (y i - y j)) = T i - v i := by
    rw [avg_sub, avg_div, avg_sub, avg_sub, avg_const hn, avg_const hn]
  have hdiff (i : Fin n) : |T i - v i| ≤ 3 * D ^ 2 := by
    rw [← havg_id i]
    exact avg_abs_le hn (hpair i)
  have hv (i : Fin n) : |v i| ≤ D := by
    have h := avg_abs_le hn (fun j ↦ hdist (hy i) (hy j))
    rw [avg_sub, avg_const hn] at h
    exact h
  have hcenter : ∑ i, T i = 0 := by
    dsimp [T]
    rw [← Finset.sum_div, Finset.sum_sub_distrib]
    have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    rw [show (n : ℝ) * m = ∑ i, u i by dsimp [m, avg]; field_simp, sub_self, zero_div]
  refine ⟨hm₀, hm₁, hcenter, hv, hdiff, fun i ↦ ?_⟩
  have h := abs_sub (T i - v i) (-v i)
  rw [sub_neg_eq_add, sub_add_cancel, abs_neg] at h
  have hD2 : 3 * D ^ 2 ≤ D := by nlinarith [mul_nonneg hD.le (show 0 ≤ 1 - 3 * D by linarith)]
  linarith only [h, hv i, hdiff i, hD2]


def chartLik (y : Fin n → ℝ) (s p q : ℝ) : ℝ :=
  (∑ i, (p * log (logistic s (y i)) + q * log (1 - logistic s (y i)) - y i)) -
    (n : ℝ) * Beta.logBeta (p, q)

private theorem scale_logistic_deriv (s y : ℝ) :
    HasDerivAt (fun t ↦ logistic t y) (-(logistic s y * (1 - logistic s y))) s := by
  have hd := (logistic_deriv 0 (y - s)).comp s ((hasDerivAt_id s).const_sub y)
  have he (v : ℝ) : logistic 0 (y - v) = logistic v y := by
    unfold logistic
    rw [show 0 - (y - v) = v - y by ring]
  convert hd using 1
  · funext t
    exact (he t).symm
  · simp only [he, mul_neg, mul_one]

private theorem chart_scale_deriv (y : Fin n → ℝ) (s p q : ℝ) :
    HasDerivAt (fun t ↦ chartLik y t p q)
      ((p + q) * (∑ i, logistic s (y i)) - (n : ℝ) * p) s := by
  have h (i : Fin n) :
      HasDerivAt (fun t ↦ p * log (logistic t (y i)) + q * log (1 - logistic t (y i)) - y i)
        ((p + q) * logistic s (y i) - p) s := by
    let u := logistic s (y i)
    have hpos : 0 < u := (logistic_mem _ _).1
    have hcomp : 0 < 1 - u := sub_pos.mpr (logistic_mem _ _).2
    have hd := scale_logistic_deriv s (y i)
    convert (((hd.log hpos.ne').const_mul p).add
      (((hd.const_sub 1).log hcomp.ne').const_mul q)).sub_const (y i) using 1
    change (p + q) * u - p = p * (-(u * (1 - u)) / u) + q * (-(-(u * (1 - u))) / (1 - u))
    rw [show -(u * (1 - u)) / u = u - 1 by field_simp; ring,
      show -(-(u * (1 - u))) / (1 - u) = u by rw [neg_neg]; exact mul_div_cancel_right₀ _ hcomp.ne']
    ring
  unfold chartLik
  convert (HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ h i).sub_const
    ((n : ℝ) * Beta.logBeta (p, q)) using 1
  simp only [Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]

private theorem logBeta_eq {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    Beta.logBeta (p, q) = log (Real.Gamma p) + log (Real.Gamma q) - log (Real.Gamma (p + q)) := by
  simp only [Beta.logBeta, Beta.normalizer]
  rw [log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hq)).ne'
    (Gamma_pos_of_pos (add_pos hp hq)).ne',
    log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne']

private theorem chart_first_deriv (y : Fin n → ℝ) (s : ℝ) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    HasDerivAt (fun t ↦ chartLik y s t q)
      ((∑ i, log (logistic s (y i))) - (n : ℝ) * (Gamma.digamma p - Gamma.digamma (p + q))) p := by
  have hβ : HasDerivAt (fun t ↦ Beta.logBeta (t, q))
      (Gamma.digamma p - Gamma.digamma (p + q)) p := by
    have h := ((logGamma_derivative hp).add_const (log (Real.Gamma q))).sub
      ((logGamma_derivative (add_pos hp hq)).comp p ((hasDerivAt_id p).add_const q))
    have hh : HasDerivAt (fun t ↦ log (Real.Gamma t) + log (Real.Gamma q) -
        log (Real.Gamma (t + q))) (Gamma.digamma p - Gamma.digamma (p + q)) p := by
      convert h using 1
      · rfl
      · simp only [mul_one]
    apply hh.congr_of_eventuallyEq
    filter_upwards [Ioi_mem_nhds hp] with t ht
    exact logBeta_eq ht hq
  have h (i : Fin n) : HasDerivAt (fun t ↦
      t * log (logistic s (y i)) + q * log (1 - logistic s (y i)) - y i)
      (log (logistic s (y i))) p := by
    simpa only [one_mul, id_eq] using (((hasDerivAt_id p).mul_const
      (log (logistic s (y i)))).add_const (q * log (1 - logistic s (y i)))).sub_const (y i)
  exact (HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ h i).sub (hβ.const_mul (n : ℝ))

private theorem chart_second_deriv (y : Fin n → ℝ) (s : ℝ) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    HasDerivAt (fun t ↦ chartLik y s p t)
      ((∑ i, log (1 - logistic s (y i))) - (n : ℝ) * (Gamma.digamma q - Gamma.digamma (p + q))) q := by
  have hβ : HasDerivAt (fun t ↦ Beta.logBeta (p, t))
      (Gamma.digamma q - Gamma.digamma (p + q)) q := by
    have h := ((logGamma_derivative hq).const_add (log (Real.Gamma p))).sub
      ((logGamma_derivative (add_pos hp hq)).comp q ((hasDerivAt_id q).const_add p))
    have hh : HasDerivAt (fun t ↦ log (Real.Gamma p) + log (Real.Gamma t) -
        log (Real.Gamma (p + t))) (Gamma.digamma q - Gamma.digamma (p + q)) q := by
      convert h using 1
      · rfl
      · simp only [mul_one]
    apply hh.congr_of_eventuallyEq
    filter_upwards [Ioi_mem_nhds hq] with t ht
    exact logBeta_eq hp ht
  have h (i : Fin n) : HasDerivAt (fun t ↦
      p * log (logistic s (y i)) + t * log (1 - logistic s (y i)) - y i)
      (log (1 - logistic s (y i))) q := by
    simpa only [one_mul, id_eq] using (((hasDerivAt_id q).mul_const
      (log (1 - logistic s (y i)))).const_add (p * log (logistic s (y i)))).sub_const (y i)
  exact (HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ h i).sub (hβ.const_mul (n : ℝ))


private theorem gap_eq_log_means {s : ℝ} (y : Fin n → ℝ) {m : ℝ}
    (hm₀ : 0 < m) (hm₁ : m < 1) (hn : 0 < n) :
    let u := fun i ↦ logistic s (y i)
    let T := fun i ↦ (u i - m) / (m * (1 - m))
    leftGap T m = log m - avg (fun i ↦ log (u i)) ∧
      rightGap T m = log (1 - m) - avg (fun i ↦ log (1 - u i)) := by
  dsimp only
  let u := fun i ↦ logistic s (y i)
  let T := fun i ↦ (u i - m) / (m * (1 - m))
  have hw : 0 < 1 - m := sub_pos.mpr hm₁
  have h₁ (i : Fin n) : 1 + (1 - m) * T i = u i / m := by
    dsimp [T]
    field_simp
    ring
  have h₂ (i : Fin n) : 1 - m * T i = (1 - u i) / (1 - m) := by
    dsimp [T]
    field_simp
    ring
  have hL (i : Fin n) : -log (1 + (1 - m) * T i) = log m - log (u i) := by
    rw [h₁, log_div (logistic_mem s (y i)).1.ne' hm₀.ne']
    ring
  have hR (i : Fin n) : -log (1 - m * T i) = log (1 - m) - log (1 - u i) := by
    rw [h₂, log_div (sub_pos.mpr (logistic_mem s (y i)).2).ne' hw.ne']
    ring
  change leftGap T m = _ ∧ rightGap T m = _
  constructor
  · unfold leftGap
    simp_rw [hL, avg_sub, avg_const hn]
    rfl
  · unfold rightGap
    simp_rw [hR, avg_sub, avg_const hn]
    rfl

/-- Every finite stationary point in the held-power beta-prime chart
satisfies the narrow-span third moment inequality. -/
theorem narrow_span_stationary_bound (hn : 0 < n) {D L U : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hspan : U - L ≤ D)
    (y : Fin n → ℝ) (hy : ∀ i, y i ∈ Icc L U)
    (s : ℝ) {p q : ℝ} (hp : 0 < p) (hq : 0 < q)
    (hs : deriv (fun t ↦ chartLik y t p q) s = 0)
    (hp' : deriv (fun t ↦ chartLik y s t q) p = 0)
    (hq' : deriv (fun t ↦ chartLik y s p t) q = 0) :
    |(∑ i, (y i - avg y) ^ 3) / n| ≤ 69 * D ^ 4 := by
  let N := p + q
  have hN : 0 < N := add_pos hp hq
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  let u := fun i ↦ logistic s (y i)
  let m := avg u
  let T := fun i ↦ (u i - m) / (m * (1 - m))
  have hm : m * N = p := by
    rw [(chart_scale_deriv y s p q).deriv] at hs
    change N * (∑ i, u i) - (n : ℝ) * p = 0 at hs
    dsimp [m, avg]
    rw [div_mul_eq_mul_div]
    apply (div_eq_iff hn').2
    linarith only [hs, mul_comm N (∑ i, u i), mul_comm (n : ℝ) p]
  have hqN : (1 - m) * N = q := by dsimp [N] at hm ⊢; nlinarith only [hm]
  have hlogs₁ : avg (fun i ↦ log (u i)) = Gamma.digamma p - Gamma.digamma N := by
    rw [(chart_first_deriv y s hp hq).deriv] at hp'
    apply (div_eq_iff hn').2
    change _ - (n : ℝ) * (Gamma.digamma p - Gamma.digamma N) = 0 at hp'
    linarith only [hp']
  have hlogs₂ : avg (fun i ↦ log (1 - u i)) = Gamma.digamma q - Gamma.digamma N := by
    rw [(chart_second_deriv y s hp hq).deriv] at hq'
    apply (div_eq_iff hn').2
    change _ - (n : ℝ) * (Gamma.digamma q - Gamma.digamma N) = 0 at hq'
    linarith only [hq']
  obtain ⟨hm₀, hm₁, hcenter, hv, hdiff, hT⟩ :=
    logistic_standardization hn hD hD' hspan s y hy
  change 0 < m at hm₀
  change m < 1 at hm₁
  have hmean := gap_eq_log_means y hm₀ hm₁ hn (s := s)
  dsimp only at hmean
  have hleft : leftGap T m = Gamma.scoreGap (m * N) - Gamma.scoreGap N := by
    have he : leftGap T m = log m - avg (fun i ↦ log (u i)) := hmean.1
    rw [he, hlogs₁, hm, Gamma.scoreGap, Gamma.scoreGap, ← hm, log_mul hm₀.ne' hN.ne', hm]
    ring
  have hright : rightGap T m = Gamma.scoreGap ((1 - m) * N) - Gamma.scoreGap N := by
    have he : rightGap T m = log (1 - m) - avg (fun i ↦ log (1 - u i)) := hmean.2
    rw [he, hlogs₂, hqN, Gamma.scoreGap, Gamma.scoreGap, ← hqN,
      log_mul (sub_pos.mpr hm₁).ne' hN.ne', hqN]
    ring
  exact normalized_score_moment_bound hn hD hD' hm₀ hm₁ hN T
    (fun i ↦ y i - avg y) hT hcenter hv hdiff hleft hright


private theorem chart_transfer {X : Fin n → ℝ} (hx : ∀ i, 0 < X i)
    {a : ℝ} (ha : 0 < a) (p q : ℝ) :
    BetaPrime.logLik X a (p, q) = chartLik (fun i ↦ log (X i)) (log a) p q := by
  have he (i : Fin n) :
      logistic (log a) (log (X i)) = (X i / a) / (1 + X i / a) := by
    unfold logistic
    rw [exp_sub, exp_log ha, exp_log (hx i)]
    field_simp [(add_pos ha (hx i)).ne', (add_pos (hx i) ha).ne', (hx i).ne', ha.ne']
    exact add_comm _ _
  have hcomp (i : Fin n) :
      1 - logistic (log a) (log (X i)) = (1 + X i / a)⁻¹ := by
    rw [he]
    have hd : 0 < 1 + X i / a := add_pos zero_lt_one (div_pos (hx i) ha)
    field_simp [(add_pos (hx i) ha).ne', (add_pos ha (hx i)).ne', (hx i).ne', ha.ne']; ring
  have h (i : Fin n) :
      (p - 1) * log (X i / a) - log a -
          (p + q) * log (1 + X i / a) - Beta.logBeta (p, q) =
        p * log (logistic (log a) (log (X i))) +
          q * log (1 - logistic (log a) (log (X i))) - log (X i) - Beta.logBeta (p, q) := by
    have hd : 0 < 1 + X i / a := add_pos zero_lt_one (div_pos (hx i) ha)
    rw [hcomp, he, log_inv, log_div (div_pos (hx i) ha).ne' hd.ne',
      log_div (hx i).ne' ha.ne']
    ring
  unfold BetaPrime.logLik chartLik
  simp only [h, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]

/-- A sufficiently large third central log moment at narrow span
excludes every finite fit with threshold and power held. -/
theorem held_power_narrow_span_no_mle (hn : 0 < n) {x : Fin n → ℝ} {μ a D L U : ℝ}
    (hx : ∀ i, μ < x i) (ha : 0 < a)
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hspan : U - L ≤ D)
    (hy : ∀ i, log (poweredExcess x μ a i) ∈ Icc L U)
    (hskew : 69 * D ^ 4 < |(∑ i, (log (poweredExcess x μ a i) -
      (∑ j, log (poweredExcess x μ a j)) / n) ^ 3) / n|) :
    ¬ ∃ z ∈ Ioi (0 : ℝ) ×ˢ Beta.domain,
      IsMaxOn (fun w : ℝ × (ℝ × ℝ) ↦ logLikelihood x μ a w.1 w.2)
        (Ioi (0 : ℝ) ×ˢ Beta.domain) z := by
  rintro ⟨⟨b, p, q⟩, hb, hmax⟩
  have hb' : 0 < b := hb.1
  have hp : 0 < p := hb.2.1
  have hq : 0 < q := hb.2.2
  let X := poweredExcess x μ a
  let y := fun i ↦ log (X i)
  let s := log (b ^ a)
  have hX (i : Fin n) : 0 < X i := rpow_pos_of_pos (sub_pos.mpr (hx i)) _
  have he (t : ℝ) (ht : 0 < t) (d e : ℝ) :
      logLikelihood x μ a t (d, e) = chartLik y (log (t ^ a)) d e + powerJacobian x μ a := by
    rw [logLik_powered_excess hx a ht, chart_transfer hX (rpow_pos_of_pos ht _) d e]
  have hchart (t d e : ℝ) (hd : 0 < d) (he' : 0 < e) : chartLik y t d e ≤ chartLik y s p q := by
    let c := exp (t / a)
    have hc : 0 < c := exp_pos _
    have hcs : log (c ^ a) = t := by
      rw [log_rpow hc]
      dsimp [c]
      rw [log_exp]
      field_simp
    have hh := hmax (show (c, d, e) ∈ Ioi (0 : ℝ) ×ˢ Beta.domain from ⟨hc, hd, he'⟩)
    change logLikelihood x μ a c (d, e) ≤ logLikelihood x μ a b (p, q) at hh
    rw [he c hc, he b hb', hcs] at hh
    exact (add_le_add_iff_right _).1 hh
  have hsmax : IsMaxOn (fun t ↦ chartLik y t p q) univ s :=
    fun t _ ↦ hchart t p q hp hq
  have hpmax : IsMaxOn (fun t ↦ chartLik y s t q) (Ioi 0) p :=
    fun t ht ↦ hchart s t q ht hq
  have hqmax : IsMaxOn (fun t ↦ chartLik y s p t) (Ioi 0) q :=
    fun t ht ↦ hchart s p t hp ht
  have hs₀ := (hsmax.isLocalMax (Filter.univ_mem : (univ : Set ℝ) ∈ 𝓝 s)).deriv_eq_zero
  have hp₀ := (hpmax.isLocalMax (Ioi_mem_nhds hp)).deriv_eq_zero
  have hq₀ := (hqmax.isLocalMax (Ioi_mem_nhds hq)).deriv_eq_zero
  exact (not_le_of_gt hskew)
    (narrow_span_stationary_bound hn hD hD' hspan y hy s hp hq hs₀ hp₀ hq₀)

end
end UnivariateML.ShapeFacesFellerPareto
