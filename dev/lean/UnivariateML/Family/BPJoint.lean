module

public import UnivariateML.Family.Endpoint3BetaPrime
public import UnivariateML.Generic.Compactification
public import UnivariateML.Generic.Escape
public import Mathlib.Analysis.SpecialFunctions.Integrals.Basic

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Joint beta-prime escape in the harmonic concentration chart
-/

@[expose] public section

open Real Set Filter Topology MeasureTheory

namespace UnivariateML.BPJoint

noncomputable section

variable {n : ℕ}

/-- Harmonic concentration. -/
def concentration (p q : ℝ) : ℝ := p * q / (p + q)

/-- Log height of the density at the mode on the log-observation scale. -/
def peak (p q : ℝ) : ℝ :=
  p * log (p / (p + q)) + q * log (q / (p + q)) - Beta.logBeta (p, q)

/-! ## Uniform bound for the mode height -/

private def kernel (p q z : ℝ) : ℝ := z ^ (p - 1) * (1 - z) ^ (q - 1)

private theorem kernel_cast (p q : ℝ) {z : ℝ} (hz : z ∈ Ioo (0 : ℝ) 1) :
    (kernel p q z : ℂ) =
      (z : ℂ) ^ ((p : ℂ) - 1) * (1 - (z : ℂ)) ^ ((q : ℂ) - 1) := by
  have h₂ : 0 < 1 - z := sub_pos.mpr hz.2
  rw [kernel, Complex.ofReal_mul, Complex.ofReal_cpow hz.1.le, Complex.ofReal_cpow h₂.le]
  push_cast
  rfl

private theorem kernel_integrable {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    IntegrableOn (kernel p q) (Ioo 0 1) := by
  have hi := (Complex.betaIntegral_convergent
    (show 0 < (p : ℂ).re from hp) (show 0 < (q : ℂ).re from hq)).1
  have he : (fun z : ℝ ↦ (kernel p q z : ℂ)) =ᵐ[volume.restrict (Ioo 0 1)]
      (fun z : ℝ ↦ (z : ℂ) ^ ((p : ℂ) - 1) * (1 - (z : ℂ)) ^ ((q : ℂ) - 1)) :=
    (ae_restrict_mem measurableSet_Ioo).mono fun _ hz ↦ kernel_cast p q hz
  exact (Integrable.iff_ofReal (𝕜 := ℂ)).mpr
    ((hi.mono_set Ioo_subset_Ioc_self).congr he.symm)

private theorem normalizer_eq_integral {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    Beta.normalizer (p, q) = ∫ z in Ioo (0 : ℝ) 1, kernel p q z := by
  apply Complex.ofReal_injective
  rw [← integral_complex_ofReal, ← integral_Ioc_eq_integral_Ioo,
    ← intervalIntegral.integral_of_le zero_le_one]
  trans Complex.betaIntegral p q
  · rw [Complex.betaIntegral_eq_Gamma_mul_div _ _
      (show 0 < (p : ℂ).re from hp) (show 0 < (q : ℂ).re from hq), ← Complex.ofReal_add]
    simp only [Beta.normalizer, Complex.ofReal_div, Complex.ofReal_mul, Complex.Gamma_ofReal]
  · exact intervalIntegral.integral_congr_Ioo_of_le zero_le_one
      (fun _ hz ↦ (kernel_cast p q hz).symm)

private theorem left_integral {p q r : ℝ} (hp : 0 < p) (hq : 0 < q) (hr : r ∈ Ioo (0 : ℝ) 1) :
    r ^ p * (1 - r) ^ q / p ≤ ∫ z in Ioo (0 : ℝ) r, kernel p q z := by
  have hr0 : 0 < r := hr.1
  have hr1 : r < 1 := hr.2
  let c := (1 - r) ^ q
  have hi := (intervalIntegral.intervalIntegrable_rpow' (a := 0) (b := r)
    (show -1 < p - 1 by linarith)).1
  have hdom : Ioo (0 : ℝ) r ⊆ Ioc 0 r := Ioo_subset_Ioc_self
  have hval : (∫ z in Ioo (0 : ℝ) r, c * z ^ (p - 1)) = r ^ p * c / p := by
    rw [← integral_Ioc_eq_integral_Ioo, ← intervalIntegral.integral_of_le hr0.le,
      intervalIntegral.integral_const_mul, integral_rpow (Or.inl (by linarith : -1 < p - 1)),
      sub_add_cancel, zero_rpow hp.ne', sub_zero]
    ring
  rw [← hval]
  apply integral_mono_ae ((hi.mono_set hdom).const_mul c)
    ((kernel_integrable hp hq).mono_set (Ioo_subset_Ioo_right hr1.le))
  filter_upwards [ae_restrict_mem measurableSet_Ioo] with z hz
  have hz1 : 0 < 1 - z := sub_pos.mpr (hz.2.trans hr1)
  have here : c ≤ (1 - z) ^ (q - 1) := by
    refine (rpow_le_rpow (sub_nonneg.mpr hr1.le) (sub_le_sub_left hz.2.le 1) hq.le).trans ?_
    exact rpow_le_rpow_of_exponent_ge hz1 (by linarith [hz.1]) (by linarith)
  exact mul_comm c (z ^ (p - 1)) ▸ mul_le_mul_of_nonneg_left here (rpow_nonneg hz.1.le _)

private theorem right_integral {p q r : ℝ} (hp : 0 < p) (hq : 0 < q) (hr : r ∈ Ioo (0 : ℝ) 1) :
    r ^ p * (1 - r) ^ q / q ≤ ∫ z in Ioo r (1 : ℝ), kernel p q z := by
  have hl := left_integral hq hp (r := 1 - r) ⟨by linarith [hr.2], by linarith [hr.1]⟩
  rw [sub_sub_cancel, mul_comm ((1 - r) ^ q)] at hl
  refine hl.trans_eq ?_
  rw [← integral_Ioc_eq_integral_Ioo, ← integral_Ioc_eq_integral_Ioo,
    ← intervalIntegral.integral_of_le (by linarith [hr.2] : 0 ≤ 1 - r),
    ← intervalIntegral.integral_of_le hr.2.le]
  have h := intervalIntegral.integral_comp_sub_left (f := kernel p q) (a := 0) (b := 1 - r) 1
  simp only [sub_zero, sub_sub_cancel] at h
  rw [← h]
  apply intervalIntegral.integral_congr
  intro z _
  simp [kernel, sub_sub_cancel, mul_comm]

/-- The logarithmic mode height is bounded above by log harmonic
concentration, uniformly in the shape ratio. -/
theorem peak_le_log_concentration {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    peak p q ≤ log (concentration p q) := by
  let r := p / (p + q)
  have hr : r ∈ Ioo (0 : ℝ) 1 :=
    ⟨div_pos hp (add_pos hp hq), (div_lt_one (add_pos hp hq)).mpr (lt_add_of_pos_right _ hq)⟩
  have he : 1 - r = q / (p + q) := by
    dsimp [r]
    field_simp [(add_pos hp hq).ne']
    ring
  have hsplit : (∫ z in Ioo 0 r, kernel p q z) + (∫ z in Ioo r 1, kernel p q z) =
      ∫ z in Ioo (0 : ℝ) 1, kernel p q z := by
    have hi : IntervalIntegrable (kernel p q) volume 0 1 := by
      rw [intervalIntegrable_iff_integrableOn_Ioo_of_le zero_le_one]
      exact kernel_integrable hp hq
    simp only [← integral_Ioc_eq_integral_Ioo,
      ← intervalIntegral.integral_of_le hr.1.le, ← intervalIntegral.integral_of_le hr.2.le,
      ← intervalIntegral.integral_of_le zero_le_one]
    exact intervalIntegral.integral_add_adjacent_intervals
      (hi.mono_set (uIcc_of_le hr.1.le ▸ uIcc_of_le (zero_le_one : (0 : ℝ) ≤ 1)
        ▸ Icc_subset_Icc_right hr.2.le))
      (hi.mono_set (uIcc_of_le hr.2.le ▸ uIcc_of_le (zero_le_one : (0 : ℝ) ≤ 1)
        ▸ Icc_subset_Icc_left hr.1.le))
  have h := add_le_add (left_integral hp hq hr) (right_integral hp hq hr)
  rw [hsplit, ← normalizer_eq_integral hp hq] at h
  have h₁ : 0 < r ^ p := rpow_pos_of_pos hr.1 _
  have h₂ : 0 < (1 - r) ^ q := rpow_pos_of_pos (sub_pos.mpr hr.2) _
  have hk : 0 < concentration p q := div_pos (mul_pos hp hq) (add_pos hp hq)
  have hu : r ^ p * (1 - r) ^ q / concentration p q ≤ Beta.normalizer (p, q) := by
    convert h using 1
    unfold concentration
    field_simp [hp.ne', hq.ne', (add_pos hp hq).ne']
    ring
  have hl := log_le_log (div_pos (mul_pos h₁ h₂) hk) hu
  rw [log_div (mul_pos h₁ h₂).ne' hk.ne', log_mul h₁.ne' h₂.ne',
    log_rpow hr.1, log_rpow (sub_pos.mpr hr.2), he] at hl
  unfold peak Beta.logBeta
  change p * log r + q * log (q / (p + q)) - log (Beta.normalizer (p, q)) ≤ _
  linarith

/-! ## Uniform bounds for the loss and joint superlevels -/

/-- Centered beta-prime loss on the log scale. -/
def deviance (r z : ℝ) : ℝ :=
  (log (1 + r * (exp z - 1)) - r * z) / (r * (1 - r))

private theorem deviance_deriv {r : ℝ} (hr : r ∈ Ioo (0 : ℝ) 1) (z : ℝ) :
    HasDerivAt (deviance r) ((exp z - 1) / (1 + r * (exp z - 1))) z := by
  have ha : 0 < r := hr.1
  have hb : 0 < 1 - r := sub_pos.mpr hr.2
  have hd : 0 < 1 + r * (exp z - 1) := by
    nlinarith [mul_pos ha (exp_pos z)]
  have h := (((((hasDerivAt_exp z).sub_const 1).const_mul r).const_add 1).log hd.ne').sub
    ((hasDerivAt_id z).const_mul r)
  convert h.div_const (r * (1 - r)) using 1
  · rfl
  · have he : 1 + r * (exp z - 1) = 1 + exp z * r - r := by ring
    rw [he] at hd ⊢
    field_simp [hd.ne', ha.ne', hb.ne']
    ring

private theorem deviance_lower {r : ℝ} (hr : r ∈ Ioo (0 : ℝ) 1) (z : ℝ) :
    |z| + exp (-|z|) - 1 ≤ deviance r z := by
  have ha : 0 < r := hr.1
  have hb : 0 < 1 - r := sub_pos.mpr hr.2
  have hd (t : ℝ) : 0 < 1 + r * (exp t - 1) := by
    nlinarith [mul_pos ha (exp_pos t)]
  have hzero : deviance r 0 = 0 := by simp [deviance]
  by_cases hz : 0 ≤ z
  · rw [abs_of_nonneg hz]
    let f := fun t : ℝ ↦ deviance r t - (t + exp (-t) - 1)
    have hder (t : ℝ) : HasDerivAt f
        ((exp t - 1) / (1 + r * (exp t - 1)) - (1 - exp (-t))) t := by
      convert (deviance_deriv hr t).sub (((hasDerivAt_id t).add
        ((hasDerivAt_exp (-t)).comp t ((hasDerivAt_id t).neg))).sub_const 1) using 1
      · rfl
      · ring
    have hmono : MonotoneOn f (Ici 0) := by
      apply monotoneOn_of_deriv_nonneg (convex_Ici _)
        (fun t _ ↦ (hder t).continuousAt.continuousWithinAt)
        (fun t _ ↦ (hder t).differentiableAt.differentiableWithinAt)
      intro t ht
      have ht' : 0 ≤ t := interior_subset ht
      rw [(hder t).deriv, exp_neg]
      apply sub_nonneg.mpr
      have he : 1 - (exp t)⁻¹ = (exp t - 1) / exp t := by
        field_simp [(exp_pos t).ne']
      rw [he]
      have hx : 1 ≤ exp t := one_le_exp_iff.mpr ht'
      exact div_le_div_of_nonneg_left (sub_nonneg.mpr hx) (hd t) (by nlinarith)
    have h := hmono (show (0 : ℝ) ∈ Ici 0 from le_rfl) hz hz
    simp only [f, hzero, neg_zero, exp_zero, zero_add, sub_self] at h
    linarith
  · have hz' : z ≤ 0 := le_of_not_ge hz
    rw [abs_of_nonpos hz', neg_neg]
    let f := fun t : ℝ ↦ deviance r t - (-t + exp t - 1)
    have hder (t : ℝ) : HasDerivAt f
        ((exp t - 1) / (1 + r * (exp t - 1)) - (-1 + exp t)) t := by
      convert (deviance_deriv hr t).sub ((((hasDerivAt_id t).neg).add
        (hasDerivAt_exp t)).sub_const 1) using 1
      rfl
    have hanti : AntitoneOn f (Iic 0) := by
      apply antitoneOn_of_deriv_nonpos (convex_Iic _)
        (fun t _ ↦ (hder t).continuousAt.continuousWithinAt)
        (fun t _ ↦ (hder t).differentiableAt.differentiableWithinAt)
      intro t ht
      have ht' : t ≤ 0 := interior_subset ht
      rw [(hder t).deriv]
      apply sub_nonpos.mpr
      have hx : exp t ≤ 1 := exp_le_one_iff.mpr ht'
      apply (div_le_iff₀ (hd t)).2
      nlinarith [mul_nonneg ha.le (sq_nonneg (exp t - 1))]
    have h := hanti hz' (show (0 : ℝ) ∈ Iic 0 from le_rfl) hz'
    simp only [f, hzero, neg_zero, exp_zero, zero_add, sub_self] at h
    linarith

/-- Joint chart domain in the coordinates (shape ratio, concentration, log mode). -/
def domain : Set (ℝ × ℝ × ℝ) := Ioo 0 1 ×ˢ (Ioi 0 ×ˢ univ)

/-- Beta-prime log likelihood expressed in the joint chart. -/
def objective (x : Fin n → ℝ) (v : ℝ × ℝ × ℝ) : ℝ :=
  BetaPrime.logLik x (exp v.2.2 * (1 - v.1) / v.1)
    (v.2.1 / (1 - v.1), v.2.1 / v.1)

private theorem centered_likelihood {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {r k m : ℝ} (hr : r ∈ Ioo (0 : ℝ) 1) (hk : 0 < k) :
    objective x (r, k, m) = n * peak (k / (1 - r)) (k / r) -
      k * (∑ i, deviance r (log (x i) - m)) - ∑ i, log (x i) := by
  have hr0 : 0 < r := hr.1
  have hr1 : 0 < 1 - r := sub_pos.mpr hr.2
  let p := k / (1 - r)
  let q := k / r
  let b := exp m * (1 - r) / r
  have hb : 0 < b := div_pos (mul_pos (exp_pos _) hr1) hr0
  have hsum : p + q = k / (r * (1 - r)) := by
    dsimp [p, q]
    field_simp [hr0.ne', hr1.ne']
    ring
  have hp : p / (p + q) = r := by
    rw [hsum]
    dsimp [p]
    field_simp [hk.ne', hr0.ne', hr1.ne']
  have hq : q / (p + q) = 1 - r := by
    rw [hsum]
    dsimp [q]
    field_simp [hk.ne', hr0.ne', hr1.ne']
  have hlogb : log b = m + log (1 - r) - log r := by
    dsimp [b]
    rw [log_div (mul_pos (exp_pos _) hr1).ne' hr0.ne', log_mul (exp_pos _).ne' hr1.ne',
      log_exp]
  change BetaPrime.logLik x b (p, q) = _
  unfold BetaPrime.logLik
  change _ = n * peak p q - k * (∑ i, deviance r (log (x i) - m)) - ∑ i, log (x i)
  rw [← show (∑ _ : Fin n, peak p q) = n * peak p q by simp, Finset.mul_sum,
    ← Finset.sum_sub_distrib, ← Finset.sum_sub_distrib]
  apply Finset.sum_congr rfl
  intro i _
  dsimp only [Prod.fst, Prod.snd]
  have hd : 1 + r * (exp (log (x i) - m) - 1) = (1 - r) * (1 + x i / b) := by
    rw [exp_sub, exp_log (hx i)]
    dsimp [b]
    field_simp [hr0.ne', hr1.ne', (exp_pos m).ne']
    ring
  have hz : 0 < 1 + x i / b := add_pos one_pos (div_pos (hx i) hb)
  rw [peak, hp, hq, deviance, hd, log_mul hr1.ne' hz.ne',
    log_div (hx i).ne' hb.ne', hlogb]
  dsimp [p, q]
  field_simp [hr0.ne', hr1.ne']
  ring

private theorem concentration_chart {r k : ℝ} (hr : r ∈ Ioo (0 : ℝ) 1) (hk : 0 < k) :
    concentration (k / (1 - r)) (k / r) = k := by
  unfold concentration
  field_simp [hr.1.ne', (sub_pos.mpr hr.2).ne',
    (add_pos (div_pos hk (sub_pos.mpr hr.2)) (div_pos hk hr.1)).ne']
  ring

private theorem loss_bound {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃ d > 0, ∀ r ∈ Ioo (0 : ℝ) 1, ∀ m,
      d ≤ ∑ i, deviance r (log (x i) - m) := by
  obtain ⟨i, j, hij⟩ := hne
  have hij' : log (x i) ≠ log (x j) := fun he ↦ hij (by
    simpa only [exp_log (hx i), exp_log (hx j)] using congrArg exp he)
  let t := |log (x i) - log (x j)| / 2
  have ht : 0 < t := half_pos (abs_pos.mpr (sub_ne_zero.mpr hij'))
  let B := fun u : ℝ ↦ u + exp (-u) - 1
  have hB₀ : B 0 = 0 := by simp [B]
  have hmono : StrictMonoOn B (Ici 0) := by
    have hd (u : ℝ) : HasDerivAt B (1 - exp (-u)) u := by
      convert (((hasDerivAt_id u).add ((hasDerivAt_exp (-u)).comp u
        ((hasDerivAt_id u).neg))).sub_const 1) using 1
      · rfl
      · ring
    apply strictMonoOn_of_deriv_pos (convex_Ici _)
      (fun u _ ↦ (hd u).continuousAt.continuousWithinAt)
    intro u hu
    rw [interior_Ici] at hu
    rw [(hd u).deriv]
    exact sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos hu))
  refine ⟨B t, hB₀ ▸ hmono (show (0 : ℝ) ∈ Ici 0 from le_rfl) ht.le ht,
    fun r hr m ↦ ?_⟩
  have hb (u : ℝ) : 0 ≤ B |u| := by
    have h := hmono.monotoneOn (show (0 : ℝ) ∈ Ici 0 from le_rfl) (abs_nonneg u) (abs_nonneg u)
    rwa [hB₀] at h
  have chooseIndex : ∃ j : Fin n, t ≤ |log (x j) - m| := by
    by_contra! h
    have htri := abs_add_le (log (x i) - m) (-(log (x j) - m))
    rw [show log (x i) - m + -(log (x j) - m) = log (x i) - log (x j) by ring,
      abs_neg] at htri
    linarith only [htri, h i, h j, show t = _ / 2 from rfl]
  obtain ⟨j, hj⟩ := chooseIndex
  have hsingle := Finset.single_le_sum (f := fun j ↦ deviance r (log (x j) - m))
    (fun u _ ↦ (hb _).trans (deviance_lower hr _)) (Finset.mem_univ j)
  exact (hmono.monotoneOn ht.le (abs_nonneg _) hj).trans ((deviance_lower hr _).trans hsingle)

/-- Every finite joint superlevel has concentration bounded away from
zero and infinity and a bounded log mode, uniformly over the shape ratio. -/
theorem superlevel_envelope (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (A : ℝ) :
    ∃ lo hi R : ℝ, 0 < lo ∧
      ∀ v ∈ domain, A ≤ objective x v → v.2 ∈ Icc lo hi ×ˢ Icc (-R) R := by
  let J := ∑ i, log (x i)
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  obtain ⟨d, hd, hloss⟩ := loss_bound hx hne
  have hbound {v : ℝ × ℝ × ℝ} (hv : v ∈ domain) :
      objective x v ≤ n * log v.2.1 - v.2.1 * d - J := by
    rw [centered_likelihood hx hv.1 hv.2.1]
    have hpeak := peak_le_log_concentration (div_pos hv.2.1 (sub_pos.mpr hv.1.2))
      (div_pos hv.2.1 hv.1.1)
    rw [concentration_chart hv.1 hv.2.1] at hpeak
    exact sub_le_sub_right (sub_le_sub (mul_le_mul_of_nonneg_left hpeak hn'.le)
      (mul_le_mul_of_nonneg_left (hloss _ hv.1 _) (le_of_lt hv.2.1))) _
  have hlo : ∀ᶠ k in 𝓝[>] (0 : ℝ), log k < (A + J) / n :=
    tendsto_log_nhdsGT_zero.eventually (eventually_lt_atBot _)
  obtain ⟨lo, hlo0, hlo⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp hlo
  have hdiv : Tendsto (fun k : ℝ ↦ log k / k) atTop (𝓝 0) :=
    isLittleO_log_id_atTop.tendsto_div_nhds_zero
  obtain ⟨B, hB⟩ := eventually_atTop.1 (hdiv.eventually (Iio_mem_nhds
    (show 0 < d / (2 * n) by positivity)))
  let hi := max (max B 1) (2 * (|A + J| + 1) / d)
  let i : Fin n := ⟨0, hn⟩
  let R := |log (x i)| + |n * log hi - J - A| / lo + 1
  refine ⟨lo, hi, R, hlo0, ?_⟩
  intro v hv hA
  rcases v with ⟨r, k, m⟩
  have hk : 0 < k := hv.2.1
  have hhi0 : 0 < hi := zero_lt_one.trans_le ((le_max_right B 1).trans (le_max_left _ _))
  have hlo' : lo ≤ k := by
    by_contra! h
    have hlog : log k < (A + J) / n := hlo ⟨hk, h⟩
    have h := (lt_div_iff₀ hn').mp hlog
    have h' := hbound hv
    dsimp only at h'
    nlinarith [mul_pos hk hd]
  have hhi' : k ≤ hi := by
    by_contra! h
    have hB' : B ≤ k := ((le_max_left B 1).trans (le_max_left _ _)).trans h.le
    have hlog := (div_lt_iff₀ hk).mp (hB k hB')
    have hlog' : (n : ℝ) * log k < k * d / 2 := by
      have hh := mul_lt_mul_of_pos_left hlog hn'
      have he : (n : ℝ) * (d / (2 * n) * k) = k * d / 2 := by field_simp
      rwa [he] at hh
    have hk' := (div_le_iff₀ hd).mp ((le_max_right (max B 1) _).trans h.le)
    have h' := hbound hv
    dsimp only at h'
    nlinarith only [hA, h', hlog', hk', neg_abs_le (A + J)]
  refine ⟨⟨hlo', hhi'⟩, ?_⟩
  have hsum : |log (x i) - m| - 1 ≤ ∑ j, deviance r (log (x j) - m) := by
    have hz (z : ℝ) : 0 ≤ deviance r z := by
      refine le_trans ?_ (deviance_lower hv.1 z)
      linarith [add_one_le_exp (-|z|)]
    have hh := Finset.single_le_sum (f := fun j ↦ deviance r (log (x j) - m))
      (fun j _ ↦ hz _) (Finset.mem_univ i)
    linarith [deviance_lower hv.1 (log (x i) - m), exp_pos (-|log (x i) - m|)]
  have hpeak := peak_le_log_concentration (div_pos hk (sub_pos.mpr hv.1.2))
    (div_pos hk hv.1.1)
  rw [concentration_chart hv.1 hk] at hpeak
  have hlog := log_le_log hk hhi'
  have hc := centered_likelihood hx (r := r) (m := m) hv.1 hk
  have hupper : k * (|log (x i) - m| - 1) ≤ n * log hi - J - A := by
    dsimp only [J] at *
    linarith only [hc, hA, mul_le_mul_of_nonneg_left hsum hk.le,
      mul_le_mul_of_nonneg_left (hpeak.trans hlog) hn'.le]
  have hM : |log (x i) - m| ≤ |n * log hi - J - A| / lo + 1 := by
    by_cases h : |log (x i) - m| - 1 ≤ 0
    · linarith [div_nonneg (abs_nonneg (n * log hi - J - A)) hlo0.le]
    · have H := mul_le_mul_of_nonneg_right hlo' (le_of_not_ge h)
      have H' : |log (x i) - m| - 1 ≤ |n * log hi - J - A| / lo :=
        (le_div_iff₀ hlo0).mpr (by
          rw [mul_comm]
          exact H.trans (hupper.trans (le_abs_self _)))
      linarith
  have habs : |m| ≤ R := by
    have hh := abs_sub (log (x i)) (log (x i) - m)
    rw [sub_sub_cancel] at hh
    dsimp [R]
    linarith
  exact abs_le.mp habs

/-! ## Continuous limits with moving nuisance parameters -/

private theorem logGamma_step {q : ℝ} (hq : 0 < q) :
    log (Real.Gamma (q + 1)) - log (Real.Gamma q) = log q := by
  rw [Real.Gamma_add_one hq.ne', log_mul hq.ne' (Gamma_pos_of_pos hq).ne']
  ring

private theorem logGamma_increment_bounds {p q : ℝ} (hp : 0 < p) (hq : 1 < q) :
    p * log (q - 1) ≤ log (Real.Gamma (p + q)) - log (Real.Gamma q) ∧
      log (Real.Gamma (p + q)) - log (Real.Gamma q) ≤ p * log (p + q) := by
  have h := convexOn_log_Gamma.slope_mono_adjacent (x := q - 1) (y := q) (z := q + p)
    (sub_pos.mpr hq) (by change 0 < q + p; linarith) (by linarith) (by linarith)
  simp only [Function.comp_apply, sub_sub_cancel, add_sub_cancel_left, div_one] at h
  have he : log (Real.Gamma q) - log (Real.Gamma (q - 1)) = log (q - 1) := by
    simpa only [sub_add_cancel] using logGamma_step (sub_pos.mpr hq)
  rw [he] at h
  have h' := convexOn_log_Gamma.slope_mono_adjacent (x := q) (y := q + p) (z := q + p + 1)
    (zero_lt_one.trans hq) (by change 0 < q + p + 1; linarith) (by linarith) (by linarith)
  simp only [Function.comp_apply, add_sub_cancel_left, div_one,
    logGamma_step (show 0 < q + p by linarith)] at h'
  rw [add_comm p q]
  exact ⟨by simpa only [mul_comm] using (le_div_iff₀ hp).mp h,
    by simpa only [mul_comm] using (div_le_iff₀ hp).mp h'⟩

private theorem logGamma_ratio_moving {ι : Type*} {l : Filter ι}
    {p q : ι → ℝ} {k : ℝ} (hk : 0 < k) (hp : Tendsto p l (𝓝 k)) (hq : Tendsto q l atTop) :
    Tendsto (fun j ↦ log (Real.Gamma (p j + q j)) - log (Real.Gamma (q j)) -
      p j * log (q j)) l (𝓝 0) := by
  have hqi := tendsto_inv_atTop_zero.comp hq
  have hfun (a : ι → ℝ) {a₀ : ℝ} (ha : Tendsto a l (𝓝 a₀)) :
      Tendsto (fun j ↦ p j * log (1 + a j * (q j)⁻¹)) l (𝓝 0) := by
    have hh := (ha.mul hqi).const_add 1
    simp only [mul_zero, add_zero] at hh
    simpa only [log_one, mul_zero, Function.comp_apply] using
      hp.mul ((continuousAt_log one_ne_zero).tendsto.comp hh)
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le'
    (hfun (fun _ ↦ -1) tendsto_const_nhds) (hfun p hp)
  · filter_upwards [hp.eventually (Ioi_mem_nhds hk), hq.eventually (eventually_gt_atTop 1)]
      with j hj hj'
    have h := (logGamma_increment_bounds hj hj').1
    have hpos : 0 < q j := zero_lt_one.trans hj'
    have he : 1 + -1 * (q j)⁻¹ = (q j - 1) / q j := by field_simp; ring
    rw [he, log_div (sub_pos.mpr hj').ne' hpos.ne']
    linarith
  · filter_upwards [hp.eventually (Ioi_mem_nhds hk), hq.eventually (eventually_gt_atTop 1)]
      with j hj hj'
    have h := (logGamma_increment_bounds hj hj').2
    have hpos : 0 < q j := zero_lt_one.trans hj'
    have he : 1 + p j * (q j)⁻¹ = (p j + q j) / q j := by field_simp; ring
    rw [he, log_div (add_pos hj hpos).ne' hpos.ne']
    linarith

private theorem density_gamma_limit {ι : Type*} {l : Filter ι} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p q b : ι → ℝ} {k a : ℝ}
    (hk : 0 < k) (ha : 0 < a) (hp : Tendsto p l (𝓝 k)) (hq : Tendsto q l atTop)
    (hb : ∀ᶠ j in l, 0 < b j) (hr : Tendsto (fun j ↦ q j / b j) l (𝓝 a)) :
    Tendsto (fun j ↦ BetaPrime.logLik x (b j) (p j, q j)) l
      (𝓝 (∑ i, Gamma.logDensity k a (x i))) := by
  have hqp := hq.eventually (eventually_gt_atTop (0 : ℝ))
  have hpp := hp.eventually (Ioi_mem_nhds hk)
  have hbi : Tendsto (fun j ↦ (b j)⁻¹) l (𝓝 0) := by
    have H := hr.mul (tendsto_inv_atTop_zero.comp hq)
    rw [mul_zero] at H
    apply H.congr'
    filter_upwards [hqp] with j hj
    change q j / b j * (q j)⁻¹ = (b j)⁻¹
    rw [div_eq_mul_inv, mul_right_comm, mul_inv_cancel₀ hj.ne', one_mul]
  have hGamma (k : ℝ) (hk : 0 < k) :
      ContinuousAt (fun y ↦ log (Real.Gamma y)) k :=
    (hasDerivAt_Gamma_of_pos hk).continuousAt.log (Gamma_pos_of_pos hk).ne'
  have hlog := (continuousAt_log ha.ne').tendsto.comp hr
  have hΓ := logGamma_ratio_moving hk hp hq
  have Hnorm := (hGamma k hk).tendsto.comp hp
  unfold BetaPrime.logLik
  apply tendsto_finsetSum
  intro i _
  let t := fun j ↦ (b j)⁻¹ * x i
  have ht : Tendsto t l (𝓝 0) := by simpa [t] using hbi.mul_const (x i)
  have htn : Tendsto t l (𝓝[≠] 0) :=
    tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within _ ht <|
      hb.mono fun j hj ↦ (mul_pos (inv_pos.mpr hj) (hx i)).ne'
  have hz : Tendsto (fun v : ℝ ↦ log (1 + v) / v) (𝓝[≠] 0) (𝓝 1) := by
    have H₀ : HasDerivAt (fun v : ℝ ↦ 1 + v) 1 0 := (hasDerivAt_id (0 : ℝ)).const_add 1
    have H := (hasDerivAt_log (show (fun v : ℝ ↦ 1 + v) 0 ≠ 0 by norm_num)).comp 0 H₀
    have H' : HasDerivAt (fun v : ℝ ↦ log (1 + v)) 1 0 := by
      change HasDerivAt (fun v : ℝ ↦ log (1 + v)) _ 0 at H
      simpa using H
    apply (hasDerivAt_iff_tendsto_slope.mp H').congr
    intro v
    simp [slope_def_field]
  have Hrat := (hr.add (hp.mul hbi)).mul_const (x i)
  simp only [mul_zero, add_zero] at Hrat
  have Hterm := Hrat.mul (hz.comp htn)
  have H := (((((hp.sub_const 1).mul_const (log (x i))).sub Hnorm).add
    (hp.mul hlog)).add hΓ).sub Hterm
  simp only [mul_one, add_zero] at H
  have he : ((k - 1) * log (x i) - log (Real.Gamma k)) + k * log a - a * x i =
      Gamma.logDensity k a (x i) := by unfold Gamma.logDensity; ring
  rw [he] at H
  apply H.congr'
  filter_upwards [hqp, hpp, hb] with j hj hjp hjb
  dsimp only [Function.comp_apply, Prod.fst, Prod.snd]
  have htj : 0 < t j := mul_pos (inv_pos.mpr hjb) (hx i)
  have hratio : t j = x i / b j := by simp [t, div_eq_mul_inv, mul_comm]
  have hfactor : (q j / b j + p j * (b j)⁻¹) * x i = (p j + q j) * t j := by
    rw [hratio]
    ring
  rw [hfactor, show (p j + q j) * t j * (log (1 + t j) / t j) =
      (p j + q j) * log (1 + t j) by field_simp [htj.ne'], hratio,
    log_div hj.ne' hjb.ne', log_div (hx i).ne' hjb.ne']
  unfold Beta.logBeta Beta.normalizer
  dsimp only [Prod.fst, Prod.snd]
  rw [log_div (mul_pos (Gamma_pos_of_pos hjp) (Gamma_pos_of_pos hj)).ne'
      (Gamma_pos_of_pos (add_pos hjp hj)).ne',
    log_mul (Gamma_pos_of_pos hjp).ne' (Gamma_pos_of_pos hj).ne']
  ring

/-- The completed chart, with gamma at ratio zero and inverse gamma at one. -/
def completedDomain : Set (ℝ × ℝ × ℝ) := Icc 0 1 ×ˢ (Ioi 0 ×ˢ univ)

/-- Gamma face in concentration and log-mode coordinates. -/
def gammaValue (x : Fin n → ℝ) (v : ℝ × ℝ) : ℝ :=
  ∑ i, Gamma.logDensity v.1 (v.1 / exp v.2) (x i)

/-- Inverse-gamma face in concentration and log-mode coordinates. -/
def inverseGammaValue (x : Fin n → ℝ) (v : ℝ × ℝ) : ℝ :=
  gammaValue (fun i ↦ (x i)⁻¹) (v.1, -v.2) - ∑ i, 2 * log (x i)

/-- The log likelihood in the chart including both proper faces. -/
def completedObjective (x : Fin n → ℝ) (v : ℝ × ℝ × ℝ) : ℝ :=
  if v.1 = 0 then gammaValue x v.2
  else if v.1 = 1 then inverseGammaValue x v.2
  else objective x v

private theorem completed_interior (x : Fin n → ℝ) {v : ℝ × ℝ × ℝ} (hv : v ∈ domain) :
    completedObjective x v = objective x v :=
  ite_eq_right hv.1.1.ne' |>.trans (ite_eq_right hv.1.2.ne)

private theorem left_chart_limit {ι : Type*} {l : Filter ι} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {u : ι → ℝ × ℝ × ℝ} {v : ℝ × ℝ} (hv : 0 < v.1)
    (ht : Tendsto u l (𝓝 (0, v))) (hu : ∀ᶠ j in l, u j ∈ domain) :
    Tendsto (fun j ↦ objective x (u j)) l (𝓝 (gammaValue x v)) := by
  let r := fun j ↦ (u j).1
  let k := fun j ↦ (u j).2.1
  let m := fun j ↦ (u j).2.2
  have hr : Tendsto r l (𝓝 0) := continuous_fst.continuousAt.tendsto.comp ht
  have hk : Tendsto k l (𝓝 v.1) := continuous_fst.continuousAt.tendsto.comp
    (continuous_snd.continuousAt.tendsto.comp ht)
  have hm : Tendsto m l (𝓝 v.2) := continuous_snd.continuousAt.tendsto.comp
    (continuous_snd.continuousAt.tendsto.comp ht)
  have h1 : Tendsto (fun j ↦ 1 - r j) l (𝓝 1) := by simpa using hr.const_sub 1
  have hp : Tendsto (fun j ↦ k j / (1 - r j)) l (𝓝 v.1) := by
    simpa only [div_one, Pi.div_def] using hk.div h1 one_ne_zero
  have hr0 : Tendsto r l (𝓝[>] (0 : ℝ)) :=
    tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within _ hr
      (hu.mono fun j h ↦ (show r j ∈ Ioi 0 from h.1.1))
  have hq : Tendsto (fun j ↦ k j / r j) l atTop := by
    simpa only [div_eq_mul_inv, Function.comp_def] using hk.pos_mul_atTop hv
      (tendsto_inv_nhdsGT_zero.comp hr0)
  have he : Tendsto (fun j ↦ exp (m j)) l (𝓝 (exp v.2)) :=
    continuous_exp.continuousAt.tendsto.comp hm
  have hrate := hp.div he (exp_pos _).ne'
  have H := density_gamma_limit hx hv (div_pos hv (exp_pos _)) hp hq
    (b := fun j ↦ exp (m j) * (1 - r j) / r j)
    (hu.mono fun j h ↦ div_pos (mul_pos (exp_pos _) (sub_pos.mpr h.1.2)) h.1.1)
    (hrate.congr' (hu.mono fun j h ↦ by
      have hr0 : 0 < r j := h.1.1
      have hr1 : 0 < 1 - r j := sub_pos.mpr h.1.2
      change k j / (1 - r j) / exp (m j) = (k j / r j) / (exp (m j) * (1 - r j) / r j)
      field_simp [hr0.ne', hr1.ne', (exp_pos (m j)).ne']))
  exact H

private theorem right_chart_limit {ι : Type*} {l : Filter ι} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {u : ι → ℝ × ℝ × ℝ} {v : ℝ × ℝ} (hv : 0 < v.1)
    (ht : Tendsto u l (𝓝 (1, v))) (hu : ∀ᶠ j in l, u j ∈ domain) :
    Tendsto (fun j ↦ objective x (u j)) l (𝓝 (inverseGammaValue x v)) := by
  let T : ℝ × ℝ × ℝ → ℝ × ℝ × ℝ := fun u ↦ (1 - u.1, u.2.1, -u.2.2)
  let y := fun i ↦ (x i)⁻¹
  have hT : Continuous T := by fun_prop
  have ht' : Tendsto (T ∘ u) l (𝓝 (0, v.1, -v.2)) := by
    simpa [T] using hT.continuousAt.tendsto.comp ht
  have hu' : ∀ᶠ j in l, (T ∘ u) j ∈ domain :=
    hu.mono fun _ h ↦ ⟨⟨sub_pos.mpr h.1.2, sub_lt_self _ h.1.1⟩, h.2⟩
  have H := (left_chart_limit (x := y) (fun i ↦ inv_pos.mpr (hx i)) (v := (v.1, -v.2))
    hv ht' hu').sub_const
    (∑ i, 2 * log (x i))
  apply H.congr'
  filter_upwards [hu] with j hj
  rcases hjv : u j with ⟨r, k, m⟩
  rw [hjv] at hj
  simp only [Function.comp_apply, hjv]
  have h0 : 0 < r := hj.1.1
  have h1 : 0 < 1 - r := sub_pos.mpr hj.1.2
  have hb : 0 < exp m * (1 - r) / r := div_pos (mul_pos (exp_pos _) h1) h0
  have he := AllFreeBetaPrime.reciprocal_logLik hx hb (k / (1 - r)) (k / r)
  change objective y (T (r, k, m)) - (∑ i, 2 * log (x i)) = objective x (r, k, m)
  unfold objective
  dsimp only [Prod.fst, Prod.snd]
  rw [he]
  simp only [T, sub_sub_cancel, exp_neg]
  congr 2
  field_simp [h0.ne', h1.ne', (exp_pos m).ne']

private theorem gamma_continuous (x : Fin n → ℝ) {v : ℝ × ℝ} (hv : 0 < v.1) :
    ContinuousAt (gammaValue x) v := by
  unfold gammaValue
  apply tendsto_finsetSum
  intro i _
  unfold Gamma.logDensity
  have hk : ContinuousAt (fun v : ℝ × ℝ ↦ v.1) v := continuous_fst.continuousAt
  have hb : ContinuousAt (fun v : ℝ × ℝ ↦ v.1 / exp v.2) v :=
    hk.div (continuous_exp.comp continuous_snd).continuousAt (exp_pos _).ne'
  have hΓ := (hasDerivAt_Gamma_of_pos hv).continuousAt.comp hk
  exact (((hk.mul (hb.log (div_pos hv (exp_pos _)).ne')).sub
    (hΓ.log (Gamma_pos_of_pos hv).ne')).add ((hk.sub continuousAt_const).mul
    continuousAt_const)).sub (hb.mul continuousAt_const)

private theorem objective_continuous {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {v : ℝ × ℝ × ℝ} (hv : v ∈ domain) : ContinuousAt (objective x) v := by
  have hr : ContinuousAt (fun v : ℝ × ℝ × ℝ ↦ v.1) v := continuous_fst.continuousAt
  have hk : ContinuousAt (fun v : ℝ × ℝ × ℝ ↦ v.2.1) v :=
    continuous_fst.continuousAt.comp continuous_snd.continuousAt
  have hm : ContinuousAt (fun v : ℝ × ℝ × ℝ ↦ v.2.2) v :=
    continuous_snd.continuousAt.comp continuous_snd.continuousAt
  have h0 : 0 < v.1 := hv.1.1
  have h1 : 0 < 1 - v.1 := sub_pos.mpr hv.1.2
  have hC : ContinuousAt (fun w : ℝ × ℝ × ℝ ↦ 1 - w.1) v := continuousAt_const.sub hr
  have hp := hk.div hC h1.ne'
  have hq := hk.div hr h0.ne'
  have hmE : ContinuousAt (fun w : ℝ × ℝ × ℝ ↦ exp w.2.2) v :=
    continuous_exp.continuousAt.comp hm
  have hb := (hmE.mul hC).div hr h0.ne'
  have hvp := div_pos hv.2.1 h1
  have hvq := div_pos hv.2.1 h0
  have hvb := div_pos (mul_pos (exp_pos v.2.2) h1) h0
  have hg {f : (ℝ × ℝ × ℝ) → ℝ} (hf : ContinuousAt f v) (hfv : 0 < f v) :
      ContinuousAt (fun w ↦ Real.Gamma (f w)) v :=
    (hasDerivAt_Gamma_of_pos hfv).continuousAt.comp hf
  have hn := ((hg hp hvp).mul (hg hq hvq)).div (hg (hp.add hq) (add_pos hvp hvq))
    (Gamma_pos_of_pos (add_pos hvp hvq)).ne'
  have ht : 0 < Beta.normalizer (v.2.1 / (1 - v.1), v.2.1 / v.1) :=
    div_pos (mul_pos (Gamma_pos_of_pos hvp) (Gamma_pos_of_pos hvq))
      (Gamma_pos_of_pos (add_pos hvp hvq))
  unfold objective BetaPrime.logLik
  apply tendsto_finsetSum
  intro i _
  have hw := (continuousAt_const (y := x i)).div hb hvb.ne'
  exact ((((hp.sub continuousAt_const).mul (hw.log (div_pos (hx i) hvb).ne')).sub
    (hb.log hvb.ne')).sub ((hp.add hq).mul ((continuousAt_const.add hw).log
    (add_pos one_pos (div_pos (hx i) hvb)).ne'))).sub (hn.log ht.ne')

/-- The two gamma faces are the continuous extensions in the joint chart.
Both concentration and log mode may vary while approaching a face. -/
theorem continuous_completed (x : Fin n → ℝ) (hx : ∀ i, 0 < x i) :
    ContinuousOn (completedObjective x) completedDomain := by
  intro v hv
  rcases v with ⟨r, k, m⟩
  have hk : 0 < k := hv.2.1
  have hr : r ∈ Icc (0 : ℝ) 1 := hv.1
  have hD : IsOpen domain := isOpen_Ioo.prod (isOpen_Ioi.prod isOpen_univ)
  have hSplit : completedDomain = domain ∪
      ({w : ℝ × ℝ × ℝ | w.1 = 0 ∧ 0 < w.2.1} ∪
       {w : ℝ × ℝ × ℝ | w.1 = 1 ∧ 0 < w.2.1}) := by
    ext w
    change (0 ≤ w.1 ∧ w.1 ≤ 1) ∧ (0 < w.2.1 ∧ True) ↔ _
    simp only [mem_union, mem_ofPred_eq, and_true, domain, mem_prod, mem_Ioo, mem_Ioi, mem_univ]
    constructor
    · rintro ⟨⟨hlo, hhi⟩, h⟩
      rcases hlo.lt_or_eq with ha | ha
      · rcases hhi.lt_or_eq with hb | hb
        · exact Or.inl ⟨⟨ha, hb⟩, h⟩
        · exact Or.inr (Or.inr ⟨hb, h⟩)
      · exact Or.inr (Or.inl ⟨ha.symm, h⟩)
    · rintro (⟨⟨hl, hh⟩, h⟩ | ⟨hl, h⟩ | ⟨hh, h⟩)
      · exact ⟨⟨hl.le, hh.le⟩, h⟩
      · simp [hl, h]
      · simp [hh, h]
  by_cases h0 : r = 0
  · subst r
    change ContinuousWithinAt (completedObjective x) completedDomain (0, k, m)
    rw [hSplit, continuousWithinAt_union, continuousWithinAt_union]
    refine ⟨?_, ?_, ?_⟩
    · have H := left_chart_limit hx (v := (k, m)) hk
        (u := fun w ↦ w) (show Tendsto (fun w ↦ w) (𝓝[domain] (0, k, m))
          (𝓝 (0, k, m)) from nhdsWithin_le_nhds) self_mem_nhdsWithin
      change Tendsto _ _ (𝓝 (completedObjective x (0, k, m)))
      rw [completedObjective, ite_eq_left rfl]
      exact H.congr' ((show ∀ᶠ w in 𝓝[domain] ((0 : ℝ), k, m), w ∈ domain from
        self_mem_nhdsWithin).mono fun _ h ↦ (completed_interior x h).symm)
    · have hs : ContinuousAt (fun w : ℝ × ℝ × ℝ ↦ w.2) (0, k, m) :=
        continuous_snd.continuousAt
      have H : ContinuousAt (fun w : ℝ × ℝ × ℝ ↦ gammaValue x w.2) (0, k, m) :=
        (gamma_continuous x (v := (k, m)) hk).tendsto.comp hs.tendsto
      exact H.continuousWithinAt.congr
        (fun w hw ↦ by simp [completedObjective, hw.1]) (by simp [completedObjective])
    · apply continuousWithinAt_of_notMem_closure
      intro h
      have hc : IsClosed {w : ℝ × ℝ × ℝ | w.1 = 1} :=
        isClosed_eq continuous_fst continuous_const
      have h' := closure_minimal (show {w : ℝ × ℝ × ℝ | w.1 = 1 ∧ 0 < w.2.1} ⊆ _
        from fun _ hw ↦ hw.1) hc h
      norm_num at h'
  · by_cases h1 : r = 1
    · subst r
      change ContinuousWithinAt (completedObjective x) completedDomain (1, k, m)
      rw [hSplit, continuousWithinAt_union, continuousWithinAt_union]
      refine ⟨?_, ?_, ?_⟩
      · have H := right_chart_limit hx (v := (k, m)) hk
          (u := fun w ↦ w) (show Tendsto (fun w ↦ w) (𝓝[domain] (1, k, m))
            (𝓝 (1, k, m)) from nhdsWithin_le_nhds) self_mem_nhdsWithin
        change Tendsto _ _ (𝓝 (completedObjective x (1, k, m)))
        simp only [completedObjective, one_ne_zero, ite_false, ite_true]
        exact H.congr' ((show ∀ᶠ w in 𝓝[domain] ((1 : ℝ), k, m), w ∈ domain from
          self_mem_nhdsWithin).mono fun _ h ↦ (completed_interior x h).symm)
      · apply continuousWithinAt_of_notMem_closure
        intro h
        have hc : IsClosed {w : ℝ × ℝ × ℝ | w.1 = 0} :=
          isClosed_eq continuous_fst continuous_const
        have h' := closure_minimal (show {w : ℝ × ℝ × ℝ | w.1 = 0 ∧ 0 < w.2.1} ⊆ _
          from fun _ hw ↦ hw.1) hc h
        norm_num at h'
      · have H : ContinuousAt (fun w : ℝ × ℝ × ℝ ↦ inverseGammaValue x w.2) (1, k, m) := by
          unfold inverseGammaValue
          have ht : Continuous (fun w : ℝ × ℝ × ℝ ↦ (w.2.1, -w.2.2)) := by fun_prop
          have hs : ContinuousAt (fun w : ℝ × ℝ × ℝ ↦ (w.2.1, -w.2.2)) (1, k, m) :=
            ht.continuousAt
          have hh : ContinuousAt (fun w : ℝ × ℝ × ℝ ↦
              gammaValue (fun i ↦ (x i)⁻¹) (w.2.1, -w.2.2)) (1, k, m) :=
            (gamma_continuous _ (v := (k, -m)) hk).tendsto.comp hs.tendsto
          exact hh.sub continuousAt_const
        exact H.continuousWithinAt.congr (fun w hw ↦ by simp [completedObjective, hw.1])
          (by simp [completedObjective])
    · have hi : (r, k, m) ∈ domain :=
        ⟨⟨hr.1.lt_of_ne (Ne.symm h0), hr.2.lt_of_ne h1⟩, hk, mem_univ _⟩
      have hEq : objective x =ᶠ[𝓝 (r, k, m)] completedObjective x := by
        filter_upwards [hD.mem_nhds hi] with w hw
        exact (completed_interior x hw).symm
      exact ((objective_continuous hx hi).congr hEq).continuousWithinAt

/-! ## Compactness and exhaustion -/

private theorem domain_subset : domain ⊆ completedDomain :=
  fun _ h ↦ ⟨⟨h.1.1.le, h.1.2.le⟩, h.2⟩

private theorem completed_subset_closure : completedDomain ⊆ closure domain := by
  rw [domain, completedDomain, closure_prod_eq, closure_Ioo zero_ne_one]
  exact Set.prod_mono subset_rfl subset_closure

/-- All superlevels of the completed likelihood are compact on nonconstant
positive samples, including their intersections with the gamma faces. -/
theorem compact_superlevels (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (A : ℝ) :
    IsCompact {v ∈ completedDomain | A ≤ completedObjective x v} := by
  obtain ⟨lo, hi, R, hlo, hb⟩ := superlevel_envelope hn hx hne (A - 1)
  let K := Icc (0 : ℝ) 1 ×ˢ (Icc lo hi ×ˢ Icc (-R) R)
  have hKS : K ⊆ completedDomain := fun _ h ↦
    ⟨h.1, hlo.trans_le h.2.1.1, mem_univ _⟩
  have hK : IsCompact K := isCompact_Icc.prod (isCompact_Icc.prod isCompact_Icc)
  have hsub : {v ∈ completedDomain | A ≤ completedObjective x v} ⊆ K := by
    intro v hv
    have : (𝓝[domain] v).NeBot := mem_closure_iff_nhdsWithin_neBot.mp
      (completed_subset_closure hv.1)
    have ht := ((continuous_completed x hx) v hv.1).mono domain_subset
    have he : ∀ᶠ w in 𝓝[domain] v, w ∈ K := by
      filter_upwards [self_mem_nhdsWithin (s := domain), ht.eventually
        (Ioi_mem_nhds (show A - 1 < completedObjective x v from by linarith [hv.2]))] with w hw h
      have hv' : A - 1 < completedObjective x w := h
      rw [completed_interior x hw] at hv'
      exact ⟨⟨hw.1.1.le, hw.1.2.le⟩, hb w hw hv'.le⟩
    exact hK.isClosed.mem_of_tendsto (show Tendsto id (𝓝[domain] v) (𝓝 v) from
      nhdsWithin_le_nhds) he
  have he : {v ∈ completedDomain | A ≤ completedObjective x v} =
      K ∩ (completedObjective x) ⁻¹' Ici A := by
    ext v
    exact ⟨fun hv ↦ ⟨hsub hv, hv.2⟩, fun hv ↦ ⟨hKS hv.1, hv.2⟩⟩
  rw [he]
  exact hK.of_isClosed_subset (((continuous_completed x hx).mono hKS).preimage_isClosed_of_isClosed
    hK.isClosed isClosed_Ici) inter_subset_left

/-- Any sequence with likelihood bounded below has a subsequence converging
to an interior fit or one of the two proper faces. The likelihood converges
to the value at that point, with positive limiting harmonic concentration. -/
theorem competitive_subsequence (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (u : ℕ → ℝ × ℝ × ℝ) (hu : ∀ j, u j ∈ domain)
    (hbound : ∃ A, ∀ᶠ j in atTop, A ≤ objective x (u j)) :
    ∃ v ∈ completedDomain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
      Tendsto (u ∘ φ) atTop (𝓝 v) ∧
      Tendsto (fun j ↦ objective x (u (φ j))) atTop (𝓝 (completedObjective x v)) ∧
      (v ∈ domain ∨ (v.1 = 0 ∧ completedObjective x v = gammaValue x v.2) ∨
        (v.1 = 1 ∧ completedObjective x v = inverseGammaValue x v.2)) := by
  obtain ⟨A, hA⟩ := hbound
  obtain ⟨N, hN⟩ := eventually_atTop.1 hA
  let w := fun j : ℕ ↦ u (N + j)
  have hw (j : ℕ) : w j ∈ {v ∈ completedDomain | A ≤ completedObjective x v} :=
    ⟨domain_subset (hu _), (completed_interior x (hu _)).symm ▸ hN _ (Nat.le_add_right _ _)⟩
  obtain ⟨v, hv, ψ, hψ, ht⟩ := (compact_superlevels hn hx hne A).tendsto_subseq hw
  let φ := fun j ↦ N + ψ j
  have hcomp : ∀ᶠ j : ℕ in atTop, u (φ j) ∈ completedDomain :=
    .of_forall fun _ ↦ domain_subset (hu _)
  have ht' : Tendsto (u ∘ φ) atTop (𝓝[completedDomain] v) :=
    tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within _ ht hcomp
  have hl := ((continuous_completed x hx) v hv.1).tendsto.comp ht'
  refine ⟨v, hv.1, φ, fun a b hab ↦ Nat.add_lt_add_left (hψ hab) N, ht, ?_, ?_⟩
  · exact hl.congr (fun j ↦ completed_interior x (hu (φ j)))
  · by_cases h0 : v.1 = 0
    · exact Or.inr (Or.inl ⟨h0, by simp [completedObjective, h0]⟩)
    · by_cases h1 : v.1 = 1
      · exact Or.inr (Or.inr ⟨h1, by simp [completedObjective, h1]⟩)
      · exact Or.inl ⟨⟨hv.1.1.1.lt_of_ne (Ne.symm h0), hv.1.1.2.lt_of_ne h1⟩, hv.1.2⟩

private theorem slice_maximum (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {r : ℝ} (hr : r ∈ Icc 0 1) :
    ∃ v ∈ Ioi (0 : ℝ) ×ˢ (univ : Set ℝ),
      IsMaxOn (fun w ↦ completedObjective x (r, w)) (Ioi 0 ×ˢ univ) v := by
  let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ
  let F := fun w : ℝ × ℝ ↦ completedObjective x (r, w)
  have hC : ContinuousOn F D := (continuous_completed x hx).comp
    (f := fun w : ℝ × ℝ ↦ (r, w)) (by fun_prop) (fun _ hw ↦ ⟨hr, hw⟩)
  apply Generic.exists_isMaxOn_of_tendsto_cocompact
    (show D.Nonempty from ⟨(1, 0), zero_lt_one, mem_univ _⟩) hC
  apply tendsto_atBot.mpr
  intro A
  let S := {v ∈ completedDomain | A ≤ completedObjective x v} ∩ {v | v.1 = r}
  have hS : IsCompact S := (compact_superlevels hn hx hne A).inter_right
    (isClosed_eq continuous_fst continuous_const)
  let K := Prod.snd '' S
  have hK : IsCompact K := hS.image continuous_snd
  have hsub : K ⊆ range (Subtype.val : D → ℝ × ℝ) := by
    rintro w ⟨v, hv, rfl⟩
    exact ⟨⟨v.2, hv.1.1.2⟩, rfl⟩
  have hK' : IsCompact ((Subtype.val : D → ℝ × ℝ) ⁻¹' K) :=
    IsEmbedding.subtypeVal.isInducing.isCompact_preimage' hK hsub
  filter_upwards [hK'.compl_mem_cocompact] with w hw
  refine le_of_lt (lt_of_not_ge (fun h ↦ hw ?_))
  exact ⟨(r, w.1), ⟨⟨⟨hr, w.2⟩, h⟩, rfl⟩, rfl⟩

/-- The profile at each ratio attains its nuisance maximum on a common
compact set, including both endpoints. -/
theorem common_profile_compact (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Ioi 0 ×ˢ univ ∧
      ∀ r ∈ Icc (0 : ℝ) 1, ∃ v ∈ K,
        IsMaxOn (fun z ↦ completedObjective x (r, z)) (Ioi 0 ×ˢ univ) v := by
  let F := completedObjective x
  let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ
  have hcont := continuous_completed x hx
  have hF : ContinuousOn (fun r : ℝ ↦ F (r, 1, 0)) (Icc 0 1) :=
    hcont.comp (by fun_prop) (fun _ hr ↦ ⟨hr, zero_lt_one, mem_univ _⟩)
  obtain ⟨r₀, hr₀, hmin⟩ := (isCompact_Icc (a := (0 : ℝ)) (b := 1)).exists_isMinOn
    ⟨0, ⟨le_rfl, zero_le_one⟩⟩ hF
  let A := F (r₀, 1, 0)
  let S := {v ∈ completedDomain | A ≤ F v}
  have hS : IsCompact S := compact_superlevels hn hx hne A
  let K := Prod.snd '' S
  have hK : IsCompact K := hS.image continuous_snd
  have hsub : K ⊆ D := by
    rintro _ ⟨v, hv, rfl⟩
    exact hv.1.2
  refine ⟨K, hK, hsub, fun r hr ↦ ?_⟩
  obtain ⟨v, hv, hm⟩ := slice_maximum hn hx hne hr
  have hval : A ≤ F (r, v) := (hmin hr).trans (hm ⟨zero_lt_one, mem_univ _⟩)
  exact ⟨v, ⟨(r, v), ⟨⟨hr, hv⟩, hval⟩, rfl⟩, hm⟩

/-! ## Attainment and strict escape -/

/-- The shape-ratio profile with its two endpoint heights. -/
def ratioProfile (x : Fin n → ℝ) (r : ℝ) : ℝ :=
  sSup ((fun v : ℝ × ℝ ↦ completedObjective x (r, v)) '' (Ioi 0 ×ˢ univ))

private theorem profile_eq_max {x : Fin n → ℝ} {r : ℝ} {v : ℝ × ℝ}
    (hv : v ∈ Ioi (0 : ℝ) ×ˢ (univ : Set ℝ))
    (hm : IsMaxOn (fun w ↦ completedObjective x (r, w)) (Ioi 0 ×ˢ univ) v) :
    ratioProfile x r = completedObjective x (r, v) :=
  (show IsGreatest ((fun w : ℝ × ℝ ↦ completedObjective x (r, w)) '' (Ioi 0 ×ˢ univ))
      (completedObjective x (r, v)) from
    ⟨⟨v, hv, rfl⟩, by rintro _ ⟨w, hw, rfl⟩; exact hm hw⟩).csSup_eq

private theorem ratio_profile_height {x : Fin n → ℝ} {K : Set (ℝ × ℝ)}
    (hsub : K ⊆ Ioi 0 ×ˢ univ)
    (hm : ∀ r ∈ Icc (0 : ℝ) 1, ∃ v ∈ K,
      IsMaxOn (fun z ↦ completedObjective x (r, z)) (Ioi 0 ×ˢ univ) v)
    {r : ℝ} (hr : r ∈ Icc (0 : ℝ) 1) :
    (∃ v ∈ K, completedObjective x (r, v) = ratioProfile x r) ∧
      ∀ v ∈ Ioi 0 ×ˢ univ, completedObjective x (r, v) ≤ ratioProfile x r := by
  obtain ⟨v, hv, hmax⟩ := hm r hr
  have h := profile_eq_max (hsub hv) hmax
  exact ⟨⟨v, hv, h.symm⟩, fun w hw ↦ (hmax hw).trans_eq h.symm⟩

private theorem nonconstant_criterion (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    (∃ v ∈ domain, IsMaxOn (objective x) domain v) ↔
      ∃ r ∈ Ioo (0 : ℝ) 1, max (ratioProfile x 0) (ratioProfile x 1) ≤ ratioProfile x r := by
  let F := completedObjective x
  let P := ratioProfile x
  obtain ⟨K, hK, hsub, hmax⟩ := common_profile_compact hn hx hne
  let S := Icc (0 : ℝ) 1 ×ˢ K
  let D := Ioo (0 : ℝ) 1 ×ˢ K
  have hDK : D ⊆ S := Set.prod_mono Ioo_subset_Icc_self subset_rfl
  have hS : S ⊆ completedDomain := Set.prod_mono subset_rfl hsub
  have hD : D ⊆ domain := Set.prod_mono subset_rfl hsub
  have h0 : (0 : ℝ) ∈ Icc 0 1 := ⟨le_rfl, zero_le_one⟩
  have h1 : (1 : ℝ) ∈ Icc 0 1 := ⟨zero_le_one, le_rfl⟩
  have hheight {r : ℝ} (hr : r ∈ Icc 0 1) := ratio_profile_height hsub hmax hr
  constructor
  · rintro ⟨v, hv, hopt⟩
    have hle (w : ℝ × ℝ × ℝ) (hw : w ∈ completedDomain) : F w ≤ objective x v := by
      have : (𝓝[domain] w).NeBot := mem_closure_iff_nhdsWithin_neBot.mp
        (completed_subset_closure hw)
      have ht := ((continuous_completed x hx) w hw).mono domain_subset
      have hm : ∀ᶠ z in 𝓝[domain] w, F z ≤ objective x v := by
        filter_upwards [self_mem_nhdsWithin (s := domain)] with z hz
        exact (completed_interior x hz).le.trans (hopt hz)
      exact le_of_tendsto ht hm
    refine ⟨v.1, hv.1, max_le ?_ ?_⟩
    · obtain ⟨w, hw, he⟩ := (hheight h0).1
      exact he ▸ (hle (0, w) ⟨h0, hsub hw⟩).trans
        ((completed_interior x hv).ge.trans ((hheight (domain_subset hv).1).2 _ hv.2))
    · obtain ⟨w, hw, he⟩ := (hheight h1).1
      exact he ▸ (hle (1, w) ⟨h1, hsub hw⟩).trans
        ((completed_interior x hv).ge.trans ((hheight (domain_subset hv).1).2 _ hv.2))
  · rintro ⟨r, hr, hW⟩
    obtain ⟨v, hv, he⟩ := (hheight (Ioo_subset_Icc_self hr)).1
    have hL := (continuous_completed x hx).mono hS
    have hboundary (w : ℝ × ℝ × ℝ) (hw : w ∈ S \ D) : F w ≤ F (r, v) := by
      have hrw : w.1 = 0 ∨ w.1 = 1 := by
        by_contra! h
        exact hw.2 ⟨⟨hw.1.1.1.lt_of_ne (Ne.symm h.1), hw.1.1.2.lt_of_ne h.2⟩, hw.1.2⟩
      have htop : P w.1 ≤ max (P 0) (P 1) := hrw.elim
        (fun h ↦ h ▸ le_max_left _ _) (fun h ↦ h ▸ le_max_right _ _)
      exact ((hheight hw.1.1).2 _ (hsub hw.1.2)).trans (htop.trans (hW.trans_eq he.symm))
    obtain ⟨z, hz, hm⟩ := Generic.exists_isMaxOn_of_boundary_le hDK
      (isCompact_Icc.prod hK) hL (show (r, v) ∈ D from ⟨hr, hv⟩) hboundary
    refine ⟨z, hD hz, fun w hw ↦ ?_⟩
    obtain ⟨a, ha, hea⟩ := (hheight (domain_subset hw).1).1
    have hwa : (w.1, a) ∈ D := ⟨hw.1, ha⟩
    exact (completed_interior x hw).ge.trans
      (((hheight (domain_subset hw).1).2 _ hw.2).trans
        (hea.ge.trans ((hm hwa).trans (completed_interior x (hD hz)).le)))

private theorem chart_surjective {p q b : ℝ} (hp : 0 < p) (hq : 0 < q) (hb : 0 < b) :
    ∃ w ∈ domain, w.2.1 / (1 - w.1) = p ∧ w.2.1 / w.1 = q ∧
      exp w.2.2 * (1 - w.1) / w.1 = b := by
  let r := p / (p + q)
  have hsum : 0 < p + q := add_pos hp hq
  have hr : r ∈ Ioo (0 : ℝ) 1 :=
    ⟨div_pos hp hsum, (div_lt_one hsum).mpr (lt_add_of_pos_right _ hq)⟩
  have he : 1 - r = q / (p + q) := by
    dsimp [r]
    field_simp [hsum.ne']
    ring
  refine ⟨(r, concentration p q, log (b * p / q)),
    ⟨hr, div_pos (mul_pos hp hq) hsum, mem_univ _⟩, ?_⟩
  change concentration p q / (1 - r) = p ∧ concentration p q / r = q ∧
    exp (log (b * p / q)) * (1 - r) / r = b
  rw [he, exp_log (div_pos (mul_pos hb hp) hq)]
  dsimp [concentration, r]
  refine ⟨?_, ?_, ?_⟩ <;> field_simp [hp.ne', hq.ne', hsum.ne']

private theorem constant_unbounded_chart (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hconst : ∀ i j, x i = x j) :
    ¬ BddAbove (objective x '' domain) := by
  intro h
  apply AllFreeBetaPrime.constant_unbounded hn hx hconst
  apply h.mono
  rintro _ ⟨b, hb, ⟨p, q⟩, hsh, rfl⟩
  obtain ⟨w, hw, hp, hq, hb'⟩ := chart_surjective hsh.1 hsh.2 hb
  exact ⟨w, hw, by simp [objective, hp, hq, hb']⟩

/-- Constant data have no finite maximum. Otherwise finite attainment
is equivalent to an interior ratio reaching the larger optimized face height. -/
theorem all_free_criterion (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃ v ∈ domain, IsMaxOn (objective x) domain v) ↔
      (∃ i j, x i ≠ x j) ∧
        ∃ r ∈ Ioo (0 : ℝ) 1, max (ratioProfile x 0) (ratioProfile x 1) ≤ ratioProfile x r := by
  by_cases h : ∃ i j, x i ≠ x j
  · simpa only [h, true_and] using nonconstant_criterion hn hx h
  · push Not at h
    constructor
    · rintro ⟨v, _, hm⟩
      exfalso
      exact constant_unbounded_chart hn hx h ⟨objective x v, by
        rintro _ ⟨w, hw, rfl⟩
        exact hm hw⟩
    · rintro ⟨⟨i, j, hij⟩, _⟩
      exact (hij (h i j)).elim

/-- When every interior height is strictly below the larger face, that
reachable face is the unattained supremum. -/
theorem strictly_winning_face (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    (hlose : ∀ r ∈ Ioo (0 : ℝ) 1, ratioProfile x r < max (ratioProfile x 0) (ratioProfile x 1)) :
    sSup (objective x '' domain) = max (ratioProfile x 0) (ratioProfile x 1) ∧
      ¬ ∃ v ∈ domain, IsMaxOn (objective x) domain v := by
  obtain ⟨K, _, hsub, hmax⟩ := common_profile_compact hn hx hne
  let P := ratioProfile x
  let r := if P 1 ≤ P 0 then (0 : ℝ) else 1
  have hr : r ∈ Icc 0 1 := by
    dsimp [r]
    split <;> constructor <;> norm_num
  have hrW : P r = max (P 0) (P 1) := by
    dsimp [r]
    split <;> rename_i h
    · exact (max_eq_left h).symm
    · exact (max_eq_right (le_of_not_ge h)).symm
  obtain ⟨v, hv, he⟩ := (ratio_profile_height hsub hmax hr).1
  have hfull : (r, v) ∈ completedDomain := ⟨hr, hsub hv⟩
  have he' : completedObjective x (r, v) = max (P 0) (P 1) := he.trans hrW
  have hstrict (w : ℝ × ℝ × ℝ) (hw : w ∈ domain) :
      completedObjective x w < completedObjective x (r, v) := by
    rw [he']
    exact ((ratio_profile_height hsub hmax (domain_subset hw).1).2 _ hw.2).trans_lt
      (hlose w.1 hw.1)
  obtain ⟨hs, hm⟩ := Generic.sSup_eq_and_not_exists_isMaxOn_of_boundary_gt domain_subset
    (continuous_completed x hx) hfull (completed_subset_closure hfull) hstrict
  have hEq : completedObjective x '' domain = objective x '' domain :=
    Set.image_congr (fun _ hw ↦ completed_interior x hw)
  rw [hEq, he'] at hs
  exact ⟨hs, fun ⟨w, hw, hwmax⟩ ↦ hm ⟨w, hw, fun z hz ↦ by
    change completedObjective x z ≤ completedObjective x w
    rw [completed_interior x hz, completed_interior x hw]
    exact hwmax hz⟩⟩

/-- The optimized likelihood is continuous in the completed ratio,
including the gamma and inverse-gamma endpoint values. -/
theorem continuous_ratio_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) : ContinuousOn (ratioProfile x) (Icc 0 1) := by
  obtain ⟨K, hK, hsub, hm⟩ := common_profile_compact hn hx hne
  let S := Icc (0 : ℝ) 1 ×ˢ K
  have hS : IsCompact S := isCompact_Icc.prod hK
  have hcont := (continuous_completed x hx).mono
    (Set.prod_mono subset_rfl hsub : S ⊆ completedDomain)
  rw [Metric.continuousOn_iff]
  intro s hs ε hε
  obtain ⟨δ, hδ, hU⟩ := Metric.uniformContinuousOn_iff.mp
    (hS.uniformContinuousOn_of_continuous hcont) ε hε
  refine ⟨δ, hδ, fun r hr hdist ↦ ?_⟩
  obtain ⟨⟨v, hv, he⟩, hrmax⟩ := ratio_profile_height hsub hm hr
  obtain ⟨⟨w, hw, hw'⟩, hsmax⟩ := ratio_profile_height hsub hm hs
  have h (a : ℝ × ℝ) (ha : a ∈ K) :
      |completedObjective x (r, a) - completedObjective x (s, a)| < ε := by
    have H := hU (r, a) (show (r, a) ∈ S from ⟨hr, ha⟩)
      (s, a) (show (s, a) ∈ S from ⟨hs, ha⟩) (by
        simpa only [Prod.dist_eq, Prod.fst, Prod.snd, dist_self, max_eq_left dist_nonneg]
          using hdist)
    rwa [Real.dist_eq] at H
  rw [Real.dist_eq, abs_lt]
  have hu := hsmax v (hsub hv)
  have hl := hrmax w (hsub hw)
  have h₁ := (abs_lt.mp (h v hv)).2
  have h₂ := (abs_lt.mp (h w hw)).1
  constructor <;> linarith only [he, hw', hu, hl, h₁, h₂]

private theorem no_interior_of_cocompact (u : ℕ → ℝ × ℝ × ℝ) (hu : ∀ j, u j ∈ domain)
    (he : Tendsto (fun j ↦ (⟨u j, hu j⟩ : domain)) atTop (cocompact domain)) :
    ¬ ∃ v ∈ domain, ∃ φ : ℕ → ℕ, StrictMono φ ∧ Tendsto (u ∘ φ) atTop (𝓝 v) := by
  rintro ⟨v, hv, φ, hφ, ht⟩
  let a : domain := ⟨v, hv⟩
  have hs : Tendsto (fun j ↦ (⟨u (φ j), hu (φ j)⟩ : domain)) atTop (𝓝 a) :=
    tendsto_subtype_rng.mpr ht
  have hopen : IsOpen domain := isOpen_Ioo.prod (isOpen_Ioi.prod isOpen_univ)
  let : LocallyCompactSpace domain := hopen.locallyCompactSpace
  obtain ⟨K, hK, hKn⟩ := exists_compact_mem_nhds a
  have H := (he.comp hφ.tendsto_atTop).eventually hK.compl_mem_cocompact
  obtain ⟨j, hj, hj'⟩ := ((hs.eventually hKn).and H).exists
  exact hj' hj

/-- The all-free failure split. Constants diverge. For nonconstant data,
any competitive sequence escaping every compact subset of the feasible
chart has a subsequence converging to one of the two gamma faces. -/
theorem all_free_failure (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    ((∀ i j, x i = x j) → ¬ BddAbove (objective x '' domain)) ∧
    ((∃ i j, x i ≠ x j) → ∀ (u : ℕ → ℝ × ℝ × ℝ) (hu : ∀ j, u j ∈ domain),
      (∃ A, ∀ᶠ j in atTop, A ≤ objective x (u j)) →
      Tendsto (fun j ↦ (⟨u j, hu j⟩ : domain)) atTop (cocompact domain) →
      ∃ v ∈ completedDomain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
        Tendsto (u ∘ φ) atTop (𝓝 v) ∧
        ((v.1 = 0 ∧ Tendsto (fun j ↦ objective x (u (φ j))) atTop (𝓝 (gammaValue x v.2))) ∨
         (v.1 = 1 ∧ Tendsto (fun j ↦ objective x (u (φ j)))
          atTop (𝓝 (inverseGammaValue x v.2))))) := by
  refine ⟨constant_unbounded_chart hn hx, fun hne u hu hb h ↦ ?_⟩
  obtain ⟨v, hv, φ, hφ, ht, hl, hclass⟩ := competitive_subsequence hn hx hne u hu hb
  refine ⟨v, hv, φ, hφ, ht, ?_⟩
  rcases hclass with hv' | ⟨h0, he⟩ | ⟨h1, he⟩
  · exact (no_interior_of_cocompact u hu h ⟨v, hv', φ, hφ, ht⟩).elim
  · exact Or.inl ⟨h0, he ▸ hl⟩
  · exact Or.inr ⟨h1, he ▸ hl⟩

end

end UnivariateML.BPJoint

end
