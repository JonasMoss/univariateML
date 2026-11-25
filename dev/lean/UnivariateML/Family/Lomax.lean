module

public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
public import Mathlib.Topology.Order.Compact
public import Mathlib.Topology.Order.IntermediateValue

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Lomax likelihood

The coefficients are shape κ and rate λ. The all-free existence criterion
includes equality with the exponential likelihood.

## Deviations

The statements concern exact real arithmetic and strictly positive samples.
The pointwise limit uses the continuous extension of the log-density at zero.
The existence proof uses a finite cutoff beyond which the profile is strictly
below the exponential value. This cutoff suffices for the compactness argument.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Lomax

noncomputable section

variable {n : ℕ}

def logDensity (κ rate x : ℝ) : ℝ :=
  log κ + log rate - (κ + 1) * log (1 + rate * x)

def expLogDensity (r x : ℝ) : ℝ := log r - r * x

def logLik (x : Fin n → ℝ) (κ rate : ℝ) : ℝ := ∑ i, logDensity κ rate (x i)

def expLogLik (x : Fin n → ℝ) (r : ℝ) : ℝ := ∑ i, expLogDensity r (x i)

def avg (x : Fin n → ℝ) : ℝ := (∑ i, x i) / n

def y (x : Fin n → ℝ) (i : Fin n) : ℝ := x i / avg x

def A (x : Fin n → ℝ) (q : ℝ) : ℝ := avg (fun i ↦ log (1 + q * y x i))

def shape (x : Fin n → ℝ) (rate : ℝ) : ℝ := (avg (fun i ↦ log (1 + rate * x i)))⁻¹

def bestExp (x : Fin n → ℝ) : ℝ := expLogLik x (avg x)⁻¹

def gap (x : Fin n → ℝ) (q : ℝ) : ℝ := log q - log (A x q) - A x q

def psi (x : Fin n → ℝ) (u : ℝ) : ℝ :=
  avg (fun i ↦ log (exp (-u) + u * y x i))

def uBar (x : Fin n → ℝ) : ℝ := avg x / (∏ i, x i) ^ ((n : ℝ)⁻¹)

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

private theorem hasDerivAt_logLinear (c t : ℝ) (h : 1 + t * c ≠ 0) :
    HasDerivAt (fun s : ℝ ↦ log (1 + s * c)) (c / (1 + t * c)) t := by
  simpa using (((hasDerivAt_id t).mul_const c).const_add 1).log h

private theorem tendsto_log_ratio :
    Tendsto (fun t : ℝ ↦ log (1 + t) / t) (𝓝[≠] 0) (𝓝 1) := by
  have h := (hasDerivAt_logLinear 1 0 (by norm_num)).tendsto_slope_zero
  simpa [smul_eq_mul, div_eq_mul_inv, mul_comm] using h

/-- The exponential limit along any path with diverging shape and convergent product. -/
theorem logDensity_tendsto_of_mul {ι : Type*} {l : Filter ι} {κ rate : ι → ℝ}
    {r x : ℝ} (hr : 0 < r) (hx : 0 ≤ x) (hk : Tendsto κ l atTop)
    (hp : Tendsto (fun i ↦ κ i * rate i) l (𝓝 r)) :
    Tendsto (fun i ↦ logDensity (κ i) (rate i) x) l (𝓝 (expLogDensity r x)) := by
  have hκ : ∀ᶠ i in l, 0 < κ i := hk.eventually (eventually_gt_atTop 0)
  have hprod : ∀ᶠ i in l, 0 < κ i * rate i := hp.eventually (Ioi_mem_nhds hr)
  have hrate : ∀ᶠ i in l, 0 < rate i := by
    filter_upwards [hκ, hprod] with i hi hip
    exact pos_of_mul_pos_right hip hi.le
  have hzero : Tendsto rate l (𝓝 0) := by
    have H := hp.mul (tendsto_inv_atTop_zero.comp hk)
    rw [mul_zero] at H
    refine H.congr' ?_
    filter_upwards [hκ] with i hi
    simp [mul_comm (κ i), hi.ne']
  have hlog : Tendsto (fun i ↦ log (κ i * rate i)) l (𝓝 (log r)) :=
    (continuousAt_log hr.ne').tendsto.comp hp
  by_cases he : x = 0
  · subst x
    rw [show expLogDensity r 0 = log r by simp [expLogDensity]]
    refine hlog.congr' ?_
    filter_upwards [hκ, hrate] with i hi hil
    simp [logDensity, log_mul hi.ne' hil.ne']
  have hx' : 0 < x := lt_of_le_of_ne hx (Ne.symm he)
  have ht : Tendsto (fun i ↦ rate i * x) l (𝓝 0) := by simpa using hzero.mul_const x
  have ht' : Tendsto (fun i ↦ rate i * x) l (𝓝[≠] 0) :=
    tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within _ ht <| by
      filter_upwards [hrate] with i hi
      simpa using (mul_pos hi hx').ne'
  have hfactor : Tendsto (fun i ↦ (κ i * rate i + rate i) * x) l (𝓝 (r * x)) := by
    simpa using (hp.add hzero).mul_const x
  have H := hlog.sub (hfactor.mul (tendsto_log_ratio.comp ht'))
  rw [mul_one] at H
  refine H.congr' ?_
  filter_upwards [hκ, hrate] with i hi hil
  dsimp [logDensity, expLogDensity]
  rw [log_mul hi.ne' hil.ne']
  field_simp [hil.ne', hx'.ne']

/-- Along the fixed-product path κ · rate = r, the log-density converges pointwise. -/
theorem logDensity_tendsto {r x : ℝ} (hr : 0 < r) (hx : 0 ≤ x) :
    Tendsto (fun κ : ℝ ↦ logDensity κ (r / κ) x) atTop (𝓝 (expLogDensity r x)) := by
  apply logDensity_tendsto_of_mul hr hx tendsto_id
  apply tendsto_const_nhds.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with κ hk
  dsimp
  field_simp [hk.ne']

/-- The fixed-product limit also holds for the sample log-likelihood. -/
theorem logLik_tendsto (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) {r : ℝ} (hr : 0 < r) :
    Tendsto (fun κ : ℝ ↦ logLik x κ (r / κ)) atTop (𝓝 (expLogLik x r)) := by
  exact tendsto_finsetSum _ (fun i _ ↦ logDensity_tendsto hr (hx i))

private theorem avg_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    0 < avg x := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact div_pos (Finset.sum_pos (fun i _ ↦ hx i) Finset.univ_nonempty) (by exact_mod_cast hn)

private theorem y_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (i : Fin n) :
    0 < y x i := div_pos (hx i) (avg_pos hn hx)

private theorem avg_log_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {rate : ℝ} (hrate : 0 < rate) : 0 < avg (fun i ↦ log (1 + rate * x i)) :=
  avg_pos hn (fun i ↦ log_pos (by linarith [mul_pos hrate (hx i)]))

private theorem normalized (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (rate : ℝ) :
    A x (rate * avg x) = avg (fun i ↦ log (1 + rate * x i)) := by
  unfold A y
  congr 1
  funext i
  rw [show rate * avg x * (x i / avg x) = rate * x i by
    field_simp [(avg_pos hn hx).ne']]

private theorem logLik_formula (x : Fin n → ℝ) (κ rate : ℝ) :
    logLik x κ rate = (n : ℝ) * (log κ + log rate) -
      (κ + 1) * ∑ i, log (1 + rate * x i) := by
  simp [logLik, logDensity, Finset.sum_sub_distrib, Finset.mul_sum, mul_add]

private theorem expLogLik_formula (x : Fin n → ℝ) (r : ℝ) :
    expLogLik x r = (n : ℝ) * log r - r * ∑ i, x i := by
  simp [expLogLik, expLogDensity, Finset.sum_sub_distrib, Finset.mul_sum]

private theorem log_sub_strict {s : ℝ} (hs : 0 < s) {t : ℝ} (ht : 0 < t)
    (hne : t ≠ s⁻¹) :
    log t - t * s < log s⁻¹ - s⁻¹ * s := by
  have h : t * s ≠ 1 := by
    intro h
    apply hne
    rw [← one_div, eq_div_iff hs.ne', h]
  have hlt := log_lt_sub_one_of_pos (mul_pos ht hs) h
  rw [log_mul ht.ne' hs.ne'] at hlt
  rw [log_inv, inv_mul_cancel₀ hs.ne']
  linarith

/-- The reciprocal mean maximizes the exponential likelihood over positive rates. -/
theorem bestExp_isMaxOn (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    IsMaxOn (expLogLik x) (Ioi 0) (avg x)⁻¹ := by
  have hm := avg_pos hn hx
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hs : (∑ i, x i) = n * avg x := by unfold avg; field_simp
  intro r hr
  change expLogLik x r ≤ expLogLik x (avg x)⁻¹
  by_cases h : r = (avg x)⁻¹
  · simp [h]
  · rw [expLogLik_formula, expLogLik_formula, hs]
    have H := mul_lt_mul_of_pos_left (log_sub_strict hm hr h)
      (show (0 : ℝ) < n by exact_mod_cast hn)
    linarith only [H]

private theorem bestExp_formula (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    bestExp x = n * (-log (avg x) - 1) := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hs : (∑ i, x i) = n * avg x := by unfold avg; field_simp
  rw [bestExp, expLogLik_formula, hs, log_inv]
  field_simp [(avg_pos hn hx).ne']

/-- The Lomax log-likelihood is strictly concave in positive shape. -/
theorem shape_strictConcave (hn : 0 < n) (x : Fin n → ℝ) (rate : ℝ) :
    StrictConcaveOn ℝ (Ioi (0 : ℝ)) (fun κ ↦ logLik x κ rate) := by
  refine ⟨convex_Ioi _, ?_⟩
  intro a ha b hb hab s t hs ht hst
  simp only [smul_eq_mul] at *
  simp only [logLik_formula]
  have H := strictConcaveOn_log_Ioi.2 ha hb hab hs ht hst
  simp only [smul_eq_mul] at H
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have E : s * (log a + log rate) + t * (log b + log rate) =
      s * log a + t * log b + log rate := by linear_combination log rate * hst
  have F : s * (a + 1) + t * (b + 1) = s * a + t * b + 1 := by linarith
  calc
    _ = n * (s * log a + t * log b + log rate) -
        (s * a + t * b + 1) * ∑ i, log (1 + rate * x i) := by
          rw [← E, ← F]
          ring
    _ < _ := by gcongr

/-- The explicit shape profile is a strict maximum over other positive shapes. -/
theorem shape_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {rate : ℝ} (hrate : 0 < rate) :
    0 < shape x rate ∧
      ∀ κ > 0, κ ≠ shape x rate → logLik x κ rate < logLik x (shape x rate) rate := by
  let s := avg (fun i ↦ log (1 + rate * x i))
  have hs : 0 < s := avg_log_pos hn hx hrate
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hsum : (∑ i, log (1 + rate * x i)) = n * s := by
    dsimp [s, avg]
    field_simp [hN.ne']
  refine ⟨inv_pos.mpr hs, ?_⟩
  intro κ hκ hne
  rw [logLik_formula, logLik_formula, hsum]
  have H := mul_lt_mul_of_pos_left (log_sub_strict hs hκ hne) hN
  change _ < n * (log s⁻¹ + log rate) - (s⁻¹ + 1) * (n * s)
  linarith only [H]

/-- The total profile improvement over the best exponential is the sample size times the gap. -/
theorem profile_gap (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {rate : ℝ} (hrate : 0 < rate) :
    logLik x (shape x rate) rate - bestExp x = n * gap x (rate * avg x) := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  rw [logLik_formula, bestExp_formula hn hx, gap, normalized hn hx, shape,
    log_inv, log_mul hrate.ne' (avg_pos hn hx).ne']
  generalize hs : avg (fun i ↦ log (1 + rate * x i)) = s
  have hspos : 0 < s := hs ▸ avg_log_pos hn hx hrate
  have hsum : (∑ i, log (1 + rate * x i)) = n * s := by
    rw [← hs, avg]
    field_simp
  rw [hsum]
  field_simp [hspos.ne']
  ring

private theorem bestExp_le_upperBound (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : b ∈ upperBounds ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain)) :
    bestExp x ≤ b := by
  have hr : 0 < (avg x)⁻¹ := inv_pos.mpr (avg_pos hn hx)
  refine le_of_tendsto (logLik_tendsto x (fun i ↦ (hx i).le) hr) ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with κ hκ
  exact hb ⟨(κ, (avg x)⁻¹ / κ), ⟨hκ, div_pos hr hκ⟩, rfl⟩

private theorem A_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {q : ℝ} (hq : 0 < q) : 0 < A x q :=
  avg_log_pos hn (y_pos hn hx) hq

private theorem avg_y (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) : avg (y x) = 1 := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hs : (∑ i, x i) = n * avg x := by unfold avg; field_simp
  simp only [avg, y, ← Finset.sum_div]
  rw [hs]
  field_simp [(avg_pos hn hx).ne', hN]

private theorem hasDerivAt_A_zero (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    HasDerivAt (A x) 1 0 := by
  have H (i : Fin n) : HasDerivAt (fun q ↦ log (1 + q * y x i)) (y x i) 0 := by
    simpa using hasDerivAt_logLinear (y x i) 0 (by norm_num)
  rw [← avg_y hn hx]
  unfold A avg
  exact (HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ H i)).div_const (n : ℝ)

private theorem gap_continuousAt_zero (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    ContinuousAt (gap x) 0 := by
  have hA := hasDerivAt_A_zero hn hx
  have h0 : A x 0 = 0 := by simp [A, avg]
  -- Cancel the singular logarithms by taking their ratio before passing to the limit.
  have hratio : Tendsto (fun q ↦ A x q / q) (𝓝[≠] 0) (𝓝 1) := by
    simpa [h0, smul_eq_mul, div_eq_mul_inv, mul_comm] using hA.tendsto_slope_zero
  have hlog := (continuousAt_log one_ne_zero).tendsto.comp hratio
  have H := hlog.neg.sub (hA.continuousAt.continuousWithinAt.tendsto)
  rw [log_one, neg_zero, h0, sub_zero] at H
  rw [continuousAt_iff_punctured_nhds, show gap x 0 = 0 by simp [gap, h0]]
  refine H.congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := ({0}ᶜ : Set ℝ)) (a := (0 : ℝ)),
    hratio.eventually (Ioi_mem_nhds (zero_lt_one : (0 : ℝ) < 1))] with q hq hpos
  change q ≠ 0 at hq
  change 0 < A x q / q at hpos
  have ha : A x q ≠ 0 := by
    intro h
    simp [h] at hpos
  simp [gap, log_div ha hq]

private theorem gap_continuousOn (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    ContinuousOn (gap x) (Ici 0) := by
  intro q hq
  have hq' : 0 ≤ q := hq
  rcases hq'.eq_or_lt with h | h
  · subst q
    exact (gap_continuousAt_zero hn hx).continuousWithinAt
  have hA : ContinuousAt (A x) q := by
    have hs : Tendsto (fun t : ℝ ↦ ∑ i, log (1 + t * y x i)) (𝓝 q)
        (𝓝 (∑ i, log (1 + q * y x i))) := by
      refine tendsto_finsetSum Finset.univ (fun i _ ↦ ?_)
      have hp : 0 < 1 + q * y x i := by linarith [mul_pos h (y_pos hn hx i)]
      exact (hasDerivAt_logLinear (y x i) q hp.ne').continuousAt.tendsto
    exact hs.div_const (n : ℝ)
  exact (((continuousAt_log h.ne').sub ((continuousAt_log (A_pos hn hx h).ne').comp hA)).sub
    hA).continuousWithinAt

private def qOf (u : ℝ) : ℝ := u * exp u

private theorem qOf_pos {u : ℝ} (hu : 0 < u) : 0 < qOf u := mul_pos hu (exp_pos _)

private theorem exists_qOf {q : ℝ} (hq : 0 < q) : ∃ u > 0, qOf u = q := by
  have hc : Continuous qOf := continuous_id.mul continuous_exp
  have htop : q ≤ qOf q := by
    have h : 1 ≤ exp q := by simpa only [exp_zero] using exp_le_exp.mpr hq.le
    change q ≤ q * exp q
    simpa only [mul_one] using mul_le_mul_of_nonneg_left h hq.le
  have hzero : qOf 0 = 0 := by simp [qOf]
  obtain ⟨u, hu, he⟩ := intermediate_value_Icc (f := qOf) (a := 0) (b := q) hq.le
    hc.continuousOn (show q ∈ Icc (qOf 0) (qOf q) by rw [hzero]; exact ⟨hq.le, htop⟩)
  have hne : u ≠ 0 := by
    intro h
    rw [h, hzero] at he
    exact hq.ne' he.symm
  exact ⟨u, lt_of_le_of_ne hu.1 hne.symm, he⟩

private theorem A_qOf (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {u : ℝ} (hu : 0 < u) :
    A x (qOf u) = u + psi x u := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hterm (i : Fin n) : log (1 + qOf u * y x i) =
      u + log (exp (-u) + u * y x i) := by
    have hpos : 0 < exp (-u) + u * y x i := add_pos (exp_pos _) (mul_pos hu (y_pos hn hx i))
    calc
      log (1 + qOf u * y x i) = log (exp u * (exp (-u) + u * y x i)) := by
        congr 1
        rw [mul_add, ← exp_add, add_neg_cancel, exp_zero, qOf]
        ring
      _ = u + log (exp (-u) + u * y x i) := by
        rw [log_mul (exp_pos u).ne' hpos.ne', log_exp]
  simp only [A, psi, avg, hterm, Finset.sum_add_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  field_simp

private theorem gap_nonneg_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {u : ℝ} (hu : 0 < u) :
    0 ≤ gap x (qOf u) ↔ psi x u ≤ 0 := by
  have ha := A_pos hn hx (qOf_pos hu)
  have hlog : log (qOf u) = log u + u := by
    simp [qOf, log_mul hu.ne' (exp_pos u).ne']
  have hA := A_qOf hn hx hu
  rw [gap, hlog]
  constructor
  · intro h
    by_contra! hp
    have hlt : u < A x (qOf u) := by linarith
    have hl := (log_lt_log_iff hu ha).mpr hlt
    linarith
  · intro h
    have hle : A x (qOf u) ≤ u := by linarith
    have hl := (log_le_log_iff ha hu).mpr hle
    linarith

private theorem uBar_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    0 < uBar x :=
  div_pos (avg_pos hn hx) (rpow_pos_of_pos (Finset.prod_pos fun i _ ↦ hx i) _)

private theorem psi_gt_log (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {u : ℝ} (hu : 0 < u) : log (u / uBar x) < psi x u := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hm := avg_pos hn hx
  have hp : 0 < ∏ i, x i := Finset.prod_pos fun i _ ↦ hx i
  have hg := rpow_pos_of_pos hp ((n : ℝ)⁻¹)
  have hterm (i : Fin n) : log (u * y x i) = (log u - log (avg x)) + log (x i) := by
    rw [y, log_mul hu.ne' (div_pos (hx i) hm).ne', log_div (hx i).ne' hm.ne']
    ring
  have hsum :
      (∑ i, log (u * y x i)) = n * (log u - log (avg x)) + ∑ i, log (x i) := by
    simp [hterm, Finset.sum_add_distrib, mul_sub]
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  have H : (∑ i, log (u * y x i)) < ∑ i, log (exp (-u) + u * y x i) := by
    apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
    intro i _
    exact strictMonoOn_log (mul_pos hu (y_pos hn hx i))
      (add_pos (exp_pos _) (mul_pos hu (y_pos hn hx i))) (lt_add_of_pos_left _ (exp_pos _))
  unfold psi avg
  rw [← div_lt_div_iff_of_pos_right hN] at H
  convert H using 1
  rw [hsum, uBar, log_div hu.ne' (div_pos hm hg).ne', log_div hm.ne' hg.ne',
    log_rpow hp, log_prod (fun i _ ↦ (hx i).ne')]
  field_simp
  ring

private theorem gap_tail_neg (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {u : ℝ} (hu : uBar x ≤ u) : gap x (qOf u) < 0 := by
  have hp := uBar_pos hn hx
  have hu' := hp.trans_le hu
  have hl := log_nonneg (one_le_div₀ hp |>.mpr hu)
  exact lt_of_not_ge fun h ↦ (not_le_of_gt (hl.trans_lt (psi_gt_log hn hx hu')))
    ((gap_nonneg_iff hn hx hu').mp h)

private theorem profile_bound (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {κ rate : ℝ} (hk : 0 < κ) (hr : 0 < rate) :
    logLik x κ rate ≤ bestExp x + n * gap x (rate * avg x) := by
  have he := profile_gap hn hx hr
  by_cases h : κ = shape x rate
  · rw [h]
    linarith only [he]
  · have H := (shape_profile hn hx hr).2 κ hk h
    linarith only [H, he]

private theorem compact_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    ∃ u ∈ Icc 0 (uBar x), IsMaxOn (fun v ↦ gap x (qOf v)) (Icc 0 (uBar x)) u := by
  have hc : Continuous qOf := continuous_id.mul continuous_exp
  apply isCompact_Icc.exists_isMaxOn ⟨0, le_rfl, (uBar_pos hn hx).le⟩
  exact (gap_continuousOn hn hx).comp hc.continuousOn
    (fun v hv ↦ mul_nonneg hv.1 (exp_pos _).le)

private theorem compact_profile_bound (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {u : ℝ} (hmax : IsMaxOn (fun v ↦ gap x (qOf v)) (Icc 0 (uBar x)) u) :
    ∀ q > 0, gap x q ≤ gap x (qOf u) := by
  have h0 : 0 ≤ gap x (qOf u) := by
    have H := hmax (show (0 : ℝ) ∈ Icc 0 (uBar x) from ⟨le_rfl, (uBar_pos hn hx).le⟩)
    change gap x (qOf 0) ≤ gap x (qOf u) at H
    simpa [gap, qOf, A, avg] using H
  intro q hq
  obtain ⟨v, hv, rfl⟩ := exists_qOf hq
  by_cases hv' : v ≤ uBar x
  · exact hmax (show v ∈ Icc 0 (uBar x) from ⟨hv.le, hv'⟩)
  · exact (gap_tail_neg hn hx (le_of_not_ge hv')).le.trans h0

private theorem likelihood_bddAbove (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    BddAbove ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain) := by
  obtain ⟨u, _, hu⟩ := compact_profile hn hx
  refine ⟨bestExp x + n * gap x (qOf u), ?_⟩
  rintro _ ⟨⟨κ, rate⟩, ⟨hk, hr⟩, rfl⟩
  have hN : (0 : ℝ) ≤ n := Nat.cast_nonneg n
  have h := compact_profile_bound hn hx hu _ (mul_pos hr (avg_pos hn hx))
  exact (profile_bound hn hx hk hr).trans
    (add_le_add_right (mul_le_mul_of_nonneg_left h hN) (bestExp x))

private theorem likelihood_nonempty (x : Fin n → ℝ) :
    ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain).Nonempty :=
  ⟨_, (1, 1), ⟨mem_Ioi.mpr zero_lt_one, mem_Ioi.mpr zero_lt_one⟩, rfl⟩

/-- The best exponential likelihood is a lower bound for the Lomax supremum. -/
theorem bestExp_le_sup (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    bestExp x ≤ sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain) :=
  bestExp_le_upperBound hn hx (fun _ h ↦ le_csSup (likelihood_bddAbove hn hx) h)

/-- A finite Lomax maximizer exists exactly when the bounded sign test has a witness. -/
theorem exists_max_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃ p ∈ domain, IsMaxOn (fun p : ℝ × ℝ ↦ logLik x p.1 p.2) domain p) ↔
      ∃ u ∈ Ioo 0 (uBar x), psi x u ≤ 0 := by
  constructor
  · rintro ⟨⟨κ, rate⟩, ⟨hk, hr⟩, hmax⟩
    have hbest : bestExp x ≤ logLik x κ rate := by
      apply bestExp_le_upperBound hn hx
      rintro _ ⟨p, hp, rfl⟩
      exact hmax hp
    obtain ⟨u, hu, he⟩ := exists_qOf (mul_pos hr (avg_pos hn hx))
    have hb := profile_bound hn hx hk hr
    rw [← he] at hb
    have hgap : 0 ≤ gap x (qOf u) := by
      by_contra! h
      exact (not_le_of_gt (add_lt_of_neg_right _ (mul_neg_of_pos_of_neg
        (by exact_mod_cast hn) h))) (hbest.trans hb)
    exact ⟨u, ⟨hu, lt_of_not_ge fun h ↦ (not_le_of_gt (gap_tail_neg hn hx h)) hgap⟩,
      (gap_nonneg_iff hn hx hu).mp hgap⟩
  · rintro ⟨w, hw, hψ⟩
    obtain ⟨u, hu, hmax⟩ := compact_profile hn hx
    have hall := compact_profile_bound hn hx hmax
    have hwgap := (gap_nonneg_iff hn hx hw.1).mpr hψ
    -- If the closed interval returns the exponential endpoint, the witness ties it.
    have hex : ∃ v > 0, ∀ q > 0, gap x q ≤ gap x (qOf v) := by
      rcases (show 0 ≤ u from hu.1).eq_or_lt with he | hpos
      · subst u
        have hzero : gap x (qOf 0) = 0 := by simp [gap, qOf, A, avg]
        refine ⟨w, hw.1, fun q hq ↦ (hall q hq).trans ?_⟩
        rw [hzero]
        exact hwgap
      · exact ⟨u, hpos, hall⟩
    obtain ⟨v, hv, hglobal⟩ := hex
    let rate := qOf v / avg x
    have hr : 0 < rate := div_pos (qOf_pos hv) (avg_pos hn hx)
    have he : rate * avg x = qOf v := div_mul_cancel₀ _ (avg_pos hn hx).ne'
    refine ⟨(shape x rate, rate), ⟨(shape_profile hn hx hr).1, hr⟩, ?_⟩
    rintro ⟨κ', rate'⟩ ⟨hk', hr'⟩
    change logLik x κ' rate' ≤ logLik x (shape x rate) rate
    have H := profile_gap hn hx hr
    rw [he] at H
    have hgap := mul_le_mul_of_nonneg_left
      (hglobal (rate' * avg x) (mul_pos hr' (avg_pos hn hx))) (show (0 : ℝ) ≤ n by positivity)
    linarith only [H, hgap, profile_bound hn hx hk' hr']

/-- Without a sign witness, the exponential likelihood is the unattained supremum. -/
theorem exponential_sup_of_no_witness (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ¬ ∃ u ∈ Ioo 0 (uBar x), psi x u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain) = bestExp x ∧
      ∀ p ∈ domain, logLik x p.1 p.2 < bestExp x := by
  have hlt : ∀ p ∈ domain, logLik x p.1 p.2 < bestExp x := by
    rintro ⟨κ, rate⟩ ⟨hk, hr⟩
    obtain ⟨u, hu, he⟩ := exists_qOf (mul_pos hr (avg_pos hn hx))
    have hg : gap x (qOf u) < 0 := by
      by_contra! hh
      exact h ⟨u, ⟨hu, lt_of_not_ge fun h' ↦ (not_le_of_gt (gap_tail_neg hn hx h')) hh⟩,
        (gap_nonneg_iff hn hx hu).mp hh⟩
    have H := profile_bound hn hx hk hr
    rw [← he] at H
    exact H.trans_lt (add_lt_of_neg_right _ (mul_neg_of_pos_of_neg (by exact_mod_cast hn) hg))
  refine ⟨le_antisymm (csSup_le (likelihood_nonempty x) ?_) (bestExp_le_sup hn hx), hlt⟩
  rintro _ ⟨p, hp, rfl⟩
  exact (hlt p hp).le

private theorem avg_const (hn : 0 < n) (c : ℝ) : avg (fun _ : Fin n ↦ c) = c := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp [avg, hN]

/-- For a positive constant sample the profile loss is the given strictly positive log-ratio. -/
theorem constant_profile_loss (hn : 0 < n) {c rate : ℝ} (hc : 0 < c) (hr : 0 < rate) :
    let x : Fin n → ℝ := fun _ ↦ c
    let z := rate * c
    (bestExp x - logLik x (shape x rate) rate) / n = log ((1 + z) * log (1 + z) / z) ∧
      0 < log ((1 + z) * log (1 + z) / z) := by
  dsimp only
  let x : Fin n → ℝ := fun _ ↦ c
  let z := rate * c
  have hz : 0 < z := mul_pos hr hc
  have h1 : 1 < 1 + z := by linarith
  have hp : 0 < 1 + z := by linarith
  have hl := log_pos h1
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hm : avg x = c := avg_const hn c
  have hy (i : Fin n) : y x i = 1 := by simp [y, hm, x, hc.ne']
  have ha : A x z = log (1 + z) := by simp [A, hy, avg_const hn]
  have hgap := profile_gap hn (x := x) (fun _ ↦ hc) hr
  rw [hm] at hgap
  change _ = n * gap x z at hgap
  have he : (bestExp x - logLik x (shape x rate) rate) = -((n : ℝ) * gap x z) := by
    linarith only [hgap]
  change (bestExp x - logLik x (shape x rate) rate) / n = _ ∧ _
  rw [he, neg_div, mul_comm (n : ℝ), mul_div_cancel_right₀ _ hN]
  constructor
  · rw [gap, ha, log_div (mul_pos hp hl).ne' hz.ne', log_mul hp.ne' hl.ne']
    ring
  · apply log_pos
    apply (lt_div_iff₀ hz).2
    have hne : (1 + z)⁻¹ ≠ 1 := by
      intro h
      have H := inv_mul_cancel₀ hp.ne'
      rw [h, one_mul] at H
      linarith
    have H := log_lt_sub_one_of_pos (inv_pos.mpr hp) hne
    rw [log_inv] at H
    have H' := mul_lt_mul_of_pos_right H hp
    rw [sub_mul, inv_mul_cancel₀ hp.ne', one_mul] at H'
    rw [neg_mul, mul_comm (log (1 + z))] at H'
    linarith

/-- A positive constant sample has no finite all-free maximum. -/
theorem constant_no_max (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    ¬ ∃ p ∈ domain, IsMaxOn (fun p : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ c) p.1 p.2) domain p := by
  let x : Fin n → ℝ := fun _ ↦ c
  rintro ⟨⟨κ, rate⟩, ⟨hk, hr⟩, hmax⟩
  have hu : bestExp x ≤ logLik x κ rate := by
    apply bestExp_le_upperBound hn (fun _ ↦ hc)
    rintro _ ⟨p, hp, rfl⟩
    exact hmax hp
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have H := constant_profile_loss hn hc hr
  dsimp only at H
  have hp : logLik x (shape x rate) rate < bestExp x := by
    have hl := H.1.symm ▸ H.2
    exact sub_pos.mp ((div_pos_iff_of_pos_right hN).mp hl)
  have hle : logLik x κ rate ≤ logLik x (shape x rate) rate := by
    by_cases h : κ = shape x rate
    · simp [h]
    · exact ((shape_profile hn (x := x) (fun _ ↦ hc) hr).2 κ hk h).le
  exact (not_le_of_gt (hle.trans_lt hp)) hu

/-- Holding the rate gives exactly one positive maximizing shape. -/
theorem heldRate_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {rate : ℝ} (hr : 0 < rate) :
    ∃! κ, κ ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun k ↦ logLik x k rate) (Ioi 0) κ := by
  have H := shape_profile hn hx hr
  refine ⟨shape x rate, ⟨H.1, ?_⟩, ?_⟩
  · intro k hk
    change logLik x k rate ≤ logLik x (shape x rate) rate
    by_cases h : k = shape x rate
    · simp [h]
    · exact (H.2 k hk h).le
  · intro k ⟨hk, hm⟩
    by_contra h
    exact (not_le_of_gt (H.2 k hk h)) (hm H.1)

def logRateScore (x : Fin n → ℝ) (κ t : ℝ) : ℝ :=
  n - (κ + 1) * ∑ i, (1 - (1 + exp t * x i)⁻¹)

private theorem hasDerivAt_logRate (x : Fin n → ℝ) (hx : ∀ i, 0 < x i) (κ t : ℝ) :
    HasDerivAt (fun s ↦ logLik x κ (exp s)) (logRateScore x κ t) t := by
  have H (i : Fin n) :
      HasDerivAt (fun s ↦ log (1 + exp s * x i)) (1 - (1 + exp t * x i)⁻¹) t := by
    have h : 0 < 1 + exp t * x i := add_pos zero_lt_one (mul_pos (exp_pos _) (hx i))
    convert (((hasDerivAt_exp t).mul_const (x i)).const_add 1).log h.ne' using 1
    field_simp [h.ne']
    ring
  have hsum := HasDerivAt.sum (u := Finset.univ) (fun i _ ↦ H i)
  have hlin := ((hasDerivAt_id t).const_add (log κ)).const_mul (n : ℝ)
  convert hlin.sub (hsum.const_mul (κ + 1)) using 1
  · ext s
    simp [logLik_formula, Finset.sum_apply]
  · simp [logRateScore]

/-- In log-rate coordinates the score is the likelihood derivative and decreases strictly. -/
theorem heldShape_score_strictAnti (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {κ : ℝ} (hk : 0 < κ) :
    (∀ t, HasDerivAt (fun s ↦ logLik x κ (exp s)) (logRateScore x κ t) t) ∧
      StrictAnti (logRateScore x κ) := by
  refine ⟨hasDerivAt_logRate x hx κ, ?_⟩
  intro a b hab
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  have H : (∑ i, (1 - (1 + exp a * x i)⁻¹)) < ∑ i, (1 - (1 + exp b * x i)⁻¹) := by
    apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
    intro i _
    apply sub_lt_sub_left
    rw [← one_div, ← one_div]
    exact one_div_lt_one_div_of_lt (add_pos zero_lt_one (mul_pos (exp_pos _) (hx i)))
      (add_lt_add_right (mul_lt_mul_of_pos_right (exp_lt_exp.mpr hab) (hx i)) 1)
  unfold logRateScore
  exact sub_lt_sub_left (mul_lt_mul_of_pos_left H (by linarith)) _

/-- The held-shape log-rate score tends to the sample size on the left and to −nκ on the right. -/
theorem heldShape_score_limits {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (κ : ℝ) :
    Tendsto (logRateScore x κ) atBot (𝓝 (n : ℝ)) ∧
      Tendsto (logRateScore x κ) atTop (𝓝 (-(n : ℝ) * κ)) := by
  have hleft (i : Fin n) :
      Tendsto (fun t ↦ 1 - (1 + exp t * x i)⁻¹) atBot (𝓝 0) := by
    have h := (tendsto_exp_atBot.mul_const (x i)).const_add 1
    rw [zero_mul, add_zero] at h
    simpa using (tendsto_const_nhds (x := (1 : ℝ))).sub (h.inv₀ one_ne_zero)
  have hright (i : Fin n) :
      Tendsto (fun t ↦ 1 - (1 + exp t * x i)⁻¹) atTop (𝓝 1) := by
    have h := tendsto_atTop_add_const_left _ 1 (tendsto_exp_atTop.atTop_mul_const (hx i))
    simpa using (tendsto_const_nhds (x := (1 : ℝ))).sub (tendsto_inv_atTop_zero.comp h)
  have hl := tendsto_finsetSum Finset.univ (fun i _ ↦ hleft i)
  have hr := tendsto_finsetSum Finset.univ (fun i _ ↦ hright i)
  constructor
  · unfold logRateScore
    simpa using (tendsto_const_nhds (x := (n : ℝ))).sub (hl.const_mul (κ + 1))
  · convert (tendsto_const_nhds (x := (n : ℝ))).sub (hr.const_mul (κ + 1)) using 1
    · rfl
    · simp
      ring

/-- Holding the shape gives exactly one positive maximizing rate. -/
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {κ : ℝ} (hk : 0 < κ) :
    ∃! rate, rate ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x κ) (Ioi 0) rate := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hneg : -(n : ℝ) * κ < 0 := mul_neg_of_neg_of_pos (neg_neg_of_pos hN) hk
  obtain ⟨hl, hr⟩ := heldShape_score_limits hx κ
  obtain ⟨a, ha, hsa⟩ := ((eventually_lt_atBot (0 : ℝ)).and
    (hl.eventually (Ioi_mem_nhds hN))).exists
  obtain ⟨b, hb, hsb⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    (hr.eventually (Iio_mem_nhds hneg))).exists
  have hcont : Continuous (logRateScore x κ) := by
    have hsum : Continuous (fun t : ℝ ↦ ∑ i, (1 - (1 + exp t * x i)⁻¹)) := by
      refine continuous_finsetSum Finset.univ (fun i _ ↦ ?_)
      exact continuous_const.sub ((continuous_const.add (continuous_exp.mul continuous_const)).inv₀
        (fun t ↦ (add_pos zero_lt_one (mul_pos (exp_pos t) (hx i))).ne'))
    exact continuous_const.sub (continuous_const.mul hsum)
  obtain ⟨t, _, ht⟩ := intermediate_value_Icc' (ha.le.trans hb.le) hcont.continuousOn
    (show (0 : ℝ) ∈ Icc (logRateScore x κ b) (logRateScore x κ a) from
      ⟨hsb.le, hsa.le⟩)
  obtain ⟨hd, hs⟩ := heldShape_score_strictAnti hn hx hk
  have hc : Continuous (fun s ↦ logLik x κ (exp s)) :=
    continuous_iff_continuousAt.mpr fun s ↦ (hd s).continuousAt
  have hinc : StrictMonoOn (fun s ↦ logLik x κ (exp s)) (Iic t) := by
    apply strictMonoOn_of_deriv_pos (convex_Iic t) hc.continuousOn
    intro s h
    rw [interior_Iic] at h
    rw [(hd s).deriv, ← ht]
    exact hs (show s < t from h)
  have hdec : StrictAntiOn (fun s ↦ logLik x κ (exp s)) (Ici t) := by
    apply strictAntiOn_of_deriv_neg (convex_Ici t) hc.continuousOn
    intro s h
    rw [interior_Ici] at h
    rw [(hd s).deriv, ← ht]
    exact hs (show t < s from h)
  have hex (r : ℝ) (h : 0 < r) (hne : r ≠ exp t) : logLik x κ r < logLik x κ (exp t) := by
    have he : log r ≠ t := by
      intro h'
      exact hne (by rw [← h', exp_log h])
    rcases lt_or_gt_of_ne he with hlt | hgt
    · have H := hinc (mem_Iic.mpr hlt.le) (mem_Iic.mpr le_rfl) hlt
      simpa [exp_log h] using H
    · have H := hdec (mem_Ici.mpr le_rfl) (mem_Ici.mpr hgt.le) hgt
      simpa [exp_log h] using H
  refine ⟨exp t, ⟨exp_pos t, ?_⟩, ?_⟩
  · intro r hp
    change logLik x κ r ≤ logLik x κ (exp t)
    by_cases he : r = exp t
    · simp [he]
    · exact (hex r hp he).le
  · intro r ⟨hp, hmax⟩
    by_contra he
    exact (not_le_of_gt (hex r hp he)) (hmax (exp_pos t))

end

end UnivariateML.Lomax
