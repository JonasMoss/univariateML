module

public import UnivariateML.Family.Cauchy
public import UnivariateML.Generic.Profile
public import Mathlib.Analysis.Calculus.ParametricIntervalIntegral
public import Mathlib.Analysis.Calculus.LocalExtr.Basic
public import Mathlib.Analysis.SpecialFunctions.Integrals.Basic

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Half-Cauchy and Gompertz likelihoods

The statements concern nonempty uncensored samples and exact real arithmetic.
For the half-Cauchy, the log likelihood is restricted to the support and
the Cauchy likelihood supplies the scale analysis after normalization.

## Deviations

The half-Cauchy collapse statements use log likelihood, and the sample
minimum is given as an attained lower bound. For Gompertz the free-fit
condition is stated in terms of the sample sums. The rate-zero face uses
the continuous extension of the integrated hazard. The dossier's strict
concavity statements concern the log likelihood, including the profile
after maximizing out the initial hazard.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators

namespace UnivariateML.HalfCauchy

noncomputable section

variable {n : ℕ}

/-- Log likelihood on the support, including the endpoint. -/
def logLik (x : Fin n → ℝ) (μ σ : ℝ) : ℝ :=
  Cauchy.logLik x μ σ + (n : ℝ) * log 2

def domain (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {μ | ∀ i, μ ≤ x i} ×ˢ Ioi 0

/-- Half-Cauchy density, with zero outside its closed support. -/
def density (μ σ y : ℝ) : ℝ :=
  if μ ≤ y then 2 * σ / (π * ((μ - y)^2 + σ^2)) else 0

theorem heldPair_support (x : Fin n → ℝ) {μ σ : ℝ} (hσ : 0 < σ) :
    (0 < ∏ i, density μ σ (x i)) ↔ (μ, σ) ∈ domain x := by
  constructor
  · intro h
    refine ⟨fun i ↦ ?_, hσ⟩
    by_contra! hμ
    have hz : density μ σ (x i) = 0 := ite_eq_right hμ.not_ge
    exact h.ne' (Finset.prod_eq_zero (Finset.mem_univ i) hz)
  · intro h
    apply Finset.prod_pos
    intro i _
    rw [density, ite_eq_left (h.1 i)]
    positivity

theorem heldPair_logLik (x : Fin n → ℝ) {μ σ : ℝ} (hp : (μ, σ) ∈ domain x) :
    log (∏ i, density μ σ (x i)) = logLik x μ σ := by
  have hσ : 0 < σ := hp.2
  have hpos (i : Fin n) : 0 < density μ σ (x i) := by
    rw [density, ite_eq_left (hp.1 i)]
    positivity
  rw [log_prod (fun i _ ↦ (hpos i).ne'), logLik, Cauchy.logLik, ← nsmul_eq_mul]
  have H (i : Fin n) : log (density μ σ (x i)) = Cauchy.logDensity μ σ (x i) + log 2 := by
    rw [density, ite_eq_left (hp.1 i), log_div (by positivity) (by positivity),
      log_mul two_ne_zero hσ.ne', log_mul pi_ne_zero (by positivity), Cauchy.logDensity]
    ring
  simp [H, Finset.sum_add_distrib, nsmul_eq_mul]

def score (x : Fin n → ℝ) (μ t : ℝ) : ℝ :=
  (n : ℝ) - 2 * ∑ i, exp t ^ 2 / (exp t ^ 2 + (x i - μ)^2)

theorem score_eq_cauchy (x : Fin n → ℝ) (μ t : ℝ) :
    score x μ t = Cauchy.heldScore x μ t := by
  have h (i : Fin n) : 1 - 2 * (exp t ^ 2 / (exp t ^ 2 + (x i - μ)^2)) =
      2 * ((μ - x i)^2 / ((μ - x i)^2 + exp t ^ 2)) - 1 := by
    have he : (x i - μ)^2 = (μ - x i)^2 := by ring
    rw [he, add_comm (exp t ^ 2)]
    have hd : (μ - x i)^2 + exp t ^ 2 ≠ 0 := by positivity
    field_simp
    ring
  unfold score Cauchy.heldScore
  simp_rw [← h, Finset.sum_sub_distrib, ← Finset.mul_sum]
  simp

theorem hasDerivAt_log_scale (x : Fin n → ℝ) (μ t : ℝ) :
    HasDerivAt (fun u ↦ logLik x μ (exp u)) (score x μ t) t := by
  have H (i : Fin n) :
      HasDerivAt (fun u ↦ Cauchy.logDensity μ (exp u) (x i))
        (1 - 2 * (exp t ^ 2 / (exp t ^ 2 + (x i - μ)^2))) t := by
    let r := μ - x i
    have hd : r^2 + exp t ^ 2 ≠ 0 := by positivity
    have he : (x i - μ)^2 = r^2 := by dsimp [r]; ring
    rw [he, add_comm (exp t ^ 2)]
    convert (((hasDerivAt_id t).sub_const (log π)).sub
      ((((hasDerivAt_exp t).pow 2).const_add (r^2)).log hd)) using 1
    · ext u
      simp [Cauchy.logDensity, r]
    · norm_num
      ring
  have H' := (HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ H i).add_const
    ((n : ℝ) * log 2)
  simpa [logLik, Cauchy.logLik, score, Finset.sum_sub_distrib, ← Finset.mul_sum] using H'

theorem score_strictAnti (x : Fin n → ℝ) (μ : ℝ) (h : ∃ i, x i ≠ μ) :
    StrictAnti (score x μ) := by
  intro t u htu
  have hsq : exp t ^ 2 < exp u ^ 2 :=
    (sq_lt_sq₀ (exp_pos t).le (exp_pos u).le).mpr (exp_lt_exp.mpr htu)
  have hterm (i : Fin n) (hi : x i ≠ μ) :
      exp t ^ 2 / (exp t ^ 2 + (x i - μ)^2) <
        exp u ^ 2 / (exp u ^ 2 + (x i - μ)^2) := by
    apply (div_lt_div_iff₀ (by positivity) (by positivity)).2
    have hp := mul_pos (sub_pos.mpr hsq) (sq_pos_of_ne_zero (sub_ne_zero.mpr hi))
    nlinarith only [hp]
  obtain ⟨j, hj⟩ := h
  have H := Finset.sum_lt_sum
    (s := Finset.univ)
    (fun i _ ↦ show exp t ^ 2 / (exp t ^ 2 + (x i - μ)^2) ≤
        exp u ^ 2 / (exp u ^ 2 + (x i - μ)^2) from by
      by_cases hi : x i = μ
      · simp [hi, (exp_pos t).ne', (exp_pos u).ne']
      · exact (hterm i hi).le)
    ⟨j, Finset.mem_univ _, hterm j hj⟩
  unfold score
  linarith only [H]

private theorem scale_max_iff (x : Fin n → ℝ) (μ σ : ℝ) :
    IsMaxOn (logLik x μ) (Ioi 0) σ ↔ IsMaxOn (Cauchy.logLik x μ) (Ioi 0) σ := by
  constructor <;> intro h s hs <;> simpa only [logLik, add_le_add_iff_right] using h hs

theorem heldEndpoint_root (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (h : 2 * Cauchy.multiplicity x μ < n) :
    (∃! σ, 0 < σ ∧ score x μ (log σ) = 0) ∧
      ∀ σ > 0, IsMaxOn (logLik x μ) (Ioi 0) σ ↔ score x μ (log σ) = 0 := by
  simpa only [score_eq_cauchy, scale_max_iff] using Cauchy.heldLocation_unique hn x μ h

theorem heldEndpoint_iff (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    (∃! σ, σ ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔
      2 * Cauchy.multiplicity x μ < n := by
  simpa only [scale_max_iff] using Cauchy.heldLocation_iff hn x μ

theorem heldScale (x : Fin n → ℝ) {m σ : ℝ} (hmin : ∀ i, m ≤ x i)
    (hm : ∃ j, x j = m) (hσ : 0 < σ) :
    ∀ μ, μ ≤ m →
      (IsMaxOn (fun v ↦ logLik x v σ) {v | v ≤ m} μ ↔ μ = m) := by
  have hmono : StrictMonoOn (fun v ↦ logLik x v σ) {v | v ≤ m} := by
    intro u hu v hv huv
    have hterm (i : Fin n) : Cauchy.logDensity u σ (x i) < Cauchy.logDensity v σ (x i) := by
      have H : (v - x i)^2 < (u - x i)^2 := by
        have hv' : v ≤ m := hv
        have hvi := hv'.trans (hmin i)
        nlinarith only [mul_pos (sub_pos.mpr huv)
          (show 0 < 2 * x i - u - v by linarith)]
      unfold Cauchy.logDensity
      exact sub_lt_sub_left (log_lt_log (by positivity) (add_lt_add_left H (σ ^ 2))) _
    obtain ⟨j, -⟩ := hm
    apply add_lt_add_left
    exact Finset.sum_lt_sum (fun i _ ↦ (hterm i).le) ⟨j, Finset.mem_univ _, hterm j⟩
  intro μ hμ
  constructor
  · intro h
    exact hμ.eq_of_not_lt (fun hlt ↦ (hmono hμ (show m ≤ m from le_rfl) hlt).not_ge
      (h (show m ≤ m from le_rfl)))
  · intro he
    rw [he]
    exact fun _ hv ↦ hmono.monotoneOn hv (show m ≤ m from le_rfl) hv

theorem joint_argmax (x : Fin n → ℝ) {m : ℝ} (hmin : ∀ i, m ≤ x i)
    (hm : ∃ j, x j = m) {p : ℝ × ℝ} (hp : p ∈ domain x) :
    IsMaxOn (fun q ↦ logLik x q.1 q.2) (domain x) p ↔
      p.1 = m ∧ IsMaxOn (logLik x m) (Ioi 0) p.2 := by
  obtain ⟨j, hj⟩ := hm
  have hm : ∃ j, x j = m := ⟨j, hj⟩
  have hsupport (μ : ℝ) : (∀ i, μ ≤ x i) ↔ μ ≤ m := by
    constructor
    · intro h
      simpa [hj] using h j
    · exact fun h i ↦ h.trans (hmin i)
  constructor
  · intro h
    have he := (heldScale x hmin hm hp.2 p.1 ((hsupport _).mp hp.1)).mp
      (fun v hv ↦ h (show (v, p.2) ∈ domain x from ⟨(hsupport _).mpr hv, hp.2⟩))
    exact ⟨he, fun s hs ↦ by
      have H := h (show (m, s) ∈ domain x from ⟨hmin, hs⟩)
      simpa [he] using H⟩
  · rintro ⟨he, h⟩ q hq
    have H := (heldScale x hmin hm hq.2 m le_rfl).mpr rfl
    change logLik x q.1 q.2 ≤ logLik x p.1 p.2
    rw [he]
    exact (H ((hsupport _).mp hq.1)).trans (h hq.2)

theorem joint_iff (hn : 0 < n) (x : Fin n → ℝ) {m : ℝ} (hmin : ∀ i, m ≤ x i)
    (hm : ∃ j, x j = m) :
    (∃! p, p ∈ domain x ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) (domain x) p) ↔
      2 * Cauchy.multiplicity x m < n := by
  rw [← heldEndpoint_iff hn x m]
  constructor
  · rintro ⟨p, ⟨hp, hmax⟩, hu⟩
    obtain ⟨he, hs⟩ := (joint_argmax x hmin hm hp).mp hmax
    refine ⟨p.2, ⟨hp.2, hs⟩, fun s ht ↦ ?_⟩
    have hq : (m, s) ∈ domain x := ⟨hmin, ht.1⟩
    exact congrArg Prod.snd (hu (m, s) ⟨hq, (joint_argmax x hmin hm hq).mpr ⟨rfl, ht.2⟩⟩)
  · rintro ⟨s, hs, hu⟩
    have hp : (m, s) ∈ domain x := ⟨hmin, hs.1⟩
    refine ⟨(m, s), ⟨hp, (joint_argmax x hmin hm hp).mpr ⟨rfl, hs.2⟩⟩, fun p ht ↦ ?_⟩
    obtain ⟨he, hmax⟩ := (joint_argmax x hmin hm ht.1).mp ht.2
    exact Prod.ext he (hu _ ⟨ht.1.2, hmax⟩)

theorem majority_collapse (x : Fin n → ℝ) {μ : ℝ} (h : n < 2 * Cauchy.multiplicity x μ) :
    Tendsto (logLik x μ) (𝓝[>] 0) atTop :=
  tendsto_atTop_add_const_right _ _ (Cauchy.majority_collapse x h)

theorem half_collapse (hn : 0 < n) (x : Fin n → ℝ) {μ : ℝ}
    (h : 2 * Cauchy.multiplicity x μ = n) :
    Tendsto (logLik x μ) (𝓝[>] 0) (𝓝 (Cauchy.collapseValue x μ + (n : ℝ) * log 2)) ∧
      (∀ σ > 0, logLik x μ σ < Cauchy.collapseValue x μ + (n : ℝ) * log 2) ∧
      sSup (logLik x μ '' Ioi 0) = Cauchy.collapseValue x μ + (n : ℝ) * log 2 := by
  have hl := (Cauchy.half_collapse x h).add_const ((n : ℝ) * log 2)
  have ha : StrictAntiOn (logLik x μ) (Ioi 0) := fun a ha b hb hab ↦
    add_lt_add_left ((Cauchy.heldLocation_noMLE hn x μ h.ge).1 ha hb hab) _
  have hub (σ : ℝ) (hs : 0 < σ) :
      logLik x μ σ ≤ Cauchy.collapseValue x μ + (n : ℝ) * log 2 := by
    apply ge_of_tendsto hl
    filter_upwards [self_mem_nhdsWithin,
      (nhdsWithin_le_nhds : 𝓝[>] (0 : ℝ) ≤ 𝓝 0) (Iio_mem_nhds hs)] with u hu hus
    exact ha.antitoneOn hu hs hus.le
  refine ⟨hl, fun σ hs ↦ (ha (half_pos hs) hs (half_lt_self hs)).trans_le
    (hub (σ / 2) (half_pos hs)), le_antisymm ?_ ?_⟩
  · exact csSup_le ⟨_, ⟨1, one_pos, rfl⟩⟩ (by rintro _ ⟨σ, hs, rfl⟩; exact hub σ hs)
  · exact Generic.limit_le_sSup (l := 𝓝[>] (0 : ℝ)) (p := id) self_mem_nhdsWithin hl
      ⟨_, by rintro _ ⟨σ, hs, rfl⟩; exact hub σ hs⟩

/-- The log-scale score starts at the endpoint-mass cutoff and ends at minus the sample size. -/
theorem score_limits (x : Fin n → ℝ) (μ : ℝ) :
    Tendsto (score x μ) atBot (𝓝 ((n : ℝ) - 2 * Cauchy.multiplicity x μ)) ∧
      Tendsto (score x μ) atTop (𝓝 (-(n : ℝ))) := by
  let f := fun (i : Fin n) (t : ℝ) ↦ exp t ^ 2 / (exp t ^ 2 + (x i - μ)^2)
  let l := fun i : Fin n ↦ if x i = μ then (1 : ℝ) else 0
  have hbot (i : Fin n) : Tendsto (f i) atBot (𝓝 (l i)) := by
    by_cases hi : x i = μ
    · simp [f, l, hi, fun t ↦ (exp_pos t).ne']
    · have hz := sq_pos_of_ne_zero (sub_ne_zero.mpr hi)
      have h : Tendsto (fun t : ℝ ↦ exp t ^ 2) atBot (𝓝 0) := by
        simpa using tendsto_exp_atBot.pow 2
      simpa [f, l, hi, Pi.div_def] using
        h.div (by simpa using h.add_const ((x i - μ)^2)) (b := (x i - μ)^2) hz.ne'
  have htop (i : Fin n) : Tendsto (f i) atTop (𝓝 1) := by
    have h : Tendsto (fun t : ℝ ↦ exp (-t) ^ 2) atTop (𝓝 0) := by
      simpa using (tendsto_exp_atBot.comp tendsto_neg_atTop_atBot).pow 2
    have he : Tendsto (fun t : ℝ ↦ 1 + (x i - μ)^2 * exp (-t) ^ 2) atTop (𝓝 1) := by
      simpa using (h.const_mul ((x i - μ)^2)).const_add 1
    refine (by simpa using he.inv₀ one_ne_zero :
      Tendsto (fun t : ℝ ↦ (1 + (x i - μ)^2 * exp (-t) ^ 2)⁻¹) atTop (𝓝 1)).congr ?_
    intro t
    dsimp [f]
    rw [exp_neg]
    have hn : exp t ≠ 0 := (exp_pos t).ne'
    have hd : exp t ^ 2 + (x i - μ)^2 ≠ 0 := by positivity
    have hD : 1 + (x i - μ)^2 * (exp t)⁻¹ ^ 2 ≠ 0 := by positivity
    field_simp
  have hl : ∑ i, l i = (Cauchy.multiplicity x μ : ℝ) := by
    simp [l, Cauchy.multiplicity]
  have h₁ := ((tendsto_finsetSum Finset.univ fun i _ ↦ hbot i).const_mul 2).const_sub (n : ℝ)
  have h₂ := ((tendsto_finsetSum Finset.univ fun i _ ↦ htop i).const_mul 2).const_sub (n : ℝ)
  have he : (fun t ↦ (n : ℝ) - 2 * ∑ i, f i t) = score x μ := rfl
  rw [he, hl] at h₁
  rw [he] at h₂
  have hn : (n : ℝ) - 2 * ∑ _i : Fin n, (1 : ℝ) = -(n : ℝ) := by simp; ring
  rw [hn] at h₂
  exact ⟨h₁, h₂⟩

theorem joint_half_collapse (hn : 0 < n) (x : Fin n → ℝ) {m : ℝ}
    (hmin : ∀ i, m ≤ x i) (hm : ∃ j, x j = m) (h : 2 * Cauchy.multiplicity x m = n) :
    let L := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
    let c := Cauchy.collapseValue x m + (n : ℝ) * log 2
    Tendsto (logLik x m) (𝓝[>] 0) (𝓝 c) ∧
      (∀ p ∈ domain x, L p < c) ∧ sSup (L '' domain x) = c := by
  dsimp
  obtain ⟨hl, hlt, _⟩ := half_collapse hn x h
  obtain ⟨j, hj⟩ := hm
  let L := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
  let c := Cauchy.collapseValue x m + (n : ℝ) * log 2
  have hs (p : ℝ × ℝ) (hp : p ∈ domain x) : L p < c := by
    have hσ : 0 < p.2 := hp.2
    have hu : p.1 ≤ m := hj ▸ hp.1 j
    exact (((heldScale x hmin ⟨j, hj⟩ hσ m le_rfl).mpr rfl) hu).trans_lt (hlt p.2 hσ)
  have hu : c ∈ upperBounds (L '' domain x) := by
    rintro _ ⟨p, hp, rfl⟩
    exact (hs p hp).le
  have hpath : ∀ᶠ σ in 𝓝[>] (0 : ℝ), (m, σ) ∈ domain x :=
    Filter.Eventually.mono self_mem_nhdsWithin fun _ hσ ↦ ⟨hmin, hσ⟩
  exact ⟨hl, hs, le_antisymm (csSup_le ⟨_, ⟨(m, 1), ⟨hmin, one_pos⟩, rfl⟩⟩ hu)
    (Generic.limit_le_sSup hpath hl ⟨_, hu⟩)⟩

theorem joint_majority_unbounded (x : Fin n → ℝ) {μ : ℝ} (hx : ∀ i, μ ≤ x i)
    (h : n < 2 * Cauchy.multiplicity x μ) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain x) := by
  rintro ⟨M, hM⟩
  obtain ⟨σ, hσ, hgt⟩ := (Filter.Eventually.and (f := 𝓝[>] (0 : ℝ)) self_mem_nhdsWithin
    ((majority_collapse x h).eventually (eventually_gt_atTop M))).exists
  exact hgt.not_ge (hM ⟨(μ, σ), ⟨hx, hσ⟩, rfl⟩)

end

end UnivariateML.HalfCauchy

namespace UnivariateML.Gompertz

open MeasureTheory intervalIntegral

noncomputable section

variable {n : ℕ}

def sampleSum (x : Fin n → ℝ) : ℝ := ∑ i, x i

def squareSum (x : Fin n → ℝ) : ℝ := ∑ i, x i ^ 2

/-- Exponential-weighted sample moment integrated over the exposure intervals. -/
def moment (x : Fin n → ℝ) (k : ℕ) (b : ℝ) : ℝ :=
  ∑ i, ∫ t in (0 : ℝ)..x i, t ^ k * exp (b * t)

/-- Total integrated unit-initial-hazard, including its value at rate zero. -/
def integrated (x : Fin n → ℝ) (b : ℝ) : ℝ := moment x 0 b

def logLik (x : Fin n → ℝ) (a b : ℝ) : ℝ :=
  (n : ℝ) * log a + b * sampleSum x - a * integrated x b

def hazard (x : Fin n → ℝ) (b : ℝ) : ℝ := n / integrated x b

def profile (x : Fin n → ℝ) (b : ℝ) : ℝ :=
  (n : ℝ) * log n - n + b * sampleSum x - (n : ℝ) * log (integrated x b)

def heldScore (x : Fin n → ℝ) (a b : ℝ) : ℝ := sampleSum x - a * moment x 1 b

def profileScore (x : Fin n → ℝ) (b : ℝ) : ℝ :=
  sampleSum x - (n : ℝ) * (moment x 1 b / integrated x b)

private theorem moment_deriv (x : Fin n → ℝ) (k : ℕ) (b : ℝ) :
    HasDerivAt (moment x k) (moment x (k + 1) b) b := by
  have D (t u : ℝ) : HasDerivAt (fun v ↦ t ^ k * exp (v * t))
      (exp (u * t) * t * t ^ k) u := by
    convert (((hasDerivAt_id u).mul_const t).exp.const_mul (t ^ k)) using 1
    <;> simp [mul_comm, mul_left_comm]
  apply HasDerivAt.fun_sum
  intro i _
  let L := ContinuousLinearMap.smulRightL ℝ ℝ ℝ (1 : ℝ →L[ℝ] ℝ)
  have hfd (t u : ℝ) : fderiv ℝ (fun v ↦ t ^ k * exp (v * t)) u =
      L (t ^ (k + 1) * exp (u * t)) := by
    rw [(D t u).hasFDerivAt.fderiv]
    change (1 : ℝ →L[ℝ] ℝ).smulRight _ = (1 : ℝ →L[ℝ] ℝ).smulRight _
    congr 1
    rw [pow_succ]
    ring
  have H := intervalIntegral.hasFDerivAt_integral_of_continuousOn_fderiv
    (μ := volume) (f := fun v t : ℝ ↦ t ^ k * exp (v * t))
    (a := 0) (b := x i) (x₀ := b) (u := univ) univ_mem
    (by change ContinuousOn (fun p : ℝ × ℝ ↦ p.2 ^ k * exp (p.1 * p.2)) _; fun_prop)
    (fun t _ u _ ↦ (D t u).differentiableAt.differentiableWithinAt)
    (by simp_rw [hfd]; exact (L.continuous.comp (by fun_prop)).continuousOn)
  have HC : Continuous (fun t : ℝ ↦ t ^ (k + 1) * exp (b * t)) := by fun_prop
  simp_rw [hfd] at H
  have Happly := H.hasDerivAt
  have hI : IntervalIntegrable (fun t ↦ L (t ^ (k + 1) * exp (b * t))) volume 0 (x i) :=
    (L.continuous.comp HC).intervalIntegrable _ _
  rw [ContinuousLinearMap.intervalIntegral_apply hI] at Happly
  have h_eval (t : ℝ) : L (t ^ (k + 1) * exp (b * t)) 1 = t ^ (k + 1) * exp (b * t) := by
    change (1 : ℝ) • (t ^ (k + 1) * exp (b * t)) = _
    exact one_smul ℝ _
  simpa only [h_eval] using Happly

private theorem sampleSum_pos_iff (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) :
    0 < sampleSum x ↔ ∃ j, 0 < x j := by
  constructor
  · intro h
    by_contra! H
    exact h.not_ge (Finset.sum_nonpos fun i _ ↦ H i)
  · rintro ⟨j, hj⟩
    exact Finset.sum_pos' (fun i _ ↦ hx i) ⟨j, Finset.mem_univ _, hj⟩

private theorem moment_pos (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) (hs : 0 < sampleSum x)
    (k : ℕ) (b : ℝ) : 0 < moment x k b := by
  obtain ⟨j, hj⟩ := (sampleSum_pos_iff x hx).mp hs
  have H (i : Fin n) : Continuous (fun t : ℝ ↦ t ^ k * exp (b * t)) := by fun_prop
  apply Finset.sum_pos' _ ⟨j, Finset.mem_univ _, ?_⟩
  · exact fun i _ ↦ integral_nonneg (hx i) fun t ht ↦
      mul_nonneg (pow_nonneg ht.1 _) (exp_pos _).le
  · exact intervalIntegral_pos_of_pos_on ((H j).intervalIntegrable _ _)
      (fun t ht ↦ mul_pos (pow_pos ht.1 _) (exp_pos _)) hj

private theorem integrated_pos (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (b : ℝ) : 0 < integrated x b := moment_pos x hx hs 0 b

theorem integrated_zero (x : Fin n → ℝ) : integrated x 0 = sampleSum x := by
  simp [integrated, moment, sampleSum]

theorem hasDerivAt_integrated_zero (x : Fin n → ℝ) :
    HasDerivAt (integrated x) (squareSum x / 2) 0 := by
  have h := moment_deriv x 0 0
  change HasDerivAt (integrated x) (moment x 1 0) 0 at h
  convert h using 1
  simp [moment, squareSum, integral_id, Finset.sum_div]

theorem integrated_eq (x : Fin n → ℝ) {b : ℝ} (hb : b ≠ 0) :
    integrated x b = ∑ i, (exp (b * x i) - 1) / b := by
  simp only [integrated, moment, pow_zero, one_mul]
  apply Finset.sum_congr rfl
  intro i _
  have h (t : ℝ) : HasDerivAt (fun u ↦ exp (b * u) / b) (exp (b * t)) t := by
    convert (((hasDerivAt_id t).const_mul b).exp.div_const b) using 1
    · simp only [id_eq]
    · field_simp
      rfl
  rw [integral_eq_sub_of_hasDerivAt (fun t _ ↦ h t) ((by fun_prop : Continuous
    (fun t : ℝ ↦ exp (b * t))).intervalIntegrable _ _)]
  simp [sub_div]

/-- The positive integrated hazard yields a unique conditional MLE of the initial hazard. -/
theorem heldRate (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (b : ℝ) :
    0 < hazard x b ∧
      ∀ a > 0, (IsMaxOn (fun u ↦ logLik x u b) (Ioi 0) a ↔ a = hazard x b) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hi := integrated_pos x hx hs b
  have hp : 0 < hazard x b := div_pos hn' hi
  have he : hazard x b * integrated x b = n := div_mul_cancel₀ _ hi.ne'
  have hlt (a : ℝ) (ha : 0 < a) (hne : a ≠ hazard x b) :
      logLik x a b < logLik x (hazard x b) b := by
    have hr := log_lt_sub_one_of_pos (div_pos ha hp)
      (fun hh ↦ hne (by simpa using (div_eq_iff hp.ne').mp hh))
    rw [log_div ha.ne' hp.ne'] at hr
    have hm := mul_lt_mul_of_pos_left hr hn'
    have hd : (n : ℝ) * (a / hazard x b) = a * integrated x b := by
      calc
        (n : ℝ) * (a / hazard x b) = (hazard x b * integrated x b) * (a / hazard x b) := by rw [he]
        _ = a * integrated x b := by field_simp
    have hm' : (n : ℝ) * (log a - log (hazard x b)) <
        a * integrated x b - n := by simpa only [mul_sub, mul_one, hd] using hm
    unfold logLik
    linarith only [hm', he]
  refine ⟨hp, fun a ha ↦ ⟨?_, ?_⟩⟩
  · intro h
    by_contra hne
    exact (hlt a ha hne).not_ge (h hp)
  · rintro rfl u hu
    by_cases heq : u = hazard x b
    · simp [heq]
    · exact (hlt u hu heq).le

theorem profile_eq (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (b : ℝ) : logLik x (hazard x b) b = profile x b := by
  have hi := integrated_pos x hx hs b
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  unfold logLik profile hazard
  rw [log_div hn'.ne' hi.ne', div_mul_cancel₀ _ hi.ne']
  ring

/-- The weighted squared distance from the weighted mean has positive integral. -/
private theorem variance_pos (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (b : ℝ) :
    0 < moment x 2 b * integrated x b - moment x 1 b ^ 2 := by
  let c := moment x 1 b / integrated x b
  have hi := integrated_pos x hx hs b
  have hc : c * integrated x b = moment x 1 b := div_mul_cancel₀ _ hi.ne'
  obtain ⟨j, hj⟩ := (sampleSum_pos_iff x hx).mp hs
  let f := fun t : ℝ ↦ (t - c) ^ 2 * exp (b * t)
  have hcont : Continuous f := by dsimp [f]; fun_prop
  have hpos : 0 < ∑ i, ∫ t in (0 : ℝ)..x i, f t := by
    apply Finset.sum_pos' (fun i _ ↦ integral_nonneg (hx i) (fun t _ ↦ by dsimp [f]; positivity))
    refine ⟨j, Finset.mem_univ _, integral_pos hj hcont.continuousOn (fun t _ ↦ ?_) ?_⟩
    · dsimp [f]; positivity
    · have he : ∃ t ∈ Icc (0 : ℝ) (x j), t ≠ c := by
        by_cases H : (0 : ℝ) = c
        · exact ⟨x j, ⟨hj.le, le_rfl⟩, by linarith⟩
        · exact ⟨0, ⟨le_rfl, hj.le⟩, H⟩
      obtain ⟨t, ht, hne⟩ := he
      exact ⟨t, ht, mul_pos (sq_pos_of_ne_zero (sub_ne_zero.mpr hne)) (exp_pos _)⟩
  have he (i : Fin n) :
      (∫ t in (0 : ℝ)..x i, f t) =
        (∫ t in (0 : ℝ)..x i, t ^ 2 * exp (b * t)) -
          (2 * c) * (∫ t in (0 : ℝ)..x i, t ^ 1 * exp (b * t)) +
          c ^ 2 * (∫ t in (0 : ℝ)..x i, t ^ 0 * exp (b * t)) := by
    let g := fun (k : ℕ) (t : ℝ) ↦ t ^ k * exp (b * t)
    have hg (k : ℕ) : IntervalIntegrable (g k) volume 0 (x i) :=
      (show Continuous (g k) by dsimp [g]; fun_prop).intervalIntegrable _ _
    change _ = (∫ t in (0 : ℝ)..x i, g 2 t) - _ * (∫ t in (0 : ℝ)..x i, g 1 t) +
      _ * (∫ t in (0 : ℝ)..x i, g 0 t)
    have hf : f = fun t ↦ g 2 t - (2 * c) * g 1 t + c ^ 2 * g 0 t := by
      funext t
      dsimp [f, g]
      ring
    rw [hf, integral_add ((hg 2).sub ((hg 1).const_mul _)) ((hg 0).const_mul _),
      integral_sub (hg 2) ((hg 1).const_mul _), intervalIntegral.integral_const_mul,
      intervalIntegral.integral_const_mul]
  simp_rw [he, Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.mul_sum] at hpos
  change 0 < moment x 2 b - 2 * c * moment x 1 b + c ^ 2 * integrated x b at hpos
  have hh := mul_pos hpos hi
  nlinarith only [hh, hc, congrArg (fun z : ℝ ↦ z ^ 2) hc,
    congrArg (fun z : ℝ ↦ z * c * integrated x b) hc]

private theorem held_deriv (x : Fin n → ℝ) (a b : ℝ) :
    HasDerivAt (logLik x a) (heldScore x a b) b := by
  unfold logLik heldScore integrated
  simpa only [Pi.sub_def, id_eq, Nat.zero_add, one_mul] using
    (((hasDerivAt_id b).mul_const (sampleSum x)).const_add ((n : ℝ) * log a)).sub
      ((moment_deriv x 0 b).const_mul a)

private theorem profile_deriv (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (b : ℝ) :
    HasDerivAt (profile x) (profileScore x b) b := by
  unfold profile profileScore integrated
  simpa only [Pi.sub_def, id_eq, Nat.zero_add, one_mul] using
    (((hasDerivAt_id b).mul_const (sampleSum x)).const_add ((n : ℝ) * log n - n)).sub
      (((moment_deriv x 0 b).log (integrated_pos x hx hs b).ne').const_mul (n : ℝ))

theorem heldScore_strictAnti (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) : StrictAnti (heldScore x a) := by
  have D (b : ℝ) : HasDerivAt (heldScore x a) (-(a * moment x 2 b)) b := by
    unfold heldScore
    simpa only [Nat.reduceAdd] using ((moment_deriv x 1 b).const_mul a).const_sub (sampleSum x)
  exact strictAntiOn_univ.mp (strictAntiOn_of_deriv_neg convex_univ
    (continuous_iff_continuousAt.mpr (fun b ↦ (D b).continuousAt)).continuousOn
    (fun b _ ↦ by rw [(D b).deriv]; exact neg_neg_of_pos (mul_pos ha (moment_pos x hx hs 2 b))))

theorem profileScore_strictAnti (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) : StrictAnti (profileScore x) := by
  have D (b : ℝ) : HasDerivAt (profileScore x)
      (-((n : ℝ) * ((moment x 2 b * integrated x b - moment x 1 b ^ 2) /
        integrated x b ^ 2))) b := by
    have H := ((moment_deriv x 1 b).div (moment_deriv x 0 b)
      (integrated_pos x hx hs b).ne').const_mul (n : ℝ)
    unfold profileScore integrated
    simpa only [Nat.reduceAdd, pow_two, Pi.div_def] using H.const_sub (sampleSum x)
  exact strictAntiOn_univ.mp (strictAntiOn_of_deriv_neg convex_univ
    (continuous_iff_continuousAt.mpr (fun b ↦ (D b).continuousAt)).continuousOn
    (fun b _ ↦ by
      rw [(D b).deriv]
      exact neg_neg_of_pos (mul_pos (by exact_mod_cast hn)
        (div_pos (variance_pos x hx hs b) (sq_pos_of_pos (integrated_pos x hx hs b))))))

theorem heldHazard_strictConcave (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) :
    StrictConcaveOn ℝ univ (logLik x a) := by
  have hd (b : ℝ) := held_deriv x a b
  apply StrictAnti.strictConcaveOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun b ↦ (hd b).continuousAt)
  have he : deriv (logLik x a) = heldScore x a := funext fun b ↦ (hd b).deriv
  rw [he]
  exact heldScore_strictAnti x hx hs ha

theorem profile_strictConcave (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) : StrictConcaveOn ℝ univ (profile x) := by
  have hd (b : ℝ) := profile_deriv x hx hs b
  apply StrictAnti.strictConcaveOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun b ↦ (hd b).continuousAt)
  have he : deriv (profile x) = profileScore x := funext fun b ↦ (hd b).deriv
  rw [he]
  exact profileScore_strictAnti hn x hx hs

theorem scores_zero (x : Fin n → ℝ) (a : ℝ) :
    heldScore x a 0 = sampleSum x - a * (squareSum x / 2) ∧
      profileScore x 0 = sampleSum x - (n : ℝ) * (squareSum x / (2 * sampleSum x)) := by
  have H : moment x 1 0 = squareSum x / 2 :=
    (moment_deriv x 0 0).unique (hasDerivAt_integrated_zero x)
  simp only [heldScore, profileScore, H, integrated_zero, div_div, and_self]

private theorem rate_exists_iff {f score : ℝ → ℝ}
    (hd : ∀ b, HasDerivAt f (score b) b) (hc : Continuous score) (ha : StrictAnti score)
    (hneg : ∃ b > 0, score b < 0) :
    (∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn f (Ioi 0) b) ↔ 0 < score 0 := by
  constructor
  · rintro ⟨b, ⟨hb, hmax⟩, _⟩
    have hz : score b = 0 := (hd b).deriv.symm.trans
      (hmax.isLocalMax (Ioi_mem_nhds hb)).deriv_eq_zero
    simpa [hz] using ha hb
  · intro h0
    obtain ⟨b, hb, hsb⟩ := hneg
    obtain ⟨a, ha', hsa⟩ := (Filter.Eventually.and (f := 𝓝[>] (0 : ℝ)) self_mem_nhdsWithin
      (hc.continuousAt.continuousWithinAt.tendsto.eventually (Ioi_mem_nhds h0))).exists
    have hab : a ≤ b := le_of_not_gt fun h ↦ (ha h).not_gt (hsb.trans hsa)
    obtain ⟨⟨r, hr, hu⟩, hmax⟩ := Generic.existsUnique_score_zero_of_signs
      (convex_Ioi 0) (fun t _ ↦ hd t) hc.continuousOn (ha.strictAntiOn _) ha' hb hab hsa hsb
    exact ⟨r, ⟨hr.1, (hmax r hr.1).mpr hr.2⟩, fun t ht ↦ hu t ⟨ht.1, (hmax t ht.1).mp ht.2⟩⟩

private theorem rate_zero_face {f score : ℝ → ℝ}
    (hd : ∀ b, HasDerivAt f (score b) b) (ha : StrictAnti score) (h0 : score 0 ≤ 0) :
    Tendsto f (𝓝[>] 0) (𝓝 (f 0)) ∧ (∀ b > 0, f b < f 0) ∧ sSup (f '' Ioi 0) = f 0 := by
  have hcont : Continuous f := continuous_iff_continuousAt.mpr fun b ↦ (hd b).continuousAt
  have hm : StrictAntiOn f (Ici 0) := by
    apply strictAntiOn_of_deriv_neg (convex_Ici 0) hcont.continuousOn
    intro b hb
    rw [interior_Ici] at hb
    rw [(hd b).deriv]
    exact (ha hb).trans_le h0
  have hlt (b : ℝ) (hb : 0 < b) : f b < f 0 := hm le_rfl hb.le hb
  have hl := hcont.continuousAt (x := (0 : ℝ)) |>.continuousWithinAt (s := Ioi 0) |>.tendsto
  have hub : f 0 ∈ upperBounds (f '' Ioi 0) := by
    rintro _ ⟨b, hb, rfl⟩
    exact (hlt b hb).le
  exact ⟨hl, hlt, le_antisymm (csSup_le ⟨_, ⟨1, one_pos, rfl⟩⟩ hub)
    (Generic.limit_le_sSup (l := 𝓝[>] (0 : ℝ)) (p := id) self_mem_nhdsWithin hl ⟨_, hub⟩)⟩

private theorem heldScore_neg (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) :
    ∃ b > 0, heldScore x a b < 0 := by
  let c := moment x 2 0
  have hc : 0 < c := moment_pos x hx hs 2 0
  let b := (sampleSum x + 1) / (a * c)
  have hb : 0 < b := div_pos (by linarith) (mul_pos ha hc)
  have hle (i : Fin n) : b * (∫ t in (0 : ℝ)..x i, t ^ 2 * exp ((0 : ℝ) * t)) ≤
      ∫ t in (0 : ℝ)..x i, t ^ 1 * exp (b * t) := by
    rw [← intervalIntegral.integral_const_mul]
    apply integral_mono_on (hx i)
    · have hcont : Continuous (fun t : ℝ ↦ b * (t ^ 2 * exp (0 * t))) := by fun_prop
      exact hcont.intervalIntegrable _ _
    · exact (by fun_prop : Continuous (fun t : ℝ ↦ t ^ 1 * exp (b * t))).intervalIntegrable _ _
    · intro t ht
      have he := (le_add_of_nonneg_right (a := b * t) zero_le_one).trans (add_one_le_exp (b * t))
      simpa [pow_two, mul_assoc, mul_left_comm] using mul_le_mul_of_nonneg_left he ht.1
  have H := Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦ hle i)
  rw [← Finset.mul_sum] at H
  change b * c ≤ moment x 1 b at H
  have he : a * (b * c) = sampleSum x + 1 := by
    dsimp [b]
    field_simp
  refine ⟨b, hb, ?_⟩
  unfold heldScore
  linarith only [mul_le_mul_of_nonneg_left H ha.le, he]

private theorem heldScore_continuous (x : Fin n → ℝ) (a : ℝ) : Continuous (heldScore x a) := by
  exact continuous_const.sub (continuous_const.mul
    (continuous_iff_continuousAt.mpr fun b ↦ (moment_deriv x 1 b).continuousAt))

private theorem profileScore_continuous (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) : Continuous (profileScore x) := by
  have H (k : ℕ) : Continuous (moment x k) :=
    continuous_iff_continuousAt.mpr fun b ↦ (moment_deriv x k b).continuousAt
  exact continuous_const.sub (continuous_const.mul ((H 1).div (H 0)
    (fun b ↦ (integrated_pos x hx hs b).ne')))

/-- For held positive coefficients the density product is the exponential of the log likelihood. -/
theorem heldPair_likelihood (x : Fin n → ℝ) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (∏ i, a * exp (b * x i - a * ((exp (b * x i) - 1) / b))) = exp (logLik x a b) := by
  rw [Finset.prod_mul_distrib, ← exp_sum, Finset.prod_const, Finset.card_univ, Fintype.card_fin,
    Finset.sum_sub_distrib, ← Finset.mul_sum, ← Finset.mul_sum, logLik, integrated_eq x hb.ne',
    sampleSum]
  have he : a ^ n = exp ((n : ℝ) * log a) := by rw [exp_nat_mul, exp_log ha]
  rw [he, ← exp_add]
  congr 1
  ring

theorem heldHazard_iff (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) :
    (∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x a) (Ioi 0) b) ↔
      a * squareSum x / 2 < sampleSum x := by
  rw [rate_exists_iff (held_deriv x a) (heldScore_continuous x a)
    (heldScore_strictAnti x hx hs ha) (heldScore_neg x hx hs ha),
    (scores_zero x a).1, sub_pos, mul_div_assoc]

theorem heldHazard_face (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) (h : sampleSum x ≤ a * squareSum x / 2) :
    Tendsto (logLik x a) (𝓝[>] 0) (𝓝 (logLik x a 0)) ∧
      (∀ b > 0, logLik x a b < logLik x a 0) ∧
      sSup (logLik x a '' Ioi 0) = logLik x a 0 := by
  apply rate_zero_face (held_deriv x a) (heldScore_strictAnti x hx hs ha)
  rwa [(scores_zero x a).1, sub_nonpos, ← mul_div_assoc]

theorem zero_logLik (x : Fin n → ℝ) (hx : ∀ i, x i = 0) (a b : ℝ) :
    logLik x a b = (n : ℝ) * log a := by
  simp [logLik, integrated, moment, sampleSum, hx]

theorem heldHazard_zero (x : Fin n → ℝ) (hx : ∀ i, x i = 0) (a b : ℝ) :
    IsMaxOn (logLik x a) (Ioi 0) b := by
  intro t _
  change logLik x a t ≤ logLik x a b
  rw [zero_logLik x hx, zero_logLik x hx]

theorem heldRate_zero (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, x i = 0) (b : ℝ) :
    Tendsto (fun a ↦ logLik x a b) atTop atTop := by
  simp_rw [zero_logLik x hx]
  exact tendsto_log_atTop.const_mul_atTop (by exact_mod_cast hn)

theorem heldRate_iff (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) (b : ℝ) :
    (∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun u ↦ logLik x u b) (Ioi 0) a) ↔
      0 < sampleSum x := by
  constructor
  · rintro ⟨a, ⟨ha, hmax⟩, _⟩
    by_contra! hs
    have hz (i : Fin n) : x i = 0 := le_antisymm (le_of_not_gt (fun hi ↦
      hs.not_gt ((sampleSum_pos_iff x hx).mpr ⟨i, hi⟩))) (hx i)
    obtain ⟨u, hu, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      ((heldRate_zero hn x hz b).eventually (eventually_gt_atTop (logLik x a b)))).exists
    exact hgt.not_ge (hmax (show u ∈ Ioi (0 : ℝ) from hu))
  · intro hs
    obtain ⟨hp, h⟩ := heldRate hn x hx hs b
    exact ⟨hazard x b, ⟨hp, (h _ hp).mpr rfl⟩, fun a ha ↦ (h a ha.1).mp ha.2⟩

private theorem moment_ratio (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {b : ℝ} (hb : 0 < b) (m : ℝ) :
    moment x 1 b / integrated x b =
      (∑ i, x i * exp (b * (x i - m))) /
        (∑ i, (exp (b * (x i - m)) - exp (b * (-m)))) - 1 / b := by
  let E := fun t : ℝ ↦ ∑ i, (exp (t * x i) - 1)
  have hE : HasDerivAt E (∑ i, x i * exp (b * x i)) b := by
    apply HasDerivAt.fun_sum
    intro i _
    simpa only [id_eq, one_mul, mul_comm (exp (b * x i))] using
      (((hasDerivAt_id b).mul_const (x i)).exp.sub_const 1)
  have hIe : integrated x b = E b / b := by
    change integrated x b = (∑ i, (exp (b * x i) - 1)) / b
    rw [integrated_eq x hb.ne', Finset.sum_div]
  have hI := (hE.div (hasDerivAt_id b) hb.ne').congr_of_eventuallyEq
    (show integrated x =ᶠ[𝓝 b] fun t ↦ E t / id t from by
      filter_upwards [Ioi_mem_nhds hb] with t ht
      change integrated x t = (∑ i, (exp (t * x i) - 1)) / t
      rw [integrated_eq x (show 0 < t from ht).ne', Finset.sum_div])
  have hm := (moment_deriv x 0 b).unique hI
  change moment x 1 b = ((∑ i, x i * exp (b * x i)) * b - E b * 1) / b ^ 2 at hm
  have hEb : 0 < E b := by
    exact (div_pos_iff_of_pos_right hb).mp (hIe ▸ integrated_pos x hx hs b)
  have hw (i : Fin n) : exp (b * (x i - m)) = exp (b * x i) * exp (b * (-m)) := by
    rw [← exp_add]
    congr 1
    ring
  simp_rw [hw, ← sub_one_mul, ← mul_assoc, ← Finset.sum_mul]
  change _ = ((∑ i, x i * exp (b * x i)) * exp (b * (-m))) /
    (E b * exp (b * (-m))) - 1 / b
  rw [hm, hIe]
  have he : exp (b * (-m)) ≠ 0 := (exp_pos _).ne'
  field_simp
  ring_nf

/-- At large rate only observations at the attained maximum contribute to the normalized sums. -/
theorem profileScore_atTop (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {m : ℝ} (hmax : ∀ i, x i ≤ m) (hm : ∃ j, x j = m) :
    Tendsto (profileScore x) atTop (𝓝 (sampleSum x - (n : ℝ) * m)) := by
  obtain ⟨j, hj⟩ := hm
  obtain ⟨k, hk⟩ := (sampleSum_pos_iff x hx).mp hs
  have hm' := hk.trans_le (hmax k)
  let l := fun i : Fin n ↦ if x i = m then (1 : ℝ) else 0
  have hweight (i : Fin n) :
      Tendsto (fun b : ℝ ↦ exp (b * (x i - m))) atTop (𝓝 (l i)) := by
    by_cases hi : x i = m
    · simp [l, hi]
    · simp only [l, ite_eq_right hi]
      exact tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg
        (sub_neg.mpr (lt_of_le_of_ne (hmax i) hi)))
  have hzero : Tendsto (fun b : ℝ ↦ exp (b * (-m))) atTop (𝓝 0) :=
    tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg (neg_neg_of_pos hm'))
  have hd : Tendsto (fun b : ℝ ↦ ∑ i, (exp (b * (x i - m)) - exp (b * (-m)))) atTop
      (𝓝 (∑ i, l i)) := by
    simpa only [sub_zero] using tendsto_finsetSum Finset.univ (fun i _ ↦ (hweight i).sub hzero)
  have hn : Tendsto (fun b : ℝ ↦ ∑ i, x i * exp (b * (x i - m))) atTop
      (𝓝 (m * ∑ i, l i)) := by
    rw [Finset.mul_sum]
    have he (i : Fin n) : x i * l i = m * l i := by
      by_cases hi : x i = m <;> simp [l, hi]
    simpa only [he] using tendsto_finsetSum Finset.univ (fun i _ ↦ (hweight i).const_mul (x i))
  have hpos : 0 < ∑ i, l i :=
    Finset.sum_pos' (fun i _ ↦ by dsimp [l]; split <;> norm_num)
      ⟨j, Finset.mem_univ _, by simp [l, hj]⟩
  have hinv : Tendsto (fun b : ℝ ↦ 1 / b) atTop (𝓝 0) := by
    simpa only [one_div] using tendsto_inv_atTop_zero
  have H := (((hn.div hd hpos.ne').sub hinv).const_mul (n : ℝ)).const_sub (sampleSum x)
  rw [mul_div_cancel_right₀ _ hpos.ne', sub_zero] at H
  refine H.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
  rw [profileScore, moment_ratio x hx hs hb m]
  rfl

private theorem profile_neg (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (hne : ∃ i j, x i ≠ x j) :
    ∃ b > 0, profileScore x b < 0 := by
  have hne' : (Finset.univ : Finset (Fin n)).Nonempty := by
    obtain ⟨i, _, _⟩ := hne
    exact ⟨i, Finset.mem_univ i⟩
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ x hne'
  have hmax (i : Fin n) : x i ≤ x j := hj i (Finset.mem_univ i)
  obtain ⟨i, hi⟩ : ∃ i, x i < x j := by
    by_contra! H
    obtain ⟨u, v, huv⟩ := hne
    exact huv ((le_antisymm (hmax u) (H u)).trans (le_antisymm (hmax v) (H v)).symm)
  have hh := Finset.sum_lt_sum (s := Finset.univ) (fun i _ ↦ hmax i)
    ⟨i, Finset.mem_univ _, hi⟩
  have hlt : sampleSum x - (n : ℝ) * x j < 0 := by
    apply sub_neg.mpr
    simpa [sampleSum] using hh
  have hlim := profileScore_atTop x hx hs hmax ⟨j, rfl⟩
  exact ((eventually_gt_atTop (0 : ℝ)).and (hlim.eventually (Iio_mem_nhds hlt))).exists

private theorem sum_pos_of_nonconstant (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hne : ∃ i j, x i ≠ x j) : 0 < sampleSum x := by
  apply (sampleSum_pos_iff x hx).mpr
  by_contra! H
  obtain ⟨i, j, h⟩ := hne
  exact h ((le_antisymm (H i) (hx i)).trans (le_antisymm (H j) (hx j)).symm)

theorem profile_iff (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hne : ∃ i j, x i ≠ x j) :
    (∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (profile x) (Ioi 0) b) ↔
      (n : ℝ) * squareSum x < 2 * sampleSum x ^ 2 := by
  have hs := sum_pos_of_nonconstant x hx hne
  rw [rate_exists_iff (profile_deriv x hx hs) (profileScore_continuous x hx hs)
    (profileScore_strictAnti hn x hx hs) (profile_neg x hx hs hne),
    (scores_zero x 1).2, sub_pos, ← mul_div_assoc, div_lt_iff₀ (by positivity)]
  ring_nf

/-- Parameters are stored in rate, initial hazard order for profiling. -/
def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

def joint (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ := logLik x p.2 p.1

theorem joint_argmax (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) :
    {p ∈ domain | IsMaxOn (joint x) domain p} =
      (fun b ↦ (b, hazard x b)) '' {b ∈ Ioi (0 : ℝ) | IsMaxOn (profile x) (Ioi 0) b} := by
  have H := Generic.joint_maximizers_eq_profile_graph (T := Ioi (0 : ℝ)) (F := Ioi 0)
    (L := fun b a ↦ logLik x a b)
    (fun b _ ↦ ⟨(heldRate hn x hx hs b).1, ((heldRate hn x hx hs b).2 _
      (heldRate hn x hx hs b).1).mpr rfl⟩)
    (fun b _ a ha ↦ ((heldRate hn x hx hs b).2 a ha).mp)
  unfold domain joint
  simpa only [profile_eq hn x hx hs] using H

theorem joint_iff (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hne : ∃ i j, x i ≠ x j) :
    (∃! p, p ∈ domain ∧ IsMaxOn (joint x) domain p) ↔
      (n : ℝ) * squareSum x < 2 * sampleSum x ^ 2 := by
  rw [← profile_iff hn x hx hne]
  have hs := sum_pos_of_nonconstant x hx hne
  change (∃! p, p ∈ {p ∈ domain | IsMaxOn (joint x) domain p}) ↔
    (∃! b, b ∈ {b ∈ Ioi (0 : ℝ) | IsMaxOn (profile x) (Ioi 0) b})
  rw [joint_argmax hn x hx hs]
  constructor
  · rintro ⟨p, ⟨b, hb, rfl⟩, hu⟩
    exact ⟨b, hb, fun c hc ↦ congrArg Prod.fst (hu _ (mem_image_of_mem _ hc))⟩
  · rintro ⟨b, hb, hu⟩
    refine ⟨(b, hazard x b), mem_image_of_mem _ hb, ?_⟩
    rintro _ ⟨c, hc, rfl⟩
    rw [hu c hc]

theorem exponential_face (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (h : 2 * sampleSum x ^ 2 ≤ (n : ℝ) * squareSum x) :
    Tendsto (fun b ↦ joint x (b, hazard x b)) (𝓝[>] 0) (𝓝 (logLik x (n / sampleSum x) 0)) ∧
      (∀ p ∈ domain, joint x p < logLik x (n / sampleSum x) 0) ∧
      sSup (joint x '' domain) = logLik x (n / sampleSum x) 0 := by
  have hscore : profileScore x 0 ≤ 0 := by
    rw [(scores_zero x 1).2, sub_nonpos, ← mul_div_assoc, le_div_iff₀ (by positivity)]
    convert h using 1
    ring
  obtain ⟨hlim, hlt, _⟩ := rate_zero_face (profile_deriv x hx hs)
    (profileScore_strictAnti hn x hx hs) hscore
  have hf : logLik x (n / sampleSum x) 0 = profile x 0 := by
    simpa only [hazard, integrated_zero] using profile_eq hn x hx hs 0
  rw [hf]
  have hl : Tendsto (fun b ↦ joint x (b, hazard x b)) (𝓝[>] 0) (𝓝 (profile x 0)) := by
    simpa only [joint, profile_eq hn x hx hs] using hlim
  have hpath : ∀ᶠ b in 𝓝[>] (0 : ℝ), (b, hazard x b) ∈ domain :=
    Filter.Eventually.mono self_mem_nhdsWithin fun b hb ↦
      ⟨hb, (heldRate hn x hx hs b).1⟩
  have hstrict (p : ℝ × ℝ) (hp : p ∈ domain) : joint x p < profile x 0 := by
    have H := ((heldRate hn x hx hs p.1).2 _ (heldRate hn x hx hs p.1).1).mpr rfl
    have H' : joint x p ≤ profile x p.1 := by
      change logLik x p.2 p.1 ≤ profile x p.1
      rw [← profile_eq hn x hx hs]
      exact H (show p.2 ∈ Ioi (0 : ℝ) from hp.2)
    exact H'.trans_lt (hlt p.1 hp.1)
  have hb : profile x 0 ∈ upperBounds (joint x '' domain) := by
    rintro _ ⟨p, hp, rfl⟩
    exact (hstrict p hp).le
  exact ⟨hl, hstrict, le_antisymm (csSup_le ⟨_, ⟨(1, 1), ⟨one_pos, one_pos⟩, rfl⟩⟩ hb)
    (Generic.limit_le_sSup hpath hl ⟨_, hb⟩)⟩

theorem constant_unbounded (hn : 0 < n) (x : Fin n → ℝ) {c : ℝ}
    (hx : ∀ i, x i = c) : ¬ BddAbove (joint x '' domain) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  rintro ⟨M, hM⟩
  obtain ⟨t, ht⟩ := exists_gt ((M + n) / n : ℝ)
  let b := exp t
  have hb : 0 < b := exp_pos _
  have hbig : M + n < t * n := (div_lt_iff₀ hn').mp ht
  by_cases hz : c = 0
  · have H := hM (mem_image_of_mem (joint x) (show (1, b) ∈ domain from ⟨one_pos, hb⟩))
    change logLik x b 1 ≤ M at H
    rw [zero_logLik x (fun i ↦ (hx i).trans hz), show log b = t from log_exp t] at H
    nlinarith only [H, hbig, hn']
  · let a := b * exp (-(b * c))
    have ha : 0 < a := mul_pos hb (exp_pos _)
    have hi : integrated x b = (n : ℝ) * ((exp (b * c) - 1) / b) := by
      simp [integrated_eq x hb.ne', hx]
    have hs : sampleSum x = (n : ℝ) * c := by simp [sampleSum, hx]
    have hprod : a * integrated x b = (n : ℝ) * (1 - exp (-(b * c))) := by
      rw [hi]
      dsimp [a]
      rw [exp_neg]
      have he : exp (b * c) ≠ 0 := (exp_pos _).ne'
      field_simp
    have H := hM (mem_image_of_mem (joint x) (show (b, a) ∈ domain from ⟨hb, ha⟩))
    change logLik x a b ≤ M at H
    rw [logLik, hprod, hs, show log a = t - b * c by
      change log (b * exp (-(b * c))) = _
      rw [log_mul hb.ne' (exp_pos _).ne', show log b = t from log_exp t, log_exp]
      ring] at H
    nlinarith only [H, hbig, mul_pos hn' (exp_pos (-(b * c)))]

end

end UnivariateML.Gompertz
