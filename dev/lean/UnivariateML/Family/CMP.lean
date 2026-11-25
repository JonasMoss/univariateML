module

public import UnivariateML.Generic.ExpFamily
public import Mathlib.Analysis.InnerProductSpace.ProdL2
public import Mathlib.Analysis.Convex.Combination
public import Mathlib.Algebra.Order.BigOperators.GroupWithZero.Finset
public import Mathlib.Analysis.SpecificLimits.Normed
public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Topology.Algebra.InfiniteSum.Real

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Conway–Maxwell–Poisson likelihood

The positive-rate CMP law is written in canonical parameters
`(log λ, -ν)`, with statistic `(x, log (x!))`. A supporting line through
adjacent statistics gives an improving direction that increases dispersion.
The separate zero-rate law is a point mass at zero.

## Deviations

The results concern the jointly free fits for samples on adjacent integers
and constant samples. They do not classify the non-adjacent samples or
held-parameter fits.
-/

@[expose] public section

open Set Filter Real
open scoped RealInnerProductSpace

namespace UnivariateML.CMP

noncomputable section

abbrev Plane := WithLp 2 (ℝ × ℝ)

def logFactorial (k : ℕ) : ℝ := log (k.factorial : ℝ)

def statistic (k : ℕ) : Plane := WithLp.toLp 2 ((k : ℝ), logFactorial k)

def naturalParameter (lam ν : ℝ) : Plane := WithLp.toLp 2 (log lam, -ν)

def weight (η ν : ℝ) (k : ℕ) : ℝ := exp (η * k - ν * logFactorial k)

theorem weight_eq_exp_inner (lam ν : ℝ) (k : ℕ) :
    weight (log lam) ν k = exp ⟪naturalParameter lam ν, statistic k⟫ := by
  simp [weight, naturalParameter, statistic, sub_eq_add_neg, mul_comm]

private theorem logFactorial_succ (k : ℕ) :
    logFactorial (k + 1) = logFactorial k + log ((k : ℝ) + 1) := by
  unfold logFactorial
  rw [Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ,
    log_mul (by positivity) (by exact_mod_cast (Nat.factorial_pos k).ne')]
  ring

private theorem weight_succ (η ν : ℝ) (k : ℕ) :
    weight η ν (k + 1) = weight η ν k * exp (η - ν * log ((k : ℝ) + 1)) := by
  simp only [weight, logFactorial_succ, Nat.cast_succ, ← exp_add]
  congr 1
  ring

/-- Positive dispersion always normalizes. The geometric member normalizes when `η < 0`. -/
theorem summable_weight {η ν : ℝ} (h : 0 < ν ∨ ν = 0 ∧ η < 0) :
    Summable (weight η ν) := by
  rcases h with hν | ⟨rfl, hη⟩
  · obtain ⟨N, hN⟩ := exists_nat_gt (exp ((η + 1) / ν))
    apply summable_of_ratio_norm_eventually_le (r := exp (-1)) (by simp)
    refine eventually_atTop.2 ⟨N, fun k hk ↦ ?_⟩
    have hlog : (η + 1) / ν < log ((k : ℝ) + 1) := (lt_log_iff_exp_lt (by positivity)).2
      (hN.trans_le (by exact_mod_cast Nat.le_succ_of_le hk))
    have he : η - ν * log ((k : ℝ) + 1) ≤ -1 := by
      have := (div_lt_iff₀ hν).mp hlog
      nlinarith
    rw [weight_succ, Real.norm_of_nonneg (by unfold weight; positivity),
      Real.norm_of_nonneg (by unfold weight; positivity), mul_comm (exp (-1))]
    exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr he) (by unfold weight; positivity)
  · rw [show weight η 0 = fun k : ℕ ↦ exp η ^ k by
        funext k
        rw [weight, zero_mul, sub_zero, mul_comm, exp_nat_mul]]
    exact summable_geometric_of_lt_one (exp_pos _).le (exp_lt_one_iff.mpr hη)

private def chordSlope (c : ℕ) : ℝ := log ((c : ℝ) + 1)

private def gap (c k : ℕ) : ℝ :=
  logFactorial k - logFactorial c - chordSlope c * ((k : ℝ) - c)

private theorem gap_succ (c k : ℕ) :
    gap c (k + 1) = gap c k + (chordSlope k - chordSlope c) := by
  simp only [gap, logFactorial_succ, Nat.cast_succ, chordSlope]
  ring

private theorem chordSlope_strictMono : StrictMono chordSlope := by
  intro i j hij
  unfold chordSlope
  apply log_lt_log (by positivity)
  exact_mod_cast Nat.succ_lt_succ hij

private theorem gap_self (c : ℕ) : gap c c = 0 := by simp [gap]

private theorem gap_next (c : ℕ) : gap c (c + 1) = 0 := by simp [gap_succ, gap_self]

private theorem gap_pos_left {k c : ℕ} (h : k < c) : 0 < gap c k := by
  induction c, h using Nat.le_induction with
  | base =>
    have hs := gap_succ (k + 1) k
    rw [gap_self] at hs
    linarith [chordSlope_strictMono (Nat.lt_succ_self k)]
  | succ c h ih =>
    have hshift (x : ℕ) : gap (c + 1) x =
        gap c x + (chordSlope c - chordSlope (c + 1)) * ((x : ℝ) - (c + 1 : ℕ)) := by
      simp only [gap, logFactorial_succ, chordSlope, Nat.cast_succ]
      ring
    rw [hshift]
    have hx : (k : ℝ) < (c + 1 : ℕ) := by exact_mod_cast (show k < c + 1 by omega)
    exact add_pos ih (mul_pos_of_neg_of_neg
      (sub_neg.mpr (chordSlope_strictMono (Nat.lt_succ_self c))) (sub_neg.mpr hx))

private theorem gap_pos_right {k c : ℕ} (h : c + 1 < k) : 0 < gap c k := by
  induction k, h using Nat.le_induction with
  | base =>
    rw [gap_succ, gap_next, zero_add]
    exact sub_pos.mpr (chordSlope_strictMono (Nat.lt_succ_self c))
  | succ k h ih =>
    rw [gap_succ]
    exact add_pos ih (sub_pos.mpr (chordSlope_strictMono (by omega)))

private theorem gap_nonneg (c k : ℕ) : 0 ≤ gap c k := by
  rcases lt_trichotomy k c with h | rfl | h
  · exact (gap_pos_left h).le
  · simp [gap_self]
  · by_cases he : k = c + 1
    · simp [he, gap_next]
    · exact (gap_pos_right (by omega)).le

/-- The affine functional defining the adjacent lower edge is nonnegative on all
statistics, with equality exactly at the two endpoints. -/
theorem adjacent_supporting_line (c k : ℕ) :
    logFactorial c + log ((c : ℝ) + 1) * ((k : ℝ) - c) ≤ logFactorial k ∧
    (logFactorial c + log ((c : ℝ) + 1) * ((k : ℝ) - c) = logFactorial k ↔
      k = c ∨ k = c + 1) := by
  have h := gap_nonneg c k
  unfold gap chordSlope at h
  refine ⟨by linarith, ?_⟩
  constructor
  · intro he
    by_contra! hn
    have hp : 0 < gap c k := by
      by_cases hk : k < c
      · exact gap_pos_left hk
      · exact gap_pos_right (by omega)
    unfold gap chordSlope at hp
    linarith
  · rintro (rfl | rfl)
    · simp
    · have hz := gap_next c
      unfold gap chordSlope at hz
      linarith

/-- Natural-coordinate domain for the positive-rate CMP law. -/
def domain : Set (ℝ × ℝ) :=
  {p | 0 < p.2 ∨ p.2 = 0 ∧ p.1 < 0}

/-- Per-observation log-likelihood as a function of log rate and dispersion. -/
def logLik {n : ℕ} (x : Fin n → ℕ) (p : ℝ × ℝ) : ℝ :=
  (∑ i, (p.1 * x i - p.2 * logFactorial (x i))) / n -
    log (∑' k, weight p.1 p.2 k)

private theorem partition_pos (p : ℝ × ℝ) (hp : p ∈ domain) :
    0 < ∑' k, weight p.1 p.2 k := by
  have hsum := summable_weight hp
  have hterm := hsum.le_tsum 0 (fun i _ ↦ (exp_pos _).le)
  exact (exp_pos _).trans_le hterm

private theorem gap_shift (c : ℕ) (p : ℝ × ℝ) (k : ℕ) :
    (p.1 + chordSlope c) * k - (p.2 + 1) * logFactorial k =
      (p.1 * k - p.2 * logFactorial k) +
        (chordSlope c * c - logFactorial c) - gap c k := by
  unfold gap
  ring

/-- Increasing dispersion by one and adjusting log rate by the adjacent slope
strictly improves the CMP log-likelihood on data supported on that edge. -/
theorem adjacent_improves {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c + 1)
    (p : ℝ × ℝ) (hp : p ∈ domain) :
    ∃ q ∈ domain, logLik x p < logLik x q := by
  let s := chordSlope c
  let b := s * c - logFactorial c
  let q : ℝ × ℝ := (p.1 + s, p.2 + 1)
  have hν : 0 ≤ p.2 := hp.elim (fun h ↦ h.le) (fun h ↦ h.1.ge)
  have hq : q ∈ domain := Or.inl (by change 0 < p.2 + 1; linarith)
  have he (k : ℕ) : weight q.1 q.2 k =
      exp b * weight p.1 p.2 k * exp (-gap c k) := by
    unfold weight
    rw [show q.1 * k - q.2 * logFactorial k =
        p.1 * k - p.2 * logFactorial k + b - gap c k from gap_shift c p k,
      sub_eq_add_neg, exp_add, exp_add, mul_comm (exp b)]
  have hle (k : ℕ) : weight q.1 q.2 k ≤ exp b * weight p.1 p.2 k := by
    rw [he]
    exact mul_le_of_le_one_right (by unfold weight; positivity)
      (exp_le_one_iff.mpr (neg_nonpos.mpr (gap_nonneg c k)))
  have hlt : weight q.1 q.2 (c + 2) < exp b * weight p.1 p.2 (c + 2) := by
    rw [he]
    exact mul_lt_of_lt_one_right (by unfold weight; positivity)
      (exp_lt_one_iff.mpr (neg_neg_of_pos (gap_pos_right (by omega))))
  have hsum := Summable.tsum_lt_tsum_of_nonneg (fun k ↦ (exp_pos _).le) hle hlt
    ((summable_weight hp).mul_left (exp b))
  rw [tsum_mul_left] at hsum
  have hl := log_lt_log (partition_pos q hq) hsum
  rw [log_mul (exp_pos _).ne' (partition_pos p hp).ne', log_exp] at hl
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hdata (i : Fin n) : q.1 * x i - q.2 * logFactorial (x i) =
      p.1 * x i - p.2 * logFactorial (x i) + b := by
    change (p.1 + chordSlope c) * x i - (p.2 + 1) * logFactorial (x i) = _
    rw [gap_shift]
    rcases hx i with hi | hi
    · simp [hi, gap_self, b, s]
    · simp [hi, gap_next, b, s]
  refine ⟨q, hq, ?_⟩
  unfold logLik
  simp_rw [hdata, Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, add_div, mul_div_cancel_left₀ _ hnR]
  linarith

private theorem logLik_le_zero {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (p : ℝ × ℝ) (hp : p ∈ domain) : logLik x p ≤ 0 := by
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  unfold logLik
  apply sub_nonpos.mpr
  apply (div_le_iff₀ hnR).2
  have hterm (i : Fin n) : p.1 * x i - p.2 * logFactorial (x i) ≤
      log (∑' k, weight p.1 p.2 k) := by
    calc
      _ = log (weight p.1 p.2 (x i)) := (log_exp _).symm
      _ ≤ _ := log_le_log (exp_pos _) ((summable_weight hp).le_tsum _ (fun k _ ↦ (exp_pos _).le))
  calc
    _ ≤ ∑ _i : Fin n, log (∑' k, weight p.1 p.2 k) := Finset.sum_le_sum (fun i _ ↦ hterm i)
    _ = _ := by simp [nsmul_eq_mul, mul_comm]

/-- The positive-rate likelihood has a real supremum, and every feasible
point lies strictly below it, for a sample on an adjacent edge. -/
theorem adjacent_unattained_supremum {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c + 1) :
    IsLUB (logLik x '' domain) (sSup (logLik x '' domain)) ∧
      ∀ p ∈ domain, logLik x p < sSup (logLik x '' domain) := by
  have hne : (logLik x '' domain).Nonempty := ⟨logLik x (0, 1), ⟨(0, 1), Or.inl zero_lt_one, rfl⟩⟩
  have hb : BddAbove (logLik x '' domain) := ⟨0, by
    rintro _ ⟨p, hp, rfl⟩
    exact logLik_le_zero hn x p hp⟩
  have hl := isLUB_csSup hne hb
  refine ⟨hl, fun p hp ↦ ?_⟩
  obtain ⟨q, hq, hbetter⟩ := adjacent_improves hn x hx p hp
  exact hbetter.trans_le (hl.1 (mem_image_of_mem _ hq))

/-- The empirical canonical statistic lies on the boundary of the countable
convex support when the observations lie on an adjacent edge. -/
theorem adjacent_mean_mem_frontier {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c + 1) :
    ((n : ℝ)⁻¹ • ∑ i, statistic (x i)) ∈ frontier (convexHull ℝ (range statistic)) := by
  let m : Plane := (n : ℝ)⁻¹ • ∑ i, statistic (x i)
  let C := convexHull ℝ (range statistic)
  let f := (innerSL ℝ (WithLp.toLp 2 (chordSlope c, -1) : Plane)).toLinearMap
  let b := chordSlope c * c - logFactorial c
  have hf (k : ℕ) : f (statistic k) = chordSlope c * k - logFactorial k := by
    change ⟪(WithLp.toLp 2 (chordSlope c, -1) : Plane), statistic k⟫ = _
    simp [statistic, sub_eq_add_neg, mul_comm]
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hC : C ⊆ {p | f p ≤ b} := convexHull_min (by
      rintro _ ⟨k, rfl⟩
      change f (statistic k) ≤ b
      rw [hf]
      have hg := gap_nonneg c k
      unfold gap at hg
      dsimp [b]
      linarith) (convex_halfSpace_le f.isLinear _)
  have hmem : m ∈ C := by
    unfold m
    rw [Finset.smul_sum]
    apply (convex_convexHull ℝ (range statistic)).sum_mem
    · intro i _
      positivity
    · simp [nsmul_eq_mul, hnR]
    · intro i _
      exact subset_convexHull ℝ _ (mem_range_self _)
  have hmean : f m = b := by
    have hi (i : Fin n) : f (statistic (x i)) = b := by
      rw [hf]
      have hg : gap c (x i) = 0 := by rcases hx i with hi | hi <;> simp [hi, gap_self, gap_next]
      unfold gap at hg
      dsimp [b]
      linarith
    simp [m, hi, nsmul_eq_mul, hnR]
  apply (mem_frontier_iff_notMem_interior hmem).2
  intro hint
  obtain ⟨ε, hε, hball⟩ := Metric.mem_nhds_iff.mp (mem_interior_iff_mem_nhds.mp hint)
  let v : Plane := WithLp.toLp 2 (0, -1)
  let q := m + (ε / 2) • v
  have hv : f v = 1 := by
    change ⟪(WithLp.toLp 2 (chordSlope c, -1) : Plane), v⟫ = 1
    simp [v]
  have hvnorm : ‖v‖ = 1 := by simp [v, WithLp.norm_toLp_snd]
  have hq : q ∈ Metric.ball m ε := by
    rw [Metric.mem_ball, dist_eq_norm]
    change ‖m + (ε / 2) • v - m‖ < ε
    rw [add_comm m, add_sub_cancel_right, norm_smul, hvnorm, mul_one,
      Real.norm_eq_abs, abs_of_pos (by positivity)]
    linarith
  have hle : f q ≤ b := hC (hball hq)
  rw [show f q = b + ε / 2 by simp [q, hmean, hv]] at hle
  linarith

/-- The full CMP sample likelihood, with the point mass at rate zero. -/
def likelihood {n : ℕ} (x : Fin n → ℕ) (lam ν : ℝ) : ℝ :=
  if lam = 0 then (if ∀ i, x i = 0 then 1 else 0)
  else ∏ i, weight (log lam) ν (x i) / ∑' k, weight (log lam) ν k

private theorem probability_pos_lt_one (p : ℝ × ℝ) (hp : p ∈ domain) (k : ℕ) :
    0 < weight p.1 p.2 k / (∑' j, weight p.1 p.2 j) ∧
      weight p.1 p.2 k / (∑' j, weight p.1 p.2 j) < 1 := by
  have hpos := partition_pos p hp
  refine ⟨div_pos (exp_pos _) hpos, (div_lt_one hpos).2 ?_⟩
  let g : ℕ → ℝ := fun j ↦ if j = k then weight p.1 p.2 k else 0
  have hsum : (∑' j, g j) = weight p.1 p.2 k := by
    rw [tsum_eq_sum (s := {k}) (by intro j hj; simp [g, show j ≠ k by simpa using hj])]
    simp [g]
  rw [← hsum]
  exact Summable.tsum_lt_tsum_of_nonneg
    (fun j ↦ by by_cases hj : j = k <;> simp [g, hj, weight, exp_nonneg])
    (fun j ↦ by by_cases hj : j = k <;> simp [g, hj, weight, exp_nonneg])
    (i := k + 1) (by simp [g, weight, exp_pos]) (summable_weight hp)

/-- Domain in the original coefficients, including the atom at zero. -/
def rateDomain : Set (ℝ × ℝ) :=
  {p | 0 ≤ p.2 ∧ (p.1 = 0 ∨ 0 < p.1 ∧ (log p.1, p.2) ∈ domain)}

/-- On all-zero data the full likelihood is maximized exactly at rate zero,
with arbitrary feasible dispersion. -/
theorem all_zero_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (hx : ∀ i, x i = 0)
    (p : ℝ × ℝ) (hp : p ∈ rateDomain) :
    IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) rateDomain p ↔ p.1 = 0 := by
  have hpositive (q : ℝ × ℝ) (hlam : 0 < q.1) (hq : (log q.1, q.2) ∈ domain) :
      likelihood x q.1 q.2 < 1 := by
    unfold likelihood
    rw [ite_eq_right hlam.ne']
    have hprod := Finset.prod_lt_prod_of_nonempty₀ (s := Finset.univ)
      (f := fun i : Fin n ↦ weight (log q.1) q.2 (x i) / ∑' k, weight (log q.1) q.2 k)
      (g := fun _ ↦ (1 : ℝ))
      (fun i _ ↦ (probability_pos_lt_one _ hq (x i)).1)
      (fun i _ ↦ (probability_pos_lt_one _ hq (x i)).2)
      ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    simpa using hprod
  constructor
  · intro hm
    rcases hp.2 with hz | ⟨hlam, hq⟩
    · exact hz
    · have hh : likelihood x 0 0 ≤ likelihood x p.1 p.2 :=
        hm (show ((0, 0) : ℝ × ℝ) ∈ rateDomain from ⟨le_rfl, Or.inl rfl⟩)
      have hlt := hpositive p hlam hq
      rw [show likelihood x 0 0 = 1 by simp [likelihood, hx]] at hh
      exact ((not_lt_of_ge hh) hlt).elim
  · intro he q hq
    change likelihood x q.1 q.2 ≤ likelihood x p.1 p.2
    rw [he, show likelihood x 0 p.2 = 1 by simp [likelihood, hx]]
    rcases hq.2 with hz | ⟨hlam, hd⟩
    · simp [likelihood, hz, hx]
    · exact (hpositive q hlam hd).le

private theorem likelihood_eq_exp {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {lam ν : ℝ} (hlam : 0 < lam) (hp : (log lam, ν) ∈ domain) :
    likelihood x lam ν = exp ((n : ℝ) * logLik x (log lam, ν)) := by
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  let z := ∑' k, weight (log lam) ν k
  have hz : 0 < z := partition_pos _ hp
  have he (i : Fin n) : weight (log lam) ν (x i) / z =
      exp (log lam * x i - ν * logFactorial (x i) - log z) := by
    rw [exp_sub, exp_log hz]
    rfl
  unfold likelihood
  rw [ite_eq_right hlam.ne']
  change (∏ i, weight (log lam) ν (x i) / z) = _
  simp_rw [he]
  rw [← exp_sum]
  congr 1
  simp [logLik, z, Finset.sum_sub_distrib, nsmul_eq_mul, mul_sub, mul_div_cancel₀ _ hnR]

/-- A nonzero sample on an adjacent edge has an unattained supremum even when
the zero-rate atom is included. This includes every positive constant sample. -/
theorem adjacent_full_unattained_supremum {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c + 1) (hx0 : ∃ i, x i ≠ 0) :
    IsLUB ((fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) '' rateDomain)
      (sSup ((fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) '' rateDomain)) ∧
    ¬ ∃ p ∈ rateDomain, IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) rateDomain p := by
  let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hz : ¬ ∀ i, x i = 0 := by obtain ⟨i, hi⟩ := hx0; exact fun h ↦ hi (h i)
  have hbase : (1, 1) ∈ rateDomain := ⟨zero_le_one, Or.inr ⟨zero_lt_one, Or.inl zero_lt_one⟩⟩
  have hb : BddAbove (L '' rateDomain) := ⟨1, by
    rintro _ ⟨p, hp, rfl⟩
    change likelihood x p.1 p.2 ≤ 1
    rcases hp.2 with h0 | ⟨hlam, hd⟩
    · simp [likelihood, h0, hz]
    · rw [likelihood_eq_exp hn x hlam hd]
      exact exp_le_one_iff.mpr (mul_nonpos_of_nonneg_of_nonpos hnR.le (logLik_le_zero hn x _ hd))⟩
  refine ⟨isLUB_csSup ⟨L (1, 1), mem_image_of_mem _ hbase⟩ hb, ?_⟩
  rintro ⟨p, hp, hm⟩
  rcases hp.2 with h0 | ⟨hlam, hd⟩
  · have hnpos : 0 < likelihood x 1 1 := by
      rw [likelihood_eq_exp hn x zero_lt_one (Or.inl zero_lt_one)]
      exact exp_pos _
    have hle : likelihood x 1 1 ≤ likelihood x p.1 p.2 := hm hbase
    rw [h0, show likelihood x 0 p.2 = 0 by simp [likelihood, hz]] at hle
    exact (not_lt_of_ge hle) hnpos
  · obtain ⟨q, hq, hbetter⟩ := adjacent_improves hn x hx (log p.1, p.2) hd
    have hqν : 0 ≤ q.2 := hq.elim (fun h ↦ h.le) (fun h ↦ h.1.ge)
    have hq' : (log (exp q.1), q.2) ∈ domain := by simpa using hq
    have hle : likelihood x (exp q.1) q.2 ≤ likelihood x p.1 p.2 :=
      hm (show (exp q.1, q.2) ∈ rateDomain from ⟨hqν, Or.inr ⟨exp_pos _, hq'⟩⟩)
    rw [likelihood_eq_exp hn x (exp_pos _) hq', likelihood_eq_exp hn x hlam hd,
      log_exp] at hle
    exact (not_lt_of_ge hle) (exp_lt_exp.mpr (mul_lt_mul_of_pos_left hbetter hnR))

/-- The finite exponential-family criterion excludes the adjacent edge of a
truncation containing both endpoints and at least three counts. -/
theorem truncated_adjacent_not_mem_intrinsicInterior {K c : ℕ} (hK : 2 ≤ K)
    (hc : c + 1 ≤ K) (u : ℝ) :
    (1 - u) • statistic c + u • statistic (c + 1) ∉
      intrinsicInterior ℝ (convexHull ℝ (range (fun k : Fin (K + 1) ↦ statistic k))) := by
  let T : Fin (K + 1) → Plane := fun k ↦ statistic k
  let m := (1 - u) • statistic c + u • statistic (c + 1)
  let d : Plane := WithLp.toLp 2 (chordSlope c, -1)
  have hd (k : ℕ) : ⟪d, statistic k⟫ = chordSlope c * k - logFactorial k := by
    simp [d, statistic, sub_eq_add_neg, mul_comm]
  have hm : ⟪d, m⟫ = chordSlope c * c - logFactorial c := by
    simp only [m, inner_add_right, real_inner_smul_right, hd]
    have h := gap_next c
    unfold gap at h
    have hendpoint : chordSlope c * ((c + 1 : ℕ) : ℝ) - logFactorial (c + 1) =
        chordSlope c * c - logFactorial c := by
      linarith
    rw [hendpoint]
    ring
  have he (k : ℕ) : ⟪d, statistic k - m⟫ = -gap c k := by
    rw [inner_sub_right, hd, hm]
    unfold gap
    ring
  have hlt : ∃ i : Fin (K + 1), ⟪d, T i - m⟫ < 0 := by
    by_cases h : c + 2 ≤ K
    · refine ⟨⟨c + 2, by omega⟩, ?_⟩
      exact (he (c + 2)).trans_lt (neg_neg_of_pos (gap_pos_right (by omega)))
    · refine ⟨⟨0, by omega⟩, ?_⟩
      exact (he 0).trans_lt (neg_neg_of_pos (gap_pos_left (by omega)))
  intro hi
  obtain ⟨θ, hθ⟩ := (ExpFamily.mle_exists_iff T (h := fun _ ↦ 1) (fun _ ↦ zero_lt_one) m).2 hi
  have hbetter := ExpFamily.improves_in_direction T (h := fun _ ↦ 1) (fun _ ↦ zero_lt_one) m θ d
    (fun i ↦ (he i).trans_le (neg_nonpos.mpr (gap_nonneg c i))) hlt
  exact (not_lt_of_ge (hθ (mem_univ (θ + d)))) hbetter

end

end UnivariateML.CMP
