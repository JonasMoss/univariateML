module

public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Topology.Order.Compact
public import UnivariateML.Generic.Score
public import UnivariateML.Generic.Limit

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Cauchy likelihood

The sample is nonempty and the scale is positive. The balanced two-point
exception in the dossier has a full semicircle of joint maximizers.

## Deviations

The statements use exact real arithmetic. They do not assert success or
coefficient accuracy for any of the floating-point searches. In the all-equal
held-location case the log-scale score is constant, with negative value.
The half-mass theorem distinguishes the finite unattained supremum from the
balanced two-point exception.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators

namespace UnivariateML.Cauchy

noncomputable section

variable {n : ℕ}

def domain : Set (ℝ × ℝ) := univ ×ˢ Ioi 0

def logDensity (m s y : ℝ) : ℝ :=
  log s - log π - log ((m - y)^2 + s^2)

def logLik (x : Fin n → ℝ) (m s : ℝ) : ℝ := ∑ i, logDensity m s (x i)

def multiplicity (x : Fin n → ℝ) (a : ℝ) : ℕ :=
  (Finset.univ.filter fun i ↦ x i = a).card

/-- The reciprocal density divided by π, at positive scale. -/
private def q (m s y : ℝ) : ℝ := ((m - y)^2 + s^2) / s

private def cost (x : Fin n → ℝ) (m s : ℝ) : ℝ := ∑ i, log (q m s (x i))

private theorem q_pos (m y : ℝ) {s : ℝ} (hs : 0 < s) : 0 < q m s y :=
  div_pos (add_pos_of_nonneg_of_pos (sq_nonneg _) (sq_pos_of_pos hs)) hs

private theorem q_ge_scale (m y : ℝ) {s : ℝ} (hs : 0 < s) : s ≤ q m s y := by
  apply (le_div_iff₀ hs).2
  nlinarith [sq_nonneg (m - y)]

private theorem logLik_eq (x : Fin n → ℝ) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    logLik x m s = -(n : ℝ) * log π - cost x m s := by
  have e (y : ℝ) : logDensity m s y = -log π - log (q m s y) := by
    rw [logDensity, q, log_div (by positivity) hs.ne']
    ring
  simp [logLik, cost, e, Finset.sum_sub_distrib]

private theorem continuousOn_cost (x : Fin n → ℝ) :
    ContinuousOn (fun p : ℝ × ℝ ↦ cost x p.1 p.2) domain := by
  apply continuousOn_finsetSum
  intro i _
  have h : ContinuousOn (fun p : ℝ × ℝ ↦ q p.1 p.2 (x i)) domain := by
    unfold q
    apply (by fun_prop : ContinuousOn
      (fun p : ℝ × ℝ ↦ (p.1 - x i)^2 + p.2^2) domain).div continuous_snd.continuousOn
    exact fun p hp ↦ (show 0 < p.2 from hp.2).ne'
  exact h.log fun p hp ↦ (q_pos _ _ hp.2).ne'

private theorem continuous_location (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    Continuous (fun m ↦ cost x m s) := by
  apply continuous_finsetSum
  intro i _
  exact (by unfold q; fun_prop : Continuous (fun m ↦ q m s (x i))).log
    fun m ↦ (q_pos _ _ hs).ne'

private theorem left_improves {x : Fin n → ℝ} (hn : 0 < n) {s a m : ℝ}
    (hs : 0 < s) (ha : ∀ i, a ≤ x i) (hm : m < a) :
    cost x a s < cost x m s := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro i _
  apply log_lt_log (q_pos _ _ hs)
  apply (div_lt_div_iff_of_pos_right hs).2
  have h₁ : 0 < a - m := sub_pos.2 hm
  have h₂ : 0 < 2 * x i - (a + m) := by linarith [ha i]
  nlinarith [mul_pos h₁ h₂]

private theorem right_improves {x : Fin n → ℝ} (hn : 0 < n) {s b m : ℝ}
    (hs : 0 < s) (hb : ∀ i, x i ≤ b) (hm : b < m) :
    cost x b s < cost x m s := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro i _
  apply log_lt_log (q_pos _ _ hs)
  apply (div_lt_div_iff_of_pos_right hs).2
  have h₁ : 0 < m - b := sub_pos.2 hm
  have h₂ : 0 < b + m - 2 * x i := by linarith [hb i]
  nlinarith [mul_pos h₁ h₂]

/-- At positive held scale, the entire argmax is a nonempty compact subset of
the sample range. Maximizing over the sample range gives exactly that set. -/
theorem heldScale_argmax (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    {i j : Fin n} (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    let A := {m | IsMaxOn (fun t ↦ logLik x t s) univ m}
    A.Nonempty ∧ IsCompact A ∧
      A = {m ∈ Icc (x i) (x j) | IsMaxOn (fun t ↦ logLik x t s) (Icc (x i) (x j)) m} := by
  let K := Icc (x i) (x j)
  have hK : K.Nonempty := ⟨x i, le_rfl, hj i⟩
  obtain ⟨a, ha, hmin⟩ := isCompact_Icc.exists_isMinOn hK (continuous_location x hs).continuousOn
  have hclip (m : ℝ) : ∃ t ∈ K, cost x t s ≤ cost x m s := by
    by_cases hl : m < x i
    · exact ⟨x i, ⟨le_rfl, hj i⟩, (left_improves hn hs hi hl).le⟩
    by_cases hr : x j < m
    · exact ⟨x j, ⟨hi j, le_rfl⟩, (right_improves hn hs hj hr).le⟩
    exact ⟨m, ⟨le_of_not_gt hl, le_of_not_gt hr⟩, le_rfl⟩
  have E (m : ℝ) :
      IsMaxOn (fun t ↦ logLik x t s) univ m ↔
        m ∈ K ∧ cost x m s = cost x a s := by
    constructor
    · intro hm
      have H (t : ℝ) : cost x m s ≤ cost x t s := by
        have hh : logLik x t s ≤ logLik x m s := hm (mem_univ t)
        rw [logLik_eq x _ hs, logLik_eq x _ hs] at hh
        linarith
      have hk : m ∈ K := ⟨
        le_of_not_gt fun h ↦ (left_improves hn hs hi h).not_ge (H _),
        le_of_not_gt fun h ↦ (right_improves hn hs hj h).not_ge (H _)⟩
      exact ⟨hk, le_antisymm (H _) (hmin hk)⟩
    · rintro ⟨_, hm⟩ t _
      obtain ⟨z, hz, hzm⟩ := hclip t
      dsimp
      rw [logLik_eq x _ hs, logLik_eq x _ hs, hm]
      have H : cost x a s ≤ cost x z s := hmin hz
      linarith only [hzm, H]
  have E' : {m | IsMaxOn (fun t ↦ logLik x t s) univ m} =
      K ∩ (fun t ↦ cost x t s) ⁻¹' {cost x a s} := Set.ext E
  refine ⟨⟨a, (E a).2 ⟨ha, rfl⟩⟩, ?_, ?_⟩
  · rw [E']
    exact isCompact_Icc.inter_right (isClosed_singleton.preimage (continuous_location x hs))
  · ext m
    change IsMaxOn (fun t ↦ logLik x t s) univ m ↔
      m ∈ K ∧ IsMaxOn (fun t ↦ logLik x t s) K m
    rw [E]
    constructor
    · intro hm
      exact ⟨hm.1, fun t ht ↦ ((E m).2 hm) (mem_univ t)⟩
    · rintro ⟨hm, hmax⟩
      have H : logLik x a s ≤ logLik x m s := hmax ha
      rw [logLik_eq x _ hs, logLik_eq x _ hs] at H
      exact ⟨hm, le_antisymm (by linarith) (hmin hm)⟩

/-- Every nonempty sample has a maximizing location at any held positive scale. -/
theorem heldScale_exists (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    ∃ m, IsMaxOn (fun t ↦ logLik x t s) univ m := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ x Finset.univ_nonempty
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ x Finset.univ_nonempty
  exact (heldScale_argmax hn x hs (fun k ↦ hi k (Finset.mem_univ _))
    (fun k ↦ hj k (Finset.mem_univ _))).1

private def off (x : Fin n → ℝ) (a : ℝ) : Finset (Fin n) :=
  Finset.univ.filter fun i ↦ x i ≠ a

private theorem mem_off {x : Fin n → ℝ} {a : ℝ} {i : Fin n} :
    i ∈ off x a ↔ x i ≠ a := by simp [off]

private theorem count_split (x : Fin n → ℝ) (a : ℝ) :
    multiplicity x a + (off x a).card = n := by
  unfold multiplicity off
  rw [Finset.card_filter_add_card_filter_not, Finset.card_univ, Fintype.card_fin]

private theorem cost_split (x : Fin n → ℝ) (a m s : ℝ) :
    cost x m s = (multiplicity x a : ℝ) * log (q m s a) +
      ∑ i ∈ off x a, log (q m s (x i)) := by
  unfold cost
  rw [← Finset.sum_filter_add_sum_filter_not Finset.univ (fun i ↦ x i = a)]
  congr 1
  rw [multiplicity, ← nsmul_eq_mul, ← Finset.sum_const]
  exact Finset.sum_congr rfl fun i hi ↦ by rw [(Finset.mem_filter.mp hi).2]

/-- The limiting log-likelihood of collapse at a half-mass location. -/
def collapseValue (x : Fin n → ℝ) (a : ℝ) : ℝ :=
  -(n : ℝ) * log π - ∑ i ∈ Finset.univ.filter (fun i ↦ x i ≠ a), log ((a - x i)^2)

private theorem collapse_formula (x : Fin n → ℝ) (a : ℝ) {s : ℝ} (hs : 0 < s) :
    logLik x a s = ((n : ℝ) - 2 * multiplicity x a) * log s -
      (n : ℝ) * log π - ∑ i ∈ off x a, log ((a - x i)^2 + s^2) := by
  rw [logLik_eq x a hs, cost_split x a]
  have ha : q a s a = s := by simp [q, pow_two, hs.ne']
  rw [ha]
  have E (i : Fin n) : log (q a s (x i)) = log ((a - x i)^2 + s^2) - log s := by
    rw [q, log_div (by positivity) hs.ne']
  simp_rw [E]
  rw [Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul]
  have h : (multiplicity x a : ℝ) + ((off x a).card : ℝ) = n := by
    exact_mod_cast count_split x a
  linear_combination log s * h

private theorem collapse_sum_limit (x : Fin n → ℝ) (a : ℝ) :
    Tendsto (fun s : ℝ ↦ ∑ i ∈ off x a, log ((a - x i)^2 + s^2))
      (𝓝[>] 0) (𝓝 (∑ i ∈ off x a, log ((a - x i)^2))) := by
  apply tendsto_finsetSum
  intro i hi
  have h : ContinuousAt (fun s : ℝ ↦ (a - x i)^2 + s^2) 0 := by fun_prop
  have hnz : (a - x i)^2 + (0 : ℝ)^2 ≠ 0 := by
    simp [sub_ne_zero.mpr (Ne.symm (mem_off.mp hi))]
  simpa using (h.log hnz).continuousWithinAt.tendsto

/-- Collapse at a strict-majority atom makes the log-likelihood unbounded. -/
theorem majority_collapse (x : Fin n → ℝ) {a : ℝ} (ha : n < 2 * multiplicity x a) :
    Tendsto (fun s ↦ logLik x a s) (𝓝[>] 0) atTop := by
  have hcoef : (n : ℝ) - 2 * multiplicity x a < 0 := by
    have hh : (n : ℝ) < 2 * multiplicity x a := by exact_mod_cast ha
    linarith
  have hmain := tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg hcoef
  have hrest := (tendsto_const_nhds (x := -(n : ℝ) * log π)).sub
    (collapse_sum_limit x a)
  apply tendsto_atTop.2
  intro b
  let c := collapseValue x a
  filter_upwards [(tendsto_atTop.1 hmain) (b - (c - 1)),
    hrest.eventually (Ioi_mem_nhds (show c - 1 < c by linarith)),
    self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s h₁ h₂ hs
  change c - 1 < -(n : ℝ) * log π - ∑ i ∈ off x a, log ((a - x i)^2 + s^2) at h₂
  rw [collapse_formula x a hs]
  linarith only [h₁, h₂]

/-- A strict-majority atom excludes a finite joint maximizer. -/
theorem majority_no_jointMLE (x : Fin n → ℝ) {a : ℝ} (ha : n < 2 * multiplicity x a) :
    ¬ ∃ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p := by
  rintro ⟨p, _, hp⟩
  have hscale : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
  obtain ⟨s, hs, hgt⟩ := (hscale.and
    ((majority_collapse x ha).eventually (eventually_gt_atTop (logLik x p.1 p.2)))).exists
  exact hgt.not_ge (hp (show (a, s) ∈ domain from ⟨mem_univ _, hs⟩))

/-- Collapse at exactly half the sample mass has a finite log-likelihood limit. -/
theorem half_collapse (x : Fin n → ℝ) {a : ℝ} (ha : 2 * multiplicity x a = n) :
    Tendsto (fun s ↦ logLik x a s) (𝓝[>] 0) (𝓝 (collapseValue x a)) := by
  have he : (n : ℝ) - 2 * multiplicity x a = 0 := by
    have hh : (2 : ℝ) * multiplicity x a = n := by exact_mod_cast ha
    linarith
  change Tendsto _ _ (𝓝 (-(n : ℝ) * log π - ∑ i ∈ off x a, log ((a - x i)^2)))
  refine ((tendsto_const_nhds (x := -(n : ℝ) * log π)).sub
    (collapse_sum_limit x a)).congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
  simp [collapse_formula x a hs, he]

private theorem pair_q (m a b : ℝ) {s : ℝ} (hs : 0 < s) :
    q m s a * q m s b = (a - b)^2 + (((m - a) * (m - b) + s^2) / s)^2 := by
  unfold q
  field_simp [hs.ne']
  ring

private theorem half_cost (x : Fin n → ℝ) {a m s : ℝ} (hs : 0 < s)
    (ha : 2 * multiplicity x a = n) :
    cost x m s = ∑ i ∈ off x a, log (q m s a * q m s (x i)) := by
  rw [cost_split x a]
  have E (i : Fin n) : log (q m s a * q m s (x i)) =
      log (q m s a) + log (q m s (x i)) := log_mul (q_pos m a hs).ne' (q_pos m _ hs).ne'
  simp_rw [E]
  rw [Finset.sum_add_distrib, Finset.sum_const, nsmul_eq_mul]
  have hc := count_split x a
  have hcard : (off x a).card = multiplicity x a := by omega
  rw [hcard]

/-- At half mass the collapse value bounds the log-likelihood. Equality holds
precisely when the parameter pair lies on every semicircle through the atom
and a distinct observed value. -/
theorem half_bound (x : Fin n → ℝ) {a m s : ℝ} (hs : 0 < s)
    (ha : 2 * multiplicity x a = n) :
    logLik x m s ≤ collapseValue x a ∧
    (logLik x m s = collapseValue x a ↔
      ∀ i, x i ≠ a → (m - a) * (m - x i) + s^2 = 0) := by
  have hpos (i : Fin n) (hi : i ∈ off x a) : 0 < (a - x i)^2 :=
    sq_pos_of_ne_zero (sub_ne_zero.mpr (Ne.symm (mem_off.mp hi)))
  have hle (i : Fin n) (hi : i ∈ off x a) :
      log ((a - x i)^2) ≤ log (q m s a * q m s (x i)) := by
    apply log_le_log (hpos i hi)
    rw [pair_q m a _ hs]
    exact le_add_of_nonneg_right (sq_nonneg _)
  have H := Finset.sum_le_sum hle
  have heq : collapseValue x a = -(n : ℝ) * log π - ∑ i ∈ off x a, log ((a - x i)^2) :=
    rfl
  rw [logLik_eq x m hs, half_cost x hs ha, heq]
  refine ⟨by linarith, ?_⟩
  constructor
  · intro he i hi
    by_contra hne
    have hlt : log ((a - x i)^2) < log (q m s a * q m s (x i)) := by
      apply log_lt_log (hpos i (mem_off.mpr hi))
      rw [pair_q m a _ hs]
      exact lt_add_of_pos_right _ (sq_pos_of_ne_zero (div_ne_zero hne hs.ne'))
    have hh := Finset.sum_lt_sum hle ⟨i, mem_off.mpr hi, hlt⟩
    linarith
  · intro h
    congr 1
    apply Finset.sum_congr rfl
    intro i hi
    rw [pair_q m a _ hs, h i (mem_off.mp hi)]
    simp

/-- The half-mass collapse value is the joint supremum, attained exactly on
the common semicircles through the half-mass atom and each other observation. -/
theorem half_supremum (x : Fin n → ℝ) {a : ℝ} (ha : 2 * multiplicity x a = n) :
    sSup ((fun p ↦ logLik x p.1 p.2) '' domain) = collapseValue x a ∧
    ∀ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p ↔
      ∀ i, x i ≠ a → (p.1 - a) * (p.1 - x i) + p.2^2 = 0 := by
  let L := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
  have hb : collapseValue x a ∈ upperBounds (L '' domain) := by
    rintro _ ⟨p, hp, rfl⟩
    exact (half_bound x hp.2 ha).1
  have H : ∀ u ∈ upperBounds (L '' domain), collapseValue x a ≤ u := by
    intro u hu
    exact Generic.limit_le_upperBound (p := fun s : ℝ ↦ (a, s))
      (by filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
          exact ⟨mem_univ _, hs⟩) (half_collapse x ha) hu
  have hnon : (L '' domain).Nonempty := ⟨L (a, 1), (a, 1), ⟨mem_univ _, zero_lt_one⟩, rfl⟩
  refine ⟨le_antisymm (csSup_le hnon hb) (H _ (fun _ hz ↦ le_csSup ⟨_, hb⟩ hz)), ?_⟩
  intro p hp
  rw [← (half_bound x hp.2 ha).2]
  constructor
  · intro hm
    exact le_antisymm (half_bound x hp.2 ha).1 (H _ (by
      rintro _ ⟨w, hw, rfl⟩
      exact hm hw))
  · intro he w hw
    change L w ≤ L p
    rw [show L p = collapseValue x a from he]
    exact (half_bound x hw.2 ha).1

/-- A half-mass atom with two distinct other observed values gives no joint MLE. -/
theorem half_no_jointMLE (x : Fin n → ℝ) {a : ℝ} (ha : 2 * multiplicity x a = n)
    {i j : Fin n} (hi : x i ≠ a) (hj : x j ≠ a) (hij : x i ≠ x j) :
    ¬ ∃ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p := by
  rintro ⟨p, hp, hm⟩
  have h := ((half_supremum x ha).2 p hp).1 hm
  have he : (p.1 - a) * (x j - x i) = 0 := by
    linear_combination h i hi - h j hj
  have hma : p.1 - a = 0 := (mul_eq_zero.mp he).resolve_right (sub_ne_zero.mpr hij.symm)
  have hzero := h i hi
  rw [hma, zero_mul, zero_add] at hzero
  exact (sq_pos_of_pos (show 0 < p.2 from hp.2)).ne' hzero

/-- At an unbalanced half-mass atom, every feasible path approaching the
supremum collapses at that atom, with displacement negligible relative to scale. -/
theorem half_maximizing_path_collapse {ι : Type*} {l : Filter ι} {p : ι → ℝ × ℝ}
    (x : Fin n → ℝ) {a : ℝ} (ha : 2 * multiplicity x a = n)
    {i j : Fin n} (hi : x i ≠ a) (hj : x j ≠ a) (hij : x i ≠ x j)
    (hp : ∀ᶠ z in l, p z ∈ domain)
    (hL : Tendsto (fun z ↦ logLik x (p z).1 (p z).2) l (𝓝 (collapseValue x a))) :
    Tendsto p l (𝓝 (a, 0)) ∧ Tendsto (fun z ↦ ((p z).1 - a) / (p z).2) l (𝓝 0) := by
  let m := fun z ↦ (p z).1
  let s := fun z ↦ (p z).2
  let C := fun k : Fin n ↦ (a - x k)^2
  let D := fun k z ↦ ((m z - a) * (m z - x k) + s z ^ 2) / s z
  let G := fun k z ↦ log (C k + D k z ^ 2) - log (C k)
  have hC (k : Fin n) (hk : k ∈ off x a) : 0 < C k :=
    sq_pos_of_ne_zero (sub_ne_zero.mpr (Ne.symm (mem_off.mp hk)))
  have hG (k : Fin n) (hk : k ∈ off x a) (z : ι) : 0 ≤ G k z :=
    sub_nonneg.mpr (log_le_log (hC k hk) (le_add_of_nonneg_right (sq_nonneg _)))
  have hsum (z : ι) (hz : p z ∈ domain) :
      ∑ k ∈ off x a, G k z = collapseValue x a - logLik x (m z) (s z) := by
    have hs : 0 < s z := hz.2
    have E (k : Fin n) : q (m z) (s z) a * q (m z) (s z) (x k) =
        C k + D k z ^ 2 := pair_q _ _ _ hs
    have hcv : collapseValue x a = -(n : ℝ) * log π - ∑ k ∈ off x a, log (C k) := rfl
    rw [hcv, logLik_eq x _ hs, half_cost x hs ha]
    simp only [G, Finset.sum_sub_distrib, E]
    ring
  have hgap : Tendsto (fun z ↦ collapseValue x a - logLik x (m z) (s z)) l (𝓝 0) := by
    simpa only [sub_self] using hL.const_sub (collapseValue x a)
  have hD (k : Fin n) (hk : k ∈ off x a) : Tendsto (D k) l (𝓝 0) := by
    have hg : Tendsto (G k) l (𝓝 0) := by
      refine tendsto_const_nhds.squeeze' hgap (Eventually.of_forall (hG k hk)) ?_
      filter_upwards [hp] with z hz
      rw [← hsum z hz]
      exact Finset.single_le_sum (fun d hd ↦ hG d hd z) hk
    have hsq : Tendsto (fun z ↦ D k z ^ 2) l (𝓝 0) := by
      have ht := ((hg.add_const (log (C k))).rexp).sub_const (C k)
      simp only [zero_add, exp_log (hC k hk), sub_self] at ht
      apply ht.congr
      intro z
      dsimp [G]
      rw [sub_add_cancel, exp_log (add_pos_of_pos_of_nonneg (hC k hk) (sq_nonneg _))]
      ring
    have habs : Tendsto (fun z ↦ |D k z|) l (𝓝 0) := by
      simpa only [sqrt_sq_eq_abs, sqrt_zero] using hsq.sqrt
    have hneg : Tendsto (fun z ↦ -|D k z|) l (𝓝 0) := by
      simpa only [neg_zero] using habs.neg
    exact hneg.squeeze habs (fun _ ↦ neg_abs_le _) (fun _ ↦ le_abs_self _)
  let r := fun z ↦ (m z - a) / s z
  have hr : Tendsto r l (𝓝 0) := by
    have hne : x j - x i ≠ 0 := sub_ne_zero.mpr hij.symm
    have H := ((hD i (mem_off.mpr hi)).sub (hD j (mem_off.mpr hj))).div_const (x j - x i)
    simp only [sub_self, zero_div] at H
    apply H.congr'
    filter_upwards [hp] with z hz
    have hs : 0 < s z := hz.2
    change (D i z - D j z) / (x j - x i) = r z
    dsimp only [D, r]
    field_simp [hs.ne', hne]
    ring
  have hs : Tendsto s l (𝓝 0) := by
    have hnum := (hD i (mem_off.mpr hi)).sub (hr.mul_const (a - x i))
    have hden := (hr.pow 2).const_add (1 : ℝ)
    simp only [zero_mul, sub_zero] at hnum
    simp only [zero_pow (by decide : (2 : ℕ) ≠ 0), add_zero] at hden
    have H := hnum.div hden one_ne_zero
    simp only [zero_div] at H
    apply H.congr'
    filter_upwards [hp] with z hz
    have hσ : 0 < s z := hz.2
    change (D i z - r z * (a - x i)) / (1 + r z ^ 2) = s z
    apply (div_eq_iff (add_pos_of_pos_of_nonneg one_pos (sq_nonneg _)).ne').2
    dsimp only [D, r]
    field_simp [hσ.ne']
    ring
  have hm : Tendsto m l (𝓝 a) := by
    have H := (hr.mul hs).const_add a
    simp only [mul_zero, add_zero] at H
    apply H.congr'
    filter_upwards [hp] with z hz
    have hσ : 0 < s z := hz.2
    change a + (m z - a) / s z * s z = m z
    rw [div_mul_cancel₀ _ hσ.ne']
    ring
  exact ⟨hm.prodMk_nhds hs, hr⟩

/-- For a balanced two-point sample the entire joint argmax is the open
upper semicircle with those endpoints. -/
theorem balanced_joint_argmax (hn : 0 < n) (x : Fin n → ℝ) {a b : ℝ} (hab : a < b)
    (ha : 2 * multiplicity x a = n) (hx : ∀ i, x i = a ∨ x i = b) :
    {p ∈ domain | IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p} =
      {p : ℝ × ℝ | a < p.1 ∧ p.1 < b ∧ 0 < p.2 ∧ p.2^2 = (p.1 - a) * (b - p.1)} := by
  have hb : ∃ i, x i = b := by
    by_contra! h
    have he : ∀ i, x i = a := fun i ↦ (hx i).resolve_right (h i)
    simp [multiplicity, he] at ha
    omega
  obtain ⟨i, hi⟩ := hb
  ext p
  change (p ∈ domain ∧ _) ↔ _
  constructor
  · rintro ⟨hp, hm⟩
    have H := ((half_supremum x ha).2 p hp).1 hm i (hi ▸ hab.ne')
    rw [hi] at H
    have hs : 0 < p.2 := hp.2
    have he : p.2^2 = (p.1 - a) * (b - p.1) := by linear_combination H
    have hprod : 0 < (p.1 - a) * (b - p.1) := he ▸ sq_pos_of_pos hs
    rcases mul_pos_iff.mp hprod with h | h
    · exact ⟨sub_pos.mp h.1, sub_pos.mp h.2, hs, he⟩
    · linarith only [h.1, h.2, hab]
  · rintro ⟨_, _, hs, he⟩
    have hp : p ∈ domain := ⟨mem_univ _, hs⟩
    refine ⟨hp, ((half_supremum x ha).2 p hp).2 (fun j hj ↦ ?_)⟩
    rw [(hx j).resolve_left hj]
    linear_combination he

private def scoreTerm (r t : ℝ) : ℝ := 2 * (r^2 / (r^2 + exp t ^ 2)) - 1

/-- The score in log scale at held location. -/
def heldScore (x : Fin n → ℝ) (m t : ℝ) : ℝ :=
  ∑ i, (2 * ((m - x i)^2 / ((m - x i)^2 + exp t ^ 2)) - 1)

private theorem heldScore_eq (x : Fin n → ℝ) (m t : ℝ) :
    heldScore x m t = ∑ i, scoreTerm (m - x i) t := rfl

private theorem heldScore_fun (x : Fin n → ℝ) (m : ℝ) :
    heldScore x m = fun t ↦ ∑ i, scoreTerm (m - x i) t := rfl

private theorem denom_pos (r t : ℝ) : 0 < r^2 + exp t ^ 2 :=
  add_pos_of_nonneg_of_pos (sq_nonneg _) (sq_pos_of_pos (exp_pos _))

private theorem held_deriv (x : Fin n → ℝ) (m t : ℝ) :
    HasDerivAt (fun u ↦ logLik x m (exp u)) (heldScore x m t) t := by
  simp only [logLik, heldScore_eq]
  apply HasDerivAt.fun_sum
  intro i _
  let r := m - x i
  have H := ((hasDerivAt_id t).sub_const (log π)).sub
    ((((hasDerivAt_exp t).pow 2).const_add (r^2)).log (denom_pos r t).ne')
  simp only [id_eq, Nat.cast_ofNat, Pi.pow_apply, Pi.sub_def] at H
  have E : (1 - (2 * exp t ^ (2 - 1) * exp t) /
      (r^2 + exp t ^ 2)) = scoreTerm r t := by
    unfold scoreTerm
    norm_num
    field_simp [(denom_pos r t).ne']
    ring
  rw [E] at H
  simpa only [logDensity, log_exp, r] using H

private theorem score_continuous (x : Fin n → ℝ) (m : ℝ) :
    Continuous (heldScore x m) := by
  rw [heldScore_fun]
  apply continuous_finsetSum
  intro i _
  unfold scoreTerm
  exact (continuous_const.mul (continuous_const.div
    (by fun_prop) (fun t ↦ (denom_pos (m - x i) t).ne'))).sub continuous_const

private theorem term_anti {t u : ℝ} (htu : t < u) (r : ℝ) :
    scoreTerm r u ≤ scoreTerm r t := by
  have h := (sq_lt_sq₀ (exp_pos t).le (exp_pos u).le).2 (exp_lt_exp.mpr htu)
  unfold scoreTerm
  gcongr

private theorem term_strict_anti {t u r : ℝ} (htu : t < u) (hr : r ≠ 0) :
    scoreTerm r u < scoreTerm r t := by
  have h := (sq_lt_sq₀ (exp_pos t).le (exp_pos u).le).2 (exp_lt_exp.mpr htu)
  have hsq := sq_pos_of_ne_zero hr
  unfold scoreTerm
  apply sub_lt_sub_right
  apply mul_lt_mul_of_pos_left _ (by norm_num)
  apply (div_lt_div_iff₀ (denom_pos r u) (denom_pos r t)).2
  exact mul_lt_mul_of_pos_left (add_lt_add_right h _) hsq

private theorem score_atBot (x : Fin n → ℝ) (m : ℝ) :
    Tendsto (heldScore x m) atBot (𝓝 ((n : ℝ) - 2 * multiplicity x m)) := by
  let lim := fun i : Fin n ↦ if x i = m then (-1 : ℝ) else 1
  have h (i : Fin n) : Tendsto (scoreTerm (m - x i)) atBot (𝓝 (lim i)) := by
    by_cases he : x i = m
    · have hzero : scoreTerm 0 = fun _ ↦ -1 := by
        funext t
        simp [scoreTerm]
      simp only [lim, he, ite_true, sub_self, hzero]
      exact tendsto_const_nhds
    · let r := m - x i
      have hr : r^2 ≠ 0 := (sq_pos_of_ne_zero (sub_ne_zero.mpr (Ne.symm he))).ne'
      have hden := (tendsto_const_nhds (x := r^2)).add (tendsto_exp_atBot.pow 2)
      simp only [zero_pow (by decide : (2 : ℕ) ≠ 0), add_zero] at hden
      have H := ((tendsto_const_nhds (x := r^2)).div hden hr).const_mul (2 : ℝ)
      have hlim : (2 : ℝ) * (r^2 / r^2) - 1 = 1 := by rw [div_self hr]; norm_num
      have H' := H.sub_const (1 : ℝ)
      rw [hlim] at H'
      unfold scoreTerm
      simpa only [lim, ite_eq_right he, r, Pi.div_apply] using H'
  have H := tendsto_finsetSum Finset.univ (fun i _ ↦ h i)
  have E : ∑ i, lim i = (n : ℝ) - 2 * multiplicity x m := by
    rw [← Finset.sum_filter_add_sum_filter_not Finset.univ (fun i ↦ x i = m)]
    have h₁ : (∑ i ∈ Finset.univ.filter (fun i ↦ x i = m), lim i) =
        -(multiplicity x m : ℝ) := by
      calc
        _ = ∑ _ ∈ Finset.univ.filter (fun i ↦ x i = m), (-1 : ℝ) :=
          Finset.sum_congr rfl fun i hi ↦ by simp [lim, (Finset.mem_filter.mp hi).2]
        _ = _ := by simp [multiplicity]
    have h₂ : (∑ i ∈ off x m, lim i) = ((off x m).card : ℝ) := by
      calc
        _ = ∑ _ ∈ off x m, (1 : ℝ) := Finset.sum_congr rfl fun i hi ↦ by simp [lim, mem_off.mp hi]
        _ = _ := by simp
    rw [h₁]
    change -((multiplicity x m : ℕ) : ℝ) + (∑ i ∈ off x m, lim i) = _
    rw [h₂]
    have hc : (multiplicity x m : ℝ) + ((off x m).card : ℝ) = n := by
      exact_mod_cast count_split x m
    linarith
  rw [E] at H
  simpa only [heldScore_fun] using H

private theorem score_atTop (x : Fin n → ℝ) (m : ℝ) :
    Tendsto (heldScore x m) atTop (𝓝 (-(n : ℝ))) := by
  have h (r : ℝ) : Tendsto (scoreTerm r) atTop (𝓝 (-1 : ℝ)) := by
    have he : Tendsto (fun t : ℝ ↦ exp (-t)) atTop (𝓝 0) :=
      tendsto_exp_atBot.comp tendsto_neg_atTop_atBot
    have ht := (he.pow 2).const_mul (r^2)
    simp only [zero_pow (by decide : (2 : ℕ) ≠ 0), mul_zero] at ht
    have hden := ht.add (tendsto_const_nhds (x := (1 : ℝ)))
    simp only [zero_add] at hden
    have H := ((ht.div hden one_ne_zero).const_mul (2 : ℝ)).sub_const (1 : ℝ)
    simp only [zero_div, mul_zero, zero_sub] at H
    refine H.congr fun t ↦ ?_
    simp only [Pi.div_apply, exp_neg, scoreTerm]
    have hd := (denom_pos r t).ne'
    have he := (exp_pos t).ne'
    have hh : r^2 * (exp t)⁻¹ ^ 2 + 1 ≠ 0 :=
      (add_pos_of_nonneg_of_pos (mul_nonneg (sq_nonneg _) (sq_nonneg _)) one_pos).ne'
    field_simp
  simpa [heldScore_fun] using (tendsto_finsetSum Finset.univ (fun i _ ↦ h (m - x i)))

/-- If the held location has strictly less than half the sample mass, the
scale is uniquely determined by the zero of its log-scale score. -/
theorem heldLocation_unique (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    (hm : 2 * multiplicity x m < n) :
    (∃! s, 0 < s ∧ heldScore x m (log s) = 0) ∧
      ∀ s > 0, IsMaxOn (logLik x m) (Ioi 0) s ↔ heldScore x m (log s) = 0 := by
  have hother : ∃ i, x i ≠ m := by
    by_contra! h
    simp [multiplicity, h] at hm
    omega
  obtain ⟨i, hi⟩ := hother
  apply Generic.existsUnique_log_score_zero (held_deriv x m) (score_continuous x m)
    (fun t u htu ↦ by
      simp only [heldScore_eq]
      exact Finset.sum_lt_sum (fun j _ ↦ term_anti htu (m - x j))
        ⟨i, Finset.mem_univ _, term_strict_anti htu (sub_ne_zero.mpr hi.symm)⟩)
    _ _ (score_atBot x m) (score_atTop x m)
  · have h : (2 : ℝ) * multiplicity x m < n := by exact_mod_cast hm
    linarith
  · exact neg_neg_of_pos (by exact_mod_cast hn)

/-- If at least half the sample equals the held location, the log-likelihood
strictly decreases with scale and no positive scale maximizes it. -/
theorem heldLocation_noMLE (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    (hm : n ≤ 2 * multiplicity x m) :
    StrictAntiOn (logLik x m) (Ioi 0) ∧
      ¬ ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (logLik x m) (Ioi 0) s := by
  have hscore (t : ℝ) : heldScore x m t < 0 := by
    by_cases h : ∃ i, x i ≠ m
    · obtain ⟨i, hi⟩ := h
      have hle : heldScore x m (t - 1) ≤ (n : ℝ) - 2 * multiplicity x m := by
        apply ge_of_tendsto (score_atBot x m)
        filter_upwards [eventually_le_atBot (t - 2)] with u hu
        have h' : u < t - 1 := by linarith
        simp only [heldScore_eq]
        exact Finset.sum_le_sum fun j _ ↦ term_anti h' (m - x j)
      have hlt : heldScore x m t < heldScore x m (t - 1) := by
        have h' : t - 1 < t := by linarith
        simp only [heldScore_eq]
        exact Finset.sum_lt_sum (fun j _ ↦ term_anti h' (m - x j))
          ⟨i, Finset.mem_univ _, term_strict_anti h' (sub_ne_zero.mpr hi.symm)⟩
      have h' : (n : ℝ) ≤ 2 * multiplicity x m := by exact_mod_cast hm
      linarith
    · push Not at h
      simpa [heldScore, scoreTerm, h] using (neg_neg_of_pos (by exact_mod_cast hn : (0 : ℝ) < n))
  have H : StrictAntiOn (fun t ↦ logLik x m (exp t)) univ := by
    apply strictAntiOn_of_deriv_neg convex_univ
    · exact (continuous_iff_continuousAt.mpr fun t ↦ (held_deriv x m t).continuousAt).continuousOn
    · intro t _
      rw [(held_deriv x m t).deriv]
      exact hscore t
  have hh : StrictAntiOn (logLik x m) (Ioi 0) := by
    intro s hs t ht hst
    simpa only [exp_log (show 0 < s from hs), exp_log (show 0 < t from ht)] using
      H (mem_univ (log s)) (mem_univ (log t)) (log_lt_log hs hst)
  refine ⟨hh, ?_⟩
  rintro ⟨s, hs, hmax⟩
  have hhalf : s / 2 ∈ Ioi (0 : ℝ) := half_pos hs
  exact (hh hhalf hs (half_lt_self hs)).not_ge (hmax hhalf)

private theorem exists_uniform_gap (x : Fin n → ℝ) :
    ∃ d : ℝ, 0 < d ∧ d ≤ 1 ∧ ∀ i j, x i ≠ x j → 2 * d ≤ |x i - x j| := by
  let g (p : Fin n × Fin n) : ℝ :=
    if x p.1 = x p.2 then 1 else min 1 (|x p.1 - x p.2| / 2)
  have hg (p : Fin n × Fin n) : 0 < g p := by
    unfold g
    split_ifs with h
    · norm_num
    · exact lt_min one_pos (half_pos (abs_pos.mpr (sub_ne_zero.mpr h)))
  by_cases h : (Finset.univ : Finset (Fin n × Fin n)).Nonempty
  · obtain ⟨p, _, hp⟩ := Finset.exists_min_image Finset.univ g h
    have h₁ : g p ≤ 1 := by unfold g; split_ifs <;> simp
    refine ⟨g p, hg p, h₁, fun i j hij ↦ ?_⟩
    have H := hp (i, j) (Finset.mem_univ _)
    simp only [g, ite_eq_right hij] at H
    linarith [H.trans (min_le_right (1 : ℝ) (|x i - x j| / 2))]
  · refine ⟨1, one_pos, le_rfl, fun i j _ ↦ ?_⟩
    exact (h ⟨(i, j), Finset.mem_univ _⟩).elim

-- Off the near atom, each residual contributes at least log(d²) - log(s).
private theorem away_cost {x : Fin n → ℝ} {a m s d : ℝ} (hs : 0 < s) (hs1 : s ≤ 1)
    (hd : 0 < d) (hd1 : d ≤ 1) (ha : 2 * multiplicity x a < n)
    (hfar : ∀ i, x i ≠ a → d ≤ |m - x i|) :
    (n : ℝ) * log (d^2) - log s ≤ cost x m s := by
  rw [cost_split x a]
  have h₀ := log_le_log hs (q_ge_scale m a hs)
  have h₁ (i : Fin n) (hi : i ∈ off x a) :
      log (d^2) - log s ≤ log (q m s (x i)) := by
    rw [← log_div (sq_pos_of_pos hd).ne' hs.ne']
    apply log_le_log (div_pos (sq_pos_of_pos hd) hs)
    apply (div_le_div_iff_of_pos_right hs).2
    have H := (sq_le_sq₀ hd.le (abs_nonneg _)).2 (hfar i (mem_off.mp hi))
    rw [sq_abs] at H
    linarith [sq_nonneg s]
  have hsum := Finset.sum_le_sum h₁
  rw [Finset.sum_const, nsmul_eq_mul] at hsum
  have hc : (multiplicity x a : ℝ) + ((off x a).card : ℝ) = n := by
    exact_mod_cast count_split x a
  have ha' : (2 : ℝ) * multiplicity x a + 1 ≤ n := by
    exact_mod_cast (show 2 * multiplicity x a + 1 ≤ n by omega)
  have hslog := log_nonpos hs.le hs1
  have hdlog : log (d^2) ≤ 0 :=
    log_nonpos (sq_nonneg _) (by nlinarith [mul_nonneg hd.le (sub_nonneg.mpr hd1)])
  have H₁ := mul_le_mul_of_nonneg_left h₀ (Nat.cast_nonneg (α := ℝ) (multiplicity x a))
  have H₂ := mul_nonneg (Nat.cast_nonneg (α := ℝ) (multiplicity x a)) (neg_nonneg.mpr hdlog)
  have H₃ := mul_nonneg (show 0 ≤ (n : ℝ) - 2 * multiplicity x a - 1 by linarith)
    (neg_nonneg.mpr hslog)
  have H₄ : ((multiplicity x a : ℝ) + ((off x a).card : ℝ) - n) *
      (log (d^2) - log s) = 0 := by rw [hc, sub_self, zero_mul]
  nlinarith only [H₁, H₂, H₃, H₄, hsum]

private theorem small_scale_cost (hn : 0 < n) (x : Fin n → ℝ)
    (hx : ∀ a, 2 * multiplicity x a < n) {d : ℝ}
    (hd : 0 < d) (hd1 : d ≤ 1) (hgap : ∀ i j, x i ≠ x j → 2 * d ≤ |x i - x j|)
    (m : ℝ) {s : ℝ} (hs : 0 < s) (hs1 : s ≤ 1) :
    (n : ℝ) * log (d^2) - log s ≤ cost x m s := by
  by_cases hnear : ∃ j, |m - x j| < d
  · obtain ⟨j, hj⟩ := hnear
    apply away_cost hs hs1 hd hd1 (hx (x j))
    intro i hi
    have H := abs_add_le (x i - m) (m - x j)
    rw [show x i - m + (m - x j) = x i - x j by ring, abs_sub_comm (x i) m] at H
    linarith [hgap i j hi]
  · push Not at hnear
    exact away_cost hs hs1 hd hd1 (hx (x ⟨0, hn⟩)) (fun i _ ↦ hnear i)

private theorem q_ge_abs (m y : ℝ) {s : ℝ} (hs : 0 < s) : |m - y| ≤ q m s y := by
  apply (le_div_iff₀ hs).2
  nlinarith [sq_nonneg (|m - y| - s), sq_abs (m - y), mul_nonneg (abs_nonneg (m - y)) hs.le]

set_option maxHeartbeats 800000 in
-- Elaborating the compact rectangle and the pointwise estimates exceeds the default limit.
private theorem joint_exists (hn : 0 < n) (x : Fin n → ℝ)
    (hx : ∀ a, 2 * multiplicity x a < n) :
    ∃ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p := by
  obtain ⟨d, hd, hd1, hgap⟩ := exists_uniform_gap x
  let T := cost x 0 1
  let B := exp (|T| + 1)
  let R := (∑ i, |x i|) + B
  let ε := exp (min 0 ((n : ℝ) * log (d^2) - T - 1))
  have hB : 1 < B := by
    rw [← exp_zero]
    exact exp_lt_exp.mpr (by linarith [abs_nonneg T])
  have hR : 0 < R := by
    exact add_pos_of_nonneg_of_pos (Finset.sum_nonneg fun i _ ↦ abs_nonneg _) (exp_pos _)
  have hε : 0 < ε := exp_pos _
  have hε1 : ε ≤ 1 := by
    rw [← exp_zero]
    exact exp_le_exp.mpr (min_le_left _ _)
  let K := Icc (-R) R ×ˢ Icc ε B
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  have hKsub : K ⊆ domain := fun _ hp ↦ ⟨mem_univ _, hε.trans_le hp.2.1⟩
  have href : (0, 1) ∈ K := ⟨⟨by linarith, hR.le⟩, ⟨hε1, hB.le⟩⟩
  obtain ⟨p, hp, hmin⟩ := hK.exists_isMinOn ⟨_, href⟩ ((continuousOn_cost x).mono hKsub)
  have hT : cost x p.1 p.2 ≤ T := hmin href
  have large (m : ℝ) {s : ℝ} (hs : 0 < s) (h : B ≤ s ∨ R ≤ |m|) : T < cost x m s := by
    have hterm (i : Fin n) : B ≤ q m s (x i) := by
      rcases h with h | h
      · exact h.trans (q_ge_scale m _ hs)
      · have hi : |x i| ≤ ∑ j, |x j| :=
          Finset.single_le_sum (fun j _ ↦ abs_nonneg (x j)) (Finset.mem_univ i)
        have htri := abs_add_le (m - x i) (x i)
        rw [sub_add_cancel] at htri
        exact (show B ≤ |m - x i| by dsimp [R] at h; linarith).trans (q_ge_abs m _ hs)
    have H := Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦ log_le_log (exp_pos _) (hterm i))
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, log_exp] at H
    have hN : (1 : ℝ) ≤ n := by exact_mod_cast hn
    change (n : ℝ) * (|T| + 1) ≤ cost x m s at H
    have H' := mul_le_mul_of_nonneg_right hN (show 0 ≤ |T| + 1 by positivity)
    have H'' := le_abs_self T
    linarith only [H, H', H'']
  refine ⟨p, hKsub hp, ?_⟩
  intro w hw
  have hs : 0 < w.2 := hw.2
  have hc : cost x p.1 p.2 ≤ cost x w.1 w.2 := by
    by_cases h : w ∈ K
    · exact hmin h
    apply hT.trans
    by_cases hlow : w.2 < ε
    · have hlog := log_lt_log hs hlow
      change log w.2 < log (exp _) at hlog
      rw [log_exp] at hlog
      have hmin' := min_le_right (0 : ℝ) ((n : ℝ) * log (d^2) - T - 1)
      have H := small_scale_cost hn x hx hd hd1 hgap w.1 hs (hlow.le.trans hε1)
      linarith only [hlog, hmin', H]
    by_cases hhigh : B < w.2
    · exact (large w.1 hs (Or.inl hhigh.le)).le
    have hfar : R ≤ |w.1| := by
      by_contra! hh
      have H := abs_lt.mp hh
      exact h ⟨⟨H.1.le, H.2.le⟩, ⟨le_of_not_gt hlow, le_of_not_gt hhigh⟩⟩
    exact (large w.1 hs (Or.inr hfar)).le
  change logLik x w.1 w.2 ≤ logLik x p.1 p.2
  rw [logLik_eq x w.1 hs, logLik_eq x p.1 (hKsub hp).2]
  linarith

private theorem three_values (hn : 0 < n) (x : Fin n → ℝ)
    (hx : ∀ a, 2 * multiplicity x a < n) :
    ∃ i j k, x i ≠ x j ∧ x i ≠ x k ∧ x j ≠ x k := by
  let i : Fin n := ⟨0, hn⟩
  have hj : ∃ j, x j ≠ x i := by
    by_contra! h
    have H := hx (x i)
    simp [multiplicity, h] at H
    omega
  obtain ⟨j, hj⟩ := hj
  have hk : ∃ k, x k ≠ x i ∧ x k ≠ x j := by
    by_contra! h
    have he : off x (x i) = Finset.univ.filter (fun k ↦ x k = x j) := by
      ext k
      simp only [mem_off, Finset.mem_filter, Finset.mem_univ, true_and]
      exact ⟨h k, fun hk ↦ hk ▸ hj⟩
    have H := count_split x (x i)
    rw [he] at H
    change multiplicity x (x i) + multiplicity x (x j) = n at H
    have hi := hx (x i)
    have h' := hx (x j)
    omega
  obtain ⟨k, hk₁, hk₂⟩ := hk
  exact ⟨i, j, k, hj.symm, hk₁.symm, hk₂.symm⟩

private theorem quadratic_three {A B C u v w : ℝ}
    (huv : u ≠ v) (huw : u ≠ w) (hvw : v ≠ w)
    (hu : A * u ^ 2 + B * u + C = 0) (hv : A * v ^ 2 + B * v + C = 0)
    (hw : A * w ^ 2 + B * w + C = 0) : A = 0 ∧ B = 0 := by
  have huv' : (u - v) * (A * (u + v) + B) = 0 := by linear_combination hu - hv
  have huw' : (u - w) * (A * (u + w) + B) = 0 := by linear_combination hu - hw
  have h₁ := (mul_eq_zero.mp huv').resolve_left (sub_ne_zero.mpr huv)
  have h₂ := (mul_eq_zero.mp huw').resolve_left (sub_ne_zero.mpr huw)
  have h₃ : A * (v - w) = 0 := by linear_combination h₁ - h₂
  have hA := (mul_eq_zero.mp h₃).resolve_right (sub_ne_zero.mpr hvw)
  exact ⟨hA, by simpa [hA] using h₁⟩

/-- The normalized matrix midpoint improves the log-likelihood of at least
one of any two distinct parameter pairs when the sample has three values. -/
private theorem midpoint_improves {x : Fin n → ℝ}
    (hthree : ∃ i j k, x i ≠ x j ∧ x i ≠ x k ∧ x j ≠ x k)
    {p w : ℝ × ℝ} (hp : p ∈ domain) (hw : w ∈ domain) (hpw : p ≠ w) :
    ∃ z ∈ domain, 2 * logLik x z.1 z.2 > logLik x p.1 p.2 + logLik x w.1 w.2 := by
  rcases p with ⟨m, s⟩
  rcases w with ⟨l, t⟩
  have hs : 0 < s := hp.2
  have ht : 0 < t := hw.2
  let d := l - m
  -- Add the determinant-one quadratic forms of the two points, then
  -- divide by the square root of the determinant of their sum.
  let K := sqrt (((s + t)^2 + d^2) * (s * t))
  let M := (m * t + l * s) / (s + t)
  let S := K / (s + t)
  have hst : 0 < s + t := add_pos hs ht
  have hpos : 0 < ((s + t)^2 + d^2) * (s * t) := by positivity
  have hK : 0 < K := sqrt_pos.mpr hpos
  have hK₂ : K^2 = ((s + t)^2 + d^2) * (s * t) := sq_sqrt hpos.le
  have hS : 0 < S := div_pos hK hst
  let P := fun y : ℝ ↦ d * (m - y)^2 + (d^2 + t^2 - s^2) * (m - y) - d * s^2
  have E (y : ℝ) : q M S y * K = t * ((m - y)^2 + s^2) + s * ((l - y)^2 + t^2) := by
    unfold q M S
    field_simp [hst.ne', hK.ne']
    dsimp [d] at hK₂
    linear_combination hK₂
  -- The geometric-mean comparison has a square as its remainder; that
  -- quadratic cannot vanish at three distinct observations.
  have F (y : ℝ) :
      (q m s y * q l t y - q M S y ^ 2) * K^2 = P y ^ 2 := by
    let U := (m - y)^2 + s^2
    let V := (l - y)^2 + t^2
    calc
      _ = (((s + t)^2 + d^2) * U * V - (t * U + s * V)^2) := by
        rw [sub_mul, ← mul_pow, E, hK₂]
        unfold q U V
        field_simp [hs.ne', ht.ne']
      _ = _ := by dsimp [U, V, P, d]; ring
  have hle (y : ℝ) : q M S y ^ 2 ≤ q m s y * q l t y := by
    have h := F y
    exact sub_nonneg.mp (nonneg_of_mul_nonneg_left (h ▸ sq_nonneg (P y)) (sq_pos_of_pos hK))
  have hlt (y : ℝ) (hy : P y ≠ 0) : q M S y ^ 2 < q m s y * q l t y := by
    have h := F y
    exact sub_pos.mp (pos_of_mul_pos_left (h ▸ sq_pos_of_ne_zero hy) (sq_nonneg K))
  have hne : ∃ i, P (x i) ≠ 0 := by
    by_contra! h
    obtain ⟨i, j, k, hij, hik, hjk⟩ := hthree
    have hu : m - x i ≠ m - x j := fun he ↦ hij (by linarith)
    have hv : m - x i ≠ m - x k := fun he ↦ hik (by linarith)
    have hw : m - x j ≠ m - x k := fun he ↦ hjk (by linarith)
    have he (a : Fin n) : d * (m - x a)^2 + (d^2 + t^2 - s^2) * (m - x a) +
        (-d * s^2) = 0 := by simpa [P, sub_eq_add_neg, neg_mul] using h a
    have H := quadratic_three hu hv hw (he i) (he j) (he k)
    have hml : m = l := by have := H.1; dsimp [d] at this; linarith
    have hes : s = t := by
      have hh := H.2
      rw [H.1] at hh
      have heq : (s - t) * (s + t) = 0 := by linear_combination -hh
      exact sub_eq_zero.mp ((mul_eq_zero.mp heq).resolve_right hst.ne')
    exact hpw (Prod.ext hml hes)
  obtain ⟨i, hi⟩ := hne
  have log_identity (y : ℝ) :
      log (q M S y ^ 2) = 2 * log (q M S y) ∧
      log (q m s y * q l t y) = log (q m s y) + log (q l t y) := by
    exact ⟨by rw [pow_two, log_mul (q_pos M y hS).ne' (q_pos M y hS).ne']; ring,
      log_mul (q_pos m y hs).ne' (q_pos l y ht).ne'⟩
  have hterm (y : ℝ) : 2 * log (q M S y) ≤ log (q m s y) + log (q l t y) := by
    rw [← (log_identity y).1, ← (log_identity y).2]
    exact log_le_log (sq_pos_of_pos (q_pos _ _ hS)) (hle y)
  have hstrict : 2 * log (q M S (x i)) < log (q m s (x i)) + log (q l t (x i)) := by
    rw [← (log_identity _).1, ← (log_identity _).2]
    exact log_lt_log (sq_pos_of_pos (q_pos _ _ hS)) (hlt _ hi)
  have H := Finset.sum_lt_sum (fun j (_ : j ∈ Finset.univ) ↦ hterm (x j))
    ⟨i, Finset.mem_univ _, hstrict⟩
  rw [← Finset.mul_sum, Finset.sum_add_distrib] at H
  change 2 * cost x M S < cost x m s + cost x l t at H
  refine ⟨(M, S), ⟨mem_univ _, hS⟩, ?_⟩
  simp only [logLik_eq x _ hS, logLik_eq x _ hs, logLik_eq x _ ht]
  linarith

/-- The strict half-mass condition gives an attained, unique joint maximum. -/
theorem jointMLE (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ a, 2 * multiplicity x a < n) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p := by
  obtain ⟨p, hp, hm⟩ := joint_exists hn x hx
  refine ⟨p, ⟨hp, hm⟩, fun w hw ↦ ?_⟩
  by_contra hwp
  obtain ⟨z, hz, hbetter⟩ := midpoint_improves (three_values hn x hx) hw.1 hp hwp
  have h₁ := hw.2 hz
  have h₂ := hm hz
  change logLik x z.1 z.2 ≤ logLik x w.1 w.2 at h₁
  change logLik x z.1 z.2 ≤ logLik x p.1 p.2 at h₂
  linarith

private theorem half_other (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ}
    (ha : 2 * multiplicity x a = n) : ∃ i, x i ≠ a := by
  by_contra! h
  simp [multiplicity, h] at ha
  omega

private theorem balanced_point (x : Fin n → ℝ) {a b u : ℝ} (hab : a ≠ b)
    (ha : 2 * multiplicity x a = n) (hx : ∀ i, x i = a ∨ x i = b)
    (hu : 0 < u) (hu' : u < 1) :
    ∃ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p ∧ p.1 = a + u * (b - a) := by
  let m := a + u * (b - a)
  have hpos : 0 < (m - a) * (b - m) := by
    have he : (m - a) * (b - m) = (u * (1 - u)) * (b - a)^2 := by dsimp [m]; ring
    rw [he]
    exact mul_pos (mul_pos hu (sub_pos.mpr hu')) (sq_pos_of_ne_zero (sub_ne_zero.mpr hab.symm))
  let s := sqrt ((m - a) * (b - m))
  have hs : 0 < s := sqrt_pos.mpr hpos
  have hp : (m, s) ∈ domain := ⟨mem_univ _, hs⟩
  refine ⟨(m, s), hp, ((half_supremum x ha).2 _ hp).2 ?_, rfl⟩
  intro i hi
  simp only [(hx i).resolve_left hi, s, sq_sqrt hpos.le]
  ring

/-- A joint MLE exists exactly under the strict half-mass condition or for a
balanced two-point sample. -/
theorem joint_exists_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p) ↔
      (∀ a, 2 * multiplicity x a < n) ∨
        ∃ a b, a ≠ b ∧ 2 * multiplicity x a = n ∧ ∀ i, x i = a ∨ x i = b := by
  constructor
  · intro hp
    by_cases hlt : ∀ a, 2 * multiplicity x a < n
    · exact Or.inl hlt
    push Not at hlt
    obtain ⟨a, ha⟩ := hlt
    have he : 2 * multiplicity x a = n := by
      exact le_antisymm (le_of_not_gt fun h ↦ majority_no_jointMLE x h hp) ha
    obtain ⟨i, hi⟩ := half_other hn x he
    refine Or.inr ⟨a, x i, hi.symm, he, fun j ↦ ?_⟩
    by_cases hj : x j = a
    · exact Or.inl hj
    · exact Or.inr (by_contra fun h ↦ half_no_jointMLE x he hj hi h hp)
  · rintro (hlt | ⟨a, b, hab, ha, hx⟩)
    · exact joint_exists hn x hlt
    · obtain ⟨p, hp, hm, _⟩ := balanced_point x hab ha hx (show 0 < (1 : ℝ) / 2 by norm_num)
        (by norm_num)
      exact ⟨p, hp, hm⟩

/-- A unique joint MLE exists exactly under the strict half-mass condition. -/
theorem jointMLE_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p) ↔
      ∀ a, 2 * multiplicity x a < n := by
  refine ⟨?_, jointMLE hn x⟩
  rintro ⟨p, ⟨hp, hm⟩, hu⟩
  rcases (joint_exists_iff hn x).1 ⟨p, hp, hm⟩ with h | ⟨a, b, hab, ha, hx⟩
  · exact h
  · obtain ⟨v, hv, hvm, h₁⟩ := balanced_point x hab ha hx (show 0 < (1 : ℝ) / 3 by norm_num)
      (by norm_num)
    obtain ⟨w, hw, hwm, h₂⟩ := balanced_point x hab ha hx (show 0 < (2 : ℝ) / 3 by norm_num)
      (by norm_num)
    have he : v.1 = w.1 := congrArg Prod.fst ((hu v ⟨hv, hvm⟩).trans (hu w ⟨hw, hwm⟩).symm)
    exfalso
    apply hab
    linarith only [he, h₁, h₂]

private theorem square_distance_min {v : ℝ} (hv : 0 ≤ v) (e : ℝ) :
    (max 0 e - e)^2 ≤ (v - e)^2 ∧
      ((max 0 e - e)^2 = (v - e)^2 ↔ v = max 0 e) := by
  by_cases h : 0 ≤ e
  · rw [max_eq_right h, sub_self, zero_pow (by decide : (2 : ℕ) ≠ 0)]
    exact ⟨sq_nonneg _, by rw [eq_comm, pow_eq_zero_iff (by decide), sub_eq_zero]⟩
  · have he : e ≤ 0 := le_of_not_ge h
    rw [max_eq_left he, zero_sub]
    have h₁ : 0 ≤ -e := neg_nonneg.mpr he
    have h₂ : 0 ≤ v - e := sub_nonneg.mpr (he.trans hv)
    exact ⟨(sq_le_sq₀ h₁ h₂).2 (by linarith),
      (sq_eq_sq₀ h₁ h₂).trans ⟨fun h' ↦ by linarith, fun h' ↦ by linarith⟩⟩

/-- For a balanced two-point sample, the held-scale argmax consists of the
locations at the stated squared distance from the midpoint. This gives two
locations below half the gap and one at or above half the gap. -/
theorem balanced_heldScale_argmax (hn : 0 < n) (x : Fin n → ℝ) {a b s : ℝ} (hab : a < b)
    (ha : 2 * multiplicity x a = n) (hx : ∀ i, x i = a ∨ x i = b) (hs : 0 < s) :
    {m | IsMaxOn (fun t ↦ logLik x t s) univ m} =
      {m | (m - (a + b) / 2)^2 = max 0 (((b - a) / 2)^2 - s^2)} := by
  let c := (a + b) / 2
  let e := ((b - a) / 2)^2 - s^2
  let G := fun v : ℝ ↦ (a - b)^2 + ((v - e) / s)^2
  have hG (v : ℝ) : 0 < G v :=
    add_pos_of_pos_of_nonneg (sq_pos_of_ne_zero (sub_ne_zero.mpr hab.ne)) (sq_nonneg _)
  have hcard : (0 : ℝ) < (off x a).card := by
    obtain ⟨i, hi⟩ := half_other hn x ha
    exact_mod_cast (Finset.card_pos.mpr ⟨i, mem_off.mpr hi⟩)
  have H (m : ℝ) : cost x m s = (off x a).card * log (G ((m - c)^2)) := by
    rw [half_cost x hs ha, ← nsmul_eq_mul, ← Finset.sum_const]
    apply Finset.sum_congr rfl
    intro i hi
    rw [(hx i).resolve_left (mem_off.mp hi), pair_q m a b hs]
    have he : (m - a) * (m - b) + s^2 = (m - c)^2 - e := by dsimp [c, e]; ring
    rw [he]
  let r := c + sqrt (max 0 e)
  have hr : (r - c)^2 = max 0 e := by
    dsimp [r]
    rw [add_sub_cancel_left, sq_sqrt (le_max_left _ _)]
  have opt (m : ℝ) : cost x r s ≤ cost x m s ∧
      (cost x r s = cost x m s ↔ (m - c)^2 = max 0 e) := by
    have h := square_distance_min (sq_nonneg (m - c)) e
    have hden : 0 < s^2 := sq_pos_of_pos hs
    have hle : G (max 0 e) ≤ G ((m - c)^2) := by
      unfold G
      rw [div_pow, div_pow]
      exact add_le_add_right (div_le_div_of_nonneg_right h.1 hden.le) _
    have heq : G (max 0 e) = G ((m - c)^2) ↔ (m - c)^2 = max 0 e := by
      unfold G
      rw [div_pow, div_pow, add_right_inj]
      constructor
      · intro E
        exact h.2.mp ((div_left_inj₀ hden.ne').mp E)
      · intro E
        rw [E]
    rw [H r, hr, H m]
    exact ⟨mul_le_mul_of_nonneg_left (log_le_log (hG _) hle) hcard.le,
      ⟨fun E ↦ heq.mp (log_injOn_pos (hG _) (hG _) (mul_left_cancel₀ hcard.ne' E)),
        fun E ↦ by rw [heq.mpr E]⟩⟩
  ext m
  change IsMaxOn (fun t ↦ logLik x t s) univ m ↔ (m - c)^2 = max 0 e
  constructor
  · intro h
    have hh : logLik x r s ≤ logLik x m s := h (mem_univ _)
    rw [logLik_eq x r hs, logLik_eq x m hs] at hh
    exact (opt m).2.1 (le_antisymm (opt m).1 (by linarith))
  · intro h t _
    change logLik x t s ≤ logLik x m s
    rw [logLik_eq x t hs, logLik_eq x m hs, ← (opt m).2.2 h]
    linarith only [(opt t).1]

/-- A unique scale MLE exists exactly when the held location has less than
half the sample mass. -/
theorem heldLocation_iff (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    (∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x m) (Ioi 0) s) ↔
      2 * multiplicity x m < n := by
  constructor
  · rintro ⟨s, hs, _⟩
    by_contra! h
    exact (heldLocation_noMLE hn x m h).2 ⟨s, hs⟩
  · intro h
    obtain ⟨⟨s, hs, hu⟩, hscore⟩ := heldLocation_unique hn x m h
    exact ⟨s, ⟨hs.1, (hscore s hs.1).2 hs.2⟩,
      fun t ht ↦ hu t ⟨ht.1, (hscore t ht.1).1 ht.2⟩⟩

end

end UnivariateML.Cauchy
