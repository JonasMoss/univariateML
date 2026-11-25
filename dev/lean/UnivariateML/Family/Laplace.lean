module

public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Data.Fin.Tuple.Sort
public import Mathlib.Order.Filter.Extr

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Laplace likelihood

The location is unrestricted and the scale is positive. Pairing each sorted
observation with its reverse rank gives the exact median interval, including
ties. A strict logarithm inequality gives the unique scale at positive deviation.

## Deviations

The results concern exact real arithmetic for nonempty samples.
Joint uniqueness applies on the domain of existence, excluding constant samples.
-/

@[expose] public section

open Set Real

namespace UnivariateML.Laplace

noncomputable section

variable {n : ℕ}

def logDensity (μ b x : ℝ) : ℝ := -log (2 * b) - |x - μ| / b

def logLik (x : Fin n → ℝ) (μ b : ℝ) : ℝ := ∑ i, logDensity μ b (x i)

def orderStat (x : Fin n → ℝ) (i : Fin n) : ℝ := x (Tuple.sort x i)

def lowerMedian (x : Fin n → ℝ) (hn : 0 < n) : ℝ :=
  orderStat x ⟨(n - 1) / 2, by omega⟩

def upperMedian (x : Fin n → ℝ) (hn : 0 < n) : ℝ :=
  orderStat x ⟨n / 2, by omega⟩

def medianSet (x : Fin n → ℝ) (hn : 0 < n) : Set ℝ :=
  Icc (lowerMedian x hn) (upperMedian x hn)

private def absSum (x : Fin n → ℝ) (μ : ℝ) : ℝ := ∑ i, |x i - μ|

def meanAbsDev (x : Fin n → ℝ) (μ : ℝ) : ℝ := (∑ i, |x i - μ|) / n

def domain : Set (ℝ × ℝ) := univ ×ˢ Ioi 0

def jointMaximizers (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {p ∈ domain | IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p}

private theorem orderStat_mono (x : Fin n → ℝ) : Monotone (orderStat x) :=
  Tuple.monotone_sort x

private theorem lower_le_upper (x : Fin n → ℝ) (hn : 0 < n) :
    lowerMedian x hn ≤ upperMedian x hn :=
  orderStat_mono x (show (n - 1) / 2 ≤ n / 2 by omega)

private theorem lower_mem (hn : 0 < n) (x : Fin n → ℝ) :
    lowerMedian x hn ∈ medianSet x hn := ⟨le_rfl, lower_le_upper x hn⟩

/-- For odd size, the median set is the singleton at the central rank. -/
theorem medianSet_odd (hn : 0 < n) (x : Fin n → ℝ) {m : ℕ} (h : n = 2 * m + 1) :
    medianSet x hn = {orderStat x ⟨m, by omega⟩} := by
  have hl : (n - 1) / 2 = m := by omega
  have hu : n / 2 = m := by omega
  simp [medianSet, lowerMedian, upperMedian, hl, hu]

/-- For even size, the median set is the closed interval between the central ranks. -/
theorem medianSet_even (hn : 0 < n) (x : Fin n → ℝ) {m : ℕ} (h : n = 2 * m) :
    medianSet x hn = Icc (orderStat x ⟨m - 1, by omega⟩) (orderStat x ⟨m, by omega⟩) := by
  have hl : (n - 1) / 2 = m - 1 := by omega
  have hu : n / 2 = m := by omega
  simp [medianSet, lowerMedian, upperMedian, hl, hu]

private theorem pair_bound {a b μ : ℝ} (hab : a ≤ b) :
    b - a ≤ |a - μ| + |b - μ| ∧
      (|a - μ| + |b - μ| = b - a ↔ μ ∈ Icc a b) := by
  rcases le_or_gt a μ with ha | ha
  · rcases le_or_gt μ b with hb | hb
    · rw [abs_of_nonpos (sub_nonpos.mpr ha), abs_of_nonneg (sub_nonneg.mpr hb)]
      have he : -(a - μ) + (b - μ) = b - a := by ring
      exact ⟨he.ge, iff_of_true he ⟨ha, hb⟩⟩
    · rw [abs_of_nonpos (sub_nonpos.mpr ha), abs_of_nonpos (by linarith : b - μ ≤ 0)]
      exact ⟨by linarith, iff_of_false (by linarith) (fun hm ↦ (not_le_of_gt hb) hm.2)⟩
  · rw [abs_of_nonneg (by linarith : 0 ≤ a - μ), abs_of_nonneg (by linarith : 0 ≤ b - μ)]
    exact ⟨by linarith, iff_of_false (by linarith) (fun hm ↦ (not_le_of_gt ha) hm.1)⟩

-- The central interval is contained in every interval between reverse ranks.
private theorem median_mem_pair (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hμ : μ ∈ medianSet x hn) (i : Fin n) :
    μ ∈ Icc (orderStat x (min i i.rev)) (orderStat x (max i i.rev)) := by
  have hlo : lowerMedian x hn ≤ μ := hμ.1
  have hhi : μ ≤ upperMedian x hn := hμ.2
  rcases le_or_gt (i : ℕ) ((n - 1) / 2) with hi | hi
  · have hir : n / 2 ≤ (i.rev : ℕ) := by rw [Fin.val_rev]; omega
    exact ⟨(orderStat_mono x (min_le_left _ _)).trans
        ((orderStat_mono x hi).trans hlo),
      hhi.trans ((orderStat_mono x hir).trans (orderStat_mono x (le_max_right _ _)))⟩
  · have hir : (i.rev : ℕ) ≤ (n - 1) / 2 := by rw [Fin.val_rev]; omega
    have hi' : n / 2 ≤ (i : ℕ) := by omega
    exact ⟨(orderStat_mono x (min_le_right _ _)).trans
        ((orderStat_mono x hir).trans hlo),
      hhi.trans ((orderStat_mono x hi').trans (orderStat_mono x (le_max_left _ _)))⟩

private def pairLoss (x : Fin n → ℝ) (μ : ℝ) (i : Fin n) : ℝ :=
  |orderStat x i - μ| + |orderStat x i.rev - μ|

private theorem pairLoss_eq (x : Fin n → ℝ) (μ : ℝ) (i : Fin n) :
    pairLoss x μ i =
      |orderStat x (min i i.rev) - μ| + |orderStat x (max i i.rev) - μ| := by
  rcases le_total i i.rev with h | h
  · simp [pairLoss, min_eq_left h, max_eq_right h]
  · simp [pairLoss, min_eq_right h, max_eq_left h, add_comm]

private theorem sum_pairLoss (x : Fin n → ℝ) (μ : ℝ) :
    ∑ i, pairLoss x μ i = 2 * absSum x μ := by
  have hs : ∑ i, |orderStat x i - μ| = absSum x μ :=
    Equiv.sum_comp (Tuple.sort x) (fun i ↦ |x i - μ|)
  have hr : ∑ i : Fin n, |orderStat x i.rev - μ| = ∑ i, |orderStat x i - μ| :=
    Equiv.sum_comp (Fin.revPerm : Equiv.Perm (Fin n)) (fun i ↦ |orderStat x i - μ|)
  simp only [pairLoss, Finset.sum_add_distrib, hr, hs, two_mul]

private theorem pairLoss_cmp (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hμ : μ ∈ medianSet x hn) (ν : ℝ) (i : Fin n) :
    pairLoss x μ i ≤ pairLoss x ν i ∧
      (pairLoss x μ i = pairLoss x ν i ↔
        ν ∈ Icc (orderStat x (min i i.rev)) (orderStat x (max i i.rev))) := by
  rw [pairLoss_eq x μ, pairLoss_eq x ν]
  have hab := orderStat_mono x (le_trans (min_le_left i i.rev) (le_max_left i i.rev))
  rw [(pair_bound hab).2.mpr (median_mem_pair hn hμ i)]
  exact ⟨(pair_bound hab).1, eq_comm.trans (pair_bound hab).2⟩

private theorem absSum_cmp (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hμ : μ ∈ medianSet x hn) (ν : ℝ) :
    absSum x μ ≤ absSum x ν ∧ (absSum x μ = absSum x ν ↔ ν ∈ medianSet x hn) := by
  have hle : ∀ i, pairLoss x μ i ≤ pairLoss x ν i :=
    fun i ↦ (pairLoss_cmp hn hμ ν i).1
  have hs := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hle i)
  rw [sum_pairLoss, sum_pairLoss] at hs
  refine ⟨by linarith, ⟨?_, ?_⟩⟩
  · intro he
    let j : Fin n := ⟨(n - 1) / 2, by omega⟩
    have hj : (j.rev : ℕ) = n / 2 := by
      rw [Fin.val_rev]
      change n - ((n - 1) / 2 + 1) = _
      omega
    have hjle : j ≤ j.rev := by
      change (n - 1) / 2 ≤ (j.rev : ℕ)
      omega
    have hp : pairLoss x μ j = pairLoss x ν j := by
      by_contra hne
      have hlt := Finset.sum_lt_sum (fun i _ ↦ hle i)
        ⟨j, Finset.mem_univ _, lt_of_le_of_ne (hle j) hne⟩
      rw [sum_pairLoss, sum_pairLoss, he] at hlt
      exact (lt_irrefl _) hlt
    have hm := (pairLoss_cmp hn hμ ν j).2.mp hp
    rw [min_eq_left hjle, max_eq_right hjle] at hm
    have hr : j.rev = (⟨n / 2, by omega⟩ : Fin n) := Fin.ext hj
    rw [hr] at hm
    exact hm
  · intro hν
    have hp : ∀ i, pairLoss x μ i = pairLoss x ν i :=
      fun i ↦ (pairLoss_cmp hn hμ ν i).2.mpr (median_mem_pair hn hν i)
    have hs' := Finset.sum_congr (s₁ := Finset.univ) rfl (fun i _ ↦ hp i)
    rw [sum_pairLoss, sum_pairLoss] at hs'
    linarith

private theorem meanAbsDev_eq (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    (n : ℝ) * meanAbsDev x μ = absSum x μ := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  unfold meanAbsDev absSum
  field_simp

private theorem logLik_eq (hn : 0 < n) (x : Fin n → ℝ) (μ b : ℝ) :
    logLik x μ b = (n : ℝ) * (-log (2 * b) - meanAbsDev x μ / b) := by
  unfold logLik logDensity
  rw [Finset.sum_sub_distrib, ← Finset.sum_div, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, mul_sub, ← mul_div_assoc, meanAbsDev_eq hn]
  rfl

private theorem absSum_le_iff_logLik_le (hn : 0 < n) (x : Fin n → ℝ) {b : ℝ} (hb : 0 < b)
    (μ ν : ℝ) : absSum x μ ≤ absSum x ν ↔ logLik x ν b ≤ logLik x μ b := by
  rw [logLik_eq hn, logLik_eq hn]
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  rw [mul_le_mul_iff_right₀ hN, sub_le_sub_iff_left, div_le_div_iff_of_pos_right hb,
    ← meanAbsDev_eq hn x μ, ← meanAbsDev_eq hn x ν, mul_le_mul_iff_right₀ hN]

/-- At fixed positive scale, the maximizing locations are exactly the sample medians. -/
theorem heldScale (hn : 0 < n) (x : Fin n → ℝ) {b : ℝ} (hb : 0 < b) :
    {μ | IsMaxOn (fun t ↦ logLik x t b) univ μ} = medianSet x hn := by
  ext μ
  change IsMaxOn (fun t ↦ logLik x t b) univ μ ↔ μ ∈ medianSet x hn
  constructor
  · intro h
    have hle := (absSum_le_iff_logLik_le hn x hb μ _).2 (h (mem_univ (lowerMedian x hn)))
    have hc := absSum_cmp hn (lower_mem hn x) μ
    exact hc.2.mp (hc.1.antisymm hle)
  · intro h t _
    exact (absSum_le_iff_logLik_le hn x hb μ t).1 (absSum_cmp hn h t).1

private theorem meanAbsDev_nonneg (x : Fin n → ℝ) (μ : ℝ) : 0 ≤ meanAbsDev x μ :=
  div_nonneg (Finset.sum_nonneg fun _ _ ↦ abs_nonneg _) (Nat.cast_nonneg _)

/-- The mean absolute deviation vanishes exactly when every residual vanishes. -/
theorem meanAbsDev_zero_iff (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    meanAbsDev x μ = 0 ↔ ∀ i, x i = μ := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  rw [meanAbsDev, div_eq_zero_iff, or_iff_left hN.ne',
    Finset.sum_eq_zero_iff_of_nonneg (fun _ _ ↦ abs_nonneg _)]
  simp [sub_eq_zero]

/-- All medians give the same mean absolute deviation. -/
theorem meanAbsDev_eq_of_mem_medianSet (hn : 0 < n) {x : Fin n → ℝ} {μ ν : ℝ}
    (hμ : μ ∈ medianSet x hn) (hν : ν ∈ medianSet x hn) :
    meanAbsDev x μ = meanAbsDev x ν := by
  unfold meanAbsDev
  rw [show (∑ i, |x i - μ|) = ∑ i, |x i - ν| from (absSum_cmp hn hμ ν).2.mpr hν]

private theorem heldLocation_strict (hn : 0 < n) {x : Fin n → ℝ} {μ b : ℝ}
    (hd : 0 < meanAbsDev x μ) (hb : 0 < b) (hne : b ≠ meanAbsDev x μ) :
    logLik x μ b < logLik x μ (meanAbsDev x μ) := by
  let d := meanAbsDev x μ
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hr : d / b ≠ 1 := by
    intro he
    apply hne
    simpa using ((div_eq_iff hb.ne').mp he).symm
  have hlog := log_lt_sub_one_of_pos (div_pos hd hb) hr
  rw [log_div hd.ne' hb.ne'] at hlog
  rw [logLik_eq hn, logLik_eq hn, mul_lt_mul_iff_right₀ hN]
  change -log (2 * b) - d / b < -log (2 * d) - d / d
  rw [log_mul (by norm_num) hb.ne', log_mul (by norm_num) hd.ne', div_self hd.ne']
  linarith

/-- With location fixed and positive deviation, the unique maximizing scale is that deviation. -/
theorem heldLocation (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hd : 0 < meanAbsDev x μ) :
    {b ∈ Ioi 0 | IsMaxOn (logLik x μ) (Ioi 0) b} = {meanAbsDev x μ} := by
  ext b
  change (0 < b ∧ IsMaxOn (logLik x μ) (Ioi 0) b) ↔ b = meanAbsDev x μ
  constructor
  · rintro ⟨hb, hmax⟩
    by_contra hne
    exact (not_le_of_gt (heldLocation_strict hn hd hb hne)) (hmax hd)
  · rintro rfl
    refine ⟨hd, ?_⟩
    intro b hb
    rcases eq_or_ne b (meanAbsDev x μ) with rfl | hne
    · exact le_rfl
    · exact (heldLocation_strict hn hd hb hne).le

/-- Zero deviation at the held location gives arbitrarily large log-likelihood. -/
theorem heldLocation_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hd : meanAbsDev x μ = 0) :
    ∀ M : ℝ, ∃ b > 0, M < logLik x μ b := by
  intro M
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let t : ℝ := -(M + 1) / n
  refine ⟨exp t / 2, by positivity, ?_⟩
  rw [logLik_eq hn, hd, zero_div, sub_zero, show 2 * (exp t / 2) = exp t by ring,
    log_exp, show (n : ℝ) * -t = M + 1 by dsimp [t]; field_simp]
  linarith

/-- The minimum deviation is zero exactly for constant samples. -/
theorem minMeanAbsDev_zero_iff (hn : 0 < n) (x : Fin n → ℝ) :
    meanAbsDev x (lowerMedian x hn) = 0 ↔ ∃ c : ℝ, ∀ i, x i = c := by
  rw [meanAbsDev_zero_iff hn]
  constructor
  · exact fun h ↦ ⟨_, h⟩
  · rintro ⟨c, hc⟩ i
    simp [lowerMedian, orderStat, hc]

/-- At positive minimum deviation the joint maximizers form a closed segment at that scale. -/
theorem joint (hn : 0 < n) {x : Fin n → ℝ}
    (hd : 0 < meanAbsDev x (lowerMedian x hn)) :
    jointMaximizers x = medianSet x hn ×ˢ {meanAbsDev x (lowerMedian x hn)} := by
  ext p
  rcases p with ⟨μ, b⟩
  change ((μ, b) ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain (μ, b)) ↔
    μ ∈ medianSet x hn ∧ b = meanAbsDev x (lowerMedian x hn)
  constructor
  · rintro ⟨hp, hm⟩
    have hb : 0 < b := hp.2
    have hμ : μ ∈ medianSet x hn := by
      rw [← heldScale hn x hb]
      exact fun ν _ ↦ hm (show (ν, b) ∈ domain from ⟨mem_univ _, hb⟩)
    have he := meanAbsDev_eq_of_mem_medianSet hn (lower_mem hn x) hμ
    have hscale := heldLocation hn (he ▸ hd)
    have hmem : b ∈ {s ∈ Ioi 0 | IsMaxOn (logLik x μ) (Ioi 0) s} :=
      ⟨hb, fun s hs ↦ hm (show (μ, s) ∈ domain from ⟨mem_univ _, hs⟩)⟩
    rw [hscale, ← he] at hmem
    exact ⟨hμ, hmem⟩
  · rintro ⟨hμ, rfl⟩
    refine ⟨⟨mem_univ _, hd⟩, ?_⟩
    have he := meanAbsDev_eq_of_mem_medianSet hn (lower_mem hn x) hμ
    have hbest : IsMaxOn (logLik x μ) (Ioi 0) (meanAbsDev x (lowerMedian x hn)) := by
      rw [he]
      have hs : meanAbsDev x μ ∈ {meanAbsDev x μ} := mem_singleton _
      rw [← heldLocation hn (he ▸ hd)] at hs
      exact hs.2
    intro q hq
    have hs : q.2 > 0 := hq.2
    have hmax : IsMaxOn (fun ν ↦ logLik x ν q.2) univ μ := by
      rw [← heldScale hn x hs] at hμ
      exact hμ
    exact (hmax (mem_univ q.1)).trans (hbest hs)

/-- The segment is also the graph of mean absolute deviation over the median set. -/
theorem joint_graph (hn : 0 < n) {x : Fin n → ℝ}
    (hd : 0 < meanAbsDev x (lowerMedian x hn)) :
    jointMaximizers x = {p | p.1 ∈ medianSet x hn ∧ p.2 = meanAbsDev x p.1} := by
  rw [joint hn hd]
  ext p
  change (p.1 ∈ medianSet x hn ∧ p.2 = meanAbsDev x (lowerMedian x hn)) ↔
    (p.1 ∈ medianSet x hn ∧ p.2 = meanAbsDev x p.1)
  constructor <;> rintro ⟨h, hb⟩
  · exact ⟨h, hb.trans (meanAbsDev_eq_of_mem_medianSet hn (lower_mem hn x) h)⟩
  · exact ⟨h, hb.trans (meanAbsDev_eq_of_mem_medianSet hn h (lower_mem hn x))⟩

/-- At zero minimum deviation, shrinking scale at a median gives arbitrarily high likelihood. -/
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    (hd : meanAbsDev x (lowerMedian x hn) = 0) :
    ∀ M : ℝ, ∃ p ∈ domain, M < logLik x p.1 p.2 := by
  intro M
  obtain ⟨b, hb, h⟩ := heldLocation_unbounded hn hd M
  exact ⟨(lowerMedian x hn, b), ⟨mem_univ _, hb⟩, h⟩

/-- A finite joint maximum exists exactly for nonconstant samples. -/
theorem joint_exists_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (jointMaximizers x).Nonempty ↔ ¬ ∃ c : ℝ, ∀ i, x i = c := by
  rw [← minMeanAbsDev_zero_iff hn]
  constructor
  · rintro ⟨p, hp⟩ hz
    obtain ⟨q, hq, hlt⟩ := joint_unbounded hn hz (logLik x p.1 p.2)
    exact (not_le_of_gt hlt) (hp.2 hq)
  · intro hnz
    have hd := lt_of_le_of_ne (meanAbsDev_nonneg x (lowerMedian x hn)) (Ne.symm hnz)
    rw [joint hn hd]
    exact ⟨(lowerMedian x hn, meanAbsDev x (lowerMedian x hn)), lower_mem hn x, mem_singleton _⟩

/-- For positive minimum deviation the joint fit is unique exactly when the central ranks agree. -/
theorem joint_unique_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hd : 0 < meanAbsDev x (lowerMedian x hn)) :
    (∃! p, p ∈ jointMaximizers x) ↔ lowerMedian x hn = upperMedian x hn := by
  rw [joint hn hd]
  let d := meanAbsDev x (lowerMedian x hn)
  have hlo : (lowerMedian x hn, d) ∈ medianSet x hn ×ˢ {d} :=
    ⟨lower_mem hn x, mem_singleton _⟩
  have hhi : (upperMedian x hn, d) ∈ medianSet x hn ×ˢ {d} :=
    ⟨⟨lower_le_upper x hn, le_rfl⟩, mem_singleton _⟩
  constructor
  · rintro ⟨p, _, h⟩
    exact congrArg Prod.fst ((h _ hlo).trans (h _ hhi).symm)
  · intro he
    refine ⟨_, hlo, ?_⟩
    intro p hp
    have hm : p.1 ∈ Icc (lowerMedian x hn) (upperMedian x hn) := hp.1
    rw [← he] at hm
    exact Prod.ext (by simpa using hm) (mem_singleton_iff.mp hp.2)

/-- The parity form of the fitter's uniqueness criterion on samples with a joint maximum. -/
theorem joint_unique_iff_odd_or_tied (hn : 0 < n) {x : Fin n → ℝ}
    (hd : 0 < meanAbsDev x (lowerMedian x hn)) :
    (∃! p, p ∈ jointMaximizers x) ↔
      Odd n ∨ ∃ (m : ℕ) (hm : n = 2 * m),
        orderStat x ⟨m - 1, by omega⟩ = orderStat x ⟨m, by omega⟩ := by
  rw [joint_unique_iff hn hd]
  by_cases ho : Odd n
  · have h : (n - 1) / 2 = n / 2 := by obtain ⟨k, hk⟩ := ho; omega
    simp [lowerMedian, upperMedian, h, ho]
  · have hm : n = 2 * (n / 2) := by
      by_contra h
      exact ho ⟨n / 2, by omega⟩
    simp only [ho, false_or]
    constructor
    · intro he
      have hl : (n - 1) / 2 = n / 2 - 1 := by omega
      exact ⟨n / 2, hm, by simpa [lowerMedian, upperMedian, hl] using he⟩
    · rintro ⟨m, hm', he⟩
      have hl : (n - 1) / 2 = m - 1 := by omega
      have hu : n / 2 = m := by omega
      simpa [lowerMedian, upperMedian, hl, hu] using he

end

end UnivariateML.Laplace
