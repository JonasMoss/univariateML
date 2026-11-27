module

public import UnivariateML.Family.Discrete3CMP

import Mathlib.Tactic

set_option linter.style.header false

/-!
# CMP with rate held

## Deviations

The zero-rate seam concerns likelihood on the mathematical parameter fiber.
The fitting interface separately rejects positive data at held rate zero.
-/

@[expose] public section

open Filter Real Set
open scoped Topology

namespace UnivariateML.HeldSweepCMP
noncomputable section

private theorem logFactorial_nonneg (k : ℕ) : 0 ≤ CMP.logFactorial k := by
  apply log_nonneg
  exact_mod_cast Nat.factorial_pos k

private theorem logFactorial_pos {k : ℕ} (hk : 1 < k) : 0 < CMP.logFactorial k := by
  apply log_pos
  exact_mod_cast (Nat.one_lt_factorial.mpr hk)

private theorem weight_limit (η : ℝ) (k : ℕ) :
    Tendsto (fun ν ↦ CMP.weight η ν k) atTop
      (𝓝 (if k ≤ 1 then exp (η * k) else 0)) := by
  by_cases hk : k ≤ 1
  · interval_cases k <;> simp [CMP.weight, CMP.logFactorial]
  · rw [ite_eq_right hk]
    have h := tendsto_atBot_add_const_left atTop (η * (k : ℝ))
      (tendsto_id.atTop_mul_const_of_neg (neg_neg_of_pos (logFactorial_pos (lt_of_not_ge hk))))
    convert tendsto_exp_atBot.comp h using 1
    ext ν
    simp [CMP.weight, sub_eq_add_neg, mul_neg]

private theorem partition_limit (η : ℝ) :
    Tendsto (fun ν ↦ ∑' k, CMP.weight η ν k) atTop (𝓝 (1 + exp η)) := by
  have h := tendsto_tsum_of_dominated_convergence
    (CMP.summable_weight (η := η) (ν := 1) (Or.inl zero_lt_one)) (weight_limit η)
    (by
      filter_upwards [eventually_ge_atTop (1 : ℝ)] with ν hν
      intro k
      unfold CMP.weight
      rw [Real.norm_of_nonneg (exp_pos _).le]
      apply exp_le_exp.mpr
      exact sub_le_sub_left (mul_le_mul_of_nonneg_right hν (logFactorial_nonneg k)) _)
  have he : (∑' k : ℕ, if k ≤ 1 then exp (η * k) else 0) = 1 + exp η := by
    rw [tsum_eq_sum (s := {0, 1})]
    · simp
    · intro k hk
      split_ifs with h
      · exfalso
        interval_cases k <;> simp at hk
      · rfl
  rwa [he] at h

/-- At every positive held rate, the large-dispersion limit is the sample
likelihood of the Bernoulli law with odds equal to that rate. -/
theorem held_rate_bernoulli_path {n : ℕ} (x : Fin n → ℕ) {r : ℝ} (hr : 0 < r) :
    Tendsto (fun ν ↦ CMP.likelihood x r ν) atTop
      (𝓝 (∏ i, (if x i ≤ 1 then r ^ x i else 0) / (1 + r))) := by
  simp only [CMP.likelihood, ite_eq_right hr.ne']
  have h := (partition_limit (log r))
  rw [exp_log hr] at h
  apply tendsto_finsetProd
  intro i _
  have hw := weight_limit (log r) (x i)
  simp_rw [mul_comm (log r), exp_nat_mul, exp_log hr] at hw
  exact hw.div h (by positivity)

/-- With positive rate held and binary data, the Bernoulli limit is the
strictly unattained supremum, also when the geometric endpoint is feasible. -/
theorem held_rate_binary_supremum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) {r : ℝ} (hr : 0 < r) :
    let D := {ν : ℝ | (r, ν) ∈ CMP.rateDomain}
    let b := ∏ i, r ^ x i / (1 + r)
    IsLUB (CMP.likelihood x r '' D) b ∧ ∀ ν ∈ D, CMP.likelihood x r ν < b := by
  dsimp only
  have hbound (ν : ℝ) (hν : (r, ν) ∈ CMP.rateDomain) :
      CMP.likelihood x r ν < ∏ i, r ^ x i / (1 + r) := by
    have hs : Summable (CMP.weight (log r) ν) := by
      rcases hν.2 with h | ⟨_, h | ⟨h, hr1⟩⟩
      · exact (hr.ne' h).elim
      · exact CMP.summable_weight (Or.inl h)
      · exact CMP.summable_weight (Or.inr ⟨h, hr1⟩)
    have hw (k : ℕ) (hk : k ≤ 1) : CMP.weight (log r) ν k = r ^ k := by
      interval_cases k <;> simp [CMP.weight, CMP.logFactorial, exp_log hr]
    have hZ : 1 + r < ∑' k, CMP.weight (log r) ν k := by
      have h := hs.sum_le_tsum (s := {0, 1, 2})
        (fun k _ ↦ show 0 ≤ CMP.weight (log r) ν k from (exp_pos _).le)
      have h2 : 0 < CMP.weight (log r) ν 2 := exp_pos _
      simp only [Finset.sum_insert, Finset.sum_singleton, Finset.mem_insert, Finset.mem_singleton,
        Nat.reduceEqDiff, or_self, not_false_eq_true, hw 0 (by decide), hw 1 (by decide),
        pow_zero, pow_one] at h
      linarith
    rw [CMP.likelihood, ite_eq_right hr.ne']
    have hne : (Finset.univ : Finset (Fin n)).Nonempty := ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    apply Finset.prod_lt_prod_of_nonempty₀ (fun i _ ↦ ?_) (fun i _ ↦ ?_) hne
    · rw [hw _ (hx i)]
      exact div_pos (pow_pos hr _) ((by linarith : 0 < 1 + r).trans hZ)
    · rw [hw _ (hx i)]
      exact div_lt_div_of_pos_left (by positivity) (by positivity) hZ
  refine ⟨⟨?_, ?_⟩, hbound⟩
  · rintro _ ⟨ν, hν, rfl⟩
    exact (hbound ν hν).le
  · intro c hc
    have ht := held_rate_bernoulli_path x hr
    simp only [ite_eq_left (hx _)] at ht
    apply le_of_tendsto ht
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with ν hν
    exact hc ⟨ν, ⟨hν.le, Or.inr ⟨hr, Or.inl hν⟩⟩, rfl⟩

private def centeredWeight (r m ν : ℝ) (k : ℕ) : ℝ :=
  exp (log r * k + ν * (m - CMP.logFactorial k))

private def centeredPartition (r m ν : ℝ) : ℝ := ∑' k, centeredWeight r m ν k

private theorem centered_summable {r m ν : ℝ} (hr : 0 < r)
    (hν : (r, ν) ∈ CMP.rateDomain) : Summable (centeredWeight r m ν) := by
  have hs : Summable (CMP.weight (log r) ν) := by
    rcases hν.2 with h | ⟨_, h⟩
    · exact (hr.ne' h).elim
    · exact CMP.summable_weight h
  convert hs.mul_left (exp (ν * m)) using 1
  ext k
  rw [centeredWeight, CMP.weight, ← exp_add]
  congr 1
  ring

private theorem fiber_convex {r : ℝ} (hr : 0 < r) :
    Convex ℝ {ν : ℝ | (r, ν) ∈ CMP.rateDomain} := by
  by_cases h : log r < 0
  · convert (convex_Ici (𝕜 := ℝ) (0 : ℝ)) using 1
    ext ν
    change (0 ≤ ν ∧ (r = 0 ∨ 0 < r ∧ (0 < ν ∨ ν = 0 ∧ log r < 0))) ↔ 0 ≤ ν
    simp only [hr.ne', false_or, hr, true_and, h, and_true]
    exact ⟨And.left, fun hν ↦ ⟨hν, hν.lt_or_eq.imp_right Eq.symm⟩⟩
  · convert (convex_Ioi (𝕜 := ℝ) (0 : ℝ)) using 1
    ext ν
    change (0 ≤ ν ∧ (r = 0 ∨ 0 < r ∧ (0 < ν ∨ ν = 0 ∧ log r < 0))) ↔ 0 < ν
    simp only [hr.ne', false_or, hr, true_and, h, and_false, or_false]
    exact ⟨And.right, fun hν ↦ ⟨hν.le, hν⟩⟩

private theorem centered_strictConvex {r m : ℝ} (hr : 0 < r) (hm : 0 < m) :
    StrictConvexOn ℝ {ν : ℝ | (r, ν) ∈ CMP.rateDomain} (centeredPartition r m) := by
  refine ⟨fiber_convex hr, ?_⟩
  intro a ha b hb hab u v hu hv huv
  have hc := (fiber_convex hr) ha hb hu.le hv.le huv
  have he (k : ℕ) :
      log r * k + (u • a + v • b) * (m - CMP.logFactorial k) =
      u * (log r * k + a * (m - CMP.logFactorial k)) +
      v * (log r * k + b * (m - CMP.logFactorial k)) := by
    simp only [smul_eq_mul]
    linear_combination -(log r * k) * huv
  have hle (k : ℕ) : centeredWeight r m (u • a + v • b) k ≤
      u * centeredWeight r m a k + v * centeredWeight r m b k := by
    rw [centeredWeight, he]
    exact strictConvexOn_exp.convexOn.2 (mem_univ _) (mem_univ _) hu.le hv.le huv
  have hlt : centeredWeight r m (u • a + v • b) 0 <
      u * centeredWeight r m a 0 + v * centeredWeight r m b 0 := by
    rw [centeredWeight, he]
    apply strictConvexOn_exp.2 (mem_univ _) (mem_univ _) _ hu hv huv
    simp only [Nat.cast_zero, mul_zero, zero_add, CMP.logFactorial, Nat.factorial_zero,
      Nat.cast_one, log_one, sub_zero]
    exact fun h ↦ hab (mul_right_cancel₀ hm.ne' h)
  have hsum := Summable.tsum_lt_tsum hle hlt (centered_summable hr hc)
    (((centered_summable hr ha).mul_left u).add ((centered_summable hr hb).mul_left v))
  rw [((centered_summable hr ha).mul_left u).tsum_add
    ((centered_summable hr hb).mul_left v), tsum_mul_left, tsum_mul_left] at hsum
  exact hsum

private theorem centered_continuous {r m : ℝ} (hr : 0 < r) :
    ContinuousOn (centeredPartition r m) {ν : ℝ | (r, ν) ∈ CMP.rateDomain} := by
  intro a ha
  let D := {ν : ℝ | (r, ν) ∈ CMP.rateDomain}
  have hl : ∃ l : ℝ, (r, l) ∈ CMP.rateDomain ∧ ∀ᶠ ν in 𝓝[D] a, l ≤ ν := by
    by_cases hp : 0 < a
    · refine ⟨a / 2, ⟨by positivity, Or.inr ⟨hr, Or.inl (by positivity)⟩⟩, ?_⟩
      exact eventually_nhdsWithin_of_eventually_nhds (eventually_ge_nhds (by linarith))
    · have he : a = 0 := le_antisymm (le_of_not_gt hp) ha.1
      exact ⟨a, ha, eventually_mem_nhdsWithin.mono (fun ν (hν : ν ∈ D) ↦ he ▸ hν.1)⟩
  obtain ⟨l, hl, hlow⟩ := hl
  have hhigh : ∀ᶠ ν in 𝓝[D] a, ν ≤ a + 1 :=
    eventually_nhdsWithin_of_eventually_nhds (eventually_le_nhds (by linarith))
  have hw (ν : ℝ) (k : ℕ) :
      centeredWeight r m ν k = exp (ν * m) * CMP.weight (log r) ν k := by
    rw [centeredWeight, CMP.weight, ← exp_add]
    congr 1
    ring
  have hs : Summable (CMP.weight (log r) l) := by
    rcases hl.2 with h | ⟨_, h⟩
    · exact (hr.ne' h).elim
    · exact CMP.summable_weight h
  have ht (k : ℕ) : Continuous fun ν ↦ centeredWeight r m ν k := by
    unfold centeredWeight
    fun_prop
  apply tendsto_tsum_of_dominated_convergence (hs.mul_left (exp ((a + 1) * |m|)))
    (fun k ↦ (ht k).continuousWithinAt)
  filter_upwards [hlow, hhigh, eventually_mem_nhdsWithin] with ν hνl hνa (hν : ν ∈ D)
  intro k
  have hν0 : 0 ≤ ν := hν.1
  rw [Real.norm_of_nonneg (show 0 ≤ centeredWeight r m ν k from (exp_pos _).le), hw]
  apply mul_le_mul _ _ (exp_pos _).le (exp_pos _).le
  · exact exp_le_exp.mpr ((mul_le_mul_of_nonneg_left (le_abs_self m) hν0).trans
      (mul_le_mul_of_nonneg_right hνa (abs_nonneg _)))
  · exact exp_le_exp.mpr (sub_le_sub_left
      (mul_le_mul_of_nonneg_right hνl (logFactorial_nonneg k)) _)

private theorem centered_atTop {r m : ℝ} (hr : 0 < r) (hm : 0 < m) :
    Tendsto (centeredPartition r m) atTop atTop := by
  have ht : Tendsto (fun ν : ℝ ↦ exp (ν * m)) atTop atTop :=
    tendsto_exp_atTop.comp (tendsto_id.atTop_mul_const hm)
  refine tendsto_atTop_mono' _ ?_ ht
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with ν hν
  have hs := centered_summable (m := m) hr
    (show (r, ν) ∈ CMP.rateDomain from ⟨hν.le, Or.inr ⟨hr, Or.inl hν⟩⟩)
  simpa only [centeredPartition, centeredWeight, Nat.cast_zero, mul_zero, CMP.logFactorial,
    Nat.factorial_zero, Nat.cast_one, log_one, sub_zero, zero_add] using
    hs.le_tsum 0 (fun k _ ↦ show 0 ≤ centeredWeight r m ν k from (exp_pos _).le)

private theorem centered_near_zero {r m : ℝ} (hr : 0 < r) (hr1 : 1 ≤ r) :
    Tendsto (centeredPartition r m) (𝓝[>] (0 : ℝ)) atTop := by
  apply tendsto_atTop.mpr
  intro C
  obtain ⟨N, hN⟩ := exists_nat_gt C
  have ht : Tendsto (fun ν : ℝ ↦ ∑ k ∈ Finset.range N, centeredWeight r m ν k)
      (𝓝[>] (0 : ℝ)) (𝓝 (∑ k ∈ Finset.range N, centeredWeight r m 0 k)) := by
    apply tendsto_finsetSum
    intro k _
    apply Continuous.continuousWithinAt
    unfold centeredWeight
    fun_prop
  have hb : C < ∑ k ∈ Finset.range N, centeredWeight r m 0 k := by
    apply hN.trans_le
    calc
      (N : ℝ) = ∑ _k ∈ Finset.range N, (1 : ℝ) := by simp
      _ ≤ _ := Finset.sum_le_sum (fun k _ ↦ by
        simp only [centeredWeight, zero_mul, add_zero, ← exp_zero, exp_le_exp]
        exact mul_nonneg (log_nonneg hr1) (Nat.cast_nonneg k))
  filter_upwards [ht.eventually (Ioi_mem_nhds hb), eventually_mem_nhdsWithin]
    with ν hν (hp : 0 < ν)
  exact hν.le.trans ((centered_summable hr
    (show (r, ν) ∈ CMP.rateDomain from ⟨hp.le, Or.inr ⟨hr, Or.inl hp⟩⟩)).sum_le_tsum
    (s := Finset.range N)
    (fun k _ ↦ show 0 ≤ centeredWeight r m ν k from (exp_pos _).le))

private theorem centered_minimum {r m : ℝ} (hr : 0 < r) (hm : 0 < m) :
    ∃ ν, (r, ν) ∈ CMP.rateDomain ∧
      IsMinOn (centeredPartition r m) {v : ℝ | (r, v) ∈ CMP.rateDomain} ν := by
  let D := {v : ℝ | (r, v) ∈ CMP.rateDomain}
  let Z := centeredPartition r m
  have hmem (v : ℝ) (hv : 0 < v) : v ∈ D :=
    ⟨hv.le, Or.inr ⟨hr, Or.inl hv⟩⟩
  obtain ⟨U, hU⟩ := eventually_atTop.mp ((centered_atTop hr hm).eventually_ge_atTop (Z 1))
  let B := max 1 U
  have hB : 1 ≤ B := le_max_left _ _
  have hhigh (v : ℝ) (hv : B < v) : Z 1 ≤ Z v :=
    hU v ((le_max_right 1 U).trans hv.le)
  have hlow : ∃ A : ℝ, A ≤ 1 ∧ A ∈ D ∧
      ∀ v ∈ D, v < A → Z 1 ≤ Z v := by
    by_cases h : log r < 0
    · exact ⟨0, zero_le_one, ⟨le_rfl, Or.inr ⟨hr, Or.inr ⟨rfl, h⟩⟩⟩,
        fun v hv hlt ↦ (hlt.not_ge hv.1).elim⟩
    · have hr1 : 1 ≤ r := le_of_not_gt (fun hlt ↦ h (log_neg hr hlt))
      have ht := eventually_nhdsWithin_iff.mp
        ((centered_near_zero (m := m) hr hr1).eventually_ge_atTop (Z 1))
      obtain ⟨ε, hε, hb⟩ := Metric.eventually_nhds_iff.mp ht
      let A := min 1 ε
      have hA : 0 < A := lt_min zero_lt_one hε
      refine ⟨A, min_le_left _ _, hmem A hA, fun v hv hlt ↦ ?_⟩
      have hvp : 0 < v := by
        rcases hv.2 with hz | ⟨_, hp | ⟨_, hn⟩⟩
        · exact (hr.ne' hz).elim
        · exact hp
        · exact (h hn).elim
      refine hb (y := v) ?_ hvp
      rw [Real.dist_eq, sub_zero, abs_of_pos hvp]
      exact hlt.trans_le (min_le_right 1 ε)
  obtain ⟨A, hA, hAD, hlow⟩ := hlow
  have hsub : Icc A B ⊆ D := by
    intro v hv
    rcases eq_or_lt_of_le hv.1 with he | hlt
    · exact he ▸ hAD
    · exact hmem v (hAD.1.trans_lt hlt)
  obtain ⟨v, hv, hmin⟩ := isCompact_Icc.exists_isMinOn
    ⟨1, hA, hB⟩ ((centered_continuous (m := m) hr).mono hsub)
  refine ⟨v, hsub hv, fun w hw ↦ ?_⟩
  have hbase : Z v ≤ Z 1 := hmin (show 1 ∈ Icc A B from ⟨hA, hB⟩)
  by_cases h₁ : w < A
  · exact hbase.trans (hlow w hw h₁)
  by_cases h₂ : B < w
  · exact hbase.trans (hhigh w h₂)
  exact hmin (show w ∈ Icc A B from ⟨le_of_not_gt h₁, le_of_not_gt h₂⟩)

private theorem likelihood_eq_centered {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r ν : ℝ} (hr : 0 < r) (_hν : (r, ν) ∈ CMP.rateDomain) :
    let m := (∑ i, CMP.logFactorial (x i)) / (n : ℝ)
    CMP.likelihood x r ν = exp (∑ i, log r * (x i : ℝ)) /
      (centeredPartition r m ν)^n := by
  dsimp only
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  let m := (∑ i, CMP.logFactorial (x i)) / (n : ℝ)
  have hz : centeredPartition r m ν =
      exp (ν * m) * ∑' k, CMP.weight (log r) ν k := by
    rw [centeredPartition, ← tsum_mul_left]
    congr 1
    ext k
    rw [centeredWeight, CMP.weight, ← exp_add]
    congr 1
    ring
  have he : (n : ℝ) * (ν * m) = ν * ∑ i, CMP.logFactorial (x i) := by
    dsimp [m]
    field_simp [hn']
  rw [CMP.likelihood, ite_eq_right hr.ne', Finset.prod_div_distrib, Finset.prod_const,
    Finset.card_univ, Fintype.card_fin, hz, mul_pow, ← exp_nat_mul, he]
  simp only [CMP.weight]
  rw [← exp_sum, Finset.sum_sub_distrib, ← Finset.mul_sum, exp_sub]
  rw [div_div, ← Finset.mul_sum]

/-- With a positive rate held and any count above one, there is exactly one
maximizing dispersion in the full domain, including its feasible endpoint. -/
theorem held_rate_nonbinary {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 1 < x i) {r : ℝ} (hr : 0 < r) :
    ∃! ν, (r, ν) ∈ CMP.rateDomain ∧
      IsMaxOn (CMP.likelihood x r) {v : ℝ | (r, v) ∈ CMP.rateDomain} ν := by
  let m := (∑ i, CMP.logFactorial (x i)) / (n : ℝ)
  have hm : 0 < m := by
    obtain ⟨i, hi⟩ := hx
    apply div_pos _ (by exact_mod_cast hn)
    exact (logFactorial_pos hi).trans_le
      (Finset.single_le_sum (fun j _ ↦ logFactorial_nonneg (x j)) (Finset.mem_univ i))
  let D := {v : ℝ | (r, v) ∈ CMP.rateDomain}
  have hp (v : ℝ) (hv : v ∈ D) : 0 < centeredPartition r m v :=
    (exp_pos _).trans_le ((centered_summable hr hv).le_tsum 0
      (fun k _ ↦ show 0 ≤ centeredWeight r m v k from (exp_pos _).le))
  have hcompare (v w : ℝ) (hv : v ∈ D) (hw : w ∈ D) :
      CMP.likelihood x r v ≤ CMP.likelihood x r w ↔
        centeredPartition r m w ≤ centeredPartition r m v := by
    rw [likelihood_eq_centered hn x hr hv, likelihood_eq_centered hn x hr hw]
    exact (div_le_div_iff_of_pos_left (exp_pos _) (pow_pos (hp v hv) _) (pow_pos (hp w hw) _)).trans
      ((pow_left_strictMonoOn₀ hn.ne').le_iff_le (hp w hw).le (hp v hv).le)
  obtain ⟨v, hv, hmin⟩ := centered_minimum hr hm
  refine ⟨v, ⟨hv, fun w hw ↦ (hcompare w v hw hv).mpr (hmin hw)⟩, ?_⟩
  rintro w ⟨hw, hmax⟩
  have hmin' : IsMinOn (centeredPartition r m) D w :=
    fun z hz ↦ (hcompare z w hz hw).mp (hmax hz)
  exact (centered_strictConvex hr hm).eq_of_isMinOn hmin' hmin hw hv

/-- Holding rate zero makes the likelihood independent of dispersion, with
value one for all zeros and zero for a sample containing a positive count. -/
theorem held_zero_rate {n : ℕ} (x : Fin n → ℕ) :
    ∀ ν ∈ Ici (0 : ℝ),
      IsMaxOn (CMP.likelihood x 0) (Ici 0) ν ∧
        CMP.likelihood x 0 ν = (if ∀ i, x i = 0 then 1 else 0) := by
  intro ν _
  constructor
  · intro q _
    exact le_of_eq (by simp [CMP.likelihood])
  · simp [CMP.likelihood]

end

end UnivariateML.HeldSweepCMP

end
