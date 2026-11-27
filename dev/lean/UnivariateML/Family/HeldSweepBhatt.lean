module

public import UnivariateML.Family.SkewLSBhatt
public import UnivariateML.Family.ClosedForm
public import UnivariateML.Family.Discrete

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Bhattacharjee range equality and the normal member

The range-equality supremum is the complementary endpoint trace at the
empirical lower-endpoint fraction among the two endpoints.
-/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.HeldSweepBhatt

open SkewLSBhatt SkewLSGaussian

/-- At held width equal to half the sample range, the optimized endpoint
trace at the midrange is a strict and unattained supremum. -/
theorem range_equality {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {a : ℝ} (ha : 0 < a) (i j : Fin n)
    (hlo : ∀ k, x i ≤ x k) (hhi : ∀ k, x k ≤ x j) (he : x j - x i = 2 * a) :
    let c := (x i + x j) / 2
    ∃ w ∈ Icc (0 : ℝ) 1, IsMaxOn (uniformTrace x a c) (Icc 0 1) w ∧
      IsLUB ((fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) '' (univ ×ˢ Ioi 0))
        (uniformTrace x a c w) ∧
      ∀ m s : ℝ, 0 < s → likelihood x a m s < uniformTrace x a c w := by
  apply coverage_uniform_envelope hn ha
  intro k
  exact abs_le.mpr ⟨by linarith [hlo k], by linarith [hhi k]⟩

private noncomputable def endpointCount {n : ℕ} (x : Fin n → ℝ) (z : ℝ) : ℕ :=
  (Finset.univ.filter (fun i ↦ x i = z)).card

private theorem trace_formula {n : ℕ} (x : Fin n → ℝ) {a c lo hi : ℝ}
    (ha : 0 < a) (hl : c - a = lo) (hh : c + a = hi) (w : ℝ) :
    uniformTrace x a c w =
      w ^ endpointCount x lo * (1 - w) ^ endpointCount x hi / (2 * a)^n := by
  have he (i : Fin n) :
      (if x i - c + a = 0 then w else 1) - (if x i - c - a = 0 then w else 0) =
        (if x i = lo then w else 1) * (if x i = hi then 1 - w else 1) := by
    have h₁ : (x i - c + a = 0) ↔ x i = lo := by constructor <;> intro h <;> linarith
    have h₂ : (x i - c - a = 0) ↔ x i = hi := by constructor <;> intro h <;> linarith
    simp only [h₁, h₂]
    have hne : lo ≠ hi := by intro h; linarith
    by_cases hlo : x i = lo <;> by_cases hhi : x i = hi
    · linarith
    · simp only [hlo, hne, ite_true, ite_false, sub_zero, mul_one]
    · simp only [hhi, Ne.symm hne, ite_true, ite_false, one_mul]
    · simp only [hlo, hhi, ite_false, sub_zero, mul_one]
  simp only [uniformTrace, he, Finset.prod_div_distrib, Finset.prod_mul_distrib,
    Finset.prod_const, Finset.card_univ, Fintype.card_fin, endpointCount]
  simp only [Finset.prod_ite, Finset.prod_const, mul_one, one_pow, mul_pow]

private theorem binomial_interior (z o : ℕ) {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) :
    Discrete.Binomial.likelihood z o w = w^z * (1 - w)^o := by
  unfold Discrete.Binomial.likelihood
  rw [ite_eq_right (by
    rintro (⟨h, _⟩ | ⟨h, _⟩)
    · exact hw.1.ne' h
    · exact hw.2.ne h)]
  rw [exp_add, exp_nat_mul, exp_nat_mul, exp_log hw.1, exp_log (sub_pos.mpr hw.2)]

/-- At range equality each endpoint is observed. The supremum uses the
empirical lower-endpoint fraction among endpoint observations. -/
theorem range_equality_height {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {a : ℝ} (ha : 0 < a) (i j : Fin n)
    (hlo : ∀ k, x i ≤ x k) (hhi : ∀ k, x k ≤ x j) (he : x j - x i = 2 * a) :
    let z := (Finset.univ.filter (fun k ↦ x k = x i)).card
    let o := (Finset.univ.filter (fun k ↦ x k = x j)).card
    let w := (z : ℝ) / ((z : ℝ) + o)
    let u := w^z * (1 - w)^o / (2 * a)^n
    IsLUB ((fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) '' (univ ×ˢ Ioi 0)) u ∧
      ∀ m s : ℝ, 0 < s → likelihood x a m s < u := by
  let z := endpointCount x (x i)
  let o := endpointCount x (x j)
  let w : ℝ := (z : ℝ) / ((z : ℝ) + o)
  let c := (x i + x j) / 2
  have hz : 0 < z := Finset.card_pos.mpr ⟨i, by simp⟩
  have ho : 0 < o := Finset.card_pos.mpr ⟨j, by simp⟩
  have hz' : (0 : ℝ) < z := by exact_mod_cast hz
  have ho' : (0 : ℝ) < o := by exact_mod_cast ho
  have hw : w ∈ Ioo (0 : ℝ) 1 :=
    ⟨div_pos hz' (add_pos hz' ho'), (div_lt_one (add_pos hz' ho')).mpr (by linarith)⟩
  have hf (v : ℝ) : uniformTrace x a c v = v^z * (1 - v)^o / (2 * a)^n :=
    trace_formula x ha (by dsimp [c]; linarith) (by dsimp [c]; linarith) v
  have hm : IsMaxOn (uniformTrace x a c) (Icc 0 1) w := by
    intro v hv
    change uniformTrace x a c v ≤ uniformTrace x a c w
    rw [hf, hf]
    apply div_le_div_of_nonneg_right _ (by positivity)
    by_cases hv₀ : v = 0
    · simp [hv₀, zero_pow hz.ne',
        mul_nonneg (pow_nonneg hw.1.le _) (pow_nonneg (sub_nonneg.mpr hw.2.le) _)]
    by_cases hv₁ : v = 1
    · simp [hv₁, zero_pow ho.ne',
        mul_nonneg (pow_nonneg hw.1.le _) (pow_nonneg (sub_nonneg.mpr hw.2.le) _)]
    have hvi : v ∈ Ioo (0 : ℝ) 1 :=
      ⟨lt_of_le_of_ne hv.1 (Ne.symm hv₀), lt_of_le_of_ne hv.2 hv₁⟩
    rw [← binomial_interior z o hvi, ← binomial_interior z o hw]
    exact ((Discrete.Binomial.fixed_size_mle hz'.le ho'.le (add_pos hz' ho') w
      ⟨hw.1.le, hw.2.le⟩).mpr rfl) hv
  obtain ⟨v, hv, hmax, hlub, hlt⟩ := range_equality hn ha i j hlo hhi he
  have hu : uniformTrace x a c v = uniformTrace x a c w :=
    le_antisymm (hm hv) (hmax ⟨hw.1.le, hw.2.le⟩)
  rw [hu, hf] at hlub hlt
  exact ⟨hlub, hlt⟩

/-- With width and centre held, closed coverage gives a strictly
unattained supremum with a factor of one half per endpoint observation. -/
theorem held_centre_supremum {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {a m : ℝ}
    (ha : 0 < a) (hx : ∀ i, |x i - m| ≤ a) :
    let u := ∏ i, (if |x i - m| = a then (1 : ℝ) / 2 else 1) / (2 * a)
    IsLUB (likelihood x a m '' Ioi 0) u ∧ ∀ s > 0, likelihood x a m s < u := by
  dsimp only
  have hs (s : ℝ) (hp : 0 < s) : likelihood x a m s <
      ∏ i, (if |x i - m| = a then (1 : ℝ) / 2 else 1) / (2 * a) := by
    simp only [likelihood, ha.ne', ite_false]
    have hne : (Finset.univ : Finset (Fin n)).Nonempty := ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    apply Finset.prod_lt_prod_of_nonempty₀ _ _ hne
    · intro i _
      apply div_pos _ (by positivity)
      exact sub_pos.mpr (cdf_strictMono ((div_lt_div_iff_of_pos_right hp).mpr (by linarith)))
    · intro i _
      apply (div_lt_div_iff_of_pos_right (by positivity : 0 < 2 * a)).mpr
      by_cases h : |x i - m| = a
      · rw [ite_eq_left h]
        rcases (abs_eq ha.le).mp h with he | he
        · rw [he, sub_self, zero_div, cdf_zero]
          linarith [cdf_lt_one ((a + a) / s)]
        · rw [he, neg_add_cancel, zero_div, cdf_zero]
          linarith [cdf_pos ((-a - a) / s)]
      · rw [ite_eq_right h]
        linarith [cdf_pos ((x i - m - a) / s), cdf_lt_one ((x i - m + a) / s)]
  refine ⟨⟨?_, ?_⟩, hs⟩
  · rintro _ ⟨s, hs', rfl⟩
    exact (hs s hs').le
  · intro v hv
    apply le_of_tendsto (held_centre_uniform_path ha hx)
    filter_upwards [eventually_mem_nhdsWithin] with s (hs' : 0 < s)
    exact hv ⟨s, hs', rfl⟩

/-- At width zero and positive scale, the Bhattacharjee log likelihood is
exactly the ordinary normal log likelihood in the same location and scale. -/
theorem normal_member_log {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    0 < likelihood x 0 m s ∧ log (likelihood x 0 m s) = Normal.logLik x m s := by
  have hp (i : Fin n) : 0 < φ ((x i - m) / s) / s := div_pos (phi_pos _) hs
  simp only [likelihood, ite_true]
  refine ⟨Finset.prod_pos (fun i _ ↦ hp i), ?_⟩
  rw [log_prod (fun i _ ↦ (hp i).ne'), Normal.logLik_formula x m hs]
  have hc : 0 < 2 * π := by positivity
  have hterm (i : Fin n) :
      log (φ ((x i - m) / s) / s) =
        -(log (2 * π)) / 2 - log s - (x i - m)^2 / (2 * s^2) := by
    rw [log_div (phi_pos _).ne' hs.ne', phi_formula,
      log_mul (div_pos one_pos (sqrt_pos.mpr hc)).ne' (exp_pos _).ne', log_exp,
      log_div one_ne_zero (sqrt_pos.mpr hc).ne', log_one, log_sqrt hc.le]
    simp only [div_pow]
    ring
  simp only [hterm, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, Normal.residual, Finset.sum_div]
  ring

/-- The complete positive-scale argmax set at width zero is the normal
argmax set, including on constant samples. -/
theorem normal_member_argmax {n : ℕ} (x : Fin n → ℝ) {p : ℝ × ℝ} (hp : p ∈ Normal.domain) :
    IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x 0 q.1 q.2) Normal.domain p ↔
      IsMaxOn (fun q : ℝ × ℝ ↦ Normal.logLik x q.1 q.2) Normal.domain p := by
  have h (q : ℝ × ℝ) (hq : q ∈ Normal.domain) :=
    normal_member_log x q.1 (show 0 < q.2 from hq.2)
  constructor
  · intro hm q hq
    change Normal.logLik x q.1 q.2 ≤ Normal.logLik x p.1 p.2
    rw [← (h q hq).2, ← (h p hp).2]
    exact log_le_log (h q hq).1 (hm hq)
  · intro hm q hq
    apply (log_le_log_iff (h q hq).1 (h p hp).1).mp
    rw [(h q hq).2, (h p hp).2]
    exact hm hq

end UnivariateML.HeldSweepBhatt

end
