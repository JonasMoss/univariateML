module

public import UnivariateML.Family.Discrete4NBinom

import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
import Mathlib.Analysis.SpecialFunctions.Pow.Continuity
import Mathlib.Analysis.SpecialFunctions.PolynomialExp
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Filter Real
open scoped Topology BigOperators

namespace UnivariateML.Discrete5NBinom

open NegBinomial Discrete4NBinom

private noncomputable def risingProduct {n : ℕ} (x : Fin n → ℕ) (r : ℝ) : ℝ :=
  ∏ i, ∏ j ∈ Finset.range (x i), (r + (j : ℝ))

private theorem regular_likelihood {n : ℕ} (x : Fin n → ℕ) {r p : ℝ}
    (hr : 0 < r) (hp : p ∈ Ioo 0 1) :
    likelihood x r p = risingProduct x r * exp (-constant x) *
      p ^ ((n : ℝ) * r) * (1 - p) ^ total x := by
  classical
  have he : likelihood x r p = exp (logLik x r p) := by
    by_cases hx : ∀ i, x i = 0
    · simpa only [likelihood, ite_eq_left hx] using zeroLikelihood_eq_exp_logLik x hx hp.1
    · simp [likelihood, hx, hr, hp]
  rw [he, logLik, sub_eq_add_neg, exp_add, exp_add, exp_add,
    mul_comm ((n : ℝ) * r) (log p), mul_comm (total x) (log (1-p)),
    ← rpow_def_of_pos hp.1, ← rpow_def_of_pos (sub_pos.mpr hp.2),
    Real.exp_sum]
  unfold risingProduct
  have hpos (j : ℕ) : 0 < r + (j : ℝ) := by positivity
  simp_rw [Real.exp_sum, exp_log (hpos _)]
  ring

private theorem size_zero_limit {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ}
    (hr : 0 < r) :
    Tendsto (likelihood x r) (𝓝[>] 0) (𝓝 0) := by
  have hf : ContinuousAt (fun p : ℝ ↦ risingProduct x r * exp (-constant x) *
      p ^ ((n : ℝ) * r) * (1 - p) ^ total x) 0 := by
    exact (continuousAt_const.mul (continuousAt_id.rpow_const (Or.inr (by positivity)))).mul
      ((continuousAt_const.sub continuousAt_id).rpow_const (Or.inl (by norm_num)))
  have h₀ : (0 : ℝ) ^ ((n : ℝ) * r) = 0 := zero_rpow (show 0 < (n : ℝ) * r by positivity).ne'
  have h := hf.tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
  simp only [h₀, mul_zero, zero_mul] at h
  apply h.congr'
  have ha : ∀ᶠ p : ℝ in 𝓝[>] 0, 0 < p := self_mem_nhdsWithin
  have hb : ∀ᶠ p : ℝ in 𝓝[>] 0, p < 1 :=
    (eventually_lt_nhds zero_lt_one).filter_mono nhdsWithin_le_nhds
  filter_upwards [ha,hb] with p hp hp'
  exact (regular_likelihood x hr ⟨hp,hp'⟩).symm

/-- With positive held size the excluded probability endpoint has zero
likelihood, strictly below an admissible fit. At size zero on compatible
data, the entire probability interval gives the zero atom. -/
theorem held_size_boundary {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ}
    (hr : 0 ≤ r) (hc : r = 0 → ∀ i, x i = 0) :
    (r = 0 ∧ ∀ p ∈ Ioc (0 : ℝ) 1, likelihood x r p = 1) ∨
      (0 < r ∧ Tendsto (likelihood x r) (𝓝[>] 0) (𝓝 0) ∧
        ∃ p ∈ Ioc (0 : ℝ) 1, 0 < likelihood x r p ∧
          IsMaxOn (likelihood x r) (Ioc 0 1) p) := by
  rcases eq_or_lt_of_le hr with h | h
  · exact Or.inl ⟨h.symm, fun p _ ↦ by simp [likelihood, hc h.symm, h.symm, zeroLikelihood]⟩
  · refine Or.inr ⟨h, size_zero_limit hn x h, ?_⟩
    by_cases hx : ∀ i, x i = 0
    · refine ⟨1,⟨zero_lt_one,le_rfl⟩,?_, ((held_size hn x hr).1 hx 1
        ⟨zero_lt_one,le_rfl⟩).mpr (Or.inr rfl)⟩
      simp [likelihood,hx,zeroLikelihood]
    · obtain ⟨hp,hM⟩ := (held_size hn x hr).2 hx |>.1 h
      have hp' : prob x r ∈ Ioo 0 1 := by
        have hm : 0 < mean x := (mean_pos_iff hn x).2 (by
          push Not at hx
          obtain ⟨i, hi⟩ := hx
          exact ⟨i, Nat.pos_of_ne_zero hi⟩)
        exact ⟨hp.1,(div_lt_one (by positivity : 0 < r + mean x)).mpr (by linarith)⟩
      refine ⟨prob x r, hp, ?_, (hM _ hp).mpr rfl⟩
      simpa [likelihood,hx,h,hp'] using exp_pos (logLik x r (prob x r))

private theorem probability_infinite_limit {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioo 0 1) :
    Tendsto (fun r ↦ likelihood x r p) atTop (𝓝 0) := by
  let a := -(n : ℝ) * log p
  have ha : 0 < a := mul_pos_of_neg_of_neg (neg_lt_zero.mpr (Nat.cast_pos.mpr hn))
    (log_neg hp.1 hp.2)
  let P : Polynomial ℝ :=
    ∏ i, ∏ j ∈ Finset.range (x i),
      (Polynomial.C a⁻¹ * Polynomial.X + Polynomial.C (j : ℝ))
  have hev (r : ℝ) : P.eval (a * r) = risingProduct x r := by
    simp [P, risingProduct, Polynomial.eval_prod, ← mul_assoc, inv_mul_cancel₀ ha.ne']
  have hT : Tendsto (fun r : ℝ ↦ a * r) atTop atTop :=
    tendsto_id.const_mul_atTop ha
  have h := (P.tendsto_div_exp_atTop.comp hT).mul_const
    (exp (-constant x) * (1-p) ^ total x)
  simp only [zero_mul] at h
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
  change P.eval (a*r) / exp (a*r) * _ = _
  rw [hev, regular_likelihood x hr hp, rpow_def_of_pos hp.1, div_eq_mul_inv, ← exp_neg]
  have he : -(a*r) = log p * ((n : ℝ)*r) := by dsimp [a]; ring
  rw [he]
  ring

/-- An interior held probability gives strictly losing likelihood at
infinite size, including on zeros. The compatible probability-one case
has a flat, maximizing size ray.
 
## Deviations

The atlas's “No escape” means no excluded value can beat the attainable
optimum. At held probability one, maximizing sequences can diverge. -/
theorem held_probability_boundary {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioc 0 1) (hc : p = 1 → ∀ i, x i = 0) :
    (p = 1 ∧ ∀ r ∈ Ici (0 : ℝ), likelihood x r p = 1) ∨
      (p < 1 ∧ Tendsto (fun r ↦ likelihood x r p) atTop (𝓝 0) ∧
        ∃ r ∈ Ici (0 : ℝ), 0 < likelihood x r p ∧
          IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r) := by
  rcases eq_or_lt_of_le hp.2 with h | h
  · exact Or.inl ⟨h, fun r _ ↦ by simp [likelihood, hc h, h, zeroLikelihood]⟩
  · have hp' : p ∈ Ioo 0 1 := ⟨hp.1,h⟩
    refine Or.inr ⟨h, probability_infinite_limit hn x hp', ?_⟩
    by_cases hx : ∀ i, x i = 0
    · refine ⟨0,le_rfl,?_, ((held_probability hn x hp).1 hx 0 le_rfl).mpr (Or.inl rfl)⟩
      simp [likelihood,hx,zeroLikelihood]
    · obtain ⟨r,⟨hr,hM⟩,_⟩ := ((held_probability hn x hp).2 hx).1 h
      have hr' : 0 < r := by
        by_contra! H
        have he : r = 0 := le_antisymm H hr
        have h₁ := hM (show (1 : ℝ) ∈ Ici 0 from zero_le_one)
        change likelihood x 1 p ≤ likelihood x r p at h₁
        rw [he] at h₁
        simp [likelihood,hx,hp'] at h₁
        exact (exp_pos _).not_ge h₁
      refine ⟨r,hr,?_,hM⟩
      simpa [likelihood,hx,hr',hp'] using exp_pos (logLik x r p)

end UnivariateML.Discrete5NBinom

end
