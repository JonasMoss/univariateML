module

public import UnivariateML.Family.TransformedGamma
public import Mathlib.Analysis.SpecialFunctions.Log.NegMulLog
public import Mathlib.Analysis.SpecialFunctions.Stirling
public import Mathlib.Analysis.SpecialFunctions.Exp

set_option linter.style.header false

/-! Transformed-gamma concentration and the free-scale power face. -/

@[expose] public section

open Real Filter Topology
open scoped Asymptotics
namespace UnivariateML.AllFreeTransformedGamma
noncomputable section

variable {n : ℕ}


private theorem avg_tendsto {α : Type*} {F : Filter α} {f : α → Fin n → ℝ} {g : Fin n → ℝ}
    (h : ∀ i, Tendsto (fun q ↦ f q i) F (𝓝 (g i))) :
    Tendsto (fun q ↦ Gamma.avg (f q)) F (𝓝 (Gamma.avg g)) := by
  exact (tendsto_finsetSum Finset.univ (fun i _ ↦ h i)).div_const _

/-- The free scale approaches the endpoint while leaving zero exponential
loss even for observations tied at the endpoint. -/
theorem power_path {x : Fin n → ℝ} {A d : ℝ} (hd : 0 < d) (_hA : 0 < A)
    (hx : ∀ i, 0 < x i ∧ x i ≤ A) :
    Tendsto (fun q : ℝ ↦ TransformedGamma.logLik x (d * q) q⁻¹
      (exp (log A - q * log q))) (𝓝[>] 0)
      (𝓝 (log d - Gamma.avgLog x + Gamma.avg (fun i ↦ d * (log (x i) - log A)))) := by
  let y : Fin n → ℝ := fun i ↦ log (x i) - log A
  have h0 : Tendsto (fun q : ℝ ↦ q) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hqlog : Tendsto (fun q : ℝ ↦ q * log q) (𝓝[>] 0) (𝓝 0) := by
    simpa using (continuous_mul_log.continuousAt (x := (0 : ℝ))).tendsto.mono_left
      (nhdsWithin_le_nhds (s := Set.Ioi (0 : ℝ)))
  have hΓ : Tendsto (fun q : ℝ ↦ log (Real.Gamma (d * q + 1))) (𝓝[>] 0) (𝓝 0) := by
    have h := (h0.const_mul d).add_const 1
    simp only [mul_zero, zero_add] at h
    have hΓ1 := (Real.hasDerivAt_Gamma_of_pos (s := 1) one_pos).continuousAt.tendsto.comp h
    simp only [Real.Gamma_one, Function.comp_def] at hΓ1
    simpa using hΓ1.log one_ne_zero
  have hL (i : Fin n) : Tendsto
      (fun q : ℝ ↦ q * exp (q⁻¹ * y i) - d * (y i + q * log q))
      (𝓝[>] 0) (𝓝 (-d * y i)) := by
    have hsmall : Tendsto (fun q : ℝ ↦ q * exp (q⁻¹ * y i)) (𝓝[>] 0) (𝓝 0) := by
      apply tendsto_of_tendsto_of_tendsto_of_le_of_le'
        tendsto_const_nhds h0
      · filter_upwards [self_mem_nhdsWithin] with q (hq : 0 < q)
        exact mul_nonneg hq.le (exp_pos _).le
      · filter_upwards [self_mem_nhdsWithin] with q (hq : 0 < q)
        have hy : y i ≤ 0 := sub_nonpos.mpr (log_le_log (hx i).1 (hx i).2)
        simpa using mul_le_mul_of_nonneg_left
          (exp_le_one_iff.mpr (mul_nonpos_of_nonneg_of_nonpos (inv_nonneg.mpr hq.le) hy))
          hq.le
    simpa [neg_mul] using hsmall.sub ((hqlog.const_add (y i)).const_mul d)
  have hlim := (((hΓ.const_sub (log d)).sub_const (Gamma.avgLog x)).sub (avg_tendsto hL))
  have havg : Gamma.avg (fun i ↦ -d * y i) = -Gamma.avg (fun i ↦ d * y i) := by
    simp only [Gamma.avg, neg_mul, Finset.sum_neg_distrib, neg_div]
  simp only [sub_zero, havg, sub_neg_eq_add] at hlim
  refine hlim.congr' ?_
  filter_upwards [self_mem_nhdsWithin] with q (hq : 0 < q)
  have hdq := mul_pos hd hq
  have hg := Real.Gamma_pos_of_pos hdq
  have hnormal : log q⁻¹ - log (Real.Gamma (d * q)) =
      log d - log (Real.Gamma (d * q + 1)) := by
    rw [Real.Gamma_add_one hdq.ne', log_mul hdq.ne' hg.ne', log_mul hd.ne' hq.ne', log_inv]
    ring
  rw [TransformedGamma.logLik, hnormal]
  congr 1
  unfold Gamma.avg
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  unfold TransformedGamma.loss
  simp only [log_exp]
  rw [show log (x i) - (log A - q * log q) = y i + q * log q by dsimp [y]; ring]
  have he : q⁻¹ * (y i + q * log q) = q⁻¹ * y i + log q := by
    field_simp
  rw [he, exp_add, exp_log hq]
  field_simp

/-- The reciprocal path gives the Pareto log likelihood, including lower-endpoint ties. -/
theorem inverse_power_path {x : Fin n → ℝ} {A d : ℝ} (hd : 0 < d) (hA : 0 < A)
    (hx : ∀ i, A ≤ x i) :
    Tendsto (fun q : ℝ ↦ InverseTransformedGamma.logLik x (d * q) q⁻¹
      (exp (log A⁻¹ - q * log q))⁻¹) (𝓝[>] 0)
      (𝓝 (log d - Gamma.avgLog x + Gamma.avg (fun i ↦ d * (log A - log (x i))))) := by
  have hpos (i : Fin n) : 0 < x i := hA.trans_le (hx i)
  have hp := (power_path (x := InverseTransformedGamma.reciprocal x) hd (inv_pos.mpr hA)
    (fun i ↦ ⟨inv_pos.mpr (hpos i), inv_le_inv₀ (hpos i) hA |>.mpr (hx i)⟩))
  have h := hp.add_const (-2 * Gamma.avgLog x)
  simp only [InverseTransformedGamma.logLik_reciprocal, inv_inv]
  have hJ : Gamma.avgLog (InverseTransformedGamma.reciprocal x) = -Gamma.avgLog x := by
    simp [Gamma.avgLog, Gamma.avg, InverseTransformedGamma.reciprocal, log_inv,
      Finset.sum_neg_distrib, neg_div]
  rw [hJ] at h
  have hval (i : Fin n) :
      log (InverseTransformedGamma.reciprocal x i) - log A⁻¹ = log A - log (x i) := by
    simp [InverseTransformedGamma.reciprocal, log_inv]
    ring
  simp only [hval] at h
  convert h using 2
  ring

private theorem exp_quadratic_limit (z : ℝ) :
    Tendsto (fun j : ℕ ↦ (j : ℝ) *
      (exp (z / sqrt j) - 1 - z / sqrt j)) atTop (𝓝 (z ^ 2 / 2)) := by
  have h : (fun u : ℝ ↦ exp u - (1 + u + u ^ 2 / 2)) =o[𝓝 0] (fun u ↦ u ^ 2) := by
    convert Real.exp_sub_sum_range_succ_isLittleO_pow 2 using 1
    funext u
    norm_num [Finset.sum_range_succ]
  have h' := (Asymptotics.isLittleO_iff_tendsto
    (fun u hu ↦ by have : u = 0 := eq_zero_of_pow_eq_zero hu; simp [this])).mp h
  have hnat : Tendsto (fun j : ℕ ↦ (j : ℝ)) atTop atTop := tendsto_natCast_atTop_atTop
  have hscale := (tendsto_inv_atTop_zero.comp (tendsto_sqrt_atTop.comp hnat)).const_mul z
  simp only [mul_zero] at hscale
  have hrem := (h'.comp hscale).const_mul (z ^ 2)
  simp only [mul_zero, Function.comp_def] at hrem
  have hlim := hrem.add_const (z ^ 2 / 2)
  simp only [zero_add] at hlim
  apply hlim.congr'
  filter_upwards [eventually_gt_atTop (0 : ℕ)] with j hj
  have hj' : (0 : ℝ) < j := by exact_mod_cast hj
  have hs := sqrt_pos.mpr hj'
  by_cases hz : z = 0
  · simp [hz]
  · change z^2 * ((exp (z * (sqrt j)⁻¹) - (1 + z * (sqrt j)⁻¹ +
        (z * (sqrt j)⁻¹)^2 / 2)) / (z * (sqrt j)⁻¹)^2) + z^2 / 2 =
      (j : ℝ) * (exp (z / sqrt j) - 1 - z / sqrt j)
    rw [← div_eq_mul_inv, ← sq_sqrt hj'.le, sqrt_sq hs.le]
    field_simp [hz, hs.ne']
    ring

/-- Integer gamma shapes tending to infinity give a lognormal likelihood
path at any fixed log location and positive log scale. -/
theorem lognormal_path (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun j : ℕ ↦ TransformedGamma.logLik x j (1 / (σ * sqrt j))
      (exp (μ - log j * (σ * sqrt j)))) atTop
      (𝓝 (Gamma.avg (fun i ↦ -log σ - log (2 * π) / 2 - log (x i) -
        ((log (x i) - μ) / σ)^2 / 2))) := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  let z : Fin n → ℝ := fun i ↦ (log (x i) - μ) / σ
  let C : ℕ → ℝ := fun j ↦ -log σ - log 2 / 2 - log (Stirling.stirlingSeq j)
  have hπ := sqrt_pos.mpr pi_pos
  have hC : Tendsto C atTop (𝓝 (-log σ - log (2 * π) / 2)) := by
    have h := (Stirling.tendsto_stirlingSeq_sqrt_pi.log hπ.ne').const_sub (-log σ - log 2 / 2)
    convert h using 2
    rw [log_sqrt pi_pos.le, log_mul two_ne_zero pi_ne_zero]
    ring
  have hf (i : Fin n) := (hC.sub_const (log (x i))).sub (exp_quadratic_limit (z i))
  refine (avg_tendsto hf).congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℕ)] with j hj
  have hj' : (0 : ℝ) < j := by exact_mod_cast hj
  have hs := sqrt_pos.mpr hj'
  let p := 1 / (σ * sqrt j)
  let w (i : Fin n) := z i / sqrt j
  have hnorm : log p - log (Real.Gamma j) + j * log j - j = C j := by
    have hQ := Real.Gamma_add_one hj'.ne'
    rw [Real.Gamma_nat_eq_factorial] at hQ
    have hQlog : log (j.factorial : ℝ) = log j + log (Real.Gamma j) := by
      rw [hQ, log_mul hj'.ne' (Gamma_pos_of_pos hj').ne']
    dsimp [p, C]
    rw [log_div one_ne_zero (mul_pos hσ hs).ne', log_mul hσ.ne' hs.ne',
      log_sqrt hj'.le, Stirling.log_stirlingSeq_formula, hQlog,
      log_mul two_ne_zero hj'.ne', log_div hj'.ne' (exp_pos 1).ne', log_exp, log_one]
    ring
  have harg (i : Fin n) : p * (log (x i) - (μ - log j * (σ * sqrt j))) =
      w i + log j := by
    dsimp [p, w, z]
    field_simp
    ring
  have hloss (i : Fin n) :
      TransformedGamma.loss j (p * (log (x i) - (μ - log j * (σ * sqrt j)))) =
        j * (exp (w i) - 1 - w i) + j - j * log j := by
    unfold TransformedGamma.loss
    rw [harg, exp_add, exp_log hj']
    ring
  change Gamma.avg (fun i ↦ C j - log (x i) -
    j * (exp (w i) - 1 - w i)) =
    TransformedGamma.logLik x j p (exp (μ - log j * (σ * sqrt j)))
  simp only [TransformedGamma.logLik, log_exp, hloss, Gamma.avgLog, Gamma.avg,
    Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  field_simp [hn']
  linear_combination (-(n : ℝ)) * hnorm

/-- Reciprocation gives the lognormal likelihood at reflected log location. -/
theorem inverse_lognormal_path (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun j : ℕ ↦ InverseTransformedGamma.logLik x j (1 / (σ * sqrt j))
      (exp (-μ - log j * (σ * sqrt j)))⁻¹) atTop
      (𝓝 (Gamma.avg (fun i ↦ -log σ - log (2 * π) / 2 - log (x i) -
        ((log (x i) - μ) / σ)^2 / 2))) := by
  have ht := lognormal_path hn (InverseTransformedGamma.reciprocal x) (-μ) hσ
  have h := ht.add_const (-2 * Gamma.avgLog x)
  simp only [InverseTransformedGamma.logLik_reciprocal, inv_inv]
  convert h using 2
  have hs (i : Fin n) :
      ((log (InverseTransformedGamma.reciprocal x i) - -μ) / σ)^2 =
        ((log (x i) - μ) / σ)^2 := by
    simp only [InverseTransformedGamma.reciprocal, log_inv]
    ring
  simp_rw [hs]
  simp only [Gamma.avgLog, Gamma.avg, InverseTransformedGamma.reciprocal, log_inv,
    Finset.sum_sub_distrib, Finset.sum_neg_distrib]
  ring

/-- When the mean gap to the supported endpoint in oriented log units is
positive, its reciprocal is the unique rate optimizing the face. -/
theorem face_rate_optimized {D : ℝ} (hD : 0 < D) :
    ∃! d, d ∈ Set.Ioi (0 : ℝ) ∧
      IsMaxOn (fun r ↦ log r - r * D) (Set.Ioi 0) d := by
  have hd : 0 < D⁻¹ := inv_pos.mpr hD
  have hstrict (r : ℝ) (hr : 0 < r) (hne : r ≠ D⁻¹) :
      log r - r * D < log D⁻¹ - D⁻¹ * D := by
    have hneq : r * D ≠ 1 := by
      intro he
      apply hne
      field_simp
      exact he
    have h := log_lt_sub_one_of_pos (mul_pos hr hD) hneq
    rw [log_mul hr.ne' hD.ne'] at h
    rw [log_inv, inv_mul_cancel₀ hD.ne']
    linarith
  refine ⟨D⁻¹, ⟨hd, ?_⟩, ?_⟩
  · intro r hr
    by_cases h : r = D⁻¹
    · simp [h]
    · exact (hstrict r hr h).le
  · rintro r ⟨hr, hm⟩
    by_contra! hne
    exact (hstrict r hr hne).not_ge (isMaxOn_iff.mp hm _ hd)

end
end UnivariateML.AllFreeTransformedGamma

end
