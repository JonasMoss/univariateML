module

public import UnivariateML.Family.Transfers3

set_option linter.style.header false

/-! Reciprocal symmetry, concentration, and the unit-shape face paths. -/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.AllFreeBetaPrime
noncomputable section

variable {n : ℕ}

/-- Inversion exchanges the two shapes and inverts the scale. -/
theorem reciprocal_logLik {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) (p q : ℝ) :
    BetaPrime.logLik x b (p, q) =
      BetaPrime.logLik (fun i ↦ (x i)⁻¹) b⁻¹ (q, p) - ∑ i, 2 * log (x i) := by
  unfold BetaPrime.logLik
  rw [← Finset.sum_sub_distrib]
  apply Finset.sum_congr rfl
  intro i _
  dsimp
  have hp : 0 < x i / b := div_pos (hx i) hb
  have h : 1 + (x i)⁻¹ / b⁻¹ = (1 + x i / b) / (x i / b) := by
    field_simp [(hx i).ne', hb.ne']; ring
  have hβ : Beta.logBeta (p, q) = Beta.logBeta (q, p) := by
    simp [Beta.logBeta, Beta.normalizer, mul_comm, add_comm]
  rw [h, hβ, log_div (by positivity) hp.ne', log_div (hx i).ne' hb.ne',
    log_div (inv_ne_zero (hx i).ne') (inv_ne_zero hb.ne'), log_inv, log_inv]
  ring

/-- Constant data have unbounded beta-prime likelihood even at any fixed positive scale. -/
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (he : ∀ i j, x i = x j) :
    ¬ BddAbove {v | ∃ b > 0, ∃ p ∈ Beta.domain, BetaPrime.logLik x b p = v} := by
  intro h
  apply BetaPrime.joint_unbounded hn (σ := 1) one_pos hx
    (fun ⟨i, j, hne⟩ ↦ hne (he i j))
  apply h.mono
  rintro _ ⟨p, hp, rfl⟩
  exact ⟨1, one_pos, p, hp, rfl⟩

private theorem log_gamma_step {q : ℝ} (hq : 0 < q) :
    log (Real.Gamma (q + 1)) = log q + log (Real.Gamma q) := by
  rw [Real.Gamma_add_one hq.ne', log_mul hq.ne' (Gamma_pos_of_pos hq).ne']

private theorem log_gamma_nat_bound {q : ℝ} (hq : 0 < q) (m : ℕ) :
    log (Real.Gamma (q + m)) - log (Real.Gamma q) ≤ m * log (q + m) := by
  induction m with
  | zero => simp
  | succ m ih =>
    have hm : 0 < q + m := by positivity
    rw [Nat.cast_succ, ← add_assoc, log_gamma_step hm]
    have hl := log_le_log hm (show q + m ≤ q + m + 1 by linarith)
    linarith only [ih, hl, mul_le_mul_of_nonneg_left hl (Nat.cast_nonneg m : (0 : ℝ) ≤ m)]

private theorem log_gamma_ratio {p : ℝ} (hp : 0 < p) :
    Tendsto (fun h : ℝ ↦ log (Real.Gamma (h⁻¹ + p)) -
      log (Real.Gamma h⁻¹) - p * log h⁻¹) (𝓝[>] 0) (𝓝 0) := by
  obtain ⟨m, hm⟩ := exists_nat_gt p
  have hm0 : (0 : ℝ) < m := hp.trans hm
  have hb (c : ℝ) : Tendsto (fun h : ℝ ↦ p * log (1 + c * h)) (𝓝[>] 0) (𝓝 0) := by
    have hh : Tendsto (fun h : ℝ ↦ h) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
    simpa using (((hh.const_mul c).const_add 1).log (by simp)).const_mul p
  have hbnds : ∀ᶠ h : ℝ in 𝓝[>] 0, 0 < h ∧ h < 1 := by
    filter_upwards [self_mem_nhdsWithin, mem_nhdsWithin_of_mem_nhds
      (Iio_mem_nhds (zero_lt_one : (0 : ℝ) < 1))] with h hh h1
    exact ⟨hh, h1⟩
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' (hb (-1)) (hb m)
  · filter_upwards [hbnds] with h ⟨hh, h1⟩
    have hq : 1 < h⁻¹ := (one_lt_inv₀ hh).mpr h1
    have hlow := convexOn_log_Gamma.slope_mono_adjacent
      (x := h⁻¹ - 1) (y := h⁻¹) (z := h⁻¹ + p)
      (show 0 < h⁻¹ - 1 by linarith) (show 0 < h⁻¹ + p by positivity)
      (by linarith) (by linarith)
    simp only [Function.comp_apply, add_sub_cancel_left,
      show h⁻¹ - (h⁻¹ - 1) = 1 by ring, div_one] at hlow
    have hstep := log_gamma_step (show 0 < h⁻¹ - 1 by linarith)
    rw [sub_add_cancel] at hstep
    rw [show log (Real.Gamma h⁻¹) - log (Real.Gamma (h⁻¹ - 1)) = log (h⁻¹ - 1)
      by linarith] at hlow
    have harg : 1 + (-1 : ℝ) * h = (h⁻¹ - 1) / h⁻¹ := by
      field_simp
      ring
    rw [harg, log_div (by linarith : h⁻¹ - 1 ≠ 0) (inv_ne_zero hh.ne')]
    linarith only [(le_div_iff₀ hp).mp hlow]
  · filter_upwards [hbnds] with h ⟨hh, _⟩
    have hq := inv_pos.mpr hh
    have hhi := convexOn_log_Gamma.secant_mono
      (a := h⁻¹) (x := h⁻¹ + p) (y := h⁻¹ + m)
      (show 0 < h⁻¹ from hq) (show 0 < h⁻¹ + p by positivity)
      (show 0 < h⁻¹ + m by positivity)
      (by linarith) (by linarith) (by linarith)
    simp only [Function.comp_apply, add_sub_cancel_left] at hhi
    have hN := log_gamma_nat_bound hq m
    have hhigher : (log (Real.Gamma (h⁻¹ + m)) - log (Real.Gamma h⁻¹)) / m
        ≤ log (h⁻¹ + m) := by
      apply (div_le_iff₀ hm0).2
      linarith only [hN]
    have harg : 1 + (m : ℝ) * h = (h⁻¹ + m) / h⁻¹ := by
      field_simp
    rw [harg, log_div (by positivity) hq.ne']
    linarith only [(div_le_iff₀ hp).mp (hhi.trans hhigher)]

/-- At any fixed positive first shape, sending the second shape and scale
to infinity at fixed ratio gives the corresponding gamma likelihood. -/
theorem gamma_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s p : ℝ} (hs : 0 < s) (hp : 0 < p) :
    Tendsto (fun h : ℝ ↦ BetaPrime.logLik x (s / h) (p, h⁻¹))
      (𝓝[>] 0) (𝓝 (∑ i, Gamma.logDensity p s⁻¹ (x i))) := by
  unfold BetaPrime.logLik
  apply tendsto_finsetSum
  intro i _
  let t := x i / s
  have hd : HasDerivAt (fun h : ℝ ↦ log (1 + h * t)) t 0 := by
    simpa using (((hasDerivAt_id (0 : ℝ)).mul_const t).const_add 1).log
      (by simp : 1 + id (0 : ℝ) * t ≠ 0)
  have h0 : Tendsto (fun h : ℝ ↦ h) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hu : Tendsto (fun h : ℝ ↦ h⁻¹ * log (1 + h * t)) (𝓝[>] 0) (𝓝 t) := by
    simpa using hd.tendsto_slope_zero_right
  have hl : Tendsto (fun h : ℝ ↦ log (1 + h * t)) (𝓝[>] 0) (𝓝 0) := by
    simpa [Function.comp_def] using hd.continuousAt.tendsto.comp h0
  have hsum : Tendsto (fun h ↦ p * log (1 + h * t) +
      h⁻¹ * log (1 + h * t)) (𝓝[>] 0) (𝓝 t) := by
    simpa using (hl.const_mul p).add hu
  let C := Gamma.logDensity p s⁻¹ (x i) + t
  have ht : Tendsto (fun h ↦ C + (log (Real.Gamma (h⁻¹ + p)) -
      log (Real.Gamma h⁻¹) - p * log h⁻¹) -
        (p * log (1 + h * t) + h⁻¹ * log (1 + h * t))) (𝓝[>] 0)
      (𝓝 (Gamma.logDensity p s⁻¹ (x i))) := by
    simpa [C] using ((log_gamma_ratio hp).const_add C).sub hsum
  apply ht.congr'
  filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
  have hi := inv_pos.mpr hh
  have hβ : Beta.logBeta (p, h⁻¹) =
      log (Real.Gamma p) + log (Real.Gamma h⁻¹) - log (Real.Gamma (h⁻¹ + p)) := by
    simp only [Beta.logBeta, Beta.normalizer]
    have hgp := (Gamma_pos_of_pos hp).ne'
    have hgi := (Gamma_pos_of_pos hi).ne'
    rw [log_div (mul_ne_zero hgp hgi) (Gamma_pos_of_pos (add_pos hp hi)).ne',
      log_mul hgp hgi, add_comm p h⁻¹]
  have he : x i / (s / h) = h * t := by dsimp [t]; field_simp
  simp only [Gamma.logDensity, C, hβ]
  rw [he, log_mul hh.ne' (div_pos (hx i) hs).ne', log_div hs.ne' hh.ne',
    log_inv, log_inv, show log t = log (x i) - log s from log_div (hx i).ne' hs.ne']
  ring

/-- At any fixed positive second shape, reciprocal symmetry gives the
inverse-gamma likelihood as the first shape diverges and scale decreases. -/
theorem inverse_gamma_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s q : ℝ} (hs : 0 < s) (hq : 0 < q) :
    Tendsto (fun h : ℝ ↦ BetaPrime.logLik x (h / s) (h⁻¹, q))
      (𝓝[>] 0)
      (𝓝 ((∑ i, Gamma.logDensity q s⁻¹ (x i)⁻¹) - ∑ i, 2 * log (x i))) := by
  have ht := gamma_path (x := fun i ↦ (x i)⁻¹) (fun i ↦ inv_pos.mpr (hx i)) hs hq
  apply (ht.sub_const (∑ i, 2 * log (x i))).congr'
  filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
  rw [reciprocal_logLik hx (div_pos hh hs), inv_div]

end
end UnivariateML.AllFreeBetaPrime

end
