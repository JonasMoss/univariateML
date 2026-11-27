module

public import UnivariateML.Family.TDist
public import Mathlib.Topology.Order.Compact

import Mathlib.Tactic

set_option linter.style.header false

/-! # Student location with shape and scale held -/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.StudentLocation

noncomputable section

variable {n : ℕ}

private theorem logLik_continuous (x : Fin n → ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    Continuous (fun μ ↦ TDist.logLik x μ ν a) := by
  apply continuous_finsetSum
  intro i _
  refine continuous_const.sub (continuous_const.mul (Continuous.log (by fun_prop) ?_))
  intro μ
  positivity

private theorem logLik_escape (x : Fin n → ℝ) (hn : 0 < n) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    Tendsto (fun μ ↦ TDist.logLik x μ ν a) (cocompact ℝ) atBot := by
  let c := TDist.logNormalizer ν - log a
  let f : Fin n → ℝ → ℝ := fun i μ ↦ c -
    (ν + 1) / 2 * log (1 + (x i - μ)^2 / (ν * a^2))
  let i : Fin n := ⟨0, hn⟩
  have hd : Tendsto (fun μ : ℝ ↦ x i - μ) atTop atBot :=
    tendsto_atBot_add_const_left _ _ tendsto_neg_atTop_atBot
  have hu : Tendsto (fun μ : ℝ ↦ x i - μ) atBot atTop :=
    tendsto_atTop_add_const_left _ _ tendsto_neg_atBot_atTop
  have hAbs : Tendsto (fun μ : ℝ ↦ |x i - μ|) (cocompact ℝ) atTop := by
    rw [cocompact_eq_atBot_atTop]
    exact (tendsto_abs_atTop_atTop.comp hu).sup (tendsto_abs_atBot_atTop.comp hd)
  have hs : Tendsto (fun μ : ℝ ↦ (x i - μ)^2) (cocompact ℝ) atTop := by
    simpa only [Function.comp_def, sq_abs] using (tendsto_pow_atTop (by decide : 2 ≠ 0)).comp hAbs
  have hl : Tendsto (fun μ : ℝ ↦ log (1 + (x i - μ)^2 / (ν * a^2)))
      (cocompact ℝ) atTop :=
    tendsto_log_atTop.comp (tendsto_atTop_add_const_left _ _ (hs.atTop_div_const (by positivity)))
  have hf : Tendsto (f i) (cocompact ℝ) atBot := by
    exact tendsto_atBot_add_const_left _ _
      (tendsto_neg_atTop_atBot.comp (hl.const_mul_atTop (by positivity : 0 < (ν+1)/2)))
  have hb (j : Fin n) (μ : ℝ) : f j μ ≤ c := by
    dsimp [f]
    exact sub_le_self _
      (mul_nonneg (by positivity) (log_nonneg (le_add_of_nonneg_right (by positivity))))
  have hsum (μ : ℝ) : TDist.logLik x μ ν a ≤ f i μ + (n : ℝ) * |c| := by
    change ∑ j, f j μ ≤ _
    rw [← Finset.add_sum_erase _ _ (Finset.mem_univ i)]
    gcongr
    calc
      ∑ j ∈ Finset.univ.erase i, f j μ ≤ ∑ _j ∈ Finset.univ.erase i, |c| :=
        Finset.sum_le_sum fun j _ ↦ (hb j μ).trans (le_abs_self c)
      _ ≤ ∑ _j : Fin n, |c| := Finset.sum_le_sum_of_subset_of_nonneg
        (Finset.erase_subset _ _) (fun _ _ _ ↦ abs_nonneg _)
      _ = _ := by simp
  exact tendsto_atBot_mono hsum (tendsto_atBot_add_const_right _ _ hf)

/-- At positive held df and standard deviation, both infinite-location
ends have zero likelihood for every nonempty finite sample. -/
theorem std_location_ends (x : Fin n → ℝ) (hn : 0 < n) {ν σ : ℝ}
    (hν : 2 < ν) (hσ : 0 < σ) :
    Tendsto (fun μ ↦ exp (TDist.stdLogLik x μ ν σ)) atBot (𝓝 0) ∧
      Tendsto (fun μ ↦ exp (TDist.stdLogLik x μ ν σ)) atTop (𝓝 0) := by
  have ha : 0 < TDist.ordinaryScale ν σ := by
    unfold TDist.ordinaryScale
    exact mul_pos hσ (sqrt_pos.mpr (div_pos (by linarith) (by linarith)))
  have h := logLik_escape x hn (by linarith : 0 < ν) ha
  simp_rw [TDist.stdLogLik_eq x _ hν hσ]
  exact ⟨tendsto_exp_atBot.comp (h.mono_left atBot_le_cocompact),
    tendsto_exp_atBot.comp (h.mono_left atTop_le_cocompact)⟩

/-- At positive held df and standard deviation every nonempty finite sample
has a location maximizer. -/
theorem std_location_attainment (x : Fin n → ℝ) (hn : 0 < n) {ν σ : ℝ}
    (hν : 2 < ν) (hσ : 0 < σ) :
    ∃ μ, IsMaxOn (fun m ↦ TDist.stdLogLik x m ν σ) univ μ := by
  have hv : 0 < ν := by linarith
  have ha : 0 < TDist.ordinaryScale ν σ := by
    unfold TDist.ordinaryScale
    exact mul_pos hσ (sqrt_pos.mpr (div_pos (by linarith) hv))
  simp_rw [TDist.stdLogLik_eq x _ hν hσ]
  obtain ⟨m, hm⟩ := (logLik_continuous x hv ha).exists_forall_ge (logLik_escape x hn hv ha)
  exact ⟨m, fun y _ ↦ hm y⟩

private theorem two_point_maximizers {c k b q : ℝ} (hk : 0 < k) (hq : 0 < q)
    (hb : q < b ^ 2) (m : ℝ) :
    IsMaxOn (fun u : ℝ ↦ (c - k * log (1 + (-b - u)^2 / q)) +
      (c - k * log (1 + (b - u)^2 / q))) univ m ↔
      m = sqrt (b^2 - q) ∨ m = -sqrt (b^2 - q) := by
  let P : ℝ → ℝ := fun u ↦ (1 + (-b - u)^2 / q) * (1 + (b - u)^2 / q)
  have hP (u : ℝ) : 0 < P u := by dsimp [P]; positivity
  have hLog (u : ℝ) :
      (c - k * log (1 + (-b - u)^2 / q)) + (c - k * log (1 + (b - u)^2 / q)) =
        2*c - k * log (P u) := by
    dsimp only [P]
    rw [log_mul (by positivity) (by positivity)]
    ring
  have hAlg (u : ℝ) : P u = ((u^2 + q - b^2)^2 + 4*b^2*q)/q^2 := by
    dsimp [P]
    field_simp [hq.ne']; ring
  have hs : sqrt (b^2 - q)^2 = b^2 - q := sq_sqrt (by linarith)
  have hCompare (u v : ℝ) :
      2*c - k * log (P u) ≤ 2*c - k * log (P v) ↔
        (v^2 + q - b^2)^2 ≤ (u^2 + q - b^2)^2 := by
    rw [sub_le_sub_iff_left, mul_le_mul_iff_right₀ hk, log_le_log_iff (hP v) (hP u),
      hAlg, hAlg, div_le_div_iff_of_pos_right (sq_pos_of_pos hq), add_le_add_iff_right]
  simp only [isMaxOn_iff]
  constructor
  · intro hm
    have H := hm (sqrt (b^2 - q)) (mem_univ _)
    simp only [hLog, hCompare, hs] at H
    have hz0 : (m^2 + q - b^2)^2 = 0 := by linarith [sq_nonneg (m^2+q-b^2)]
    have hz : m^2 = b^2 - q := by linarith [sq_eq_zero_iff.mp hz0]
    have hz' : (m - sqrt (b^2-q)) * (m + sqrt (b^2-q)) = 0 := by nlinarith
    rcases mul_eq_zero.mp hz' with h | h
    · exact Or.inl (by linarith)
    · exact Or.inr (by linarith)
  · intro hm u _
    rw [hLog, hLog, hCompare]
    rcases hm with rfl | rfl <;> nlinarith [sq_nonneg (u^2+q-b^2)]

/-- With df three and standard deviation one, the sample `(-2, 2)`
has exactly the two stated maximizing locations. -/
theorem std_two_point_maximizers (m : ℝ) :
    IsMaxOn (fun u ↦ TDist.stdLogLik ![-2, 2] u 3 1) univ m ↔
      m = sqrt 3 ∨ m = -sqrt 3 := by
  unfold TDist.stdLogLik
  simp only [Fin.sum_univ_two, Matrix.cons_val_zero, Matrix.cons_val_one]
  convert two_point_maximizers
    (c := log (Gamma ((3+1)/2)) - log (Gamma (3/2)) - log ((3-2)*π)/2 - log 1)
    (k := (3+1)/2) (b := 2) (q := (3-2)*1^2) (by norm_num) (by norm_num) (by norm_num) m
    using 1; norm_num

/-- With df and ordinary scale held positive, both infinite-location ends
have zero likelihood for every nonempty finite sample. -/
theorem lst_location_ends (x : Fin n → ℝ) (hn : 0 < n) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    Tendsto (fun μ ↦ TDist.likelihood x μ ν a) atBot (𝓝 0) ∧
      Tendsto (fun μ ↦ TDist.likelihood x μ ν a) atTop (𝓝 0) := by
  have h := logLik_escape x hn hν ha
  exact ⟨tendsto_exp_atBot.comp (h.mono_left atBot_le_cocompact),
    tendsto_exp_atBot.comp (h.mono_left atTop_le_cocompact)⟩

/-- With df and ordinary scale held positive every nonempty finite sample
has a location maximizer. -/
theorem lst_location_attainment (x : Fin n → ℝ) (hn : 0 < n) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    ∃ μ, IsMaxOn (fun m ↦ TDist.logLik x m ν a) univ μ := by
  obtain ⟨m, hm⟩ := (logLik_continuous x hν ha).exists_forall_ge (logLik_escape x hn hν ha)
  exact ⟨m, fun y _ ↦ hm y⟩

/-- With df one and scale three, the sample `(-5, 5)` has exactly the
maximizing locations `-4` and `4`. -/
theorem lst_two_point_maximizers (m : ℝ) :
    IsMaxOn (fun u ↦ TDist.logLik ![-5, 5] u 1 3) univ m ↔ m = 4 ∨ m = -4 := by
  unfold TDist.logLik
  simp only [Fin.sum_univ_two, Matrix.cons_val_zero, Matrix.cons_val_one]
  have h : sqrt ((5 : ℝ)^2 - 1*3^2) = 4 := by
    rw [show (5 : ℝ)^2 - 1*3^2 = 4^2 by norm_num, sqrt_sq (by norm_num)]
  rw [← h]
  exact two_point_maximizers (k := (1+1)/2) (b := 5) (q := 1*3^2)
    (by norm_num) (by norm_num) (by norm_num) m

end
end UnivariateML.StudentLocation
end
