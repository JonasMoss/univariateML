module

public import UnivariateML.Family.Endpoint2FellerPareto
public import UnivariateML.Family.Transfers3

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Pareto III threshold paths

The log-logistic excess likelihood is the Feller–Pareto kernel with
both beta shapes equal to one. The collapse theorem covers held scale
when it equals the common positive excess.
-/

@[expose] public section

open Real Filter Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2ParetoIII

noncomputable section

variable {n : ℕ}

private theorem kernel (d : Fin n → ℝ) (t c s : ℝ) (hc : c ≠ 0) :
    ParetoIII.logLik d (-t) (c, s) =
      Endpoint2FellerPareto.logLik d c c c s (log c - log s) t := by
  rw [ParetoIII.logLik_transfer]
  unfold LogLogistic.logLik LogLogistic.logDensity Endpoint2FellerPareto.logLik
  apply Finset.sum_congr rfl
  intro i _
  have h : (c + c) / c * log (1 + ((t + d i) / s) ^ c) =
      2 * log (1 + ((t + d i) / s) ^ c) := by
    rw [← two_mul, mul_div_cancel_right₀ _ hc]
  simp only [ParetoIII.excess, sub_neg_eq_add, add_comm (d i) t, h]

/-- With subunit shape and any fixed scale, all observations tied at the
sample minimum contribute to the free-threshold spike. -/
theorem threshold_spike {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c s : ℝ} (hc : 0 < c) (hsub : c < 1) (hs : 0 < s) :
    Tendsto (fun t ↦ ParetoIII.logLik d (-t) (c, s)) (𝓝[>] 0) atTop := by
  exact (Endpoint2FellerPareto.threshold_spike hd hz
    (b := c) (K := log c - log s) hsub hc hs).congr
    (fun t ↦ (kernel d t c s hc.ne').symm)

/-- Freeing the threshold and shape gives an unbounded objective for any
positive scale, hence also when scale is free. -/
theorem free_threshold_unbounded {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {s : ℝ} (hs : 0 < s) :
    let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
    let L : ℝ × ℝ → ℝ := fun p ↦ ParetoIII.logLik d (-p.1) (p.2, s)
    ¬ BddAbove (L '' D) ∧ ¬ ∃ p ∈ D, IsMaxOn L D p := by
  dsimp only
  apply Endpoint2FellerPareto.unbounded_of_threshold_path hd hz
    (a := 1 / 2) (b := 1 / 2) (c := 1 / 2) (s := s) (K := log (1 / 2) - log s)
    (by norm_num) (by norm_num) hs (fun t ↦ (t, 1 / 2))
  intro t ht
  exact ⟨⟨ht.le, by norm_num⟩, kernel d t (1 / 2) s (by norm_num)⟩

/-- Increasing shape at the scale of a positive constant excess makes the
held-threshold sample log likelihood diverge. -/
theorem constant_excess_path (hn : 0 < n) {v μ : ℝ} (hv : μ < v) :
    Tendsto (fun c ↦ ParetoIII.logLik (fun _ : Fin n ↦ v) μ (c, v - μ)) atTop atTop := by
  have hs := sub_pos.mpr hv
  have he (c : ℝ) :
      ParetoIII.logLik (fun _ : Fin n ↦ v) μ (c, v - μ) =
        (n : ℝ) * log c + (n : ℝ) * (-log (v - μ) - 2 * log 2) := by
    rw [ParetoIII.logLik_transfer]
    simp [ParetoIII.excess, LogLogistic.logLik, LogLogistic.logDensity, div_self hs.ne',
      nsmul_eq_mul]
    ring_nf
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  simpa only [he] using (tendsto_log_atTop.const_mul_atTop hnR).atTop_add
    (tendsto_const_nhds (x := (n : ℝ) * (-log (v - μ) - 2 * log 2)))

end

end UnivariateML.Endpoint2ParetoIII

end
