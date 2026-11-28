module

public import UnivariateML.Family.GBTGB2

set_option linter.style.header false

/-! The fixed-threshold Feller–Pareto chart as GB2.

## Deviations

The identity supplies the parameter-preserving transfer on strict
support. It leaves the GB2 exterior and stationary comparisons open.
-/

@[expose] public section

open Real Filter Topology
namespace UnivariateML.GBTFellerPareto

/-- Translation to positive excesses identifies the entire finite
objective, including every tied excess, with the GB2 objective. -/
theorem logLik_positive_excess {n : ℕ} {x : Fin n → ℝ} {μ s : ℝ}
    (hx : ∀ i, μ < x i) (hs : 0 < s) (b : ℝ) (sh : ℝ × ℝ) :
    ShapeFacesFellerPareto.logLikelihood x μ b s sh =
      Shape2GB2.logLik (fun i ↦ x i - μ) s b sh := by
  unfold ShapeFacesFellerPareto.logLikelihood Shape2GB2.logLik
  apply Finset.sum_congr rfl
  intro i _
  have hz := div_pos (sub_pos.mpr (hx i)) hs
  rw [Real.rpow_def_of_pos hz]
  unfold Shape2BetaPrime.logisticLoss
  ring_nf

/-- With the threshold and lower beta shape held, the cutoff path
preserves the full Pareto trace on the excesses, including ties. -/
theorem pareto_excess_path {n : ℕ} {x : Fin n → ℝ} {μ p D m : ℝ}
    (hx : ∀ i, μ < x i) (hp : 0 < p) (hD : 0 < D)
    (hm : ∀ i, m ≤ log (x i - μ)) :
    Tendsto (fun a : ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ a
      (exp (m - log a / a)) (p, D / a)) atTop
      (𝓝 ((∑ i, (log D - D * (log (x i - μ) - m))) - ∑ i, log (x i - μ))) := by
  have h := (GBTGB2.pareto_cutoff_path (fun i ↦ log (x i - μ)) hp hD hm).sub_const
    (∑ i, log (x i - μ))
  refine h.congr (fun a ↦ ?_)
  rw [logLik_positive_excess hx (exp_pos _) a]
  conv_lhs =>
    arg 1
    rw [← log_exp (m - log a / a)]
  rw [← GBTGB2.logLik_log_chart (fun i ↦ sub_pos.mpr (hx i)) (exp_pos _) a,
    add_sub_cancel_right]

/-- With the threshold and upper beta shape held, the reflected cutoff
path preserves the full power trace on the excesses, including ties. -/
theorem power_excess_path {n : ℕ} {x : Fin n → ℝ} {μ q A m : ℝ}
    (hx : ∀ i, μ < x i) (hq : 0 < q) (hA : 0 < A)
    (hm : ∀ i, log (x i - μ) ≤ m) :
    Tendsto (fun a : ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ a
      (exp (m + log a / a)) (A / a, q)) atTop
      (𝓝 ((∑ i, (log A - A * (m - log (x i - μ)))) - ∑ i, log (x i - μ))) := by
  have h := (GBTGB2.power_cutoff_path (fun i ↦ log (x i - μ)) hq hA hm).sub_const
    (∑ i, log (x i - μ))
  refine h.congr (fun a ↦ ?_)
  rw [logLik_positive_excess hx (exp_pos _) a]
  conv_lhs =>
    arg 1
    rw [← log_exp (m + log a / a)]
  rw [← GBTGB2.logLik_log_chart (fun i ↦ sub_pos.mpr (hx i)) (exp_pos _) a,
    add_sub_cancel_right]

/-- At a constant positive excess, fixing both beta shapes and taking
power to infinity gives divergence with scale equal to the excess. -/
theorem constant_excess_concentration {n : ℕ} (hn : 0 < n) {μ x : ℝ} (hx : μ < x)
    (sh : ℝ × ℝ) :
    Tendsto (fun a : ℝ ↦ ShapeFacesFellerPareto.logLikelihood (fun _ : Fin n ↦ x)
      μ a (x - μ) sh) atTop atTop := by
  refine (Shape2GB2.constant_concentration hn (sub_pos.mpr hx) sh).congr (fun a ↦ ?_)
  exact (logLik_positive_excess (fun _ ↦ hx) (sub_pos.mpr hx) a sh).symm

end UnivariateML.GBTFellerPareto
end
