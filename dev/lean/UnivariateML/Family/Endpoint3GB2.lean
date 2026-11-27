module

public import UnivariateML.Family.Shape2GB2
public import UnivariateML.Family.ShapeFacesFellerPareto
public import UnivariateML.Family.Endpoint3BetaPrime

import Mathlib.Tactic

set_option linter.style.header false

/-!
# GB2 with two held beta shapes and the gamma faces

## Deviations

The two gamma orientations below give sample log-likelihood limits at
fixed power. Normalization of the weak limits and exclusion of other
competitive sequences are outside these path statements.
-/

@[expose] public section

open Real Set Filter
open scoped BigOperators Topology

namespace UnivariateML.Endpoint3GB2

/-- With positive held beta shapes and a positive nonempty sample, the
free power and scale attain exactly on nonconstant data. On constants
the same held-shape likelihood is unbounded. -/
theorem held_beta_shapes_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    let L := fun v : ℝ × ℝ ↦ Shape2GB2.logLik x v.2 v.1 (p, q)
    ((∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔ ∃ i j, x i ≠ x j) ∧
      ((∀ i j, x i = x j) → ¬ BddAbove (L '' Beta.domain)) := by
  intro L
  have hu (h : ∀ i j, x i = x j) : ¬ BddAbove (L '' Beta.domain) := by
    rintro ⟨B, hB⟩
    let j : Fin n := ⟨0, hn⟩
    let t := x j
    have ht : 0 < t := hx j
    have hpath := Shape2GB2.constant_concentration hn ht (p, q)
    obtain ⟨b, hb, hbB⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (hpath.eventually_gt_atTop B)).exists
    have he : x = fun _ ↦ t := funext fun i ↦ h i j
    apply hbB.not_ge
    have hle := hB (mem_image_of_mem L (show (b, t) ∈ Beta.domain from ⟨hb, ht⟩))
    simpa only [L, he] using hle
  refine ⟨⟨?_, ?_⟩, hu⟩
  · rintro ⟨v, _, hm⟩
    by_contra! h
    exact hu h hm.bddAbove
  · intro h
    exact (Shape2GB2.held_beta_shapes_mle hx h hp hq).exists.elim fun v hv ↦ ⟨v, hv⟩

/-- Both transformed-gamma likelihood faces of Feller–Pareto,
expressed on the powered positive excesses. The upper shape can
diverge while the lower shape is held, and the reciprocal ray
holds the upper shape while the lower shape diverges. -/
theorem feller_pareto_gamma_face_paths {n : ℕ} {x : Fin n → ℝ} {μ c p q r : ℝ}
    (hx : ∀ i, μ < x i) (hc : 0 < c) (hp : 0 < p) (hq : 0 < q) (hr : 0 < r) :
    let y := ShapeFacesFellerPareto.poweredExcess x μ c
    let J := ShapeFacesFellerPareto.powerJacobian x μ c
    (Tendsto (fun t : ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ c ((t / r) ^ c⁻¹) (p, t))
      atTop (𝓝 ((∑ i, Gamma.logDensity p r (y i)) + J))) ∧
    (Tendsto (fun t : ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ c ((r / t) ^ c⁻¹) (t, q))
      atTop (𝓝 ((∑ i, Gamma.logDensity q r (y i)⁻¹) - ∑ i, 2 * log (y i) + J))) := by
  intro y J
  have hy : ∀ i, 0 < y i := fun i ↦ rpow_pos_of_pos (sub_pos.mpr (hx i)) _
  have hpos : ∀ᶠ t : ℝ in atTop, 0 < t := eventually_gt_atTop 0
  have h₁ : Tendsto (fun t : ℝ ↦ t / (t / r)) atTop (𝓝 r) := by
    apply tendsto_const_nhds.congr'
    filter_upwards [hpos] with t ht
    field_simp [ht.ne', hr.ne']
  have h₂ : Tendsto (fun t : ℝ ↦ t * (r / t)) atTop (𝓝 r) := by
    apply tendsto_const_nhds.congr'
    filter_upwards [hpos] with t ht
    exact (mul_div_cancel₀ _ ht.ne').symm
  have hleft := Endpoint3BetaPrime.gamma_of_shape_rate_limit hy hp hr tendsto_id
    (hpos.mono fun _ ht ↦ div_pos ht hr) h₁
  have hright := Endpoint3BetaPrime.inverse_gamma_of_shape_rate_limit hy hq hr tendsto_id
    (hpos.mono fun _ ht ↦ div_pos hr ht) h₂
  have hpow {z : ℝ} (hz : 0 < z) : (z ^ c⁻¹) ^ c = z := by
    rw [← rpow_mul hz.le, inv_mul_cancel₀ hc.ne', rpow_one]
  constructor
  · apply (hleft.add_const J).congr'
    filter_upwards [hpos] with t ht
    rw [ShapeFacesFellerPareto.logLik_powered_excess hx c
      (rpow_pos_of_pos (div_pos ht hr) _) (p,t), hpow (div_pos ht hr)]
    rfl
  · apply (hright.add_const J).congr'
    filter_upwards [hpos] with t ht
    rw [ShapeFacesFellerPareto.logLik_powered_excess hx c
      (rpow_pos_of_pos (div_pos hr ht) _) (t,q), hpow (div_pos hr ht)]
    rfl

private theorem gb2_kernel {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (c : ℝ) (p : ℝ × ℝ) :
    Shape2GB2.logLik x s c p = ShapeFacesFellerPareto.logLikelihood x 0 c s p := by
  unfold Shape2GB2.logLik Shape2BetaPrime.logisticLoss ShapeFacesFellerPareto.logLikelihood
  apply Finset.sum_congr rfl
  intro i _
  rw [sub_zero, rpow_def_of_pos (div_pos (hx i) hs), mul_comm (log (x i / s)) c]
  ring

/-- At any fixed positive power, GB2 has both transformed-gamma
likelihood limits, with either beta shape fixed along the appropriate
joint ray. The Jacobian restores the original data coordinates. -/
theorem gamma_face_paths {n : ℕ} {x : Fin n → ℝ} {c p q r : ℝ}
    (hx : ∀ i, 0 < x i) (hc : 0 < c) (hp : 0 < p) (hq : 0 < q) (hr : 0 < r) :
    let y := fun i ↦ (x i) ^ c
    let J := ∑ i, (log c + (c - 1) * log (x i))
    (Tendsto (fun t : ℝ ↦ Shape2GB2.logLik x ((t / r) ^ c⁻¹) c (p,t))
      atTop (𝓝 ((∑ i, Gamma.logDensity p r (y i)) + J))) ∧
    (Tendsto (fun t : ℝ ↦ Shape2GB2.logLik x ((r / t) ^ c⁻¹) c (t,q))
      atTop (𝓝 ((∑ i, Gamma.logDensity q r (y i)⁻¹) - ∑ i, 2 * log (y i) + J))) := by
  intro y J
  obtain ⟨h₁, h₂⟩ := feller_pareto_gamma_face_paths hx hc hp hq hr
  have hpos : ∀ᶠ t : ℝ in atTop, 0 < t := eventually_gt_atTop 0
  constructor
  · apply (show Tendsto _ atTop (𝓝 ((∑ i, Gamma.logDensity p r (y i)) + J)) from by
      simpa only [ShapeFacesFellerPareto.poweredExcess, ShapeFacesFellerPareto.powerJacobian,
        sub_zero, y, J] using h₁).congr'
    filter_upwards [hpos] with t ht
    exact (gb2_kernel hx (rpow_pos_of_pos (div_pos ht hr) _) c (p,t)).symm
  · apply (show Tendsto _ atTop
      (𝓝 ((∑ i, Gamma.logDensity q r (y i)⁻¹) - ∑ i, 2 * log (y i) + J)) from by
      simpa only [ShapeFacesFellerPareto.poweredExcess, ShapeFacesFellerPareto.powerJacobian,
        sub_zero, y, J] using h₂).congr'
    filter_upwards [hpos] with t ht
    exact (gb2_kernel hx (rpow_pos_of_pos (div_pos hr ht) _) c (t,q)).symm

end UnivariateML.Endpoint3GB2

end
