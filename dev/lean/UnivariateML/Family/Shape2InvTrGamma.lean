module

public import UnivariateML.Family.Shape2TrGamma

set_option linter.style.header false

/-!
Reciprocal transfer of held-scale transformed-gamma results.

## Deviations

The held-scale path fixes the shape-power product exactly, and the strict trace comparison
assumes the sample is in the Pareto support. The centered uniqueness slice requires a
nonzero residual, as in the written proof.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.Shape2InvTrGamma

open InverseTransformedGamma

variable {n : ℕ}

private theorem reciprocal_avgLog (x : Fin n → ℝ) :
    Gamma.avgLog (reciprocal x) = -Gamma.avgLog x := by
  simp [Gamma.avgLog, Gamma.avg, reciprocal, log_inv, Finset.sum_neg_distrib, neg_div]

private theorem reciprocal_residual (x : Fin n → ℝ) (a : ℝ) (i : Fin n) :
    log (reciprocal x i) - log a⁻¹ = log a - log (x i) := by
  simp only [reciprocal, log_inv]
  ring

private theorem reciprocal_contact (x : Fin n → ℝ) (a : ℝ) (i : Fin n) :
    reciprocal x i = a⁻¹ ↔ x i = a :=
  inv_injective.eq_iff

theorem held_scale_centered_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) (hne : ∃ i, x i ≠ a)
    (hmean : Gamma.avg (fun i ↦ log a - log (x i)) = 0) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 r.2 a) Gamma.domain q := by
  obtain ⟨i, hi⟩ := hne
  refine (Transfer.existsUnique_max_iff
    (g := fun p : ℝ × ℝ ↦ TransformedGamma.logLik (reciprocal x) p.1 p.2 a⁻¹)
    (bijOn_id Gamma.domain)
    (fun p _ ↦ logLik_reciprocal x p.1 p.2 a)).2 ?_
  exact Shape2TrGamma.held_scale_centered_mle (x := reciprocal x) hn (fun j ↦ inv_pos.mpr (hx j))
    (inv_pos.mpr ha) ⟨i, fun h ↦ hi ((reciprocal_contact x a i).1 h)⟩
    (by simpa only [reciprocal_residual] using hmean)

theorem held_scale_pareto_path {x : Fin n → ℝ} {a d : ℝ}
    (ha : 0 < a) (hd : 0 < d) (hx : ∀ i, a ≤ x i) :
    Tendsto (fun q : ℝ ↦ logLik x (d * q) q⁻¹ a) (𝓝[>] 0)
      (𝓝 (log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log a - log (x i)) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0))) := by
  have h := Shape2TrGamma.held_scale_power_path (x := reciprocal x) (inv_pos.mpr ha) hd
    (fun i ↦ inv_pos.mpr (ha.trans_le (hx i))) (fun i ↦ inv_anti₀ ha (hx i))
  have ht := h.add_const (-2 * Gamma.avgLog x)
  simp only [reciprocal_avgLog, reciprocal_residual, reciprocal_contact] at ht
  convert ht using 1
  · exact funext fun q ↦ logLik_reciprocal x (d * q) q⁻¹ a
  · congr 1
    ring

theorem held_scale_pareto_trace_beaten {x : Fin n → ℝ} {a d : ℝ}
    (ha : 0 < a) (hd : 0 < d) (hx : ∀ i, a ≤ x i) :
    ∃ k > 0, ∃ p > 0,
      log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log a - log (x i)) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0) < logLik x k p a := by
  obtain ⟨k, hk, p, hp, h⟩ := Shape2TrGamma.held_scale_power_trace_beaten
    (x := reciprocal x) (inv_pos.mpr ha) hd
    (fun i ↦ inv_pos.mpr (ha.trans_le (hx i))) (fun i ↦ inv_anti₀ ha (hx i))
  refine ⟨k, hk, p, hp, ?_⟩
  simp only [reciprocal_avgLog, reciprocal_residual, reciprocal_contact] at h
  rw [logLik_reciprocal]
  linarith only [h]

theorem held_scale_constant_concentration (hn : 0 < n) {v a : ℝ}
    (hv : 0 < v) (hva : v ≤ a) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ v) (exp (p * (log a - log v))) p a)
      atTop atTop := by
  have h := Shape2TrGamma.held_scale_constant_concentration (n := n) hn
    (inv_pos.mpr (hv.trans_le hva)) (inv_anti₀ hv hva)
  apply (tendsto_atTop_add_const_right _ (-2 * Gamma.avgLog (fun _ : Fin n ↦ v)) h).congr
  intro p
  rw [logLik_reciprocal]
  simp only [log_inv]
  rw [show -log v - -log a = log a - log v by ring]
  rfl

end UnivariateML.Shape2InvTrGamma

end
