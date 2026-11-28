module

public import UnivariateML.Family.Discrete5BBinom

set_option linter.style.header false

@[expose] public section

open Set Filter Topology
open scoped BigOperators

namespace UnivariateML.Base8BBinom

open Discrete4BBinom

/-- At held alpha and size, positive-size endpoint constants approach their
respective point masses with unattained likelihood one. Size zero is flat. -/
theorem held_alpha_size_failure {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) {a : ℝ} (ha : 0 < a) :
    (m = 0 → ∀ b > 0, likelihood m x a b = 1) ∧
    (0 < m →
      ((∀ i, x i = 0) → Tendsto (likelihood m x a) atTop (𝓝 1)) ∧
      ((∀ i, x i = m) → Tendsto (likelihood m x a) (𝓝[>] 0) (𝓝 1)) ∧
      (((∀ i, x i = 0) ∨ (∀ i, x i = m)) →
        IsLUB (likelihood m x a '' Ioi 0) 1 ∧ ∀ b > 0, likelihood m x a b < 1)) := by
  have H := fixed_alpha_attainment hn x hx ha
  have P := fixed_alpha_paths x hx ha
  exact ⟨H.1, fun hm ↦ ⟨fun hz ↦ by simpa [hz] using P.2,
    fun hf ↦ by simpa [hf] using P.1, (H.2 hm).2⟩⟩

/-- At held beta and size, the directions to the two point masses are
reflected. Size zero is flat. -/
theorem held_beta_size_failure {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) {b : ℝ} (hb : 0 < b) :
    (m = 0 → ∀ a > 0, likelihood m x a b = 1) ∧
    (0 < m →
      (∀ k ≤ m,
        Tendsto (fun a ↦ Discrete5BBinom.countMass m k a b) (𝓝[>] 0)
          (𝓝 (if k = 0 then 1 else 0)) ∧
        Tendsto (fun a ↦ Discrete5BBinom.countMass m k a b) atTop
          (𝓝 (if k = m then 1 else 0))) ∧
      (((∀ i, x i = 0) ∨ (∀ i, x i = m)) →
        IsLUB ((fun a ↦ likelihood m x a b) '' Ioi 0) 1 ∧ ∀ a > 0, likelihood m x a b < 1)) := by
  have H := fixed_beta_attainment hn x hx hb
  refine ⟨H.1, fun hm ↦ ⟨fun k hk ↦ ?_, (H.2 hm).2⟩⟩
  have hlow : Tendsto (fun a : ℝ ↦ a / (a + b)) (𝓝[>] 0) (𝓝 0) := by
    have h := continuousAt_id.div (continuousAt_id.add continuousAt_const)
      (show (0 : ℝ) + b ≠ 0 by simpa using hb.ne')
    simpa [Pi.div_def, Pi.add_def] using h.tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
  have htop : Tendsto (fun a : ℝ ↦ a / (a + b)) atTop (𝓝 1) := by
    have h := (tendsto_const_nhds (x := (1 : ℝ))).div
      (((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul b).const_add 1)
      (by simp)
    simp only [mul_zero, add_zero, div_one] at h
    apply h.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
    change 1 / (1 + b * a⁻¹) = a / (a + b)
    have hab := (add_pos ha hb).ne'
    field_simp
  exact ⟨(Discrete5BBinom.endpoint_mixing_paths
    ((show ∀ᶠ a : ℝ in 𝓝[>] (0 : ℝ), a ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin).mono fun a (ha : a ∈ Ioi (0 : ℝ)) ↦ ⟨ha, hb⟩) hk).1 hlow,
    (Discrete5BBinom.endpoint_mixing_paths
      ((eventually_gt_atTop (0 : ℝ)).mono fun _ ha ↦ ⟨ha, hb⟩) hk).2 htop⟩

end UnivariateML.Base8BBinom

end
