module

public import UnivariateML.Family.Discrete5NBinom

set_option linter.style.header false

@[expose] public section

open Set Filter Topology
namespace UnivariateML.Base8NBinom

open Discrete4NBinom Discrete5NBinom

/-- Under a compatible probability hold, the size-infinity limit is zero
in the interior and one on the flat probability-one ray. The interior
optimum has strictly positive likelihood. -/
theorem held_probability_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioc 0 1) (hc : p = 1 → ∀ i, x i = 0) :
    (p = 1 →
      (∀ r ∈ Ici (0 : ℝ), likelihood x r p = 1) ∧
      Tendsto (fun r ↦ likelihood x r p) atTop (𝓝 1)) ∧
    (p < 1 →
      Tendsto (fun r ↦ likelihood x r p) atTop (𝓝 0) ∧
      ∃ r ∈ Ici (0 : ℝ), 0 < likelihood x r p ∧
        IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r) := by
  rcases held_probability_boundary hn x hp hc with ⟨he, hf⟩ | ⟨hlt, hlim, hM⟩
  · refine ⟨fun _ ↦ ⟨hf, ?_⟩, fun h ↦ (h.ne he).elim⟩
    exact tendsto_const_nhds.congr'
      ((eventually_ge_atTop (0 : ℝ)).mono fun r hr ↦ (hf r hr).symm)
  · exact ⟨fun h ↦ (hlt.ne h).elim, fun _ ↦ ⟨hlim, hM⟩⟩

/-- Under a compatible size hold, the probability-zero limit is zero
for positive size and one at the flat size-zero fit. The positive-size
optimum has strictly positive likelihood. -/
theorem held_size_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r : ℝ} (hr : 0 ≤ r) (hc : r = 0 → ∀ i, x i = 0) :
    (r = 0 →
      (∀ p ∈ Ioc (0 : ℝ) 1, likelihood x r p = 1) ∧
      Tendsto (likelihood x r) (𝓝[>] 0) (𝓝 1)) ∧
    (0 < r →
      Tendsto (likelihood x r) (𝓝[>] 0) (𝓝 0) ∧
      ∃ p ∈ Ioc (0 : ℝ) 1, 0 < likelihood x r p ∧
        IsMaxOn (likelihood x r) (Ioc 0 1) p) := by
  rcases held_size_boundary hn x hr hc with ⟨he, hf⟩ | ⟨hlt, hlim, hM⟩
  · refine ⟨fun _ ↦ ⟨hf, ?_⟩, fun h ↦ (h.ne' he).elim⟩
    apply tendsto_const_nhds.congr'
    have hlow : ∀ᶠ p : ℝ in 𝓝[>] (0 : ℝ), 0 < p := self_mem_nhdsWithin
    have hhigh : ∀ᶠ p : ℝ in 𝓝[>] (0 : ℝ), p < 1 :=
      (eventually_lt_nhds zero_lt_one).filter_mono nhdsWithin_le_nhds
    filter_upwards [hlow, hhigh] with p hp hp'
    exact (hf p ⟨hp,hp'.le⟩).symm
  · exact ⟨fun h ↦ (hlt.ne' h).elim, fun _ ↦ ⟨hlim, hM⟩⟩

end UnivariateML.Base8NBinom

end
