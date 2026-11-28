module

public import UnivariateML.Family.AllFreePGW

/-!
# Power generalized Weibull with shape held

## Deviations

The criterion compares finite stationary points with the optimized Gompertz
face. The finiteness of the retained stationary profile list is open here.
-/

@[expose] public section

open Set

namespace UnivariateML.Base9PGW

/-- The held-shape finite-fit criterion, including constant samples and
equality at the optimized Gompertz face. -/
theorem held_shape_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    let L := fun p : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b p.1 p.2
    let G := AllFreePGW.gompertzLogLik x b
    ∃ r ∈ Ioi 0, IsMaxOn G (Ioi 0) r ∧
      ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔
        (∃ i j, x i ≠ x j) ∧ ∃ a > 0, ∃ c > 0,
          deriv (fun s ↦ L (s, c)) a = 0 ∧
          deriv (fun s ↦ L (a, s)) c = 0 ∧ G r ≤ L (a, c)) := by
  intro L G
  obtain ⟨r, hr, hG⟩ := (AllFreePGW.gompertz_unique (b := b) hn hx).exists
  have hiff := AllFreePGW.held_shape_attains_iff_stationary hn hx hb
  refine ⟨r, hr, hG, ?_⟩
  constructor
  · intro hm
    have hne : ∃ i j, x i ≠ x j := by
      by_contra! he
      let i : Fin n := ⟨0, hn⟩
      apply AllFreePGW.held_shape_narrow_no_max hn hb hx
        (X := x i) (fun j ↦ (he j i).le) ?_ hm
      intro j
      rw [he j i, div_self (hx i).ne', Real.one_rpow]
      norm_num
    obtain ⟨a, ha, c, hc, h₁, h₂, h⟩ := hiff.mp hm
    exact ⟨hne, a, ha, c, hc, h₁, h₂, h r hr⟩
  · rintro ⟨_, a, ha, c, hc, h₁, h₂, h⟩
    exact hiff.mpr ⟨a, ha, c, hc, h₁, h₂, fun s hs ↦ (hG hs).trans h⟩

end UnivariateML.Base9PGW

end
