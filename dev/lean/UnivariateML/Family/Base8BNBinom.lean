module

public import UnivariateML.Family.Discrete4BNBinom

set_option linter.style.header false

@[expose] public section

open Set Filter Topology Real
namespace UnivariateML.Base8BNBinom

open Discrete3BNBinom Discrete4BNBinom

/-- Swapping size and beta transports the entire fixed-hold maximizing
set, preserving the coordinates of the pair after renaming the free shape. -/
theorem held_beta_size_maximizers_swap {n : ℕ} (x : Fin n → ℕ) (b : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    {p ∈ D | IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x q.1 q.2 b) D p} =
      {p ∈ D | IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x b q.2 q.1) D p} := by
  have h : (fun q : ℝ × ℝ ↦ logLikelihood x q.1 q.2 b) =
      fun q ↦ logLikelihood x b q.2 q.1 :=
    funext fun q ↦ logLikelihood_swap x q.1 q.2 b
  rw [h]

/-- Holding either of the symmetric shapes fixed, the all-zero likelihood
has strict supremum one. Varying the other shape toward zero reaches it. -/
theorem held_beta_size_zero_suprema {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ exp (logLikelihood x q.1 q.2 b)
    let R := fun q : ℝ × ℝ ↦ exp (logLikelihood x b q.2 q.1)
    (IsLUB (L '' D) 1 ∧ ∀ p ∈ D, L p < 1) ∧
    (IsLUB (R '' D) 1 ∧ ∀ p ∈ D, R p < 1) ∧
    ∀ a > 0, Tendsto (fun r ↦ L (r,a)) (𝓝[>] 0) (𝓝 1) ∧
      Tendsto (fun r ↦ R (r,a)) (𝓝[>] 0) (𝓝 1) := by
  intro D L R
  have he : L = R := funext fun q ↦ congrArg exp (logLikelihood_swap x q.1 q.2 b)
  rw [← he]
  have hstrict (p : ℝ × ℝ) (hp : p ∈ D) : L p < 1 :=
    (zero_atom_likelihood_paths hn x hx hp.1 hp.2 hb).1
  have hpath (a : ℝ) (ha : 0 < a) :
      Tendsto (fun r ↦ L (r,a)) (𝓝[>] 0) (𝓝 1) :=
    (zero_atom_likelihood_paths hn x hx zero_lt_one ha hb).2.1
  have hL : IsLUB (L '' D) 1 := by
    refine ⟨?_, ?_⟩
    · rintro _ ⟨p,hp,rfl⟩
      exact (hstrict p hp).le
    · intro C hC
      apply le_of_tendsto (hpath 1 zero_lt_one)
      filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with r hr
      exact hC (mem_image_of_mem L (show (r,1) ∈ D from ⟨hr,zero_lt_one⟩))
  exact ⟨⟨hL,hstrict⟩, ⟨hL,hstrict⟩, fun a ha ↦ ⟨hpath a ha,hpath a ha⟩⟩

end UnivariateML.Base8BNBinom

end
