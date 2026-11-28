module

public import UnivariateML.Family.JoinsBinomialSize

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! The all-free binomial maximizing set.

## Deviations
On underdispersed nonconstant data the set is stated as a finite profile
argmax. Adjacent-ratio sufficiency, adjacency and prime-power exclusion
require additional proofs.
-/

@[expose] public section

namespace UnivariateML.Base7Binomial

open Set BinomialSize
open Discrete.Binomial (sampleLikelihood)

private theorem positive_constant {n c : ℕ} (hn : 0 < n) (hc : 0 < c) :
    let L := fun p : ℕ × ℝ ↦ sampleLikelihood (fun _ : Fin n ↦ c) p.1 p.2
    {p ∈ parameterDomain | IsMaxOn L parameterDomain p} = {(c, 1)} := by
  dsimp only
  let mass (N : ℕ) (p : ℝ) (j : ℕ) := (N.choose j : ℝ) * p ^ j * (1 - p) ^ (N - j)
  have hbound (N : ℕ) (p : ℝ) (hp : p ∈ Icc 0 1) (hnp : (N, p) ≠ (c, 1)) :
      mass N p c < 1 := by
    by_cases hN : N < c
    · simp [mass, Nat.choose_eq_zero_of_lt hN]
    by_cases hp1 : p = 1
    · subst p
      have hn' : N ≠ c := fun H ↦ hnp (by simp [H])
      have hsub : N - c ≠ 0 := by omega
      simp [mass, zero_pow hsub]
    have hpc : 0 < 1 - p := sub_pos.mpr (lt_of_le_of_ne hp.2 hp1)
    have hp0 : 0 ≤ p := hp.1
    have hnon (j : ℕ) : 0 ≤ mass N p j := by
      dsimp [mass]
      positivity
    have hsum : (∑ j ∈ Finset.range (N + 1), mass N p j) = 1 := by
      have he := add_pow p (1 - p) N
      rw [show p + (1 - p) = 1 by ring, one_pow] at he
      simpa only [mass, mul_assoc, mul_left_comm, mul_comm] using he.symm
    rw [← hsum, ← Finset.sum_erase_add _ _ (Finset.mem_range.mpr (show c < N + 1 by omega))]
    have hm : 0 ∈ (Finset.range (N + 1)).erase c :=
      Finset.mem_erase.mpr ⟨hc.ne, Finset.mem_range.mpr (by omega)⟩
    have hpos : 0 < mass N p 0 := by simpa [mass] using pow_pos hpc N
    have hsumPos := hpos.trans_le (Finset.single_le_sum
      (f := mass N p) (fun j _ ↦ hnon j) hm)
    linarith
  have hone : sampleLikelihood (fun _ : Fin n ↦ c) c 1 = 1 := by
    simp [sampleLikelihood]
  have hlt (N : ℕ) (p : ℝ) (hp : p ∈ Icc 0 1) (hne : (N, p) ≠ (c, 1)) :
      sampleLikelihood (fun _ : Fin n ↦ c) N p < 1 := by
    simp only [sampleLikelihood, Finset.prod_const, Finset.card_univ, Fintype.card_fin]
    have hp0 : 0 ≤ p := hp.1
    have hp1 : 0 ≤ 1 - p := sub_nonneg.mpr hp.2
    exact pow_lt_one₀ (by positivity) (hbound N p hp hne) hn.ne'
  have hdom : (c, (1 : ℝ)) ∈ parameterDomain := by simp [parameterDomain]
  ext p
  constructor
  · rintro ⟨hp, hm⟩
    by_contra hne
    have hcontra := hlt p.1 p.2 hp.2 (show p ≠ (c, 1) from hne)
    exact hcontra.not_ge (hone ▸ hm hdom)
  · intro hp
    have he : p = (c, 1) := hp
    subst p
    refine ⟨hdom, ?_⟩
    intro q hq
    change sampleLikelihood (fun _ : Fin n ↦ c) q.1 q.2 ≤ sampleLikelihood (fun _ : Fin n ↦ c) c 1
    rw [hone]
    by_cases heq : q = (c, 1)
    · simpa [heq] using hone.le
    · exact (hlt q.1 q.2 hq.2 heq).le

/-- Constant samples have explicit winner sets. Positive-mean data have
the profile graph, which is empty at or above equidispersion and finite
and nonempty below it. -/
theorem all_free_maximizer_branches {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℕ × ℝ ↦ sampleLikelihood x p.1 p.2
    let W := {p ∈ parameterDomain | IsMaxOn L parameterDomain p}
    ((∀ i, x i = 0) → W = {p ∈ parameterDomain | p.1 = 0 ∨ p.2 = 0}) ∧
    (∀ c : ℕ, 0 < c → (∀ i, x i = c) → W = {(c, 1)}) ∧
    (0 < mean x →
      W = (fun N : ℕ ↦ (N, mean x / N)) ''
        {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N} ∧
      (W.Nonempty ↔ variance x < mean x) ∧
      (variance x < mean x → W.Finite)) := by
  dsimp only
  refine ⟨?_, ?_, fun hm ↦ ⟨joint_maximizers_eq_profile_graph x hn hm,
    joint_mle_iff_underdispersion x hn hm, finite_joint_maximizers_of_underdispersion x hn hm⟩⟩
  · intro hz
    rw [show x = fun _ ↦ 0 from funext hz]
    ext p
    exact and_congr_right (fun hp ↦ all_zero_maximizers hn p hp)
  · intro c hc hx
    rw [show x = fun _ ↦ c from funext hx]
    exact positive_constant hn hc

end UnivariateML.Base7Binomial

end
