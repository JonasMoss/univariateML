module

public import UnivariateML.Family.Base7Binomial

set_option linter.style.header false

/-! Integer comparison of adjacent profiled binomial likelihoods.

## Deviations
This proves the arithmetic comparison. Sufficiency of the two neighbor
tests and the exclusion at prime-power sizes require separate arguments.
-/

@[expose] public section

namespace UnivariateML.Base7bBinomial

open Real BinomialSize

private theorem exp_profile_integer_form {n N : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hmean : 0 < mean x) (hN : N ∈ feasible x) :
    let S : ℕ := ∑ i, x i
    let F := n * N - S
    exp (profile x N) =
      (∏ i, (N.choose (x i) : ℝ)) *
        ((S : ℝ) ^ S * (F : ℝ) ^ F) / ((n : ℝ) * N) ^ (n * N) := by
  intro S F
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hN' : (N : ℝ) ≠ 0 := (hmean.trans_le (Discrete.avg_le x hN)).ne'
  have hF : ∑ i, (N - x i) = F := by
    have H := Finset.sum_add_distrib (s := Finset.univ) (f := fun i ↦ x i)
      (g := fun i ↦ N - x i)
    have he : ∀ i, x i + (N - x i) = N := fun i ↦ Nat.add_sub_of_le (hN i)
    simp only [he, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at H
    change _ = n * N - (∑ i, x i)
    have H' : n * N = (∑ i, x i) + ∑ i, (N - x i) := by simpa using H
    omega
  have hSf : S ≤ n * N := by dsimp [S]; simpa using Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦ hN i)
  have hS : mean x / (N : ℝ) = (S : ℝ) / ((n : ℝ) * N) := by
    simp [mean, S, Nat.cast_sum, div_div]
  have hfail : 1 - mean x / (N : ℝ) = (F : ℝ) / ((n : ℝ) * N) := by
    rw [hS]
    dsimp [F]
    rw [Nat.cast_sub hSf, Nat.cast_mul, sub_div, div_self (mul_ne_zero hn' hN')]
  have hsum : S + F = n * N := Nat.add_sub_of_le hSf
  rw [exp_profile_eq_sampleLikelihood x hn hmean hN]
  unfold Discrete.Binomial.sampleLikelihood
  simp_rw [mul_assoc, Finset.prod_mul_distrib, Finset.prod_pow_eq_pow_sum]
  change (∏ i, (N.choose (x i) : ℝ)) *
    ((mean x / N) ^ S * (1 - mean x / N) ^ (∑ i, (N - x i))) = _
  rw [hF, hfail, hS, div_pow, div_pow, div_mul_div_comm, ← pow_add, hsum]
  ring

/-- The exact integer criterion for the outgoing profile to be at most the
current value. The formula applies at the positive-constant endpoint too,
with the natural-power convention `0 ^ 0 = 1`. -/
theorem adjacent_profile_le_iff {n N : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hmean : 0 < mean x) (hN : N ∈ feasible x) :
    let S : ℕ := ∑ i, x i
    let F := n * N - S
    let G := F + n
    profile x (N + 1) ≤ profile x N ↔
      N ^ (n * N) * G ^ G ≤
        (N + 1) ^ (n * N) * n ^ n * F ^ F * ∏ i, (N + 1 - x i) := by
  intro S F G
  have hN1 : N + 1 ∈ feasible x := fun i ↦ (hN i).trans (Nat.le_succ _)
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hNr : 0 < (N : ℝ) := hmean.trans_le (Discrete.avg_le x hN)
  have hSf : S ≤ n * N := by
    dsimp [S]
    simpa [nsmul_eq_mul] using Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦ hN i)
  have hSp : 0 < (S : ℝ) := by
    rw [Nat.cast_sum]
    exact (div_pos_iff_of_pos_right hn').mp hmean
  have hG : n * (N + 1) - S = G := by dsimp [G, F]; rw [Nat.mul_add, Nat.mul_one]; omega
  let C : ℝ := ∏ i, (N.choose (x i) : ℝ)
  let C' : ℝ := ∏ i, ((N + 1).choose (x i) : ℝ)
  let D : ℝ := ∏ i, ((N + 1 - x i : ℕ) : ℝ)
  have hc : 0 < C := Finset.prod_pos fun i _ ↦ by exact_mod_cast Nat.choose_pos (hN i)
  have hd : 0 < D := Finset.prod_pos fun i _ ↦ by
    exact_mod_cast (show 0 < N + 1 - x i by have := hN i; omega)
  have hchoose : C' * D = C * ((N : ℝ) + 1) ^ n := by
    unfold C' C D
    rw [show ((N : ℝ) + 1) ^ n = ∏ _ : Fin n, ((N : ℝ) + 1) by simp,
      ← Finset.prod_mul_distrib, ← Finset.prod_mul_distrib]
    apply Finset.prod_congr rfl
    intro i _
    exact_mod_cast (Nat.choose_mul_succ_eq N (x i)).symm
  have hC' : C' = C * ((N : ℝ) + 1) ^ n / D := (eq_div_iff hd.ne').mpr hchoose
  rw [← exp_le_exp, exp_profile_integer_form hn x hmean hN1,
    exp_profile_integer_form hn x hmean hN, hG, Nat.cast_succ]
  change C' * ((S : ℝ) ^ S * (G : ℝ) ^ G) / ((n : ℝ) * (N + 1)) ^ (n * (N + 1)) ≤
      C * ((S : ℝ) ^ S * (F : ℝ) ^ F) / ((n : ℝ) * N) ^ (n * N) ↔ _
  rw [hC']
  have hA : 0 < ((n : ℝ) * (N + 1)) ^ (n * (N + 1)) := by positivity
  have hB : 0 < ((n : ℝ) * N) ^ (n * N) := by positivity
  rw [div_le_div_iff₀ hA hB]
  rw [div_mul_eq_mul_div, div_mul_eq_mul_div, div_le_iff₀ hd]
  have hT : n * (N + 1) = n * N + n := by ring
  rw [hT, pow_add ((n : ℝ) * (N + 1)) (n * N) n, mul_pow (n : ℝ) (N + 1), mul_pow (n : ℝ) (N : ℝ),
    mul_pow (n : ℝ) (N + 1)]
  let K : ℝ := C * (S : ℝ) ^ S * (n : ℝ) ^ (n * N) * (N + 1 : ℝ) ^ n
  have hK : 0 < K := by dsimp [K]; positivity
  have H₁ : C * ((N : ℝ) + 1) ^ n * ((S : ℝ) ^ S * (G : ℝ) ^ G) *
      ((n : ℝ) ^ (n * N) * (N : ℝ) ^ (n * N)) =
      K * ((N : ℝ) ^ (n * N) * (G : ℝ) ^ G) := by dsimp [K]; ring
  have H₂ : (C * ((S : ℝ) ^ S * (F : ℝ) ^ F) *
      (((n : ℝ) ^ (n * N) * (N + 1 : ℝ) ^ (n * N)) *
        ((n : ℝ) ^ n * (N + 1 : ℝ) ^ n))) * D =
      K * ((N + 1 : ℝ) ^ (n * N) * (n : ℝ) ^ n * (F : ℝ) ^ F * D) := by
    dsimp [K]; ring
  rw [H₁, H₂, mul_le_mul_iff_right₀ hK]
  dsimp [D]
  exact_mod_cast (Iff.rfl :
    N ^ (n * N) * G ^ G ≤ (N + 1) ^ (n * N) * n ^ n * F ^ F * ∏ i, (N + 1 - x i) ↔ _)

/-- Strict quasiconcavity of the continuous-size profile, restricted to
integer sizes, suffices for the two neighbor tests and for adjacency of
distinct co-winners. The strict middle-point comparison is an explicit
hypothesis here. -/
theorem neighbor_test_of_strict_middle {n : ℕ} (x : Fin n → ℕ)
    (hmid : ∀ a b c : ℕ, a ∈ feasible x → a < b → b < c →
      min (profile x a) (profile x c) < profile x b) :
    (∀ N ∈ feasible x, IsMaxOn (profile x) (feasible x) N ↔
      profile x (N + 1) ≤ profile x N ∧
      ∀ M ∈ feasible x, M + 1 = N → profile x M ≤ profile x N) ∧
    (∀ a ∈ feasible x, ∀ c ∈ feasible x, a < c →
      IsMaxOn (profile x) (feasible x) a → IsMaxOn (profile x) (feasible x) c → c = a + 1) := by
  have up {a b : ℕ} (ha : a ∈ feasible x) (hab : a ≤ b) : b ∈ feasible x :=
    fun i ↦ (ha i).trans hab
  constructor
  · intro N hN
    constructor
    · intro hm
      exact ⟨hm (up hN (Nat.le_succ _)), fun _ hM _ ↦ hm hM⟩
    · rintro ⟨hout, hin⟩ q hq
      change profile x q ≤ profile x N
      by_contra! hbad
      rcases lt_trichotomy q N with h | rfl | h
      · have hle : q ≤ N - 1 := by omega
        have hm := up hq hle
        have hn := hin (N - 1) hm (by omega)
        rcases hle.eq_or_lt with rfl | hlt
        · exact hbad.not_ge hn
        · have H := hmid q (N - 1) N hq hlt (by omega)
          rw [min_eq_right hbad.le] at H
          exact H.not_ge hn
      · exact lt_irrefl _ hbad
      · have hle : N + 1 ≤ q := by omega
        rcases hle.eq_or_lt with he | hlt
        · rw [← he] at hbad
          exact hbad.not_ge hout
        · have H := hmid N (N + 1) q hN (Nat.lt_succ_self _) hlt
          rw [min_eq_left hbad.le] at H
          exact H.not_ge hout
  · intro a ha c hc hac hma hmc
    by_contra hne
    have H := hmid a (a + 1) c ha (Nat.lt_succ_self _) (by omega)
    have he : profile x a = profile x c := (hmc ha).antisymm (hma hc)
    rw [he, min_self] at H
    exact H.not_ge (hmc (up ha (Nat.le_succ _)))

end UnivariateML.Base7bBinomial

end
