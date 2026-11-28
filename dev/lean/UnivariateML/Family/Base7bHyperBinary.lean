module

public import UnivariateML.Family.Base7Hyper

set_option linter.style.header false

/-! Exact hypergeometric saturation on mixed binary samples.

## Deviations
The binary maximizing rays and the deterministic branches are separate from
the general finite-prefix and residue computation.
-/

@[expose] public section

namespace UnivariateML.Base7bHyperBinary

open Real Set Hypergeometric

private theorem mass_pos_of_compatible {K N k c : ℕ} (hc : Compatible K N k c) :
    0 < mass K N k c := by
  rw [mass_choose hc]
  have hK := hc.2.2.1
  have hc' : k - c ≤ N - K := by
    have ⟨_, _, _, _, _⟩ := hc
    omega
  exact div_pos (mul_pos (Nat.cast_pos.mpr (Nat.choose_pos hK))
    (Nat.cast_pos.mpr (Nat.choose_pos hc'))) (Nat.cast_pos.mpr (Nat.choose_pos hc.2.1))

private theorem two_mass_sum {K N k : ℕ} (h0 : Compatible K N k 0)
    (h1 : Compatible K N k 1) :
    mass K N k 0 + mass K N k 1 ≤ 1 ∧
      (2 ≤ min K k → mass K N k 0 + mass K N k 1 < 1) := by
  let f := fun ij : ℕ × ℕ ↦ K.choose ij.1 * (N - K).choose ij.2
  let T := Finset.antidiagonal k
  have hN : (N.choose k : ℝ) > 0 := Nat.cast_pos.mpr (Nat.choose_pos h0.2.1)
  have hS : ∑ ij ∈ T, f ij = N.choose k := by
    unfold T f
    rw [← Nat.add_choose_eq, Nat.add_sub_of_le h0.1]
  have hk : 1 ≤ k := h1.2.2.2.1
  let B : Finset (ℕ × ℕ) := {(0, k), (1, k - 1)}
  have hB : B ⊆ T := by
    intro z hz
    simp [B] at hz
    rcases hz with rfl | rfl
    · simp [T]
    · exact Finset.mem_antidiagonal.mpr (by omega)
  have hm : mass K N k 0 + mass K N k 1 = (∑ ij ∈ B, (f ij : ℝ)) / N.choose k := by
    rw [mass_choose h0, mass_choose h1, ← add_div]
    simp [B, f]
  have htotal : (∑ ij ∈ T, (f ij : ℝ)) = N.choose k := by exact_mod_cast hS
  rw [hm]
  constructor
  · rw [div_le_one hN, ← htotal]
    exact Finset.sum_le_sum_of_subset_of_nonneg hB (fun _ _ _ ↦ by positivity)
  · intro htwo
    have h₂ : Compatible K N k 2 := by
      have hA := le_trans htwo (min_le_left K k)
      have hB := le_trans htwo (min_le_right K k)
      unfold Compatible at *
      omega
    have hpos : 0 < (f (2, k - 2) : ℝ) := by
      have hm := mass_pos_of_compatible h₂
      rw [mass_choose h₂] at hm
      dsimp [f]
      exact_mod_cast ((div_pos_iff_of_pos_right hN).mp hm)
    have ht : (2, k - 2) ∈ T := Finset.mem_antidiagonal.mpr (by
      have := h₂.2.2.2.1
      omega)
    have hnot : (2, k - 2) ∉ B := by simp [B]
    have hsub : insert (2, k - 2) B ⊆ T := Finset.insert_subset ht hB
    have H := Finset.sum_le_sum_of_subset_of_nonneg (f := fun ij ↦ (f ij : ℝ))
      hsub (fun _ _ _ ↦ by positivity)
    rw [Finset.sum_insert hnot, htotal] at H
    exact (div_lt_one hN).mpr (by linarith)

private theorem binary_height {h : ℕ} (x : Fin h → ℕ) (hx : ∀ i, x i = 0 ∨ x i = 1)
    (K N k : ℕ) :
    likelihood K N k x =
      mass K N k 0 ^ (Finset.univ.filter fun i ↦ x i = 0).card *
      mass K N k 1 ^ (Finset.univ.filter fun i ↦ x i = 1).card := by
  unfold likelihood
  rw [← Finset.prod_filter_mul_prod_filter_not Finset.univ (fun i ↦ x i = 0)]
  congr 1
  · rw [← Finset.prod_const]
    exact Finset.prod_congr rfl fun i hi ↦ by rw [(Finset.mem_filter.mp hi).2]
  · have H : ∀ i, (¬ x i = 0) ↔ x i = 1 := fun i ↦ by have := hx i; omega
    simp_rw [H]
    rw [← Finset.prod_const]
    exact Finset.prod_congr rfl fun i hi ↦ by rw [(Finset.mem_filter.mp hi).2]

private theorem binary_saturation {z o : ℕ} (hz : 0 < z) (ho : 0 < o)
    {p q : ℝ} (hp : 0 < p) (hq : 0 < q) (ht : p + q ≤ 1) :
    let a : ℝ := (z : ℝ) / (z + o)
    let b : ℝ := (o : ℝ) / (z + o)
    p ^ z * q ^ o ≤ a ^ z * b ^ o ∧
      (p ^ z * q ^ o = a ^ z * b ^ o ↔ p = a ∧ q = b) := by
  intro a b
  let T : ℝ := (z : ℝ) + o
  have hz' : 0 < (z : ℝ) := by exact_mod_cast hz
  have ho' : 0 < (o : ℝ) := by exact_mod_cast ho
  have hT : 0 < T := add_pos hz' ho'
  have ha : 0 < a := div_pos hz' hT
  have hb : 0 < b := div_pos ho' hT
  have H₁ : (z : ℝ) * (p / a - 1) = T * p - z := by
    dsimp [a, T]
    field_simp
  have H₂ : (o : ℝ) * (q / b - 1) = T * q - o := by
    dsimp [b, T]
    field_simp
  have hsum : (z : ℝ) * (p / a - 1) + (o : ℝ) * (q / b - 1) ≤ 0 := by
    rw [H₁, H₂]
    have H := mul_le_mul_of_nonneg_left ht hT.le
    dsimp [T] at H ⊢
    nlinarith only [H]
  have hP : 0 < p ^ z * q ^ o := by positivity
  have hQ : 0 < a ^ z * b ^ o := by positivity
  have H (r s : ℝ) (hr : 0 < r) (hs : 0 < s) :
      log r - log s ≤ r / s - 1 ∧ (r ≠ s → log r - log s < r / s - 1) := by
    rw [← log_div hr.ne' hs.ne']
    refine ⟨log_le_sub_one_of_pos (div_pos hr hs), fun h ↦
      log_lt_sub_one_of_pos (div_pos hr hs) ?_⟩
    intro e
    exact h (by simpa using (div_eq_iff hs.ne').mp e)
  have hlog : log (p ^ z * q ^ o) - log (a ^ z * b ^ o) =
      (z : ℝ) * (log p - log a) + (o : ℝ) * (log q - log b) := by
    rw [log_mul (by positivity) (by positivity), log_mul (by positivity) (by positivity),
      log_pow, log_pow, log_pow, log_pow]
    ring
  have h₁ := mul_le_mul_of_nonneg_left (H p a hp ha).1 hz'.le
  have h₂ := mul_le_mul_of_nonneg_left (H q b hq hb).1 ho'.le
  refine ⟨(log_le_log_iff hP hQ).mp (by linarith only [hlog, h₁, h₂, hsum]), ?_⟩
  constructor
  · intro e
    constructor
    · by_contra h
      have H := mul_lt_mul_of_pos_left ((H p a hp ha).2 h) hz'
      rw [e, sub_self] at hlog
      linarith only [hlog, H, h₂, hsum]
    · by_contra h
      have H := mul_lt_mul_of_pos_left ((H q b hq hb).2 h) ho'
      rw [e, sub_self] at hlog
      linarith only [hlog, h₁, H, hsum]
  · rintro ⟨rfl, rfl⟩
    rfl

private theorem one_draw_masses {K N : ℕ} (h0 : Compatible K N 1 0)
    (h1 : Compatible K N 1 1) :
    mass K N 1 0 = 1 - (K : ℝ) / N ∧ mass K N 1 1 = (K : ℝ) / N := by
  have hNp : 0 < (N : ℝ) := Nat.cast_pos.mpr (by have := h0.2.1; omega)
  rw [mass_choose h0, mass_choose h1]
  simp [Nat.cast_sub h0.1, sub_div, div_self hNp.ne']

/-- Every maximizing compatible urn on a mixed binary sample has one
design count one and the other proportion equal to the empirical fraction
of ones. Both orientations and every integer representation are retained. -/
theorem mixed_binary_maximizers {h : ℕ} (x : Fin h → ℕ)
    (hx : ∀ i, x i = 0 ∨ x i = 1) (hzero : ∃ i, x i = 0) (hone : ∃ i, x i = 1) :
    let D := {p : ℕ × ℕ × ℕ | ∀ i, Compatible p.1 p.2.1 p.2.2 (x i)}
    let L := fun p : ℕ × ℕ × ℕ ↦ likelihood p.1 p.2.1 p.2.2 x
    let o := (Finset.univ.filter fun i ↦ x i = 1).card
    let z := (Finset.univ.filter fun i ↦ x i = 0).card
    ∀ p ∈ D, IsMaxOn L D p ↔
      (p.2.2 = 1 ∧ p.1 * (z + o) = o * p.2.1) ∨
        (p.1 = 1 ∧ p.2.2 * (z + o) = o * p.2.1) := by
  intro D L o z
  obtain ⟨i, hi⟩ := hzero
  obtain ⟨j, hj⟩ := hone
  have hz : 0 < z := Finset.card_pos.mpr ⟨i, by simp [hi]⟩
  have ho : 0 < o := Finset.card_pos.mpr ⟨j, by simp [hj]⟩
  let a : ℝ := (z : ℝ) / (z + o)
  let b : ℝ := (o : ℝ) / (z + o)
  have hab : 1 - b = a := by
    dsimp [a, b]
    have hzo : (z : ℝ) + o ≠ 0 := by positivity
    field_simp [hzo]
    ring
  have h0 (p : ℕ × ℕ × ℕ) (hp : p ∈ D) : Compatible p.1 p.2.1 p.2.2 0 := hi ▸ hp i
  have h1 (p : ℕ × ℕ × ℕ) (hp : p ∈ D) : Compatible p.1 p.2.1 p.2.2 1 := hj ▸ hp j
  have hbound (p : ℕ × ℕ × ℕ) (hp : p ∈ D) :
      L p ≤ a ^ z * b ^ o ∧
        (L p = a ^ z * b ^ o ↔ mass p.1 p.2.1 p.2.2 0 = a ∧
          mass p.1 p.2.1 p.2.2 1 = b) := by
    unfold L
    rw [binary_height x hx]
    exact binary_saturation hz ho (mass_pos_of_compatible (h0 p hp))
      (mass_pos_of_compatible (h1 p hp)) (two_mass_sum (h0 p hp) (h1 p hp)).1
  have hW (p : ℕ × ℕ × ℕ) (hp : p ∈ D) (hk : p.2.2 = 1)
      (hrel : p.1 * (z + o) = o * p.2.1) : L p = a ^ z * b ^ o := by
    have H := h1 p hp
    have hN : 0 < (p.2.1 : ℝ) := Nat.cast_pos.mpr (by unfold Compatible at H; omega)
    have hT : 0 < (z : ℝ) + o := by positivity
    have he : (p.1 : ℝ) / p.2.1 = b := by
      dsimp [b]
      exact (div_eq_div_iff hN.ne' hT.ne').mpr (by exact_mod_cast hrel)
    have hh₀ := h0 p hp
    rw [hk] at hh₀ H
    have hv := one_draw_masses hh₀ H
    apply (hbound p hp).2.mpr
    rw [hk, hv.1, hv.2, he, hab]
    exact ⟨rfl, rfl⟩
  let q : ℕ × ℕ × ℕ := (o, z + o, 1)
  have hq : q ∈ D := by
    intro l
    change Compatible o (z + o) 1 (x l)
    rcases hx l with h | h <;> rw [h] <;> unfold Compatible <;> omega
  have hqval := hW q hq rfl rfl
  intro p hp
  have hpN : 0 < (p.2.1 : ℝ) :=
    Nat.cast_pos.mpr (by have H := h1 p hp; unfold Compatible at H; omega)
  have hT : 0 < (z : ℝ) + o := by positivity
  have hswap (p : ℕ × ℕ × ℕ) (hp : p ∈ D) : (p.2.2, p.2.1, p.1) ∈ D := by
    intro l
    have H := hp l
    change Compatible p.2.2 p.2.1 p.1 (x l)
    unfold Compatible at H ⊢
    omega
  constructor
  · intro hm
    have he := (hbound p hp).1.antisymm (hqval ▸ hm hq)
    have hmass := (hbound p hp).2.mp he
    have hsum := two_mass_sum (h0 p hp) (h1 p hp)
    have hm1 : min p.1 p.2.2 = 1 := by
      have H := h1 p hp
      have hle : 1 ≤ min p.1 p.2.2 := le_min H.2.2.1 H.2.2.2.1
      refine hle.antisymm' ?_
      by_contra! h
      have hl := hsum.2 (by omega)
      rw [hmass.1, hmass.2, ← hab] at hl
      linarith
    have hrel {A : ℕ} (hprop : (A : ℝ) / p.2.1 = b) :
        A * (z + o) = o * p.2.1 := by
      dsimp [b] at hprop
      exact_mod_cast (div_eq_div_iff hpN.ne' hT.ne').mp hprop
    rcases min_choice p.1 p.2.2 with h | h
    · have hk : p.1 = 1 := h.symm.trans hm1
      have hh₀ := h0 _ (hswap p hp)
      have hh₁ := h1 _ (hswap p hp)
      rw [hk] at hh₀ hh₁
      have he := hmass.2
      rw [mass_swap, hk, (one_draw_masses hh₀ hh₁).2] at he
      exact Or.inr ⟨hk, hrel he⟩
    · have hk : p.2.2 = 1 := h.symm.trans hm1
      have hh₀ := h0 p hp
      have hh₁ := h1 p hp
      rw [hk] at hh₀ hh₁
      have he := hmass.2
      rw [hk, (one_draw_masses hh₀ hh₁).2] at he
      exact Or.inl ⟨hk, hrel he⟩
  · intro H
    have he : L p = a ^ z * b ^ o := by
      rcases H with ⟨hk, hr⟩ | ⟨hk, hr⟩
      · exact hW p hp hk hr
      · change likelihood p.1 p.2.1 p.2.2 x = _
        rw [likelihood_swap]
        exact hW _ (hswap p hp) hk hr
    exact fun v hv ↦ (hbound v hv).1.trans_eq he.symm

/-- The sample `(0, 1)` has exactly the two labeled even-population
rays, with both labels coinciding at population two. -/
theorem binary_pair_even_population :
    let x : Fin 2 → ℕ := fun i ↦ i.val
    let D := {p : ℕ × ℕ × ℕ | ∀ i, Compatible p.1 p.2.1 p.2.2 (x i)}
    let L := fun p : ℕ × ℕ × ℕ ↦ likelihood p.1 p.2.1 p.2.2 x
    (∀ p ∈ D, IsMaxOn L D p ↔
      (p.2.2 = 1 ∧ 2 * p.1 = p.2.1) ∨ (p.1 = 1 ∧ 2 * p.2.2 = p.2.1)) ∧
    (∀ d > 0, (d, 2 * d, 1) ∈ D ∧ IsMaxOn L D (d, 2 * d, 1) ∧
      (1, 2 * d, d) ∈ D ∧ IsMaxOn L D (1, 2 * d, d)) := by
  intro x D L
  have hx (i : Fin 2) : x i = 0 ∨ x i = 1 := by
    fin_cases i <;> decide
  have hw := mixed_binary_maximizers x hx ⟨0, rfl⟩ ⟨1, rfl⟩
  change ∀ p ∈ D, IsMaxOn L D p ↔
    (p.2.2 = 1 ∧ p.1 * (1 + 1) = 1 * p.2.1) ∨
      (p.1 = 1 ∧ p.2.2 * (1 + 1) = 1 * p.2.1) at hw
  have he (p : ℕ × ℕ × ℕ) (hp : p ∈ D) :
      IsMaxOn L D p ↔
        (p.2.2 = 1 ∧ 2 * p.1 = p.2.1) ∨ (p.1 = 1 ∧ 2 * p.2.2 = p.2.1) := by
    simpa [Nat.mul_comm] using hw p hp
  refine ⟨he, fun d hd ↦ ?_⟩
  have h₁ : (d, 2 * d, 1) ∈ D := by
    intro i
    change Compatible d (2 * d) 1 (x i)
    have H := hx i
    unfold Compatible
    omega
  have h₂ : (1, 2 * d, d) ∈ D := by
    intro i
    change Compatible 1 (2 * d) d (x i)
    have H := hx i
    unfold Compatible
    omega
  exact ⟨h₁, (he _ h₁).mpr (Or.inl ⟨rfl, rfl⟩),
    h₂, (he _ h₂).mpr (Or.inr ⟨rfl, rfl⟩)⟩

end UnivariateML.Base7bHyperBinary

end
