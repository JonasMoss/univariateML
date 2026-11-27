module

public import UnivariateML.Family.Discrete4BBinom

import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Filter Real
open scoped Topology BigOperators

namespace UnivariateML.Discrete5BBinom

private noncomputable def rising (k : ℕ) (a : ℝ) : ℝ :=
  ∏ j ∈ Finset.range k, (a + (j : ℝ))

private theorem rising_pos (k : ℕ) {a : ℝ} (ha : 0 < a) : 0 < rising k a :=
  Finset.prod_pos fun _ _ ↦ by positivity

/-- Fixed-size beta-binomial count mass, expressed as a finite product. -/
@[no_expose] noncomputable def countMass (m k : ℕ) (a b : ℝ) : ℝ :=
  (m.choose k : ℝ) * rising k a * rising (m-k) b / rising m (a+b)

private theorem countMass_nonneg {m k : ℕ} {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    0 ≤ countMass m k a b := by
  unfold countMass
  exact div_nonneg (mul_nonneg (mul_nonneg (Nat.cast_nonneg _) (rising_pos k ha).le)
    (rising_pos (m-k) hb).le)
    (rising_pos _ (add_pos ha hb)).le

private theorem positive_cell_le {m k : ℕ} (hk : 0 < k) (hkm : k ≤ m)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    countMass m k a b ≤ (m.choose k : ℝ) * (a/(a+b)) := by
  let s := a+b
  have hs : 0 < s := add_pos ha hb
  have hsplit : rising m s = rising k s * rising (m-k) (s+k) := by
    unfold rising
    rw [← Nat.add_sub_of_le hkm, Finset.prod_range_add]
    simp only [Nat.add_sub_of_le hkm, Nat.cast_add, add_assoc]
  have h₁ : rising k a ≤ (a/s) * rising k s := by
    obtain ⟨j,rfl⟩ := Nat.exists_eq_succ_of_ne_zero hk.ne'
    unfold rising
    rw [Finset.prod_range_succ', Finset.prod_range_succ']
    simp only [Nat.cast_zero, add_zero]
    have H : (∏ i ∈ Finset.range j, (a + ((i+1 : ℕ) : ℝ))) ≤
        ∏ i ∈ Finset.range j, (s + ((i+1 : ℕ) : ℝ)) := by
      apply Finset.prod_le_prod₀
      · intro i _; positivity
      · intro i _; dsimp [s]; linarith
    have he : a/s * ((∏ i ∈ Finset.range j, (s + ((i+1 : ℕ) : ℝ))) * s) =
        a * ∏ i ∈ Finset.range j, (s + ((i+1 : ℕ) : ℝ)) := by
      field_simp
    rw [he, mul_comm a]
    exact mul_le_mul_of_nonneg_right H ha.le
  have h₂ : rising (m-k) b ≤ rising (m-k) (s+k) := by
    unfold rising
    apply Finset.prod_le_prod₀
    · intro i _; positivity
    · intro i _; dsimp [s]; linarith [Nat.cast_nonneg (α := ℝ) k]
  change _ ≤ (m.choose k : ℝ) * (a/s)
  unfold countMass
  change (m.choose k : ℝ) * rising k a * rising (m-k) b / rising m s ≤ _
  apply (div_le_iff₀ (rising_pos _ hs)).mpr
  rw [hsplit]
  calc
    _ ≤ ((m.choose k : ℝ) * ((a/s) * rising k s)) * rising (m-k) (s+k) :=
      mul_le_mul (mul_le_mul_of_nonneg_left h₁ (Nat.cast_nonneg _)) h₂
        (rising_pos _ hb).le (mul_nonneg (Nat.cast_nonneg _) (mul_nonneg (div_nonneg ha.le hs.le) (rising_pos k hs).le))
    _ = _ := by ring

private theorem zero_cell_bounds (m : ℕ) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (b/(a+b))^m ≤ countMass m 0 a b ∧ countMass m 0 a b ≤ 1 := by
  let s := a+b
  have hs : 0 < s := add_pos ha hb
  have he : countMass m 0 a b = ∏ j ∈ Finset.range m, (b+(j : ℝ))/(s+(j : ℝ)) := by
    simp [countMass, rising, Finset.prod_div_distrib, s]
  rw [he]
  constructor
  · have he' : (b/(a+b))^m = ∏ _j ∈ Finset.range m, b/(a+b) := by simp only [Finset.prod_const, Finset.card_range]
    refine he'.le.trans ?_
    apply Finset.prod_le_prod₀
    · intro j _; positivity
    · intro j _
      apply (div_le_div_iff₀ hs (by positivity : 0 < s+(j : ℝ))).mpr
      have hj : 0 ≤ (j : ℝ) := Nat.cast_nonneg _
      dsimp [s]
      nlinarith [mul_nonneg ha.le hj]
  · apply Finset.prod_le_one₀
    · intro j _; positivity
    · intro j _
      apply (div_le_one (by positivity : 0 < s+(j : ℝ))).mpr
      dsimp [s]
      linarith

private theorem lower_atom {ι : Type*} {l : Filter ι} {a b : ι → ℝ}
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < b t)
    (h : Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 0))
    {m k : ℕ} (hk : k ≤ m) :
    Tendsto (fun t ↦ countMass m k (a t) (b t)) l (𝓝 (if k = 0 then 1 else 0)) := by
  rcases eq_or_lt_of_le (Nat.zero_le k) with he | hp
  · subst k
    simp only [ite_true]
    have h' : Tendsto (fun t ↦ (1-a t/(a t+b t))^m) l (𝓝 1) := by
      simpa using ((tendsto_const_nhds (x := (1 : ℝ))).sub h).pow m
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' h' tendsto_const_nhds
    · filter_upwards [hpos] with t ht
      have he : 1-a t/(a t+b t) = b t/(a t+b t) := by
        field_simp [show a t+b t ≠ 0 from (add_pos ht.1 ht.2).ne']
        ring
      rw [he]
      exact (zero_cell_bounds m ht.1 ht.2).1
    · exact hpos.mono fun _ ht ↦ (zero_cell_bounds m ht.1 ht.2).2
  · simp only [ite_eq_right hp.ne']
    have h' := h.const_mul (m.choose k : ℝ)
    rw [mul_zero] at h'
    exact tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds h'
      (hpos.mono fun _ ht ↦ countMass_nonneg ht.1 ht.2)
      (hpos.mono fun _ ht ↦ positive_cell_le hp hk ht.1 ht.2)

/-- At fixed size, arbitrary positive-shape paths with mixing mean tending
to either endpoint yield the corresponding atom, uniformly in total shape.
Size zero gives the same atom in both cases. -/
theorem endpoint_mixing_paths {ι : Type*} {l : Filter ι} {a b : ι → ℝ}
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < b t) {m k : ℕ} (hk : k ≤ m) :
    (Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 0) →
      Tendsto (fun t ↦ countMass m k (a t) (b t)) l (𝓝 (if k = 0 then 1 else 0))) ∧
    (Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 1) →
      Tendsto (fun t ↦ countMass m k (a t) (b t)) l (𝓝 (if k = m then 1 else 0))) := by
  refine ⟨fun h ↦ lower_atom hpos h hk, fun h ↦ ?_⟩
  have h' : Tendsto (fun t ↦ b t/(b t+a t)) l (𝓝 0) := by
    have H : Tendsto (fun t ↦ 1-a t/(a t+b t)) l (𝓝 0) := by
      simpa using (tendsto_const_nhds (x := (1 : ℝ))).sub h
    apply H.congr'
    filter_upwards [hpos] with t ht
    rw [add_comm (b t) (a t)]
    field_simp [show a t+b t ≠ 0 from (add_pos ht.1 ht.2).ne']
    ring
  have H := lower_atom (a := b) (b := a) (hpos.mono fun _ ht ↦ ht.symm) h' (Nat.sub_le m k)
  have he : m-k = 0 ↔ k = m := by omega
  rw [show (if m-k = 0 then (1 : ℝ) else 0) = if k = m then 1 else 0 by simp only [he]] at H
  apply H.congr
  intro t
  have hmk : m-(m-k) = k := by omega
  simp only [countMass, Nat.choose_symm hk, hmk]
  rw [add_comm (b t) (a t)]
  ring

end UnivariateML.Discrete5BBinom

end
