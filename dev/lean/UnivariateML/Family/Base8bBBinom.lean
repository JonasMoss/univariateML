module

public import UnivariateML.Family.Base8BBinom
public import UnivariateML.Family.BinomialSize
public import UnivariateML.Family.NegBinomial
public import UnivariateML.Generic.Base8bGammaRatio
import Mathlib.Data.Nat.Choose.Sum
import Mathlib.Data.Nat.Choose.Bounds
import Mathlib.NumberTheory.Harmonic.Bounds

/-!
# Beta-binomial likelihood branches

## Deviations

The unrestricted results give the saturated constant branch, the underdispersed
criterion with an interior observation at the smallest compatible size, and fixed-size
boundary traces. The growing-size joint closure and the equidispersed and overdispersed
comparisons remain separate. The rational integer-hold identity and exact peak
comparisons are inputs to the written cardinality procedures.
-/

set_option linter.style.header false

@[expose] public section

open Set Filter Topology
open scoped BigOperators

namespace UnivariateML.Base8bBBinom

open BetaBinomial

private noncomputable def rising (k : ℕ) (a : ℝ) : ℝ :=
  ∏ j ∈ Finset.range k, (a + j)

private theorem rising_succ (k : ℕ) (a : ℝ) :
    rising (k + 1) a = rising k a * (a + k) :=
  Finset.prod_range_succ _ _

private theorem rising_pos (k : ℕ) {a : ℝ} (ha : 0 < a) : 0 < rising k a :=
  Finset.prod_pos fun _ _ ↦ by positivity

private theorem rising_convolution (m : ℕ) (a b : ℝ) :
    ∑ ij ∈ Finset.antidiagonal m,
      (m.choose ij.1 : ℝ) * (rising ij.1 a * rising ij.2 b) = rising m (a + b) := by
  induction m with
  | zero => simp [rising]
  | succ m ih =>
    rw [Finset.sum_antidiagonal_choose_succ_mul (fun i j ↦ rising i a * rising j b),
      rising_succ, ← ih, Finset.sum_mul, ← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro ij hij
    have hs := Finset.mem_antidiagonal.mp hij
    rw [rising_succ, rising_succ, ← Nat.choose_symm_of_eq_add hs.symm]
    have hc : (ij.1 : ℝ) + (ij.2 : ℝ) = m := by exact_mod_cast hs
    rw [← hc]
    ring

private noncomputable def cell (m k : ℕ) (a b : ℝ) :=
  (m.choose k : ℝ) * (rising k a * rising (m-k) b) / rising m (a+b)

private theorem cell_pos {m k : ℕ} (hk : k ≤ m) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    0 < cell m k a b :=
  div_pos (mul_pos (Nat.cast_pos.mpr (Nat.choose_pos hk))
    (mul_pos (rising_pos _ ha) (rising_pos _ hb))) (rising_pos _ (add_pos ha hb))

private theorem cell_sum (m : ℕ) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∑ k ∈ Finset.range (m+1), cell m k a b = 1 := by
  have h := rising_convolution m a b
  rw [Finset.Nat.sum_antidiagonal_eq_sum_range_succ
    (fun i j ↦ (m.choose i : ℝ) * (rising i a * rising j b))] at h
  simp only [cell, ← Finset.sum_div, h, div_self (rising_pos _ (add_pos ha hb)).ne']

private theorem log_cell {m k : ℕ} (hk : k ≤ m) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    Real.log (cell m k a b) = Real.log (m.choose k) + risingLog k a +
      risingLog (m-k) b - risingLog m (a+b) := by
  have hr (k : ℕ) {z : ℝ} (hz : 0 < z) : Real.log (rising k z) = risingLog k z := by
    unfold rising risingLog
    exact Real.log_prod fun j _ ↦ (show 0 < z + (j : ℝ) by positivity).ne'
  have hchoose : 0 < (m.choose k : ℝ) := Nat.cast_pos.mpr (Nat.choose_pos hk)
  have hA := rising_pos k ha
  have hB := rising_pos (m-k) hb
  unfold cell
  rw [Real.log_div (mul_pos hchoose (mul_pos hA hB)).ne'
    (rising_pos _ (add_pos ha hb)).ne', Real.log_mul hchoose.ne' (mul_pos hA hB).ne',
    Real.log_mul hA.ne' hB.ne', hr _ ha, hr _ hb, hr _ (add_pos ha hb)]
  ring

private theorem cell_lt_one {m k : ℕ} (hm : 0 < m) (hk : k ≤ m)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    cell m k a b < 1 := by
  let j := if k = 0 then 1 else 0
  have hj : j ≤ m := by dsimp [j]; split <;> omega
  have hjk : j ≠ k := by
    dsimp [j]
    split <;> omega
  have hsub : {k, j} ⊆ Finset.range (m + 1) := by
    intro l hl
    simp only [Finset.mem_insert, Finset.mem_singleton] at hl
    rcases hl with rfl | rfl <;> exact Finset.mem_range.mpr (by omega)
  have h := Finset.sum_le_sum_of_subset_of_nonneg (f := fun l ↦ cell m l a b)
    hsub (fun l hl _ ↦ (cell_pos (by have := Finset.mem_range.mp hl; omega) ha hb).le)
  rw [cell_sum m ha hb, Finset.sum_pair hjk.symm] at h
  linarith [cell_pos hj ha hb]

/-- The all-free zero-count branch has exactly the size-zero ridge. Positive
constant samples have supremum zero in log units and no finite winner. -/
theorem all_free_constant {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c) :
    let feasible : Set (ℕ × ℝ × ℝ) :=
      {q | (∀ i, x i ≤ q.1) ∧ q.2 ∈ domain}
    let L : ℕ × ℝ × ℝ → ℝ := fun q ↦ logLik q.1 x q.2.1 q.2.2
    IsLUB (L '' feasible) 0 ∧
      ∀ q ∈ feasible, IsMaxOn L feasible q ↔ q.1 = 0 := by
  dsimp only
  have hbound (m : ℕ) (hcm : c ≤ m) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
      logLik m x a b ≤ 0 ∧ (logLik m x a b = 0 ↔ m = 0) := by
    by_cases hm : m = 0
    · subst m
      have hc : c = 0 := by omega
      simp [logLik, risingLog, hx, hc]
    · have hpos : 0 < m := Nat.pos_of_ne_zero hm
      have he : logLik m x a b = (n : ℝ) * Real.log (cell m c a b) := by
        simp [logLik, hx, log_cell hcm ha hb]
        ring
      have hlt : logLik m x a b < 0 := by
        rw [he]
        exact mul_neg_of_pos_of_neg (Nat.cast_pos.mpr hn)
          (Real.log_neg (cell_pos hcm ha hb) (cell_lt_one hpos hcm ha hb))
      exact ⟨hlt.le, by simp [hlt.ne, hm]⟩
  have hu : IsLUB ((fun q : ℕ × ℝ × ℝ ↦ logLik q.1 x q.2.1 q.2.2) ''
      {q | (∀ i, x i ≤ q.1) ∧ q.2 ∈ domain}) 0 := by
    refine ⟨?_, ?_⟩
    · rintro _ ⟨⟨m, a, b⟩, ⟨hf, ha, hb⟩, rfl⟩
      exact (hbound m (by simpa [hx] using hf ⟨0, hn⟩) ha hb).1
    · intro y hy
      by_cases hc : c = 0
      · let hm : ℕ × ℝ × ℝ := (0, 1, 1)
        have hval : logLik 0 x 1 1 = 0 := (hbound 0 hc.le one_pos one_pos).2.mpr rfl
        rw [← hval]
        exact hy ⟨hm, ⟨fun i ↦ by simp [hm, hx, hc], one_pos, one_pos⟩, rfl⟩
      · have hsup := (allSize_sup (Nat.pos_of_ne_zero hc) hn hx).1
        rw [← hsup]
        apply csSup_le
        · exact ⟨_, ⟨(1, 1), ⟨one_pos, one_pos⟩, rfl⟩⟩
        · rintro _ ⟨⟨a, b⟩, hab, rfl⟩
          exact hy ⟨(c, a, b), ⟨fun i ↦ (hx i).le, hab⟩, rfl⟩
  refine ⟨hu, ?_⟩
  rintro ⟨m, a, b⟩ ⟨hf, ha, hb⟩
  have H := (hbound m (by simpa [hx] using hf ⟨0, hn⟩) ha hb).2
  rw [← H]
  constructor
  · intro h
    apply le_antisymm (hu.1 ⟨(m, a, b), ⟨hf, ha, hb⟩, rfl⟩)
    apply hu.2
    rintro _ ⟨r, hr, rfl⟩
    exact h hr
  · intro h r hr
    change logLik r.1 x r.2.1 r.2.2 ≤ logLik m x a b
    rw [h]
    exact hu.1 ⟨r, hr, rfl⟩

private theorem endpoint_sum {m : ℕ} (hm : 0 < m) {a b : ℝ}
    (ha : 0 < a) (hb : 0 < b) :
    cell m 0 a b + cell m m a b ≤ 1 ∧
      (1 < m → cell m 0 a b + cell m m a b < 1) := by
  have hs {s : Finset ℕ} (hs : s ⊆ Finset.range (m + 1)) :
      ∑ j ∈ s, cell m j a b ≤ 1 := by
    rw [← cell_sum m ha hb]
    exact Finset.sum_le_sum_of_subset_of_nonneg hs
      (fun j hj _ ↦ (cell_pos (by have := Finset.mem_range.mp hj; omega) ha hb).le)
  constructor
  · have H := hs (s := {0, m}) (by
      intro j hj
      simp only [Finset.mem_insert, Finset.mem_singleton] at hj
      rcases hj with rfl | rfl <;> exact Finset.mem_range.mpr (by omega))
    rwa [Finset.sum_pair hm.ne'.symm] at H
  · intro h
    have H := hs (s := {1, 0, m}) (by
      intro j hj
      simp only [Finset.mem_insert, Finset.mem_singleton] at hj
      rcases hj with rfl | rfl | rfl <;> exact Finset.mem_range.mpr (by omega))
    rw [Finset.sum_insert (by simp; omega), Finset.sum_pair hm.ne'.symm] at H
    linarith [cell_pos (show 1 ≤ m by omega) ha hb]

private theorem one_cell (k : ℕ) (hk : k ≤ 1) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    cell 1 k a b = if k = 0 then 1 - a / (a+b) else a / (a+b) := by
  have hD := (add_pos ha hb).ne'
  interval_cases k
  · simp [cell, rising]
    field_simp
    ring
  · simp [cell, rising]

private theorem two_cell_likelihood {m n u : ℕ} (hm : u ≤ m) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0 ∨ x i = u) (hu' : 0 < u) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    let s := cell m 0 a b + cell m u a b
    let p := cell m u a b / s
    logLik m x a b = binomialLogLik 1 (fun i ↦ if x i = 0 then 0 else 1) p + (n : ℝ) * Real.log s := by
  dsimp only
  let z := cell m 0 a b
  let v := cell m u a b
  let s := z + v
  let p := v/s
  have hz : 0 < z := cell_pos m.zero_le ha hb
  have hu : 0 < v := cell_pos hm ha hb
  have hs : 0 < s := add_pos hz hu
  have hp : 0 < p := div_pos hu hs
  have hq : 0 < 1-p := sub_pos.mpr ((div_lt_one hs).mpr (by dsimp [s]; linarith))
  have hZ : z = (1-p)*s := by dsimp [p,s]; field_simp; ring
  have hU : v = p*s := by dsimp [p]; field_simp
  change _ = binomialLogLik 1 (fun i ↦ if x i = 0 then 0 else 1) p + (n : ℝ) * Real.log s
  unfold logLik binomialLogLik
  rw [show (n : ℝ) * Real.log s = ∑ _i : Fin n, Real.log s by simp, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  rw [← log_cell ((hx i).elim (fun h ↦ h ▸ m.zero_le) (fun h ↦ h ▸ hm)) ha hb]
  rcases hx i with hi | hi
  · simp [hi]
    change Real.log z = Real.log (1-p) + Real.log s
    conv_lhs => rw [hZ, Real.log_mul hq.ne' hs.ne']
  · simp [hi,hu'.ne']
    change Real.log v = Real.log p + Real.log s
    conv_lhs => rw [hU, Real.log_mul hp.ne' hs.ne']

private theorem endpoint_path {m k : ℕ} (hm : 0 < m) (hk : k = 0 ∨ k = m)
    (p : ℝ) :
    Tendsto (fun t : ℝ ↦ cell m k (p*t) ((1-p)*t)) (𝓝[>] 0)
      (𝓝 (if k = 0 then 1-p else p)) := by
  obtain ⟨j, rfl⟩ := Nat.exists_eq_succ_of_ne_zero hm.ne'
  have hr (b : ℝ) : rising (j+1) b = b * rising j (b+1) := by
    unfold rising
    rw [Finset.prod_range_succ', mul_comm]
    simp only [Nat.cast_zero, add_zero, Nat.cast_add, Nat.cast_one, add_right_comm b, ← add_assoc]
  have hc : Continuous (rising j) := by unfold rising; fun_prop
  have core (v : ℝ) :
      Tendsto (fun t : ℝ ↦ v * (rising j (1+v*t) / rising j (1+t))) (𝓝[>] 0) (𝓝 v) := by
    have hN (w : ℝ) : Tendsto (fun t : ℝ ↦ rising j (1+w*t)) (𝓝[>] 0) (𝓝 (rising j 1)) := by
      have h : Continuous (fun t : ℝ ↦ rising j (1+w*t)) := hc.comp (by fun_prop)
      simpa using (h.continuousAt (x := (0 : ℝ))).tendsto.mono_left
        (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
    have d := rising_pos j (zero_lt_one : (0 : ℝ) < 1)
    simpa [d.ne'] using ((hN v).div (by simpa using hN 1) d.ne').const_mul v
  have hsum (t : ℝ) : p*t+(1-p)*t = t := by ring
  rcases hk with rfl | rfl
  · simp only [ite_true]
    apply (core (1-p)).congr'
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    have ht' : 0 < t := ht
    have hd := rising_pos j (show 0 < 1+t by positivity)
    simp only [cell, Nat.choose_zero_right, Nat.cast_one, one_mul, Nat.sub_zero, hsum,
      hr, show rising 0 (p*t) = 1 by simp [rising]]
    rw [add_comm t 1, add_comm ((1-p)*t) 1]
    field_simp [ht'.ne', hd.ne']
  · simp only [Nat.succ_ne_zero, ite_false]
    apply (core p).congr'
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    have ht' : 0 < t := ht
    have hd := rising_pos j (show 0 < 1+t by positivity)
    simp only [cell, Nat.choose_self, Nat.cast_one, one_mul, Nat.sub_self, hsum,
      hr, show rising 0 ((1-p)*t) = 1 by simp [rising]]
    rw [mul_one, add_comm t 1, add_comm (p*t) 1]
    field_simp [ht'.ne', hd.ne']

private theorem endpoint_likelihood_path {m n : ℕ} (hm : 0 < m) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0 ∨ x i = m) {p : ℝ} (hp : p ∈ Ioo 0 1) :
    Tendsto (fun t : ℝ ↦ logLik m x (p*t) ((1-p)*t)) (𝓝[>] 0)
      (𝓝 (binomialLogLik 1 (fun i ↦ if x i = 0 then 0 else 1) p)) := by
  have h (i : Fin n) := (endpoint_path hm (hx i) p).log
    (show (if x i = 0 then 1-p else p) ≠ 0 by
      split
      · exact (sub_pos.mpr hp.2).ne'
      · exact hp.1.ne')
  have H := tendsto_finsetSum Finset.univ (fun i (_ : i ∈ (Finset.univ : Finset (Fin n))) ↦ h i)
  have hlim : (∑ i : Fin n, Real.log (if x i = 0 then 1-p else p)) =
      binomialLogLik 1 (fun i ↦ if x i = 0 then 0 else 1) p := by
    unfold binomialLogLik
    apply Finset.sum_congr rfl
    intro i _
    split <;> simp_all
  rw [hlim] at H
  apply H.congr'
  filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
  have ht' : 0 < t := ht
  unfold logLik
  apply Finset.sum_congr rfl
  intro i _
  have hi : x i ≤ m := (hx i).elim (fun h ↦ h ▸ m.zero_le) (fun h ↦ h.le)
  exact log_cell hi (mul_pos hp.1 ht') (mul_pos (sub_pos.mpr hp.2) ht')

/-- For every compatible size, the endpoint histogram is bounded by its
Bernoulli profile. The bound is strict above size one and is attained at
size one when the empirical endpoint proportion is interior. -/
theorem endpoint_profile_bound {m n : ℕ} (hm : 0 < m) (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0 ∨ x i = m)
    (hp : proportion 1 (fun i ↦ if x i = 0 then 0 else 1) ∈ Ioo 0 1) :
    let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
    let B := binomialLogLik 1 y (proportion 1 y)
    (∀ a > 0, ∀ b > 0, logLik m x a b ≤ B ∧ (1 < m → logLik m x a b < B)) ∧
      (m = 1 → logLik m x (proportion 1 y) (1 - proportion 1 y) = B) := by
  dsimp only
  let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
  let p := proportion 1 y
  change (∀ a > 0, ∀ b > 0, _ ≤ binomialLogLik 1 y p ∧ (_ → _ < binomialLogLik 1 y p)) ∧
    (m = 1 → logLik m x p (1-p) = binomialLogLik 1 y p)
  constructor
  · intro a ha b hb
    let s := cell m 0 a b + cell m m a b
    let q := cell m m a b / s
    have h0 := cell_pos (m := m) (k := 0) m.zero_le ha hb
    have h1 := cell_pos (m := m) (k := m) le_rfl ha hb
    have hs : 0 < s := add_pos h0 h1
    have hq : q ∈ Ioo 0 1 := ⟨div_pos h1 hs, (div_lt_one hs).mpr (by dsimp [s]; linarith)⟩
    have h := binomial_isMaxOn (m := 1) (by decide) hn
      (x := y) (fun i ↦ by dsimp [y]; split <;> omega) hp.1 hp.2 hq
    change binomialLogLik 1 y q ≤ binomialLogLik 1 y p at h
    have hS := endpoint_sum hm ha hb
    have he := two_cell_likelihood (m := m) le_rfl x hx hm ha hb
    change logLik m x a b = binomialLogLik 1 y q + (n : ℝ) * Real.log s at he
    rw [he]
    have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
    exact ⟨add_le_of_nonpos_right (mul_nonpos_of_nonneg_of_nonpos hN.le
      (Real.log_nonpos hs.le hS.1)) |>.trans h,
      fun hm ↦ (add_lt_of_neg_right _ (mul_neg_of_pos_of_neg hN
        (Real.log_neg hs (hS.2 hm)))).trans_le h⟩
  · rintro rfl
    have hq : 0 < 1-p := sub_pos.mpr hp.2
    have hz := one_cell 0 (by decide) hp.1 hq
    have hu := one_cell 1 (by decide) hp.1 hq
    simp only [ite_true, Nat.one_ne_zero, ite_false] at hz hu
    change cell 1 0 p (1-p) = 1 - p / (p+(1-p)) at hz
    change cell 1 1 p (1-p) = p / (p+(1-p)) at hu
    rw [show p+(1-p) = 1 by ring, div_one] at hz hu
    have he := two_cell_likelihood (m := 1) le_rfl x hx (by decide) hp.1 hq
    dsimp only at he
    change logLik 1 x p (1-p) = _
    rw [he, hz, hu, show 1-p+p = 1 by ring]
    simp [y]

private theorem binary_proportion {n : ℕ} (hn : 0 < n) (y : Fin n → ℕ)
    (hy : ∀ i, y i ≤ 1) (h0 : ∃ i, y i = 0) (h1 : ∃ i, y i = 1) :
    proportion 1 y ∈ Ioo 0 1 := by
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hp : 0 < countSum y := by
    obtain ⟨i, hi⟩ := h1
    calc
      0 < (y i : ℝ) := by simp [hi]
      _ ≤ countSum y := Finset.single_le_sum (fun j _ ↦ Nat.cast_nonneg (y j)) (Finset.mem_univ i)
  have hq : countSum y < (n : ℝ) := by
    obtain ⟨i, hi⟩ := h0
    calc
      countSum y < ∑ _j : Fin n, (1 : ℝ) :=
        Finset.sum_lt_sum (fun j _ ↦ by exact_mod_cast hy j)
          ⟨i, Finset.mem_univ i, by simp [hi]⟩
      _ = n := by simp
  exact ⟨by simpa [proportion, mean] using div_pos hp hN,
    by simpa [proportion, mean] using (div_lt_one hN).mpr hq⟩

private theorem mixed_endpoint_sup {m n : ℕ} (hm : 1 < m) (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0 ∨ x i = m) (h0 : ∃ i, x i = 0) (hm' : ∃ i, x i = m) :
    let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
    let B := binomialLogLik 1 y (proportion 1 y)
    IsLUB ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) B ∧
      ∀ q ∈ domain, logLik m x q.1 q.2 < B := by
  dsimp only
  let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
  let p := proportion 1 y
  let B := binomialLogLik 1 y p
  have hp : p ∈ Ioo 0 1 := binary_proportion hn y
    (fun i ↦ by dsimp [y]; split <;> omega)
    (h0.imp fun i hi ↦ by simp [y, hi])
    (hm'.imp fun i hi ↦ by simp [y, hi, show m ≠ 0 by omega])
  have H (q : ℝ × ℝ) (hq : q ∈ domain) : logLik m x q.1 q.2 < B :=
    ((endpoint_profile_bound (by omega) hn x hx hp).1 q.1 hq.1 q.2 hq.2).2 hm
  refine ⟨⟨?_, ?_⟩, H⟩
  · rintro _ ⟨q, hq, rfl⟩
    exact (H q hq).le
  · intro c hc
    apply le_of_tendsto (endpoint_likelihood_path (by omega) x hx hp)
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    exact hc ⟨(p*t, (1-p)*t), ⟨mul_pos hp.1 ht, mul_pos (sub_pos.mpr hp.2) ht⟩, rfl⟩

/-- At held size, the compatible sample attains exactly at size zero,
in the mixed Bernoulli case, or with an interior count and positive
dispersion excess at size at least two. -/
theorem held_size_criterion {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) :
    (∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q) ↔
      m = 0 ∨
      (m = 1 ∧ (∃ i, x i = 0) ∧ ∃ i, x i = 1) ∨
      (2 ≤ m ∧ (∃ i, 0 < x i ∧ x i < m) ∧
        (m : ℝ) * proportion m x * (1 - proportion m x) < variance x) := by
  let L : ℝ × ℝ → ℝ := fun q ↦ logLik m x q.1 q.2
  change (∃ q ∈ domain, IsMaxOn L domain q) ↔ _
  by_cases hm0 : m = 0
  · subst m
    have hz (i : Fin n) : x i = 0 := Nat.eq_zero_of_le_zero (hx i)
    simp only [true_or, iff_true]
    exact ⟨(1, 1), ⟨one_pos, one_pos⟩, fun q _ ↦ by simp [L, logLik, risingLog, hz]⟩
  have hm : 0 < m := Nat.pos_of_ne_zero hm0
  by_cases hi : ∃ i, 0 < x i ∧ x i < m
  · have hm2 : 2 ≤ m := by obtain ⟨i, h, h'⟩ := hi; omega
    have hm1 : m ≠ 1 := by omega
    simpa [hm0, hm1, hm2, hi, L] using exists_max_iff hm2 hn hx hi
  have he (i : Fin n) : x i = 0 ∨ x i = m := by
    have h := hx i
    by_contra! h'
    exact hi ⟨i, by omega, by omega⟩
  have hNo {B : ℝ} (hB : sSup (L '' domain) = B) (hlt : ∀ q ∈ domain, L q < B) :
      ¬ ∃ q ∈ domain, IsMaxOn L domain q := by
    rintro ⟨q, hq, hmax⟩
    have h := hlt q hq
    rw [← hB] at h
    exact (not_le_of_gt h) (csSup_le ⟨_, ⟨q, hq, rfl⟩⟩ (by
      rintro _ ⟨r, hr, rfl⟩
      exact hmax hr))
  by_cases h0 : ∃ i, x i = 0
  · by_cases hM : ∃ i, x i = m
    · by_cases hm1 : m = 1
      · subst m
        have hy (i : Fin n) : (if x i = 0 then (0 : ℕ) else 1) ≤ 1 := by split <;> omega
        let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
        let p := proportion 1 y
        have hp : p ∈ Ioo 0 1 := binary_proportion hn y hy
          (h0.imp fun i hi ↦ by simp [y, hi]) (hM.imp fun i hi ↦ by simp [y, hi])
        have H := endpoint_profile_bound (by decide) hn x he hp
        simp only [true_and, h0, hM, or_true, iff_true, Nat.one_ne_zero, true_or]
        exact ⟨(p, 1-p), ⟨hp.1, sub_pos.mpr hp.2⟩,
          fun q hq ↦ (H.1 q.1 hq.1 q.2 hq.2).1.trans_eq (H.2 rfl).symm⟩
      · have H := mixed_endpoint_sup (by omega) hn x he h0 hM
        simp only [hm0, hm1, hi, false_and, and_false, false_or, iff_false]
        exact hNo (H.1.csSup_eq ⟨_, ⟨(1, 1), ⟨one_pos, one_pos⟩, rfl⟩⟩) H.2
    · have hz (i : Fin n) : x i = 0 :=
        (he i).resolve_right (fun h ↦ hM ⟨i, h⟩)
      have hnil : ¬ ∃ i, x i = 1 := fun ⟨i, h⟩ ↦ by simp [hz i] at h
      simp only [hm0, hi, hnil, and_false, false_and, or_false, iff_false]
      exact hNo (allZero_sup hm hn hz).1 (allZero_sup hm hn hz).2
  · have hfull (i : Fin n) : x i = m :=
      (he i).resolve_left (fun h ↦ h0 ⟨i, h⟩)
    simp only [hm0, hi, h0, false_and, and_false, or_false, iff_false]
    exact hNo (allSize_sup hm hn hfull).1 (allSize_sup hm hn hfull).2

/-- The identifiable attained size profiles on underdispersed positive-mean
data lie strictly below the dispersion threshold. This does not compare
their heights against the binomial suffix. -/
theorem underdispersed_attained_size_prefix {m n : ℕ} (hm : 2 ≤ m) (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) (hi : ∃ i, 0 < x i ∧ x i < m)
    (hv : variance x < mean x) :
    (∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q) ↔
      (m : ℝ) < mean x ^ 2 / (mean x - variance x) := by
  rw [exists_max_iff hm hn hx hi]
  have hM : 0 < (m : ℝ) := by exact_mod_cast (show 0 < m by omega)
  have Hbin : (m : ℝ) * proportion m x * (1 - proportion m x) =
      mean x - mean x ^ 2 / m := by
    unfold proportion
    field_simp [hM.ne']
  rw [Hbin]
  rw [lt_div_iff₀ (sub_pos.mpr hv)]
  constructor <;> intro h
  · have H := (lt_div_iff₀ hM).mp (show mean x - variance x < mean x ^ 2 / m by linarith)
    simpa [mul_comm] using H
  · have H := (lt_div_iff₀ hM).mpr (by simpa [mul_comm] using h :
      (mean x - variance x) * m < mean x ^ 2)
    linarith

private theorem interior_proportion {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) (hi : ∃ i, 0 < x i ∧ x i < m) :
    proportion m x ∈ Ioo 0 1 := by
  obtain ⟨i, hi, hi'⟩ := hi
  have hM : 0 < (m : ℝ) := Nat.cast_pos.mpr (by omega)
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hp : 0 < countSum x :=
    (Nat.cast_pos.mpr hi).trans_le
      (Finset.single_le_sum (fun j _ ↦ Nat.cast_nonneg (x j)) (Finset.mem_univ i))
  have hq : countSum x < (n : ℝ) * m := by
    calc
      countSum x < ∑ _j : Fin n, (m : ℝ) :=
        Finset.sum_lt_sum (fun j _ ↦ by exact_mod_cast hx j)
          ⟨i, Finset.mem_univ i, by exact_mod_cast hi'⟩
      _ = (n : ℝ) * m := by simp
  refine ⟨div_pos (div_pos hp hN) hM, (div_lt_one hM).mpr ?_⟩
  exact (div_lt_iff₀ hN).mpr (by simpa [mul_comm] using hq)

private theorem weighted_tilt {m : ℕ} {w r t : ℕ → ℝ}
    (hw : ∀ j, 0 ≤ w j) (hr : Antitone r) (ht : Monotone t) :
    (∑ j ∈ Finset.range m, w j * t j * r j) * (∑ j ∈ Finset.range m, w j) ≤
      (∑ j ∈ Finset.range m, w j * r j) * (∑ j ∈ Finset.range m, w j * t j) := by
  let S := Finset.range m
  have h (i j : ℕ) : 0 ≤ w i * w j * ((r i - r j) * (t j - t i)) := by
    apply mul_nonneg (mul_nonneg (hw i) (hw j))
    rcases le_total i j with hij | hji
    · exact mul_nonneg (sub_nonneg.mpr (hr hij)) (sub_nonneg.mpr (ht hij))
    · exact mul_nonneg_of_nonpos_of_nonpos (sub_nonpos.mpr (hr hji)) (sub_nonpos.mpr (ht hji))
  have H := Finset.sum_nonneg (s := S) (f := fun i ↦ ∑ j ∈ S, _) fun i _ ↦
    Finset.sum_nonneg (s := S) fun j _ ↦ h i j
  have he (i j : ℕ) :
      w i * w j * ((r i - r j) * (t j - t i)) =
        w i * r i * (w j * t j) + w i * t i * (w j * r j) -
        w i * t i * r i * w j - w i * (w j * t j * r j) := by ring
  simp only [he, Finset.sum_sub_distrib, Finset.sum_add_distrib, ← Finset.mul_sum,
    ← Finset.sum_mul] at H
  change _ * _ ≤ _ * _
  linarith [show (∑ i ∈ S, w i * t i) * (∑ i ∈ S, w i * r i) =
    (∑ i ∈ S, w i * r i) * (∑ i ∈ S, w i * t i) by ring,
    show (∑ i ∈ S, w i) * (∑ i ∈ S, w i * t i * r i) =
    (∑ i ∈ S, w i * t i * r i) * (∑ i ∈ S, w i) by ring]

private noncomputable def recipSum (m : ℕ) (z : ℝ) : ℝ :=
  ∑ j ∈ Finset.range m, 1 / (z + j)

private theorem recipSum_pos {m : ℕ} (hm : 0 < m) {z : ℝ} (hz : 0 < z) :
    0 < recipSum m z := by
  unfold recipSum
  exact Finset.sum_pos (fun j _ ↦ by positivity) ⟨0, Finset.mem_range.mpr hm⟩

private theorem ratio_decreases {m k : ℕ} (hk : k ≤ m) (hm : 0 < m)
    {a b c : ℝ} (ha : 0 < a) (hb : 0 < b) (hbc : b < c) :
    recipSum k c / recipSum m (a+c) ≤ recipSum k b / recipSum m (a+b) ∧
      (0 < k → recipSum k c / recipSum m (a+c) < recipSum k b / recipSum m (a+b)) := by
  let w (j : ℕ) : ℝ := 1 / (a+b+j)
  let t (j : ℕ) : ℝ := (a+b+j) / (a+c+j)
  let r (z : ℝ) (j : ℕ) : ℝ := if j < k then 1 + a/(z+j) else 0
  have hc : 0 < c := hb.trans hbc
  have hD₀ (j : ℕ) : 0 < a+b+(j : ℝ) := by positivity
  have hD₁ (j : ℕ) : 0 < a+c+(j : ℝ) := by positivity
  have hT : Monotone t := by
    intro i j hij
    have h : (i : ℝ) ≤ j := by exact_mod_cast hij
    apply (div_le_div_iff₀ (hD₁ i) (hD₁ j)).mpr
    nlinarith [mul_nonneg (sub_nonneg.mpr h) (sub_nonneg.mpr hbc.le)]
  have hR : Antitone (r b) := by
    intro i j hij
    dsimp [r]
    by_cases hj : j < k
    · rw [ite_eq_left hj, ite_eq_left (hij.trans_lt hj)]
      gcongr
    · rw [ite_eq_right hj]
      split <;> positivity
  have hW (j : ℕ) : w j * t j = 1 / (a+c+j) := by
    dsimp [w, t]
    field_simp
  have hN (z : ℝ) (hz : 0 < z) :
      ∑ j ∈ Finset.range m, (1 / (a+z+j)) * r z j = recipSum k z := by
    unfold recipSum
    rw [← Finset.sum_subset (Finset.range_mono hk)
      (f := fun j ↦ (1 / (a+z+j)) * r z j)
      (fun j _ h ↦ by simp [r, Finset.mem_range.not.mp h])]
    apply Finset.sum_congr rfl
    intro j hj
    have h₀ : 0 < z+(j : ℝ) := by positivity
    have h₁ : 0 < a+z+(j : ℝ) := by positivity
    rw [show r z j = 1 + a/(z+j) by simp [r, Finset.mem_range.mp hj]]
    field_simp
    ring
  have h₀ := recipSum_pos hm (add_pos ha hb)
  have h₁ := recipSum_pos hm (add_pos ha hc)
  have H := weighted_tilt (m := m) (w := w) (fun j ↦ (one_div_pos.mpr (hD₀ j)).le) hR hT
  simp only [hW] at H
  change (∑ j ∈ Finset.range m, (1 / (a+c+j)) * r b j) * recipSum m (a+b) ≤
    (∑ j ∈ Finset.range m, (1 / (a+b+j)) * r b j) * recipSum m (a+c) at H
  rw [hN b hb] at H
  have H' := (div_le_div_iff₀ h₁ h₀).mpr H
  have hterm (j : ℕ) :
      (1 / (a+c+j)) * r c j ≤ (1 / (a+c+j)) * r b j ∧
        (j < k → (1 / (a+c+j)) * r c j < (1 / (a+c+j)) * r b j) := by
    by_cases hj : j < k
    · have h₀' : 0 < b+(j : ℝ) := by positivity
      have hlt : a/(c+j) < a/(b+j) := by
        apply div_lt_div_of_pos_left ha h₀'
        linarith
      have hh : r c j < r b j := by simpa [r, hj] using hlt
      have HH := mul_lt_mul_of_pos_left hh (one_div_pos.mpr (hD₁ j))
      exact ⟨HH.le, fun _ ↦ HH⟩
    · simp [r, hj]
  constructor
  · refine (div_le_div_of_nonneg_right ?_ h₁.le).trans H'
    rw [← hN c hc]
    exact Finset.sum_le_sum fun j _ ↦ (hterm j).1
  · intro hk'
    refine (div_lt_div_of_pos_right ?_ h₁).trans_le H'
    rw [← hN c hc]
    exact Finset.sum_lt_sum (fun j _ ↦ (hterm j).1)
      ⟨0, Finset.mem_range.mpr hm, (hterm 0).2 hk'⟩

private theorem held_alpha_log_deriv {m n : ℕ} (x : Fin n → ℕ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    HasDerivAt (logLik m x a)
      ((∑ i, recipSum (m-x i) b) - (n : ℝ) * recipSum m (a+b)) b := by
  have h (k : ℕ) (v : ℝ) (hv : 0 < v+b) :
      HasDerivAt (fun z ↦ risingLog k (v+z)) (recipSum k (v+b)) b := by
    unfold risingLog recipSum
    apply HasDerivAt.fun_sum
    intro j _
    have hj : 0 < v+b+(j : ℝ) := by positivity
    simpa using (((hasDerivAt_id b).const_add v).add_const (j : ℝ)).log hj.ne'
  unfold logLik
  have H (i : Fin n) :
      HasDerivAt
        (fun z ↦ Real.log (m.choose (x i)) + risingLog (x i) a +
          risingLog (m-x i) z - risingLog m (a+z))
        (recipSum (m-x i) b - recipSum m (a+b)) b :=
    ((by simpa using h (m-x i) 0 (by simpa using hb) :
      HasDerivAt (risingLog (m-x i)) (recipSum (m-x i) b) b).const_add _).sub
      (h m a (add_pos ha hb))
  simpa [Finset.sum_sub_distrib] using HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ H i)

private theorem held_alpha_stationary_injective {m n : ℕ} (hm : 0 < m)
    (x : Fin n → ℕ) (hi : ∃ i, x i < m)
    {a b c : ℝ} (ha : 0 < a) (hb : 0 < b) (hc : 0 < c)
    (h₁ : deriv (logLik m x a) b = 0) (h₂ : deriv (logLik m x a) c = 0) :
    b = c := by
  have root (z : ℝ) (hz : 0 < z) (h : deriv (logLik m x a) z = 0) :
      ∑ i, recipSum (m-x i) z / recipSum m (a+z) = (n : ℝ) := by
    rw [← Finset.sum_div, (div_eq_iff (recipSum_pos hm (add_pos ha hz)).ne')]
    rw [(held_alpha_log_deriv x ha hz).deriv, sub_eq_zero] at h
    exact h
  have decrease {v w : ℝ} (hv : 0 < v) (hvw : v < w) :
      ∑ i, recipSum (m-x i) w / recipSum m (a+w) <
        ∑ i, recipSum (m-x i) v / recipSum m (a+v) := by
    obtain ⟨i, hi⟩ := hi
    exact Finset.sum_lt_sum (fun j _ ↦ (ratio_decreases (Nat.sub_le _ _) hm ha hv hvw).1)
      ⟨i, Finset.mem_univ i,
        (ratio_decreases (Nat.sub_le m (x i)) hm ha hv hvw).2 (by omega)⟩
  rcases lt_trichotomy b c with h | h | h
  · exact ((decrease hb h).ne ((root c hc h₂).trans (root b hb h₁).symm)).elim
  · exact h
  · exact ((decrease hc h).ne ((root b hb h₁).trans (root c hc h₂).symm)).elim

/-- With positive alpha and fixed size, all positive beta values maximize at
size zero. At positive size any two attained beta maximizers coincide. -/
theorem held_alpha_size_unique {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    {a : ℝ} (ha : 0 < a) :
    (m = 0 → ∀ b > 0, IsMaxOn (logLik m x a) (Ioi 0) b) ∧
    (0 < m → ∀ b ∈ Ioi 0, ∀ c ∈ Ioi 0,
      IsMaxOn (logLik m x a) (Ioi 0) b → IsMaxOn (logLik m x a) (Ioi 0) c → b = c) := by
  refine ⟨?_, ?_⟩
  · rintro rfl b _ c _
    have hz (i : Fin n) : x i = 0 := Nat.eq_zero_of_le_zero (hx i)
    simp [logLik, hz, risingLog]
  · intro hm b hb c hc h₁ h₂
    have stat {v : ℝ} (hv : v ∈ Ioi 0) (h : IsMaxOn (logLik m x a) (Ioi 0) v) :
        deriv (logLik m x a) v = 0 :=
      (h.isLocalMax (Ioi_mem_nhds hv)).deriv_eq_zero
    by_cases hi : ∃ i, x i < m
    · exact held_alpha_stationary_injective hm x hi ha hb hc (stat hb h₁) (stat hc h₂)
    · have hf (i : Fin n) : x i = m := by have hh := hx i; push Not at hi; exact le_antisymm hh (hi i)
      have HH := (held_alpha_log_deriv (m := m) x ha hb).deriv
      rw [stat hb h₁] at HH
      have hp := mul_pos (Nat.cast_pos.mpr hn) (recipSum_pos hm (add_pos ha hb))
      simp [hf, recipSum] at HH hp
      rcases HH with h | h
      · omega
      · simp [h] at hp

private theorem reflected_logLik {m n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    (a b : ℝ) : logLik m (fun i ↦ m-x i) b a = logLik m x a b := by
  unfold logLik
  apply Finset.sum_congr rfl
  intro i _
  simp only [Nat.choose_symm (hx i), show m-(m-x i) = x i by have := hx i; omega]
  abel_nf

/-- With positive beta and fixed size, all positive alpha values maximize at
size zero. At positive size any two attained alpha maximizers coincide. -/
theorem held_beta_size_unique {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    {b : ℝ} (hb : 0 < b) :
    (m = 0 → ∀ a > 0, IsMaxOn (fun a ↦ logLik m x a b) (Ioi 0) a) ∧
    (0 < m → ∀ a ∈ Ioi 0, ∀ c ∈ Ioi 0,
      IsMaxOn (fun a ↦ logLik m x a b) (Ioi 0) a →
      IsMaxOn (fun a ↦ logLik m x a b) (Ioi 0) c → a = c) := by
  have he : logLik m (fun i ↦ m-x i) b = fun a ↦ logLik m x a b :=
    funext fun a ↦ reflected_logLik x hx a b
  rw [← he]
  exact held_alpha_size_unique hn (fun i ↦ m-x i) (fun _ ↦ Nat.sub_le _ _) hb

/-- Exact neighboring binomial-profile comparisons for the sample
`(7,8,13)`, and a feasible size-13 beta-binomial point above the size-38
binomial peak. The binomial probabilities are the fitted (28/(3N)). -/
theorem separated_peak_witness :
    let x : Fin 3 → ℕ := ![7, 8, 13]
    let binom (N : ℕ) :=
      ∏ i, (N.choose (x i) : ℝ) * (28 / (3 * (N : ℝ))) ^ (x i) *
        (1 - 28 / (3 * (N : ℝ))) ^ (N - x i)
    let witness :=
      ∏ i, ((13 : ℕ).choose (x i) : ℝ) *
        ((∏ j ∈ Finset.range (x i), ((52/21 : ℝ) + j)) *
          (∏ j ∈ Finset.range (13-x i), ((73/83 : ℝ) + j))) /
        (∏ j ∈ Finset.range 13, ((52/21 : ℝ) + 73/83 + j))
    binom 36 < binom 37 ∧ binom 37 < binom 38 ∧ binom 39 < binom 38 ∧
      binom 38 < witness := by
  dsimp only
  simp only [Fin.prod_univ_succ, Fin.prod_univ_zero, Matrix.cons_val_zero,
    Matrix.cons_val_succ, mul_one]
  norm_num [Nat.choose_eq_descFactorial_div_factorial, Nat.descFactorial,
    Nat.factorial, Finset.prod_range_succ]

private theorem rising_scaled {m : ℕ} {s : ℝ} (hs : 0 < s) (a : ℝ) :
    rising m a = s ^ m * ∏ j ∈ Finset.range m, (a/s+(j : ℝ)/s) := by
  unfold rising
  rw [← show (∏ _j ∈ Finset.range m, s) = s^m by simp, ← Finset.prod_mul_distrib]
  apply Finset.prod_congr rfl
  intro j _
  field_simp [hs.ne']

/-- The fixed-size binomial face is valid along simultaneous shape
sequences. The limiting mixing proportion may be either endpoint. -/
theorem fixed_size_binomial_trace {ι : Type*} {l : Filter ι} {a b : ι → ℝ}
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < b t) {m k : ℕ} (hk : k ≤ m) {p : ℝ}
    (hS : Tendsto (fun t ↦ a t+b t) l atTop)
    (hP : Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 p)) :
    Tendsto (fun t ↦
      (m.choose k : ℝ) * ((∏ j ∈ Finset.range k, (a t+(j : ℝ))) *
        (∏ j ∈ Finset.range (m-k), (b t+(j : ℝ)))) /
        (∏ j ∈ Finset.range m, (a t+b t+(j : ℝ)))) l
      (𝓝 ((m.choose k : ℝ) * p^k * (1-p)^(m-k))) := by
  let s (t : ι) := a t+b t
  have hQ : Tendsto (fun t ↦ b t/s t) l (𝓝 (1-p)) := by
    apply (tendsto_const_nhds.sub hP : Tendsto
      (fun t ↦ 1-a t/s t) l (𝓝 (1-p))).congr'
    filter_upwards [hpos] with t ht
    have hs : 0 < s t := add_pos ht.1 ht.2
    field_simp [hs.ne']
    dsimp [s]
    ring
  have ht := tendsto_inv_atTop_zero.comp hS
  have hj (j : ℕ) : Tendsto (fun t ↦ (j : ℝ)/s t) l (𝓝 0) := by
    simpa [s, div_eq_mul_inv] using ht.const_mul (j : ℝ)
  have hA : Tendsto (fun t ↦ ∏ j ∈ Finset.range k, (a t/s t + (j : ℝ)/s t))
      l (𝓝 (p^k)) := by
    simpa only [add_zero, Finset.prod_const, Finset.card_range] using
      tendsto_finsetProd (Finset.range k) (fun j _ ↦ hP.add (hj j))
  have hB : Tendsto (fun t ↦ ∏ j ∈ Finset.range (m-k), (b t/s t + (j : ℝ)/s t))
      l (𝓝 ((1-p)^(m-k))) := by
    simpa only [add_zero, Finset.prod_const, Finset.card_range] using
      tendsto_finsetProd (Finset.range (m-k)) (fun j _ ↦ hQ.add (hj j))
  have hC : Tendsto (fun t ↦ ∏ j ∈ Finset.range m, (1 + (j : ℝ)/s t)) l (𝓝 1) := by
    simpa only [add_zero, Finset.prod_const_one] using
      tendsto_finsetProd (Finset.range m) (fun j _ ↦ (hj j).const_add (1 : ℝ))
  have H := (((hA.mul hB).const_mul (m.choose k : ℝ)).div hC one_ne_zero)
  simp only [div_one] at H
  have hval : (m.choose k : ℝ) * (p^k * (1-p)^(m-k)) =
      (m.choose k : ℝ) * p^k * (1-p)^(m-k) := by ring
  rw [hval] at H
  apply H.congr'
  filter_upwards [hpos] with t ht
  have hs : 0 < s t := add_pos ht.1 ht.2
  have hD : (∏ j ∈ Finset.range m, (1+(j : ℝ)/s t)) ≠ 0 :=
    (Finset.prod_pos fun j _ ↦ by positivity).ne'
  change _ = cell m k (a t) (b t)
  rw [cell, rising_scaled hs (a t), rising_scaled hs (b t)]
  change _ = _ / rising m (s t)
  rw [rising_scaled hs (s t), div_self hs.ne']
  have hpow : s t^m = s t^k * s t^(m-k) := by rw [← pow_add, Nat.add_sub_of_le hk]
  rw [hpow]
  simp only [Pi.div_apply]
  have eq : (∏ j ∈ Finset.range m, ((s t+(j : ℝ))/s t)) =
      ∏ j ∈ Finset.range m, (1+(j : ℝ)/s t) := by
    apply Finset.prod_congr rfl
    intro j _
    rw [add_div, div_self hs.ne']
  field_simp [hs.ne', hD]

private theorem rising_first {m : ℕ} (hm : 0 < m) (a : ℝ) :
    rising m a = a * rising (m-1) (a+1) := by
  obtain ⟨j,rfl⟩ := Nat.exists_eq_succ_of_ne_zero hm.ne'
  unfold rising
  rw [Finset.prod_range_succ', mul_comm]
  simp only [Nat.succ_sub_one, Nat.cast_zero, add_zero, Nat.cast_add, Nat.cast_one,
    ← add_assoc, add_right_comm a]

/-- All simultaneous paths to zero total shape at positive fixed size
give the two-endpoint trace at the limiting mixing proportion. -/
theorem fixed_size_two_point_trace {ι : Type*} {l : Filter ι} {a b : ι → ℝ}
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < b t) {m k : ℕ} (hm : 0 < m) (hk : k ≤ m)
    {p : ℝ} (hS : Tendsto (fun t ↦ a t+b t) l (𝓝 0))
    (hP : Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 p)) :
    Tendsto (fun t ↦
      (m.choose k : ℝ) * ((∏ j ∈ Finset.range k, (a t+(j : ℝ))) *
        (∏ j ∈ Finset.range (m-k), (b t+(j : ℝ)))) /
        (∏ j ∈ Finset.range m, (a t+b t+(j : ℝ)))) l
      (𝓝 (if k = 0 then 1-p else if k = m then p else 0)) := by
  let s (t : ι) := a t+b t
  have hA : Tendsto a l (𝓝 0) := by
    have H := hP.mul hS
    rw [mul_zero] at H
    apply H.congr'
    filter_upwards [hpos] with t ht
    exact div_mul_cancel₀ _ (add_pos ht.1 ht.2).ne'
  have hB : Tendsto b l (𝓝 0) := by
    have H := hS.sub hA
    rw [sub_self] at H
    simpa only [add_sub_cancel_left] using H
  have hQ : Tendsto (fun t ↦ b t/s t) l (𝓝 (1-p)) := by
    apply (tendsto_const_nhds.sub hP : Tendsto
      (fun t ↦ 1-a t/s t) l (𝓝 (1-p))).congr'
    filter_upwards [hpos] with t ht
    have hs : 0 < s t := add_pos ht.1 ht.2
    field_simp [hs.ne']
    dsimp [s]
    ring
  have R (j : ℕ) {f : ι → ℝ} (hf : Tendsto f l (𝓝 0)) :
      Tendsto (fun t ↦ rising j (f t+1)) l (𝓝 (rising j 1)) := by
    have hc : Continuous (rising j) := by unfold rising; fun_prop
    simpa only [zero_add, Function.comp_def] using hc.continuousAt.tendsto.comp (hf.add_const 1)
  have hD := R (m-1) hS
  change Tendsto (fun t ↦ cell m k (a t) (b t)) l _
  by_cases h₀ : k = 0
  · subst k
    have H := hQ.mul ((R (m-1) hB).div hD (rising_pos _ one_pos).ne')
    rw [div_self (rising_pos _ one_pos).ne', mul_one] at H
    simp only [ite_true]
    apply H.congr'
    filter_upwards [hpos] with t ht
    have hS' : 0 < s t := add_pos ht.1 ht.2
    simp only [cell, Nat.choose_zero_right, Nat.cast_one, Nat.sub_zero, one_mul,
      show rising 0 (a t) = 1 by simp [rising], rising_first hm]
    change _ = b t * rising (m-1) (b t+1) / (s t * rising (m-1) (s t+1))
    simp only [Pi.div_apply]
    dsimp [s] at hS' ⊢
    field_simp [hS'.ne']
  by_cases hM : k = m
  · subst k
    have H := hP.mul ((R (m-1) hA).div hD (rising_pos _ one_pos).ne')
    rw [div_self (rising_pos _ one_pos).ne', mul_one] at H
    simp only [ite_eq_right hm.ne', ite_true]
    apply H.congr'
    filter_upwards [hpos] with t ht
    have hS' : 0 < s t := add_pos ht.1 ht.2
    simp only [cell, Nat.choose_self, Nat.cast_one, Nat.sub_self, one_mul,
      show rising 0 (b t) = 1 by simp [rising], rising_first hm, mul_one]
    change _ = a t * rising (m-1) (a t+1) / (s t * rising (m-1) (s t+1))
    simp only [Pi.div_apply]
    dsimp [s] at hS' ⊢
    field_simp [hS'.ne']
  · have h₁ : 0 < k := Nat.pos_of_ne_zero h₀
    have h₂ : 0 < m-k := by omega
    have H := (((hP.mul hB).mul ((R (k-1) hA).mul (R (m-k-1) hB))).div hD
      (rising_pos _ one_pos).ne').const_mul (m.choose k : ℝ)
    simp only [mul_zero, zero_mul, zero_div] at H
    simp only [ite_eq_right h₀, ite_eq_right hM]
    apply H.congr'
    filter_upwards [hpos] with t ht
    have hS' : 0 < s t := add_pos ht.1 ht.2
    simp only [cell, rising_first hm, rising_first h₁, rising_first h₂]
    change _ = _ / (s t * rising (m-1) (s t+1))
    simp only [Pi.div_apply]
    dsimp [s] at hS' ⊢
    field_simp [hS'.ne']

private theorem rising_append (m k : ℕ) (z : ℝ) :
    rising (m+k) z = rising m z * rising k (z+m) := by
  unfold rising
  rw [Finset.prod_range_add]
  congr 1
  apply Finset.prod_congr rfl
  intro j _
  push_cast
  ring

/-- At a positive integer alpha hold, fixed-count beta-binomial masses
at every feasible size have a rational extension in size and beta. -/
theorem integer_held_alpha_rational_mass {A N k : ℕ} (hk : k ≤ N)
    {b : ℝ} (hb : 0 < b) :
    (N.choose k : ℝ) *
        ((∏ j ∈ Finset.range k, ((A : ℝ)+j)) *
          (∏ j ∈ Finset.range (N-k), (b+j))) /
        (∏ j ∈ Finset.range N, ((A : ℝ)+b+j)) =
      ((∏ j ∈ Finset.range A, (b+j)) / (∏ j ∈ Finset.range A, (b+(N : ℝ)+j))) *
        (((∏ j ∈ Finset.range k, ((N : ℝ)-j)) *
          (∏ j ∈ Finset.range k, ((A : ℝ)+j))) /
          ((k.factorial : ℝ) * (∏ j ∈ Finset.range k, (b+(N : ℝ)-k+j)))) := by
  change cell N k A b = rising A b / rising A (b+N) *
    (((∏ j ∈ Finset.range k, ((N : ℝ)-j)) * rising k A) /
      ((k.factorial : ℝ) * rising k (b+N-k)))
  have hC : (N.choose k : ℝ) * k.factorial =
      ∏ j ∈ Finset.range k, ((N : ℝ)-j) := by
    have h := congrArg (fun n : ℕ ↦ (n : ℝ)) (Nat.descFactorial_eq_factorial_mul_choose N k)
    simp only [Nat.descFactorial_eq_prod_range, Nat.cast_mul, Nat.cast_prod] at h
    rw [mul_comm, ← h]
    apply Finset.prod_congr rfl
    intro j hj
    exact Nat.cast_sub ((Finset.mem_range.mp hj).le.trans hk)
  have hD : rising N ((A : ℝ)+b) * rising A b = rising N b * rising A (b+N) := by
    have h₁ := rising_append A N b
    have h₂ := rising_append N A b
    rw [Nat.add_comm, h₂, add_comm b (A : ℝ), mul_comm] at h₁
    simpa only [mul_comm] using h₁.symm
  have hN : rising N b = rising (N-k) b * rising k (b+N-k) := by
    have h := rising_append (N-k) k b
    rwa [Nat.sub_add_cancel hk, Nat.cast_sub hk, ← add_sub_assoc] at h
  have hN' : 0 < rising N ((A : ℝ)+b) := rising_pos _ (by positivity)
  have hA' : 0 < rising A (b+(N : ℝ)) := rising_pos _ (by positivity)
  have hK' : 0 < rising k (b+(N : ℝ)-k) := by
    apply rising_pos
    have h : (k : ℝ) ≤ N := by exact_mod_cast hk
    linarith
  rw [cell, ← hC]
  field_simp [hN'.ne', hA'.ne', hK'.ne']
  rw [hN] at hD
  linear_combination -((N.choose k : ℝ) * rising k A) * hD

/-- For underdispersed data with an interior observation at the smallest
compatible size, the unrestricted fit attains exactly when a finite
shape profile in the dispersion prefix reaches the best ordinary binomial. -/
theorem underdispersed_global_criterion {n M : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hM : ∃ i, x i = M) (hi : ∃ i, 0 < x i ∧ x i < M)
    (hv : variance x < mean x) :
    let D : Set (ℕ × ℝ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ q.2 ∈ domain}
    let L := fun q : ℕ × ℝ × ℝ ↦ logLik q.1 x q.2.1 q.2.2
    let cutoff := mean x ^ 2 / (mean x - variance x)
    let B := sSup (BinomialSize.profile x '' BinomialSize.feasible x)
    (∃ q ∈ D, IsMaxOn L D q) ↔
      ∃ q ∈ D, (q.1 : ℝ) < cutoff ∧ B ≤ L q := by
  classical
  intro D L cutoff B
  have hvar : BinomialSize.variance x < BinomialSize.mean x := hv
  obtain ⟨K,hK,hmaxK⟩ := (BinomialSize.finite_profile_maximizers_of_underdispersion x hn hvar).1
  have hb : B = BinomialSize.profile x K := by
    have hne : (BinomialSize.profile x '' BinomialSize.feasible x).Nonempty :=
      ⟨_, ⟨K,hK,rfl⟩⟩
    apply IsLUB.csSup_eq _ hne
    refine ⟨?_,?_⟩
    · rintro _ ⟨N,hN,rfl⟩
      exact hmaxK hN
    · intro a ha
      exact ha ⟨K,hK,rfl⟩
  have hsz {N : ℕ} (hN : ∀ i, x i ≤ N) : 2 ≤ N ∧ ∃ i, 0 < x i ∧ x i < N := by
    obtain ⟨j,hj⟩ := hM
    obtain ⟨i,hi,hi'⟩ := hi
    have h := hj ▸ hN j
    exact ⟨by omega, ⟨i,hi,hi'.trans_le h⟩⟩
  have hpos : 0 < BinomialSize.mean x := by
    obtain ⟨i,hi,_⟩ := hi
    unfold BinomialSize.mean
    exact div_pos (Finset.sum_pos' (fun _ _ ↦ Nat.cast_nonneg _)
      ⟨i,Finset.mem_univ _,Nat.cast_pos.mpr hi⟩) (Nat.cast_pos.mpr hn)
  have hbin {N : ℕ} (hN : ∀ i, x i ≤ N) :
      BinomialSize.profile x N = binomialLogLik N x (proportion N x) := by
    have hN' : 0 < N := by have := (hsz hN).1; omega
    rw [BinomialSize.profile_eq_binomial_logLik x hn hpos hN' hN]
    unfold binomialLogLik
    apply Finset.sum_congr rfl
    intro i _
    rw [Nat.cast_sub (hN i)]
    rfl
  have outside {q : ℕ × ℝ × ℝ} (hq : q ∈ D) (hN : cutoff ≤ q.1) : L q < B := by
    have H := hsz hq.1
    have hu := interior_proportion hn x hq.1 H.2
    have hdisp : variance x ≤ (q.1 : ℝ) * proportion q.1 x * (1 - proportion q.1 x) := by
      by_contra! hh
      have H' := (exists_max_iff H.1 hn hq.1 H.2).mpr hh
      exact (not_lt_of_ge hN) ((underdispersed_attained_size_prefix H.1 hn x hq.1 H.2 hv).mp H')
    apply (necessity H.1 hn hq.1 hu.1 hu.2 hdisp hq.2.1 hq.2.2).trans_le
    rw [← hbin hq.1, hb]
    exact hmaxK hq.1
  constructor
  · rintro ⟨q,hq,hm⟩
    have hUK := interior_proportion hn x hK (hsz hK).2
    have hf : B ≤ L q := by
      rw [hb,hbin hK]
      apply le_of_tendsto (logLik_tendsto hK hUK.1 hUK.2)
      filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
      exact hm (a := (K, proportion K x/t, (1-proportion K x)/t))
        ⟨hK, ⟨div_pos hUK.1 ht, div_pos (sub_pos.mpr hUK.2) ht⟩⟩
    exact ⟨q,hq,not_le.mp (fun h ↦ (outside hq h).not_ge hf),hf⟩
  · rintro ⟨q,hq,hprefix,hqB⟩
    let S := (Finset.range (⌈cutoff⌉₊+1)).filter (fun N ↦
      (∀ i, x i ≤ N) ∧ (N : ℝ) < cutoff)
    have hS {N : ℕ} (hN : ∀ i, x i ≤ N) (hN' : (N : ℝ) < cutoff) : N ∈ S := by
      have hN'' : N ≤ ⌈cutoff⌉₊ := by exact_mod_cast hN'.le.trans (Nat.le_ceil cutoff)
      simp [S,hN,hN',hN'']
    have hf {N : ℕ} (hN : N ∈ S) : (∀ i, x i ≤ N) ∧ (N : ℝ) < cutoff :=
      (Finset.mem_filter.mp hN).2
    have prof {N : ℕ} (hN : N ∈ S) :
        ∃ p ∈ domain, IsMaxOn (fun p : ℝ × ℝ ↦ logLik N x p.1 p.2) domain p :=
      (underdispersed_attained_size_prefix (hsz (hf hN).1).1 hn x (hf hN).1
        (hsz (hf hN).1).2 hv).mpr (hf hN).2
    let p (N : ℕ) : ℝ × ℝ := if h : N ∈ S then (prof h).choose else (1,1)
    have spec {N : ℕ} (hN : N ∈ S) :
        p N ∈ domain ∧ IsMaxOn (fun p : ℝ × ℝ ↦ logLik N x p.1 p.2) domain (p N) := by
      simpa only [p,dite_eq_left hN] using (prof hN).choose_spec
    have hqS : q.1 ∈ S := hS hq.1 hprefix
    obtain ⟨N,hN,hNmax⟩ := Finset.exists_max_image S (fun N ↦ L (N,p N)) ⟨_,hqS⟩
    have hbN : B ≤ L (N,p N) := hqB.trans (((spec hqS).2 hq.2).trans (hNmax _ hqS))
    refine ⟨(N,p N), ⟨(hf hN).1,(spec hN).1⟩,fun t ht ↦ ?_⟩
    by_cases h : (t.1 : ℝ) < cutoff
    · have hT := hS ht.1 h
      exact ((spec hT).2 ht.2).trans (hNmax _ hT)
    · exact (outside ht (le_of_not_gt h)).le.trans hbN

private theorem cell_zero (m : ℕ) (a b : ℝ) :
    cell m 0 a b = rising m b / rising m (a+b) := by
  simp [cell, rising]

private theorem rise_mono (m : ℕ) {a b : ℝ} (ha : 0 ≤ a) (hab : a ≤ b) :
    rising m a ≤ rising m b := by
  unfold rising
  exact Finset.prod_le_prod₀ (fun j _ ↦ by positivity)
    (fun j _ ↦ by linarith)

private theorem rise_ge_pow (m : ℕ) {a : ℝ} (ha : 0 ≤ a) : a^m ≤ rising m a := by
  have h : a^m = ∏ _j ∈ Finset.range m, a := by simp
  rw [h]
  exact Finset.prod_le_prod₀ (fun _ _ ↦ ha) (fun _ _ ↦ le_add_of_nonneg_right (Nat.cast_nonneg _))

private theorem cell_zero_anti (m : ℕ) {a c b : ℝ} (ha : 0 < a) (hb : 0 < b) (hac : a ≤ c) :
    cell m 0 c b ≤ cell m 0 a b := by
  simp only [cell_zero]
  exact div_le_div_of_nonneg_left (rising_pos _ hb).le
    (rising_pos _ (add_pos ha hb))
    (rise_mono _ (add_pos ha hb).le (by linarith))

private theorem rising_integer_swap (A m : ℕ) {b : ℝ} (hb : 0 < b) :
    rising m b / rising m ((A : ℝ)+b) = rising A b / rising A (b+m) := by
  have H := rising_append A m b
  rw [Nat.add_comm, rising_append, add_comm b (A : ℝ)] at H
  field_simp [(rising_pos m (show 0 < (A : ℝ)+b by positivity)).ne',
    (rising_pos A (show 0 < b+(m : ℝ) by positivity)).ne']
  linear_combination H

private theorem fixed_zero_tendsto {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    Tendsto (fun m : ℕ ↦ cell m 0 a b) atTop (𝓝 0) := by
  obtain ⟨j, hj⟩ := exists_nat_gt (1/a)
  have hj' : 1 ≤ (j : ℝ)*a := ((div_lt_iff₀ ha).mp hj).le
  have hge (m : ℕ) : (cell m 0 a b)^j ≤ b/(b+m) := by
    have hterm (k : ℕ) : ((b+k)/(a+b+k))^j ≤ (b+k)/(1+b+k) := by
      let z : ℝ := b+k
      have hz : 0 < z := by dsimp [z]; positivity
      have h1 : 0 < 1+a/z := by positivity
      have h2 : 0 < 1+1/z := by positivity
      have h := one_add_mul_le_pow (show (-2 : ℝ) ≤ a/z by linarith [div_nonneg ha.le hz.le]) j
      have he : 1+1/z ≤ (1+a/z)^j := by
        apply le_trans _ h
        gcongr
        simpa [mul_div_assoc] using (div_le_div_of_nonneg_right hj' hz.le)
      have H := one_div_le_one_div_of_le h2 he
      have H₁ : 1/(1+a/z) = z/(a+z) := by field_simp; ring
      have H₂ : 1/(1+1/z) = z/(1+z) := by field_simp; ring
      rw [show a+b+(k : ℝ) = a+z by dsimp [z]; ring,
        show 1+b+(k : ℝ) = 1+z by dsimp [z]; ring]
      have hpow : 1/(1+a/z)^j = (1/(1+a/z))^j := by rw [div_pow, one_pow]
      rwa [hpow, H₁, H₂] at H
    have hz (c : ℝ) : cell m 0 c b =
        ∏ k ∈ Finset.range m, (b+(k : ℝ))/(c+b+k) := by
      simp [cell_zero,rising,Finset.prod_div_distrib]
    have h1 : cell m 0 1 b = b/(b+m) := by
      rw [cell_zero]
      simpa [rising] using rising_integer_swap 1 m hb
    rw [hz, ← Finset.prod_pow]
    calc
      _ ≤ ∏ k ∈ Finset.range m, (b+(k : ℝ))/(1+b+k) :=
        Finset.prod_le_prod₀ (fun k _ ↦ by positivity) (fun k _ ↦ hterm k)
      _ = _ := (hz 1).symm.trans h1
  have hlim : Tendsto (fun m : ℕ ↦ b/(b+m)) atTop (𝓝 0) := by
    have hm : Tendsto (fun m : ℕ ↦ b+(m : ℝ)) atTop atTop :=
      tendsto_atTop_add_const_left _ _ tendsto_natCast_atTop_atTop
    simpa [div_eq_mul_inv, Function.comp_def] using (tendsto_inv_atTop_zero.comp hm).const_mul b
  rw [Metric.tendsto_nhds]
  intro ε hε
  filter_upwards [hlim.eventually_lt_const (pow_pos hε j)] with m hm
  have hz : 0 ≤ cell m 0 a b := (cell_pos m.zero_le ha hb).le
  rw [Real.dist_eq, sub_zero, abs_of_nonneg hz]
  by_contra! hh
  exact (not_lt_of_ge (pow_le_pow_left₀ hε.le hh j)) ((hge m).trans_lt hm)

private theorem positive_cell_bounds {m k : ℕ} (hk : 0 < k) (hm : 2*k ≤ m)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (a ≤ (k : ℝ)+1 →
      cell m k a b ≤ ((2 : ℝ)^k * rising (k-1) ((k : ℝ)+2)) * a * cell m 0 a b) ∧
    ((k : ℝ)+1 ≤ a →
      cell m k a b ≤ (rising (k+1) b * (2 : ℝ)^(k+1)) / (m : ℝ)) := by
  have hkm : k ≤ m := by omega
  have hm0 : 0 < (m : ℝ) := Nat.cast_pos.mpr (by omega)
  have hmk : (m : ℝ)/2 ≤ (m-k : ℕ) := by
    rw [Nat.cast_sub hkm]
    have hh : (2 : ℝ)*k ≤ m := by exact_mod_cast hm
    linarith
  have hbk : (m : ℝ)/2 ≤ b+((m-k : ℕ) : ℝ) := by linarith
  have hC : (m.choose k : ℝ) ≤ (m : ℝ)^k := by exact_mod_cast Nat.choose_le_pow m k
  have hS : 0 < a+b := add_pos ha hb
  have hBk : 0 < b+((m-k : ℕ) : ℝ) := by positivity
  have hsplit_b : rising m b = rising (m-k) b * rising k (b+((m-k : ℕ) : ℝ)) := by
    have h := rising_append (m-k) k b
    rwa [Nat.sub_add_cancel hkm] at h
  have hsplit_s : rising m (a+b) =
      rising (m-k) (a+b) * rising k (a+b+((m-k : ℕ) : ℝ)) := by
    have h := rising_append (m-k) k (a+b)
    rwa [Nat.sub_add_cancel hkm] at h
  constructor
  · intro hsmall
    have hR : rising k a ≤ a * rising (k-1) ((k : ℝ)+2) := by
      rw [rising_first hk]
      exact mul_le_mul_of_nonneg_left (rise_mono _ (by positivity) (by linarith)) ha.le
    have hratio : (m.choose k : ℝ) / rising k (b+((m-k : ℕ) : ℝ)) ≤ (2 : ℝ)^k := by
      apply (div_le_iff₀ (rising_pos _ hBk)).mpr
      refine hC.trans ?_
      have hD := (pow_le_pow_left₀ (by positivity : 0 ≤ (m : ℝ)/2) hbk k).trans
        (rise_ge_pow k hBk.le)
      calc
        _ = (2 : ℝ)^k * ((m : ℝ)/2)^k := by rw [← mul_pow]; congr 1; ring
        _ ≤ _ := mul_le_mul_of_nonneg_left hD (by positivity)
    have hrel : cell m k a b =
        ((m.choose k : ℝ) / rising k (b+((m-k : ℕ) : ℝ))) * rising k a * cell m 0 a b := by
      rw [cell_zero]
      unfold cell
      rw [hsplit_b]
      field_simp [(rising_pos _ hBk).ne', (rising_pos m hS).ne']
      exact (mul_div_cancel_right₀ _ (rising_pos k hBk).ne').symm
    rw [hrel]
    have H := mul_le_mul hratio hR (rising_pos _ ha).le (by positivity : 0 ≤ (2 : ℝ)^k)
    apply mul_le_mul_of_nonneg_right _ (cell_pos m.zero_le ha hb).le
    simpa [mul_comm a, mul_assoc] using H
  · intro hbig
    let a₀ : ℝ := (k+1 : ℕ)
    have ha₀ : 0 < a₀ := by dsimp [a₀]; positivity
    have hC₀ : (m.choose k : ℝ) * cell (m-k) 0 a b ≥ cell m k a b := by
      have hstep : rising k a ≤ rising k (a+b+((m-k : ℕ) : ℝ)) :=
        rise_mono _ ha.le (by linarith [Nat.cast_nonneg (α := ℝ) (m-k)])
      rw [cell_zero]
      unfold cell
      rw [hsplit_s]
      apply (div_le_iff₀ (mul_pos (rising_pos _ hS)
        (rising_pos _ (by positivity : 0 < a+b+((m-k : ℕ) : ℝ))))).mpr
      have he : (m.choose k : ℝ) * (rising (m-k) b / rising (m-k) (a+b)) *
            (rising (m-k) (a+b) * rising k (a+b+((m-k : ℕ) : ℝ))) =
          (m.choose k : ℝ) * (rising k (a+b+((m-k : ℕ) : ℝ)) * rising (m-k) b) := by
        field_simp [(rising_pos (m-k) hS).ne']
      rw [he]
      exact mul_le_mul_of_nonneg_left
        (mul_le_mul_of_nonneg_right hstep (rising_pos _ hb).le) (by positivity)
    have hA : a₀ ≤ a := by simpa [a₀] using hbig
    have H₀ := cell_zero_anti (m-k) ha₀ hb hA
    rw [cell_zero (m-k) a₀ b, rising_integer_swap (k+1) (m-k) hb] at H₀
    have hD := (pow_le_pow_left₀ (by positivity : 0 ≤ (m : ℝ)/2) hbk (k+1)).trans
      (rise_ge_pow (k+1) hBk.le)
    have hc : cell (m-k) 0 a b ≤ rising (k+1) b / ((m : ℝ)/2)^(k+1) :=
      H₀.trans (div_le_div_of_nonneg_left (rising_pos _ hb).le (by positivity) hD)
    calc
      cell m k a b ≤ (m.choose k : ℝ) * cell (m-k) 0 a b := hC₀
      _ ≤ (m : ℝ)^k * (rising (k+1) b / ((m : ℝ)/2)^(k+1)) :=
        mul_le_mul hC hc (cell_pos (m-k).zero_le ha hb).le (by positivity)
      _ = rising (k+1) b * (2 : ℝ)^(k+1) / (m : ℝ) := by
        rw [div_pow,pow_succ (m : ℝ)]
        field_simp [hm0.ne']

/-- At fixed positive beta, every positive fixed-count mass tends to zero
uniformly over the remaining positive shape as trial size increases. -/
theorem held_beta_uniform_positive_cell {k : ℕ} (hk : 0 < k) {b : ℝ} (hb : 0 < b)
    {ε : ℝ} (hε : 0 < ε) :
    ∀ᶠ m : ℕ in atTop, ∀ a > 0,
      (m.choose k : ℝ) * ((∏ j ∈ Finset.range k, (a+(j : ℝ))) *
        (∏ j ∈ Finset.range (m-k), (b+(j : ℝ)))) /
        (∏ j ∈ Finset.range m, (a+b+(j : ℝ))) < ε := by
  let C := (2 : ℝ)^k * rising (k-1) ((k : ℝ)+2)
  let D := rising (k+1) b * (2 : ℝ)^(k+1)
  have hC : 0 < C := mul_pos (by positivity) (rising_pos _ (by positivity))
  let δ := ε/(2*C)
  have hδ : 0 < δ := div_pos hε (by positivity)
  have hD : Tendsto (fun m : ℕ ↦ D/(m : ℝ)) atTop (𝓝 0) := by
    simpa only [Function.comp_def, div_eq_mul_inv, mul_zero] using
      ((tendsto_inv_atTop_zero (𝕜 := ℝ)).comp tendsto_natCast_atTop_atTop).const_mul D
  have hZ : Tendsto (fun m : ℕ ↦ C*((k : ℝ)+1)*cell m 0 δ b) atTop (𝓝 0) := by
    simpa only [mul_zero] using (fixed_zero_tendsto hδ hb).const_mul (C*((k : ℝ)+1))
  filter_upwards [eventually_ge_atTop (2*k), hD.eventually_lt_const hε,
    hZ.eventually_lt_const hε] with m hm hmd hmz
  intro a ha
  change cell m k a b < ε
  have H := positive_cell_bounds hk hm ha hb
  by_cases hbig : (k : ℝ)+1 ≤ a
  · exact (H.2 hbig).trans_lt hmd
  have ha' : a ≤ (k : ℝ)+1 := (le_of_not_ge hbig)
  have hcell : cell m k a b ≤ C*a*cell m 0 a b := H.1 ha'
  by_cases hs : a ≤ δ
  · have hm' : 0 < m := by omega
    have hq := (cell_lt_one hm' m.zero_le ha hb).le
    calc
      cell m k a b ≤ C*a*cell m 0 a b := hcell
      _ ≤ C*δ*1 := mul_le_mul (mul_le_mul_of_nonneg_left hs hC.le) hq
        (cell_pos m.zero_le ha hb).le (by positivity)
      _ < ε := by dsimp [δ]; field_simp; nlinarith
  · have hp := cell_zero_anti m hδ hb (le_of_not_ge hs)
    apply hcell.trans_lt _
    refine (mul_le_mul (mul_le_mul_of_nonneg_left ha' hC.le) hp
      (cell_pos m.zero_le ha hb).le (by positivity)).trans_lt hmz

private noncomputable def closedCell (m k : ℕ) (b p : ℝ) : ℝ :=
  (m.choose k : ℝ) *
    ((∏ j ∈ Finset.range k, (p+(j : ℝ)*(1-p))) * ((1-p)^(m-k)*rising (m-k) b)) /
    (∏ j ∈ Finset.range m, (p+(b+(j : ℝ))*(1-p)))

private theorem closedCell_eq {m k : ℕ} (hk : k ≤ m) {b p : ℝ}
    (hp : p ∈ Ioo 0 1) :
    closedCell m k b p = cell m k (p/(1-p)) b := by
  let s := (1-p)⁻¹
  have hs : 0 < s := inv_pos.mpr (sub_pos.mpr hp.2)
  have H : 0 < 1-p := sub_pos.mpr hp.2
  have ha : p/(1-p)/s = p := by dsimp [s]; field_simp
  have hj (j : ℕ) : (j : ℝ)/s = (j : ℝ)*(1-p) := by simp [s, div_inv_eq_mul]
  have hm : s^m = s^k*s^(m-k) := by rw [← pow_add, Nat.add_sub_of_le hk]
  unfold closedCell cell
  rw [rising_scaled hs (p/(1-p)), rising_scaled hs (p/(1-p)+b), hm]
  simp only [ha,hj]
  dsimp [s]
  simp only [inv_pow]
  field_simp
  ring

private theorem fixed_beta_slice {n m : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    (hplus : ∃ i, 0 < x i) (hunder : ∃ i, x i < m) {b : ℝ} (hb : 0 < b) :
    ∃ a ∈ Ioi (0 : ℝ), IsMaxOn (fun a ↦ ∏ i, cell m (x i) a b) (Ioi 0) a := by
  let L (p : ℝ) := ∏ i, closedCell m (x i) b p
  have hd (p : ℝ) (hp : p ∈ Icc (0 : ℝ) 1) (j : ℕ) :
      0 < p+(b+(j : ℝ))*(1-p) := by
    by_cases h : p = 0
    · subst p
      positivity
    · have H : 0 < p := lt_of_le_of_ne hp.1 (Ne.symm h)
      exact add_pos_of_pos_of_nonneg H (mul_nonneg (by positivity) (sub_nonneg.mpr hp.2))
  have hc : ContinuousOn L (Icc 0 1) := by
    apply continuousOn_finsetProd
    intro i _
    unfold closedCell
    apply ContinuousOn.div
    · fun_prop
    · fun_prop
    · exact fun p hp ↦ (Finset.prod_pos fun j _ ↦ hd p hp j).ne'
  obtain ⟨p,hp,hm⟩ := isCompact_Icc.exists_isMaxOn
    (show (Icc (0 : ℝ) 1).Nonempty from ⟨0,by norm_num⟩) hc
  have hout : L 0 = 0 ∧ L 1 = 0 := by
    obtain ⟨i,hi⟩ := hplus
    obtain ⟨j,hj⟩ := hunder
    constructor
    · apply Finset.prod_eq_zero (Finset.mem_univ i)
      have hz : (∏ j ∈ Finset.range (x i), ((0 : ℝ)+(j : ℝ)*(1-0))) = 0 :=
        Finset.prod_eq_zero (Finset.mem_range.mpr hi) (by simp)
      simp only [closedCell,hz,zero_mul,mul_zero,zero_div]
    · apply Finset.prod_eq_zero (Finset.mem_univ j)
      unfold closedCell
      rw [sub_self, zero_pow (show m-x j ≠ 0 by omega), zero_mul,mul_zero,mul_zero,zero_div]
  have hpos : 0 < L p := by
    have H : 0 < L (1/2) := by
      unfold L
      simp_rw [closedCell_eq (hx _) (by norm_num : (1/2 : ℝ) ∈ Ioo 0 1)]
      exact Finset.prod_pos fun i _ ↦ cell_pos (hx i) (by norm_num) hb
    exact H.trans_le (hm (show (1/2 : ℝ) ∈ Icc 0 1 by norm_num))
  have hp' : p ∈ Ioo (0 : ℝ) 1 := by
    refine ⟨lt_of_le_of_ne hp.1 ?_,lt_of_le_of_ne hp.2 ?_⟩
    · intro h; rw [← h,hout.1] at hpos; exact (lt_irrefl _ hpos)
    · intro h; rw [h,hout.2] at hpos; exact (lt_irrefl _ hpos)
  refine ⟨p/(1-p),div_pos hp'.1 (sub_pos.mpr hp'.2),?_⟩
  intro a ha
  have a' : 0 < a := ha
  let q := a/(1+a)
  have hq : q ∈ Ioo (0 : ℝ) 1 :=
    ⟨div_pos a' (by positivity),(div_lt_one (by positivity)).mpr (by linarith)⟩
  have hqa : q/(1-q) = a := by dsimp [q]; field_simp; ring
  have h := hm (show q ∈ Icc (0 : ℝ) 1 from ⟨hq.1.le,hq.2.le⟩)
  change L q ≤ L p at h
  change (∏ i, cell m (x i) a b) ≤ ∏ i, cell m (x i) (p/(1-p)) b
  simpa only [L, closedCell_eq (hx _) hq, closedCell_eq (hx _) hp', hqa] using h

private theorem exp_logLik_prod {n m : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    Real.exp (logLik m x a b) = ∏ i, cell m (x i) a b := by
  rw [← Real.exp_log (Finset.prod_pos fun i _ ↦ cell_pos (hx i) ha hb),
    Real.log_prod (fun i _ ↦ (cell_pos (hx i) ha hb).ne')]
  congr 1
  unfold logLik
  exact Finset.sum_congr rfl fun i _ ↦ (log_cell (hx i) ha hb).symm

private theorem held_beta_finite_profiles {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hnc : ∀ c, ¬ ∀ i, x i = c) {b : ℝ} (hb : 0 < b) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x q.2 b
    ∃ K : ℕ, ∃ a : ℕ → ℝ,
      (∀ m, (∀ i, x i ≤ m) → a m ∈ Ioi 0 ∧
        ∀ c ∈ Ioi 0, IsMaxOn (fun t ↦ logLik m x t b) (Ioi 0) c ↔ c = a m) ∧
      (∃ q ∈ D, IsMaxOn L D q) ∧
      ∀ q ∈ D, IsMaxOn L D q ↔ q.1 ≤ K ∧ q.2 = a q.1 ∧
        ∀ m ≤ K, (∀ i, x i ≤ m) → L (m,a m) ≤ L q := by
  classical
  intro D L
  have hpos : ∃ i, 0 < x i := by
    obtain ⟨i,hi⟩ := not_forall.mp (hnc 0)
    exact ⟨i,Nat.pos_of_ne_zero hi⟩
  have slice (m : ℕ) (hm : ∀ i, x i ≤ m) :
      ∃ a ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ logLik m x t b) (Ioi 0) a := by
    obtain ⟨j,hj⟩ := not_forall.mp (hnc m)
    obtain ⟨a,ha,ha'⟩ := fixed_beta_slice x hm hpos ⟨j,lt_of_le_of_ne (hm j) hj⟩ hb
    refine ⟨a,ha,fun c hc ↦ Real.exp_le_exp.mp ?_⟩
    rw [exp_logLik_prod x hm hc hb,exp_logLik_prod x hm ha hb]
    exact ha' hc
  let a (m : ℕ) : ℝ := if hm : ∀ i, x i ≤ m then (slice m hm).choose else 1
  have has {m : ℕ} (hm : ∀ i, x i ≤ m) : 0 < a m ∧
      IsMaxOn (fun t ↦ logLik m x t b) (Ioi 0) (a m) := by
    simpa only [a,dite_eq_left hm] using (show 0 < (slice m hm).choose ∧ _ from (slice m hm).choose_spec)
  have uniq {m : ℕ} (hm : ∀ i, x i ≤ m) {c : ℝ} (hc : c ∈ Ioi 0) :
      IsMaxOn (fun t ↦ logLik m x t b) (Ioi 0) c ↔ c = a m := by
    have hm' : 0 < m := by obtain ⟨i,hi⟩ := hpos; exact hi.trans_le (hm i)
    exact ⟨fun h ↦ (held_beta_size_unique hn x hm hb).2 hm' c hc (a m) (has hm).1 h
      (has hm).2, fun h ↦ h ▸ (has hm).2⟩
  let M := Finset.univ.sup x
  have hM (i : Fin n) : x i ≤ M := Finset.le_sup (Finset.mem_univ i)
  have hMpos : 0 < M := by obtain ⟨i,hi⟩ := hpos; exact hi.trans_le (hM i)
  obtain ⟨i,hi⟩ := hpos
  have H := held_beta_uniform_positive_cell hi hb (Real.exp_pos (L (M,a M)))
  obtain ⟨J,hJ⟩ := eventually_atTop.mp H
  let K := max J M
  have htail (m : ℕ) (hm : K < m) (hf : ∀ i, x i ≤ m) (c : ℝ) (hc : 0 < c) :
      L (m,c) < L (M,a M) := by
    apply Real.exp_lt_exp.mp
    change Real.exp (logLik m x c b) < _
    rw [exp_logLik_prod x hf hc hb]
    have hcell : cell m (x i) c b < Real.exp (L (M,a M)) :=
      hJ m ((le_max_left _ _).trans hm.le) c hc
    refine lt_of_le_of_lt ?_ hcell
    have hright : ∏ j ∈ Finset.univ.erase i, cell m (x j) c b ≤ 1 :=
      Finset.prod_le_one₀
        (fun j _ ↦ (cell_pos (hf j) hc hb).le)
        (fun j _ ↦ (cell_lt_one (hi.trans_le (hf i)) (hf j) hc hb).le)
    rw [← Finset.prod_erase_mul _ _ (Finset.mem_univ i)]
    exact mul_le_of_le_one_left (cell_pos (hf i) hc hb).le hright
  let S := (Finset.range (K+1)).filter (fun m ↦ ∀ i, x i ≤ m)
  have hMS : M ∈ S := by simp [S,K,hM]
  have mem {m : ℕ} : m ∈ S ↔ m ≤ K ∧ ∀ i, x i ≤ m := by simp [S]
  obtain ⟨m,hm,hbest⟩ := Finset.exists_max_image S (fun m ↦ L (m,a m)) ⟨M,hMS⟩
  have hf := (mem.mp hm).2
  have hq : (m,a m) ∈ D := ⟨hf,(has hf).1⟩
  have fitted : IsMaxOn L D (m,a m) := by
    rintro ⟨j,c⟩ ⟨hj,hc⟩
    by_cases h : j ≤ K
    · exact ((has hj).2 (show c ∈ Ioi (0 : ℝ) from hc)).trans (hbest j (mem.mpr ⟨h,hj⟩))
    · exact (htail j (lt_of_not_ge h) hj c hc).le.trans (hbest M hMS)
  refine ⟨K,a,fun m hm ↦ ⟨(has hm).1,fun _ hc ↦ uniq hm hc⟩,
    ⟨(m,a m),hq,fitted⟩,?_⟩
  rintro ⟨j,c⟩ ⟨hj,hc⟩
  constructor
  · intro hm
    have hjK : j ≤ K := by
      by_contra! h
      exact (htail j h hj c hc).not_ge (hm (a := (M,a M)) ⟨hM,(has hM).1⟩)
    have hc' : c = a j := (uniq hj (show c ∈ Ioi (0 : ℝ) from hc)).mp
      (fun t ht ↦ hm (a := (j,t)) ⟨hj,ht⟩)
    exact ⟨hjK,hc',fun k _ hk ↦ hm (a := (k,a k)) ⟨hk,(has hk).1⟩⟩
  · rintro ⟨_,hc',hW⟩ ⟨k,t⟩ ⟨hk,ht⟩
    by_cases h : k ≤ K
    · exact ((has hk).2 (show t ∈ Ioi (0 : ℝ) from ht)).trans (hW k h hk)
    · exact (htail k (lt_of_not_ge h) hk t ht).le.trans (hW M (le_max_right _ _) hM)

private theorem upper_atom_at_fixed_beta {m k : ℕ} (hk : k ≤ m) {b : ℝ} (hb : 0 < b) :
    Tendsto (fun a : ℝ ↦ cell m k a b) atTop (𝓝 (if k = m then 1 else 0)) := by
  have hS : Tendsto (fun a : ℝ ↦ a+b) atTop atTop :=
    tendsto_atTop_add_const_right _ _ tendsto_id
  have hB : Tendsto (fun a : ℝ ↦ b/(a+b)) atTop (𝓝 0) := by
    simpa only [Function.comp_def,div_eq_mul_inv,mul_zero] using
      ((tendsto_inv_atTop_zero (𝕜 := ℝ)).comp hS).const_mul b
  have hP : Tendsto (fun a : ℝ ↦ a/(a+b)) atTop (𝓝 1) := by
    have h := (tendsto_const_nhds (x := (1 : ℝ))).sub hB
    rw [sub_zero] at h
    apply h.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
    field_simp [(add_pos ha hb).ne']
    ring
  have H := fixed_size_binomial_trace
    ((eventually_gt_atTop (0 : ℝ)).mono fun _ ha ↦ ⟨ha,hb⟩) hk hS hP
  by_cases hkm : k = m
  · subst k
    simpa [cell,rising] using H
  · have hsub : m-k ≠ 0 := by omega
    simpa [cell,rising,hkm,zero_pow hsub] using H

private theorem constant_fixed_beta {n c : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∀ i, x i = c) {b : ℝ} (hb : 0 < b) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x q.2 b
    IsLUB (L '' D) 0 ∧ (∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 0) := by
  intro D L
  have hcell {m : ℕ} (hm : ∀ i, x i ≤ m) {a : ℝ} (ha : 0 < a) :
      L (m,a) ≤ 0 ∧ (L (m,a) = 0 ↔ m = 0) := by
    by_cases hm0 : m = 0
    · subst m
      have hz (i : Fin n) : x i = 0 := Nat.eq_zero_of_le_zero (hm i)
      simp [L,logLik,risingLog,hz]
    · have hpos : 0 < m := Nat.pos_of_ne_zero hm0
      have Hp : Real.exp (L (m,a)) < 1 := by
        change Real.exp (logLik m x a b) < 1
        rw [exp_logLik_prod x hm ha hb]
        have H := Finset.prod_lt_prod_of_nonempty₀ (s := (Finset.univ : Finset (Fin n)))
          (f := fun i ↦ cell m (x i) a b) (g := fun _ ↦ (1 : ℝ))
          (fun i _ ↦ cell_pos (hm i) ha hb) (fun i _ ↦ cell_lt_one hpos (hm i) ha hb)
          ⟨⟨0,hn⟩,Finset.mem_univ _⟩
        simpa only [Finset.prod_const_one] using H
      have hh := Real.exp_lt_one_iff.mp Hp
      exact ⟨hh.le,by simp [hh.ne, hm0]⟩
  have hLub : IsLUB (L '' D) 0 := by
    refine ⟨?_,?_⟩
    · rintro _ ⟨⟨m,a⟩,⟨hm,ha⟩,rfl⟩
      exact (hcell hm ha).1
    · intro z hz
      have hf (i : Fin n) : x i ≤ c := (hx i).le
      have HL : Tendsto (fun a : ℝ ↦ ∏ i, cell c (x i) a b) atTop (𝓝 1) := by
        simpa only [hx, ite_true, Finset.prod_const_one] using
          tendsto_finsetProd (Finset.univ : Finset (Fin n))
            (fun i _ ↦ upper_atom_at_fixed_beta (hf i) hb)
      have hZ : 1 ≤ Real.exp z := by
        apply le_of_tendsto HL
        filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
        rw [← exp_logLik_prod x hf ha hb]
        exact Real.exp_le_exp.mpr (hz ⟨(c,a),⟨hf,ha⟩,rfl⟩)
      exact Real.one_le_exp_iff.mp hZ
  refine ⟨hLub,?_⟩
  rintro ⟨m,a⟩ ⟨hm,ha⟩
  rw [← (hcell hm ha).2]
  constructor
  · intro h
    refine le_antisymm (hcell hm ha).1 (hLub.2 ?_)
    rintro _ ⟨q,hq,rfl⟩
    exact h hq
  · intro h q hq
    change L q ≤ L (m,a)
    rw [h]
    exact hLub.1 ⟨q,hq,rfl⟩

/-- With positive beta held, all-zero samples give the size-zero ray,
positive constants have no maximum, and other samples have a finite
size comparison with one conditional alpha winner at each compatible size. -/
theorem held_beta_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x q.2 b
    ((∀ i, x i = 0) → ∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 0) ∧
    ((∃ c > 0, ∀ i, x i = c) → ¬ ∃ q ∈ D, IsMaxOn L D q) ∧
    ((∀ c, ¬ ∀ i, x i = c) →
      ∃ K : ℕ, ∃ a : ℕ → ℝ,
        (∀ m, (∀ i, x i ≤ m) → a m ∈ Ioi 0 ∧
          ∀ c ∈ Ioi 0, IsMaxOn (fun t ↦ logLik m x t b) (Ioi 0) c ↔ c = a m) ∧
        (∃ q ∈ D, IsMaxOn L D q) ∧
        ∀ q ∈ D, IsMaxOn L D q ↔ q.1 ≤ K ∧ q.2 = a q.1 ∧
          ∀ m ≤ K, (∀ i, x i ≤ m) → L (m,a m) ≤ L q) := by
  intro D L
  refine ⟨fun hz ↦ (constant_fixed_beta hn x hz hb).2, ?_,
    fun hnc ↦ held_beta_finite_profiles hn x hnc hb⟩
  rintro ⟨c,hc,hx⟩ ⟨q,hq,hm⟩
  have he := ((constant_fixed_beta hn x hx hb).2 q hq).mp hm
  have h := hx ⟨0,hn⟩ ▸ hq.1 ⟨0,hn⟩
  omega

/-- At held positive beta, a finite maximum exists exactly for
nonconstant samples and for the all-zero sample. -/
theorem held_beta_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x q.2 b
    (∃ q ∈ D, IsMaxOn L D q) ↔ (∀ i, x i = 0) ∨ (∀ c, ¬ ∀ i, x i = c) := by
  intro D L
  have H := held_beta_classification hn x hb
  constructor
  · intro h
    by_cases hc : ∃ c, ∀ i, x i = c
    · obtain ⟨c,hx⟩ := hc
      by_cases h0 : c = 0
      · exact Or.inl (by simpa [h0] using hx)
      · exact (H.2.1 ⟨c,Nat.pos_of_ne_zero h0,hx⟩ h).elim
    · exact Or.inr (fun c hc' ↦ hc ⟨c,hc'⟩)
  · rintro (hz | hnc)
    · have hq : (0, 1) ∈ D := ⟨fun i ↦ (hz i).le,one_pos⟩
      exact ⟨_,hq,(H.1 hz _ hq).mpr rfl⟩
    · obtain ⟨_,_,_,hh,_⟩ := H.2.2 hnc
      exact hh

private noncomputable def zeroLoss (m : ℕ) (a b : ℝ) : ℝ :=
  ∑ j ∈ Finset.range m, Real.log (1+a/(b+j))

private theorem zeroLoss_terms {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (j : ℕ) :
    a/(a+b+(j : ℝ)) ≤ Real.log (1+a/(b+j)) ∧
      Real.log (1+a/(b+j)) ≤ a/(b+j) := by
  have H : 0 < b+(j : ℝ) := by positivity
  have E : 0 < 1+a/(b+(j : ℝ)) := by positivity
  have h₁ := Real.one_sub_inv_le_log_of_pos E
  have h₂ := Real.log_le_sub_one_of_pos E
  have he : 1-(1+a/(b+(j : ℝ)))⁻¹ = a/(a+b+j) := by
    have hh : 0 < a+b+(j : ℝ) := by positivity
    rw [show 1 + a/(b+(j : ℝ)) = (a+b+j)/(b+j) by field_simp; ring, inv_div]
    field_simp [hh.ne']
    ring
  exact ⟨he ▸ h₁, by linarith only [h₂]⟩

private theorem zeroLoss_bounds {m : ℕ} (hm : 0 < m) {a b : ℝ}
    (ha : 0 < a) (hb : 0 < b) :
    0 ≤ zeroLoss m a b ∧
    zeroLoss m a b ≤ (a/min b 1)*(1 + Real.log (m : ℝ)) ∧
    (a ≤ 1 → (a/(b+2))*Real.log (m : ℝ) ≤ zeroLoss m a b) := by
  unfold zeroLoss
  let H := (harmonic m : ℝ)
  have hH : H = ∑ j ∈ Finset.range m, (1 : ℝ)/((j : ℝ)+1) := by
    simp [H, harmonic, Rat.cast_sum, Rat.cast_inv, one_div]
  have hμ : 0 < min b 1 := lt_min hb one_pos
  have hUpper : (∑ j ∈ Finset.range m, Real.log (1+a/(b+j))) ≤ (a/min b 1)*H := by
    rw [hH, Finset.mul_sum]
    apply Finset.sum_le_sum
    intro j _
    refine (zeroLoss_terms ha hb j).2.trans ?_
    have hm : min b 1 * ((j : ℝ)+1) ≤ b+j := by
      have h₁ := min_le_left b 1
      have h₂ := mul_le_mul_of_nonneg_right (min_le_right b 1)
        (Nat.cast_nonneg (α := ℝ) j)
      nlinarith
    rw [mul_one_div, div_div]
    exact div_le_div_of_nonneg_left ha.le (by positivity) hm
  have hLower (ha₁ : a ≤ 1) :
      a/(b+2)*H ≤ ∑ j ∈ Finset.range m, Real.log (1+a/(b+j)) := by
    rw [hH, Finset.mul_sum]
    apply Finset.sum_le_sum
    intro j _
    rw [mul_one_div, div_div]
    apply (div_le_div_of_nonneg_left ha.le (by positivity) ?_).trans (zeroLoss_terms ha hb j).1
    nlinarith [mul_nonneg hb.le (Nat.cast_nonneg (α := ℝ) j)]
  have hU := harmonic_le_one_add_log m
  have hL : Real.log (m : ℝ) ≤ H :=
    (Real.log_le_log (Nat.cast_pos.mpr hm) (by push_cast; linarith)).trans
      (log_add_one_le_harmonic m)
  exact ⟨Finset.sum_nonneg (fun j _ ↦ (div_nonneg ha.le (by positivity)).trans (zeroLoss_terms ha hb j).1),
    hUpper.trans (mul_le_mul_of_nonneg_left hU (by positivity)),
    fun h ↦ (mul_le_mul_of_nonneg_left hL (by positivity)).trans (hLower h)⟩

private theorem loss_one_term {m : ℕ} (hm : 0 < m) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    a ≤ b*(Real.exp (zeroLoss m a b)-1) := by
  have H := Finset.single_le_sum
    (f := fun j : ℕ ↦ Real.log (1+a/(b+(j : ℝ))))
    (fun j _ ↦ (div_nonneg ha.le (by positivity)).trans (zeroLoss_terms ha hb j).1)
    (Finset.mem_range.mpr hm)
  have he := Real.exp_le_exp.mpr H
  simp only [Nat.cast_zero,add_zero] at he
  change Real.exp (Real.log (1+a/b)) ≤ Real.exp (zeroLoss m a b) at he
  rw [Real.exp_log (by positivity)] at he
  have HH := (div_le_iff₀ hb).mp (show a/b ≤ Real.exp (zeroLoss m a b)-1 by linarith)
  simpa [mul_comm b] using HH

private theorem zero_loss_log_ratio {m : ℕ} {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    Real.log (cell m 0 a b) = -zeroLoss m a b := by
  rw [cell_zero, Real.log_div (rising_pos _ hb).ne' (rising_pos _ (add_pos ha hb)).ne']
  unfold rising zeroLoss
  rw [Real.log_prod (fun (j : ℕ) _ ↦ (show 0 < b+(j : ℝ) by positivity).ne'),
    Real.log_prod (fun (j : ℕ) _ ↦ (show 0 < a+b+(j : ℝ) by positivity).ne'), ← Finset.sum_sub_distrib,
    ← Finset.sum_neg_distrib]
  apply Finset.sum_congr rfl
  intro j _
  have H : 1+a/(b+(j : ℝ)) = (a+b+j)/(b+j) := by field_simp; ring
  rw [H,Real.log_div (by positivity) (by positivity)]
  ring

/-- At growing trial sizes, the zero-cell probability with beta held
tends to one exactly when alpha times log size tends to zero. -/
theorem held_beta_growing_zero_atom_iff {ι : Type*} {l : Filter ι}
    {N : ι → ℕ} {a : ι → ℝ} {b : ℝ} (hb : 0 < b)
    (ha : ∀ᶠ t in l, 0 < a t) (hN : Tendsto N l atTop) :
    Tendsto (fun t ↦ (∏ j ∈ Finset.range (N t), (b+(j : ℝ))) /
      (∏ j ∈ Finset.range (N t), (a t+b+(j : ℝ)))) l (𝓝 1) ↔
      Tendsto (fun t ↦ a t * Real.log (N t : ℝ)) l (𝓝 0) := by
  have h0 : ∀ᶠ t in l, 0 < N t := hN.eventually_gt_atTop 0
  have hlog : Tendsto (fun t ↦ Real.log (N t : ℝ)) l atTop :=
    Real.tendsto_log_atTop.comp (tendsto_natCast_atTop_atTop.comp hN)
  have hloss : Tendsto (fun t ↦ zeroLoss (N t) (a t) b) l (𝓝 0) ↔
      Tendsto (fun t ↦ a t * Real.log (N t : ℝ)) l (𝓝 0) := by
    constructor
    · intro h
      have hE : Tendsto (fun t ↦ b*(Real.exp (zeroLoss (N t) (a t) b)-1)) l (𝓝 0) := by
        have H := (Real.continuous_exp.tendsto (0 : ℝ)).comp h
        simpa only [Function.comp_def,Real.exp_zero,sub_self,mul_zero] using (H.sub_const 1).const_mul b
      have hA : Tendsto a l (𝓝 0) :=
        tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hE
          (ha.mono fun _ h ↦ h.le)
          (by filter_upwards [ha,h0] with t ht hn; exact loss_one_term hn ht hb)
      have hu := h.const_mul (b+2)
      rw [mul_zero] at hu
      apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hu
      · filter_upwards [ha,hlog.eventually_ge_atTop 0] with t ht hl
        exact mul_nonneg ht.le hl
      · filter_upwards [ha,h0,hA.eventually_lt_const (zero_lt_one : (0 : ℝ) < 1)] with t ht hn ha'
        have H := (zeroLoss_bounds hn ht hb).2.2 ha'.le
        rw [div_mul_eq_mul_div] at H
        simpa only [mul_comm (b+2)] using (div_le_iff₀ (by positivity : 0 < b+2)).mp H
    · intro h
      have hU := h.const_mul (2/min b 1)
      rw [mul_zero] at hU
      apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hU
      · filter_upwards [ha,h0] with t ht hn
        exact (zeroLoss_bounds hn ht hb).1
      · filter_upwards [ha,h0,hlog.eventually_ge_atTop 1] with t ht hn hl
        refine (zeroLoss_bounds hn ht hb).2.1.trans ?_
        have hμ : 0 < min b 1 := lt_min hb one_pos
        rw [div_mul_eq_mul_div, div_mul_eq_mul_div]
        apply div_le_div_of_nonneg_right _ hμ.le
        nlinarith [mul_nonneg ht.le (sub_nonneg.mpr hl)]
  have hc : (fun t ↦ cell (N t) 0 (a t) b) = fun t ↦
      (∏ j ∈ Finset.range (N t), (b+(j : ℝ))) /
        (∏ j ∈ Finset.range (N t), (a t+b+(j : ℝ))) := by
    funext t
    simp [cell_zero,rising]
  rw [← hc]
  rw [← hloss]
  constructor
  · intro h
    have H := (h.log one_ne_zero).neg
    simp only [Real.log_one, neg_zero] at H
    apply H.congr'
    filter_upwards [ha] with t ht
    rw [zero_loss_log_ratio ht hb, neg_neg]
  · intro h
    have H := (Real.continuous_exp.tendsto (-(0 : ℝ))).comp h.neg
    simp only [Function.comp_def,neg_zero,Real.exp_zero] at H
    apply H.congr'
    filter_upwards [ha] with t ht
    rw [← zero_loss_log_ratio ht hb, Real.exp_log (cell_pos (Nat.zero_le _) ht hb)]

private theorem lower_atom_at_fixed_beta {m k : ℕ} (hk : k ≤ m) {b : ℝ} (hb : 0 < b) :
    Tendsto (fun a : ℝ ↦ cell m k a b) (𝓝[>] 0)
      (𝓝 (if k = 0 then 1 else 0)) := by
  have hc (j : ℕ) : Continuous (rising j) := by unfold rising; fun_prop
  have hD : ContinuousAt (fun a : ℝ ↦ rising m (a+b)) 0 :=
    ((hc m).continuousAt (x := (0 : ℝ)+b)).comp
      (f := fun a : ℝ ↦ a+b) (x := 0) (by fun_prop)
  have hC : ContinuousAt (fun a : ℝ ↦ cell m k a b) 0 := by
    unfold cell
    refine (((hc k).continuousAt.mul_const _).const_mul _).div hD ?_
    simpa using (rising_pos m hb).ne'
  have he : cell m k 0 b = if k = 0 then 1 else 0 := by
    by_cases h : k = 0
    · subst k
      simp [cell_zero,(rising_pos m hb).ne']
    · have hz : rising k 0 = 0 :=
        Finset.prod_eq_zero (Finset.mem_range.mpr (Nat.pos_of_ne_zero h)) (by simp)
      simp [cell,h,hz]
  rw [← he]
  exact hC.tendsto.mono_left nhdsWithin_le_nhds

private theorem nonconstant_compact_mass_superlevels {n : ℕ} (x : Fin n → ℕ)
    (hnc : ∀ c, ¬ ∀ i, x i = c) {b ε : ℝ} (hb : 0 < b) (hε : 0 < ε) :
    ∃ K : ℕ, ∃ δ > 0, ∃ A : ℝ, ∀ m a, (∀ i, x i ≤ m) → 0 < a →
      ε ≤ ∏ i, cell m (x i) a b → m ≤ K ∧ δ ≤ a ∧ a ≤ A := by
  classical
  obtain ⟨i,hi'⟩ := not_forall.mp (hnc 0)
  have hi : 0 < x i := Nat.pos_of_ne_zero hi'
  obtain ⟨K,hK⟩ := eventually_atTop.mp (held_beta_uniform_positive_cell hi hb hε)
  have mass_lt {m : ℕ} (hm : K ≤ m) (hmx : ∀ i, x i ≤ m) {a : ℝ} (ha : 0 < a) :
      (∏ i, cell m (x i) a b) < ε := by
    have hcell : cell m (x i) a b < ε := hK m hm a ha
    have hprod : (∏ j ∈ Finset.univ.erase i, cell m (x j) a b) ≤ 1 :=
      Finset.prod_le_one₀ (fun j _ ↦ (cell_pos (hmx j) ha hb).le)
        (fun j _ ↦ (cell_lt_one (hi.trans_le (hmx i)) (hmx j) ha hb).le)
    rw [← Finset.prod_erase_mul _ _ (Finset.mem_univ i)]
    exact (mul_le_of_le_one_left (cell_pos (hmx i) ha hb).le hprod).trans_lt hcell
  have cut (m : ℕ) : ∃ d > 0, ∃ A : ℝ, ∀ a, (∀ i, x i ≤ m) → 0 < a →
      ε ≤ ∏ i, cell m (x i) a b → d ≤ a ∧ a ≤ A := by
    by_cases hmx : ∀ i, x i ≤ m
    · obtain ⟨j,hj⟩ := not_forall.mp (hnc m)
      have hL : Tendsto (fun a : ℝ ↦ ∏ i, cell m (x i) a b) (𝓝[>] 0) (𝓝 0) := by
        have H := tendsto_finsetProd (Finset.univ : Finset (Fin n))
          (fun i _ ↦ lower_atom_at_fixed_beta (hmx i) hb)
        rwa [Finset.prod_eq_zero (Finset.mem_univ i) (by simp [hi'])] at H
      have hU : Tendsto (fun a : ℝ ↦ ∏ i, cell m (x i) a b) atTop (𝓝 0) := by
        have H := tendsto_finsetProd (Finset.univ : Finset (Fin n))
          (fun i _ ↦ upper_atom_at_fixed_beta (hmx i) hb)
        rwa [Finset.prod_eq_zero (Finset.mem_univ j) (by simp [hj])] at H
      obtain ⟨d,hd,hD⟩ := Metric.mem_nhdsWithin_iff.mp (hL.eventually_lt_const hε)
      obtain ⟨A,hA⟩ := eventually_atTop.mp (hU.eventually_lt_const hε)
      refine ⟨d,hd,A,fun a _ ha h ↦ ⟨?_,?_⟩⟩
      · by_contra! hh
        have hmem : a ∈ Metric.ball 0 d ∩ Ioi (0 : ℝ) := by
          exact ⟨by simpa [Metric.mem_ball,Real.dist_eq,abs_of_nonneg ha.le] using hh,ha⟩
        exact (not_lt_of_ge h) (hD hmem)
      · exact le_of_not_ge (fun h' ↦ (not_lt_of_ge h) (hA a h'))
    · exact ⟨1,one_pos,1,fun _ h ↦ (hmx h).elim⟩
  choose d hd A hA using cut
  let S := Finset.range (K+1)
  have hS : S.Nonempty := Finset.nonempty_range_add_one
  let δ := S.inf' hS d
  let T := S.sup' hS A
  have hδ : 0 < δ := by
    obtain ⟨j,_,he⟩ := Finset.exists_mem_eq_inf' hS d
    change 0 < S.inf' hS d
    rw [he]
    exact hd j
  refine ⟨K,δ,hδ,T,fun m a hm ha h ↦ ?_⟩
  have hK' : m ≤ K := by by_contra! hh; exact (mass_lt hh.le hm ha).not_ge h
  have hms : m ∈ S := Finset.mem_range.mpr (Nat.lt_succ_of_le hK')
  have H := hA m a hm ha h
  exact ⟨hK',(Finset.inf'_le _ hms).trans H.1,H.2.trans (Finset.le_sup' _ hms)⟩

/-- Holding beta fixed, positive-count nonconstant samples confine every
positive likelihood superlevel to finitely many sizes and to an interior
bounded interval for the free shape. -/
theorem held_beta_nonconstant_no_escape {n : ℕ} (x : Fin n → ℕ)
    (hnc : ∀ c, ¬ ∀ i, x i = c) {b ε : ℝ} (hb : 0 < b) (hε : 0 < ε) :
    ∃ K : ℕ, ∃ δ > 0, ∃ A : ℝ, ∀ m a, (∀ i, x i ≤ m) → 0 < a →
      ε ≤ Real.exp (logLik m x a b) → m ≤ K ∧ δ ≤ a ∧ a ≤ A := by
  obtain ⟨K,δ,hδ,A,H⟩ := nonconstant_compact_mass_superlevels x hnc hb hε
  refine ⟨K,δ,hδ,A,fun m a hm ha h ↦ H m a hm ha ?_⟩
  rwa [← exp_logLik_prod x hm ha hb]

/-- The held-beta failure and boundary branches. Positive constants
recover their unattained unit height at the upper atom. Nonconstant
positive superlevels stay within a compact parameter box. On all zeros,
the size-zero ray is tied by the lower atom, and at growing sizes the
zero height is approached exactly at the stated logarithmic scale. -/
theorem held_beta_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ Real.exp (logLik q.1 x q.2 b)
    (∀ c > 0, (∀ i, x i = c) → IsLUB (L '' D) 1 ∧ (∀ q ∈ D, L q < 1) ∧
      Tendsto (fun a ↦ L (c,a)) atTop (𝓝 1)) ∧
    ((∀ c, ¬ ∀ i, x i = c) → ∀ ε > 0,
      ∃ K : ℕ, ∃ δ > 0, ∃ A : ℝ, ∀ q ∈ D, ε ≤ L q →
        q.1 ≤ K ∧ δ ≤ q.2 ∧ q.2 ≤ A) ∧
    ((∀ i, x i = 0) →
      (∀ q ∈ D, L q = 1 ↔ q.1 = 0) ∧
      (∀ m > 0, Tendsto (fun a ↦ L (m,a)) (𝓝[>] 0) (𝓝 1)) ∧
      ∀ {N : ℕ → ℕ} {a : ℕ → ℝ}, (∀ᶠ j in atTop, 0 < a j) →
        Tendsto N atTop atTop →
        (Tendsto (fun j ↦ L (N j,a j)) atTop (𝓝 1) ↔
          Tendsto (fun j ↦ a j * Real.log (N j : ℝ)) atTop (𝓝 0))) := by
  intro D L
  refine ⟨?_,?_,?_⟩
  · intro c hc hx
    have hF (m : ℕ) (hm : ∀ i, x i ≤ m) {a : ℝ} (ha : 0 < a) :
        L (m,a) = ∏ i, cell m (x i) a b := exp_logLik_prod x hm ha hb
    have strict (q : ℕ × ℝ) (hq : q ∈ D) : L q < 1 := by
      have hqpos : 0 < q.1 := hc.trans_le (hx ⟨0,hn⟩ ▸ hq.1 ⟨0,hn⟩)
      rw [show L q = ∏ i, cell q.1 (x i) q.2 b from hF _ hq.1 hq.2]
      simpa only [Finset.prod_const_one] using
        Finset.prod_lt_prod_of_nonempty₀ (s := (Finset.univ : Finset (Fin n)))
          (fun i _ ↦ cell_pos (hq.1 i) hq.2 hb)
          (g := fun _ ↦ (1 : ℝ)) (fun i _ ↦ cell_lt_one hqpos (hq.1 i) hq.2 hb)
          ⟨⟨0,hn⟩,Finset.mem_univ _⟩
    have hcx (i : Fin n) : x i ≤ c := (hx i).le
    have ht : Tendsto (fun a ↦ L (c,a)) atTop (𝓝 1) := by
      have H : Tendsto (fun a : ℝ ↦ ∏ i, cell c (x i) a b) atTop (𝓝 1) := by
        simpa only [hx,ite_true,Finset.prod_const_one] using
          tendsto_finsetProd (Finset.univ : Finset (Fin n))
            (fun i _ ↦ upper_atom_at_fixed_beta (hcx i) hb)
      apply H.congr'
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
      exact (hF _ hcx ha).symm
    refine ⟨⟨?_,?_⟩,strict,ht⟩
    · rintro _ ⟨q,hq,rfl⟩
      exact (strict q hq).le
    · intro v hv
      apply le_of_tendsto ht
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
      exact hv ⟨(c,a),⟨hcx,ha⟩,rfl⟩
  · intro hnc ε hε
    obtain ⟨K,δ,hδ,A,H⟩ := held_beta_nonconstant_no_escape x hnc hb hε
    exact ⟨K,δ,hδ,A,fun q hq h ↦ H q.1 q.2 hq.1 hq.2 h⟩
  · intro hx
    have hprod (m : ℕ) {a : ℝ} (ha : 0 < a) : L (m,a) = (cell m 0 a b)^n := by
      change Real.exp (logLik m x a b) = _
      rw [exp_logLik_prod x (fun i ↦ hx i ▸ Nat.zero_le _) ha hb]
      simp [hx]
    refine ⟨?_,?_,?_⟩
    · rintro ⟨m,a⟩ hq
      rw [hprod m hq.2]
      constructor
      · intro he
        by_contra h
        have H := cell_lt_one (Nat.pos_of_ne_zero h) m.zero_le hq.2 hb
        exact (pow_lt_one₀ (cell_pos m.zero_le hq.2 hb).le H hn.ne').ne he
      · rintro (rfl : m = 0)
        simp [cell,rising]
    · intro m _
      have H := (lower_atom_at_fixed_beta (Nat.zero_le m) hb).pow n
      simp only [ite_true,one_pow] at H
      apply H.congr'
      filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with a ha
      exact (hprod m ha).symm
    · intro N a ha hN
      have h0 : ∀ᶠ j in atTop, 0 < N j := hN.eventually_gt_atTop 0
      have H : Tendsto (fun j ↦ cell (N j) 0 (a j) b) atTop (𝓝 1) ↔
          Tendsto (fun j ↦ a j * Real.log (N j : ℝ)) atTop (𝓝 0) := by
        simpa only [cell_zero,rising] using held_beta_growing_zero_atom_iff hb ha hN
      rw [← H]
      constructor
      · intro h
        apply tendsto_of_tendsto_of_tendsto_of_le_of_le' h tendsto_const_nhds
        · filter_upwards [ha,h0] with j hj hNj
          rw [hprod (N j) hj]
          obtain ⟨k,hk⟩ := Nat.exists_eq_succ_of_ne_zero hn.ne'
          rw [hk,pow_succ]
          have hP := cell_pos (Nat.zero_le (N j)) hj hb
          have hB := (cell_lt_one hNj (Nat.zero_le (N j)) hj hb).le
          exact mul_le_of_le_one_left hP.le (pow_le_one₀ hP.le hB)
        · filter_upwards [ha,h0] with j hj hNj
          exact (cell_lt_one hNj (Nat.zero_le (N j)) hj hb).le
      · intro h
        have H := h.pow n
        simp only [one_pow] at H
        apply H.congr'
        filter_upwards [ha] with j hj
        exact (hprod (N j) hj).symm

private theorem rising_gamma (n : ℕ) {u : ℝ} (hu : 0 < u) :
    Real.Gamma (u+n) = Real.Gamma u * rising n u := by
  induction n with
  | zero => simp [rising]
  | succ n ih =>
    rw [Nat.cast_succ, ← add_assoc, Real.Gamma_add_one (by positivity : u+(n : ℝ) ≠ 0),
      ih,rising_succ]
    ring

private theorem cell_zero_gamma {m : ℕ} {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    cell m 0 a b =
      (Real.Gamma (b+a)/Real.Gamma b) /
        (Real.Gamma (b+(m : ℝ)+a)/Real.Gamma (b+(m : ℝ))) := by
  rw [cell_zero]
  have ha' : 0 < a+b := add_pos ha hb
  rw [show b+a = a+b by ring, show b+(m : ℝ)+a = a+b+m by ring,
    rising_gamma m ha', rising_gamma m hb]
  field_simp [(Real.Gamma_pos_of_pos ha').ne', (Real.Gamma_pos_of_pos hb).ne',
    (rising_pos m ha').ne', (rising_pos m hb).ne']

private theorem cell_ratio_fixed_count {m k : ℕ} (hk : k ≤ m) {a b : ℝ}
    (ha : 0 < a) (hb : 0 < b) :
    cell m k a b =
      cell m 0 a b * (rising k a / k.factorial) *
        (∏ j ∈ Finset.range k, ((m : ℝ)-j)/(b+(m : ℝ)-k+j)) := by
  have he : b+(m : ℝ)-k = b+((m-k : ℕ) : ℝ) := by rw [Nat.cast_sub hk]; ring
  have hR : rising m b = rising (m-k) b * rising k (b+(m : ℝ)-k) := by
    rw [he, ← rising_append, Nat.sub_add_cancel hk]
  have hchoose : (m.choose k : ℝ) * k.factorial =
      ∏ j ∈ Finset.range k, ((m : ℝ)-j) := by
    have h := congrArg (fun N : ℕ ↦ (N : ℝ)) (Nat.descFactorial_eq_factorial_mul_choose m k)
    simp only [Nat.descFactorial_eq_prod_range,Nat.cast_mul,Nat.cast_prod] at h
    rw [mul_comm,← h]
    apply Finset.prod_congr rfl
    intro j hj
    exact Nat.cast_sub ((Finset.mem_range.mp hj).le.trans hk)
  have hD : 0 < b+(m : ℝ)-k := by rw [he]; positivity
  rw [Finset.prod_div_distrib, ← hchoose, cell_zero]
  unfold cell
  change _ = _ * (_ / _) * (_ / rising k (b+(m : ℝ)-k))
  rw [hR]
  field_simp [(rising_pos _ hD).ne', (rising_pos m (add_pos ha hb)).ne']

/-- At fixed positive alpha, changing beta and growing size give an
NB trace at every interior limiting value of beta divided by beta plus
size. The endpoints give the zero atom or no mass at finite counts. -/
theorem held_alpha_growing_trace {ι : Type*} {l : Filter ι} {N : ι → ℕ}
    {b : ι → ℝ} {a p : ℝ} (ha : 0 < a) (hp : p ∈ Icc 0 1)
    (hb : ∀ᶠ t in l, 0 < b t) (hN : Tendsto N l atTop)
    (hP : Tendsto (fun t ↦ b t/(b t + (N t : ℝ))) l (𝓝 p)) (k : ℕ) :
    Tendsto (fun t ↦
      ((N t).choose k : ℝ) * ((∏ j ∈ Finset.range k, (a+(j : ℝ))) *
        (∏ j ∈ Finset.range (N t-k), (b t+(j : ℝ)))) /
          (∏ j ∈ Finset.range (N t), (a+b t+(j : ℝ)))) l
      (𝓝 (p^a * ((∏ j ∈ Finset.range k, (a+(j : ℝ))) / k.factorial) * (1-p)^k)) := by
  let T (t : ι) := b t+(N t : ℝ)
  have hcast : Tendsto (fun t ↦ (N t : ℝ)) l atTop :=
    tendsto_natCast_atTop_atTop.comp hN
  have hT : Tendsto T l atTop := by
    apply tendsto_atTop_mono' _ ?_ hcast
    filter_upwards [hb] with t ht
    dsimp [T]
    linarith
  have hM : Tendsto (fun t ↦ (N t : ℝ)/T t) l (𝓝 (1-p)) := by
    have H := (tendsto_const_nhds (x := (1 : ℝ))).sub hP
    apply H.congr'
    filter_upwards [hb] with t ht
    have ht' : 0 < T t := by dsimp [T]; positivity
    change 1-b t/T t = (N t : ℝ)/T t
    field_simp [ht'.ne']
    dsimp [T]
    ring
  have h0 := Base8bGammaRatio.scaled_limit ha hp.1 hb hT hP
  have hZ : Tendsto (fun t ↦ cell (N t) 0 a (b t)) l (𝓝 (p^a)) := by
    apply h0.congr'
    filter_upwards [hb] with t ht
    exact (cell_zero_gamma ha ht).symm
  have hj := tendsto_inv_atTop_zero.comp hT
  have hF (j : ℕ) : Tendsto (fun t ↦ ((N t : ℝ)-(j : ℝ))/(T t-k+j)) l (𝓝 (1-p)) := by
    have hNum := hM.sub (hj.const_mul (j : ℝ))
    have hDen := (hj.const_mul ((j : ℝ)-k)).const_add (1 : ℝ)
    simp only [mul_zero,sub_zero,add_zero] at hNum hDen
    have H := hNum.div hDen one_ne_zero
    rw [div_one] at H
    apply H.congr'
    filter_upwards [hb,hN.eventually_ge_atTop k] with t ht hk
    have hT' : 0 < T t := by dsimp [T]; positivity
    have hden : 0 < T t-(k : ℝ)+(j : ℝ) := by
      have hh : (k : ℝ) ≤ N t := by exact_mod_cast hk
      dsimp [T]
      linarith [Nat.cast_nonneg (α := ℝ) j]
    change (((N t : ℝ)/T t)-(j : ℝ)*(T t)⁻¹)/(1+((j : ℝ)-k)*(T t)⁻¹) = _
    have he : 1+((j : ℝ)-k)*(T t)⁻¹ = (T t-(k : ℝ)+j)/T t := by
      field_simp [hT'.ne']
      ring
    rw [he]
    field_simp [hT'.ne',hden.ne']
  have hprod : Tendsto (fun t ↦ ∏ j ∈ Finset.range k,
      ((N t : ℝ)-(j : ℝ))/(T t-k+j)) l (𝓝 ((1-p)^k)) := by
    simpa only [Finset.prod_const, Finset.card_range] using
      tendsto_finsetProd (Finset.range k) (fun j _ ↦ hF j)
  have H := (hZ.mul_const (rising k a / k.factorial)).mul hprod
  change Tendsto (fun t ↦ cell (N t) k a (b t)) l
    (𝓝 (p^a * (rising k a / k.factorial) * (1-p)^k))
  apply H.congr'
  filter_upwards [hb,hN.eventually_ge_atTop k] with t ht hk
  exact (cell_ratio_fixed_count hk ha ht).symm

private noncomputable def alphaFaceCell (k : ℕ) (a p : ℝ) :=
  p^a * (rising k a / k.factorial) * (1-p)^k

private theorem alpha_face_log {n : ℕ} (x : Fin n → ℕ) {a p : ℝ}
    (ha : 0 < a) (hp : p ∈ Ioo 0 1) :
    Real.log (∏ i, alphaFaceCell (x i) a p) = NegBinomial.logLik x a p := by
  have hq : 0 < 1-p := sub_pos.mpr hp.2
  have hpos (k : ℕ) : 0 < alphaFaceCell k a p :=
    mul_pos (mul_pos (Real.rpow_pos_of_pos hp.1 _) (div_pos (rising_pos _ ha) (by positivity)))
      (pow_pos hq _)
  have hlog (k : ℕ) : Real.log (alphaFaceCell k a p) =
      a*Real.log p + (∑ j ∈ Finset.range k, Real.log (a+j)) -
        Real.log (k.factorial : ℝ) + (k : ℝ)*Real.log (1-p) := by
    have hD : (k.factorial : ℝ) ≠ 0 := by positivity
    have hR : 0 < rising k a := rising_pos _ ha
    have hP : 0 < p^a := Real.rpow_pos_of_pos hp.1 _
    have hW : 0 < rising k a / k.factorial := div_pos hR (by positivity)
    unfold alphaFaceCell
    rw [Real.log_mul (mul_pos hP hW).ne' (pow_pos hq k).ne',
      Real.log_mul hP.ne' hW.ne', Real.log_rpow hp.1, Real.log_div hR.ne' hD, Real.log_pow]
    unfold rising
    rw [Real.log_prod (fun (j : ℕ) _ ↦ (show 0 < a+(j : ℝ) by positivity).ne')]
    ring
  rw [Real.log_prod (fun i _ ↦ (hpos (x i)).ne')]
  simp only [hlog, Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.sum_mul]
  unfold NegBinomial.logLik NegBinomial.total NegBinomial.constant
  ring

private theorem alpha_face_optimum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {a p : ℝ} (ha : 0 < a) (hp : p ∈ Icc 0 1) :
    (∏ i, alphaFaceCell (x i) a p) ≤
      Real.exp (NegBinomial.profile x a) := by
  have H := NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) ha
  by_cases hp' : p ∈ Ioo 0 1
  · have hpos : 0 < ∏ i, alphaFaceCell (x i) a p := by
      unfold alphaFaceCell
      apply Finset.prod_pos
      intro i _
      exact mul_pos (mul_pos (Real.rpow_pos_of_pos hp'.1 _)
        (div_pos (rising_pos _ ha) (by positivity))) (pow_pos (sub_pos.mpr hp'.2) _)
    rw [← Real.exp_log hpos, alpha_face_log x ha hp']
    by_cases he : p = NegBinomial.prob x a
    · simp [NegBinomial.profile,he]
    · exact Real.exp_le_exp.mpr (H.2 p hp' he).le
  · have he : p = 0 ∨ p = 1 := by
      by_contra! h
      exact hp' ⟨lt_of_le_of_ne hp.1 h.1.symm,lt_of_le_of_ne hp.2 h.2⟩
    obtain ⟨i,hi⟩ := hx
    have hz : alphaFaceCell (x i) a p = 0 := by
      rcases he with rfl | rfl
      · simp [alphaFaceCell,Real.zero_rpow ha.ne']
      · simp [alphaFaceCell,zero_pow hi.ne']
    rw [Finset.prod_eq_zero (Finset.mem_univ i) hz]
    exact (Real.exp_pos _).le

private theorem alpha_tail_cut {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {a v : ℝ} (ha : 0 < a)
    (hv : Real.exp (NegBinomial.profile x a) < v) :
    ∃ K : ℕ, ∀ m > K, (∀ i, x i ≤ m) → ∀ b > 0,
      Real.exp (logLik m x a b) < v := by
  classical
  by_contra! h
  choose N hN hNx b hb hL using h
  have hNt : Tendsto N atTop atTop :=
    tendsto_atTop_mono (fun j ↦ (hN j).le) tendsto_id
  let P (j : ℕ) := b j/(b j+(N j : ℝ))
  have hp (j : ℕ) : P j ∈ Icc 0 1 := by
    have hS : 0 < b j+(N j : ℝ) := by have := hb j; positivity
    exact ⟨div_nonneg (hb j).le hS.le,
      (div_le_one hS).mpr (le_add_of_nonneg_right (Nat.cast_nonneg _))⟩
  obtain ⟨p,hp',φ,hφ,hP⟩ := isCompact_Icc.tendsto_subseq hp
  have hN' := hNt.comp hφ.tendsto_atTop
  have hB : ∀ᶠ j in atTop, 0 < b (φ j) := Eventually.of_forall fun j ↦ hb (φ j)
  have H (k : ℕ) : Tendsto (fun j ↦ cell (N (φ j)) k a (b (φ j))) atTop
      (𝓝 (alphaFaceCell k a p)) :=
    held_alpha_growing_trace ha hp' hB hN' hP k
  have H' := tendsto_finsetProd (Finset.univ : Finset (Fin n)) (fun i _ ↦ H (x i))
  have hV : v ≤ ∏ i, alphaFaceCell (x i) a p := by
    apply ge_of_tendsto H'
    exact Eventually.of_forall fun j ↦ by
      rw [← exp_logLik_prod x (hNx (φ j)) ha (hb (φ j))]
      exact hL (φ j)
  exact hv.not_ge (hV.trans (alpha_face_optimum hn x hx ha hp'))

private theorem alpha_face_reachable {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    let p := NegBinomial.prob x a
    let b (m : ℕ) := p/(1-p) * (m : ℝ)
    Tendsto (fun m : ℕ ↦ Real.exp (logLik m x a (b m))) atTop
      (𝓝 (Real.exp (NegBinomial.profile x a))) := by
  intro p b
  have hp := (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) ha).1
  have hq := sub_pos.mpr hp.2
  have hb : ∀ᶠ m : ℕ in atTop, 0 < b m := by
    filter_upwards [eventually_gt_atTop (0 : ℕ)] with m hm
    exact mul_pos (div_pos hp.1 hq) (Nat.cast_pos.mpr hm)
  have hP : Tendsto (fun m : ℕ ↦ b m/(b m+(m : ℝ))) atTop (𝓝 p) := by
    apply tendsto_const_nhds.congr'
    filter_upwards [eventually_gt_atTop (0 : ℕ)] with m hm
    have hm' : 0 < (m : ℝ) := Nat.cast_pos.mpr hm
    have hn : 1-p ≠ 0 := hq.ne'
    have he : p/(1-p)*(m : ℝ)+(m : ℝ) = (m : ℝ)/(1-p) := by field_simp; ring
    change p = (p/(1-p)*(m : ℝ))/(p/(1-p)*(m : ℝ)+(m : ℝ))
    rw [he]
    field_simp [hn,hm'.ne']
  have H (k : ℕ) :
      Tendsto (fun m : ℕ ↦ cell m k a (b m)) atTop (𝓝 (alphaFaceCell k a p)) :=
    held_alpha_growing_trace ha ⟨hp.1.le,hp.2.le⟩ hb tendsto_id hP k
  have H' := tendsto_finsetProd (Finset.univ : Finset (Fin n)) (fun i _ ↦ H (x i))
  have hpos : 0 < ∏ i, alphaFaceCell (x i) a p := by
    unfold alphaFaceCell
    exact Finset.prod_pos fun i _ ↦
      mul_pos (mul_pos (Real.rpow_pos_of_pos hp.1 _) (div_pos (rising_pos _ ha) (by positivity)))
        (pow_pos hq _)
  have he : (∏ i, alphaFaceCell (x i) a p) = Real.exp (NegBinomial.profile x a) := by
    rw [← Real.exp_log hpos,alpha_face_log x ha hp]
    rfl
  rw [he] at H'
  apply H'.congr'
  filter_upwards [hb,eventually_ge_atTop (Finset.univ.sup x)] with m hm hM
  have hmx (i : Fin n) : x i ≤ m := (Finset.le_sup (f := x) (Finset.mem_univ i)).trans hM
  exact (exp_logLik_prod x hmx ha hm).symm

private theorem fixed_alpha_slice_of_nonconstant {n m : ℕ} (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) (hnc : ∀ c, ¬ ∀ i, x i = c) {a : ℝ} (ha : 0 < a) :
    ∃ b ∈ Ioi (0 : ℝ), IsMaxOn (fun b ↦ logLik m x a b) (Ioi 0) b := by
  let y (i : Fin n) := m-x i
  have hy (i : Fin n) : y i ≤ m := Nat.sub_le _ _
  have hplus : ∃ i, 0 < y i := by
    obtain ⟨i,hi⟩ := not_forall.mp (hnc m)
    exact ⟨i,Nat.sub_pos_of_lt (lt_of_le_of_ne (hx i) hi)⟩
  have hunder : ∃ i, y i < m := by
    obtain ⟨i,hi⟩ := not_forall.mp (hnc 0)
    have h := hx i
    exact ⟨i,by dsimp [y]; omega⟩
  obtain ⟨b,hb,hmax⟩ := fixed_beta_slice y hy hplus hunder ha
  refine ⟨b,hb,fun c hc ↦ ?_⟩
  change logLik m x a c ≤ logLik m x a b
  rw [← reflected_logLik x hx a c,← reflected_logLik x hx a b]
  apply Real.exp_le_exp.mp
  rw [exp_logLik_prod y hy hc ha,exp_logLik_prod y hy hb ha]
  exact hmax hc

private theorem alpha_nonconstant_face_comparison {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hnc : ∀ c, ¬ ∀ i, x i = c) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x a q.2
    (∃ q ∈ D, IsMaxOn L D q) ↔
      ∃ q ∈ D, IsMaxOn (logLik q.1 x a) (Ioi 0) q.2 ∧ NegBinomial.profile x a ≤ L q := by
  classical
  intro D L
  have hx : ∃ i, 0 < x i := by
    obtain ⟨i,hi⟩ := not_forall.mp (hnc 0)
    exact ⟨i,Nat.pos_of_ne_zero hi⟩
  constructor
  · rintro ⟨q,hq,hm⟩
    refine ⟨q,hq,fun b hb ↦ hm (a := (q.1,b)) ⟨hq.1,hb⟩,?_⟩
    apply Real.exp_le_exp.mp
    let p := NegBinomial.prob x a
    let b (m : ℕ) := p/(1-p)*(m : ℝ)
    have hp := (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) ha).1
    have ht : Tendsto (fun m ↦ Real.exp (L (m,b m))) atTop
        (𝓝 (Real.exp (NegBinomial.profile x a))) :=
      alpha_face_reachable hn x hx ha
    apply le_of_tendsto ht
    filter_upwards [eventually_gt_atTop (0 : ℕ),
      eventually_ge_atTop (Finset.univ.sup x)] with m hm' hM
    apply Real.exp_le_exp.mpr
    exact hm (a := (m,b m))
      ⟨fun i ↦ (Finset.le_sup (f := x) (Finset.mem_univ i)).trans hM,
        mul_pos (div_pos hp.1 (sub_pos.mpr hp.2)) (Nat.cast_pos.mpr hm')⟩
  · rintro ⟨q,hq,_,hF⟩
    by_cases H : ∃ w ∈ D, NegBinomial.profile x a < L w
    · obtain ⟨w,hw,hwF⟩ := H
      obtain ⟨K,hK⟩ := alpha_tail_cut hn x hx ha (Real.exp_lt_exp.mpr hwF)
      let S := (Finset.range (K+1)).filter (fun m ↦ ∀ i, x i ≤ m)
      have mem {m : ℕ} : m ∈ S ↔ m ≤ K ∧ ∀ i, x i ≤ m := by simp [S]
      have hwK : w.1 ≤ K := by
        by_contra! h
        exact (lt_irrefl _) (hK _ h hw.1 _ hw.2)
      let b (m : ℕ) : ℝ := if h : ∀ i, x i ≤ m then
        (fixed_alpha_slice_of_nonconstant x h hnc ha).choose else 1
      have hs {m : ℕ} (h : ∀ i, x i ≤ m) : b m ∈ Ioi 0 ∧
          IsMaxOn (logLik m x a) (Ioi 0) (b m) := by
        simpa only [b,dite_eq_left h] using
          (fixed_alpha_slice_of_nonconstant x h hnc ha).choose_spec
      have hwS : w.1 ∈ S := mem.mpr ⟨hwK,hw.1⟩
      obtain ⟨m,hm,hMax⟩ := Finset.exists_max_image S (fun m ↦ L (m,b m)) ⟨_,hwS⟩
      have hr := (mem.mp hm).2
      refine ⟨(m,b m),⟨hr,(hs hr).1⟩,?_⟩
      rintro ⟨j,c⟩ ⟨hj,hc⟩
      by_cases h : j ≤ K
      · exact ((hs hj).2 (show c ∈ Ioi (0 : ℝ) from hc)).trans (hMax j (mem.mpr ⟨h,hj⟩))
      · have hh := Real.exp_lt_exp.mp (hK j (lt_of_not_ge h) hj c hc)
        exact hh.le.trans (((hs hw.1).2 hw.2).trans (hMax _ hwS))
    · refine ⟨q,hq,fun w hw ↦ ?_⟩
      have H' : L w ≤ NegBinomial.profile x a := le_of_not_gt (fun h ↦ H ⟨w,hw,h⟩)
      exact H'.trans hF

/-- With alpha held positive, the nonconstant problem attains exactly
when one of the fixed-size conditional optima reaches the optimized
fixed-alpha negative-binomial face. Equality is included. -/
theorem held_alpha_nonconstant_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hnc : ∀ c, ¬ ∀ i, x i = c) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x a q.2
    (∃ q ∈ D, IsMaxOn L D q) ↔
      ∃ q ∈ D, IsMaxOn (logLik q.1 x a) (Ioi 0) q.2 ∧ NegBinomial.profile x a ≤ L q :=
  alpha_nonconstant_face_comparison hn x hnc ha

private theorem constant_alpha_atom {n c : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i = c)
    {a : ℝ} (ha : 0 < a) :
    Tendsto (fun b : ℝ ↦ Real.exp (logLik c x a b)) (𝓝[>] 0) (𝓝 1) := by
  have H : Tendsto (fun b : ℝ ↦ cell c c a b) (𝓝[>] 0) (𝓝 1) := by
    have H' := lower_atom_at_fixed_beta (Nat.zero_le c) ha
    simp only [ite_true] at H'
    apply H'.congr
    intro b
    simp [cell,rising,add_comm a b]
  have H' : Tendsto (fun b : ℝ ↦ ∏ i, cell c (x i) a b) (𝓝[>] 0) (𝓝 1) := by
    simpa only [hx, Finset.prod_const_one] using
      tendsto_finsetProd (Finset.univ : Finset (Fin n)) (fun _ _ ↦ H)
  apply H'.congr'
  filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with b hb
  exact (exp_logLik_prod x (fun i ↦ (hx i).le) ha hb).symm

/-- With positive alpha held, zeros attain at size zero and positive
constants have only the excluded upper atom. On nonconstant samples
a finite maximizer exists exactly when a conditional beta optimum
meets the optimized negative-binomial face. Each conditional optimum
is unique. -/
theorem held_alpha_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x a q.2
    ((∃ q ∈ D, IsMaxOn L D q) ↔
      (∀ i, x i = 0) ∨
      ((∀ c, ¬ ∀ i, x i = c) ∧ ∃ q ∈ D,
        IsMaxOn (logLik q.1 x a) (Ioi 0) q.2 ∧ NegBinomial.profile x a ≤ L q)) ∧
    ((∀ c, ¬ ∀ i, x i = c) → ∀ m, (∀ i, x i ≤ m) →
      ∃! b, b ∈ Ioi 0 ∧ IsMaxOn (logLik m x a) (Ioi 0) b) := by
  intro D L
  refine ⟨?_,?_⟩
  · constructor
    · rintro ⟨q,hq,hm⟩
      by_cases hconst : ∃ c, ∀ i, x i = c
      · obtain ⟨c,hc⟩ := hconst
        by_cases hc0 : c = 0
        · exact Or.inl (fun i ↦ (hc i).trans hc0)
        · have hc' : 0 < c := Nat.pos_of_ne_zero hc0
          have hq' : 0 < q.1 := hc'.trans_le (hc ⟨0,hn⟩ ▸ hq.1 ⟨0,hn⟩)
          have strict : Real.exp (L q) < 1 := by
            change Real.exp (logLik q.1 x a q.2) < 1
            rw [exp_logLik_prod x hq.1 ha hq.2]
            simpa only [Finset.prod_const_one] using
              Finset.prod_lt_prod_of_nonempty₀ (s := (Finset.univ : Finset (Fin n)))
                (fun i _ ↦ cell_pos (hq.1 i) ha hq.2)
                (g := fun _ ↦ (1 : ℝ)) (fun i _ ↦ cell_lt_one hq' (hq.1 i) ha hq.2)
                ⟨⟨0,hn⟩,Finset.mem_univ _⟩
          exfalso
          apply strict.not_ge
          apply le_of_tendsto (constant_alpha_atom x hc ha)
          filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with b hb
          exact Real.exp_le_exp.mpr (hm (a := (c,b)) ⟨fun i ↦ (hc i).le,hb⟩)
      · have hnc : ∀ c, ¬ ∀ i, x i = c := fun c hc ↦ hconst ⟨c,hc⟩
        exact Or.inr ⟨hnc,(alpha_nonconstant_face_comparison hn x hnc ha).mp ⟨q,hq,hm⟩⟩
    · rintro (hz | ⟨hnc,h⟩)
      · have H := (all_free_constant hn x hz).2
        have hq : (0,1) ∈ D := ⟨fun i ↦ (hz i).le,one_pos⟩
        refine ⟨(0,1),hq,fun q hq' ↦ ?_⟩
        have h' := (H (0,a,1) ⟨hq.1,ha,hq.2⟩).mpr rfl
        exact h' (a := (q.1,a,q.2)) ⟨hq'.1,ha,hq'.2⟩
      · exact (alpha_nonconstant_face_comparison hn x hnc ha).mpr h
  · intro hnc m hm
    obtain ⟨b,hb,hwin⟩ := fixed_alpha_slice_of_nonconstant x hm hnc ha
    have hm' : 0 < m := by
      by_contra! h
      exact hnc 0 (fun i ↦ by have := hm i; omega)
    refine ⟨b,⟨hb,hwin⟩,fun c hc ↦ ?_⟩
    exact (held_alpha_size_unique hn x hm ha).2 hm' c hc.1 b hb hc.2 hwin

private theorem bounded_alpha_superlevel {n K : ℕ} (x : Fin n → ℕ)
    (hnc : ∀ c, ¬ ∀ i, x i = c) {a e : ℝ} (ha : 0 < a) (he : 0 < e) :
    ∃ δ > 0, ∃ A : ℝ, ∀ m ≤ K, (∀ i, x i ≤ m) → ∀ b > 0,
      e ≤ Real.exp (logLik m x a b) → b ∈ Icc δ A := by
  classical
  have f (m : ℕ) : ∃ δ > 0, ∃ A : ℝ, (∀ i, x i ≤ m) → ∀ b > 0,
      e ≤ Real.exp (logLik m x a b) → b ∈ Icc δ A := by
    by_cases hm : ∀ i, x i ≤ m
    · let y (i : Fin n) := m-x i
      have hy (i : Fin n) : y i ≤ m := Nat.sub_le _ _
      have hY : ∀ c, ¬ ∀ i, y i = c := by
        intro c hc
        apply hnc (m-c)
        intro i
        have he := hc i
        dsimp [y] at he
        have := hm i
        omega
      obtain ⟨_,δ,hδ,A,H⟩ := held_beta_nonconstant_no_escape y hY ha he
      refine ⟨δ,hδ,A,fun _ b hb hL ↦ (H m b hy hb ?_).2⟩
      rwa [reflected_logLik x hm a b]
    · exact ⟨1,one_pos,1,fun h ↦ (hm h).elim⟩
  choose d hd A hA using f
  let S := Finset.range (K+1)
  have hS : S.Nonempty := Finset.nonempty_range_add_one
  let δ := S.inf' hS d
  let T := S.sup' hS A
  have hδ : 0 < δ := by
    obtain ⟨j,_,hj⟩ := Finset.exists_mem_eq_inf' hS d
    change 0 < S.inf' hS d
    rw [hj]
    exact hd j
  refine ⟨δ,hδ,T,fun m hm hmx b hb h ↦ ?_⟩
  have hmS : m ∈ S := Finset.mem_range.mpr (Nat.lt_succ_of_le hm)
  have H := hA m hmx b hb h
  exact ⟨(Finset.inf'_le _ hmS).trans H.1,H.2.trans (Finset.le_sup' _ hmS)⟩

/-- For held alpha and nonconstant data, a maximizing sequence that
leaves every bounded interior parameter box grows in trial size and
converges to the fitted fixed-alpha negative-binomial face, including
the case where that face ties a finite maximum. -/
theorem held_alpha_nonconstant_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hnc : ∀ c, ¬ ∀ i, x i = c) {a : ℝ} (ha : 0 < a)
    {N : ℕ → ℕ} {b : ℕ → ℝ} (hNx : ∀ j i, x i ≤ N j) (hb : ∀ j, 0 < b j)
    (he : ∀ K : ℕ, ∀ δ A : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (N j ≤ K ∧ b j ∈ Icc δ A))
    {H : ℝ} (hH : ∀ m, (∀ i, x i ≤ m) → ∀ c > 0, Real.exp (logLik m x a c) ≤ H)
    (hlim : Tendsto (fun j ↦ Real.exp (logLik (N j) x a (b j))) atTop (𝓝 H)) :
    H = Real.exp (NegBinomial.profile x a) ∧ Tendsto N atTop atTop ∧
      Tendsto (fun j ↦ b j/(b j+(N j : ℝ))) atTop (𝓝 (NegBinomial.prob x a)) := by
  have hx : ∃ i, 0 < x i := by
    obtain ⟨i,hi⟩ := not_forall.mp (hnc 0)
    exact ⟨i,Nat.pos_of_ne_zero hi⟩
  let p := NegBinomial.prob x a
  have hp := (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) ha).1
  have hFace : Real.exp (NegBinomial.profile x a) ≤ H := by
    have H' := alpha_face_reachable hn x hx ha
    dsimp only at H'
    apply le_of_tendsto H'
    filter_upwards [eventually_gt_atTop (0 : ℕ),
      eventually_ge_atTop (Finset.univ.sup x)] with m hm hM
    exact hH m (fun i ↦ (Finset.le_sup (f := x) (Finset.mem_univ i)).trans hM) _
      (mul_pos (div_pos hp.1 (sub_pos.mpr hp.2)) (Nat.cast_pos.mpr hm))
  have hHpos : 0 < H := (Real.exp_pos _).trans_le hFace
  have hN : Tendsto N atTop atTop := by
    apply tendsto_atTop.mpr
    intro K
    obtain ⟨δ,hδ,A,hS⟩ := bounded_alpha_superlevel (K := K) x hnc ha (half_pos hHpos)
    have hlevel := hlim.eventually_const_lt (half_lt_self hHpos)
    filter_upwards [he K δ A hδ, hlevel] with j hj hl
    by_contra! hh
    have hjK : N j ≤ K := hh.le
    exact hj ⟨hjK,hS _ hjK (hNx j) _ (hb j) hl.le⟩
  let P (j : ℕ) := b j/(b j+(N j : ℝ))
  have hP (j : ℕ) : P j ∈ Icc 0 1 :=
    ⟨div_nonneg (hb j).le (add_nonneg (hb j).le (Nat.cast_nonneg _)),
      (div_le_one (by have := hb j; positivity)).mpr
        (le_add_of_nonneg_right (Nat.cast_nonneg _))⟩
  have identify {φ : ℕ → ℕ} (hφ : StrictMono φ) {q : ℝ} (hq : q ∈ Icc 0 1)
      (hq' : Tendsto (fun j ↦ P (φ j)) atTop (𝓝 q)) :
      H = Real.exp (NegBinomial.profile x a) ∧ q = p := by
    have hB : ∀ᶠ j in atTop, 0 < b (φ j) := Eventually.of_forall fun j ↦ hb (φ j)
    have H₁ (k : ℕ) : Tendsto (fun j ↦ cell (N (φ j)) k a (b (φ j))) atTop
        (𝓝 (alphaFaceCell k a q)) :=
      held_alpha_growing_trace ha hq hB (hN.comp hφ.tendsto_atTop) hq' k
    have H₂ := tendsto_finsetProd (Finset.univ : Finset (Fin n)) (fun i _ ↦ H₁ (x i))
    have hh : H = ∏ i, alphaFaceCell (x i) a q := by
      apply tendsto_nhds_unique (hlim.comp hφ.tendsto_atTop)
      apply H₂.congr
      intro j
      exact (exp_logLik_prod x (hNx (φ j)) ha (hb (φ j))).symm
    have hH' : H = Real.exp (NegBinomial.profile x a) :=
      le_antisymm (hh.trans_le (alpha_face_optimum hn x hx ha hq)) hFace
    have hqI : q ∈ Ioo 0 1 := by
      obtain ⟨i,hi⟩ := hx
      refine ⟨lt_of_le_of_ne hq.1 ?_,lt_of_le_of_ne hq.2 ?_⟩
      · intro e
        have hz : alphaFaceCell (x i) a q = 0 := by simp [← e,alphaFaceCell,Real.zero_rpow ha.ne']
        exact hHpos.ne' (hh.trans (Finset.prod_eq_zero (Finset.mem_univ i) hz))
      · intro e
        have hz : alphaFaceCell (x i) a q = 0 := by simp [e,alphaFaceCell,zero_pow hi.ne']
        exact hHpos.ne' (hh.trans (Finset.prod_eq_zero (Finset.mem_univ i) hz))
    refine ⟨hH',?_⟩
    by_contra h
    have H' := (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) ha).2 q hqI h
    rw [← alpha_face_log x ha hqI, ← hh, hH', Real.log_exp] at H'
    exact (lt_irrefl _) H'
  obtain ⟨q,hq,φ,hφ,hq'⟩ := isCompact_Icc.tendsto_subseq hP
  refine ⟨(identify hφ hq hq').1,hN,?_⟩
  rw [Metric.tendsto_nhds]
  intro ε hε
  by_contra h
  obtain ⟨ψ,hψ,hbad⟩ := extraction_of_frequently_atTop
    (show ∃ᶠ j in atTop, ¬dist (P j) p < ε by simpa only [Filter.Frequently,not_not] using h)
  obtain ⟨q',hq',χ,hχ,hl⟩ := isCompact_Icc.tendsto_subseq
    (x := fun j ↦ P (ψ j)) (fun j ↦ hP (ψ j))
  have heq := (identify (hψ.comp hχ) hq' hl).2
  rw [heq] at hl
  obtain ⟨j,hj⟩ := ((Metric.tendsto_nhds.mp hl) ε hε).exists
  exact hbad (χ j) hj

private theorem alpha_growing_zero_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) {a : ℝ} (ha : 0 < a) {N : ℕ → ℕ} {b : ℕ → ℝ}
    (hb : ∀ j, 0 < b j) (hN : Tendsto N atTop atTop) :
    Tendsto (fun j ↦ Real.exp (logLik (N j) x a (b j))) atTop (𝓝 1) ↔
      Tendsto (fun j ↦ b j/(N j : ℝ)) atTop atTop := by
  let P (j : ℕ) := b j/(b j+(N j : ℝ))
  let R (j : ℕ) := b j/(N j : ℝ)
  have hPmem (j : ℕ) : P j ∈ Icc 0 1 :=
    ⟨div_nonneg (hb j).le (add_nonneg (hb j).le (Nat.cast_nonneg _)),
      (div_le_one (add_pos_of_pos_of_nonneg (hb j) (Nat.cast_nonneg _))).mpr
        (le_add_of_nonneg_right (Nat.cast_nonneg _))⟩
  have p_to_ratio : Tendsto P atTop (𝓝 1) ↔ Tendsto R atTop atTop := by
    constructor
    · intro h
      have hZ : Tendsto (fun j ↦ 1-P j) atTop (𝓝[>] (0 : ℝ)) := by
        refine tendsto_nhdsWithin_iff.mpr ⟨?_,?_⟩
        · simpa only [sub_self] using (tendsto_const_nhds (x := (1 : ℝ))).sub h
        · filter_upwards [hN.eventually_gt_atTop 0] with j hj
          exact sub_pos.mpr ((div_lt_one (add_pos (hb j) (Nat.cast_pos.mpr hj))).mpr
            (lt_add_of_pos_right _ (Nat.cast_pos.mpr hj)))
      have hR := tendsto_atTop_add_const_right atTop (-1 : ℝ) hZ.inv_tendsto_nhdsGT_zero
      apply hR.congr'
      filter_upwards [hN.eventually_gt_atTop 0] with j hj
      have hN' : 0 < (N j : ℝ) := Nat.cast_pos.mpr hj
      have hD := (add_pos (hb j) hN').ne'
      change (1-P j)⁻¹ + (-1) = R j
      have he : 1-P j = (N j : ℝ)/(b j+N j) := by dsimp [P]; field_simp [hD]; ring
      rw [he,inv_div]
      dsimp [R]
      field_simp [hN'.ne']
      ring
    · intro h
      have hR := tendsto_inv_atTop_zero.comp h
      have hZ := ((tendsto_const_nhds (x := (1 : ℝ))).add hR).inv₀ (by simp)
      simp only [add_zero,inv_one] at hZ
      apply hZ.congr'
      filter_upwards [hN.eventually_gt_atTop 0] with j hj
      change (1+(R j)⁻¹)⁻¹ = P j
      dsimp [P,R]
      rw [inv_div, show 1+(N j : ℝ)/b j = (b j+(N j : ℝ))/b j by
        field_simp [(hb j).ne'], inv_div]
  rw [← p_to_ratio]
  have hL (j : ℕ) : Real.exp (logLik (N j) x a (b j)) = (cell (N j) 0 a (b j))^n := by
    rw [exp_logLik_prod x (fun i ↦ hx i ▸ Nat.zero_le _) ha (hb j)]
    simp [hx]
  have lim (φ : ℕ → ℕ) (hφ : StrictMono φ) {q : ℝ} (hq : q ∈ Icc 0 1)
      (hp : Tendsto (fun j ↦ P (φ j)) atTop (𝓝 q)) :
      Tendsto (fun j ↦ Real.exp (logLik (N (φ j)) x a (b (φ j)))) atTop
        (𝓝 ((q^a)^n)) := by
    have hB : ∀ᶠ j in atTop, 0 < b (φ j) := Eventually.of_forall fun j ↦ hb (φ j)
    have h := held_alpha_growing_trace ha hq hB (hN.comp hφ.tendsto_atTop) hp 0
    have h0 : Tendsto (fun j ↦ cell (N (φ j)) 0 a (b (φ j))) atTop (𝓝 (q^a)) := by
      simpa [cell,rising] using h
    exact (h0.pow n).congr (fun j ↦ (hL (φ j)).symm)
  constructor
  · intro h
    rw [Metric.tendsto_nhds]
    intro ε hε
    by_contra hh
    obtain ⟨φ,hφ,hout⟩ := extraction_of_frequently_atTop
      (show ∃ᶠ j in atTop, ¬dist (P j) 1 < ε by simpa only [Filter.Frequently,not_not] using hh)
    obtain ⟨q,hq,ψ,hψ,hq'⟩ := isCompact_Icc.tendsto_subseq (x := fun j ↦ P (φ j))
      (fun j ↦ hPmem (φ j))
    have H := lim (φ ∘ ψ) (hφ.comp hψ) hq hq'
    have he : (q^a)^n = 1 := tendsto_nhds_unique H (h.comp (hφ.comp hψ).tendsto_atTop)
    have hq1 : q = 1 := by
      by_contra hne
      have hh := (Real.rpow_lt_one_iff' hq.1 ha).mpr (lt_of_le_of_ne hq.2 hne)
      exact (pow_lt_one₀ (Real.rpow_nonneg hq.1 _) hh hn.ne').ne he
    rw [hq1] at hq'
    obtain ⟨j,hj⟩ := ((Metric.tendsto_nhds.mp hq') ε hε).exists
    exact hout (ψ j) hj
  · intro h
    simpa only [Function.comp_def, id_def, Real.one_rpow,one_pow] using
      lim id strictMono_id ⟨zero_le_one,le_rfl⟩ h

/-- The held-alpha escape classification joins positive constants, nonconstant
maximizing sequences, and the lower-atom escapes on all-zero data. -/
theorem held_alpha_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ Real.exp (logLik q.1 x a q.2)
    (∀ c > 0, (∀ i, x i = c) → IsLUB (L '' D) 1 ∧ (∀ q ∈ D, L q < 1) ∧
      Tendsto (fun b ↦ L (c,b)) (𝓝[>] 0) (𝓝 1)) ∧
    ((∀ c, ¬ ∀ i, x i = c) → ∀ {N : ℕ → ℕ} {b : ℕ → ℝ},
      (∀ j, (N j,b j) ∈ D) →
      (∀ K : ℕ, ∀ δ A : ℝ, 0 < δ →
        ∀ᶠ j in atTop, ¬ (N j ≤ K ∧ b j ∈ Icc δ A)) →
      ∀ {H : ℝ}, (∀ q ∈ D, L q ≤ H) →
        Tendsto (fun j ↦ L (N j,b j)) atTop (𝓝 H) →
        H = Real.exp (NegBinomial.profile x a) ∧ Tendsto N atTop atTop ∧
          Tendsto (fun j ↦ b j/(b j+(N j : ℝ))) atTop (𝓝 (NegBinomial.prob x a))) ∧
    ((∀ i, x i = 0) →
      (∀ q ∈ D, L q = 1 ↔ q.1 = 0) ∧
      (∀ m > 0, Tendsto (fun b ↦ L (m,b)) atTop (𝓝 1)) ∧
      ∀ {N : ℕ → ℕ} {b : ℕ → ℝ}, (∀ j, 0 < b j) → Tendsto N atTop atTop →
        (Tendsto (fun j ↦ L (N j,b j)) atTop (𝓝 1) ↔
          Tendsto (fun j ↦ b j/(N j : ℝ)) atTop atTop)) := by
  intro D L
  have strict (q : ℕ × ℝ) (hq : q ∈ D) (hm : 0 < q.1) : L q < 1 := by
    change Real.exp (logLik q.1 x a q.2) < 1
    rw [exp_logLik_prod x hq.1 ha hq.2]
    simpa only [Finset.prod_const_one] using
      Finset.prod_lt_prod_of_nonempty₀ (s := (Finset.univ : Finset (Fin n)))
        (fun i _ ↦ cell_pos (hq.1 i) ha hq.2) (g := fun _ ↦ (1 : ℝ))
        (fun i _ ↦ cell_lt_one hm (hq.1 i) ha hq.2) ⟨⟨0,hn⟩,Finset.mem_univ _⟩
  refine ⟨?_,?_,?_⟩
  · intro c hc hx
    have H (q : ℕ × ℝ) (hq : q ∈ D) : L q < 1 :=
      strict q hq (hc.trans_le (hx ⟨0,hn⟩ ▸ hq.1 ⟨0,hn⟩))
    have ht := constant_alpha_atom x hx ha
    refine ⟨⟨?_,?_⟩,H,ht⟩
    · rintro _ ⟨q,hq,rfl⟩
      exact (H q hq).le
    · intro v hv
      apply le_of_tendsto ht
      filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with b hb
      exact hv ⟨(c,b),⟨fun i ↦ (hx i).le,hb⟩,rfl⟩
  · intro hnc N b hD he H hH hl
    exact held_alpha_nonconstant_maximizing_escape hn x hnc ha
      (fun j ↦ (hD j).1) (fun j ↦ (hD j).2) he (fun m hm c hc ↦ hH (m,c) ⟨hm,hc⟩) hl
  · intro hx
    refine ⟨?_,?_,fun hb hN ↦ alpha_growing_zero_iff hn x hx ha hb hN⟩
    · rintro ⟨m,b⟩ hq
      cases m with
      | zero => simp [L,logLik,risingLog,hx]
      | succ m =>
        exact iff_of_false (strict (m+1,b) hq (Nat.succ_pos m)).ne (Nat.succ_ne_zero m)
    · intro m hm
      have H : Tendsto (fun b : ℝ ↦ cell m 0 a b) atTop (𝓝 1) := by
        have H' := upper_atom_at_fixed_beta (m := m) (k := m) le_rfl ha
        simp only [ite_true] at H'
        apply H'.congr
        intro b
        simp [cell,rising,add_comm a b]
      have H' : Tendsto (fun b : ℝ ↦ ∏ i, cell m (x i) a b) atTop (𝓝 1) := by
        simpa only [hx,Finset.prod_const_one] using
          tendsto_finsetProd (Finset.univ : Finset (Fin n)) (fun _ _ ↦ H)
      apply H'.congr'
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
      exact (exp_logLik_prod x (fun i ↦ hx i ▸ Nat.zero_le _) ha hb).symm

private theorem binary_binomial_strict {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) (hp : proportion 1 x ∈ Ioo 0 1) {q : ℝ}
    (hq : q ∈ Ioo 0 1) (hne : q ≠ proportion 1 x) :
    binomialLogLik 1 x q < binomialLogLik 1 x (proportion 1 x) := by
  let p := proportion 1 x
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hs : (∑ i, (x i : ℝ)) = (n : ℝ)*p := by
    simp only [p,proportion,mean,countSum,Nat.cast_one,div_one]
    field_simp
  have he (t : ℝ) : binomialLogLik 1 x t =
      (n : ℝ) * (p*Real.log t + (1-p)*Real.log (1-t)) := by
    unfold binomialLogLik
    have hchoose (i : Fin n) : (1 : ℕ).choose (x i) = 1 := by
      have := hx i
      interval_cases x i <;> decide
    simp only [hchoose,Nat.cast_one,Real.log_one,zero_add,Nat.cast_sub (hx _),
      Finset.sum_add_distrib, ← Finset.sum_mul]
    rw [Finset.sum_sub_distrib,hs]
    simp
    ring
  have hp' : 0 < 1-p := sub_pos.mpr hp.2
  have hq' : 0 < 1-q := sub_pos.mpr hq.2
  have ha := Real.log_lt_sub_one_of_pos (div_pos hq.1 hp.1)
    (fun h ↦ hne (by simpa using (div_eq_iff hp.1.ne').mp h))
  have hb := Real.log_le_sub_one_of_pos (div_pos hq' hp')
  rw [Real.log_div hq.1.ne' hp.1.ne'] at ha
  rw [Real.log_div hq'.ne' hp'.ne'] at hb
  have hA := mul_lt_mul_of_pos_left ha hp.1
  have hB := mul_le_mul_of_nonneg_left hb hp'.le
  change p * (Real.log q - Real.log p) < p * (q/p-1) at hA
  change (1-p) * (Real.log (1-q) - Real.log (1-p)) ≤ (1-p)*((1-q)/(1-p)-1) at hB
  have hp0 : p ≠ 0 := hp.1.ne'
  rw [show p*(q/p-1) = q-p by field_simp [hp0]] at hA
  rw [show (1-p)*((1-q)/(1-p)-1) = p-q by field_simp [hp'.ne']; ring] at hB
  rw [he,he]
  exact mul_lt_mul_of_pos_left (by linarith) hN

/-- Mixed zero-one data have precisely the size-one empirical-proportion
ridge. Holding positive alpha intersects that ridge in one beta. -/
theorem mixed_binary_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) (h0 : ∃ i, x i = 0) (h1 : ∃ i, x i = 1) :
    let D : Set (ℕ × ℝ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ q.2 ∈ domain}
    let L := fun q : ℕ × ℝ × ℝ ↦ logLik q.1 x q.2.1 q.2.2
    let p := proportion 1 x
    (∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 1 ∧ q.2.1/(q.2.1+q.2.2) = p) ∧
    (∀ a > 0, ∃! b, (1,a,b) ∈ D ∧ IsMaxOn L D (1,a,b)) := by
  intro D L p
  have hp : p ∈ Ioo 0 1 := binary_proportion hn x hx h0 h1
  let H := binomialLogLik 1 x p
  have bounds (q : ℕ × ℝ × ℝ) (hq : q ∈ D) :
      L q ≤ H ∧ (L q = H ↔ q.1 = 1 ∧ q.2.1/(q.2.1+q.2.2) = p) := by
    obtain ⟨m,a,b⟩ := q
    obtain ⟨hxm,ha,hb⟩ := hq
    have hm : 0 < m := by obtain ⟨i,hi⟩ := h1; exact Nat.succ_le_iff.mp (by simpa [hi] using hxm i)
    change 0 < a at ha
    change 0 < b at hb
    let s := cell m 0 a b + cell m 1 a b
    let t := cell m 1 a b / s
    have hs : 0 < s := add_pos (cell_pos m.zero_le ha hb) (cell_pos hm ha hb)
    have ht : t ∈ Ioo 0 1 := ⟨div_pos (cell_pos hm ha hb) hs,
      (div_lt_one hs).mpr (lt_add_of_pos_left _ (cell_pos m.zero_le ha hb))⟩
    have he : L (m,a,b) = binomialLogLik 1 x t + (n : ℝ)*Real.log s :=
      by
        have H := two_cell_likelihood hm x
          (fun i ↦ (show x i = 0 ∨ x i = 1 by have := hx i; omega)) (by decide) ha hb
        have he (i : Fin n) : (if x i = 0 then 0 else 1) = x i := by
          have := hx i
          split <;> omega
        simpa only [he] using H
    have hB : binomialLogLik 1 x t ≤ H :=
      binomial_isMaxOn (by decide) hn hx hp.1 hp.2 ht
    by_cases hm1 : m = 1
    · subst m
      have hz := one_cell 0 (by decide) ha hb
      have hu := one_cell 1 le_rfl ha hb
      simp only [ite_true] at hz
      simp only [Nat.one_ne_zero,ite_false] at hu
      have hS : s = 1 := by dsimp [s]; rw [hz,hu]; ring
      have hT : t = a/(a+b) := by dsimp [t]; rw [hS,hu,div_one]
      simp only [he,hS,Real.log_one,mul_zero,add_zero,true_and]
      refine ⟨hB,?_⟩
      rw [← hT]
      exact ⟨fun h ↦ by
        by_contra hne
        exact (binary_binomial_strict hn x hx hp ht hne).ne h,
        fun h ↦ by rw [h]⟩
    · have hm2 : 2 ≤ m := by omega
      have hsub : ({0,1,2} : Finset ℕ) ⊆ Finset.range (m+1) := by
        intro j hj
        simp only [Finset.mem_insert,Finset.mem_singleton] at hj
        rcases hj with rfl | rfl | rfl <;> exact Finset.mem_range.mpr (by omega)
      have hsum := Finset.sum_le_sum_of_subset_of_nonneg (f := fun j ↦ cell m j a b) hsub
        (fun j hj _ ↦ (cell_pos (by have := Finset.mem_range.mp hj; omega) ha hb).le)
      rw [cell_sum m ha hb,Finset.sum_insert (by decide), Finset.sum_pair (by decide : 1 ≠ 2)] at hsum
      have hs' : s < 1 := by dsimp [s]; linarith [cell_pos hm2 ha hb]
      have hlt : L (m,a,b) < H := by
        rw [he]
        linarith [mul_neg_of_pos_of_neg (Nat.cast_pos.mpr hn : 0 < (n : ℝ))
          (Real.log_neg hs hs')]
      exact ⟨hlt.le,iff_of_false hlt.ne (fun h ↦ hm1 h.1)⟩
  have hb (a : ℝ) (ha : 0 < a) : 0 < a*(1-p)/p :=
    div_pos (mul_pos ha (sub_pos.mpr hp.2)) hp.1
  have hratio (a : ℝ) (ha : 0 < a) : a/(a + a*(1-p)/p) = p := by
    have hD : 0 < a + a*(1-p)/p := add_pos ha (hb a ha)
    field_simp [hp.1.ne',hD.ne']; ring
  let r : ℕ × ℝ × ℝ := (1,1,1*(1-p)/p)
  have hr : r ∈ D := ⟨hx,one_pos,hb 1 one_pos⟩
  have hrH : L r = H := (bounds r hr).2.mpr ⟨rfl,hratio 1 one_pos⟩
  have iff_max (q : ℕ × ℝ × ℝ) (hq : q ∈ D) :
      IsMaxOn L D q ↔ q.1 = 1 ∧ q.2.1/(q.2.1+q.2.2) = p := by
    rw [← (bounds q hq).2]
    exact ⟨fun h ↦ le_antisymm (bounds q hq).1 (hrH ▸ h hr),
      fun h _ hw ↦ h ▸ (bounds _ hw).1⟩
  refine ⟨iff_max,?_⟩
  intro a ha
  have h : (1,a,a*(1-p)/p) ∈ D := ⟨hx,ha,hb a ha⟩
  refine ⟨_,⟨h,(iff_max _ h).mpr ⟨rfl,hratio a ha⟩⟩,?_⟩
  intro b hb'
  have hz : 0 < b := hb'.1.2.2
  have hQ : a/(a+b) = p := ((iff_max _ hb'.1).mp hb'.2).2
  apply (eq_div_iff hp.1.ne').mpr
  have he := (div_eq_iff (add_pos ha hz).ne').mp hQ
  linarith only [he]

/-- On a fixed compatible size, polynomial comparisons of the two rising
products define exactly the original maximizing set. Denominators are
positive throughout the shape quadrant. -/
theorem held_size_polynomial_argmax {m n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) :
    let A := fun a b : ℝ ↦ ∏ i, (∏ j ∈ Finset.range (x i), (a+(j : ℝ))) *
      (∏ j ∈ Finset.range (m-x i), (b+(j : ℝ)))
    let B := fun a b : ℝ ↦ (∏ j ∈ Finset.range m, (a+b+(j : ℝ)))^n
    ∀ q : ℝ × ℝ, q ∈ domain →
      (IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q ↔
        ∀ r : ℝ × ℝ, r ∈ domain → A r.1 r.2 * B q.1 q.2 ≤ A q.1 q.2 * B r.1 r.2) := by
  intro A B q hq
  have hc : 0 < ∏ i, (m.choose (x i) : ℝ) :=
    Finset.prod_pos fun i _ ↦ Nat.cast_pos.mpr (Nat.choose_pos (hx i))
  have hB (v : ℝ × ℝ) (hv : v ∈ domain) : 0 < B v.1 v.2 :=
    pow_pos (rising_pos _ (add_pos hv.1 hv.2)) _
  have he (v : ℝ × ℝ) (hv : v ∈ domain) :
      Real.exp (logLik m x v.1 v.2) =
        (∏ i, (m.choose (x i) : ℝ)) * (A v.1 v.2 / B v.1 v.2) := by
    rw [exp_logLik_prod x hx hv.1 hv.2]
    simp only [cell,Finset.prod_div_distrib,Finset.prod_mul_distrib, Finset.prod_const,
      Finset.card_univ,Fintype.card_fin,rising, A, B]
    ring
  have hcmp (r : ℝ × ℝ) (hr : r ∈ domain) :
      logLik m x r.1 r.2 ≤ logLik m x q.1 q.2 ↔
        A r.1 r.2 * B q.1 q.2 ≤ A q.1 q.2 * B r.1 r.2 := by
    rw [← Real.exp_le_exp, he r hr, he q hq, mul_le_mul_iff_right₀ hc,
      div_le_div_iff₀ (hB r hr) (hB q hq)]
  exact ⟨fun H r hr ↦ (hcmp r hr).mp (H hr),fun H r hr ↦ (hcmp r hr).mpr (H r hr)⟩

/-- At a fixed positive size, one held positive beta gives explicit
countwise recovery paths for both endpoint-constant branches. -/
theorem fixed_size_point_mass_traces {m k : ℕ} (hk : k ≤ m) {b : ℝ} (hb : 0 < b) :
    let P := fun a : ℝ ↦
      (m.choose k : ℝ) * ((∏ j ∈ Finset.range k, (a+(j : ℝ))) *
        (∏ j ∈ Finset.range (m-k), (b+(j : ℝ)))) /
          (∏ j ∈ Finset.range m, (a+b+(j : ℝ)))
    Tendsto P (𝓝[>] 0) (𝓝 (if k = 0 then 1 else 0)) ∧
      Tendsto P atTop (𝓝 (if k = m then 1 else 0)) :=
  ⟨lower_atom_at_fixed_beta hk hb,upper_atom_at_fixed_beta hk hb⟩

/-- Failure at held size at least two has a sharp excluded height. With an
interior count and nonpositive dispersion it is the binomial height.
Endpoint-only histograms instead reach their saturated point or pair height. -/
theorem held_size_failure {m n : ℕ} (hm : 2 ≤ m) (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) :
    let L := fun q : ℝ × ℝ ↦ logLik m x q.1 q.2
    let Excluded := fun v ↦ IsLUB (L '' domain) v ∧ ∀ q ∈ domain, L q < v
    ((∃ i, 0 < x i ∧ x i < m) →
      variance x ≤ (m : ℝ) * proportion m x * (1 - proportion m x) →
      Excluded (binomialLogLik m x (proportion m x)) ∧
      Tendsto (fun s : ℝ ↦ logLik m x (proportion m x / s)
        ((1 - proportion m x) / s)) (𝓝[>] 0) (𝓝 (binomialLogLik m x (proportion m x)))) ∧
    ((∀ i, x i = 0) → Excluded 0 ∧
      ∀ b > 0, Tendsto (fun a ↦ L (a,b)) (𝓝[>] 0) (𝓝 0)) ∧
    ((∀ i, x i = m) → Excluded 0 ∧
      ∀ b > 0, Tendsto (fun a ↦ L (a,b)) atTop (𝓝 0)) ∧
    ((∀ i, x i = 0 ∨ x i = m) → (∃ i, x i = 0) → (∃ i, x i = m) →
      let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
      let p := proportion 1 y
      let B := binomialLogLik 1 y p
      Excluded B ∧
        Tendsto (fun s : ℝ ↦ logLik m x (p*s) ((1-p)*s)) (𝓝[>] 0) (𝓝 B)) := by
  intro L Excluded
  have hm' : 0 < m := by omega
  have hlub {v : ℝ} (hs : sSup (L '' domain) = v) (h : ∀ q ∈ domain, L q < v) :
      Excluded v := by
    refine ⟨?_, h⟩
    rw [← hs]
    exact isLUB_csSup ⟨_, ⟨(1, 1), ⟨one_pos, one_pos⟩, rfl⟩⟩
      ⟨v, by rintro _ ⟨q, hq, rfl⟩; exact (h q hq).le⟩
  have cellLog {c : ℕ} (hc : c ≤ m) (hxc : ∀ i, x i = c) {l : Filter ℝ}
      (hl : ∀ᶠ a in l, 0 < a) {b : ℝ} (hb : 0 < b)
      (h : Tendsto (fun a ↦ cell m c a b) l (𝓝 1)) :
      Tendsto (fun a ↦ L (a,b)) l (𝓝 0) := by
    have H := ((h.log one_ne_zero).const_mul (n : ℝ))
    simp only [Real.log_one,mul_zero] at H
    apply H.congr'
    filter_upwards [hl] with a ha
    simp [L,logLik,hxc,log_cell hc ha hb]
    ring
  refine ⟨?_, ?_, ?_, ?_⟩
  · intro hi hV
    have hu := interior_proportion hn x hx hi
    exact ⟨hlub (binomial_sup_of_nonpos hm hn hx hu.1 hu.2 hV)
      (fun q hq ↦ necessity hm hn hx hu.1 hu.2 hV hq.1 hq.2),
      logLik_tendsto hx hu.1 hu.2⟩
  · intro hz
    refine ⟨hlub (allZero_sup hm' hn hz).1 (allZero_sup hm' hn hz).2,fun b hb ↦ ?_⟩
    apply cellLog m.zero_le hz (l := 𝓝[>] 0) self_mem_nhdsWithin hb
    simpa using lower_atom_at_fixed_beta m.zero_le hb
  · intro hf
    refine ⟨hlub (allSize_sup hm' hn hf).1 (allSize_sup hm' hn hf).2,fun b hb ↦ ?_⟩
    apply cellLog le_rfl hf (eventually_gt_atTop _ ) hb
    simpa using upper_atom_at_fixed_beta (m := m) le_rfl hb
  · intro he hz hf y p B
    have hp : p ∈ Ioo 0 1 := binary_proportion hn y
      (fun i ↦ by dsimp [y]; split <;> omega)
      (hz.imp fun i hi ↦ by simp [y, hi])
      (hf.imp fun i hi ↦ by simp [y, hi, hm'.ne'])
    exact ⟨mixed_endpoint_sup (by omega) hn x he hz hf, endpoint_likelihood_path hm' x he hp⟩

/-- If the observed support is exactly zero and one positive count above
one, the saturated endpoint mixture strictly dominates every finite fit
at every compatible size. -/
theorem all_free_mixed_endpoint_failure {n u : ℕ} (hn : 0 < n) (hu : 2 ≤ u)
    (x : Fin n → ℕ) (hx : ∀ i, x i = 0 ∨ x i = u)
    (h0 : ∃ i, x i = 0) (hu' : ∃ i, x i = u) :
    let D : Set (ℕ × ℝ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ q.2 ∈ domain}
    let L := fun q : ℕ × ℝ × ℝ ↦ logLik q.1 x q.2.1 q.2.2
    let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
    let p := proportion 1 y
    let H := binomialLogLik 1 y p
    IsLUB (L '' D) H ∧ (∀ q ∈ D, L q < H) ∧
      Tendsto (fun t : ℝ ↦ L (u,p*t,(1-p)*t)) (𝓝[>] 0) (𝓝 H) := by
  intro D L y p H
  have hy (i : Fin n) : y i ≤ 1 := by dsimp [y]; split <;> omega
  have hp : p ∈ Ioo 0 1 := binary_proportion hn y hy
    (h0.imp fun i hi ↦ by simp [y,hi]) (hu'.imp fun i hi ↦ by simp [y,hi,show u ≠ 0 by omega])
  have strict (q : ℕ × ℝ × ℝ) (hq : q ∈ D) : L q < H := by
    obtain ⟨m,a,b⟩ := q
    obtain ⟨hxm,ha,hb⟩ := hq
    change 0 < a at ha
    change 0 < b at hb
    have hum : u ≤ m := by obtain ⟨i,hi⟩ := hu'; simpa [hi] using hxm i
    let s := cell m 0 a b + cell m u a b
    let t := cell m u a b / s
    have hs : 0 < s := add_pos (cell_pos m.zero_le ha hb) (cell_pos hum ha hb)
    have ht : t ∈ Ioo 0 1 := ⟨div_pos (cell_pos hum ha hb) hs,
      (div_lt_one hs).mpr (lt_add_of_pos_left _ (cell_pos m.zero_le ha hb))⟩
    have hB : binomialLogLik 1 y t ≤ H :=
      binomial_isMaxOn (by decide) hn hy hp.1 hp.2 ht
    have hsub : ({0,1,u} : Finset ℕ) ⊆ Finset.range (m+1) := by
      intro j hj
      simp only [Finset.mem_insert,Finset.mem_singleton] at hj
      rcases hj with rfl | rfl | rfl <;> exact Finset.mem_range.mpr (by omega)
    have hsum := Finset.sum_le_sum_of_subset_of_nonneg (f := fun j ↦ cell m j a b) hsub
      (fun j hj _ ↦ (cell_pos (by have := Finset.mem_range.mp hj; omega) ha hb).le)
    rw [cell_sum m ha hb,Finset.sum_insert (by simp; omega),
      Finset.sum_pair (show 1 ≠ u by omega)] at hsum
    have hs' : s < 1 := by dsimp [s]; linarith [cell_pos (show 1 ≤ m by omega) ha hb]
    have he : L (m,a,b) = binomialLogLik 1 y t + (n : ℝ)*Real.log s :=
      two_cell_likelihood hum x hx (by omega) ha hb
    rw [he]
    linarith [mul_neg_of_pos_of_neg (Nat.cast_pos.mpr hn : 0 < (n : ℝ)) (Real.log_neg hs hs')]
  have path : Tendsto (fun t : ℝ ↦ L (u,p*t,(1-p)*t)) (𝓝[>] 0) (𝓝 H) :=
    endpoint_likelihood_path (by omega) x hx hp
  refine ⟨⟨?_,?_⟩,strict,path⟩
  · rintro _ ⟨q,hq,rfl⟩
    exact (strict q hq).le
  · intro v hv
    apply le_of_tendsto path
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    exact hv ⟨(u,p*t,(1-p)*t),
      ⟨fun i ↦ (hx i).elim (fun h ↦ h ▸ u.zero_le) (fun h ↦ h.le),
        mul_pos hp.1 ht,mul_pos (sub_pos.mpr hp.2) ht⟩,rfl⟩

end UnivariateML.Base8bBBinom

end
