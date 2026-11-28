module

public import UnivariateML.Family.Base7Hyper
public import UnivariateML.Family.Base7bHyperBinary
public import UnivariateML.Family.Discrete
import Mathlib.Topology.Sequences

set_option linter.style.header false

/-! Bracketing fixed-draw composition winners around the fitted proportion.

## Deviations
The bracket bounds a finite set of possible integer offsets by elementary
log bounds. It does not locate the two sharp offsets from the quadratic
adjacent-ratio expansion.
-/

@[expose] public section

namespace UnivariateML.Base7bHyperResidue

open Real Filter Set Hypergeometric Polynomial
open scoped Topology

private theorem log_one_sub_bounds {z : ℝ} (h : z < 1) :
    -(z / (1 - z)) ≤ log (1 - z) ∧ log (1 - z) ≤ -z := by
  have hp := sub_pos.mpr h
  have H := log_le_sub_one_of_pos (inv_pos.mpr hp)
  rw [log_inv] at H
  have he : (1 - z)⁻¹ - 1 = z / (1 - z) := by field_simp [hp.ne']; ring
  rw [he] at H
  exact ⟨by linarith, (log_le_sub_one_of_pos hp).trans_eq (by ring)⟩

private theorem composition_log_bounds {h N K k : ℕ} (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hK : k ≤ K) (hN : K + k < N) :
    let S : ℝ := ∑ i, (x i : ℝ)
    let T : ℝ := ∑ i, ((k : ℝ) - x i)
    0 < compositionRatio K N k x ∧
    S / ((K : ℝ) + 1) - T / ((N : ℝ) - K - k) ≤ log (compositionRatio K N k x) ∧
    log (compositionRatio K N k x) ≤ S / ((K : ℝ) + 1 - k) - T / ((N : ℝ) - K) := by
  intro S T
  let A := (K : ℝ) + 1
  let B := (N : ℝ) - K
  have hA : 0 < A := by dsimp [A]; positivity
  have hBK : 0 < B - k := by
    have E : (K : ℝ) + k < N := by exact_mod_cast hN
    dsimp [B]; linarith
  have hB : 0 < B := by linarith [Nat.cast_nonneg (α := ℝ) k]
  have hAK : 0 < A - k := by
    have E : (k : ℝ) ≤ K := by exact_mod_cast hK
    dsimp [A]; linarith
  let xp (i : Fin h) := (x i : ℝ) / A
  let yp (i : Fin h) := ((k : ℝ) - x i) / B
  have hp (i : Fin h) : xp i < 1 ∧ yp i < 1 := by
    have hi : (x i : ℝ) ≤ k := by exact_mod_cast hx i
    exact ⟨(div_lt_one hA).mpr (by linarith),
      (div_lt_one hB).mpr (by linarith [Nat.cast_nonneg (α := ℝ) (x i)])⟩
  have he (i : Fin h) :
      ((K + 1 : ℕ) : ℝ) * ((N + x i - K - k : ℕ) : ℝ) /
        (((K + 1 - x i : ℕ) : ℝ) * ((N - K : ℕ) : ℝ)) =
        (1 - yp i) / (1 - xp i) := by
    have hxK : x i ≤ K + 1 := (hx i).trans (hK.trans (Nat.le_succ _))
    have hKN : K ≤ N := by omega
    have hKn : K ≤ N + x i := by omega
    have hkn : k ≤ N + x i - K := by omega
    push_cast [Nat.cast_sub hxK, Nat.cast_sub hKN, Nat.cast_sub hKn, Nat.cast_sub hkn]
    rw [show (N : ℝ) + x i - K - k = B - ((k : ℝ) - x i) by dsimp [B]; ring]
    change A * (B - ((k : ℝ) - x i)) / ((A - x i) * B) =
      (1 - ((k : ℝ) - x i) / B) / (1 - (x i : ℝ) / A)
    have hxp : 0 < 1 - (x i : ℝ) / A := sub_pos.mpr (hp i).1
    have hAx : 0 < A - x i := (by
      have E : (x i : ℝ) ≤ k := by exact_mod_cast hx i
      linarith)
    field_simp [hA.ne', hB.ne', hAx.ne', hxp.ne']
  unfold compositionRatio
  simp_rw [he]
  have hfac (i : Fin h) : 0 < (1 - yp i) / (1 - xp i) :=
    div_pos (sub_pos.mpr (hp i).2) (sub_pos.mpr (hp i).1)
  refine ⟨Finset.prod_pos (fun i _ ↦ hfac i), ?_⟩
  rw [log_prod (fun i _ ↦ (hfac i).ne')]
  simp_rw [log_div (sub_pos.mpr (hp _).2).ne' (sub_pos.mpr (hp _).1).ne']
  have hdiv (i : Fin h) :
      xp i / (1 - xp i) = (x i : ℝ) / (A - x i) ∧
      yp i / (1 - yp i) = ((k : ℝ) - x i) / (B - ((k : ℝ) - x i)) := by
    have hAx : 0 < A - x i := by
      have E : (x i : ℝ) ≤ k := by exact_mod_cast hx i
      linarith
    have hBy : 0 < B - ((k : ℝ) - x i) := by linarith [Nat.cast_nonneg (α := ℝ) (x i)]
    constructor
    · dsimp [xp]
      field_simp [hAx.ne', hA.ne', (sub_pos.mpr (hp i).1).ne']
    · dsimp [yp]
      field_simp [hBy.ne', hB.ne', (sub_pos.mpr (hp i).2).ne']
  have hlow (i : Fin h) :
      (x i : ℝ) / A - ((k : ℝ) - x i) / (B - k) ≤
        log (1 - yp i) - log (1 - xp i) := by
    have H := (log_one_sub_bounds (hp i).2).1
    rw [(hdiv i).2] at H
    have J := (log_one_sub_bounds (hp i).1).2
    have hi : (x i : ℝ) ≤ k := by exact_mod_cast hx i
    have hd := div_le_div_of_nonneg_left (sub_nonneg.mpr hi) hBK
      (by linarith [Nat.cast_nonneg (α := ℝ) (x i)] :
        B - k ≤ B - ((k : ℝ) - x i))
    change log (1 - xp i) ≤ -((x i : ℝ) / A) at J
    linarith only [H, J, hd]
  have hhigh (i : Fin h) : log (1 - yp i) - log (1 - xp i) ≤
      (x i : ℝ) / (A - k) - ((k : ℝ) - x i) / B := by
    have H := (log_one_sub_bounds (hp i).1).1
    rw [(hdiv i).1] at H
    have J := (log_one_sub_bounds (hp i).2).2
    have hi : (x i : ℝ) ≤ k := by exact_mod_cast hx i
    have hd := div_le_div_of_nonneg_left (Nat.cast_nonneg (α := ℝ) (x i)) hAK
      (show A - k ≤ A - x i by linarith)
    change log (1 - yp i) ≤ -(((k : ℝ) - x i) / B) at J
    linarith only [H, J, hd]
  have hsum (a b : ℝ) :
      (∑ i, ((x i : ℝ) / a - ((k : ℝ) - x i) / b)) = S / a - T / b := by
    simp only [Finset.sum_sub_distrib, div_eq_mul_inv, ← Finset.sum_mul, S, T]
  exact ⟨(hsum A (B - k)) ▸ Finset.sum_le_sum (fun i _ ↦ hlow i),
    (hsum (A - k) B) ▸ Finset.sum_le_sum (fun i _ ↦ hhigh i)⟩

/-- Away from the two support margins, the adjacent likelihood ratio has
a strict sign outside a fixed-width band around the fitted binomial
composition. This supplies outer brackets without an asymptotic expansion. -/
theorem held_draw_ratio_brackets {h N K k : ℕ} (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hK : k ≤ K) (hN : K + k < N)
    (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let p : ℝ := (∑ i, (x i : ℝ)) / ((∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i))
    ((K : ℝ) + 1 + k < p * N → 1 < compositionRatio K N k x) ∧
    (p * N + k < (K : ℝ) → compositionRatio K N k x < 1) := by
  intro p
  let S : ℝ := ∑ i, (x i : ℝ)
  let T : ℝ := ∑ i, ((k : ℝ) - x i)
  have hp : 0 < p := div_pos hS (add_pos hS hT)
  have hp1 : p < 1 := (div_lt_one (add_pos hS hT)).mpr (by linarith)
  have hST : p * (S + T) = S := div_mul_cancel₀ _ (add_pos hS hT).ne'
  have hk : 0 ≤ (k : ℝ) := Nat.cast_nonneg _
  have hKN : (K : ℝ) + k < N := by exact_mod_cast hN
  have hK' : (k : ℝ) ≤ K := by exact_mod_cast hK
  obtain ⟨hr, hlow, hhigh⟩ := composition_log_bounds x hx hK hN
  constructor
  · intro H
    apply (log_lt_log_iff zero_lt_one hr).mp
    rw [log_one]
    have H₁ : (K : ℝ) + 1 < p * ((N : ℝ) - k + 1) := by
      have H₂ : p * (k : ℝ) ≤ k := by nlinarith only [mul_nonneg (sub_nonneg.mpr hp1.le) hk]
      nlinarith only [H, H₂, hp]
    have H₃ := mul_lt_mul_of_pos_right H₁ (add_pos hS hT)
    have hdiv : T / ((N : ℝ) - K - k) < S / ((K : ℝ) + 1) := by
      apply (div_lt_div_iff₀ (by linarith) (by positivity)).mpr
      calc
        _ = (S + T) * ((K : ℝ) + 1) - S * ((K : ℝ) + 1) := by ring
        _ < S * ((N : ℝ) - k + 1) - S * ((K : ℝ) + 1) := by
          apply sub_lt_sub_right
          nlinarith only [H₃, congrArg (fun z ↦ z * ((N : ℝ) - k + 1)) hST]
        _ = _ := by ring
    linarith only [hlow, hdiv]
  · intro H
    apply (log_lt_log_iff hr zero_lt_one).mp
    rw [log_one]
    have H₁ : p * N + (1 - p) * ((k : ℝ) - 1) < K := by
      have H₂ : (1 - p) * ((k : ℝ) - 1) ≤ k := by
        nlinarith only [mul_nonneg hp.le hk, hp1]
      linarith
    have H₃ := mul_lt_mul_of_pos_right H₁ (add_pos hS hT)
    have hdiv : S / ((K : ℝ) + 1 - k) < T / ((N : ℝ) - K) := by
      apply (div_lt_div_iff₀ (by linarith) (by linarith)).mpr
      have H₄ : (1 - p) * (S + T) = T := by linarith [hST]
      calc
        _ = S * N - S * K := by ring
        _ < (S + T) * K - T * ((k : ℝ) - 1) - S * K := by
          apply sub_lt_sub_right
          linarith only [show S * N + T * ((k : ℝ) - 1) < (S + T) * K by
            nlinarith only [H₃, congrArg (fun z : ℝ ↦ z * (N : ℝ)) hST,
              congrArg (fun z ↦ z * ((k : ℝ) - 1)) H₄]]
        _ = _ := by ring
    linarith only [hhigh, hdiv]

private theorem composition_likelihood_pos {h N k M : ℕ} {x : Fin h → ℕ}
    (hk : k ≤ N) (hm : M ∈ compositionDomain N k x) : 0 < likelihood M N k x := by
  apply Finset.prod_pos
  intro i _
  have hi := hm.2 i
  have hc : Compatible M N k (x i) := ⟨hm.1, hk, hi.1, hi.2⟩
  rw [mass_choose hc]
  have hdiff : k - x i ≤ N - M := by
    have := hi.2.2
    omega
  exact div_pos (mul_pos (Nat.cast_pos.mpr (Nat.choose_pos hi.1))
    (Nat.cast_pos.mpr (Nat.choose_pos hdiff))) (Nat.cast_pos.mpr (Nat.choose_pos hk))

/-- At every sufficiently large population, all fixed-draw composition
winners lie in a finite band of integer offsets from the fitted proportion.
The bound covers both maximizers when a slice ties. -/
theorem held_draw_population_band {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let p : ℝ := (∑ i, (x i : ℝ)) / ((∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i))
    ∃ B : ℕ, ∀ N ≥ B, ∀ M ∈ compositionDomain N k x,
      IsMaxOn (fun m ↦ likelihood m N k x) (compositionDomain N k x) M →
      ⌊p * N⌋₊ - (k + 2) < M ∧ M ≤ ⌊p * N⌋₊ + (k + 2) := by
  intro p
  have hp : 0 < p := div_pos hS (add_pos hS hT)
  have hp1 : p < 1 := (div_lt_one (add_pos hS hT)).mpr (by linarith)
  have hk : 0 < k := by
    by_contra! H
    have hz : k = 0 := by omega
    have hx0 (i : Fin h) : x i = 0 := by have := hx i; omega
    simp [hx0] at hS
  have hP : Tendsto (fun N : ℕ ↦ p * N) atTop atTop :=
    tendsto_natCast_atTop_atTop.const_mul_atTop hp
  have hQ : Tendsto (fun N : ℕ ↦ (1 - p) * N) atTop atTop :=
    tendsto_natCast_atTop_atTop.const_mul_atTop (sub_pos.mpr hp1)
  obtain ⟨B, hB⟩ := eventually_atTop.mp
    ((hP.eventually_gt_atTop ((2 : ℝ) * k + 4)).and (hQ.eventually_gt_atTop ((2 : ℝ) * k + 4)))
  refine ⟨B, fun N hN M hM hMax ↦ ?_⟩
  have ⟨hP', hQ'⟩ := hB N hN
  let A := ⌊p * N⌋₊
  let L := A - (k + 2)
  let U := A + (k + 2)
  have hfloor : (A : ℝ) ≤ p * N := Nat.floor_le (by positivity)
  have hfloor' : p * N < (A : ℝ) + 1 := Nat.lt_floor_add_one _
  have hAk : 2 * k + 2 ≤ A := by
    have H : ((2 * k + 2 : ℕ) : ℝ) ≤ A := by
      push_cast
      linarith
    exact_mod_cast H
  have hLk : k ≤ L := by dsimp [L]; omega
  have hUk : k ≤ U := by dsimp [U]; omega
  have hLU : L ≤ U := by dsimp [L, U]; omega
  have hUN : U + k < N := by
    have H : ((U + k : ℕ) : ℝ) < N := by
      simp [U, Nat.cast_add]
      nlinarith only [hfloor, hQ']
    exact_mod_cast H
  have hkN : k ≤ N := by omega
  have hMN : M ≤ N := hM.1
  have hLn : L + k < N := by omega
  have hLbr : (L : ℝ) + 1 + k < p * N := by
    dsimp [L]
    rw [Nat.cast_sub (by omega : k + 2 ≤ A), Nat.cast_add, Nat.cast_ofNat]
    linarith
  have hUbr : p * N + k < (U : ℝ) := by
    simp only [U, Nat.cast_add, Nat.cast_ofNat]
    linarith
  have hlow : 1 < compositionRatio L N k x :=
    (held_draw_ratio_brackets x hx hLk hLn hS hT).1 hLbr
  have hhigh : compositionRatio U N k x < 1 :=
    (held_draw_ratio_brackets x hx hUk hUN hS hT).2 hUbr
  have feasible (m : ℕ) (hm : k ≤ m) (hN : m + k ≤ N) : m ∈ compositionDomain N k x :=
    ⟨by omega, fun i ↦ ⟨(hx i).trans hm, hx i, by omega⟩⟩
  have hLD := feasible L hLk (by omega)
  have hLD' := feasible (L + 1) (by omega) (by omega)
  have hUD := feasible U hUk (by omega)
  have hUD' := feasible (U + 1) (by omega) (by omega)
  constructor
  · by_contra! hm
    have hD : M + 1 ∈ compositionDomain N k x := by
      refine ⟨by omega, fun i ↦ ?_⟩
      have H := hM.2 i
      omega
    have hR : 1 < compositionRatio M N k x := by
      rcases hm.lt_or_eq with h | rfl
      · exact hlow.trans (composition_ratio_strict hh hk h hM hD hLD hLD')
      · exact hlow
    have hle := hMax hD
    change likelihood (M + 1) N k x ≤ likelihood M N k x at hle
    rw [composition_adjacent hkN hM hD] at hle
    exact (not_le_of_gt (lt_mul_of_one_lt_right (composition_likelihood_pos hkN hM) hR)) hle
  · by_contra! hm
    let J := M - 1
    have hJ : J + 1 = M := by dsimp [J]; omega
    have hUJ : U ≤ J := by dsimp [J]; omega
    have hD : J ∈ compositionDomain N k x := by
      refine ⟨by omega, fun i ↦ ?_⟩
      have H := hx i
      have H' := (hM.2 i).2.2
      omega
    have hD' : J + 1 ∈ compositionDomain N k x := hJ.symm ▸ hM
    have hR : compositionRatio J N k x < 1 := by
      rcases hUJ.lt_or_eq with h | he
      · exact (composition_ratio_strict hh hk h hUD hUD' hD hD').trans hhigh
      · rwa [← he]
    have hle := hMax hD
    change likelihood J N k x ≤ likelihood M N k x at hle
    rw [← hJ, composition_adjacent hkN hD hD'] at hle
    exact (not_le_of_gt (mul_lt_of_lt_one_right (composition_likelihood_pos hkN hD) hR)) hle

/-- A finite table of rational affine rays contains every tail slice winner.
For each table entry, the leading sign gives the comparison with a rational
height and the zero polynomial identifies all equality along that ray. -/
theorem held_draw_affine_tail_table {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i))
    (face : ℚ) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      (∀ N ≥ B, ∀ m ∈ compositionDomain N k x,
        IsMaxOn (fun v ↦ likelihood v N k x) (compositionDomain N k x) m →
        ∃ e ∈ E, (d : ℤ) * m = (a : ℤ) * N + e) ∧
      ∀ N ≥ B, ∀ (m : ℕ) (e : ℤ), e ∈ E → (d : ℤ) * m = (a : ℤ) * N + e →
        (∀ i, Compatible m N k (x i)) →
        (((face : ℝ) < likelihood m N k x ↔ 0 < (P e).leadingCoeff) ∧
         (likelihood m N k x = (face : ℝ) ↔ P e = 0) ∧
         (likelihood m N k x < (face : ℝ) ↔ (P e).leadingCoeff < 0)) := by
  classical
  intro a d E
  let p : ℝ := (∑ i, (x i : ℝ)) / ((∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i))
  have hp : 0 < p := div_pos hS (add_pos hS hT)
  have hd : 0 < (d : ℝ) := by
    have he : (d : ℝ) = (∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i) := by
      simp [d, Finset.sum_sub_distrib, nsmul_eq_mul]
    rw [he]
    exact add_pos hS hT
  have hpd : p * (d : ℝ) = a := by
    have he : (d : ℝ) = (∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i) := by
      simp [d, Finset.sum_sub_distrib, nsmul_eq_mul]
    rw [he]
    dsimp [p, a]
    rw [div_mul_cancel₀ _ (add_pos hS hT).ne', Nat.cast_sum]
  have hdq : (d : ℚ) ≠ 0 := by exact_mod_cast hd.ne'
  obtain ⟨B, hB⟩ := held_draw_population_band hh k x hx hS hT
  have H (e : ℤ) := Discrete5HyperResidue.affine_face_tail hh k x
    ((a : ℚ) / d) ((e : ℚ) / d) face
  choose P C hP using H
  refine ⟨max B (E.sup C), P, ?_, ?_⟩
  · intro N hN m hm hmax
    obtain ⟨hlo, hhi⟩ := hB N ((le_max_left _ _).trans hN) m hm hmax
    let A := ⌊p * N⌋₊
    change A - (k + 2) < m at hlo
    change m ≤ A + (k + 2) at hhi
    have hA₀ : (A : ℝ) ≤ p * N := Nat.floor_le (by positivity)
    have hA₁ : p * N < (A : ℝ) + 1 := Nat.lt_floor_add_one _
    have hmL : (A : ℝ) - ((k : ℝ) + 2) ≤ m := by
      by_cases hh : k + 2 ≤ A
      · have H : ((A - (k + 2) : ℕ) : ℝ) ≤ m := by exact_mod_cast hlo.le
        rw [Nat.cast_sub hh, Nat.cast_add, Nat.cast_ofNat] at H
        exact H
      · have H : (A : ℝ) ≤ k + 2 := by exact_mod_cast Nat.le_of_not_ge hh
        linarith [Nat.cast_nonneg (α := ℝ) m]
    have hmU : (m : ℝ) ≤ (A : ℝ) + (k + 2) := by exact_mod_cast hhi
    let e : ℤ := (d : ℤ) * m - (a : ℤ) * N
    have he : (e : ℝ) = (d : ℝ) * ((m : ℝ) - p * N) := by
      push_cast [e]
      linear_combination (N : ℝ) * hpd
    have hwidth :
        -((d : ℝ) * (k + 3)) ≤ (e : ℝ) ∧ (e : ℝ) ≤ (d : ℝ) * (k + 3) := by
      rw [he, ← mul_neg]
      exact ⟨mul_le_mul_of_nonneg_left (by linarith) hd.le,
        mul_le_mul_of_nonneg_left (by linarith) hd.le⟩
    refine ⟨e, Finset.mem_Icc.mpr ⟨?_, ?_⟩, ?_⟩
    · exact_mod_cast hwidth.1
    · exact_mod_cast hwidth.2
    · dsimp [e]
      omega
  · intro N hN m e he hrel hc
    have hN' : C e ≤ N := (Finset.le_sup he).trans ((le_max_right _ _).trans hN)
    have hA : (m : ℚ) = ((a : ℚ) / d) * N + (e : ℚ) / d := by
      have H : (d : ℚ) * m = (a : ℚ) * N + e := by exact_mod_cast hrel
      field_simp [hdq]
      linarith
    exact hP e m N hN' hA hc

private theorem binomial_face_max {h k : ℕ} (x : Fin h → ℕ) (hx : ∀ i, x i ≤ k)
    (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let p : ℝ := (∑ i, (x i : ℝ)) / ((∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i))
    p ∈ Icc 0 1 ∧
      IsMaxOn (fun t ↦ ∏ i, Discrete4Hyper.binomialFace k (x i) t) (Icc 0 1) p := by
  intro p
  let S := ∑ i, x i
  let T := ∑ i, (k - x i)
  have hSr : (S : ℝ) = ∑ i, (x i : ℝ) := by simp [S]
  have hTr : (T : ℝ) = ∑ i, ((k : ℝ) - x i) := by
    simp only [T, Nat.cast_sum]
    exact Finset.sum_congr rfl fun i _ ↦ Nat.cast_sub (hx i)
  have hSp : 0 < (S : ℝ) := hSr ▸ hS
  have hTp : 0 < (T : ℝ) := hTr ▸ hT
  have hp : p = (S : ℝ) / (S + T) := by simp [p, hSr, hTr]
  have hpos : 0 < (S : ℝ) + T := add_pos hSp hTp
  have hpI : p ∈ Icc 0 1 := by
    rw [hp]
    exact ⟨div_nonneg hSp.le hpos.le, (div_le_one hpos).mpr (by linarith)⟩
  let C := ∏ i, (k.choose (x i) : ℝ)
  have hEq (t : ℝ) (ht : t ∈ Icc 0 1) :
      (∏ i, Discrete4Hyper.binomialFace k (x i) t) =
        C * Discrete.Binomial.likelihood (S : ℝ) (T : ℝ) t := by
    simp only [Discrete4Hyper.binomialFace, Finset.prod_mul_distrib, Finset.prod_pow_eq_pow_sum]
    have H : t ^ S * (1 - t) ^ T = Discrete.Binomial.likelihood (S : ℝ) (T : ℝ) t := by
      have hsn : S ≠ 0 := by exact_mod_cast hSp.ne'
      have htn : T ≠ 0 := by exact_mod_cast hTp.ne'
      by_cases he : t = 0
      · simp [he, hsn, Discrete.Binomial.likelihood, hSp.ne']
      by_cases he' : t = 1
      · simp [he', htn, Discrete.Binomial.likelihood, hTp.ne']
      have ha : 0 < t := ht.1.lt_of_ne (Ne.symm he)
      have hb : 0 < 1 - t := sub_pos.mpr (ht.2.lt_of_ne he')
      rw [Discrete.Binomial.likelihood, ite_eq_right (by simp [he, he']), exp_add,
        ← log_pow, ← log_pow, exp_log (pow_pos ha _), exp_log (pow_pos hb _)]
    rw [← H]
    dsimp [C, S, T]
    ring
  refine ⟨hpI, fun t ht ↦ ?_⟩
  change (∏ i, Discrete4Hyper.binomialFace k (x i) t) ≤ ∏ i, Discrete4Hyper.binomialFace k (x i) p
  rw [hEq t ht, hEq p hpI]
  have hm := ((Discrete.Binomial.fixed_size_mle hSp.le hTp.le hpos p hpI).mpr hp) ht
  exact mul_le_mul_of_nonneg_left hm (by dsimp [C]; positivity)

private theorem eligible_ray_has_tail {h a d k : ℕ} (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (ha : 0 < a) (had : a < d)
    (e : ℤ) (r : ℕ) (hr : (d : ℤ) ∣ (a : ℤ) * r + e) (B : ℕ) :
    ∃ N ≥ B, ∃ m : ℕ, k ≤ N ∧ (∀ i, Compatible m N k (x i)) ∧
      (d : ℤ) * m = (a : ℤ) * N + e := by
  obtain ⟨q, hq⟩ := hr
  let c : ℕ := B + e.natAbs + d * k + 1
  let N := r + d * c
  let M : ℤ := q + (a : ℤ) * c
  have hd : 0 < d := ha.trans had
  have hcN : c ≤ N := by
    calc
      c = 1 * c := (one_mul c).symm
      _ ≤ d * c := Nat.mul_le_mul_right c hd
      _ ≤ N := Nat.le_add_left _ _
  have hN : B ≤ N := (by dsimp [c]; omega : B ≤ c).trans hcN
  have H₁ : e.natAbs + d * k ≤ N := (by dsimp [c]; omega : e.natAbs + d * k ≤ c).trans hcN
  have hN' : (e.natAbs : ℤ) + d * k ≤ (N : ℤ) := by exact_mod_cast H₁
  have hM : (d : ℤ) * M = (a : ℤ) * N + e := by
    dsimp [M, N]
    linear_combination -hq
  have hE₁ : -(e.natAbs : ℤ) ≤ e := by omega
  have hE₂ : e ≤ (e.natAbs : ℤ) := by omega
  have ha' : (1 : ℤ) ≤ a := by exact_mod_cast ha
  have hda : (a : ℤ) + 1 ≤ d := by exact_mod_cast (Nat.succ_le_of_lt had)
  have hd' : (0 : ℤ) < d := by exact_mod_cast hd
  have hMlo : (k : ℤ) ≤ M := by
    by_contra! H
    have h₁ := mul_lt_mul_of_pos_left H hd'
    have h₂ := mul_le_mul_of_nonneg_right ha' (Int.natCast_nonneg N)
    nlinarith only [h₁, h₂, hM, hN', hE₁]
  have hMhi : M + k ≤ (N : ℤ) := by
    by_contra! H
    have h₁ := mul_lt_mul_of_pos_left H hd'
    have h₂ := mul_le_mul_of_nonneg_right hda (Int.natCast_nonneg N)
    nlinarith only [h₁, h₂, hM, hN', hE₂]
  let m := M.toNat
  have hm : (m : ℤ) = M := Int.toNat_of_nonneg ((Int.natCast_nonneg k).trans hMlo)
  have hmk : k ≤ m := by exact_mod_cast (show (k : ℤ) ≤ m by rw [hm]; exact hMlo)
  have hmN : m + k ≤ N := by exact_mod_cast (show (m : ℤ) + k ≤ N by rw [hm]; exact hMhi)
  exact ⟨N, hN, m, by omega, fun i ↦
    ⟨by omega, by omega, (hx i).trans hmk, hx i, by omega⟩, by rw [hm, hM]⟩

private theorem residue_eligible {a d N : ℕ} (e : ℤ) (hd : 0 < d) (h : (d : ℤ) ∣ a * (N : ℤ) + e) :
    ∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e := by
  refine ⟨N % d, Nat.mod_lt _ hd, ?_⟩
  have H : (N % d : ℕ) + d * (N / d) = N := Nat.mod_add_div _ _
  have H' : ((N % d : ℕ) : ℤ) + (d : ℤ) * ((N / d : ℕ) : ℤ) = N := by exact_mod_cast H
  have hsub : (d : ℤ) ∣ a * (d : ℤ) * ((N / d : ℕ) : ℤ) := by
    exact ⟨(a : ℤ) * ((N / d : ℕ) : ℤ), by ring⟩
  have E := dvd_sub h hsub
  convert E using 1
  linear_combination (a : ℤ) * H'

/-- For nondegenerate fitted proportions, fixed-draw attainment is decided
by a finite prefix and finitely many congruence classes of affine rays.
The zero comparison polynomial counts as a finite co-winner. -/
theorem held_draw_finite_test_interior {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      ((∃ v ∈ D, IsMaxOn L D v) ↔
        (∃ N < B, ∃ m, (m, N) ∈ D ∧ (F : ℝ) ≤ L (m, N)) ∨
        (∃ e ∈ E, (∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e) ∧
          (0 < (P e).leadingCoeff ∨ P e = 0))) := by
  intro a d E D L F
  have hSr : (a : ℝ) = ∑ i, (x i : ℝ) := by simp [a]
  have htotal : (d : ℝ) = (∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i) := by
    simp [d, Finset.sum_sub_distrib, nsmul_eq_mul]
  have ha : 0 < a := by
    have : 0 < (a : ℝ) := hSr ▸ hS
    exact_mod_cast this
  have had : a < d := by
    have : (a : ℝ) < (d : ℝ) := by rw [hSr, htotal]; linarith
    exact_mod_cast this
  have hdr : (d : ℝ) ≠ 0 := by exact_mod_cast (ha.trans had).ne'
  have hd : 0 < d := ha.trans had
  let p : ℝ := (a : ℝ) / d
  have H := binomial_face_max x hx hS hT
  change (∑ i, (x i : ℝ)) / ((∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i)) ∈ Icc 0 1 ∧ _ at H
  rw [← htotal, ← hSr] at H
  have hF : (F : ℝ) = ∏ i, Discrete4Hyper.binomialFace k (x i) p := by
    unfold F Discrete4Hyper.binomialFace
    push_cast
    rfl
  have hcrit : (∃ v ∈ D, IsMaxOn L D v) ↔ ∃ v ∈ D, (F : ℝ) ≤ L v := by
    rw [hF]
    exact Base7Hyper.held_draw_attains_iff hh x k hx H.1 H.2
  obtain ⟨B, P, hB, hP⟩ := held_draw_affine_tail_table hh k x hx hS hT F
  refine ⟨B, P, hcrit.trans ⟨?_, ?_⟩⟩
  · rintro ⟨⟨m, N⟩, hv, hvF⟩
    by_cases hN : N < B
    · exact Or.inl ⟨N, hN, m, hv, hvF⟩
    have hN' : B ≤ N := le_of_not_gt hN
    have hm : m ∈ compositionDomain N k x := ⟨hv.1, fun i ↦ (hv.2.2 i).2.2⟩
    obtain ⟨u, hu, hslice⟩ := composition_maximizers x hh (show 0 < k by
      by_contra! H
      have : k = 0 := by omega
      have hx0 (i : Fin h) : x i = 0 := by have := hx i; omega
      simp [hx0] at hS) hv.2.1 ⟨m, hm⟩
    have hw := (hslice u).mpr (Or.inl rfl)
    obtain ⟨z, hz, hrel⟩ := hB N hN' u hw.1 hw.2
    have hcompat (i : Fin h) : Compatible u N k (x i) :=
      ⟨hu.1, hv.2.1, (hu.2 i).1, (hu.2 i).2⟩
    have hf : (F : ℝ) ≤ likelihood u N k x := hvF.trans (hw.2 hm)
    refine Or.inr ⟨z, hz, residue_eligible z hd ⟨u, hrel.symm⟩, ?_⟩
    rcases hf.lt_or_eq with hf | he
    · exact Or.inl ((hP N hN' u z hz hrel hcompat).1.mp hf)
    · exact Or.inr ((hP N hN' u z hz hrel hcompat).2.1.mp he.symm)
  · rintro (⟨N, -, m, hv, hval⟩ | ⟨e, he, ⟨j, -, hj⟩, hsign⟩)
    · exact ⟨(m, N), hv, hval⟩
    · obtain ⟨N, hN, m, hkn, hcomp, hrel⟩ := eligible_ray_has_tail x hx ha had e j hj B
      have hm : m ≤ N := (hcomp ⟨0, hh⟩).1
      refine ⟨(m, N), ⟨hm, hkn, hcomp⟩, ?_⟩
      rcases hsign with hsign | hsign
      · exact ((hP N hN m e he hrel hcomp).1.mpr hsign).le
      · exact ((hP N hN m e he hrel hcomp).2.1.mpr hsign).ge

/-- At fixed draws, compatible endpoint data attain. Every other sample
is decided by a finite prefix and the exact rational ray test, with
finite equality eligible. -/
theorem held_draw_finite_test {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    (((∀ i, x i = 0) ∨ (∀ i, x i = k)) → ∃ v ∈ D, IsMaxOn L D v) ∧
    ((∃ i, 0 < x i) → (∃ i, x i < k) →
      ∃ (B : ℕ) (P : ℤ → ℚ[X]),
        ((∃ v ∈ D, IsMaxOn L D v) ↔
          (∃ N < B, ∃ m, (m, N) ∈ D ∧ (F : ℝ) ≤ L (m, N)) ∨
          (∃ e ∈ E, (∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e) ∧
            (0 < (P e).leadingCoeff ∨ P e = 0)))) := by
  intro a d E D L F
  constructor
  · rintro (hz | hk)
    · have he : x = fun _ ↦ 0 := funext hz
      have hd : (0, k) ∈ D := ⟨Nat.zero_le _, le_rfl, fun i ↦ by
        rw [hz i]
        exact ⟨Nat.zero_le _, le_rfl, le_rfl, Nat.zero_le _, by omega⟩⟩
      refine ⟨(0, k), hd, fun v hv ↦ ?_⟩
      change likelihood v.1 v.2 k x ≤ likelihood 0 k k x
      rw [he, (Discrete4Hyper.constant_saturated_iff hh).mpr
        ⟨Nat.zero_le _, le_rfl, Or.inl ⟨rfl, Or.inl rfl⟩⟩]
      rw [← (Discrete4Hyper.constant_attains_saturated (h := h) 0).1]
      exact (Discrete4Hyper.constant_attains_saturated (h := h) 0).2 _ _ _
    · have he : x = fun _ ↦ k := funext hk
      have hd : (k, k) ∈ D := ⟨le_rfl, le_rfl, fun i ↦ by
        rw [hk i]
        exact ⟨le_rfl, le_rfl, le_rfl, le_rfl, le_rfl⟩⟩
      refine ⟨(k, k), hd, fun v hv ↦ ?_⟩
      change likelihood v.1 v.2 k x ≤ likelihood k k k x
      rw [he]
      exact (Discrete4Hyper.constant_attains_saturated (h := h) k).2 _ _ _
  · rintro ⟨i, hi⟩ ⟨j, hj⟩
    have hS : 0 < ∑ i, (x i : ℝ) :=
      (Nat.cast_pos.mpr hi).trans_le (Finset.single_le_sum
        (fun i _ ↦ Nat.cast_nonneg (x i)) (Finset.mem_univ i))
    have hT : 0 < ∑ i, ((k : ℝ) - x i) :=
      (sub_pos.mpr (by exact_mod_cast hj : (x j : ℝ) < k)).trans_le (Finset.single_le_sum
        (f := fun i ↦ ((k : ℝ) - x i))
        (fun i _ ↦ sub_nonneg.mpr (by exact_mod_cast hx i)) (Finset.mem_univ j))
    exact held_draw_finite_test_interior hh k x hx hS hT

/-- At fixed marked count the same exact finite test applies after
exchanging draws and marks, with both endpoint cases included. -/
theorem held_mark_finite_test {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible k v.2 v.1 (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood k v.2 v.1 x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    (((∀ i, x i = 0) ∨ (∀ i, x i = k)) → ∃ v ∈ D, IsMaxOn L D v) ∧
    ((∃ i, 0 < x i) → (∃ i, x i < k) →
      ∃ (B : ℕ) (P : ℤ → ℚ[X]),
        ((∃ v ∈ D, IsMaxOn L D v) ↔
          (∃ N < B, ∃ m, (m, N) ∈ D ∧ (F : ℝ) ≤ L (m, N)) ∨
          (∃ e ∈ E, (∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e) ∧
            (0 < (P e).leadingCoeff ∨ P e = 0)))) := by
  intro a d E D L F
  let D' := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
  let L' := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
  have hD : D = D' := by
    ext v
    change (_ ∧ _ ∧ ∀ i, Compatible k v.2 v.1 (x i)) ↔
      (_ ∧ _ ∧ ∀ i, Compatible v.1 v.2 k (x i))
    constructor <;> rintro ⟨ha, hb, hc⟩ <;> refine ⟨ha, hb, fun i ↦ ?_⟩
    all_goals
      have H := hc i
      unfold Compatible at H ⊢
      omega
  have hL : L = L' := funext fun v ↦ likelihood_swap _ _ _ _
  rw [hD, hL]
  exact held_draw_finite_test hh k x hx

private theorem held_draw_cutoff {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    {H : ℝ} (hH : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, Discrete4Hyper.binomialFace k (x i) p) < H) :
    ∃ B : ℕ, ∀ N ≥ B, ∀ m, (∀ i, Compatible m N k (x i)) → likelihood m N k x < H := by
  by_contra! hb
  choose N hN m hc hl using hb
  have hN' : Tendsto N atTop atTop :=
    tendsto_atTop_mono (show ∀ j : ℕ, id j ≤ N j from hN) tendsto_id
  have hm (t : ℕ) : m t ≤ N t := (hc t ⟨0, hh⟩).1
  have hprop (t : ℕ) : (m t : ℝ) / N t ∈ Icc (0 : ℝ) 1 := by
    refine ⟨by positivity, ?_⟩
    by_cases h : N t = 0
    · simp [h]
    · exact (div_le_one (Nat.cast_pos.mpr (Nat.pos_of_ne_zero h))).mpr (by exact_mod_cast hm t)
  obtain ⟨p, hp, f, hf, hpf⟩ := isCompact_Icc.tendsto_subseq hprop
  have hlim := Discrete4Hyper.held_draw_binomial_limit x k hp (hN'.comp hf.tendsto_atTop)
    (Eventually.of_forall fun t ↦ hm (f t)) hpf
  exact (hH p hp).not_ge (ge_of_tendsto' hlim fun t ↦ hl (f t))

/-- Complete held-draws winner set at an interior fitted binomial
proportion. A strict witness restricts the global comparison to a finite
population box. Otherwise the set is exactly the prefix equalities and
every compatible point on an identically equal tail ray. -/
theorem held_draw_interior_winners {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    ((∃ v ∈ D, (F : ℝ) < L v) → ∃ C : ℕ,
      let box := {v ∈ D | v.2 < C}
      ∀ v, (v ∈ D ∧ IsMaxOn L D v) ↔ v ∈ box ∧ IsMaxOn L box v) ∧
    ((∀ v ∈ D, L v ≤ (F : ℝ)) → ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      ∀ v ∈ D, IsMaxOn L D v ↔
        (v.2 < B ∧ L v = (F : ℝ)) ∨
        (B ≤ v.2 ∧ ∃ e ∈ E, (d : ℤ) * v.1 = (a : ℤ) * v.2 + e ∧ P e = 0)) := by
  intro a d E D L F
  have hSr : (a : ℝ) = ∑ i, (x i : ℝ) := by simp [a]
  have htotal : (d : ℝ) = (∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i) := by
    simp [d, Finset.sum_sub_distrib, nsmul_eq_mul]
  let p : ℝ := (a : ℝ) / d
  have H := binomial_face_max x hx hS hT
  change (∑ i, (x i : ℝ)) / ((∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i)) ∈ Icc 0 1 ∧ _ at H
  rw [← htotal, ← hSr] at H
  have hF : (F : ℝ) = ∏ i, Discrete4Hyper.binomialFace k (x i) p := by
    unfold F Discrete4Hyper.binomialFace
    push_cast
    rfl
  constructor
  · rintro ⟨w, hw, hFw⟩
    have hH q (hq : q ∈ Icc (0 : ℝ) 1) :
        (∏ i, Discrete4Hyper.binomialFace k (x i) q) < L w := by
      have Hq : (∏ i, Discrete4Hyper.binomialFace k (x i) q) ≤ (F : ℝ) := by
        rw [hF]
        exact H.2 hq
      exact Hq.trans_lt hFw
    obtain ⟨C, hC⟩ := held_draw_cutoff hh k x hH
    let box := {v ∈ D | v.2 < C}
    have hwC : w ∈ box := ⟨hw, lt_of_not_ge fun h ↦
      (lt_irrefl _ (hC w.2 h w.1 hw.2.2))⟩
    refine ⟨C, fun v ↦ ⟨?_, ?_⟩⟩
    · rintro ⟨hv, hm⟩
      have hvC : v.2 < C := lt_of_not_ge fun h ↦
        ((hC v.2 h v.1 hv.2.2).not_ge (hm hw))
      exact ⟨⟨hv, hvC⟩, fun q hq ↦ hm hq.1⟩
    · rintro ⟨hv, hm⟩
      refine ⟨hv.1, fun q hq ↦ ?_⟩
      by_cases he : q.2 < C
      · exact hm ⟨hq, he⟩
      · exact (hC q.2 (le_of_not_gt he) q.1 hq.2.2).le.trans (hm hwC)
  · intro hbound
    obtain ⟨B, P, hB, hP⟩ := held_draw_affine_tail_table hh k x hx hS hT F
    obtain ⟨A, N, _, hcomp, _, hlim⟩ :=
      Discrete4Hyper.held_draw_reaches_binomial_face x k hx H.1
    rw [← hF] at hlim
    refine ⟨B, P, fun v hv ↦ ⟨?_, ?_⟩⟩
    · intro hm
      have he : L v = (F : ℝ) := (hbound v hv).antisymm (le_of_tendsto' hlim fun t ↦
        hm (a := (A t, N t)) ⟨(hcomp t ⟨0, hh⟩).1, (hcomp t ⟨0, hh⟩).2.1, hcomp t⟩)
      by_cases hN : v.2 < B
      · exact Or.inl ⟨hN, he⟩
      have hN' : B ≤ v.2 := le_of_not_gt hN
      have hd : v.1 ∈ compositionDomain v.2 k x :=
        ⟨hv.1, fun i ↦ (hv.2.2 i).2.2⟩
      have hMax : IsMaxOn (fun m ↦ likelihood m v.2 k x) (compositionDomain v.2 k x) v.1 :=
        fun m hD ↦ hm (a := (m, v.2)) ⟨hD.1, hv.2.1,
          fun i ↦ ⟨hD.1, hv.2.1, (hD.2 i).1, (hD.2 i).2⟩⟩
      obtain ⟨e, he', hrel⟩ := hB v.2 hN' v.1 hd hMax
      exact Or.inr ⟨hN', e, he', hrel, (hP v.2 hN' v.1 e he' hrel hv.2.2).2.1.mp he⟩
    · intro he
      have hech : L v = (F : ℝ) := by
        rcases he with ⟨_, hvF⟩ | ⟨hN, e, he, hrel, hz⟩
        · exact hvF
        · exact (hP v.2 hN v.1 e he hrel hv.2.2).2.1.mpr hz
      exact fun q hq ↦ (hbound q hq).trans_eq hech.symm

/-- Constant data have exactly the saturated urns in the fixed-draw chart,
including the entire zero-draw set. -/
theorem held_draw_constant_winners {h : ℕ} (hh : 0 < h) {c k : ℕ} (hc : c ≤ k) :
    let x := fun _ : Fin h ↦ c
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    ∀ v ∈ D, IsMaxOn L D v ↔
      (c = 0 ∧ (v.1 = 0 ∨ k = 0)) ∨ (v.1 = v.2 ∧ k = c) ∨ (k = v.2 ∧ v.1 = c) := by
  intro x D L v hv
  have hw : (c, k) ∈ D := ⟨hc, le_rfl, fun i ↦ ⟨hc, le_rfl, le_rfl, hc, by change c + k ≤ k + c; omega⟩⟩
  have hL : L (c, k) = 1 := (Discrete4Hyper.constant_saturated_iff hh).mpr
    ⟨hc, le_rfl, Or.inr (Or.inr ⟨rfl, rfl⟩)⟩
  have H := Discrete4Hyper.constant_attains_saturated (h := h) c
  have hbound (q : ℕ × ℕ) : L q ≤ 1 :=
    (H.2 q.1 q.2 k).trans_eq H.1
  rw [show IsMaxOn L D v ↔ L v = 1 from
    ⟨fun hm ↦ (hbound v).antisymm (hL ▸ hm hw),
      fun he q _ ↦ (hbound q).trans_eq he.symm⟩]
  rw [Discrete4Hyper.constant_saturated_iff hh]
  exact ⟨fun h ↦ h.2.2, fun h ↦ ⟨hv.1, hv.2.1, h⟩⟩

private theorem binary_pair_draw_winners :
    (let y : Fin 2 → ℕ := fun i ↦ i.val
     let D₀ := {v : ℕ × ℕ | ∀ i, Compatible v.1 v.2 1 (y i)}
     let L₀ := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 1 y
     (∀ v ∈ D₀, IsMaxOn L₀ D₀ v ↔ 2 * v.1 = v.2) ∧
     ∀ d > 0, (d, 2 * d) ∈ D₀ ∧ IsMaxOn L₀ D₀ (d, 2 * d)) := by
  intro y D L
  have E := Base7bHyperBinary.binary_pair_even_population
  have H := E.2 1 Nat.one_pos
  have h₁ : (1, 2) ∈ D := H.1
  have hrestriction (a n : ℕ)
      (hw : IsMaxOn (fun v : ℕ × ℕ × ℕ ↦ likelihood v.1 v.2.1 v.2.2 y)
        {v | ∀ i, Compatible v.1 v.2.1 v.2.2 (y i)} (a, n, 1)) :
      IsMaxOn L D (a, n) := fun q hq ↦ hw (a := (q.1, q.2, 1)) hq
  refine ⟨?_, fun d hd ↦ ?_⟩
  · rintro ⟨m, n⟩ hm
    constructor
    · intro hmax
      have hw : IsMaxOn (fun v : ℕ × ℕ × ℕ ↦ likelihood v.1 v.2.1 v.2.2 y)
          {v | ∀ i, Compatible v.1 v.2.1 v.2.2 (y i)} (m, n, 1) :=
        fun q hq ↦ (show likelihood q.1 q.2.1 q.2.2 y ≤ likelihood 1 2 1 y from H.2.1 hq).trans
          (hmax h₁)
      rcases (E.1 (m, n, 1) hm).mp hw with h | h
      · exact h.2
      · have hm₁ : m = 1 := h.1
        have hn₂ : 2 * 1 = n := h.2
        omega
    · intro h
      exact hrestriction m n ((E.1 (m, n, 1) hm).mpr (Or.inl ⟨rfl, h⟩))
  · have h := E.2 d hd
    exact ⟨h.1, hrestriction d (2 * d) h.2.1⟩

private theorem binary_pair_mark_winners :
    (let y : Fin 2 → ℕ := fun i ↦ i.val
     let D₀ := {v : ℕ × ℕ | ∀ i, Compatible 1 v.2 v.1 (y i)}
     let L₀ := fun v : ℕ × ℕ ↦ likelihood 1 v.2 v.1 y
     (∀ v ∈ D₀, IsMaxOn L₀ D₀ v ↔ 2 * v.1 = v.2) ∧
     ∀ d > 0, (d, 2 * d) ∈ D₀ ∧ IsMaxOn L₀ D₀ (d, 2 * d)) := by
  intro y D L
  let D' := {v : ℕ × ℕ | ∀ i, Compatible v.1 v.2 1 (y i)}
  let L' := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 1 y
  have hD : D = D' := by
    ext v
    constructor <;> intro hc i <;> have H := hc i <;> unfold Compatible at H ⊢ <;> omega
  have hL : L = L' := funext fun v ↦ likelihood_swap _ _ _ _
  rw [hD, hL]
  exact binary_pair_draw_winners

/-- The complete fixed-draw maximizing set, in deterministic branches
and in the finite-prefix and affine-ray comparison. -/
theorem held_draw_winners {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    (∀ c : ℕ, (∀ i, x i = c) → ∀ v ∈ D, IsMaxOn L D v ↔
      (c = 0 ∧ (v.1 = 0 ∨ k = 0)) ∨ (v.1 = v.2 ∧ k = c) ∨ (k = v.2 ∧ v.1 = c)) ∧
    ((∃ i j, x i ≠ x j) →
      ((∃ v ∈ D, (F : ℝ) < L v) → ∃ C : ℕ,
        let box := {v ∈ D | v.2 < C}
        ∀ v, (v ∈ D ∧ IsMaxOn L D v) ↔ v ∈ box ∧ IsMaxOn L box v) ∧
      ((∀ v ∈ D, L v ≤ (F : ℝ)) → ∃ (B : ℕ) (P : ℤ → ℚ[X]),
        ∀ v ∈ D, IsMaxOn L D v ↔
          (v.2 < B ∧ L v = (F : ℝ)) ∨
          (B ≤ v.2 ∧ ∃ e ∈ E, (d : ℤ) * v.1 = (a : ℤ) * v.2 + e ∧ P e = 0))) ∧
    (let y : Fin 2 → ℕ := fun i ↦ i.val
     let D₀ := {v : ℕ × ℕ | ∀ i, Compatible v.1 v.2 1 (y i)}
     let L₀ := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 1 y
     (∀ v ∈ D₀, IsMaxOn L₀ D₀ v ↔ 2 * v.1 = v.2) ∧
     ∀ d > 0, (d, 2 * d) ∈ D₀ ∧ IsMaxOn L₀ D₀ (d, 2 * d)) := by
  intro a d E D L F
  refine ⟨?_, ?_, binary_pair_draw_winners⟩
  · intro c hc
    have hk : c ≤ k := hc ⟨0, hh⟩ ▸ hx ⟨0, hh⟩
    have he : x = fun _ ↦ c := funext hc
    subst x
    exact held_draw_constant_winners hh hk
  · rintro ⟨i, j, hij⟩
    have hS : 0 < ∑ i, (x i : ℝ) := by
      have H : ∃ i, 0 < x i := by
        by_contra! h
        exact hij (by have := h i; have := h j; omega)
      obtain ⟨i, hi⟩ := H
      exact (Nat.cast_pos.mpr hi).trans_le (Finset.single_le_sum
        (fun i _ ↦ Nat.cast_nonneg (x i)) (Finset.mem_univ i))
    have hT : 0 < ∑ i, ((k : ℝ) - x i) := by
      have H : ∃ i, x i < k := by
        by_contra! h
        exact hij ((le_antisymm (hx i) (h i)).trans (le_antisymm (hx j) (h j)).symm)
      obtain ⟨j, hj⟩ := H
      exact (sub_pos.mpr (by exact_mod_cast hj : (x j : ℝ) < k)).trans_le
        (Finset.single_le_sum (f := fun i ↦ ((k : ℝ) - x i))
          (fun i _ ↦ sub_nonneg.mpr (by exact_mod_cast hx i)) (Finset.mem_univ j))
    exact held_draw_interior_winners hh k x hx hS hT

/-- The complete fixed-mark maximizing set, including finite equality and
all equality rays, is the draw/mark exchange of the fixed-draw result. -/
theorem held_mark_winners {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible k v.2 v.1 (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood k v.2 v.1 x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    (∀ c : ℕ, (∀ i, x i = c) → ∀ v ∈ D, IsMaxOn L D v ↔
      (c = 0 ∧ (v.1 = 0 ∨ k = 0)) ∨ (v.1 = v.2 ∧ k = c) ∨ (k = v.2 ∧ v.1 = c)) ∧
    ((∃ i j, x i ≠ x j) →
      ((∃ v ∈ D, (F : ℝ) < L v) → ∃ C : ℕ,
        let box := {v ∈ D | v.2 < C}
        ∀ v, (v ∈ D ∧ IsMaxOn L D v) ↔ v ∈ box ∧ IsMaxOn L box v) ∧
      ((∀ v ∈ D, L v ≤ (F : ℝ)) → ∃ (B : ℕ) (P : ℤ → ℚ[X]),
        ∀ v ∈ D, IsMaxOn L D v ↔
          (v.2 < B ∧ L v = (F : ℝ)) ∨
          (B ≤ v.2 ∧ ∃ e ∈ E, (d : ℤ) * v.1 = (a : ℤ) * v.2 + e ∧ P e = 0))) ∧
    (let y : Fin 2 → ℕ := fun i ↦ i.val
     let D₀ := {v : ℕ × ℕ | ∀ i, Compatible 1 v.2 v.1 (y i)}
     let L₀ := fun v : ℕ × ℕ ↦ likelihood 1 v.2 v.1 y
     (∀ v ∈ D₀, IsMaxOn L₀ D₀ v ↔ 2 * v.1 = v.2) ∧
     ∀ d > 0, (d, 2 * d) ∈ D₀ ∧ IsMaxOn L₀ D₀ (d, 2 * d)) := by
  intro a d E D L F
  let D' := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
  let L' := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
  have hD : D = D' := by
    ext v
    change (_ ∧ _ ∧ ∀ i, Compatible k v.2 v.1 (x i)) ↔
      (_ ∧ _ ∧ ∀ i, Compatible v.1 v.2 k (x i))
    constructor <;> rintro ⟨ha, hb, hc⟩ <;> refine ⟨ha, hb, fun i ↦ ?_⟩
    all_goals
      have H := hc i
      unfold Compatible at H ⊢
      omega
  have hL : L = L' := funext fun v ↦ likelihood_swap _ _ _ _
  rw [hD, hL]
  have H := held_draw_winners hh k x hx
  exact ⟨H.1, H.2.1, binary_pair_mark_winners⟩

end UnivariateML.Base7bHyperResidue

end
