module

public import UnivariateML.Family.Hypergeometric
public import Mathlib.Data.Nat.Choose.Vandermonde

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Hypergeometric constant samples and fixed population

The design counts are the marked population count (K) and the draw count
(k). The likelihood uses the existing four-cell support convention.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators Classical

namespace UnivariateML.Discrete4Hyper

open Hypergeometric

private theorem mass_nonneg' (K N k c : ℕ) : 0 ≤ mass K N k c := by
  by_cases hc : Compatible K N k c
  · rw [mass_choose hc]
    positivity
  · simp [mass, hc]

private theorem choose_term_le {K N k c : ℕ} (hc : Compatible K N k c) :
    K.choose c * (N-K).choose (k-c) ≤ N.choose k := by
  have hm : (c, k-c) ∈ Finset.antidiagonal k := by
    rw [Finset.mem_antidiagonal]
    have := hc.2.2.2.1
    omega
  have he : K + (N-K) = N := Nat.add_sub_of_le hc.1
  conv_rhs => rw [← he, Nat.add_choose_eq]
  exact Finset.single_le_sum
    (f := fun ij : ℕ × ℕ ↦ K.choose ij.1 * (N-K).choose ij.2)
    (fun _ _ ↦ Nat.zero_le _) hm

private theorem mass_le_one' (K N k c : ℕ) : mass K N k c ≤ 1 := by
  by_cases hc : Compatible K N k c
  · rw [mass_choose hc, div_le_iff₀ (Nat.cast_pos.mpr (Nat.choose_pos hc.2.1)), one_mul]
    exact_mod_cast choose_term_le hc
  · rw [mass, ite_eq_right hc]
    exact zero_le_one

private theorem likelihood_le_one' {h : ℕ} (x : Fin h → ℕ) (K N k : ℕ) :
    likelihood K N k x ≤ 1 := by
  unfold likelihood
  exact Finset.prod_le_one₀ (fun i _ ↦ mass_nonneg' K N k (x i))
    (fun i _ ↦ mass_le_one' K N k (x i))

/-- A constant sample attains the saturated likelihood, including at count zero. -/
theorem constant_attains_saturated {h : ℕ} (c : ℕ) :
    likelihood c c c (fun _ : Fin h ↦ c) = 1 ∧
      ∀ K N k, likelihood K N k (fun _ : Fin h ↦ c) ≤
        likelihood c c c (fun _ : Fin h ↦ c) := by
  have hm : mass c c c c = 1 := by
    rw [mass_choose (show Compatible c c c c from ⟨le_rfl, le_rfl, le_rfl, le_rfl, le_rfl⟩)]
    simp
  have hl : likelihood c c c (fun _ : Fin h ↦ c) = 1 := by simp [likelihood, hm]
  refine ⟨hl, fun K N k ↦ ?_⟩
  rw [hl]
  exact likelihood_le_one' _ K N k

private theorem mass_lt_one_of_other {K N k c y : ℕ} (hc : Compatible K N k c)
    (hy : Compatible K N k y) (hne : y ≠ c) : mass K N k c < 1 := by
  let f (ij : ℕ × ℕ) := K.choose ij.1 * (N-K).choose ij.2
  let a := (c, k-c)
  let b := (y, k-y)
  let s := Finset.antidiagonal k
  have ha : a ∈ s := Finset.mem_antidiagonal.mpr (Nat.add_sub_of_le hc.2.2.2.1)
  have hb : b ∈ s := Finset.mem_antidiagonal.mpr (Nat.add_sub_of_le hy.2.2.2.1)
  have hab : b ≠ a := fun h ↦ hne (congrArg Prod.fst h)
  have hpos : 0 < f b := Nat.mul_pos (Nat.choose_pos hy.2.2.1)
    (Nat.choose_pos (by change k-y ≤ N-K; have := hy.1; have := hy.2.2.2.2; omega))
  have hs : f b ≤ ∑ ij ∈ s.erase a, f ij :=
    Finset.single_le_sum (fun _ _ ↦ Nat.zero_le _) (Finset.mem_erase.mpr ⟨hab, hb⟩)
  have he : (∑ ij ∈ s.erase a, f ij) + f a = N.choose k := by
    rw [Finset.sum_erase_add _ _ ha]
    change (∑ ij ∈ Finset.antidiagonal k, f ij) = N.choose k
    rw [← Nat.add_choose_eq, Nat.add_sub_of_le hc.1]
  have hlt : f a < N.choose k := by omega
  rw [mass_choose hc, div_lt_iff₀ (Nat.cast_pos.mpr (Nat.choose_pos hc.2.1)), one_mul]
  exact_mod_cast hlt

private theorem full_marked_mass {N k : ℕ} (hk : k ≤ N) : mass N N k k = 1 := by
  rw [mass_choose (show Compatible N N k k from ⟨le_rfl, hk, hk, le_rfl, le_rfl⟩)]
  simp [(Nat.cast_ne_zero.mpr (Nat.choose_pos hk).ne' : (N.choose k : ℝ) ≠ 0)]

private theorem no_marks_mass {N k : ℕ} (hk : k ≤ N) : mass 0 N k 0 = 1 := by
  rw [mass_choose (show Compatible 0 N k 0 from ⟨Nat.zero_le _, hk, le_rfl,
    Nat.zero_le _, by simpa using hk⟩)]
  simp [(Nat.cast_ne_zero.mpr (Nat.choose_pos hk).ne' : (N.choose k : ℝ) ≠ 0)]

/-- Every saturated urn for a nonempty constant sample. A zero sample permits
either design count to vanish. At a positive count one design margin occupies
the whole population and the other equals the observed count. -/
theorem constant_saturated_iff {h K N k c : ℕ} (hh : 0 < h) :
    likelihood K N k (fun _ : Fin h ↦ c) = 1 ↔
      K ≤ N ∧ k ≤ N ∧
        ((c = 0 ∧ (K = 0 ∨ k = 0)) ∨ (K = N ∧ k = c) ∨ (k = N ∧ K = c)) := by
  have hm : likelihood K N k (fun _ : Fin h ↦ c) = 1 ↔ mass K N k c = 1 := by
    rw [likelihood, Finset.prod_const, Finset.card_univ, Fintype.card_fin]
    constructor
    · intro H
      by_contra hne
      have hlt := lt_of_le_of_ne (mass_le_one' K N k c) hne
      exact (ne_of_lt (pow_lt_one₀ (mass_nonneg' K N k c) hlt (Nat.ne_of_gt hh))) H
    · intro H
      simp [H]
  rw [hm]
  constructor
  · intro H
    have hc : Compatible K N k c := by
      by_contra h
      simp [mass, h] at H
    have hy {y : ℕ} (hy : Compatible K N k y) : y = c := by
      by_contra hne
      exact (ne_of_lt (mass_lt_one_of_other hc hy hne)) H
    have ha : Compatible K N k (K+k-N) := by
      unfold Compatible
      have := hc.1
      have := hc.2.1
      omega
    have hb : Compatible K N k (min K k) := by
      unfold Compatible
      have := hc.1
      have := hc.2.1
      omega
    refine ⟨hc.1, hc.2.1, ?_⟩
    have H₁ := hy ha
    have H₂ := hy hb
    omega
  · rintro ⟨hK, hk, ⟨rfl, hK₀ | hk₀⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩⟩
    · simpa [hK₀] using no_marks_mass hk
    · rw [mass_swap, hk₀]
      exact no_marks_mass hK
    · exact full_marked_mass hk
    · rw [mass_swap]
      exact full_marked_mass hK

/-- A fixed population admits the observations exactly when twice every
observed count is at most the population plus every observed count. -/
theorem fixed_population_feasible_iff {h N : ℕ} (x : Fin h → ℕ) :
    (∃ K k, K ≤ N ∧ k ≤ N ∧ ∀ i, Compatible K N k (x i)) ↔
      ∀ i j, 2 * x i ≤ N + x j := by
  constructor
  · rintro ⟨K, k, _, _, hs⟩ i j
    have hi := hs i
    have hj := hs j
    unfold Compatible at hi hj
    omega
  · intro H
    let u := Finset.univ.sup x
    have hu (i : Fin h) : x i ≤ u := Finset.le_sup (Finset.mem_univ i)
    have huN : u ≤ N := Finset.sup_le fun i _ ↦ by have := H i i; omega
    have ht (j : Fin h) : u + u ≤ N + x j := by
      have h₁ : ∀ i, x i ≤ N + x j - x i := fun i ↦ by have := H i j; omega
      have h₂ : ∀ i, x i + u ≤ N + x j := by
        intro i
        have hj : u ≤ N + x j - x i := Finset.sup_le fun l _ ↦ by
          have := H i j
          have := H l j
          omega
        have := h₁ i
        omega
      have : u ≤ N + x j - u := Finset.sup_le fun i _ ↦ by have := h₂ i; omega
      have := huN
      omega
    exact ⟨u, u, huN, huN, fun i ↦ ⟨huN, huN, hu i, hu i, ht i⟩⟩

/-- At fixed population the compatible design grid is finite, so every
nonempty grid attains its maximum. The inequality includes every tied design. -/
theorem fixed_population_attainment {h N : ℕ} (x : Fin h → ℕ) :
    (({p : ℕ × ℕ | p.1 ≤ N ∧ p.2 ≤ N ∧
      ∀ i, Compatible p.1 N p.2 (x i)} : Set (ℕ × ℕ))).Finite ∧
      ((∃ K k, K ≤ N ∧ k ≤ N ∧ (∀ i, Compatible K N k (x i)) ∧
        ∀ J j, J ≤ N → j ≤ N → (∀ i, Compatible J N j (x i)) →
          likelihood J N j x ≤ likelihood K N k x) ↔
        ∀ i j, 2 * x i ≤ N + x j) := by
  let grid := ((Finset.range (N+1)).product (Finset.range (N+1))).filter
    fun p ↦ ∀ i, Compatible p.1 N p.2 (x i)
  have hm (a b : ℕ) : (a, b) ∈ grid ↔
      a ≤ N ∧ b ≤ N ∧ ∀ i, Compatible a N b (x i) := by
    simp [grid, and_assoc]
  refine ⟨grid.finite_toSet.subset ?_, ?_⟩
  · rintro ⟨a, b⟩ h
    exact (hm a b).mpr h
  constructor
  · rintro ⟨a, b, ha, hb, hs, _⟩
    exact (fixed_population_feasible_iff x).mp ⟨a, b, ha, hb, hs⟩
  · intro H
    obtain ⟨a, b, ha, hb, hs⟩ := (fixed_population_feasible_iff x).mpr H
    obtain ⟨⟨K, k⟩, hp, ht⟩ := grid.exists_max_image
      (fun p ↦ likelihood p.1 N p.2 x) ⟨(a,b), (hm a b).mpr ⟨ha,hb,hs⟩⟩
    obtain ⟨hK, hk, hc⟩ := (hm K k).mp hp
    exact ⟨K, k, hK, hk, hc, fun J j hJ hj hS ↦ ht (J,j) ((hm J j).mpr ⟨hJ,hj,hS⟩)⟩

/-- The binomial face with the draw count held fixed. -/
def binomialFace (k c : ℕ) (p : ℝ) : ℝ :=
  (k.choose c : ℝ) * p^c * (1-p)^(k-c)

private theorem mass_falling {K N k c : ℕ} (hK : K ≤ N) (hk : k ≤ N) (hc : c ≤ k) :
    mass K N k c =
      (k.choose c : ℝ) * K.descFactorial c * (N-K).descFactorial (k-c) /
        N.descFactorial k := by
  by_cases h : Compatible K N k c
  · rw [mass_choose h]
    have hd (a b : ℕ) : (a.descFactorial b : ℝ) = b.factorial * (a.choose b : ℝ) := by
      rw [Nat.descFactorial_eq_factorial_mul_choose, Nat.cast_mul]
    rw [hd, hd, hd]
    have e : (k.choose c : ℝ) * c.factorial * (k-c).factorial = k.factorial := by
      exact_mod_cast Nat.choose_mul_factorial_mul_factorial hc
    have hn : (N.choose k : ℝ) ≠ 0 := (Nat.cast_pos.mpr (Nat.choose_pos hk)).ne'
    field_simp [hn]
    linear_combination -((K.choose c : ℝ) * ((N-K).choose (k-c) : ℝ)) * e
  · rw [mass, ite_eq_right h]
    by_cases hcK : c ≤ K
    · have hs : N-K < k-c := by
        unfold Compatible at h
        omega
      simp [Nat.descFactorial_of_lt hs]
    · simp [Nat.descFactorial_of_lt (Nat.lt_of_not_ge hcK)]

private theorem normalized_falling {A B : ℕ → ℕ} {q : ℝ}
    (hB : Tendsto B atTop atTop)
    (hq : Tendsto (fun t ↦ (A t : ℝ) / B t) atTop (𝓝 q)) (hq₀ : 0 ≤ q) (d : ℕ) :
    Tendsto (fun t ↦ (A t).descFactorial d / (B t : ℝ)^d) atTop (𝓝 (q^d)) := by
  have hinv : Tendsto (fun t ↦ (B t : ℝ)⁻¹) atTop (𝓝 0) :=
    tendsto_inv_atTop_zero.comp (tendsto_natCast_atTop_atTop.comp hB)
  have hfactor (i : ℕ) :
      Tendsto (fun t ↦ ((A t-i : ℕ) : ℝ) / B t) atTop (𝓝 q) := by
    have ht := (hq.sub (hinv.const_mul (i : ℝ))).max
      (tendsto_const_nhds (x := (0 : ℝ)))
    rw [mul_zero, sub_zero, max_eq_left hq₀] at ht
    apply ht.congr'
    filter_upwards [hB.eventually (eventually_ge_atTop 1)] with t ht
    have hb : 0 < (B t : ℝ) := Nat.cast_pos.mpr ht
    rw [← div_eq_mul_inv, ← sub_div]
    by_cases hi : i ≤ A t
    · rw [Nat.cast_sub hi, max_eq_left (div_nonneg (sub_nonneg.mpr (by exact_mod_cast hi)) hb.le)]
    · have hi' : (A t : ℝ) ≤ i := by exact_mod_cast (Nat.le_of_not_ge hi)
      rw [max_eq_right (div_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr hi') hb.le)]
      simp [Nat.sub_eq_zero_of_le (Nat.le_of_not_ge hi)]
  simpa only [Nat.descFactorial_eq_prod_range, Nat.cast_prod, Finset.prod_div_distrib,
    Finset.prod_const, Finset.card_range] using
    (tendsto_finsetProd (Finset.range d) fun i _ ↦ hfactor i)

private theorem mass_binomial_limit {A N : ℕ → ℕ} {p : ℝ} (hp : p ∈ Icc 0 1)
    (hN : Tendsto N atTop atTop) (hA : ∀ᶠ t in atTop, A t ≤ N t)
    (hAN : Tendsto (fun t ↦ (A t : ℝ) / N t) atTop (𝓝 p))
    (k c : ℕ) : Tendsto (fun t ↦ mass (A t) (N t) k c) atTop (𝓝 (binomialFace k c p)) := by
  by_cases hc : c ≤ k
  · have hNN : Tendsto (fun t ↦ (N t : ℝ) / N t) atTop (𝓝 1) := by
      apply tendsto_const_nhds.congr'
      filter_upwards [hN.eventually (eventually_ge_atTop 1)] with t ht
      exact (div_self (Nat.cast_pos.mpr ht).ne').symm
    have hrest : Tendsto (fun t ↦ ((N t-A t : ℕ) : ℝ) / N t) atTop (𝓝 (1-p)) := by
      apply (hNN.sub hAN).congr'
      filter_upwards [hA] with t ht
      rw [Nat.cast_sub ht, sub_div]
    have hnum := ((normalized_falling hN hAN hp.1 c).const_mul (k.choose c : ℝ)).mul
      (normalized_falling hN hrest (sub_nonneg.mpr hp.2) (k-c))
    have hden := normalized_falling hN hNN zero_le_one k
    rw [one_pow] at hden
    have ht := hnum.div hden one_ne_zero
    rw [div_one] at ht
    apply ht.congr'
    filter_upwards [hA, hN.eventually (eventually_ge_atTop (max k 1))] with t ha hb
    have hk : k ≤ N t := (le_max_left _ _).trans hb
    have hpos : 0 < (N t : ℝ) := Nat.cast_pos.mpr ((le_max_right _ _).trans hb)
    have hD : 0 < ((N t).descFactorial k : ℝ) := Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hk)
    have he : (N t : ℝ)^k = (N t : ℝ)^c * (N t : ℝ)^(k-c) := by
      rw [← pow_add, Nat.add_sub_of_le hc]
    simp only [Pi.div_apply]
    rw [mass_falling ha hk hc, he]
    field_simp [hD.ne', hpos.ne']
  · have hm (t : ℕ) : mass (A t) (N t) k c = 0 :=
      by rw [mass, ite_eq_right (fun h ↦ hc h.2.2.2.1)]
    simp [hm, binomialFace, Nat.choose_eq_zero_of_lt (Nat.lt_of_not_ge hc)]

/-- With fixed draws and a convergent marked fraction, the sample likelihood
converges to the corresponding binomial face, including both point masses. -/
theorem held_draw_binomial_limit {h : ℕ} (x : Fin h → ℕ) (k : ℕ)
    {A N : ℕ → ℕ} {p : ℝ} (hp : p ∈ Icc 0 1) (hN : Tendsto N atTop atTop)
    (hA : ∀ᶠ t in atTop, A t ≤ N t)
    (hAN : Tendsto (fun t ↦ (A t : ℝ) / N t) atTop (𝓝 p)) :
    Tendsto (fun t ↦ likelihood (A t) (N t) k x) atTop
      (𝓝 (∏ i, binomialFace k (x i) p)) := by
  exact tendsto_finsetProd _ fun i _ ↦ mass_binomial_limit hp hN hA hAN k (x i)

/-- Swapping the two design counts gives the fixed-composition binomial limit. -/
theorem held_mark_binomial_limit {h : ℕ} (x : Fin h → ℕ) (K : ℕ)
    {A N : ℕ → ℕ} {p : ℝ} (hp : p ∈ Icc 0 1) (hN : Tendsto N atTop atTop)
    (hA : ∀ᶠ t in atTop, A t ≤ N t)
    (hAN : Tendsto (fun t ↦ (A t : ℝ) / N t) atTop (𝓝 p)) :
    Tendsto (fun t ↦ likelihood K (N t) (A t) x) atTop
      (𝓝 (∏ i, binomialFace K (x i) p)) :=
  (held_draw_binomial_limit x K hp hN hA hAN).congr
    (fun t ↦ likelihood_swap (A t) (N t) K x)

private noncomputable def correction (N k : ℕ) : ℝ := (N : ℝ)^k / N.descFactorial k

private theorem mass_bound {K N k c : ℕ} (hK : K ≤ N) (hk : k ≤ N) (hN : 0 < N) :
    mass K N k c ≤ binomialFace k c ((K : ℝ)/N) * correction N k := by
  have hNr : 0 < (N : ℝ) := Nat.cast_pos.mpr hN
  have hD : 0 < (N.descFactorial k : ℝ) := Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hk)
  by_cases hc : c ≤ k
  · rw [mass_falling hK hk hc, correction, binomialFace]
    have he : (N : ℝ)^k = (N : ℝ)^c * (N : ℝ)^(k-c) := by
      rw [← pow_add, Nat.add_sub_of_le hc]
    have ha : 1 - (K : ℝ)/N = ((N-K : ℕ) : ℝ)/N := by
      rw [Nat.cast_sub hK, sub_div, div_self hNr.ne']
    rw [ha, div_pow, div_pow, he]
    have hr (a b : ℝ) : ((k.choose c : ℝ) * (a / (N : ℝ)^c) *
        (b / (N : ℝ)^(k-c))) * ((N : ℝ)^c * (N : ℝ)^(k-c) / N.descFactorial k) =
        (k.choose c : ℝ) * a * b / N.descFactorial k := by
      field_simp [hNr.ne', hD.ne']
    rw [hr]
    apply div_le_div_of_nonneg_right _ hD.le
    have hcast (a b : ℕ) : (a.descFactorial b : ℝ) ≤ (a : ℝ)^b := by
      exact_mod_cast Nat.descFactorial_le_pow a b
    exact mul_le_mul
      (mul_le_mul_of_nonneg_left (hcast K c) (Nat.cast_nonneg _))
      (hcast (N-K) (k-c)) (Nat.cast_nonneg _) (by positivity)
  · rw [mass, ite_eq_right (fun h ↦ hc h.2.2.2.1)]
    simp [binomialFace, Nat.choose_eq_zero_of_lt (Nat.lt_of_not_ge hc)]

private theorem correction_tendsto (k : ℕ) :
    Tendsto (fun N ↦ correction N k) atTop (𝓝 1) := by
  have hNN : Tendsto (fun N : ℕ ↦ (N : ℝ)/N) atTop (𝓝 1) := by
    apply tendsto_const_nhds.congr'
    filter_upwards [eventually_ge_atTop 1] with N hN
    exact (div_self (Nat.cast_pos.mpr hN).ne').symm
  simpa only [one_pow, inv_one, inv_div, correction, id_eq] using
    (normalized_falling tendsto_id hNN zero_le_one k).inv₀ (by positivity)

private theorem held_draw_cutoff {h : ℕ} (x : Fin h → ℕ) (k : ℕ) {C : ℝ}
    (hC : ∀ p ∈ Icc (0 : ℝ) 1, (∏ i, binomialFace k (x i) p) < C) :
    ∃ B : ℕ, ∀ N ≥ B, ∀ K ≤ N, likelihood K N k x < C := by
  have cont : Continuous (fun p ↦ ∏ i, binomialFace k (x i) p) := by
    unfold binomialFace
    fun_prop
  obtain ⟨p, hp, hmax⟩ := isCompact_Icc.exists_isMaxOn
    (nonempty_Icc.mpr (show (0 : ℝ) ≤ 1 by norm_num)) cont.continuousOn
  let F := ∏ i, binomialFace k (x i) p
  have ht : Tendsto (fun N ↦ F * correction N k ^ h) atTop (𝓝 F) := by
    simpa using ((correction_tendsto k).pow h).const_mul F
  obtain ⟨B, hB⟩ := eventually_atTop.mp (ht.eventually_lt_const (hC p hp))
  refine ⟨max B (max k 1), fun N hN K hK ↦ ?_⟩
  have hk : k ≤ N := (le_max_left _ _).trans ((le_max_right _ _).trans hN)
  have h1 : 1 ≤ N := (le_max_right _ _).trans ((le_max_right _ _).trans hN)
  have hq : (K : ℝ)/N ∈ Icc (0 : ℝ) 1 := ⟨by positivity,
    (div_le_one (Nat.cast_pos.mpr h1)).mpr (by exact_mod_cast hK)⟩
  calc
    likelihood K N k x ≤ ∏ i, binomialFace k (x i) ((K : ℝ)/N) * correction N k :=
      Finset.prod_le_prod₀ (fun i _ ↦ mass_nonneg' K N k (x i))
        (fun i _ ↦ mass_bound hK hk h1)
    _ = (∏ i, binomialFace k (x i) ((K : ℝ)/N)) * correction N k ^ h := by
      rw [Finset.prod_mul_distrib, Finset.prod_const, Finset.card_univ, Fintype.card_fin]
    _ ≤ F * correction N k ^ h :=
      mul_le_mul_of_nonneg_right (hmax hq) (by unfold correction; positivity)
    _ < C := hB N ((le_max_left _ _).trans hN)

/-- With draws held, a finite compatible urn strictly above every
compatible binomial height implies finite attainment. -/
theorem held_draw_strict_witness {h K₀ N₀ : ℕ} (x : Fin h → ℕ) (k : ℕ)
    (hK₀ : K₀ ≤ N₀) (hk₀ : k ≤ N₀) (hx : ∀ i, Compatible K₀ N₀ k (x i))
    (hC : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, binomialFace k (x i) p) < likelihood K₀ N₀ k x) :
    ∃ K N, K ≤ N ∧ k ≤ N ∧ (∀ i, Compatible K N k (x i)) ∧
      ∀ J M, J ≤ M → k ≤ M → (∀ i, Compatible J M k (x i)) →
        likelihood J M k x ≤ likelihood K N k x := by
  obtain ⟨B, hB⟩ := held_draw_cutoff x k hC
  have h₀ : N₀ < B := by
    by_contra hn
    exact (lt_irrefl _) (hB N₀ (Nat.le_of_not_gt hn) K₀ hK₀)
  let grid := ((Finset.range B).product (Finset.range B)).filter
    fun p ↦ p.1 ≤ p.2 ∧ k ≤ p.2 ∧ ∀ i, Compatible p.1 p.2 k (x i)
  have hm (a b : ℕ) : (a,b) ∈ grid ↔
      a < B ∧ b < B ∧ a ≤ b ∧ k ≤ b ∧ ∀ i, Compatible a b k (x i) := by
    simp [grid, and_assoc]
  have hmem := (hm K₀ N₀).mpr ⟨hK₀.trans_lt h₀, h₀, hK₀, hk₀, hx⟩
  obtain ⟨⟨K,N⟩, hKN, hmax⟩ := grid.exists_max_image
    (fun p ↦ likelihood p.1 p.2 k x) ⟨(K₀,N₀), hmem⟩
  obtain ⟨_, _, hK, hk, hs⟩ := (hm K N).mp hKN
  refine ⟨K,N,hK,hk,hs, fun J M hJ hM hJM ↦ ?_⟩
  by_cases ht : M < B
  · exact hmax (J,M) ((hm J M).mpr ⟨hJ.trans_lt ht,ht,hJ,hM,hJM⟩)
  · exact (hB M (Nat.le_of_not_gt ht) J hJ).le.trans (hmax (K₀,N₀) hmem)

/-- Fixed composition inherits the finite attainment criterion by symmetry. -/
theorem held_mark_strict_witness {h k₀ N₀ : ℕ} (x : Fin h → ℕ) (K : ℕ)
    (hK₀ : K ≤ N₀) (hk₀ : k₀ ≤ N₀) (hx : ∀ i, Compatible K N₀ k₀ (x i))
    (hC : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, binomialFace K (x i) p) < likelihood K N₀ k₀ x) :
    ∃ k N, k ≤ N ∧ K ≤ N ∧ (∀ i, Compatible K N k (x i)) ∧
      ∀ j M, j ≤ M → K ≤ M → (∀ i, Compatible K M j (x i)) →
        likelihood K M j x ≤ likelihood K N k x := by
  have swap {a b c z : ℕ} : Compatible a b c z → Compatible c b a z := by
    unfold Compatible
    omega
  obtain ⟨k,N,hk,hK,hs,hm⟩ := held_draw_strict_witness x K hk₀ hK₀
    (fun i ↦ swap (hx i)) (by simpa only [likelihood_swap k₀ N₀ K x] using hC)
  refine ⟨k,N,hk,hK,fun i ↦ swap (hs i), fun j M hj hM hJM ↦ ?_⟩
  rw [likelihood_swap K M j, likelihood_swap K N k]
  exact hm j M hj hM (fun i ↦ swap (hJM i))

/-- Each binomial probability in the closed unit interval is reached
by a sequence of compatible urns, including both degenerate probabilities. -/
theorem held_draw_reaches_binomial_face {h : ℕ} (x : Fin h → ℕ) (k : ℕ)
    (hx : ∀ i, x i ≤ k) {p : ℝ} (hp : p ∈ Icc 0 1) :
    ∃ A N : ℕ → ℕ, Tendsto N atTop atTop ∧
      (∀ t i, Compatible (A t) (N t) k (x i)) ∧
      Tendsto (fun t ↦ (A t : ℝ)/N t) atTop (𝓝 p) ∧
      Tendsto (fun t ↦ likelihood (A t) (N t) k x) atTop
        (𝓝 (∏ i, binomialFace k (x i) p)) := by
  let A (t : ℕ) := k + ⌊p*(t : ℝ)⌋₊
  let N (t : ℕ) := 2*k+t
  have hft (t : ℕ) : ⌊p*(t : ℝ)⌋₊ ≤ t := by
    have hp' : p*(t : ℝ) ≤ t := by
      simpa using mul_le_mul_of_nonneg_right hp.2 (Nat.cast_nonneg t)
    exact_mod_cast (Nat.floor_le (mul_nonneg hp.1 (Nat.cast_nonneg t))).trans hp'
  have hN : Tendsto N atTop atTop := by
    change Tendsto (fun t : ℕ ↦ 2*k+t) atTop atTop
    exact tendsto_atTop_mono (fun t ↦ by change t ≤ 2*k+t; omega : ∀ t : ℕ, id t ≤ 2*k+t) tendsto_id
  have hA (t : ℕ) : A t ≤ N t := by dsimp [A,N]; have := hft t; omega
  have hs (t : ℕ) (i : Fin h) : Compatible (A t) (N t) k (x i) := by
    have hi := hx i
    have ht := hft t
    unfold Compatible A N
    omega
  have hlim (c : ℝ) : Tendsto (fun t : ℕ ↦ (c+p*(t : ℝ))/(2*(k : ℝ)+t)) atTop (𝓝 p) := by
    simpa using tendsto_add_mul_div_add_mul_atTop_nhds c (2*(k : ℝ)) p (d := (1 : ℝ)) one_ne_zero
  have hr : Tendsto (fun t ↦ (A t : ℝ)/N t) atTop (𝓝 p) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' (hlim ((k : ℝ)-1)) (hlim k)
    · filter_upwards [eventually_ge_atTop 1] with t ht
      have hf := Nat.lt_floor_add_one (p*(t : ℝ))
      have hden : 0 ≤ 2*(k : ℝ)+t := by positivity
      simp only [A,N,Nat.cast_add,Nat.cast_mul,Nat.cast_ofNat]
      exact div_le_div_of_nonneg_right (by linarith) hden
    · apply Filter.Eventually.of_forall
      intro t
      have hf := Nat.floor_le (mul_nonneg hp.1 (Nat.cast_nonneg t))
      simp only [A,N,Nat.cast_add,Nat.cast_mul,Nat.cast_ofNat]
      exact div_le_div_of_nonneg_right (add_le_add_right hf _) (by positivity)
  exact ⟨A,N,hN,hs,hr,held_draw_binomial_limit x k hp hN
    (Filter.Eventually.of_forall hA) hr⟩

/-- The marked-count hold has the same compatible closed binomial paths
by exchanging the design counts. -/
theorem held_mark_reaches_binomial_face {h : ℕ} (x : Fin h → ℕ) (K : ℕ)
    (hx : ∀ i, x i ≤ K) {p : ℝ} (hp : p ∈ Icc 0 1) :
    ∃ k N : ℕ → ℕ, Tendsto N atTop atTop ∧
      (∀ t i, Compatible K (N t) (k t) (x i)) ∧
      Tendsto (fun t ↦ (k t : ℝ)/N t) atTop (𝓝 p) ∧
      Tendsto (fun t ↦ likelihood K (N t) (k t) x) atTop
        (𝓝 (∏ i, binomialFace K (x i) p)) := by
  obtain ⟨k,N,hN,hs,hk,hL⟩ := held_draw_reaches_binomial_face x K hx hp
  refine ⟨k,N,hN,fun t i ↦ ?_,hk,hL.congr (fun t ↦ likelihood_swap (k t) (N t) K x)⟩
  have h := hs t i
  unfold Compatible at h ⊢
  omega

private theorem poisson_factorization {K N k c : ℕ} (hc : Compatible K N k c)
    (hN : c < N) :
    mass K N k c =
      ((K.descFactorial c : ℝ) * k.descFactorial c / (c.factorial * (N.descFactorial c : ℝ))) *
      (((N-K).descFactorial (k-c) : ℝ)/((N-c).descFactorial (k-c) : ℝ)) := by
  have hk := hc.2.1
  have hck := hc.2.2.2.1
  have hcN := hN.le
  have hD : 0 < (N.descFactorial c : ℝ) := Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hcN)
  have hE : 0 < ((N-c).descFactorial (k-c) : ℝ) :=
    Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr (by omega))
  have hF : 0 < (c.factorial : ℝ) := by positivity
  have he : (N.descFactorial k : ℝ) =
      ((N-c).descFactorial (k-c) : ℝ) * (N.descFactorial c : ℝ) := by
    exact_mod_cast (Nat.descFactorial_mul_descFactorial (n := N) hck).symm
  have hk' : (k.descFactorial c : ℝ) = (c.factorial : ℝ) * (k.choose c : ℝ) := by
    rw [Nat.descFactorial_eq_factorial_mul_choose,Nat.cast_mul]
  rw [mass_falling hc.1 hk hck,he,hk']
  field_simp [hD.ne',hE.ne',hF.ne']

private theorem depletion_bound {K N k c : ℕ} (hc : Compatible K N k c) (hN : c < N) :
    (((N-K).descFactorial (k-c) : ℝ)/((N-c).descFactorial (k-c) : ℝ)) ≤
      exp (-(((K : ℝ)-c)*((k : ℝ)-c)/(N : ℝ))) := by
  have hK := hc.1
  have hk := hc.2.1
  have hck := hc.2.2.2.1
  have hcK := hc.2.2.1
  have hsup := hc.2.2.2.2
  have hN' : 0 < (N : ℝ) := Nat.cast_pos.mpr (Nat.zero_lt_of_lt hN)
  let R := (Finset.range (k-c))
  let a := ((K : ℝ)-c)/(N : ℝ)
  have ha : 0 ≤ a := div_nonneg (sub_nonneg.mpr (by exact_mod_cast hcK)) hN'.le
  have hfac (i : ℕ) (hi : i ∈ R) :
      (((N-K-i : ℕ) : ℝ)/((N-c-i : ℕ) : ℝ)) ≤ exp (-a) := by
    have hi' : i < k-c := Finset.mem_range.mp hi
    have hiK : i ≤ N-K := by omega
    have hic : i ≤ N-c := by omega
    have hd' : 0 < (N : ℝ)-c-i := by
      have H : (c : ℝ)+i < N := by exact_mod_cast (show c+i < N by omega)
      linarith
    push_cast [Nat.cast_sub hiK,Nat.cast_sub hic,Nat.cast_sub hK,Nat.cast_sub hN.le]
    have he : ((N : ℝ)-K-i)/((N : ℝ)-c-i) = 1-((K : ℝ)-c)/((N : ℝ)-c-i) := by
      field_simp [hd'.ne']
      ring
    rw [he]
    have hle : a ≤ ((K : ℝ)-c)/((N : ℝ)-c-i) :=
      div_le_div_of_nonneg_left (sub_nonneg.mpr (by exact_mod_cast hcK)) hd'
        (by linarith [show (0 : ℝ) ≤ c from Nat.cast_nonneg _, show (0 : ℝ) ≤ i from Nat.cast_nonneg _])
    exact (sub_le_sub_left hle 1).trans (by simpa [sub_eq_add_neg,add_comm] using add_one_le_exp (-a))
  rw [Nat.descFactorial_eq_prod_range,Nat.descFactorial_eq_prod_range,Nat.cast_prod,Nat.cast_prod,
    ← Finset.prod_div_distrib]
  calc
    _ ≤ ∏ _ ∈ R, exp (-a) := Finset.prod_le_prod₀ (fun _ _ ↦ by positivity) hfac
    _ = _ := by
      simp only [R,Finset.prod_const,Finset.card_range,← exp_nat_mul]
      rw [Nat.cast_sub hck]
      congr 1
      unfold a
      ring

/-- The observation-wise Poisson envelope used in the all-free tail
argument. The expression keeps the exact two design depletions. -/
theorem poisson_envelope_mass {K N k c : ℕ} (hc : Compatible K N k c) (hN : c < N) :
    let lam := (K : ℝ)*k/N
    mass K N k c ≤
      lam^c / ((c.factorial : ℝ)*(1-(c : ℝ)/N)^c) *
        exp (-(((K : ℝ)-c)*((k : ℝ)-c)/(N : ℝ))) := by
  intro lam
  have hN' : 0 < (N : ℝ) := Nat.cast_pos.mpr (Nat.zero_lt_of_lt hN)
  have hgap : 0 < (N : ℝ)-c := sub_pos.mpr (by exact_mod_cast hN)
  have hn : ((N : ℝ)-c)^c ≤ (N.descFactorial c : ℝ) := by
    have H : (N-c)^c ≤ N.descFactorial c := by
      rw [Nat.descFactorial_eq_prod_range]
      have H' : (N-c)^c = ∏ _ ∈ Finset.range c, (N-c) := by simp
      rw [H']
      exact Finset.prod_le_prod fun i hi ↦ Nat.sub_le_sub_left (Finset.mem_range.mp hi).le N
    have hr : ((↑(N-c : ℕ) : ℝ)^c) ≤ (N.descFactorial c : ℝ) := by exact_mod_cast H
    rw [Nat.cast_sub hN.le] at hr
    exact hr
  have hNum (a : ℕ) : (a.descFactorial c : ℝ) ≤ (a : ℝ)^c := by
    exact_mod_cast Nat.descFactorial_le_pow a c
  have hE : (0 : ℝ) < (c.factorial : ℝ) := by positivity
  have hC : ((K.descFactorial c : ℝ)*(k.descFactorial c : ℝ)) /
        (c.factorial*(N.descFactorial c : ℝ)) ≤
      lam^c / ((c.factorial : ℝ)*(1-(c : ℝ)/N)^c) := by
    have he : lam^c / ((c.factorial : ℝ)*(1-(c : ℝ)/N)^c) =
        ((K : ℝ)^c*(k : ℝ)^c)/((c.factorial : ℝ)*((N : ℝ)-c)^c) := by
      have h' : 1-(c : ℝ)/N = ((N : ℝ)-c)/N := by
        rw [sub_div,div_self hN'.ne']
      unfold lam
      rw [h',div_pow,div_pow,mul_pow]
      field_simp [hN'.ne',hgap.ne',hE.ne']
    rw [he]
    apply div_le_div₀ (by positivity)
    · exact mul_le_mul (hNum K) (hNum k) (by positivity) (by positivity)
    · positivity
    · exact mul_le_mul_of_nonneg_left hn hE.le
  rw [poisson_factorization hc hN]
  have hhat : 0 ≤ 1-(c : ℝ)/N := sub_nonneg.mpr ((div_le_one hN').mpr (by exact_mod_cast hN.le))
  exact mul_le_mul hC (depletion_bound hc hN) (by positivity)
    (by unfold lam; positivity)

end UnivariateML.Discrete4Hyper

end
