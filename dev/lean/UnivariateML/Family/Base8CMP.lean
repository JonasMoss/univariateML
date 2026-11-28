module

public import UnivariateML.Family.Discrete5CMP

set_option linter.style.header false

@[expose] public section

open Set Real
open scoped BigOperators
namespace UnivariateML.Base8CMP

open CMP Discrete5CMP

/-- The excluded adjacent-cell heights and the included zero-rate fiber,
in the same coefficient domain. A positive constant is included among
samples supported on an adjacent pair. -/
private theorem degenerate_samples {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    ((∀ i, x i = 0) → ∀ p ∈ rateDomain, IsMaxOn L rateDomain p ↔ p.1 = 0) ∧
    ((∃ i, x i ≠ 0) → ∀ c, (∀ i, x i = c ∨ x i = c+1) →
      ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (adjacentHeight x c) (Icc 0 1) q ∧
        IsLUB (L '' rateDomain) (adjacentHeight x c q) ∧
        ∀ p ∈ rateDomain, L p < adjacentHeight x c q) := by
  exact ⟨all_zero_maximizers hn x, fun hx _ hc ↦ adjacent_sharp_supremum hn x hc hx⟩

private theorem partition_pos (p : ℝ × ℝ) (hp : p ∈ domain) :
    0 < ∑' k, weight p.1 p.2 k :=
  (Real.exp_pos _).trans_le ((summable_weight hp).le_tsum 0
    (fun _ _ ↦ (Real.exp_pos _).le))

private theorem regular_likelihood {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r d : ℝ} (hr : 0 < r) (hd : (Real.log r, d) ∈ domain) :
    likelihood x r d = Real.exp ((n : ℝ) * logLik x (Real.log r, d)) := by
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  let z := ∑' k, weight (Real.log r) d k
  have hz : 0 < z := partition_pos _ hd
  have he (i : Fin n) : weight (Real.log r) d (x i) / z =
      Real.exp (Real.log r * x i - d * logFactorial (x i) - Real.log z) := by
    rw [Real.exp_sub, Real.exp_log hz]
    rfl
  unfold likelihood
  rw [ite_eq_right hr.ne']
  change (∏ i, weight (Real.log r) d (x i) / z) = _
  simp_rw [he]
  rw [← Real.exp_sum]
  congr 1
  simp [logLik, z, Finset.sum_sub_distrib, nsmul_eq_mul, mul_sub, mul_div_cancel₀ _ hnR]

private theorem midpoint_improves {n : ℕ} (x : Fin n → ℕ) {p q : ℝ × ℝ}
    (hp : p ∈ domain) (hq : q ∈ domain) (hne : p ≠ q) :
    let m := ((p.1 + q.1) / 2, (p.2 + q.2) / 2)
    m ∈ domain ∧ (logLik x p + logLik x q) / 2 < logLik x m := by
  intro m
  have hm : m ∈ domain := by
    rcases hp with hp | ⟨hp, hp'⟩ <;> rcases hq with hq | ⟨hq, hq'⟩
    · exact Or.inl (by dsimp [m]; linarith)
    · exact Or.inl (by dsimp [m]; linarith)
    · exact Or.inl (by dsimp [m]; linarith)
    · exact Or.inr ⟨by dsimp [m]; linarith, by dsimp [m]; linarith⟩
  refine ⟨hm, ?_⟩
  let Z (a : ℝ × ℝ) := ∑' k, weight a.1 a.2 k
  let f (a : ℝ × ℝ) (k : ℕ) := a.1 * k - a.2 * logFactorial k - Real.log (Z a)
  have hsum (a : ℝ × ℝ) (ha : a ∈ domain) :
      Summable (fun k ↦ Real.exp (f a k)) ∧ ∑' k, Real.exp (f a k) = 1 := by
    have h : (fun k ↦ Real.exp (f a k)) = fun k ↦ weight a.1 a.2 k / Z a := by
      funext k
      dsimp only [f]
      rw [Real.exp_sub, Real.exp_log (partition_pos a ha)]
      rfl
    rw [h]
    exact ⟨(summable_weight ha).div_const _, by
      rw [tsum_div_const, show (∑' k, weight a.1 a.2 k) = Z a from rfl,
        div_self (partition_pos a ha).ne']⟩
  have hdiff : ∃ k, f p k ≠ f q k := by
    by_contra! h
    have h₀ := h 0
    have h₁ := h 1
    have h₂ := h 2
    norm_num [f, logFactorial] at h₀ h₁ h₂
    have hη : p.1 = q.1 := by linarith
    have hlog : 0 < Real.log 2 := Real.log_pos (by norm_num)
    have hν : p.2 = q.2 := by nlinarith [h₂]
    exact hne (Prod.ext hη hν)
  let c := (Real.log (Z p) + Real.log (Z q)) / 2
  have he (k : ℕ) :
      Real.exp (m.1 * k - m.2 * logFactorial k - c) =
        Real.exp ((f p k + f q k) / 2) := by
    congr 1
    dsimp [m, c, f]
    ring
  have hle (k : ℕ) : Real.exp (m.1 * k - m.2 * logFactorial k - c) ≤
      (Real.exp (f p k) + Real.exp (f q k)) / 2 := by
    rw [he]
    by_cases hf : f p k = f q k
    · rw [hf, add_self_div_two, add_self_div_two]
    · convert (strictConvexOn_exp.2 (mem_univ (f p k)) (mem_univ (f q k)) hf
        (by norm_num : 0 < (1/2 : ℝ)) (by norm_num : 0 < (1/2 : ℝ))
        (by norm_num : (1/2 : ℝ) + 1/2 = 1)).le using 1 <;>
        simp only [smul_eq_mul] <;> ring_nf
  obtain ⟨k, hk⟩ := hdiff
  have hlt : Real.exp (m.1 * k - m.2 * logFactorial k - c) <
      (Real.exp (f p k) + Real.exp (f q k)) / 2 := by
    rw [he]
    convert strictConvexOn_exp.2 (mem_univ (f p k)) (mem_univ (f q k)) hk
      (by norm_num : 0 < (1/2 : ℝ)) (by norm_num : 0 < (1/2 : ℝ))
      (by norm_num : (1/2 : ℝ) + 1/2 = 1) using 1 <;>
      simp only [smul_eq_mul] <;> ring_nf
  have hfun : (fun k ↦ Real.exp (m.1 * k - m.2 * logFactorial k - c)) =
      fun k ↦ weight m.1 m.2 k / Real.exp c := by
    funext k
    rw [Real.exp_sub]
    rfl
  have ht := Summable.tsum_lt_tsum hle hlt
    (hfun.symm ▸ (summable_weight hm).div_const (Real.exp c))
    (((hsum p hp).1.add (hsum q hq).1).div_const (2 : ℝ))
  rw [hfun, tsum_div_const, tsum_div_const,
    (hsum p hp).1.tsum_add (hsum q hq).1, (hsum p hp).2, (hsum q hq).2] at ht
  have hZ : Real.log (Z m) < c := by
    rw [← Real.log_exp c]
    apply (Real.log_lt_log_iff (partition_pos m hm) (Real.exp_pos c)).mpr
    apply (div_lt_one (Real.exp_pos c)).mp
    simpa [Z] using ht
  have ha : (∑ i, (m.1 * x i - m.2 * logFactorial (x i))) =
      ((∑ i, (p.1 * x i - p.2 * logFactorial (x i))) +
        ∑ i, (q.1 * x i - q.2 * logFactorial (x i))) / 2 := by
    rw [← Finset.sum_add_distrib, Finset.sum_div]
    apply Finset.sum_congr rfl
    intro i _
    dsimp [m]
    ring
  unfold logLik
  rw [ha, div_right_comm _ 2 (n : ℝ), add_div]
  dsimp [c, Z] at hZ
  rw [add_div (∑ i, (p.1 * x i - p.2 * logFactorial (x i)))]
  linarith

/-- If the full CMP likelihood attains on a sample with a positive count,
that maximizer is unique, including on the geometric seam.
On all-zero data the entire zero-rate fiber maximizes. -/
theorem all_free_uniqueness {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    ((∀ i, x i = 0) → ∀ p ∈ rateDomain, IsMaxOn L rateDomain p ↔ p.1 = 0) ∧
    ((∃ i, x i ≠ 0) → ∀ p ∈ rateDomain, IsMaxOn L rateDomain p →
      ∀ q ∈ rateDomain, IsMaxOn L rateDomain q → p = q) := by
  intro L
  refine ⟨all_zero_maximizers hn x, fun hx p hp hP q hq hQ ↦ ?_⟩
  have hz : ¬ ∀ i, x i = 0 := by
    obtain ⟨i, hi⟩ := hx
    exact fun h ↦ hi (h i)
  have hpos {a : ℝ × ℝ} (ha : a ∈ rateDomain) (hA : IsMaxOn L rateDomain a) :
      0 < a.1 ∧ (Real.log a.1, a.2) ∈ domain := by
    rcases ha.2 with hzero | h
    · let b := (Real.exp 0, (1 : ℝ))
      have hb : b ∈ rateDomain := ⟨zero_le_one, Or.inr
        ⟨Real.exp_pos _, Or.inl zero_lt_one⟩⟩
      have hh := hA hb
      change likelihood x b.1 b.2 ≤ likelihood x a.1 a.2 at hh
      rw [regular_likelihood hn x (Real.exp_pos _) (show (Real.log b.1, b.2) ∈ domain from
        Or.inl zero_lt_one)] at hh
      simp [hzero, likelihood, hz] at hh
      exact ((Real.exp_pos _).not_ge hh).elim
    · exact h
  obtain ⟨hp₁,hp₀⟩ := hpos hp hP
  obtain ⟨hq₁,hq₀⟩ := hpos hq hQ
  let a := (Real.log p.1, p.2)
  let b := (Real.log q.1, q.2)
  by_contra hpq
  have hab : a ≠ b := by
    intro he
    have h₁ := congrArg (fun t : ℝ × ℝ ↦ Real.exp t.1) he
    have h₂ := congrArg Prod.snd he
    simp [a, b, Real.exp_log hp₁, Real.exp_log hq₁] at h₁
    exact hpq (Prod.ext h₁ h₂)
  let c := ((a.1+b.1)/2, (a.2+b.2)/2)
  have ⟨hc,hlt⟩ := midpoint_improves x hp₀ hq₀ hab
  let t := (Real.exp c.1, c.2)
  have ht : t ∈ rateDomain := by
    have hd : (Real.log t.1, t.2) ∈ domain := by simpa [t] using hc
    have hc₂ : 0 ≤ c.2 := by
      rcases hc with h | ⟨h,_⟩
      · exact h.le
      · exact h.ge
    exact ⟨hc₂, Or.inr ⟨Real.exp_pos _, hd⟩⟩
  have hT := hP ht
  have he : L p = L q := le_antisymm (hQ hp) (hP hq)
  change likelihood x t.1 t.2 ≤ likelihood x p.1 p.2 at hT
  change likelihood x p.1 p.2 = likelihood x q.1 q.2 at he
  rw [regular_likelihood hn x hp₁ hp₀, regular_likelihood hn x hq₁ hq₀,
    Real.exp_eq_exp] at he
  have htd : (Real.log t.1, t.2) ∈ domain := by simpa [t] using hc
  rw [regular_likelihood hn x (Real.exp_pos _) htd] at hT
  rw [regular_likelihood hn x hp₁ hp₀, Real.exp_le_exp] at hT
  have hn' : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  change (n : ℝ) * logLik x a = (n : ℝ) * logLik x b at he
  have he' := (mul_left_cancel₀ hn'.ne' he)
  simp only [t, Real.log_exp] at hT
  have hle := (mul_le_mul_iff_of_pos_left hn').mp hT
  change (logLik x a + logLik x b) / 2 < logLik x c at hlt
  linarith

private theorem compact_hypograph {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hpos : ∃ i, x i ≠ 0) (hspread : ¬ ∃ c, ∀ i, x i = c ∨ x i = c+1) (v₀ : ℝ) :
    ∃ E : Set ((ℝ × ℝ) × ℝ), IsCompact E ∧ ∀ p v,
      (p,v) ∈ E ↔ p ∈ domain ∧ v₀ ≤ v ∧ v ≤ logLik x p := by
  classical
  let A := (∑ i, (x i : ℝ)) / (n : ℝ)
  let B := (∑ i, logFactorial (x i)) / (n : ℝ)
  have hn' : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hA : 0 < A := by
    obtain ⟨i, hi⟩ := hpos
    exact div_pos ((Nat.cast_pos.mpr (Nat.pos_of_ne_zero hi)).trans_le
      (Finset.single_le_sum (fun j _ ↦ Nat.cast_nonneg (x j)) (Finset.mem_univ i))) hn'
  let c := ⌊A⌋₊
  let w := A - (c : ℝ)
  have hw : 0 ≤ w := sub_nonneg.mpr (Nat.floor_le hA.le)
  have hw' : w ≤ 1 := by dsimp [w, c]; linarith [Nat.lt_floor_add_one A]
  let H := logFactorial c + Real.log ((c : ℝ)+1) * w
  have hH : H < B := by
    have hex : ∃ i, x i ≠ c ∧ x i ≠ c+1 := by
      by_contra! hh
      exact hspread ⟨c, fun j ↦ or_iff_not_imp_left.mpr (hh j)⟩
    obtain ⟨i, hi, hi'⟩ := hex
    let line (k : ℕ) := logFactorial c + Real.log ((c : ℝ)+1) * ((k : ℝ)-c)
    have hsum : (∑ i, line (x i)) < ∑ i, logFactorial (x i) :=
      Finset.sum_lt_sum (fun j _ ↦ (adjacent_supporting_line c (x j)).1)
        ⟨i, Finset.mem_univ i, lt_of_le_of_ne (adjacent_supporting_line c (x i)).1
          (fun he ↦ ((adjacent_supporting_line c (x i)).2.mp he).elim hi hi')⟩
    have he : (∑ i, line (x i)) = (n : ℝ)*H := by
      simp only [line, Finset.sum_add_distrib, ← Finset.mul_sum, Finset.sum_sub_distrib,
        Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
      dsimp [H, w, A]
      field_simp
    rw [he] at hsum
    exact (lt_div_iff₀ hn').2 (by simpa [mul_comm] using hsum)
  have hH' : H = (1-w)*logFactorial c+w*logFactorial (c+1) := by
    have hh := (adjacent_supporting_line c (c+1)).2.mpr (Or.inr rfl)
    push_cast at hh
    have hh' : logFactorial (c+1) = logFactorial c + Real.log ((c:ℝ)+1) := by linarith
    rw [hh']
    dsimp [H]
    ring
  let J (p : ℝ × ℝ) := p.1*A-p.2*B
  have hJ (p : ℝ × ℝ) : logLik x p = J p - Real.log (∑' k, weight p.1 p.2 k) := by
    unfold logLik
    simp only [Finset.sum_sub_distrib, ← Finset.mul_sum]
    dsimp [J, A, B]
    ring
  let E : Set ((ℝ × ℝ) × ℝ) := {u | 0 ≤ u.1.2 ∧ v₀ ≤ u.2 ∧
    ∀ N : ℕ, (∑ k ∈ Finset.range N, weight u.1.1 u.1.2 k) ≤ exp (J u.1-u.2)}
  have hE : IsClosed E := by
    have he : E = {u : (ℝ × ℝ) × ℝ | 0 ≤ u.1.2} ∩
        ({u : (ℝ × ℝ) × ℝ | v₀ ≤ u.2} ∩ ⋂ N : ℕ,
          {u : (ℝ × ℝ) × ℝ |
            (∑ k ∈ Finset.range N, weight u.1.1 u.1.2 k) ≤ exp (J u.1-u.2)}) := by
      ext u
      simp only [E, mem_ofPred_eq, mem_inter_iff, mem_iInter]
    rw [he]
    apply IsClosed.inter (isClosed_le continuous_const (by fun_prop))
    apply IsClosed.inter (isClosed_le continuous_const (by fun_prop))
    apply isClosed_iInter
    intro N
    unfold weight J
    exact isClosed_le (by fun_prop) (by fun_prop)
  have hmem {p : ℝ × ℝ} (hp : p ∈ domain) {v : ℝ} (hv : v₀ ≤ v)
      (hv' : v ≤ logLik x p) : (p,v) ∈ E := by
    have hs := summable_weight hp
    have hz := partition_pos p hp
    have hν : 0 ≤ p.2 := by rcases hp with h | ⟨h,_⟩ <;> linarith
    refine ⟨hν, hv, fun N ↦ ?_⟩
    rw [hJ] at hv'
    apply (hs.sum_le_tsum _ (fun _ _ ↦ (exp_pos _).le)).trans
    change (∑' k, weight p.1 p.2 k) ≤ exp (J p-v)
    rw [← exp_log hz, exp_le_exp]
    linarith
  have hcell {u : (ℝ × ℝ) × ℝ} (hu : u ∈ E) (k : ℕ) :
      u.2 ≤ J u.1 - (u.1.1*k-u.1.2*logFactorial k) := by
    have he := hu.2.2 (k+1)
    have hh := (Finset.single_le_sum (f := fun j ↦ weight u.1.1 u.1.2 j)
      (fun j _ ↦ (exp_pos _).le) (Finset.mem_range.mpr (Nat.lt_succ_self k))).trans he
    rw [weight, exp_le_exp] at hh
    linarith
  have hdom {u : (ℝ × ℝ) × ℝ} (hu : u ∈ E) : u.1 ∈ domain := by
    rcases eq_or_lt_of_le hu.1 with h | h
    · right
      refine ⟨h.symm, ?_⟩
      by_contra! hh
      obtain ⟨N, hN⟩ := exists_nat_gt (exp (J u.1-u.2))
      have hs : (N : ℝ) ≤ ∑ k ∈ Finset.range N, weight u.1.1 u.1.2 k := by
        rw [show (N : ℝ) = ∑ _k ∈ Finset.range N, (1 : ℝ) by simp]
        apply Finset.sum_le_sum
        intro k _
        rw [weight, h.symm, zero_mul, sub_zero, one_le_exp_iff]
        positivity
      exact hN.not_ge (hs.trans (hu.2.2 N))
    · exact Or.inl h
  have hupper {u : (ℝ × ℝ) × ℝ} (hu : u ∈ E) : u.2 ≤ logLik x u.1 := by
    have hs := summable_weight (hdom hu)
    have hz := partition_pos _ (hdom hu)
    have hl := hs.hasSum.tendsto_sum_nat
    have he := le_of_tendsto hl (Filter.Eventually.of_forall hu.2.2)
    rw [← exp_log hz, exp_le_exp] at he
    rw [hJ]
    linarith
  let D := |v₀|/(B-H)
  obtain ⟨K, hK⟩ := exists_nat_gt A
  let L := v₀/A
  let U := (D*|logFactorial K-B|+|v₀|)/((K : ℝ)-A)
  let V := (|L|+|U|)*A+D*|B|
  have hbounds {u : (ℝ × ℝ) × ℝ} (hu : u ∈ E) :
      u ∈ (Icc L U ×ˢ Icc 0 D) ×ˢ Icc v₀ V := by
    have hc := hcell hu c
    have hc' := hcell hu (c+1)
    have hav := add_le_add (mul_le_mul_of_nonneg_left hc (sub_nonneg.mpr hw'))
      (mul_le_mul_of_nonneg_left hc' hw)
    push_cast at hav
    have he : (1-w)*(c : ℝ)+w*((c : ℝ)+1) = A := by dsimp [w]; ring
    have htail : u.2 ≤ -(B-H)*u.1.2 := by
      dsimp [J] at hav
      calc
        u.2 = (1-w)*u.2+w*u.2 := by ring
        _ ≤ _ := hav
        _ = _ := by linear_combination -u.1.1 * he - u.1.2 * hH'
    have hd : u.1.2 ≤ D := (le_div_iff₀ (sub_pos.mpr hH)).2 (by
      nlinarith only [htail, hu.2.1, neg_abs_le v₀])
    have h0 := hcell hu 0
    simp [J, logFactorial] at h0
    have hL : L ≤ u.1.1 := by
      apply (div_le_iff₀ hA).2
      nlinarith only [h0, hu.2.1, hu.1, show 0 ≤ B from by
        dsimp [B, logFactorial]
        exact div_nonneg (Finset.sum_nonneg fun i _ ↦ log_nonneg (by
          exact_mod_cast Nat.factorial_pos (x i))) hn'.le]
    have hK' := hcell hu K
    dsimp [J] at hK'
    have hU : u.1.1 ≤ U := by
      apply (le_div_iff₀ (sub_pos.mpr hK)).2
      have hmul := (mul_le_mul_of_nonneg_left (le_abs_self (logFactorial K-B)) hu.1).trans
        (mul_le_mul_of_nonneg_right hd (abs_nonneg _))
      nlinarith only [hK', hu.2.1, neg_abs_le v₀, hmul]
    have ha : u.1.1 ≤ |L|+|U| := by linarith [le_abs_self U, abs_nonneg L]
    have hv : u.2 ≤ V := by
      have hmul := (mul_le_mul_of_nonneg_left (neg_le_abs B) hu.1).trans
        (mul_le_mul_of_nonneg_right hd (abs_nonneg _))
      dsimp [V]
      nlinarith only [h0, hmul, mul_le_mul_of_nonneg_right ha hA.le]
    exact ⟨⟨⟨hL,hU⟩,⟨hu.1,hd⟩⟩,⟨hu.2.1,hv⟩⟩
  have hcpt : IsCompact E :=
    ((isCompact_Icc.prod isCompact_Icc).prod isCompact_Icc).of_isClosed_subset hE
      (fun _ hu ↦ hbounds hu)
  exact ⟨E, hcpt, fun p v ↦ ⟨fun h ↦ ⟨hdom h, h.2.1, hupper h⟩,
    fun ⟨hp, hv, hv'⟩ ↦ hmem hp hv hv'⟩⟩

private theorem nonadjacent_attains_natural {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hpos : ∃ i, x i ≠ 0) (hspread : ¬ ∃ c, ∀ i, x i = c ∨ x i = c+1) :
    ∃ p ∈ domain, IsMaxOn (logLik x) domain p := by
  let v₀ := logLik x (0,1)
  obtain ⟨E, hcpt, hE⟩ := compact_hypograph hn x hpos hspread v₀
  have hEne : E.Nonempty :=
    ⟨((0,1),v₀), (hE _ _).mpr ⟨Or.inl zero_lt_one, le_rfl, le_rfl⟩⟩
  obtain ⟨u, hu, huM⟩ := hcpt.exists_isMaxOn hEne continuous_snd.continuousOn
  obtain ⟨hd, huv, hupper⟩ := (hE u.1 u.2).mp hu
  refine ⟨u.1, hd, fun p hp ↦ ?_⟩
  change logLik x p ≤ logLik x u.1
  by_cases hv : v₀ ≤ logLik x p
  · exact (huM ((hE p _).mpr ⟨hp, hv, le_rfl⟩)).trans hupper
  · exact (le_of_not_ge hv).trans (huv.trans hupper)

/-- The full CMP fit exists precisely for zeros and for samples that are
not supported on a single adjacent pair. -/
theorem all_free_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    (∃ p ∈ rateDomain, IsMaxOn L rateDomain p) ↔
      (∀ i, x i = 0) ∨ ¬ ∃ c, ∀ i, x i = c ∨ x i = c+1 := by
  intro L
  by_cases hz : ∀ i, x i = 0
  · have hD : ((0,0) : ℝ × ℝ) ∈ rateDomain := ⟨le_rfl, Or.inl rfl⟩
    exact iff_of_true ⟨(0,0),hD,(all_zero_maximizers hn x hz _ hD).mpr rfl⟩ (Or.inl hz)
  have hpos : ∃ i, x i ≠ 0 := not_forall.mp hz
  constructor
  · rintro ⟨p,hp,hM⟩
    right
    rintro ⟨c,hc⟩
    exact (adjacent_full_unattained_supremum hn x hc hpos).2 ⟨p,hp,hM⟩
  · rintro (h | h)
    · exact (hz h).elim
    obtain ⟨p,hp,hM⟩ := nonadjacent_attains_natural hn x hpos h
    let q := (exp p.1, p.2)
    have hq : (log q.1, q.2) ∈ domain := by simpa [q] using hp
    have hp₂ : 0 ≤ p.2 := by rcases hp with hh | ⟨hh,_⟩ <;> linarith
    refine ⟨q, ⟨hp₂, Or.inr ⟨exp_pos _, hq⟩⟩, fun r hr ↦ ?_⟩
    change likelihood x r.1 r.2 ≤ likelihood x q.1 q.2
    rw [regular_likelihood hn x (exp_pos _) hq]
    rcases hr.2 with hzero | ⟨hr,hr₀⟩
    · simpa [hzero, likelihood, hz] using (exp_pos ((n : ℝ)*logLik x (p.1,q.2))).le
    · rw [regular_likelihood hn x hr hr₀, exp_le_exp]
      simp only [q, log_exp]
      exact mul_le_mul_of_nonneg_left (hM hr₀) (Nat.cast_nonneg n)

private theorem logFactorial_bounds (k : ℕ) :
    0 ≤ logFactorial k ∧ logFactorial k ≤ (k : ℝ)^2 := by
  have hpos : 0 < (k.factorial : ℝ) := Nat.cast_pos.mpr (Nat.factorial_pos k)
  refine ⟨log_nonneg (by exact_mod_cast Nat.factorial_pos k), ?_⟩
  induction k with
  | zero => simp [logFactorial]
  | succ k ih =>
    have hk : 0 < (k.factorial : ℝ) := Nat.cast_pos.mpr (Nat.factorial_pos k)
    have hkn : 0 < (k : ℝ)+1 := by positivity
    have hk₂ : (logFactorial k ≤ (k : ℝ)^2) := ih hk
    unfold logFactorial
    rw [Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ, log_mul hkn.ne' hk.ne']
    have hl := log_le_sub_one_of_pos hkn
    dsimp [logFactorial] at hk₂
    nlinarith [Nat.cast_nonneg (α := ℝ) k]

private theorem weight_zero_dispersion {r : ℝ} (hr : 0 < r) (k : ℕ) :
    weight (log r) 0 k = r ^ k := by
  rw [weight, zero_mul, sub_zero, mul_comm, exp_nat_mul, exp_log hr]

/-- At the geometric rate matching the sample mean, the sample log-factorial
mean at or above the geometric mean suffices for global optimality. -/
theorem geometric_optimal {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ}
    (hr : r ∈ Ioo 0 1)
    (hm : (∑ i, (x i : ℝ)) / (n : ℝ) = r/(1-r))
    (ht : (1-r) * (∑' k, r^k * logFactorial k) ≤
      (∑ i, logFactorial (x i)) / (n : ℝ)) :
    IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) rateDomain (r,0) := by
  let m := (∑ i, (x i : ℝ)) / (n : ℝ)
  let t := (∑ i, logFactorial (x i)) / (n : ℝ)
  let z := (1-r)⁻¹
  let v := (1-r) * (∑' k, r^k * logFactorial k)
  have hr₀ : (log r, 0) ∈ domain := Or.inr ⟨rfl, log_neg hr.1 hr.2⟩
  have hn' : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hr' : ‖r‖ < 1 := by simpa [abs_of_pos hr.1] using hr.2
  have hrpos : 0 ≤ r := hr.1.le
  have hz : 0 < z := inv_pos.mpr (sub_pos.mpr hr.2)
  have hzsum : (∑' k : ℕ, r^k) = z := tsum_geometric_of_lt_one hr.1.le hr.2
  have hgeom := summable_geometric_of_norm_lt_one hr'
  have hmean := hasSum_coe_mul_geometric_of_norm_lt_one hr'
  have hsum : Summable (fun k : ℕ ↦ r^k * logFactorial k) := by
    refine Summable.of_nonneg_of_le (fun k ↦ mul_nonneg (by positivity) (logFactorial_bounds k).1)
      (fun k ↦ ?_) (hasSum_sq_mul_geometric_of_norm_lt_one hr').summable
    rw [mul_comm ((k : ℝ)^2)]
    exact mul_le_mul_of_nonneg_left (logFactorial_bounds k).2 (by positivity)
  have hmz : (∑' k : ℕ, (k : ℝ)*r^k) = z*m := by
    rw [hmean.tsum_eq]
    change _ = z*((∑ i, (x i : ℝ))/(n : ℝ))
    rw [hm]
    dsimp [z]
    field_simp [sub_ne_zero.mpr hr.2.ne']
  have hvz : (∑' k : ℕ, r^k * logFactorial k) = z*v := by
    dsimp [z,v]
    field_simp [sub_ne_zero.mpr hr.2.ne']
  have hJ (p : ℝ × ℝ) : logLik x p =
      p.1*m - p.2*t - log (∑' k, weight p.1 p.2 k) := by
    unfold logLik
    simp only [Finset.sum_sub_distrib, ← Finset.mul_sum]
    dsimp [m,t]
    ring
  have hmax (p : ℝ × ℝ) (hp : p ∈ domain) : logLik x p ≤ logLik x (log r,0) := by
    let d := p.1-log r
    let a := d*m-p.2*v
    have he (k : ℕ) : exp (-a) * weight p.1 p.2 k =
        r^k * exp (d*k-p.2*logFactorial k-a) := by
      rw [← weight_zero_dispersion hr.1, weight, weight, ← exp_add, ← exp_add]
      congr 1
      dsimp [d]
      ring
    have hb (k : ℕ) : (1-a)*r^k + d*((k : ℝ)*r^k) -
        p.2*(r^k*logFactorial k) ≤ exp (-a) * weight p.1 p.2 k := by
      rw [he]
      calc
        _ = r^k*(1+(d*k-p.2*logFactorial k-a)) := by ring
        _ ≤ _ := mul_le_mul_of_nonneg_left (by
          simpa only [add_comm (1 : ℝ)] using add_one_le_exp (d*k-p.2*logFactorial k-a))
          (pow_nonneg hrpos k)
    have hf := (hgeom.mul_left (1-a)).add (hmean.summable.mul_left d)
    have hq := (summable_weight hp).mul_left (exp (-a))
    have H := (hf.sub (hsum.mul_left p.2)).tsum_le_tsum hb hq
    rw [hf.tsum_sub (hsum.mul_left p.2),
      (hgeom.mul_left (1-a)).tsum_add (hmean.summable.mul_left d),
      tsum_mul_left, tsum_mul_left, tsum_mul_left, tsum_mul_left, hzsum, hmz, hvz] at H
    have H' : z ≤ exp (-a) * (∑' k, weight p.1 p.2 k) := by
      convert H using 1
      dsimp [a]
      ring
    have hZ : log z + a ≤ log (∑' k, weight p.1 p.2 k) := by
      have hlog := (log_le_log_iff hz
        (mul_pos (exp_pos _) (partition_pos p hp))).mpr H'
      rw [log_mul (exp_pos _).ne' (partition_pos p hp).ne', log_exp] at hlog
      linarith
    have hν : 0 ≤ p.2 := by rcases hp with h | ⟨h,_⟩ <;> linarith
    have ht' : v ≤ t := ht
    rw [hJ, hJ, show (∑' k, weight (log r) 0 k) = z by
      simpa only [weight_zero_dispersion hr.1] using hzsum]
    dsimp [a,d] at hZ
    simp only [zero_mul, sub_zero]
    nlinarith only [hZ, mul_le_mul_of_nonneg_left ht' hν]
  intro p hp
  change likelihood x p.1 p.2 ≤ likelihood x r 0
  rw [regular_likelihood hn x hr.1 hr₀]
  rcases hp.2 with h | ⟨h, h'⟩
  · have hx : ¬ ∀ i, x i = 0 := by
      intro hx
      simp [hx] at hm
      exact (div_pos hr.1 (sub_pos.mpr hr.2)).ne' hm.symm
    simpa [likelihood, h, hx] using (exp_pos ((n : ℝ)*logLik x (log r,0))).le
  · rw [regular_likelihood hn x h h', exp_le_exp]
    exact mul_le_mul_of_nonneg_left (hmax _ h') hn'.le

private theorem geometric_is_reachable :
    ∃ (n : ℕ) (x : Fin n → ℕ), (∃ i, x i ≠ 0) ∧
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) rateDomain ((1/2 : ℝ),0) := by
  classical
  obtain ⟨c,hc⟩ := exists_nat_gt (exp 6)
  have hc₀ : 0 < c := Nat.cast_pos.mp ((exp_pos _).trans hc)
  let n := 2*c
  have hn : 0 < n := by omega
  let i₀ : Fin n := ⟨0,hn⟩
  let x (i : Fin n) := if i = i₀ then n else 0
  refine ⟨n,x,⟨i₀,by simp [x, hn.ne']⟩,?_⟩
  have hcase (f : ℕ → ℝ) (h0 : f 0 = 0) : (∑ i, f (x i)) = f n := by
    rw [Finset.sum_eq_single i₀]
    · simp [x]
    · intro i _ hi
      simp [x, hi,h0]
    · simp
  apply geometric_optimal hn x (show (1/2 : ℝ) ∈ Ioo 0 1 from ⟨by norm_num, by norm_num⟩)
  · rw [hcase (fun k ↦ (k : ℝ)) (by simp)]
    field_simp [show (n : ℝ) ≠ 0 from (Nat.cast_pos.mpr hn).ne']
    norm_num
  · have hlog : 6 < log ((c : ℝ)+1) := by
      rw [← exp_lt_exp, exp_log (by positivity)]
      linarith
    have hline := (adjacent_supporting_line c n).1
    have hd : (n : ℝ) - c = (c : ℝ) := by simp [n]; ring
    rw [hd] at hline
    have hbig : 3*(n : ℝ) ≤ logFactorial n := by
      have hpos : 0 < (c : ℝ) := Nat.cast_pos.mpr hc₀
      have hh := mul_le_mul_of_nonneg_right hlog.le hpos.le
      push_cast [n]
      nlinarith only [hline, hh, (logFactorial_bounds c).1]
    have hr : ‖(1/2 : ℝ)‖ < 1 := by norm_num
    have hsum : (∑' k, (1/2 : ℝ)^k * logFactorial k) ≤ 6 := by
      have hs := hasSum_sq_mul_geometric_of_norm_lt_one hr
      have hle (k : ℕ) : (1/2 : ℝ)^k * logFactorial k ≤ (k : ℝ)^2*(1/2 : ℝ)^k := by
        rw [mul_comm ((k : ℝ)^2)]
        exact mul_le_mul_of_nonneg_left (logFactorial_bounds k).2 (by positivity)
      have hp := Summable.of_nonneg_of_le
        (fun k ↦ mul_nonneg (by positivity) (logFactorial_bounds k).1) hle hs.summable
      have H := hp.tsum_le_tsum hle hs.summable
      rw [hs.tsum_eq] at H
      norm_num at H
      exact H
    rw [hcase logFactorial (by simp [logFactorial])]
    have hv := (le_div_iff₀ (Nat.cast_pos.mpr hn : 0 < (n : ℝ))).2 hbig
    linarith

/-- Adjacent support gives a strictly unattained sharp height. All-zero
data attain on the zero-rate fiber, and the geometric boundary is a possible
attained optimum. Off the adjacent edges, every positive-rate log-likelihood
superlevel is compact in the normalizable natural chart. -/
theorem all_free_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    ((∀ i, x i = 0) → ∀ p ∈ rateDomain, IsMaxOn L rateDomain p ↔ p.1 = 0) ∧
    ((∃ i, x i ≠ 0) → ∀ c, (∀ i, x i = c ∨ x i = c+1) →
      ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (adjacentHeight x c) (Icc 0 1) q ∧
        IsLUB (L '' rateDomain) (adjacentHeight x c q) ∧
        ∀ p ∈ rateDomain, L p < adjacentHeight x c q) ∧
    ((∃ i, x i ≠ 0) → (¬ ∃ c, ∀ i, x i = c ∨ x i = c+1) →
      ∀ v : ℝ, IsCompact {p ∈ domain | v ≤ logLik x p}) ∧
    (∃ (j : ℕ) (y : Fin j → ℕ), (∃ i, y i ≠ 0) ∧
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood y p.1 p.2) rateDomain ((1/2 : ℝ),0)) := by
  intro L
  obtain ⟨hzero, hedge⟩ := degenerate_samples hn x
  refine ⟨hzero, hedge, ?_, geometric_is_reachable⟩
  intro hpos hspread v
  obtain ⟨E, hc, hE⟩ := compact_hypograph hn x hpos hspread v
  have he : {p ∈ domain | v ≤ logLik x p} = Prod.fst '' E := by
    ext p
    constructor
    · rintro ⟨hp,hv⟩
      exact ⟨(p,v), (hE _ _).mpr ⟨hp,le_rfl,hv⟩, rfl⟩
    · rintro ⟨⟨q,w⟩, h, rfl⟩
      obtain ⟨hq,hw,hlog⟩ := (hE _ _).mp h
      exact ⟨hq,hw.trans hlog⟩
  rw [he]
  exact hc.image continuous_fst

end UnivariateML.Base8CMP

end
