module

public import UnivariateML.Family.FinishSkellam

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! The one-sided supporting inequality for Skellam.

## Deviations
The general finite list of stationary points and unequal-mean
single-magnitude fits are separate.
-/

@[expose] public section

namespace UnivariateML.Base7Skellam

open Set Real Discrete3Skellam

private theorem poisson_shift_deriv (j : ℕ) (t : ℝ) :
    HasDerivAt (fun u ↦ exp u * poissonMass u (j + 1))
      (exp t * poissonMass t j) t := by
  have he (k : ℕ) (u : ℝ) :
      exp u * poissonMass u k = u ^ k / (k.factorial : ℝ) := by
    unfold poissonMass
    rw [← mul_div_assoc, ← mul_assoc, ← exp_add, add_neg_cancel, exp_zero, one_mul]
  simp only [he]
  convert (hasDerivAt_pow (j + 1) t).div_const (((j + 1).factorial : ℕ) : ℝ) using 1
  rw [Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ, Nat.add_sub_cancel]
  have hj : (j.factorial : ℝ) ≠ 0 := by exact_mod_cast (Nat.factorial_pos j).ne'
  have hj' : (j : ℝ) + 1 ≠ 0 := by positivity
  field_simp

/-- Each positive shift of a Poisson supporting sum is bounded by
`1 - exp (-t)`. This gives a strict deficit at every finite rate. -/
theorem poisson_supporting_sum_shift {n : ℕ} (k : Fin n → ℕ) (w : Fin n → ℝ)
    (h : ∀ t ≥ 0, (∑ i, w i * poissonMass t (k i)) ≤ 1) :
    ∀ j : ℕ, 0 < j → ∀ t ≥ 0, (∑ i, w i * poissonMass t (k i + j)) ≤ 1 - exp (-t) := by
  intro j
  induction j with
  | zero => simp
  | succ j IH =>
    intro _ t ht
    let S (a : ℕ) (u : ℝ) := ∑ i, w i * poissonMass u (k i + a)
    have hprev (u : ℝ) (hu : 0 ≤ u) : S j u ≤ 1 := by
      cases j with
      | zero => simpa [S] using h u hu
      | succ j => exact (IH (Nat.zero_lt_succ j) u hu).trans (by linarith [exp_pos (-u)])
    let F (u : ℝ) := exp u * S (j + 1) u - exp u
    have hder (u : ℝ) : HasDerivAt F (exp u * (S j u - 1)) u := by
      have hv (i : Fin n) := (poisson_shift_deriv (k i + j) u).const_mul (w i)
      have hd := (HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ hv i)).sub (hasDerivAt_exp u)
      convert hd using 1
      · ext v
        simp only [F, S, Finset.mul_sum]
        congr 1
        apply Finset.sum_congr rfl
        intro i _
        rw [← Nat.add_assoc]
        ring
      · simp only [S, mul_sub, mul_one, Finset.mul_sum]
        congr 1
        apply Finset.sum_congr rfl
        intro i _
        ring
    have hmono : AntitoneOn F (Ici 0) := by
      apply antitoneOn_of_deriv_nonpos (convex_Ici _)
      · exact fun u _ ↦ (hder u).continuousAt.continuousWithinAt
      · exact fun u _ ↦ (hder u).differentiableAt.differentiableWithinAt
      · intro u hu
        rw [(hder u).deriv]
        have hu' : 0 ≤ u := interior_subset hu
        exact mul_nonpos_of_nonneg_of_nonpos (exp_pos u).le (sub_nonpos.mpr (hprev u hu'))
    have hz : F 0 = -1 := by
      have hz' (i : Fin n) : k i + (j + 1) ≠ 0 := by omega
      simp [F, S, poissonMass, zero_pow (hz' _)]
    have hb : F t ≤ -1 := (hmono (show (0 : ℝ) ∈ Ici 0 from le_rfl) ht ht).trans_eq hz
    have hex : exp t * exp (-t) = 1 := by rw [← exp_add, add_neg_cancel, exp_zero]
    apply le_of_mul_le_mul_left _ (exp_pos t)
    rw [mul_sub, mul_one, hex]
    dsimp [F, S] at hb
    linarith only [hb]

private theorem poisson_nonnegative {r : ℝ} (hr : 0 ≤ r) (j : ℕ) :
    0 ≤ poissonMass r j := by unfold poissonMass; positivity

private theorem poisson_probability {r : ℝ} (hr : 0 ≤ r) : HasSum (poissonMass r) 1 :=
  ProbabilityTheory.hasSum_one_poissonMeasure ⟨r, hr⟩

private theorem poisson_rate_zero (j : ℕ) :
    poissonMass 0 j = if j = 0 then 1 else 0 := by
  by_cases h : j = 0
  · simp [h, poissonMass]
  · simp [h, poissonMass, zero_pow h]

/-- For a nonnegative sample of positive mean, the Poisson supporting
inequality suffices for the unique Skellam fit at the Poisson boundary. -/
theorem nonnegative_supporting_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {d : ℝ}
    (hd : 0 < d) (hmean : d = (∑ i, (x i : ℝ)) / n)
    (hs : ∀ t ≥ 0, (∑ i, poissonMass t (x i) / ((n : ℝ) * poissonMass d (x i))) ≤ 1) :
    let y : Fin n → ℤ := fun i ↦ x i
    (d, 0) ∈ domain ∧ IsMaxOn (likelihood y) domain (d, 0) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood y) domain p → p = (d, 0) := by
  intro y
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let w (i : Fin n) := 1 / ((n : ℝ) * poissonMass d (x i))
  have hbase (i : Fin n) : 0 < poissonMass d (x i) := by unfold poissonMass; positivity
  have hw (i : Fin n) : 0 < w i := by exact div_pos one_pos (mul_pos hn' (hbase i))
  have hs' : ∀ t ≥ 0, (∑ i, w i * poissonMass t (x i)) ≤ 1 := by
    simpa only [w, one_div, inv_mul_eq_div] using hs
  have hm (i : Fin n) (p : ℝ × ℝ) :
      mass (y i) p = ∑' j, poissonMass p.1 (x i + j) * poissonMass p.2 j := by
    simp [mass, y, Nat.add_comm]
  have hm0 (i : Fin n) : mass (y i) (d, 0) = poissonMass d (x i) := by
    rw [hm]
    rw [tsum_eq_single 0]
    · simp [poisson_rate_zero]
    · intro j hj
      simp [poisson_rate_zero, hj]
  have hdom : (d, (0 : ℝ)) ∈ domain := ⟨hd.le, le_rfl⟩
  have hbaseL : 0 < likelihood y (d, 0) := by
    unfold likelihood
    exact Finset.prod_pos fun i _ ↦ hm0 i ▸ hbase i
  have hstrict (p : ℝ × ℝ) (hp : p ∈ domain) (hp2 : 0 < p.2) :
      likelihood y p < likelihood y (d, 0) := by
    let g (j : ℕ) := (∑ i, w i * poissonMass p.1 (x i + j)) * poissonMass p.2 j
    have hle (j : ℕ) : g j ≤ poissonMass p.2 j := by
      apply mul_le_of_le_one_left (poisson_nonnegative hp.2 j)
      by_cases hj : j = 0
      · simpa [hj] using hs' p.1 hp.1
      · exact (poisson_supporting_sum_shift x w hs' j (Nat.pos_of_ne_zero hj) p.1 hp.1).trans
          (by linarith [exp_pos (-p.1)])
    have hpos1 : 0 < poissonMass p.2 1 := by unfold poissonMass; positivity
    have hlt : g 1 < poissonMass p.2 1 := by
      have h := poisson_supporting_sum_shift x w hs' 1 (by decide) p.1 hp.1
      exact mul_lt_of_lt_one_left hpos1 (h.trans_lt (by linarith [exp_pos (-p.1)]))
    have hgn (j : ℕ) : 0 ≤ g j :=
      mul_nonneg (Finset.sum_nonneg fun i _ ↦
        mul_nonneg (hw i).le (poisson_nonnegative hp.1 _)) (poisson_nonnegative hp.2 _)
    have ht := Summable.tsum_lt_tsum_of_nonneg hgn hle hlt (poisson_probability hp.2).summable
    rw [(poisson_probability hp.2).tsum_eq] at ht
    have hsum : ∑ i, w i * mass (y i) p = ∑' j, g j := by
      simp only [hm, ← tsum_mul_left, g, Finset.sum_mul, mul_assoc]
      rw [Summable.tsum_finsetSum]
      intro i _
      have hmass (j : ℕ) : poissonMass p.1 (x i + j) ≤ 1 := by
        rw [← (poisson_probability hp.1).tsum_eq]
        exact (poisson_probability hp.1).summable.le_tsum _ fun j _ ↦ poisson_nonnegative hp.1 j
      exact Summable.of_nonneg_of_le
        (fun j ↦ mul_nonneg (hw i).le (mul_nonneg (poisson_nonnegative hp.1 _)
          (poisson_nonnegative hp.2 _)))
        (fun j ↦ mul_le_mul_of_nonneg_left
          (mul_le_of_le_one_left (poisson_nonnegative hp.2 j) (hmass j)) (hw i).le)
        ((poisson_probability hp.2).summable.mul_left (w i))
    let R (i : Fin n) := mass (y i) p / poissonMass d (x i)
    have hR (i : Fin n) : 0 ≤ R i := by
      apply div_nonneg _ (hbase i).le
      rw [hm]
      exact tsum_nonneg fun j ↦ mul_nonneg (poisson_nonnegative hp.1 _) (poisson_nonnegative hp.2 _)
    have hsumR : (∑ i, R i) < n := by
      have he (i : Fin n) : w i * mass (y i) p = R i / n := by dsimp [w, R]; ring
      simp only [← hsum, he, ← Finset.sum_div] at ht
      simpa only [one_mul] using (div_lt_iff₀ hn').mp ht
    have hprod : (∏ i, R i) < 1 := by
      by_cases hz : ∃ i, R i = 0
      · obtain ⟨i, hi⟩ := hz
        rw [Finset.prod_eq_zero (Finset.mem_univ i) hi]
        norm_num
      · have hpos (i : Fin n) : 0 < R i := lt_of_le_of_ne (hR i) (fun h ↦ hz ⟨i, h.symm⟩)
        apply (log_lt_log_iff (Finset.prod_pos fun i _ ↦ hpos i) one_pos).mp
        rw [log_one, log_prod (fun i _ ↦ (hpos i).ne')]
        apply lt_of_le_of_lt (Finset.sum_le_sum fun i _ ↦ log_le_sub_one_of_pos (hpos i))
        simpa [Finset.sum_sub_distrib, nsmul_eq_mul] using sub_neg.mpr hsumR
    have he : (∏ i, R i) = likelihood y p / likelihood y (d, 0) := by
      simp [R, likelihood, Finset.prod_div_distrib, hm0]
    rw [he] at hprod
    exact (div_lt_one hbaseL).mp hprod
  have winner (p : ℝ × ℝ) (hp : p ∈ domain) (h : IsMaxOn (likelihood y) domain p) :
      p = (d, 0) := by
    have hz : p.2 = 0 := by
      by_contra hne
      exact (hstrict p hp (lt_of_le_of_ne hp.2 (Ne.symm hne))).not_ge (h hdom)
    have he := FinishSkellam.maximizing_rate_difference hn y p hp h
    rw [hz, sub_zero] at he
    rw [hmean]
    exact Prod.ext (by simpa [y] using he) hz
  obtain ⟨p, hp, hm⟩ := joint_attained hn y
  exact ⟨hdom, winner p hp hm ▸ hm, winner⟩

/-- Nonnegative adjacent counts satisfy the Poisson supporting inequality.
The upper-cell fraction is the difference between the mean and the lower
cell, and the exponential tangent bounds both factors of the profile. -/
theorem adjacent_supporting_inequality {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {j : ℕ}
    (hx : ∀ i, x i = j ∨ x i = j + 1) {d : ℝ}
    (hd : 0 < d) (hmean : d = (∑ i, (x i : ℝ)) / n) :
    ∀ t ≥ 0, (∑ i, poissonMass t (x i) / ((n : ℝ) * poissonMass d (x i))) ≤ 1 := by
  intro t ht
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let u := t / d
  have hu : 0 ≤ u := div_nonneg ht hd.le
  have hratio (k : ℕ) :
      poissonMass t k / ((n : ℝ) * poissonMass d k) = exp (d - t) * u ^ k / n := by
    unfold poissonMass
    rw [exp_sub, div_pow]
    have hf : (k.factorial : ℝ) ≠ 0 := by exact_mod_cast (Nat.factorial_pos k).ne'
    field_simp
    calc
      _ = t ^ k * (exp (-t) * exp t) := by ring
      _ = t ^ k * (exp (-d) * exp d) := by simp [← exp_add]
      _ = _ := by ring
  have hpow (i : Fin n) :
      u ^ x i = u ^ j * (1 + ((x i : ℝ) - j) * (u - 1)) := by
    rcases hx i with hi | hi <;> rw [hi]
    · simp
    · simp only [Nat.cast_add, Nat.cast_one, pow_succ]
      ring
  have hsplit : (∑ i, poissonMass t (x i) / ((n : ℝ) * poissonMass d (x i))) =
      exp (d - t) * u ^ j * (1 + (d - j) * (u - 1)) := by
    simp only [hratio, hpow, ← mul_assoc, ← Finset.sum_div, ← Finset.mul_sum,
      Finset.sum_add_distrib, ← Finset.sum_mul, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    have hm := (eq_div_iff hn'.ne').mp hmean
    rw [← hm]
    field_simp
  have h1 : u ≤ exp (u - 1) := by
    linarith only [add_one_le_exp (u - 1)]
  have h2 : 1 + (d - j) * (u - 1) ≤ exp ((d - j) * (u - 1)) := by
    linarith only [add_one_le_exp ((d - j) * (u - 1))]
  have hj : u ^ j ≤ exp ((j : ℝ) * (u - 1)) := by
    rw [exp_nat_mul]
    exact pow_le_pow_left₀ hu h1 j
  rw [hsplit]
  calc
    _ ≤ exp (d - t) * exp ((j : ℝ) * (u - 1)) * exp ((d - j) * (u - 1)) := by
      calc
        _ ≤ exp (d - t) * u ^ j * exp ((d - j) * (u - 1)) :=
          mul_le_mul_of_nonneg_left h2 (by positivity)
        _ ≤ _ := mul_le_mul_of_nonneg_right
          (mul_le_mul_of_nonneg_left hj (exp_pos _).le) (exp_pos _).le
    _ = 1 := by
      rw [← exp_add, ← exp_add]
      have he : d - t + (j : ℝ) * (u - 1) + (d - j) * (u - 1) = 0 := by
        dsimp [u]
        field_simp
        ring
      rw [he, exp_zero]

/-- Reflection transports the positive-mean supporting test to
nonpositive observations and interchanges the fitted rates. -/
theorem nonpositive_supporting_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {d : ℝ}
    (hd : 0 < d) (hmean : d = (∑ i, (x i : ℝ)) / n)
    (hs : ∀ t ≥ 0, (∑ i, poissonMass t (x i) / ((n : ℝ) * poissonMass d (x i))) ≤ 1) :
    let y : Fin n → ℤ := fun i ↦ -(x i : ℤ)
    (0, d) ∈ domain ∧ IsMaxOn (likelihood y) domain (0, d) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood y) domain p → p = (0, d) := by
  intro y
  let z : Fin n → ℤ := fun i ↦ x i
  have hswap (p : ℝ × ℝ) : likelihood y p = likelihood z p.swap := by
    apply Finset.prod_congr rfl
    intro i _
    unfold mass
    apply tsum_congr
    intro j
    simp [y, z, mul_comm]
  have hdom (p : ℝ × ℝ) (hp : p ∈ domain) : p.swap ∈ domain := ⟨hp.2, hp.1⟩
  obtain ⟨hD, hm, hu⟩ := nonnegative_supporting_unique hn x hd hmean hs
  refine ⟨⟨le_rfl, hd.le⟩, ?_, ?_⟩
  · intro p hp
    change likelihood y p ≤ likelihood y (0, d)
    rw [hswap, hswap]
    exact hm (hdom p hp)
  · intro p hp hm'
    have hm'' : IsMaxOn (likelihood z) domain p.swap := by
      intro q hq
      change likelihood z q ≤ likelihood z p.swap
      have h := hm' (hdom q hq)
      change likelihood y q.swap ≤ likelihood y p at h
      simpa only [hswap, Prod.swap_swap] using h
    have he := hu p.swap (hdom p hp) hm''
    exact congrArg Prod.swap he

end UnivariateML.Base7Skellam

end
