module

public import UnivariateML.Family.Discrete4CMP

import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Filter Real
open scoped Topology BigOperators

namespace UnivariateML.Discrete5CMP

open CMP

private theorem cell_tail {ν : ℝ} (hν : 0 < ν) (k : ℕ) :
    Tendsto (fun r ↦ likelihood (fun _ : Fin 1 ↦ k) r ν) atTop (𝓝 0) := by
  let C := exp (ν * (logFactorial (k+1) - logFactorial k))
  have hC : Tendsto (fun r : ℝ ↦ C / r) atTop (𝓝 0) := by
    simpa only [div_eq_mul_inv, mul_zero] using
      (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul C
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hC ?_ ?_
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
    simp only [likelihood, ite_eq_right hr.ne', Fin.prod_univ_one]
    exact div_nonneg (exp_pos _).le (tsum_nonneg fun _ ↦ (exp_pos _).le)
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
    have hs := (summable_weight (η := log r) (Or.inl hν)).le_tsum (k+1)
      (fun _ _ ↦ (exp_pos _).le)
    have hw : 0 < weight (log r) ν (k+1) := exp_pos _
    simp only [likelihood, ite_eq_right hr.ne', Fin.prod_univ_one]
    calc
      _ ≤ weight (log r) ν k / weight (log r) ν (k+1) :=
        div_le_div_of_nonneg_left (exp_pos _).le hw hs
      _ = C / r := by
        rw [weight, weight, ← exp_sub, ← exp_log hr, ← exp_sub]
        congr 1
        rw [log_exp]
        push_cast
        ring

private theorem geometric_cell_tail (k : ℕ) :
    Tendsto (fun r ↦ likelihood (fun _ : Fin 1 ↦ k) r 0) (𝓝[<] 1) (𝓝 0) := by
  have h : Tendsto (fun r : ℝ ↦ r^k * (1-r)) (𝓝[<] 1) (𝓝 0) := by
    have hf : Continuous (fun r : ℝ ↦ r^k * (1-r)) :=
      (continuous_id.pow k).mul (continuous_const.sub continuous_id)
    simpa using (hf.continuousAt (x := 1)).tendsto.mono_left (nhdsWithin_le_nhds (s := Iio (1 : ℝ)))
  apply h.congr'
  have ha : ∀ᶠ r : ℝ in 𝓝[<] 1, 0 < r :=
    (eventually_gt_nhds zero_lt_one).filter_mono nhdsWithin_le_nhds
  have hb : ∀ᶠ r : ℝ in 𝓝[<] 1, r < 1 := self_mem_nhdsWithin
  filter_upwards [ha,hb] with r hr hr'
  have hw (j : ℕ) : weight (log r) 0 j = r^j := by
    rw [weight, zero_mul, sub_zero, mul_comm, exp_nat_mul, exp_log hr]
  simp [likelihood, hr.ne', hw, tsum_geometric_of_lt_one hr.le hr', div_inv_eq_mul]

private theorem sample_cells {n : ℕ} (x : Fin n → ℕ) (r ν : ℝ) (hr : r ≠ 0) :
    likelihood x r ν = ∏ i, likelihood (fun _ : Fin 1 ↦ x i) r ν := by
  simp [likelihood, hr]

/-- With held dispersion, the only excluded rate endpoint has zero limiting
likelihood. This includes the geometric branch and all-zero samples. -/
theorem held_dispersion_tail {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {ν : ℝ} (hν : 0 ≤ ν) :
    if ν = 0 then Tendsto (fun r ↦ likelihood x r ν) (𝓝[<] 1) (𝓝 0)
    else Tendsto (fun r ↦ likelihood x r ν) atTop (𝓝 0) := by
  split
  · rename_i h
    subst ν
    have hlim := tendsto_finsetProd Finset.univ (fun i _ ↦ geometric_cell_tail (x i))
    simp only [Finset.prod_const, Finset.card_univ, Fintype.card_fin, zero_pow (Nat.ne_of_gt hn)] at hlim
    apply hlim.congr'
    filter_upwards [(eventually_gt_nhds zero_lt_one).filter_mono
      (nhdsWithin_le_nhds (s := Iio (1 : ℝ)))] with r hr
    exact (sample_cells x r 0 hr.ne').symm
  · rename_i h
    have hp : 0 < ν := lt_of_le_of_ne hν (Ne.symm h)
    have hlim := tendsto_finsetProd Finset.univ (fun i _ ↦ cell_tail hp (x i))
    simp only [Finset.prod_const, Finset.card_univ, Fintype.card_fin, zero_pow (Nat.ne_of_gt hn)] at hlim
    apply hlim.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
    exact (sample_cells x r ν hr.ne').symm

noncomputable def adjacentHeight {n : ℕ} (x : Fin n → ℕ) (c : ℕ) (q : ℝ) : ℝ :=
  ∏ i, if x i = c then 1-q else q

private theorem adjacent_recovery {n c : ℕ} (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c+1) {q : ℝ} (hq : q ∈ Ioo 0 1) :
    let s := log ((c : ℝ)+1)
    let η := log (q/(1-q)) + s
    Tendsto (fun t : ℝ ↦ likelihood x (exp (η+t*s)) (1+t))
      atTop (𝓝 (adjacentHeight x c q)) := by
  intro s η
  have hs : logFactorial (c+1) = logFactorial c + s := by
    simp only [logFactorial, Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ]
    rw [log_mul (by positivity) (by positivity), add_comm]
  have hrat : weight η 1 (c+1) = weight η 1 c * (q/(1-q)) := by
    have hp : 0 < q/(1-q) := div_pos hq.1 (sub_pos.mpr hq.2)
    rw [weight, weight, hs, ← exp_log hp, ← exp_add]
    congr 1
    push_cast
    dsimp [η]
    ring
  have hw : weight η 1 c ≠ 0 := (exp_pos _).ne'
  have hq' : 1-q ≠ 0 := (sub_pos.mpr hq.2).ne'
  have hf (i : Fin n) :
      (if x i ∈ ({c,c+1} : Finset ℕ) then weight η 1 (x i) else 0) /
        (weight η 1 c + weight η 1 (c+1)) = if x i = c then 1-q else q := by
    rcases hx i with he | he
    · simp only [he, Finset.mem_insert, true_or, ite_true, hrat]
      field_simp
      ring
    · simp only [he, Finset.mem_insert, Finset.mem_singleton, or_true, ite_true,
        show c+1 ≠ c by omega, ite_false, hrat]
      field_simp
      ring
  have hlim := tendsto_finsetProd Finset.univ (fun i _ ↦ Discrete4CMP.adjacent_path c η (x i))
  simp_rw [hf] at hlim
  apply hlim.congr'
  exact Filter.Eventually.of_forall fun t ↦ (sample_cells x _ _ (exp_pos _).ne').symm

/-- The exact excluded height on a nonzero adjacent-supported sample is the
maximized Bernoulli likelihood on the two occupied candidate cells. This
includes positive constants, whose adjacent-height maximum is one. -/
theorem adjacent_sharp_supremum {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c+1) (hx0 : ∃ i, x i ≠ 0) :
    ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (adjacentHeight x c) (Icc 0 1) q ∧
      IsLUB ((fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) '' rateDomain) (adjacentHeight x c q) ∧
      ∀ p ∈ rateDomain, likelihood x p.1 p.2 < adjacentHeight x c q := by
  let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
  let A := L '' rateDomain
  obtain ⟨hL,hno⟩ := adjacent_full_unattained_supremum hn x hx hx0
  have hc : Continuous (adjacentHeight x c) := by
    unfold adjacentHeight
    apply continuous_finsetProd
    intro i _
    by_cases he : x i = c
    · rw [show (fun q : ℝ ↦ if x i = c then 1-q else q) = fun q ↦ 1-q by simp [he]]
      fun_prop
    · simp only [ite_eq_right he]
      fun_prop
  obtain ⟨q,hq,hM⟩ := isCompact_Icc.exists_isMaxOn
    (nonempty_Icc.mpr (zero_le_one : (0 : ℝ) ≤ 1)) hc.continuousOn
  have hupper (p : ℝ × ℝ) (hp : p ∈ rateDomain) : L p ≤ adjacentHeight x c q := by
    rcases hp.2 with hzero | ⟨hr,hd⟩
    · have hz : ¬ ∀ i, x i = 0 := by
        obtain ⟨i,hi⟩ := hx0
        exact fun h ↦ hi (h i)
      rw [show L p = 0 by simp [L,likelihood,hzero,hz]]
      exact Finset.prod_nonneg fun i _ ↦ by
        split <;> linarith [hq.1,hq.2]
    · let a := weight (log p.1) p.2 c
      let b := weight (log p.1) p.2 (c+1)
      have ha : 0 < a := exp_pos _
      have hb : 0 < b := exp_pos _
      have hab : 0 < a+b := add_pos ha hb
      let t := b/(a+b)
      have ht : t ∈ Icc (0 : ℝ) 1 :=
        ⟨div_nonneg hb.le hab.le,(div_le_one hab).mpr (by linarith)⟩
      have hs : a+b ≤ ∑' j, weight (log p.1) p.2 j := by
        have hsum : Summable (weight (log p.1) p.2) := summable_weight hd
        have H := hsum.sum_le_tsum ({c,c+1} : Finset ℕ)
          (fun j _ ↦ show 0 ≤ weight (log p.1) p.2 j from (exp_pos _).le)
        simpa only [Finset.sum_pair (show c ≠ c+1 by omega)] using H
      refine le_trans ?_ (hM ht)
      change likelihood x p.1 p.2 ≤ adjacentHeight x c t
      rw [likelihood, ite_eq_right hr.ne', adjacentHeight]
      apply Finset.prod_le_prod₀
      · intro i _
        exact div_nonneg (exp_pos _).le (hab.le.trans hs)
      · intro i _
        have he : weight (log p.1) p.2 (x i)/(a+b) =
            if x i = c then 1-t else t := by
          rcases hx i with hi | hi
          · simp only [hi]
            change a/(a+b) = 1-b/(a+b)
            field_simp
            ring
          · simp only [hi, ite_eq_right (show c+1 ≠ c by omega)]
            rfl
        rw [← he]
        exact div_le_div_of_nonneg_left (exp_pos _).le hab hs
  have hqle : adjacentHeight x c q ≤ sSup A := by
    have hs : IsClosed {t | adjacentHeight x c t ≤ sSup A} :=
      isClosed_le hc continuous_const
    have ht : Ioo (0 : ℝ) 1 ⊆ {t | adjacentHeight x c t ≤ sSup A} := by
      intro t ht
      apply le_of_tendsto (adjacent_recovery x hx ht)
      filter_upwards [eventually_ge_atTop (0 : ℝ)] with u hu
      apply hL.1
      let p : ℝ × ℝ := (exp ((log (t/(1-t)) + log ((c : ℝ)+1)) + u*log ((c : ℝ)+1)),1+u)
      have hp : p ∈ rateDomain := ⟨by dsimp [p]; linarith,
        Or.inr ⟨exp_pos _,Or.inl (by dsimp [p]; linarith)⟩⟩
      exact mem_image_of_mem L hp
    exact hs.closure_subset_iff.mpr ht (by rwa [closure_Ioo zero_ne_one])
  have he : sSup A = adjacentHeight x c q :=
    le_antisymm (hL.2 (by rintro _ ⟨p,hp,rfl⟩; exact hupper p hp)) hqle
  refine ⟨q,hq,hM,he ▸ hL,fun p hp ↦ lt_of_le_of_ne (hupper p hp) ?_⟩
  intro hp'
  apply hno
  refine ⟨p,hp,fun r hr ↦ ?_⟩
  change L r ≤ L p
  rw [show L p = adjacentHeight x c q from hp']
  exact hupper r hr

end UnivariateML.Discrete5CMP

end
