module

public import UnivariateML.Family.BetaBinomial

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Fixed-size beta-binomial held shapes

## Deviations

The held-shape theorem treats attainment and the two endpoint limits.
Uniqueness on the attained positive-size branch requires the score crossing.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators Classical

namespace UnivariateML.Discrete4BBinom

private noncomputable def rising (k : ℕ) (a : ℝ) : ℝ :=
  ∏ j ∈ Finset.range k, (a+(j : ℝ))

private theorem rising_pos (k : ℕ) {a : ℝ} (ha : 0 < a) : 0 < rising k a :=
  Finset.prod_pos fun _ _ ↦ by positivity

private theorem rising_zero {k : ℕ} (hk : 0 < k) : rising k 0 = 0 :=
  Finset.prod_eq_zero (Finset.mem_range.mpr hk) (by simp)

private theorem rising_cont (k : ℕ) : Continuous (rising k) := by
  unfold rising
  fun_prop

/-- Beta-binomial mass in rising factorials, on its fixed-size support. -/
@[no_expose] noncomputable def mass (m k : ℕ) (a b : ℝ) : ℝ :=
  (m.choose k : ℝ) * rising k a * rising (m-k) b / rising m (a+b)

noncomputable def likelihood {n : ℕ} (m : ℕ) (x : Fin n → ℕ) (a b : ℝ) : ℝ :=
  ∏ i, mass m (x i) a b

private theorem mass_pos {m k : ℕ} (hk : k ≤ m) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    0 < mass m k a b :=
  div_pos (mul_pos (mul_pos (Nat.cast_pos.mpr (Nat.choose_pos hk)) (rising_pos _ ha))
    (rising_pos _ hb)) (rising_pos _ (by positivity))

private theorem log_likelihood {m n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    log (likelihood m x a b) = BetaBinomial.logLik m x a b := by
  have hlog (k : ℕ) {z : ℝ} (hz : 0 < z) :
      log (rising k z) = BetaBinomial.risingLog k z := by
    unfold rising BetaBinomial.risingLog
    rw [log_prod (f := fun j : ℕ ↦ z+(j : ℝ))
      (fun j _ ↦ (show 0 < z+(j : ℝ) by positivity).ne')]
  rw [likelihood, log_prod (fun i _ ↦ (mass_pos (hx i) ha hb).ne')]
  unfold BetaBinomial.logLik
  apply Finset.sum_congr rfl
  intro i _
  have hm : 0 < (m.choose (x i) : ℝ) := Nat.cast_pos.mpr (Nat.choose_pos (hx i))
  rw [mass, log_div (by exact
    (mul_pos (mul_pos hm (rising_pos _ ha)) (rising_pos _ hb)).ne')
    (rising_pos _ (add_pos ha hb)).ne',
    log_mul (mul_pos hm (rising_pos _ ha)).ne' (rising_pos _ hb).ne',
    log_mul hm.ne' (rising_pos _ ha).ne', hlog _ ha, hlog _ hb, hlog _ (add_pos ha hb)]

private theorem mass_at_zero {m k : ℕ} (hk : k ≤ m) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun b ↦ mass m k a b) (𝓝[>] 0) (𝓝 (if k = m then 1 else 0)) := by
  have c : ContinuousAt (fun b ↦ mass m k a b) 0 := by
    unfold mass
    apply ContinuousAt.div
    · exact (rising_cont _).continuousAt.const_mul _
    · exact (rising_cont _).continuousAt.comp (continuousAt_const.add continuousAt_id)
    · simpa using (rising_pos m ha).ne'
  have hval : mass m k a 0 = if k = m then 1 else 0 := by
    by_cases h : k = m
    · subst k
      simp [mass,show rising 0 0 = 1 by simp [rising],(rising_pos m ha).ne']
    · have hc : 0 < m-k := by omega
      simp [mass,h,rising_zero hc]
  rw [← hval]
  exact c.tendsto.mono_left nhdsWithin_le_nhds

private noncomputable def scaled (k : ℕ) (z b : ℝ) : ℝ :=
  ∏ j ∈ Finset.range k, (1 + (z+(j : ℝ))/b)

private theorem scaled_limit (k : ℕ) (z : ℝ) :
    Tendsto (scaled k z) atTop (𝓝 1) := by
  have hw (j : ℕ) : Tendsto (fun b : ℝ ↦ 1 + (z+(j : ℝ))/b) atTop (𝓝 1) := by
    simpa [div_eq_mul_inv] using ((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul (z+j)).const_add 1
  change Tendsto (fun b ↦ ∏ j ∈ Finset.range k, (1 + (z+(j : ℝ))/b)) atTop (𝓝 1)
  simpa only [Finset.prod_const_one] using
    tendsto_finsetProd (Finset.range k) fun j _ ↦ hw j

private theorem rising_scaled (k : ℕ) (z : ℝ) {b : ℝ} (hb : 0 < b) :
    rising k (z+b) = b^k * scaled k z b := by
  unfold rising scaled
  have he : b^k = ∏ _ ∈ Finset.range k, b := by simp
  rw [he, ← Finset.prod_mul_distrib]
  apply Finset.prod_congr rfl
  intro j _
  field_simp [hb.ne']
  ring

private theorem mass_at_infinity {m k : ℕ} (hk : k ≤ m) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun b ↦ mass m k a b) atTop (𝓝 (if k = 0 then 1 else 0)) := by
  have H : Tendsto (fun b : ℝ ↦ (b⁻¹)^k) atTop (𝓝 (0^k)) :=
    (tendsto_inv_atTop_zero (𝕜 := ℝ)).pow k
  have H' := ((H.mul (scaled_limit (m-k) 0)).div (scaled_limit m a) one_ne_zero).const_mul
    ((m.choose k : ℝ)*rising k a)
  have he : ((m.choose k : ℝ)*rising k a) * (0^k*1/1) = if k = 0 then 1 else 0 := by
    by_cases h : k = 0
    · subst k
      simp [rising]
    · simp [h,zero_pow]
  rw [he] at H'
  apply H'.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
  have h1 : rising (m-k) b = b^(m-k) * scaled (m-k) 0 b := by
    simpa using rising_scaled (m-k) 0 hb
  have h2 : b^m = b^(m-k)*b^k := by rw [← pow_add, Nat.sub_add_cancel hk]
  have hD : scaled m a b ≠ 0 := by
    unfold scaled
    exact Finset.prod_ne_zero_iff.mpr fun j _ ↦ (show 0 < 1+(a+(j : ℝ))/b by positivity).ne'
  change ((m.choose k : ℝ)*rising k a) * ((b⁻¹)^k * scaled (m-k) 0 b / scaled m a b) =
    mass m k a b
  simp only [mass,h1,rising_scaled m a hb,h2,inv_pow]
  field_simp [hb.ne',hD]

/-- With alpha held, beta tending to zero gives the upper atom, and beta
tending to infinity gives the lower atom. -/
theorem fixed_alpha_paths {m n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    {a : ℝ} (ha : 0 < a) :
    Tendsto (likelihood m x a) (𝓝[>] 0) (𝓝 (if ∀ i, x i = m then 1 else 0)) ∧
    Tendsto (likelihood m x a) atTop (𝓝 (if ∀ i, x i = 0 then 1 else 0)) := by
  have H (c : ℕ) : (∏ i, if x i = c then (1 : ℝ) else 0) =
      if ∀ i, x i = c then 1 else 0 := by
    by_cases he : ∀ i, x i = c
    · simp [he]
    · obtain ⟨i,hi⟩ := not_forall.mp he
      rw [ite_eq_right he]
      exact Finset.prod_eq_zero (Finset.mem_univ i) (by simp [hi])
  unfold likelihood
  constructor
  · simpa only [H] using tendsto_finsetProd Finset.univ
      (fun i _ ↦ mass_at_zero (hx i) ha)
  · simpa only [H] using tendsto_finsetProd Finset.univ
      (fun i _ ↦ mass_at_infinity (hx i) ha)

/-- At held positive alpha and size, exactly the two endpoint-constant
samples fail to attain. Each such sample has supremum one and strict
inequality at finite beta. At size zero beta is arbitrary. -/
theorem fixed_alpha_attainment {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) {a : ℝ} (ha : 0 < a) :
    (m = 0 → ∀ b > 0, likelihood m x a b = 1) ∧
    (0 < m →
      ((∃ b ∈ Ioi (0 : ℝ), IsMaxOn (likelihood m x a) (Ioi 0) b) ↔
        (¬ ∀ i, x i = 0) ∧ (¬ ∀ i, x i = m)) ∧
      (((∀ i, x i = 0) ∨ (∀ i, x i = m)) →
        IsLUB (likelihood m x a '' Ioi 0) 1 ∧ ∀ b > 0, likelihood m x a b < 1)) := by
  refine ⟨fun h b _ ↦ ?_, fun hm ↦ ?_⟩
  · subst m
    have he (i : Fin n) : x i = 0 := Nat.eq_zero_of_le_zero (hx i)
    simp [likelihood,mass,rising,he]
  let f := likelihood m x a
  have hpos (b : ℝ) (hb : 0 < b) : 0 < f b :=
    Finset.prod_pos fun i _ ↦ mass_pos (hx i) ha hb
  have hpath := fixed_alpha_paths x hx ha
  have hf0 : ContinuousAt f 0 := by
    unfold f likelihood
    apply tendsto_finsetProd
    intro i _
    change ContinuousAt (fun t ↦ mass m (x i) a t) _
    unfold mass
    apply ContinuousAt.div
    · exact (rising_cont _).continuousAt.const_mul _
    · exact (rising_cont _).continuousAt.comp (continuousAt_const.add continuousAt_id)
    · simpa using (rising_pos m ha).ne'
  have hcont : ContinuousOn f (Ioi 0) := by
    intro b hb
    apply ContinuousAt.continuousWithinAt
    unfold f likelihood
    apply tendsto_finsetProd
    intro i _
    change ContinuousAt (fun t ↦ mass m (x i) a t) _
    unfold mass
    apply ContinuousAt.div
    · exact (rising_cont _).continuousAt.const_mul _
    · exact (rising_cont _).continuousAt.comp (continuousAt_const.add continuousAt_id)
    · exact (rising_pos m (add_pos ha hb)).ne'
  have hend (he : (∀ i, x i = 0) ∨ (∀ i, x i = m)) :
      IsLUB (f '' Ioi 0) 1 ∧ ∀ b > 0, f b < 1 := by
    have hstrict (b : ℝ) (hb : 0 < b) : f b < 1 := by
      rw [← log_lt_log_iff (hpos b hb) one_pos,log_one,log_likelihood x hx ha hb]
      rcases he with h | h
      · exact (BetaBinomial.allZero_sup hm hn h).2 (a,b) ⟨ha,hb⟩
      · exact (BetaBinomial.allSize_sup hm hn h).2 (a,b) ⟨ha,hb⟩
    refine ⟨⟨?_,?_⟩,hstrict⟩
    · rintro _ ⟨b,hb,rfl⟩
      exact (hstrict b hb).le
    · intro C hC
      rcases he with h | h
      · have H : Tendsto f atTop (𝓝 1) := by simpa [f,h] using hpath.2
        exact le_of_tendsto H ((eventually_gt_atTop 0).mono fun b hb ↦ hC (mem_image_of_mem _ hb))
      · have H : Tendsto f (𝓝[>] 0) (𝓝 1) := by simpa [f,h] using hpath.1
        exact le_of_tendsto H ((show ∀ᶠ t : ℝ in 𝓝[>] (0:ℝ), t ∈ Ioi (0:ℝ) from self_mem_nhdsWithin).mono
          fun b hb ↦ hC (mem_image_of_mem _ hb))
  refine ⟨⟨?_,?_⟩,hend⟩
  · rintro ⟨b,hb,hmax⟩
    constructor <;> intro he
    · have H := hend (Or.inl he)
      exact (H.2 b hb).not_ge (H.1.2 (by
        rintro _ ⟨q,hq,rfl⟩
        exact hmax hq))
    · have H := hend (Or.inr he)
      exact (H.2 b hb).not_ge (H.1.2 (by
        rintro _ ⟨q,hq,rfl⟩
        exact hmax hq))
  · rintro ⟨hzero,hfull⟩
    have ht : Tendsto f atTop (𝓝 0) := by simpa [f,hzero] using hpath.2
    have ht₀ : Tendsto f (𝓝[>] (0 : ℝ)) (𝓝 0) := by simpa [f,hfull] using hpath.1
    have hfz : f 0 = 0 := tendsto_nhds_unique
      (hf0.tendsto.mono_left nhdsWithin_le_nhds) ht₀
    obtain ⟨δ,hδ,hlow⟩ := Metric.eventually_nhds_iff.mp
      (hf0.tendsto.eventually (Iio_mem_nhds (show f 0 < f 1 by rw [hfz]; exact hpos _ one_pos)))
    obtain ⟨R,hhigh⟩ := eventually_atTop.mp (ht.eventually_lt_const (hpos _ one_pos))
    let a₀ := min 1 δ
    let b₀ := max 1 R
    have ha₀ : 0 < a₀ := lt_min one_pos hδ
    have ha₁ : a₀ ≤ 1 := min_le_left _ _
    have hb₁ : 1 ≤ b₀ := le_max_left _ _
    obtain ⟨b,hb,hbest⟩ := isCompact_Icc.exists_isMaxOn
      (⟨1,ha₁,hb₁⟩ : (Icc a₀ b₀).Nonempty)
      (hcont.mono fun _ h ↦ ha₀.trans_le h.1)
    refine ⟨b,ha₀.trans_le hb.1,fun q hq ↦ ?_⟩
    by_cases hq' : q ∈ Icc a₀ b₀
    · exact hbest hq'
    have hbound : f q < f 1 := by
      by_cases hl : q < a₀
      · apply hlow
        rw [Real.dist_eq,sub_zero,abs_of_pos hq]
        exact hl.trans_le (min_le_right _ _)
      · have hh : b₀ < q := lt_of_not_ge fun h ↦ hq' ⟨le_of_not_gt hl,h⟩
        exact hhigh q ((le_max_right _ _).trans hh.le)
    exact hbound.le.trans (hbest ⟨ha₁,hb₁⟩)

private theorem reflect_likelihood {m n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    (a b : ℝ) : likelihood m (fun i ↦ m-x i) b a = likelihood m x a b := by
  unfold likelihood
  apply Finset.prod_congr rfl
  intro i _
  have he : m-(m-x i) = x i := by have := hx i; omega
  simp only [mass,Nat.choose_symm (hx i),he,add_comm b a]
  ring

/-- Holding beta at known size gives the reflected criterion, with the
same size-zero flat branch and excluded endpoint-constant suprema. -/
theorem fixed_beta_attainment {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) {b : ℝ} (hb : 0 < b) :
    (m = 0 → ∀ a > 0, likelihood m x a b = 1) ∧
    (0 < m →
      ((∃ a ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ likelihood m x t b) (Ioi 0) a) ↔
        (¬ ∀ i, x i = 0) ∧ (¬ ∀ i, x i = m)) ∧
      (((∀ i, x i = 0) ∨ (∀ i, x i = m)) →
        IsLUB ((fun t ↦ likelihood m x t b) '' Ioi 0) 1 ∧
          ∀ a > 0, likelihood m x a b < 1)) := by
  let y := fun i ↦ m-x i
  have he : likelihood m y b = fun a ↦ likelihood m x a b :=
    funext fun a ↦ reflect_likelihood x hx a b
  have h₀ : (∀ i, y i = 0) ↔ ∀ i, x i = m := by
    constructor
    · intro H i
      have := H i
      dsimp [y] at this
      have := hx i
      omega
    · intro H i
      simp [y,H i]
  have h₁ : (∀ i, y i = m) ↔ ∀ i, x i = 0 := by
    constructor
    · intro H i
      have := H i
      dsimp [y] at this
      have := hx i
      omega
    · intro H i
      simp [y,H i]
  have H := fixed_alpha_attainment hn y (fun i ↦ Nat.sub_le _ _) hb
  rw [he,h₀,h₁] at H
  exact ⟨H.1,fun hm ↦ by simpa only [and_comm,or_comm] using H.2 hm⟩

private theorem rising_first {m : ℕ} (hm : 0 < m) (a : ℝ) :
    rising m a = a * rising (m-1) (1+a) := by
  have he : m-1+1 = m := Nat.sub_add_cancel hm
  unfold rising
  conv_lhs => rw [← he,Finset.prod_range_succ']
  simp only [Nat.cast_zero,add_zero,Nat.cast_succ]
  rw [mul_comm a]
  congr 1
  apply Finset.prod_congr rfl
  intro j _
  ring

/-- At positive held trial size, sending both shapes to zero at fixed
interior mixing mean leaves only the two endpoint atoms. -/
theorem two_point_path {m k : ℕ} (hm : 0 < m) (hk : k ≤ m) {p : ℝ}
    (hp : p ∈ Ioo 0 1) :
    Tendsto (fun s : ℝ ↦ mass m k (p*s) ((1-p)*s)) (𝓝[>] 0)
      (𝓝 (if k = 0 then 1-p else if k = m then p else 0)) := by
  have h₀ : 0 < 1-p := sub_pos.mpr hp.2
  have hr (j : ℕ) (a : ℝ) :
      Tendsto (fun t : ℝ ↦ rising j (1+a*t)) (𝓝[>] 0) (𝓝 (rising j 1)) := by
    have hc : Continuous (fun t : ℝ ↦ rising j (1+a*t)) :=
      (rising_cont j).comp (by fun_prop)
    have h : Tendsto (fun t : ℝ ↦ rising j (1+a*t)) (𝓝[>] 0) (𝓝 (rising j (1+a*0))) :=
      hc.continuousAt.tendsto.mono_left nhdsWithin_le_nhds
    simpa only [mul_zero,add_zero] using h
  have hd := hr (m-1) 1
  simp only [one_mul] at hd
  have hden := (rising_pos (m-1) one_pos).ne'
  have hh (t : ℝ) : p*t+(1-p)*t = t := by ring
  by_cases hz : k = 0
  · subst k
    have H := ((hr (m-1) (1-p)).div hd hden).const_mul (1-p)
    rw [div_self hden,mul_one] at H
    rw [ite_eq_left rfl]
    apply H.congr'
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    have ht' : 0 < t := ht
    have hD := (rising_pos (m-1) (show 0 < 1+t by positivity)).ne'
    simp only [Pi.div_apply,mass,Nat.choose_zero_right,Nat.cast_one,one_mul,Nat.sub_zero,hh,
      show ∀ b : ℝ, rising 0 b = 1 by intro b; simp [rising],
      rising_first hm t,rising_first hm ((1-p)*t)]
    field_simp [ht'.ne',hD]
  by_cases hf : k = m
  · subst k
    have H := ((hr (m-1) p).div hd hden).const_mul p
    rw [div_self hden,mul_one] at H
    rw [ite_eq_right hz,ite_eq_left rfl]
    apply H.congr'
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    have ht' : 0 < t := ht
    have hD := (rising_pos (m-1) (show 0 < 1+t by positivity)).ne'
    simp only [Pi.div_apply,mass,Nat.choose_self,Nat.cast_one,one_mul,Nat.sub_self,hh,
      show ∀ b : ℝ, rising 0 b = 1 by intro b; simp [rising],
      rising_first hm t,rising_first hm (p*t),mul_one]
    field_simp [ht'.ne',hD]
  · have hk₀ := Nat.pos_of_ne_zero hz
    have hk₁ : 0 < m-k := by omega
    have hT : Tendsto (fun t : ℝ ↦ t) (𝓝[>] 0) (𝓝 0) :=
      tendsto_id.mono_left nhdsWithin_le_nhds
    have H := (((hT.mul (hr (k-1) p)).mul (hr (m-k-1) (1-p))).div hd hden)
      |>.const_mul ((m.choose k : ℝ)*p*(1-p))
    simp only [zero_mul,zero_div,mul_zero] at H
    rw [ite_eq_right hz,ite_eq_right hf]
    apply H.congr'
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    have ht' : 0 < t := ht
    have hD := (rising_pos (m-1) (show 0 < 1+t by positivity)).ne'
    simp only [Pi.div_apply,mass,hh,rising_first hm t,
      rising_first hk₀ (p*t),rising_first hk₁ ((1-p)*t)]
    field_simp [ht'.ne',hD]

end UnivariateML.Discrete4BBinom

end
