module

public import UnivariateML.Family.SkewLSSlash
public import Mathlib.Topology.Order.Compact

import Mathlib.Tactic

set_option linter.style.header false

/-! # Slash location and scale likelihoods -/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.StudentSlash

noncomputable section

open SkewLSSlash

variable {n : ℕ}

private def c : ℝ := 1 / sqrt (2 * π)

private theorem c_pos : 0 < c := by unfold c; positivity

private def A (z : ℝ) : ℝ := if z = 0 then 1 else (1-exp (-z))/z

private theorem A_continuous : Continuous A := by
  apply continuous_iff_continuousAt.mpr
  intro z
  by_cases hz : z = 0
  · subst z
    rw [continuousAt_iff_punctured_nhds]
    have hd : HasDerivAt (fun u : ℝ ↦ 1-exp (-u)) 1 0 := by
      simpa using (((hasDerivAt_id (0 : ℝ)).neg.exp).const_sub 1)
    have ht : Tendsto (fun u : ℝ ↦ (1-exp (-u))/u) (𝓝[≠] 0) (𝓝 1) := by
      simpa [div_eq_mul_inv, mul_comm] using hd.tendsto_slope_zero
    simp only [A]
    refine ht.congr' ?_
    filter_upwards [self_mem_nhdsWithin (s := ({0} : Set ℝ)ᶜ)] with u hu
    simp [A, show u ≠ 0 from hu]
  · have h : ContinuousAt (fun u : ℝ ↦ (1-exp (-u))/u) z := by
      exact (continuousAt_const.sub (continuous_exp.continuousAt.comp (by fun_prop))).div
        continuousAt_id hz
    apply h.congr
    filter_upwards [isOpen_compl_singleton.mem_nhds (show z ∈ ({0} : Set ℝ)ᶜ from hz)] with u hu
    simp [A, show u ≠ 0 from hu]

private theorem density_A (r : ℝ) {s : ℝ} (hs : 0 < s) :
    density r s = c/(2*s) * A (r^2/(2*s^2)) := by
  by_cases hr : r = 0
  · simp [hr, density, A, c]
  · have he : r^2/(2*s^2) ≠ 0 := by positivity
    simp only [density, c, hr, ite_false, A, he]
    field_simp [hr, hs.ne']

private theorem density_positive (r : ℝ) {s : ℝ} (hs : 0 < s) : 0 < density r s := by
  by_cases h : r = 0
  · simp only [density, h, ite_true]; exact div_pos c_pos (by positivity)
  · simp only [density, h, ite_false]
    have hq : 0 < r^2/(2*s^2) := by positivity
    exact mul_pos (div_pos (mul_pos c_pos hs) (sq_pos_of_ne_zero h))
      (sub_pos.mpr (exp_lt_one_iff.mpr (neg_lt_zero.mpr hq)))

/-- Slash likelihood in location and scale coordinates. -/
def likelihood (x : Fin n → ℝ) (m s : ℝ) : ℝ :=
  ∏ i, density (x i-m) s

private theorem likelihood_positive (x : Fin n → ℝ) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    0 < likelihood x m s := Finset.prod_pos fun i _ ↦ density_positive (x i-m) hs

private theorem likelihood_continuous (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    Continuous (fun m ↦ likelihood x m s) := by
  unfold likelihood
  simp_rw [density_A _ hs]
  apply continuous_finsetProd
  intro i _
  exact continuous_const.mul (A_continuous.comp (by fun_prop))

private theorem likelihood_far (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ} (hs : 0 < s) :
    Tendsto (fun m ↦ likelihood x m s) (cocompact ℝ) (𝓝 0) := by
  have ht (i : Fin n) :
      Tendsto (fun m ↦ density (x i-m) s) (cocompact ℝ) (𝓝 0) := by
    have hfar : Tendsto (fun m : ℝ ↦ |x i-m|) (cocompact ℝ) atTop := by
      rw [cocompact_eq_atBot_atTop]
      exact (tendsto_abs_atTop_atTop.comp
        (tendsto_atTop_add_const_left _ _ tendsto_neg_atBot_atTop)).sup
        (tendsto_abs_atBot_atTop.comp
          (tendsto_atBot_add_const_left _ _ tendsto_neg_atTop_atBot))
    have hsq : Tendsto (fun m : ℝ ↦ (x i-m)^2) (cocompact ℝ) atTop := by
      simpa only [Function.comp_def, sq_abs] using
        (tendsto_pow_atTop (by decide : (2 : ℕ) ≠ 0)).comp hfar
    have hex : Tendsto (fun m : ℝ ↦ exp (-((x i-m)^2/(2*s^2)))) (cocompact ℝ) (𝓝 0) :=
      tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp
        (hsq.atTop_div_const (by positivity)))
    have hd := (tendsto_const_nhds (x := c*s)).div_atTop hsq
    have h := hd.mul (hex.const_sub (1 : ℝ))
    rw [sub_zero, zero_mul] at h
    refine h.congr' ?_
    filter_upwards [hsq.eventually_gt_atTop (0 : ℝ)] with m hm
    have hne : x i-m ≠ 0 := by intro he; simp [he] at hm
    simp [density, hne, c]
  have h := tendsto_finsetProd Finset.univ fun i _ ↦ ht i
  simpa [likelihood, zero_pow hn.ne'] using h

/-- With any positive held scale, every nonempty finite sample has a
location maximum. Both infinite-location ends vanish. -/
theorem held_scale_attainment (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ} (hs : 0 < s) :
    (∃ m, IsMaxOn (fun u ↦ likelihood x u s) univ m) ∧
      Tendsto (fun m ↦ likelihood x m s) atBot (𝓝 0) ∧
      Tendsto (fun m ↦ likelihood x m s) atTop (𝓝 0) := by
  have ht := likelihood_far x hn hs
  have hbound := (ht.eventually (Iio_mem_nhds (likelihood_positive x 0 hs))).mono
    (fun _ h ↦ h.le)
  obtain ⟨m,hm⟩ := (likelihood_continuous x hs).exists_forall_ge' 0 hbound
  exact ⟨⟨m,fun _ _ ↦ hm _⟩, ht.mono_left atBot_le_cocompact, ht.mono_left atTop_le_cocompact⟩

private theorem density_even (r s : ℝ) : density (-r) s = density r s := by
  simp [density]

/-- At scale one the separated symmetric sample `(-10, 10)` has
two distinct reflected global maxima. -/
theorem held_scale_multiple_maxima :
    ∃ m : ℝ, m ≠ -m ∧
      IsMaxOn (fun u ↦ likelihood ![-10,10] u 1) univ m ∧
      IsMaxOn (fun u ↦ likelihood ![-10,10] u 1) univ (-m) := by
  let x : Fin 2 → ℝ := ![-10,10]
  have hEval (m : ℝ) : likelihood x m 1 = density (-10-m) 1 * density (10-m) 1 := by
    simp [x, likelihood, Fin.prod_univ_two]
  have hEven (m : ℝ) : likelihood x (-m) 1 = likelihood x m 1 := by
    rw [hEval, hEval, show (-10:ℝ)- -m = -(10-m) by ring,
      show (10:ℝ)- -m = -(-10-m) by ring, density_even, density_even, mul_comm]
  have hCentre : likelihood x 0 1 < likelihood x 10 1 := by
    have hD (r : ℝ) : density r 1 =
        if r = 0 then c/2 else c/r^2 * (1-exp (-(r^2/2))) := by
      simp [density, c]
    rw [hEval, hEval]
    simp only [hD]
    norm_num
    have he : exp (-200 : ℝ) < 1/2 := by
      rw [exp_neg, inv_lt_comm₀ (exp_pos _) (by norm_num)]
      linarith [add_one_le_exp (200 : ℝ)]
    have ht : 0 ≤ 1 - exp (-50 : ℝ) := by
      linarith [exp_lt_one_iff.mpr (show (-50 : ℝ) < 0 by norm_num)]
    have ht' : 1 - exp (-50 : ℝ) ≤ 1 := by linarith [exp_pos (-50 : ℝ)]
    have h₁ : (1-exp (-50 : ℝ))^2 ≤ 1 := by nlinarith [mul_nonneg ht (sub_nonneg.mpr ht')]
    have hc2 : 0 < c^2 := sq_pos_of_pos c_pos
    nlinarith [mul_pos hc2 (show 0 < 1/2-exp (-200 : ℝ) by linarith),
      mul_nonneg hc2.le (sub_nonneg.mpr h₁)]
  obtain ⟨m,hm⟩ := (held_scale_attainment x (by decide) (by norm_num : (0 : ℝ) < 1)).1
  have hm0 : m ≠ 0 := by
    rintro rfl
    exact hCentre.not_ge (hm (mem_univ 10))
  refine ⟨m, by intro h; exact hm0 (by linarith), hm, ?_⟩
  intro u hu
  change likelihood x u 1 ≤ likelihood x (-m) 1
  rw [hEven]
  exact hm hu

/-- At a held location, half ties yield the finite unattained scale
supremum and a strict majority yields divergence. -/
theorem held_location_failure (x : Fin n → ℝ) (hn : 0 < n) (m : ℝ) :
    let r := fun i ↦ x i-m
    (n < 2 * ties r → Tendsto (logLik r) (𝓝[>] 0) atTop) ∧
    (n = 2 * ties r → Tendsto (logLik r) (𝓝[>] 0) (𝓝 (collapseLog r)) ∧
      IsLUB (logLik r '' Ioi 0) (collapseLog r) ∧
      ∀ s > 0, logLik r s < collapseLog r) :=
  ⟨held_centre_unbounded _, held_centre_half_tie hn _⟩

private def tied (x : Fin n → ℝ) (b : ℝ) : Finset (Fin n) :=
  Finset.univ.filter fun i ↦ x i = b

private def off (x : Fin n → ℝ) (b : ℝ) : Finset (Fin n) :=
  Finset.univ.filter fun i ↦ x i ≠ b

private theorem tied_eq (x : Fin n → ℝ) (b : ℝ) :
    (tied x b).card = ties (fun i ↦ x i-b) := by
  simp [tied, ties, sub_eq_zero]

private theorem count_split (x : Fin n → ℝ) (b : ℝ) :
    (tied x b).card + (off x b).card = n := by
  unfold tied off
  rw [Finset.card_filter_add_card_filter_not, Finset.card_univ, Fintype.card_fin]

private theorem density_peak (r : ℝ) {s : ℝ} (hs : 0 < s) : density r s ≤ c/(2*s) := by
  rw [density_A r hs]
  apply mul_le_of_le_one_right (div_nonneg c_pos.le (by positivity))
  let z := r^2/(2*s^2)
  have hz : 0 ≤ z := by dsimp [z]; positivity
  change A z ≤ 1
  by_cases he : z = 0
  · simp [A, he]
  · simp only [A, ite_eq_right he]
    apply (div_le_iff₀ (lt_of_le_of_ne hz (Ne.symm he))).2
    linarith [add_one_le_exp (-z)]

private theorem off_formula {r s : ℝ} (hr : r ≠ 0) (hs : 0 < s) :
    density r s / s = (c/r^2) * (1 - exp (-(r^2/(2*s^2)))) := by
  simp only [density, hr, ite_false, c]
  field_simp [hs.ne']

private theorem off_bound {r s : ℝ} (hr : r ≠ 0) (hs : 0 < s) :
    density r s / s ≤ c/r^2 := by
  rw [off_formula hr hs]
  exact mul_le_of_le_one_right (div_nonneg c_pos.le (by positivity))
    (by linarith [exp_pos (-(r^2/(2*s^2)))])

private def partialTrace (x : Fin n → ℝ) (b m : ℝ) : ℝ :=
  (c/2)^((tied x b).card) * ∏ i ∈ off x b, c/(x i-m)^2

/-- The vertical collapse value at a half-tied atom. -/
def collapseValue (x : Fin n → ℝ) (b : ℝ) : ℝ :=
  ((1 / sqrt (2 * π))/2)^(ties fun i ↦ x i-b) *
    ∏ i ∈ Finset.univ.filter (fun i ↦ x i ≠ b), (1 / sqrt (2 * π))/(x i-b)^2

private theorem trace_eq (x : Fin n → ℝ) (b : ℝ) : partialTrace x b b = collapseValue x b := by
  simp [partialTrace, collapseValue, off, tied_eq, c]

private theorem split_likelihood (x : Fin n → ℝ) (b m : ℝ) {s : ℝ} (hs : 0 < s)
    (hc : (tied x b).card ≤ (off x b).card) :
    likelihood x m s =
      ((∏ i ∈ tied x b, density (x i-m) s * s) *
        (∏ i ∈ off x b, density (x i-m) s / s)) *
        s^((off x b).card - (tied x b).card) := by
  unfold likelihood
  rw [← Finset.prod_filter_mul_prod_filter_not Finset.univ (fun i ↦ x i = b)]
  change _ * _ =
    ((∏ i ∈ tied x b, density (x i-m) s * s) *
      (∏ i ∈ off x b, density (x i-m) s / s)) *
      s^((off x b).card - (tied x b).card)
  simp only [Finset.prod_mul_distrib, Finset.prod_div_distrib, Finset.prod_const]
  rw [show s^((off x b).card) = s^((tied x b).card) *
      s^((off x b).card - (tied x b).card) by rw [← pow_add, Nat.add_sub_of_le hc]]
  field_simp [hs.ne']
  rfl

private theorem nonmajority_bound (x : Fin n → ℝ) (b m : ℝ) {s : ℝ} (hs : 0 < s)
    (hc : (tied x b).card ≤ (off x b).card)
    (hm : ∀ i ∈ off x b, x i ≠ m) :
    likelihood x m s ≤ partialTrace x b m * s^((off x b).card-(tied x b).card) := by
  rw [split_likelihood x b m hs hc, partialTrace,
    ← Finset.prod_const (s := tied x b) (c/2)]
  apply mul_le_mul_of_nonneg_right _ (by positivity)
  apply mul_le_mul
  · apply Finset.prod_le_prod₀
    · intro i _
      exact mul_nonneg (density_positive (x i-m) hs).le hs.le
    · intro i _
      have h := mul_le_mul_of_nonneg_right (density_peak (x i-m) hs) hs.le
      convert h using 1
      field_simp [hs.ne']
  · exact Finset.prod_le_prod₀ (fun i _ ↦ div_nonneg (density_positive (x i-m) hs).le hs.le)
      (fun i hi ↦ off_bound (sub_ne_zero.mpr (hm i hi)) hs)
  · exact Finset.prod_nonneg fun i _ ↦ div_nonneg (density_positive (x i-m) hs).le hs.le
  · exact Finset.prod_nonneg fun _ _ ↦ div_nonneg c_pos.le (by norm_num)

/-- The half-tie moving collapse path at finite scaled displacement.
At zero displacement it gives the vertical collapse value. -/
theorem moving_half_tie {ι : Type*} {l : Filter ι} (x : Fin n → ℝ) (b t : ℝ)
    (he : n = 2 * ties (fun i ↦ x i-b)) {m s : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (hs : Tendsto s l (𝓝[>] 0))
    (ht : Tendsto (fun j ↦ (m j-b)/s j) l (𝓝 t)) :
    Tendsto (fun j ↦ likelihood x (m j) (s j)) l
      (𝓝 (collapseValue x b *
        (if t = 0 then 1 else (1-exp (-(t^2/2)))/(t^2/2)) ^ (ties fun i ↦ x i-b))) := by
  have hpos : ∀ᶠ j in l, 0 < s j := (tendsto_nhdsWithin_iff.mp hs).2
  have hcount : (tied x b).card = (off x b).card := by
    have h := count_split x b
    rw [tied_eq] at h ⊢
    omega
  have hOn (i : Fin n) (hi : i ∈ tied x b) :
      Tendsto (fun j ↦ density (x i-m j) (s j) * s j) l (𝓝 ((c/2)*A (t^2/2))) := by
    have hi' : x i = b := (Finset.mem_filter.mp hi).2
    have hv := ((ht.pow 2).div_const 2)
    refine ((A_continuous.continuousAt.tendsto.comp hv).const_mul (c/2)).congr' ?_
    filter_upwards [hpos] with j hj
    rw [hi', density_A _ hj]
    have h : (b-m j)^2/(2*s j^2) = ((m j-b)/s j)^2/2 := by
      field_simp [hj.ne']; ring
    change (c/2)*A (((m j-b)/s j)^2/2) = _
    rw [← h]
    field_simp [hj.ne']
  have hOff (i : Fin n) (hi : i ∈ off x b) :
      Tendsto (fun j ↦ density (x i-m j) (s j) / s j) l (𝓝 (c/(x i-b)^2)) := by
    have hi' : x i ≠ b := (Finset.mem_filter.mp hi).2
    have hsq := ((tendsto_const_nhds (x := x i)).sub hm).pow 2
    have hsqpos : 0 < (x i-b)^2 := sq_pos_of_ne_zero (sub_ne_zero.mpr hi')
    have hinv : Tendsto (fun j ↦ (s j)⁻¹) l atTop := tendsto_inv_nhdsGT_zero.comp hs
    have hpow : Tendsto (fun j ↦ (2*s j^2)⁻¹) l atTop := by
      refine ((hinv.atTop_pow₀ (by decide : (2 : ℕ) ≠ 0)).const_mul_atTop (by norm_num :
        0 < (2 : ℝ)⁻¹)).congr ?_
      intro j
      simp [inv_pow, mul_comm]
    have harg : Tendsto (fun j ↦ (x i-m j)^2/(2*s j^2)) l atTop := by
      simpa [div_eq_mul_inv] using hsq.pos_mul_atTop hsqpos hpow
    have hex := tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp harg)
    have h := ((tendsto_const_nhds (x := c)).div hsq hsqpos.ne').mul (hex.const_sub (1 : ℝ))
    rw [sub_zero, mul_one] at h
    refine h.congr' ?_
    filter_upwards [hpos, hsq.eventually (Ioi_mem_nhds hsqpos)] with j hj hj'
    have hne : x i-m j ≠ 0 := by intro h; simp [h] at hj'
    exact (off_formula hne hj).symm
  have h := (tendsto_finsetProd (tied x b) hOn).mul (tendsto_finsetProd (off x b) hOff)
  rw [Finset.prod_const, mul_pow, mul_assoc, mul_comm (A (t^2/2) ^ _), ← mul_assoc] at h
  rw [← partialTrace, trace_eq, tied_eq] at h
  have hA : A (t^2/2) = if t = 0 then 1 else (1-exp (-(t^2/2)))/(t^2/2) := by
    by_cases ht' : t = 0
    · simp [A, ht']
    · have hne : t^2/2 ≠ 0 := by positivity
      simp [A, ht', hne]
  rw [← hA]
  refine h.congr' ?_
  filter_upwards [hpos] with j hj
  rw [split_likelihood x b (m j) hj hcount.le, hcount, Nat.sub_self, pow_zero, mul_one]

private theorem vertical_half {x : Fin n → ℝ} {b : ℝ}
    (he : n = 2 * ties (fun i ↦ x i-b)) :
    Tendsto (fun s ↦ likelihood x b s) (𝓝[>] 0) (𝓝 (collapseValue x b)) := by
  simpa using moving_half_tie x b 0 he (m := fun _ : ℝ ↦ b) (s := id)
    tendsto_const_nhds tendsto_id (by simp)

private theorem small_scale_local (x : Fin n → ℝ) (b B : ℝ) (hB : 0 < B)
    (hle : 2 * ties (fun i ↦ x i-b) ≤ n)
    (hface : n = 2 * ties (fun i ↦ x i-b) → collapseValue x b < B) :
    ∀ᶠ p : ℝ × ℝ in (𝓝 b) ×ˢ (𝓝[>] 0), likelihood x p.1 p.2 < B := by
  let l : Filter (ℝ × ℝ) := (𝓝 b) ×ˢ (𝓝[>] 0)
  have hfst : Tendsto Prod.fst l (𝓝 b) := Filter.tendsto_fst
  have hsnd : Tendsto Prod.snd l (𝓝[>] 0) := Filter.tendsto_snd
  have hs0 := (tendsto_nhdsWithin_iff.mp hsnd).1
  have hpos := (tendsto_nhdsWithin_iff.mp hsnd).2
  have heq := count_split x b
  have hT := tied_eq x b
  have hcard : (tied x b).card ≤ (off x b).card := by omega
  let e := (off x b).card - (tied x b).card
  have hpart : Tendsto (fun p : ℝ × ℝ ↦ partialTrace x b p.1) l (𝓝 (collapseValue x b)) := by
    have H (i : Fin n) (hi : i ∈ off x b) :
        Tendsto (fun p : ℝ × ℝ ↦ c/(x i-p.1)^2) l (𝓝 (c/(x i-b)^2)) :=
      tendsto_const_nhds.div (((tendsto_const_nhds (x := x i)).sub hfst).pow 2)
        (pow_ne_zero _ (sub_ne_zero.mpr ((Finset.mem_filter.mp hi).2)))
    rw [← trace_eq]
    exact (tendsto_finsetProd _ H).const_mul _
  have hlimit : collapseValue x b * (0 : ℝ)^e < B := by
    by_cases he : n = 2 * ties (fun i ↦ x i-b)
    · have hz : e = 0 := by dsimp [e]; omega
      simpa [hz] using hface he
    · have hnz : e ≠ 0 := by dsimp [e]; omega
      simpa [zero_pow hnz] using hB
  have hoff : ∀ᶠ p : ℝ × ℝ in l, ∀ i ∈ off x b, x i ≠ p.1 := by
    apply (off x b).eventually_all.mpr
    intro i hi
    have hi' : b ≠ x i := (Finset.mem_filter.mp hi).2.symm
    exact (hfst.eventually (isOpen_compl_singleton.mem_nhds
      (show b ∈ ({x i} : Set ℝ)ᶜ from hi'))).mono fun p hp ↦
        (show p.1 ≠ x i from hp).symm
  filter_upwards [hpos, hoff, (hpart.mul (hs0.pow e)).eventually (Iio_mem_nhds hlimit)]
    with p hp ho hL
  exact (nonmajority_bound x b p.1 hp hcard ho).trans_lt hL

private theorem small_scale_uniform (x : Fin n → ℝ) (M B : ℝ) (hB : 0 < B)
    (hle : ∀ b, 2 * ties (fun i ↦ x i-b) ≤ n)
    (hface : ∀ b, n = 2 * ties (fun i ↦ x i-b) → collapseValue x b < B) :
    ∃ ε > 0, ∀ m ∈ Icc (-M) M, ∀ s ∈ Ioo 0 ε, likelihood x m s < B := by
  have H (b : ℝ) := Filter.eventually_prod_iff.mp (small_scale_local x b B hB (hle b) (hface b))
  choose U hU V hV hbox using H
  obtain ⟨t, _, hcover⟩ := (isCompact_Icc (a := -M) (b := M)).elim_nhds_subcover
    (fun b ↦ {m | U b m}) (fun b _ ↦ hU b)
  have hv : ∀ᶠ s : ℝ in 𝓝[>] 0, ∀ b ∈ t, V b s := t.eventually_all.mpr (fun b _ ↦ hV b)
  obtain ⟨ε,hε,hεsub⟩ := Metric.mem_nhdsWithin_iff.mp hv
  refine ⟨ε,hε,fun m hm s hs ↦ ?_⟩
  have hmem := hcover hm
  simp only [mem_iUnion, mem_ofPred_eq] at hmem
  obtain ⟨b,hb,hbu⟩ := hmem
  have hsv : ∀ b ∈ t, V b s :=
    hεsub ⟨by rw [Metric.mem_ball, Real.dist_eq, sub_zero, abs_of_pos hs.1]; exact hs.2, hs.1⟩
  exact hbox b hbu (hsv b hb)

private theorem density_far_uniform {r s : ℝ} (hr : r ≠ 0) (hs : 0 < s) :
    density r s ≤ c/|r| := by
  have hR : 0 < |r| := abs_pos.mpr hr
  by_cases h : s ≤ |r|
  · have H := (div_le_iff₀ hs).mp (off_bound hr hs)
    calc
      _ ≤ c/r^2*s := H
      _ ≤ c/r^2*|r| := mul_le_mul_of_nonneg_left h (div_nonneg c_pos.le (sq_nonneg _))
      _ = _ := by rw [← sq_abs r]; field_simp [hR.ne']
  · exact (density_peak r hs).trans (div_le_div_of_nonneg_left c_pos.le hR (by linarith))

private theorem joint_continuousOn (x : Fin n → ℝ) :
    ContinuousOn (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) (univ ×ˢ Ioi 0) := by
  apply ContinuousOn.congr
    (f := fun p : ℝ × ℝ ↦ ∏ i, c/(2*p.2) * A ((x i-p.1)^2/(2*p.2^2)))
  · apply continuousOn_finsetProd
    intro i _
    have h1 : ContinuousOn (fun p : ℝ × ℝ ↦ c/(2*p.2)) (univ ×ˢ Ioi 0) :=
      continuousOn_const.div (by fun_prop) (fun p hp ↦ by have h : 0 < p.2 := hp.2; positivity)
    have h2 : ContinuousOn (fun p : ℝ × ℝ ↦ (x i-p.1)^2/(2*p.2^2)) (univ ×ˢ Ioi 0) :=
      ContinuousOn.div (by fun_prop) (by fun_prop) (fun p hp ↦ by
        have h : 0 < p.2 := hp.2
        positivity)
    exact h1.mul (A_continuous.comp_continuousOn h2)
  · intro p hp
    unfold likelihood
    exact Finset.prod_congr rfl (fun i _ ↦ density_A _ hp.2)

set_option maxHeartbeats 900000 in
-- Uniform bounds and the compact superlevel witness elaborate together.
private theorem attain_above_faces (x : Fin n → ℝ) (hn : 0 < n)
    (hle : ∀ b, 2 * ties (fun i ↦ x i-b) ≤ n)
    (m s : ℝ) (hs : 0 < s)
    (hface : ∀ b, n = 2 * ties (fun i ↦ x i-b) → collapseValue x b < likelihood x m s) :
    ∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) (univ ×ˢ Ioi 0) p := by
  let B := likelihood x m s
  have hB : 0 < B := likelihood_positive x m hs
  let F := min B 1 / 2
  have hF : 0 < F := half_pos (lt_min hB one_pos)
  have hF1 : F ≤ 1 := by dsimp [F]; linarith [min_le_right B (1 : ℝ)]
  have hFB : F < B := by dsimp [F]; linarith [min_le_left B (1 : ℝ)]
  have hpow : F^n < B := by
    cases n with
    | zero => omega
    | succ k =>
      rw [pow_succ]
      exact (mul_le_of_le_one_left hF.le (pow_le_one₀ hF.le hF1)).trans_lt hFB
  let H := c/F
  have hH : 0 < H := div_pos c_pos hF
  have hF' : c/H = F := div_div_cancel₀ c_pos.ne'
  let M := (∑ i, |x i|) + H
  have outside (u a : ℝ) (ha : 0 < a) (h : H ≤ a ∨ M ≤ |u|) :
      likelihood x u a < B := by
    apply lt_of_le_of_lt _ hpow
    unfold likelihood
    rw [show F^n = ∏ _i : Fin n, F by simp]
    apply Finset.prod_le_prod₀ (fun i _ ↦ (density_positive (x i-u) ha).le)
    intro i _
    rcases h with h | h
    · exact (density_peak _ ha).trans (hF' ▸ div_le_div_of_nonneg_left c_pos.le hH (by linarith))
    · have htri := abs_add_le (u-x i) (x i)
      rw [sub_add_cancel, abs_sub_comm u (x i)] at htri
      have hi : |x i| ≤ ∑ j, |x j| :=
        Finset.single_le_sum (fun j _ ↦ abs_nonneg (x j)) (Finset.mem_univ i)
      have hr : H ≤ |x i-u| := by dsimp [M] at h; linarith
      have hr' : x i-u ≠ 0 := abs_pos.mp (hH.trans_le hr)
      exact (density_far_uniform hr' ha).trans (hF' ▸ div_le_div_of_nonneg_left c_pos.le hH hr)
  obtain ⟨ε,hε,hsmall⟩ := small_scale_uniform x M B hB hle hface
  let K := Icc (-M) M ×ˢ Icc ε H
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  have hKsub : K ⊆ univ ×ˢ Ioi 0 := fun p hp ↦ ⟨mem_univ _,hε.trans_le hp.2.1⟩
  have hmem (u a : ℝ) (ha : 0 < a) (h : B ≤ likelihood x u a) : (u,a) ∈ K := by
    have hA : a ≤ H := le_of_not_gt (fun hh ↦ (outside u a ha (Or.inl hh.le)).not_ge h)
    have hM : |u| ≤ M := le_of_not_gt (fun hh ↦ (outside u a ha (Or.inr hh.le)).not_ge h)
    refine ⟨abs_le.mp hM, ⟨le_of_not_gt (fun hh ↦ ?_),hA⟩⟩
    exact (hsmall u (abs_le.mp hM) a ⟨ha,hh⟩).not_ge h
  have href := hmem m s hs le_rfl
  obtain ⟨p,hp,hm⟩ := hK.exists_isMaxOn ⟨_,href⟩ ((joint_continuousOn x).mono hKsub)
  refine ⟨p,hKsub hp,fun q hq ↦ ?_⟩
  by_cases h : B ≤ likelihood x q.1 q.2
  · exact hm (hmem q.1 q.2 hq.2 h)
  · exact (le_of_not_ge h).trans (hm href)

/-- A strict majority atom makes the joint slash likelihood unbounded. -/
theorem joint_majority_unbounded (x : Fin n → ℝ) (b : ℝ)
    (h : n < 2 * ties (fun i ↦ x i-b)) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) '' (univ ×ˢ Ioi 0)) := by
  rintro ⟨B,hB⟩
  have ht := tendsto_exp_atTop.comp (held_centre_unbounded (fun i ↦ x i-b) h)
  have hpos : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
  obtain ⟨s,hs,hgt⟩ := (hpos.and (ht.eventually_gt_atTop B)).exists
  have hL : exp (logLik (fun i ↦ x i-b) s) = likelihood x b s := by
    unfold logLik likelihood
    rw [exp_sum]
    exact Finset.prod_congr rfl (fun i _ ↦ exp_log (density_positive (x i-b) hs))
  change B < exp (logLik (fun i ↦ x i-b) s) at hgt
  rw [hL] at hgt
  exact hgt.not_ge (hB ⟨(b,s),⟨mem_univ _,hs⟩,rfl⟩)

/-- At most half mass everywhere is necessary for a joint fit. In that
regime attainment is equivalent to a positive-scale witness reaching every
critical collapse value. With strict masses the comparison is vacuous. -/
theorem joint_attainment_iff (x : Fin n → ℝ) (hn : 0 < n) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) (univ ×ˢ Ioi 0) p) ↔
      (∀ b, 2 * ties (fun i ↦ x i-b) ≤ n) ∧
      ∃ m s : ℝ, 0 < s ∧
        ∀ b, n = 2 * ties (fun i ↦ x i-b) → collapseValue x b ≤ likelihood x m s := by
  constructor
  · rintro ⟨p,hp,hm⟩
    have hb : BddAbove ((fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) '' (univ ×ˢ Ioi 0)) :=
      ⟨likelihood x p.1 p.2, by rintro _ ⟨q,hq,rfl⟩; exact hm hq⟩
    refine ⟨fun b ↦ le_of_not_gt (fun h ↦ joint_majority_unbounded x b h hb),
      p.1,p.2,hp.2, fun b he ↦ ?_⟩
    apply le_of_tendsto (vertical_half he)
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
    exact hm (show (b,s) ∈ univ ×ˢ Ioi 0 from ⟨mem_univ _,hs⟩)
  · rintro ⟨hle,m,s,hs,hface⟩
    by_cases hm : IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) (univ ×ˢ Ioi 0) (m,s)
    · exact ⟨(m,s),⟨mem_univ _,hs⟩,hm⟩
    · unfold IsMaxOn IsMaxFilter at hm
      rw [eventually_principal] at hm
      push Not at hm
      obtain ⟨p,hp,hgt⟩ := hm
      exact attain_above_faces x hn hle p.1 p.2 hp.2
        (fun b he ↦ (hface b he).trans_lt hgt)

private theorem collapse_positive (x : Fin n → ℝ) (b : ℝ) : 0 < collapseValue x b := by
  rw [← trace_eq, partialTrace]
  apply mul_pos (pow_pos (half_pos c_pos) _)
  apply Finset.prod_pos
  intro i hi
  exact div_pos c_pos (sq_pos_of_ne_zero (sub_ne_zero.mpr (Finset.mem_filter.mp hi).2))

/-- The joint envelope at a half-tied atom is the vertical collapse value.
Every approaching path is eventually below each strict upper bound for
this value, and the vertical path reaches it as a limit. -/
theorem half_tie_envelope (x : Fin n → ℝ) (b : ℝ)
    (he : n = 2 * ties (fun i ↦ x i-b)) :
    Tendsto (fun s ↦ likelihood x b s) (𝓝[>] 0) (𝓝 (collapseValue x b)) ∧
      ∀ B > collapseValue x b, ∀ᶠ p : ℝ × ℝ in (𝓝 b) ×ˢ (𝓝[>] 0),
        likelihood x p.1 p.2 < B :=
  ⟨vertical_half he, fun B hB ↦
    small_scale_local x b B ((collapse_positive x b).trans hB) he.ge (fun _ ↦ hB)⟩

/-- At half mass the greatest critical collapse value exists. Failure of
attainment is equivalent to strict loss at every finite point, in which
case this collapse value is the exact supremum. -/
theorem joint_half_tie_failure (x : Fin n → ℝ) (hn : 0 < n)
    (hle : ∀ b, 2 * ties (fun i ↦ x i-b) ≤ n)
    (hh : ∃ b, n = 2 * ties (fun i ↦ x i-b)) :
    let D := univ ×ˢ Ioi (0 : ℝ)
    let F := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    ∃ b, n = 2 * ties (fun i ↦ x i-b) ∧
      (∀ z, n = 2 * ties (fun i ↦ x i-z) → collapseValue x z ≤ collapseValue x b) ∧
      ((¬ ∃ p ∈ D, IsMaxOn F D p) ↔
        IsLUB (F '' D) (collapseValue x b) ∧ ∀ p ∈ D, F p < collapseValue x b) := by
  dsimp
  have hatom (b : ℝ) (he : n = 2 * ties (fun i ↦ x i-b)) : ∃ i, x i = b := by
    have h : 0 < ties (fun i ↦ x i-b) := by omega
    rw [← tied_eq] at h
    obtain ⟨i,hi⟩ := Finset.card_pos.mp h
    exact ⟨i,(Finset.mem_filter.mp hi).2⟩
  let S := Finset.univ.filter (fun i : Fin n ↦ n = 2 * ties (fun j ↦ x j-x i))
  have hS : S.Nonempty := by
    obtain ⟨z,hz⟩ := hh
    obtain ⟨i,hi⟩ := hatom z hz
    exact ⟨i,Finset.mem_filter.mpr ⟨Finset.mem_univ _, by rwa [hi]⟩⟩
  obtain ⟨i,hi,hm⟩ := Finset.exists_max_image S (fun i ↦ collapseValue x (x i)) hS
  have he : n = 2 * ties (fun j ↦ x j-x i) := (Finset.mem_filter.mp hi).2
  have hface (z : ℝ) (hz : n = 2 * ties (fun j ↦ x j-z)) :
      collapseValue x z ≤ collapseValue x (x i) := by
    obtain ⟨j,hj⟩ := hatom z hz
    rw [← hj]
    exact hm j (Finset.mem_filter.mpr ⟨Finset.mem_univ _, by rwa [hj]⟩)
  refine ⟨x i,he,hface,?_⟩
  have hlim := vertical_half he
  constructor
  · intro h
    have hstrict (p : ℝ × ℝ) (hp : p ∈ univ ×ˢ Ioi 0) :
        likelihood x p.1 p.2 < collapseValue x (x i) := by
      by_contra! hnot
      exact h ((joint_attainment_iff x hn).2
        ⟨hle,p.1,p.2,hp.2,fun z hz ↦ (hface z hz).trans hnot⟩)
    refine ⟨⟨?_,?_⟩,hstrict⟩
    · rintro _ ⟨p,hp,rfl⟩
      exact (hstrict p hp).le
    · intro y hy
      apply le_of_tendsto hlim
      filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
      exact hy ⟨(x i,s),⟨mem_univ _,hs⟩,rfl⟩
  · rintro ⟨h,hs⟩ ⟨p,hp,hm'⟩
    have hU : likelihood x p.1 p.2 ∈ upperBounds
        ((fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) '' (univ ×ˢ Ioi 0)) := by
      rintro _ ⟨q,hq,rfl⟩
      exact hm' hq
    exact (hs p hp).not_ge (h.2 hU)

end
end UnivariateML.StudentSlash
end
