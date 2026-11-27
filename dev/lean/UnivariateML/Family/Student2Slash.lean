module

public import Mathlib.Analysis.Analytic.IsolatedZeros

public import UnivariateML.Family.StudentSlash

public import Mathlib.Analysis.Calculus.DSlope
public import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Algebra.BigOperators.Fin
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Joint slash likelihood at and above half mass

-/

@[expose] public section

open Set Filter Topology

namespace UnivariateML.Student2Slash

/-- A majority gives divergence. With no majority, a half-mass escape
is an unattained supremum exactly when all finite points lose to the
largest half-mass trace. A product of exact paired blocks cannot attain. -/
theorem joint_majority_and_half_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let F := fun p : ℝ × ℝ ↦ StudentSlash.likelihood x p.1 p.2
    let k := fun b ↦ SkewLSSlash.ties (fun i ↦ x i - b)
    ((∃ b, n < 2 * k b) → ¬ BddAbove (F '' D)) ∧
    ((∀ b, 2 * k b ≤ n) → (∃ b, n = 2 * k b) →
      ∃ b, n = 2 * k b ∧
        (∀ z, n = 2 * k z → StudentSlash.collapseValue x z ≤ StudentSlash.collapseValue x b) ∧
        ((¬ ∃ p ∈ D, IsMaxOn F D p) ↔
          IsLUB (F '' D) (StudentSlash.collapseValue x b) ∧
          ∀ p ∈ D, F p < StudentSlash.collapseValue x b)) ∧
    (∀ {r : ℕ}, 0 < r → ∀ (d : Fin r → ℝ), (∀ i, 0 < d i) →
      ∀ (e : Fin r × Fin 4 ≃ Fin n) (b : ℝ),
      (∀ i, x (e (i, 0)) = b ∧ x (e (i, 1)) = b ∧
        x (e (i, 2)) = b - d i ∧ x (e (i, 3)) = b + d i) →
      ¬ ∃ p ∈ D, IsMaxOn F D p) := by
  dsimp only
  refine ⟨?_, StudentSlash.joint_half_tie_failure x hn, ?_⟩
  · rintro ⟨b, hb⟩
    exact StudentSlash.joint_majority_unbounded x b hb
  · intro r hr d hd e b hx
    have hBlock (m s : ℝ) :
        StudentSlash.likelihood x (b + m) s = ∏ i, SkewLSSlash.pairedBlock (d i) m s := by
      unfold StudentSlash.likelihood
      rw [← e.prod_comp (fun j ↦ SkewLSSlash.density (x j - (b + m)) s),
        Fintype.prod_prod_type]
      apply Finset.prod_congr rfl
      intro i _
      rw [Fin.prod_univ_four, (hx i).1, (hx i).2.1, (hx i).2.2.1, (hx i).2.2.2]
      rw [show b - (b + m) = -m by ring,
        show b - d i - (b + m) = -d i - m by ring,
        show b + d i - (b + m) = d i - m by ring]
      rw [SkewLSSlash.pairedBlock, pow_two]
    obtain ⟨hlub, hstrict⟩ := SkewLSSlash.paired_collapse_supremum hr d hd
    rintro ⟨p, hp, hm⟩
    have hbound : (∏ i, SkewLSSlash.pairedTrace (d i)) ≤ StudentSlash.likelihood x p.1 p.2 := by
      apply hlub.2
      rintro _ ⟨q, hq, rfl⟩
      change (∏ i, SkewLSSlash.pairedBlock (d i) q.1 q.2) ≤ _
      rw [← hBlock]
      exact hm (show (b + q.1, q.2) ∈ (univ : Set ℝ) ×ˢ Ioi 0 from ⟨mem_univ _, hq.2⟩)
    have hlt := hstrict (p.1 - b) p.2 hp.2
    rw [← hBlock, show b + (p.1 - b) = p.1 by ring] at hlt
    exact hlt.not_ge hbound

open Real StudentSlash
open SkewLSSlash (density)

private theorem density_even (r s : ℝ) : density (-r) s = density r s := by
  simp [density]

/-- At scale one the separated symmetric sample `(-100, 100)` has
two distinct reflected global maxima. -/
theorem held_scale_reflected_maxima_100 :
    ∃ m : ℝ, m ≠ -m ∧
      IsMaxOn (fun u ↦ likelihood ![-100,100] u 1) univ m ∧
      IsMaxOn (fun u ↦ likelihood ![-100,100] u 1) univ (-m) := by
  let x : Fin 2 → ℝ := ![-100,100]
  have hEval (m : ℝ) : likelihood x m 1 = density (-100-m) 1 * density (100-m) 1 := by
    simp [x, likelihood, Fin.prod_univ_two]
  have hEven (m : ℝ) : likelihood x (-m) 1 = likelihood x m 1 := by
    rw [hEval, hEval, show (-100:ℝ)- -m = -(100-m) by ring,
      show (100:ℝ)- -m = -(-100-m) by ring, density_even, density_even, mul_comm]
  have hCentre : likelihood x 0 1 < likelihood x 100 1 := by
    let c : ℝ := 1 / sqrt (2 * π)
    have c_pos : 0 < c := by unfold c; positivity
    have hD (r : ℝ) : density r 1 =
        if r = 0 then c/2 else c/r^2 * (1-exp (-(r^2/2))) := by
      simp [density, c]
    rw [hEval, hEval]
    simp only [hD]
    norm_num
    have he : exp (-20000 : ℝ) < 1/2 := by
      rw [exp_neg, inv_lt_comm₀ (exp_pos _) (by norm_num)]
      linarith [add_one_le_exp (20000 : ℝ)]
    have ht : 0 ≤ 1 - exp (-5000 : ℝ) := by
      linarith [exp_lt_one_iff.mpr (show (-5000 : ℝ) < 0 by norm_num)]
    have ht' : 1 - exp (-5000 : ℝ) ≤ 1 := by linarith [exp_pos (-5000 : ℝ)]
    have h₁ : (1-exp (-5000 : ℝ))^2 ≤ 1 := by nlinarith [mul_nonneg ht (sub_nonneg.mpr ht')]
    have hc2 : 0 < c^2 := sq_pos_of_pos c_pos
    nlinarith [mul_pos hc2 (show 0 < 1/2-exp (-20000 : ℝ) by linarith),
      mul_nonneg hc2.le (sub_nonneg.mpr h₁)]
  obtain ⟨m,hm⟩ := (held_scale_attainment x (by decide) (by norm_num : (0 : ℝ) < 1)).1
  have hm0 : m ≠ 0 := by
    rintro rfl
    exact hCentre.not_ge (hm (mem_univ 100))
  refine ⟨m, by intro h; exact hm0 (by linarith), hm, ?_⟩
  intro u hu
  change likelihood x u 1 ≤ likelihood x (-m) 1
  rw [hEven]
  exact hm hu

private noncomputable def normDensity (r s : ℝ) : ℝ :=
  if r = 0 then 1/(2*s) else s/r^2*(1-exp (-(r^2/(2*s^2))))

private theorem normDensity_off_lower {r s a : ℝ} (hr : r ≠ 0) (hs : 0 < s)
    (ha : 0 < a) (hE : a ≤ exp (r^2/(2*s^2))) :
    s/r^2*(1-1/a) ≤ normDensity r s := by
  simp only [normDensity, ite_eq_right hr, exp_neg]
  apply mul_le_mul_of_nonneg_left _ (by positivity)
  exact sub_le_sub_left (by simpa [one_div] using inv_anti₀ ha hE) _

private theorem normDensity_positive (r : ℝ) {s : ℝ} (hs : 0 < s) :
    0 < normDensity r s := by
  by_cases h : r = 0
  · simp only [normDensity, ite_eq_left h]; positivity
  · simp only [normDensity, ite_eq_right h]
    exact mul_pos (by positivity) (sub_pos.mpr (exp_lt_one_iff.mpr (neg_lt_zero.mpr (by positivity))))

private theorem exp_at_least_100000 {u : ℝ} (hu : 12 ≤ u) : 100000 ≤ exp u := by
  apply le_trans _ (exp_le_exp.mpr hu)
  exact le_trans (by norm_num [Finset.sum_range_succ, Nat.factorial_succ])
    (sum_le_exp_of_nonneg (by norm_num : 0 ≤ (12 : ℝ)) 20)

private theorem left_off_product :
    1/(2^10*10^18) < normDensity (1/625) (1/5)^10 *
      normDensity (-624/625) (1/5) * normDensity (6251/625) (1/5)^9 := by
  have h0 : (249995999/100000000 : ℝ) ≤ normDensity (1/625) (1/5) := by
    refine le_trans ?_ (normDensity_off_lower (by norm_num) (by norm_num) (a :=
      1+1/31250+(1/31250)^2/2) (by norm_num) ?_)
    · norm_num
    · have hh := sum_le_exp_of_nonneg (by norm_num : 0 ≤ (1/31250 : ℝ)) 3
      norm_num [Finset.sum_range_succ, Nat.factorial_succ] at hh ⊢
      exact hh
  have h1 : (20063953/100000000 : ℝ) ≤ normDensity (-624/625) (1/5) := by
    refine le_trans ?_ (normDensity_off_lower (by norm_num) (by norm_num)
      (a := 100000) (by norm_num) (exp_at_least_100000 (by norm_num)))
    norm_num
  have h2 : (199934015/100000000000 : ℝ) ≤ normDensity (6251/625) (1/5) := by
    refine le_trans ?_ (normDensity_off_lower (by norm_num) (by norm_num)
      (a := 100000) (by norm_num) (exp_at_least_100000 (by norm_num)))
    norm_num
  have h0p := normDensity_positive (1/625) (s := 1/5) (by norm_num)
  have h1p := normDensity_positive (-624/625) (s := 1/5) (by norm_num)
  apply lt_of_lt_of_le _ (mul_le_mul (mul_le_mul (pow_le_pow_left₀ (by norm_num) h0 10)
    h1 (by norm_num) (by positivity)) (pow_le_pow_left₀ (by norm_num) h2 9)
    (by positivity) (by positivity))
  norm_num

private theorem right_off_product :
    1/(2^10*10^18) < normDensity (-9/2) (7/2)^10 *
      normDensity (-11/2) (7/2) * normDensity (11/2) (7/2)^9 := by
  have h0 : (97/1000 : ℝ) ≤ normDensity (-9/2) (7/2) := by
    refine le_trans ?_ (normDensity_off_lower (by norm_num) (by norm_num) (a := (228/100 : ℝ))
      (by norm_num) ?_)
    · norm_num
    · apply le_trans _ (sum_le_exp_of_nonneg (by positivity : 0 ≤ ((-9/2 : ℝ)^2/(2*(7/2)^2))) 7)
      norm_num [Finset.sum_range_succ, Nat.factorial_succ]
  have h1 : (82/1000 : ℝ) ≤ normDensity (-11/2) (7/2) := by
    refine le_trans ?_ (normDensity_off_lower (by norm_num) (by norm_num) (a := (3435/1000 : ℝ))
      (by norm_num) ?_)
    · norm_num
    · apply le_trans _ (sum_le_exp_of_nonneg (by positivity : 0 ≤ ((-11/2 : ℝ)^2/(2*(7/2)^2))) 7)
      norm_num [Finset.sum_range_succ, Nat.factorial_succ]
  have h2 : (82/1000 : ℝ) ≤ normDensity (11/2) (7/2) := by
    have he : normDensity (11/2) (7/2) = normDensity (-11/2) (7/2) := by
      simp [normDensity]
      norm_num
    rw [he]
    exact h1
  have h0p := normDensity_positive (-9/2) (s := 7/2) (by norm_num)
  have h1p := normDensity_positive (-11/2) (s := 7/2) (by norm_num)
  apply lt_of_lt_of_le _ (mul_le_mul (mul_le_mul (pow_le_pow_left₀ (by norm_num) h0 10)
    h1 (by norm_num) (by positivity)) (pow_le_pow_left₀ (by norm_num) h2 9)
    (by positivity) (by positivity))
  norm_num


/-- Ten zero observations, one minus one, and nine tens. -/
def offPointSample : Fin 20 → ℝ :=
  Fin.append (fun _ : Fin 10 ↦ 0)
    (Fin.append (fun _ : Fin 1 ↦ -1) (fun _ : Fin 9 ↦ 10))

private noncomputable def slashPeakConstant : ℝ := 1/sqrt (2*π)

private theorem density_normalized (r s : ℝ) :
    density r s = slashPeakConstant*normDensity r s := by
  unfold density normDensity slashPeakConstant
  split_ifs <;> ring

private theorem grouped_offPoint_likelihood (m s : ℝ) :
    likelihood offPointSample m s = slashPeakConstant^20 *
      (normDensity (-m) s^10 * normDensity (-1-m) s * normDensity (10-m) s^9) := by
  unfold likelihood offPointSample
  rw [Fin.prod_univ_add (a := 10) (b := 10)]
  simp only [Fin.append_left,Fin.append_right,zero_sub,Fin.prod_const]
  rw [Fin.prod_univ_add (a := 1) (b := 9)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const,pow_one,density_normalized,mul_pow]
  ring

private theorem offPoint_collapse_eq :
    collapseValue offPointSample 0 = slashPeakConstant^20/(2^10*10^18) := by
  have hcount : SkewLSSlash.ties (fun i ↦ offPointSample i-0) = 10 := by
    classical
    unfold SkewLSSlash.ties
    rw [Finset.card_eq_sum_ones,Finset.sum_filter,Fin.sum_univ_add (a := 10) (b := 10)]
    simp only [offPointSample, Fin.append_left,Fin.append_right,sub_zero,ite_true,Fin.sum_const,
      nsmul_eq_mul,mul_one]
    rw [Fin.sum_univ_add (a := 1) (b := 9)]
    norm_num [Fin.append_left,Fin.append_right]
  classical
  rw [collapseValue,hcount,Finset.prod_filter,Fin.prod_univ_add (a := 10) (b := 10)]
  simp only [offPointSample, Fin.append_left,Fin.append_right,ne_eq,not_true_eq_false,ite_false,
    Fin.prod_const,one_pow,one_mul,sub_zero]
  rw [Fin.prod_univ_add (a := 1) (b := 9)]
  norm_num [Fin.append_left,Fin.append_right,slashPeakConstant]
  ring

private theorem normDensity_vertical_lt {s r : ℝ} (hs : 0 < s) (hr : r ≠ 0) :
    normDensity r s < s/r^2 := by
  rw [normDensity,ite_eq_right hr]
  exact mul_lt_of_lt_one_right (by positivity) (by linarith [exp_pos (-(r^2/(2*s^2)))])

private theorem offPoint_vertical_lt {s : ℝ} (hs : 0 < s) :
    likelihood offPointSample 0 s < collapseValue offPointSample 0 := by
  have hi : normDensity (-1) s < s/(-1)^2 := normDensity_vertical_lt hs (by norm_num)
  have hj : normDensity 10 s < s/10^2 := normDensity_vertical_lt hs (by norm_num)
  have he : normDensity 0 s = 1/(2*s) := by simp [normDensity]
  have hp : 0 < slashPeakConstant^20 := by unfold slashPeakConstant; positivity
  rw [grouped_offPoint_likelihood,offPoint_collapse_eq,neg_zero,sub_zero,sub_zero,he]
  rw [show slashPeakConstant^20/(2^10*10^18) = slashPeakConstant^20*(1/(2^10*10^18)) by
    rw [mul_one_div]]
  apply mul_lt_mul_of_pos_left _ hp
  calc
    _ < (1/(2*s))^10*(s/(-1)^2)*(s/10^2)^9 := by
      have h1 : 0 < normDensity 10 s^9 := pow_pos (normDensity_positive 10 hs) _
      have h2 : 0 < (1/(2*s))^10 := by positivity
      exact mul_lt_mul_of_lt_of_le_of_pos_of_nonneg (mul_lt_mul_of_pos_left hi h2)
        (pow_le_pow_left₀ (normDensity_positive 10 hs).le hj.le 9) h1 (by positivity)
    _ = _ := by field_simp

/-- Two exact positive-scale witnesses on opposite sides beat the
half-mass collapse height of the three-level slash sample. Every
finite scale at the collapse atom loses strictly to that height. -/
theorem exact_off_point_witnesses :
    collapseValue offPointSample 0 < likelihood offPointSample (-1/625) (1/5) ∧
    collapseValue offPointSample 0 < likelihood offPointSample (9/2) (7/2) ∧
    ∀ s > 0, likelihood offPointSample 0 s < collapseValue offPointSample 0 := by
  have hp : 0 < slashPeakConstant^20 := by unfold slashPeakConstant; positivity
  refine ⟨?_,?_,fun _ hs ↦ offPoint_vertical_lt hs⟩
  · rw [offPoint_collapse_eq,grouped_offPoint_likelihood,← mul_one_div]
    apply mul_lt_mul_of_pos_left _ hp
    convert left_off_product using 2 <;> norm_num
  · rw [offPoint_collapse_eq,grouped_offPoint_likelihood,← mul_one_div]
    apply mul_lt_mul_of_pos_left _ hp
    convert right_off_product using 2 <;> norm_num


private noncomputable def slashShape : ℝ → ℝ := dslope (fun t : ℝ ↦ 1-exp (-t)) 0

private theorem slashShape_zero : slashShape 0 = 1 := by
  have hD : HasDerivAt (fun t : ℝ ↦ 1-exp (-t)) 1 0 := by
    simpa using (((hasDerivAt_id (0:ℝ)).neg.exp).const_sub (1:ℝ))
  simp [slashShape, dslope_same, hD.deriv]

private theorem slashShape_off {t : ℝ} (ht : t ≠ 0) :
    slashShape t = (1-exp (-t))/t := by
  simp [slashShape, dslope_of_ne _ ht, slope_def_field]

private theorem slashShape_pos {t : ℝ} (ht : 0 ≤ t) : 0 < slashShape t := by
  rcases ht.eq_or_lt with h | h
  · rw [← h,slashShape_zero]; exact zero_lt_one
  · rw [slashShape_off h.ne']
    exact div_pos (sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos h))) h

private theorem slashShape_strictAnti : StrictAntiOn slashShape (Ici 0) := by
  apply strictAntiOn_of_deriv_neg (convex_Ici 0)
  · have hc (x : ℝ) : AnalyticAt ℝ slashShape x := by
      unfold slashShape
      exact (by fun_prop : AnalyticAt ℝ (fun t : ℝ ↦ 1-exp (-t)) x).dslope 0
    exact (continuous_iff_continuousAt.mpr fun x ↦ (hc x).continuousAt).continuousOn
  · intro t ht
    rw [interior_Ici] at ht
    have ht' : 0 < t := ht
    have hd : HasDerivAt (fun t : ℝ ↦ (1-exp (-t))/t)
        ((exp (-t)*t-(1-exp (-t)))/t^2) t := by
      convert ((((hasDerivAt_id t).neg.exp).const_sub (1:ℝ)).div
        (hasDerivAt_id t) ht'.ne') using 1 <;> simp <;> rfl
    have he : slashShape =ᶠ[𝓝 t] (fun t : ℝ ↦ (1-exp (-t))/t) := by
      filter_upwards [Ioi_mem_nhds ht'] with u hu
      exact slashShape_off (ne_of_gt hu)
    rw [(hd.congr_of_eventuallyEq he).deriv]
    apply div_neg_of_neg_of_pos _ (by positivity)
    have h := Real.add_one_lt_exp ht'.ne'
    have H : exp (-t)*(1+t) < 1 := by
      rw [exp_neg, inv_mul_lt_iff₀ (exp_pos _), mul_one]
      linarith
    linarith

private theorem density_shape (r : ℝ) {s : ℝ} (hs : 0 < s) :
    density r s = (1/sqrt (2*π))/(2*s) * slashShape (r^2/(2*s^2)) := by
  by_cases h : r = 0
  · simp [h, density, slashShape_zero]
  · rw [slashShape_off (by positivity)]
    simp only [density, ite_eq_right h]
    field_simp

private theorem log_density_analytic (m y : ℝ) {s : ℝ} (hs : 0 < s) :
    AnalyticAt ℝ (fun x ↦ log (density (x-m) s)) y := by
  simp_rw [density_shape _ hs]
  have h : AnalyticAt ℝ slashShape ((y-m)^2/(2*s^2)) := by
    unfold slashShape
    exact (by fun_prop : AnalyticAt ℝ (fun t : ℝ ↦ 1-exp (-t)) _).dslope 0
  have hArg : AnalyticAt ℝ (fun x : ℝ ↦ (x-m)^2/(2*s^2)) y := by fun_prop
  have hC : AnalyticAt ℝ (fun _ : ℝ ↦ (1/sqrt (2*π))/(2*s)) y := analyticAt_const
  exact (hC.mul (h.comp (f := fun x : ℝ ↦ (x-m)^2/(2*s^2)) hArg)).log (mul_pos (by positivity) (slashShape_pos (by positivity)))

/-- Spatial score used for analytic incidence on the free and held scale
slash charts. -/
noncomputable def spatialScore (m s y : ℝ) : ℝ :=
  deriv (fun x ↦ log (density (x-m) s)) y

/-- The slash score is real analytic through the location contact, and
equal scores on the whole observation line identify location and scale. -/
theorem slash_spatial_score_separation {m k s a : ℝ} (hs : 0 < s) (ha : 0 < a) :
    AnalyticOnNhd ℝ (spatialScore m s) univ ∧
    ((∀ y, spatialScore m s y = spatialScore k a y) → m = k ∧ s = a) := by
  refine ⟨fun y _ ↦ (log_density_analytic m y hs).deriv,?_⟩
  intro he
  let F := fun y : ℝ ↦ log (density (y-m) s)-log (density (y-k) a)
  have hd (y : ℝ) : HasDerivAt F 0 y := by
    have h := (log_density_analytic m y hs).differentiableAt.hasDerivAt.sub
      (log_density_analytic k y ha).differentiableAt.hasDerivAt
    change HasDerivAt (fun y : ℝ ↦ log (density (y-m) s)-log (density (y-k) a)) 0 y
    convert h using 1
    exact (sub_eq_zero.mpr (he y)).symm
  have hConst (u v : ℝ) : F u = F v := by
    have ord (x y : ℝ) (hxy : x < y) : F x = F y := by
      obtain ⟨z,_,hz⟩ := exists_hasDerivAt_eq_slope F (fun _ ↦ 0) hxy
        (fun t _ ↦ (hd t).continuousAt.continuousWithinAt) (fun t _ ↦ hd t)
      exact (sub_eq_zero.mp ((div_eq_zero_iff.mp hz.symm).resolve_right (sub_ne_zero.mpr hxy.ne'))).symm
    rcases lt_trichotomy u v with h | rfl | h
    · exact ord u v h
    · rfl
    · exact (ord v u h).symm
  have peak {r t : ℝ} (ht : 0 < t) (hr : r ≠ 0) : density r t < density 0 t := by
    rw [density_shape _ ht,density_shape _ ht]
    apply mul_lt_mul_of_pos_left _ (by positivity)
    have hh : 0 < r^2/(2*t^2) := by positivity
    simpa using slashShape_strictAnti (show (0:ℝ) ∈ Ici 0 from le_rfl)
      (show r^2/(2*t^2) ∈ Ici 0 from hh.le) hh
  have pos (r : ℝ) {t : ℝ} (ht : 0 < t) : 0 < density r t := by
    rw [density_shape _ ht]
    exact mul_pos (by positivity) (slashShape_pos (by positivity))
  have hmk : m = k := by
    by_contra hn
    have h₁ := log_lt_log (pos _ ha) (peak ha (sub_ne_zero.mpr hn))
    have h₂ := log_lt_log (pos _ hs) (peak hs (sub_ne_zero.mpr (Ne.symm hn)))
    have hh := hConst m k
    dsimp [F] at hh
    simp only [sub_self] at hh
    linarith
  subst k
  refine ⟨rfl,?_⟩
  have hh := hConst (m+1) m
  dsimp [F] at hh
  rw [add_sub_cancel_left, sub_self] at hh
  have hRatio : log (density 1 s/density 0 s) = log (density 1 a/density 0 a) := by
    rw [log_div (pos _ hs).ne' (pos _ hs).ne',log_div (pos _ ha).ne' (pos _ ha).ne']
    linarith
  have hShape : slashShape (1/(2*s^2)) = slashShape (1/(2*a^2)) := by
    simp only [density_shape _ hs,density_shape _ ha,zero_pow two_ne_zero,zero_div,slashShape_zero,mul_one,one_pow,
      mul_div_cancel_left₀ _ (by positivity : (1/sqrt (2*π))/(2*s) ≠ 0),
      mul_div_cancel_left₀ _ (by positivity : (1/sqrt (2*π))/(2*a) ≠ 0)] at hRatio
    exact (Real.log_injOn_pos (slashShape_pos (by positivity))
      (slashShape_pos (by positivity)) hRatio)
  have hWidths := slashShape_strictAnti.injOn (show 1/(2*s^2) ∈ Ici 0 from mem_Ici.mpr (by positivity))
    (show 1/(2*a^2) ∈ Ici 0 from mem_Ici.mpr (by positivity)) hShape
  field_simp at hWidths
  have hProduct : (s-a)*(s+a) = 0 := by nlinarith
  exact sub_eq_zero.mp ((mul_eq_zero.mp hProduct).resolve_right (by positivity))


/-- At any held positive scale the open two-observation stratum with
half-gap exceeding four scales has distinct reflected global winners. -/
theorem held_scale_open_reflected_maxima {s b h : ℝ} (hs : 0 < s) (hh : 4*s < h) :
    ∃ m : ℝ, m ≠ b ∧
      IsMaxOn (fun u ↦ likelihood ![b-h,b+h] u s) univ m ∧
      IsMaxOn (fun u ↦ likelihood ![b-h,b+h] u s) univ (2*b-m) := by
  let x : Fin 2 → ℝ := ![b-h,b+h]
  have hh0 : 0 < h := (by positivity : 0 < 4*s).trans hh
  have hScale : 0 < slashPeakConstant := by unfold slashPeakConstant; positivity
  have hEval (m : ℝ) :
      likelihood x m s = slashPeakConstant^2 *
        (normDensity (b-h-m) s * normDensity (b+h-m) s) := by
    simp [x,likelihood,Fin.prod_univ_two,density_normalized]
    ring
  have hD (r : ℝ) : normDensity (-r) s = normDensity r s := by simp [normDensity]
  have hEven (m : ℝ) : likelihood x (2*b-m) s = likelihood x m s := by
    rw [hEval,hEval,show b-h-(2*b-m)=-(b+h-m) by ring,show b+h-(2*b-m)=-(b-h-m) by ring,
      hD,hD,mul_comm (normDensity (b+h-m) s)]
  have hCenter : likelihood x b s < likelihood x (b+h) s := by
    rw [hEval,hEval,show b-h-b = -h by ring,show b+h-b = h by ring,
      show b-h-(b+h) = -(2*h) by ring,sub_self,hD,hD]
    apply mul_lt_mul_of_pos_left _ (pow_pos hScale _)
    have h1 := normDensity_vertical_lt hs hh0.ne'
    have hP := normDensity_positive h hs
    have h2 : 2 ≤ exp ((2*h)^2/(2*s^2)) := by
      have h3 : 1 ≤ (2*h)^2/(2*s^2) := by
        apply (le_div_iff₀ (by positivity)).2
        nlinarith [mul_pos (sub_pos.mpr hh) (show 0 < h+4*s by positivity)]
      linarith [add_one_le_exp ((2*h)^2/(2*s^2))]
    have hEnd := normDensity_off_lower (show 2*h ≠ 0 by positivity) hs (by norm_num : 0 < (2:ℝ)) h2
    have hσ : 0 < (1:ℝ)/(2*s) := by positivity
    have hb : (s/h^2)^2 < 1/(16*h^2) := by
      have hs2 : (4*s)^2 < h^2 := pow_lt_pow_left₀ hh (by positivity) (by decide : 2 ≠ 0)
      apply (lt_div_iff₀ (by positivity)).2
      have H : (s/h^2)^2*(16*h^2) = (4*s)^2/h^2 := by field_simp; ring
      rw [H]
      exact (div_lt_one (by positivity)).2 hs2
    have hEnd' : 1/(16*h^2) ≤ normDensity (2*h) s * normDensity 0 s := by
      rw [show normDensity 0 s = 1/(2*s) by simp [normDensity]]
      have hG := mul_le_mul_of_nonneg_right hEnd hσ.le
      have hEQ : (s/(2*h)^2*(1-1/2))*(1/(2*s)) = 1/(16*h^2) := by field_simp; ring
      simpa only [hEQ] using hG
    have hhPow : normDensity h s * normDensity h s < (s/h^2)^2 := by
      simpa [pow_two] using pow_lt_pow_left₀ h1 hP.le (by decide : 2 ≠ 0)
    exact hhPow.trans (hb.trans_le hEnd')
  obtain ⟨m,hm⟩ := (held_scale_attainment x (by decide) hs).1
  have hne : m ≠ b := by
    intro h
    subst m
    exact hCenter.not_ge (@hm (b+h) (mem_univ _))
  refine ⟨m,hne,hm,?_⟩
  intro u hu
  change likelihood x u s ≤ likelihood x (2*b-m) s
  rw [hEven]
  exact @hm u hu

/-- A slash collapse trace has a pole in its noncontact score, so it
cannot agree on any neighborhood with a finite analytic score. -/
theorem slash_collapse_score_separated {m s b z : ℝ} (hs : 0 < s) (hz : z ≠ b) :
    ¬ (∀ᶠ y in 𝓝 z, spatialScore m s y = -2/(y-b)) := by
  intro he
  have hA : AnalyticOnNhd ℝ (spatialScore m s) univ := fun y _ ↦ (log_density_analytic m y hs).deriv
  let G := fun y ↦ (y-b)*spatialScore m s y
  have hG : AnalyticOnNhd ℝ G univ := fun y hy ↦
    (by fun_prop : AnalyticAt ℝ (fun t : ℝ ↦ t-b) y).mul (hA y hy)
  have hc : AnalyticOnNhd ℝ (fun _ : ℝ ↦ (-2:ℝ)) univ := fun _ _ ↦ analyticAt_const
  have hh : ∀ᶠ y in 𝓝 z, G y = -2 := by
    filter_upwards [he,isOpen_compl_singleton.mem_nhds (show z ∈ ({b}:Set ℝ)ᶜ from hz)] with y h hy
    unfold G
    rw [h]
    have hy' : y-b ≠ 0 := sub_ne_zero.mpr hy
    field_simp
  have hZ : ∃ᶠ y in 𝓝[≠] z, G y = -2 := (hh.filter_mono nhdsWithin_le_nhds).frequently
  have hk := hG.eqOn_of_preconnected_of_frequently_eq hc isPreconnected_univ (mem_univ z) hZ (mem_univ b)
  norm_num [G] at hk



private theorem normDensity_peak (r : ℝ) {s : ℝ} (hs : 0 < s) :
    normDensity r s ≤ 1/(2*s) := by
  by_cases hr : r = 0
  · simp [normDensity,hr]
  unfold normDensity
  rw [ite_eq_right hr]
  have h : 1-exp (-(r^2/(2*s^2))) ≤ r^2/(2*s^2) := by
    linarith [add_one_le_exp (-(r^2/(2*s^2)))]
  refine (mul_le_mul_of_nonneg_left h (by positivity : 0 ≤ s/r^2)).trans ?_
  field_simp
  exact le_rfl

private theorem normDensity_remote {r : ℝ} {s : ℝ} (hs : 0 < s) (hr : r ≠ 0) :
    normDensity r s ≤ 1/|r| := by
  rcases le_total s |r| with h | h
  · refine (normDensity_vertical_lt hs hr).le.trans ?_
    apply (div_le_div_iff₀ (by positivity) (abs_pos.mpr hr)).mpr
    have H := mul_le_mul_of_nonneg_right h (abs_nonneg r)
    simpa [← pow_two, sq_abs] using H
  · refine (normDensity_peak r hs).trans ?_
    apply one_div_le_one_div_of_le (abs_pos.mpr hr)
    linarith

private theorem offPoint_joint_continuous :
    ContinuousOn (fun p : ℝ × ℝ ↦ likelihood offPointSample p.1 p.2) (univ ×ˢ Ioi 0) := by
  unfold likelihood
  apply continuousOn_finsetProd _ fun i _ ↦ ?_
  have h (p : ℝ × ℝ) (hp : p ∈ univ ×ˢ Ioi (0:ℝ)) :
      density (offPointSample i-p.1) p.2 = slashPeakConstant/(2*p.2) *
        slashShape ((offPointSample i-p.1)^2/(2*p.2^2)) := density_shape _ hp.2
  apply ContinuousOn.congr _ h
  have hA : Continuous slashShape := continuous_iff_continuousAt.mpr fun t ↦ by
    unfold slashShape
    exact ((by fun_prop : AnalyticAt ℝ (fun z : ℝ ↦ 1-exp (-z)) t).dslope 0).continuousAt
  apply ContinuousOn.mul
  · exact continuousOn_const.div (by fun_prop) (fun p hp ↦ by have : 0 < p.2 := hp.2; positivity)
  · apply hA.comp_continuousOn
    exact (by fun_prop : ContinuousOn (fun p : ℝ × ℝ ↦ (offPointSample i-p.1)^2) _).div
      (by fun_prop) (fun p hp ↦ by have : 0 < p.2 := hp.2; positivity)

private theorem offPoint_spatial_small {B : ℝ} (hB : collapseValue offPointSample 0 < B) :
    ∃ δ > 0, δ ≤ 1/2 ∧ ∀ m, |m| < δ → ∀ s > 0, likelihood offPointSample m s < B := by
  let U := fun m : ℝ ↦ slashPeakConstant^20/(2^10*(1+m)^2*(10-m)^18)
  have hU : ContinuousAt U 0 := by
    unfold U
    apply ContinuousAt.div (by fun_prop) (by fun_prop)
    norm_num
  have h0 : U 0 = collapseValue offPointSample 0 := by
    rw [offPoint_collapse_eq]
    norm_num [U]
  obtain ⟨δ,hδ,hball⟩ := Metric.mem_nhds_iff.mp (hU (Iio_mem_nhds (h0 ▸ hB)))
  refine ⟨min δ (1/2),lt_min hδ (by norm_num),min_le_right _ _,fun m hm s hs ↦ ?_⟩
  have hml : |m| < 1/2 := hm.trans_le (min_le_right _ _)
  have h₁ : -1-m ≠ 0 := by linarith [(abs_lt.mp hml).1]
  have h₂ : 10-m ≠ 0 := by linarith [(abs_lt.mp hml).2]
  have hC : 0 < slashPeakConstant^20 := by unfold slashPeakConstant; positivity
  have hUpper : likelihood offPointSample m s ≤ U m := by
    rw [grouped_offPoint_likelihood]
    have hprod : normDensity (-m) s^10*normDensity (-1-m) s*normDensity (10-m) s^9 ≤
        (1/(2*s))^10*(s/(-1-m)^2)*(s/(10-m)^2)^9 := by
      apply mul_le_mul
      · exact mul_le_mul (pow_le_pow_left₀ (normDensity_positive _ hs).le (normDensity_peak _ hs) 10)
          (normDensity_vertical_lt hs h₁).le (normDensity_positive _ hs).le (by positivity)
      · exact pow_le_pow_left₀ (normDensity_positive _ hs).le (normDensity_vertical_lt hs h₂).le 9
      · exact pow_nonneg (normDensity_positive _ hs).le _
      · positivity
    apply (mul_le_mul_of_nonneg_left hprod hC.le).trans (le_of_eq ?_)
    unfold U
    rw [show -1-m = -(1+m) by ring,neg_sq]
    field_simp
  apply hUpper.trans_lt
  apply hball
  rw [Metric.mem_ball,Real.dist_eq,sub_zero]
  exact hm.trans_le (min_le_left _ _)

private theorem offPoint_small_scale {B : ℝ} (hB : collapseValue offPointSample 0 < B) :
    ∃ ε > 0, ∀ m s : ℝ, 0 < s → s < ε → likelihood offPointSample m s < B := by
  obtain ⟨δ,hδ,hδ1,hNear⟩ := offPoint_spatial_small hB
  have hC0 : 0 < slashPeakConstant := by unfold slashPeakConstant; positivity
  have hB0 : 0 < B := by
    apply lt_trans _ hB
    rw [offPoint_collapse_eq]
    positivity
  let C := slashPeakConstant^20
  let K := max 4 (1/δ^2)
  have hK : 0 < K := (by norm_num : (0:ℝ)<4).trans_le (le_max_left _ _)
  have H {r s : ℝ} (hs : 0 < s) (hr : δ ≤ |r|) : normDensity r s ≤ K*s := by
    have hr0 : 0 < |r| := hδ.trans_le hr
    have h := normDensity_vertical_lt hs (abs_pos.mp hr0)
    have hsq : δ^2 ≤ r^2 := by simpa only [sq_abs] using pow_le_pow_left₀ hδ.le hr 2
    calc
      _ ≤ s/r^2 := h.le
      _ ≤ s/δ^2 := div_le_div_of_nonneg_left hs.le (by positivity) hsq
      _ = (1/δ^2)*s := by ring
      _ ≤ K*s := mul_le_mul_of_nonneg_right (le_max_right _ _) hs.le
  have exterior (m s : ℝ) (hs : 0 < s) (hm : δ ≤ |m|) :
      likelihood offPointSample m s ≤ C*(K^19*s^18+K^11*s^2+K^20*s^20) := by
    have h0 := H hs (r := -m) (by simpa only [abs_neg] using hm)
    rw [grouped_offPoint_likelihood]
    apply mul_le_mul_of_nonneg_left _ (by positivity : 0 ≤ C)
    have hP (r : ℝ) : 0 ≤ normDensity r s := (normDensity_positive _ hs).le
    have hScale : 0 ≤ 1/(2*s) := by positivity
    by_cases h₁ : δ ≤ |-1-m|
    · by_cases h₂ : δ ≤ |10-m|
      · have hbound := mul_le_mul
          (mul_le_mul (pow_le_pow_left₀ (hP (-m)) h0 10) (H hs h₁) (hP (-1-m)) (by positivity : 0 ≤ (K*s)^10))
          (pow_le_pow_left₀ (hP (10-m)) (H hs h₂) 9) (pow_nonneg (hP (10-m)) 9) (by positivity)
        exact hbound.trans (by
          have hA : 0 ≤ K^19*s^18 := by positivity
          have hB : 0 ≤ K^11*s^2 := by positivity
          calc
            _ = K^20*s^20 := by ring
            _ ≤ _ := by linarith)
      · have hbound := mul_le_mul
          (mul_le_mul (pow_le_pow_left₀ (hP (-m)) h0 10) (H hs h₁) (hP (-1-m)) (by positivity : 0 ≤ (K*s)^10))
          (pow_le_pow_left₀ (hP (10-m)) (normDensity_peak (10-m) hs) 9) (pow_nonneg (hP (10-m)) 9) (by positivity)
        have he : (K*s)^10*(K*s)*(1/(2*s))^9 ≤ K^11*s^2 := by
          have H : (K*s)^10*(K*s)*(1/(2*s))^9 = (K^11*s^2)/2^9 := by field_simp <;> ring
          rw [H]
          exact div_le_self (by positivity) (by norm_num)
        exact hbound.trans (he.trans (by
          have hA : 0 ≤ K^19*s^18 := by positivity
          have hB : 0 ≤ K^20*s^20 := by positivity
          linarith))
    · have h₂ : δ ≤ |10-m| := by
        have h := abs_lt.mp (lt_of_not_ge h₁)
        linarith [le_abs_self (10-m),h.1,h.2,hδ1]
      have hbound := mul_le_mul
        (mul_le_mul (pow_le_pow_left₀ (hP (-m)) h0 10) (normDensity_peak (-1-m) hs) (hP (-1-m)) (by positivity : 0 ≤ (K*s)^10))
        (pow_le_pow_left₀ (hP (10-m)) (H hs h₂) 9) (pow_nonneg (hP (10-m)) 9) (by positivity)
      have he : (K*s)^10*(1/(2*s))*(K*s)^9 ≤ K^19*s^18 := by
        have H : (K*s)^10*(1/(2*s))*(K*s)^9 = (K^19*s^18)/2 := by field_simp <;> ring
        rw [H]
        exact div_le_self (by positivity) (by norm_num)
      exact hbound.trans (he.trans (by
        have hA : 0 ≤ K^11*s^2 := by positivity
        have hB : 0 ≤ K^20*s^20 := by positivity
        linarith))
  have hcont : Continuous (fun s : ℝ ↦ C*(K^19*s^18+K^11*s^2+K^20*s^20)) := by fun_prop
  have ev : ∀ᶠ s in 𝓝 (0:ℝ), C*(K^19*s^18+K^11*s^2+K^20*s^20) < B :=
    hcont.continuousAt (Iio_mem_nhds (by simpa using hB0))
  obtain ⟨ε,hε,hball⟩ := Metric.mem_nhds_iff.mp ev
  refine ⟨ε,hε,fun m s hs hse ↦ ?_⟩
  rcases lt_or_ge |m| δ with h | h
  · exact hNear m h s hs
  · apply (exterior m s hs h).trans_lt
    exact hball (by rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]; exact hse)


private theorem offPoint_attains_on_closed_side {Z : Set ℝ} (hZ : IsClosed Z) {m s : ℝ}
    (hm : m ∈ Z) (hs : 0 < s) (hw : collapseValue offPointSample 0 < likelihood offPointSample m s) :
    ∃ p ∈ Z ×ˢ Ioi (0:ℝ), IsMaxOn (fun p : ℝ × ℝ ↦ likelihood offPointSample p.1 p.2)
      (Z ×ˢ Ioi 0) p := by
  let B := likelihood offPointSample m s
  let C := slashPeakConstant^20
  have hC : 0 < C := by unfold C slashPeakConstant; positivity
  have hB : 0 < B := by
    apply lt_trans _ hw
    rw [offPoint_collapse_eq]
    exact div_pos hC (by norm_num)
  let F := min (B/(C+1)) 1/2
  have hF : 0 < F := half_pos (lt_min (div_pos hB (by positivity)) zero_lt_one)
  have hF1 : F ≤ 1 := by unfold F; linarith [min_le_right (B/(C+1)) (1:ℝ)]
  have hFB : C*F^20 < B := by
    have hpow : F^20 ≤ F := by
      rw [show (20:ℕ) = 19+1 by decide,pow_succ]
      exact mul_le_of_le_one_left hF.le (pow_le_one₀ hF.le hF1)
    have hF' : 2*F ≤ B/(C+1) := by dsimp [F]; linarith [min_le_left (B/(C+1)) (1:ℝ)]
    have hcf := (le_div_iff₀ (show 0 < C+1 by positivity)).1 hF'
    have hh : C*F < B := by nlinarith [mul_pos hC hF]
    exact (mul_le_mul_of_nonneg_left hpow hC.le).trans_lt hh
  let H := 1/F
  let M := 10+H
  have hH : 0 < H := by unfold H; positivity
  have hHF : 1/H = F := by unfold H; field_simp
  have outside (u a : ℝ) (ha : 0 < a) (h : H ≤ a ∨ M ≤ |u|) :
      likelihood offPointSample u a < B := by
    have hp (y : ℝ) (hy : |y| ≤ 10) : normDensity (y-u) a ≤ F := by
      rcases h with hs | hu
      · refine (normDensity_peak _ ha).trans ?_
        rw [← hHF]
        exact one_div_le_one_div_of_le hH (by linarith)
      · have he : H ≤ |y-u| := by
          have htri := abs_add_le (y-u) (-y)
          rw [show (y-u)+(-y) = -u by ring,abs_neg,abs_neg] at htri
          unfold M at hu
          linarith
        have hh : y-u ≠ 0 := abs_pos.mp (hH.trans_le he)
        exact (normDensity_remote ha hh).trans (hHF ▸ one_div_le_one_div_of_le hH he)
    rw [grouped_offPoint_likelihood]
    have h0 : normDensity (-u) a ≤ F := by simpa using hp 0 (by norm_num)
    have h1 : normDensity (-1-u) a ≤ F := hp (-1) (by norm_num)
    have h2 : normDensity (10-u) a ≤ F := hp 10 (by norm_num)
    have hn (r : ℝ) : 0 ≤ normDensity r a := (normDensity_positive _ ha).le
    have hh := mul_le_mul (mul_le_mul (pow_le_pow_left₀ (hn _) h0 10) h1 (hn _)
      (pow_nonneg hF.le _)) (pow_le_pow_left₀ (hn _) h2 9) (pow_nonneg (hn _) _) (by positivity)
    have he : F^10*F*F^9 = F^20 := by ring
    exact (mul_le_mul_of_nonneg_left (he ▸ hh) hC.le).trans_lt hFB
  obtain ⟨ε,hε,hSmall⟩ := offPoint_small_scale (show collapseValue offPointSample 0 < B from hw)
  let K := (Icc (-M) M ∩ Z) ×ˢ Icc ε H
  have hK : IsCompact K := (isCompact_Icc.inter_right hZ).prod isCompact_Icc
  have sub : K ⊆ Z ×ˢ Ioi (0:ℝ) := fun p hp ↦ ⟨hp.1.2,hε.trans_le hp.2.1⟩
  have valid (p : ℝ × ℝ) (hp : p ∈ Z ×ˢ Ioi (0:ℝ))
      (h : B ≤ likelihood offPointSample p.1 p.2) : p ∈ K := by
    have hscale : p.2 ≤ H := le_of_not_gt fun hh ↦ (outside p.1 p.2 hp.2 (Or.inl hh.le)).not_ge h
    have hloc : |p.1| ≤ M := le_of_not_gt fun hh ↦ (outside p.1 p.2 hp.2 (Or.inr hh.le)).not_ge h
    exact ⟨⟨abs_le.mp hloc,hp.1⟩,⟨le_of_not_gt fun hh ↦ (hSmall p.1 p.2 hp.2 hh).not_ge h,hscale⟩⟩
  have hRef : (m,s) ∈ K := valid (m,s) ⟨hm,hs⟩ le_rfl
  have hCont := offPoint_joint_continuous.mono
    (fun _ h ↦ ⟨mem_univ _,(sub h).2⟩ : K ⊆ univ ×ˢ Ioi 0)
  obtain ⟨p,hp,hM⟩ := hK.exists_isMaxOn ⟨_,hRef⟩ hCont
  refine ⟨p,sub hp,?_⟩
  intro q hq
  by_cases h : B ≤ likelihood offPointSample q.1 q.2
  · exact @hM q (valid q hq h)
  · exact (le_of_not_ge h).trans (@hM (m,s) hRef)

/-- The exact three-level slash sample has distinct positive-scale
local maxima on either side of the critical half-mass atom. -/
theorem exact_off_point_local_maxima :
    ∃ p q : ℝ × ℝ, p.1 < 0 ∧ 0 < q.1 ∧ 0 < p.2 ∧ 0 < q.2 ∧
      IsLocalMax (fun z : ℝ × ℝ ↦ likelihood offPointSample z.1 z.2) p ∧
      IsLocalMax (fun z : ℝ × ℝ ↦ likelihood offPointSample z.1 z.2) q := by
  obtain ⟨p,hp,hm⟩ := offPoint_attains_on_closed_side isClosed_Iic (by norm_num : (-1/625:ℝ) ∈ Iic 0)
    (by norm_num : (0:ℝ) < 1/5) exact_off_point_witnesses.1
  obtain ⟨q,hq,hn⟩ := offPoint_attains_on_closed_side isClosed_Ici (by norm_num : (9/2:ℝ) ∈ Ici 0)
    (by norm_num : (0:ℝ) < 7/2) exact_off_point_witnesses.2.1
  have hl : p.1 < 0 := by
    refine lt_of_le_of_ne hp.1 ?_
    intro hz
    have hh : likelihood offPointSample (-1/625) (1/5) ≤ likelihood offPointSample p.1 p.2 :=
      @hm (-1/625,1/5) (by constructor <;> norm_num)
    rw [hz] at hh
    exact exact_off_point_witnesses.1.not_ge (hh.trans (offPoint_vertical_lt hp.2).le)
  have hr : 0 < q.1 := by
    refine lt_of_le_of_ne hq.1 ?_
    intro hz
    have hh : likelihood offPointSample (9/2) (7/2) ≤ likelihood offPointSample q.1 q.2 :=
      @hn (9/2,7/2) (by constructor <;> norm_num)
    rw [← hz] at hh
    exact exact_off_point_witnesses.2.1.not_ge (hh.trans (offPoint_vertical_lt hq.2).le)
  refine ⟨p,q,hl,hr,hp.2,hq.2,?_ ,?_⟩
  · exact hm.isLocalMax (prod_mem_nhds
      (mem_of_superset (Iio_mem_nhds hl) Iio_subset_Iic_self) (Ioi_mem_nhds hp.2))
  · exact hn.isLocalMax (prod_mem_nhds
      (mem_of_superset (Ioi_mem_nhds hr) Ioi_subset_Ici_self) (Ioi_mem_nhds hq.2))



end UnivariateML.Student2Slash
end
