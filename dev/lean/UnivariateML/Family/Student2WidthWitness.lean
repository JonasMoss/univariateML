module

public import UnivariateML.Family.Student2Half
public import UnivariateML.Family.Student2Normal
public import UnivariateML.Family.Student2Free
public import UnivariateML.Family.StudentCauchy
public import UnivariateML.Family.StudentGeodesic
import Mathlib.Analysis.Real.Pi.Bounds
import Mathlib.Analysis.SpecialFunctions.Exp

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Held-width Student witnesses

The two-point product separates the location and shape comparisons.
-/

@[expose] public section

open Set Real Filter Topology

namespace UnivariateML.Student2WidthWitness

private theorem normalizer_differentiable {q : ℝ} (hq : 0 < q) :
    DifferentiableAt ℝ TDist.logNormalizer q := by
  have h := (Student2Half.log_gamma_half_step_derivative hq).differentiableAt
  have hL : DifferentiableAt ℝ (fun v : ℝ ↦ log (v*π)/2) q :=
    ((by fun_prop : DifferentiableAt ℝ (fun v : ℝ ↦ v*π) q).log (by positivity)).div_const 2
  exact h.sub hL

private theorem shape_one_normalizer_eq : TDist.logNormalizer 1 = -log π := by
  unfold TDist.logNormalizer
  norm_num [Gamma_one,Gamma_one_half_eq,log_sqrt pi_pos.le]
  ring

private noncomputable def lowShapeProfile (q : ℝ) : ℝ :=
  TDist.logNormalizer q-(q+1)/4*log (40000/q)

private theorem lowShapeProfile_derivative {q : ℝ} (hq : 0 < q) :
    HasDerivAt lowShapeProfile
      (deriv TDist.logNormalizer q-log (40000/q)/4+(q+1)/(4*q)) q := by
  have hN := (normalizer_differentiable hq).hasDerivAt
  have hL : HasDerivAt (fun v : ℝ ↦ log (40000/v)) (-1/q) q := by
    have he : (fun v : ℝ ↦ log ((40000:ℝ)/v)) =ᶠ[nhds q] (fun v ↦ log 40000-log v) := by
      filter_upwards [Ioi_mem_nhds hq] with v hv
      rw [log_div (by norm_num) (ne_of_gt hv)]
    have h := (Real.hasDerivAt_log hq.ne').const_sub (log 40000)
    exact (h.congr_of_eventuallyEq he).congr_deriv (by ring)
  have h := hN.sub ((((hasDerivAt_id q).add_const 1).div_const 4).mul hL)
  unfold lowShapeProfile
  convert h using 1
  · rfl
  · simp only [id_eq]
    ring

private theorem low_shape_profile_strictAnti :
    StrictAntiOn lowShapeProfile (Icc (1 : ℝ) 10000) := by
  apply strictAntiOn_of_deriv_neg (convex_Icc ..)
  · exact fun q hq ↦ (lowShapeProfile_derivative (zero_lt_one.trans_le hq.1)).continuousAt.continuousWithinAt
  intro q hq
  rw [interior_Icc] at hq
  have h1 : 1 < q := hq.1
  have hQ : q < 10000 := hq.2
  have h0 : 0 < q := zero_lt_one.trans h1
  have hN := (Student2Half.student_log_normalizer_score_bounds h0).2
  rw [(lowShapeProfile_derivative h0).deriv]
  have he : (q+1)/(4*q) = (1+1/q)/4 := by field_simp
  rw [he]
  have hi : 1/q ≤ 1 := (div_le_one h0).2 h1.le
  have hnsq : 1/q^2 ≤ 1 := (div_le_one (by positivity)).2 (by nlinarith)
  by_cases h : q ≤ 4
  · have hlog : 3 < log (40000/q) := by
      have h32 : (32:ℝ) ≤ 40000/q := (le_div_iff₀ h0).2 (by linarith)
      have hlog32 : (3:ℝ) < log 32 := by
        have h' := le_log_one_add_of_nonneg (by norm_num : 0 ≤ (1:ℝ))
        rw [show (32:ℝ) = 2^5 by norm_num,log_pow]
        norm_num at h' ⊢
        linarith
      exact hlog32.trans_le (log_le_log (by norm_num) h32)
    have hN' : deriv TDist.logNormalizer q ≤ (1/q^2)/4 := hN.trans (le_of_eq (by ring))
    linarith
  · have hi4 : 1/q ≤ 1/4 := one_div_le_one_div_of_le (by norm_num) (le_of_not_ge h)
    have hs16 : 1/q^2 ≤ 1/16 := by
      have hh := pow_le_pow_left₀ (by positivity : 0 ≤ (1:ℝ)/q) hi4 2
      norm_num [one_div_pow] at hh ⊢
      exact hh
    have hlog : (21/16:ℝ) < log (40000/q) := by
      have h4 : (4:ℝ) ≤ 40000/q := (le_div_iff₀ h0).2 (by linarith)
      have h' := le_log_one_add_of_nonneg (by norm_num : 0 ≤ (1:ℝ))
      have h4log : (21/16:ℝ) < log 4 := by
        rw [show (4:ℝ) = 2^2 by norm_num,log_pow]
        norm_num at h' ⊢
        linarith
      exact h4log.trans_le (log_le_log (by norm_num) h4)
    have hN' : deriv TDist.logNormalizer q ≤ (1/q^2)/4 := hN.trans (le_of_eq (by ring))
    linarith

private theorem two_hundred_product (m q : ℝ) :
    (q+(-100-m)^2)*(q+(100-m)^2) = 40000*q+(m^2+q-10000)^2 := by ring

private theorem two_hundred_log_likelihood (m : ℝ) {q : ℝ} (hq : 0 < q) :
    TDist.logLik ![-100,100] m q 1 / 2 =
      TDist.logNormalizer q-(q+1)/4 *
        log (((q+(-100-m)^2)*(q+(100-m)^2))/q^2) := by
  unfold TDist.logLik
  simp only [Fin.sum_univ_two,Matrix.cons_val_zero,Matrix.cons_val_one,log_one,sub_zero,one_pow,mul_one]
  have he (u : ℝ) : 1+(u-m)^2/q = (q+(u-m)^2)/q := by field_simp
  rw [he,he,← show ((q+(-100-m)^2)/q)*((q+(100-m)^2)/q) =
      ((q+(-100-m)^2)*(q+(100-m)^2))/q^2 by ring,log_mul (by positivity) (by positivity)]
  ring

private theorem two_hundred_le_low_profile (m : ℝ) {q : ℝ} (hq : 0 < q) :
    TDist.logLik ![-100,100] m q 1 / 2 ≤ lowShapeProfile q := by
  rw [two_hundred_log_likelihood m hq,two_hundred_product]
  have h : (40000:ℝ)/q ≤ (40000*q+(m^2+q-10000)^2)/q^2 := by
    apply (le_div_iff₀ (by positivity)).2
    have he : (40000:ℝ)/q*q^2 = 40000*q := by field_simp
    rw [he]
    linarith [sq_nonneg (m^2+q-10000)]
  have hL := log_le_log (show 0 < (40000:ℝ)/q by positivity) h
  dsimp [lowShapeProfile]
  linarith [mul_le_mul_of_nonneg_left hL (by positivity : 0 ≤ (q+1)/4)]

private theorem two_hundred_high_shape_loses (m : ℝ) {q : ℝ} (hq : 10000 ≤ q) :
    TDist.logLik ![-100,100] m q 1 / 2 < lowShapeProfile 1 := by
  have h0 : 0 < q := (by norm_num : (0:ℝ)<10000).trans_le hq
  have hw : 0 ≤ q-10000 := sub_nonneg.mpr hq
  have hSq : (q-10000)^2 ≤ (m^2+q-10000)^2 :=
    pow_le_pow_left₀ hw (by linarith [sq_nonneg m]) 2
  have hR : (1+10000/q)^2 ≤ (40000*q+(m^2+q-10000)^2)/q^2 := by
    apply (le_div_iff₀ (by positivity)).2
    have h : (1+10000/q)^2*q^2 = (q+10000)^2 := by field_simp
    rw [h]
    linarith
  have hL := log_le_log (show 0 < (1+10000/q)^2 by positivity) hR
  rw [log_pow] at hL
  have t0 : 0 ≤ (10000:ℝ)/q := by positivity
  have t1 : (10000:ℝ)/q ≤ 1 := (div_le_one h0).2 hq
  have hlog : (10000:ℝ)/q/2 ≤ log (1+10000/q) := by
    refine le_trans ((le_div_iff₀ (by positivity)).2 ?_) (le_log_one_add_of_nonneg t0)
    nlinarith [mul_nonneg t0 (sub_nonneg.mpr t1)]
  have hc := mul_le_mul_of_nonneg_left hlog (show 0 ≤ q/2 by positivity)
  have hc' : (2500:ℝ) ≤ (q+1)/2*log (1+10000/q) := by
    have H : q/2*((10000:ℝ)/q/2) = 2500 := by field_simp; norm_num
    rw [H] at hc
    have hn := log_nonneg (le_add_of_nonneg_right t0)
    linarith [mul_nonneg (show (0:ℝ) ≤ 1/2 by norm_num) hn]
  have hN : TDist.logNormalizer q ≤ 0 :=
    (Student2Normal.normalizer_le_gaussian h0).trans (neg_nonpos.mpr
      (div_nonneg (log_nonneg (by linarith [pi_gt_three])) (by norm_num)))
  rw [two_hundred_log_likelihood m h0,two_hundred_product]
  have hB : (-2500:ℝ) < lowShapeProfile 1 := by
    unfold lowShapeProfile
    rw [shape_one_normalizer_eq]
    norm_num
    have hπ := log_le_sub_one_of_pos pi_pos
    have h200 := log_le_sub_one_of_pos (by norm_num : (0:ℝ)<200)
    rw [show (40000:ℝ)=(200:ℝ)^2 by norm_num,log_pow]
    linarith [pi_lt_four]
  have HH := mul_le_mul_of_nonneg_left hL (show 0 ≤ (q+1)/4 by positivity)
  norm_num at HH
  linarith

private theorem shape_one_max_at {m : ℝ} :
    TDist.logLik ![-100,100] m 1 1 / 2 = lowShapeProfile 1 ↔
      m = sqrt 9999 ∨ m = -sqrt 9999 := by
  rw [two_hundred_log_likelihood m zero_lt_one,two_hundred_product]
  unfold lowShapeProfile
  norm_num
  have H : 0 < (40000:ℝ)+(m^2+1-10000)^2 := by positivity
  constructor
  · intro he
    have hL : log ((40000:ℝ)+(m^2+1-10000)^2) = log 40000 := by linarith
    have hs := Real.log_injOn_pos H (by norm_num) hL
    have hsq : m^2 = 9999 := by nlinarith [sq_nonneg (m^2+1-10000)]
    have h := sq_sqrt (by norm_num : (0:ℝ)≤9999)
    exact sq_eq_sq_iff_eq_or_eq_neg.mp (hsq.trans h.symm)
  · have hs : (sqrt (9999:ℝ))^2 = 9999 := sq_sqrt (by norm_num)
    rintro (rfl|rfl) <;> norm_num [neg_sq,hs]

/-- At held ordinary scale one on the atlas sample, the maximizing
shape is one and the entire maximizing location set is the reflected pair. -/
theorem lst_fixed_scale_reflected_pair (p : ℝ × ℝ) :
    let D := (univ : Set ℝ) ×ˢ Ici (1 : ℝ)
    let L := fun z : ℝ × ℝ ↦ TDist.logLik ![-100,100] z.1 z.2 1
    (p ∈ D ∧ IsMaxOn L D p) ↔
      p.2 = 1 ∧ (p.1 = sqrt 9999 ∨ p.1 = -sqrt 9999) := by
  dsimp only
  let p₀ : ℝ × ℝ := (sqrt 9999,1)
  have h0 : TDist.logLik ![-100,100] p₀.1 p₀.2 1/2 = lowShapeProfile 1 :=
    shape_one_max_at.mpr (Or.inl rfl)
  have hShape (p : ℝ × ℝ) (hp : 1 < p.2) :
      TDist.logLik ![-100,100] p.1 p.2 1/2 < lowShapeProfile 1 := by
    by_cases hQ : p.2 ≤ 10000
    · exact (two_hundred_le_low_profile p.1 (zero_lt_one.trans hp)).trans_lt
        (low_shape_profile_strictAnti (by norm_num) ⟨hp.le,hQ⟩ hp)
    · exact two_hundred_high_shape_loses p.1 (le_of_not_ge hQ)
  constructor
  · rintro ⟨hp,hm⟩
    rcases hp.2.eq_or_lt with h | h
    · have hm₀ : TDist.logLik ![-100,100] p₀.1 p₀.2 1 ≤
          TDist.logLik ![-100,100] p.1 p.2 1 := @hm p₀ ⟨mem_univ _,le_rfl⟩
      have hB := two_hundred_le_low_profile p.1 zero_lt_one
      rw [← h] at hm₀
      have he : TDist.logLik ![-100,100] p.1 1 1/2 = lowShapeProfile 1 := by linarith
      exact ⟨h.symm,shape_one_max_at.mp he⟩
    · have hm₀ : TDist.logLik ![-100,100] p₀.1 p₀.2 1 ≤
          TDist.logLik ![-100,100] p.1 p.2 1 := @hm p₀ ⟨mem_univ _,le_rfl⟩
      exact False.elim (by linarith [hShape p h])
  · rintro ⟨hp,hm⟩
    refine ⟨⟨mem_univ _,hp.ge⟩,?_⟩
    intro z hz
    have he := shape_one_max_at.mpr hm
    change TDist.logLik ![-100,100] z.1 z.2 1 ≤
      TDist.logLik ![-100,100] p.1 p.2 1
    rw [hp]
    rcases hz.2.eq_or_lt with h | h
    · rw [← h]
      linarith [two_hundred_le_low_profile z.1 zero_lt_one]
    · linarith [hShape z h]


private def stdWitnessSample : Fin 2 → ℝ := ![-100,100]

private theorem std_witness_exponent_positive (m : ℝ) :
    0 < TDist.exponent stdWitnessSample m 2 := by
  have h : TDist.multiplicity stdWitnessSample m ≤ 1 := by
    classical
    unfold TDist.multiplicity
    rw [Finset.card_le_one_iff]
    intro i j hi hj
    have p : stdWitnessSample i = m := (Finset.mem_filter.mp hi).2
    have q : stdWitnessSample j = m := (Finset.mem_filter.mp hj).2
    have equal := p.trans q.symm
    fin_cases i <;> fin_cases j <;> norm_num [stdWitnessSample] at equal <;> rfl
  have hh : (TDist.multiplicity stdWitnessSample m : ℝ) ≤ 1 := by exact_mod_cast h
  unfold TDist.exponent
  norm_num
  linarith

private theorem std_witness_exact :
    exp (TDist.stdLogLik stdWitnessSample 100 3 1) = (2/π)^2/(40001:ℝ)^2 := by
  have hG : Gamma (3/2) = sqrt π/2 := by
    rw [show (3/2:ℝ)=1/2+1 by norm_num,Gamma_add_one (by norm_num),Gamma_one_half_eq]
    ring
  have h2 : Gamma 2 = 1 := Gamma_two
  have hlogG : log (Gamma (3/2)) = log π/2-log 2 := by
    rw [hG,log_div (by positivity) (by norm_num),log_sqrt pi_pos.le]
  have h : TDist.stdLogLik stdWitnessSample 100 3 1 =
      (2:ℕ)*(log 2-log π)-(2:ℕ)*log 40001 := by
    unfold TDist.stdLogLik stdWitnessSample
    norm_num [Fin.sum_univ_two,h2,hlogG]
    ring
  rw [h,exp_sub,exp_nat_mul,exp_nat_mul,exp_sub,exp_log (by norm_num),exp_log pi_pos,
    exp_log (by norm_num)]

private theorem std_witness_beats_bound :
    (81:ℝ)/100^6 < exp (TDist.stdLogLik stdWitnessSample 100 3 1) := by
  rw [std_witness_exact]
  have h : (1:ℝ)/2 < 2/π := (lt_div_iff₀ pi_pos).2 (by linarith [pi_lt_four])
  have hp := pow_lt_pow_left₀ h (by norm_num) (by decide : 2 ≠ 0)
  exact (by norm_num : (81:ℝ)/100^6 < (1/2)^2/40001^2).trans
    (div_lt_div_of_pos_right hp (by norm_num))

private theorem std_centered_bound {q : ℝ} (hq : 2 < q) :
    exp (TDist.stdLogLik stdWitnessSample 0 q 1) ≤ (81:ℝ)/100^6 := by
  rw [TDist.stdLogLik_eq stdWitnessSample 0 hq zero_lt_one,TDist.logLik,exp_sum]
  change (∏ i, TDist.density 0 q (TDist.ordinaryScale q 1) (stdWitnessSample i)) ≤ _
  simp only [Fin.prod_univ_two,stdWitnessSample,Matrix.cons_val_zero,Matrix.cons_val_one]
  have h : TDist.density 0 q (TDist.ordinaryScale q 1) (-100) =
      TDist.density 0 q (TDist.ordinaryScale q 1) 100 := by
    simp only [TDist.density,sub_zero,neg_sq]
  rw [h, ← pow_two]
  have H := Student2Normal.std_unit_density_remote_upper hq (by norm_num : (0:ℝ)<100)
  have h : 0 ≤ TDist.density 0 q (TDist.ordinaryScale q 1) 100 := (exp_pos _).le
  exact (pow_le_pow_left₀ h H 2).trans (by norm_num)

private theorem std_gaussian_bound (m : ℝ) :
    (∏ i, TDist.normalDensity m 1 (stdWitnessSample i)) ≤ (81:ℝ)/100^6 := by
  have h : (∏ i, TDist.normalDensity m 1 (stdWitnessSample i)) ≤ exp (-10000 : ℝ) := by
    simp only [Fin.prod_univ_two,stdWitnessSample,Matrix.cons_val_zero,Matrix.cons_val_one,
      TDist.normalDensity,log_one,sub_zero,one_pow,div_one,← exp_add,exp_le_exp]
    have H : 0 ≤ log (2*π) := log_nonneg (by linarith [Real.pi_gt_three])
    nlinarith [sq_nonneg m]
  have hh : exp (-10000 : ℝ) ≤ (81:ℝ)/100^6 := by
    rw [exp_neg,inv_le_iff_one_le_mul₀ (exp_pos _)]
    have H := sum_le_exp_of_nonneg (by norm_num : (0:ℝ)≤10000) 4
    norm_num [Finset.sum_range_succ,Nat.factorial_succ] at H ⊢
    linarith
  exact h.trans hh

private theorem std_witness_reflect (m q : ℝ) :
    TDist.stdLogLik stdWitnessSample (-m) q 1 =
      TDist.stdLogLik stdWitnessSample m q 1 := by
  unfold TDist.stdLogLik
  simp only [Fin.sum_univ_two,stdWitnessSample,Matrix.cons_val_zero,Matrix.cons_val_one]
  rw [show (-100:ℝ)- -m=-(100-m) by ring,show (100:ℝ)- -m=-(-100-m) by ring,neg_sq,neg_sq]
  exact add_comm _ _

/-- At held standard deviation one, the prescribed symmetric sample
has distinct reflected finite winners. The shape-three witness beats
the entire centered family and all excluded heights. -/
theorem std_fixed_sd_reflected_maxima :
    let L := fun p : ℝ × ℝ ↦ exp (TDist.stdLogLik ![-100,100] p.1 p.2 1)
    let D := (univ : Set ℝ) ×ˢ Ioi (2 : ℝ)
    ∃ m q : ℝ, m ≠ 0 ∧ 2 < q ∧ IsMaxOn L D (m,q) ∧ IsMaxOn L D (-m,q) := by
  dsimp only
  let L := fun p : ℝ × ℝ ↦ exp (TDist.stdLogLik stdWitnessSample p.1 p.2 1)
  let D := (univ : Set ℝ) ×ˢ Ioi (2 : ℝ)
  let F := {y : ℝ | (∃ b, y = ∏ i, TDist.normalDensity b 1 (stdWitnessSample i)) ∨
    (∃ b, TDist.exponent stdWitnessSample b 2 = 0 ∧
      y = exp (StudentFaces.fixedSdCollapseLog stdWitnessSample b))}
  obtain ⟨⟨m,q⟩,hp,hm⟩ : ∃ p ∈ D, IsMaxOn L D p := by
    by_contra hn
    have H := Student2Free.std_held_sd_decision stdWitnessSample (by decide)
      (σ := 1) zero_lt_one
    have HB (v : ℝ) (hv : v ∈ F) : v ≤ (81:ℝ)/100^6 := by
      rcases hv with ⟨b,rfl⟩ | ⟨b,hb,_⟩
      · exact std_gaussian_bound b
      · exact ((std_witness_exponent_positive b).ne' hb).elim
    have hSup : sSup F ≤ (81:ℝ)/100^6 := csSup_le ⟨_,Or.inl ⟨0,rfl⟩⟩ HB
    have hc := (H.2.2 (fun b ↦ (std_witness_exponent_positive b).le)).2.mp hn
    have hw : (100,3) ∈ D := ⟨mem_univ _,by norm_num⟩
    exact std_witness_beats_bound.not_ge ((hc.2 _ hw).le.trans hSup)
  have hn : m ≠ 0 := by
    intro h
    subst m
    have hh : L (100,3) ≤ L (0,q) := @hm (100,3) ⟨mem_univ _,by norm_num⟩
    exact std_witness_beats_bound.not_ge (hh.trans (std_centered_bound hp.2))
  have hq : 2 < q := hp.2
  refine ⟨m,q,hn,hq,hm,?_⟩
  intro z hz
  change L z ≤ exp (TDist.stdLogLik stdWitnessSample (-m) q 1)
  rw [std_witness_reflect]
  exact @hm z hz


/-- At a held location, a single nonzero absolute residual magnitude
makes every candidate strictly improvable at a larger shape. -/
theorem lst_held_location_one_magnitude_escape {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {m r q s : ℝ} (hr : 0 < r) (hq : 0 < q) (hs : 0 < s)
    (hx : ∀ i, (x i-m)^2 = r^2) :
    ∃ v > q, TDist.likelihood x m q s < TDist.likelihood x m v r := by
  let y : Fin n → ℝ := fun _ ↦ m+r
  have hEq (v a : ℝ) : TDist.likelihood y m v a = TDist.likelihood x m v a := by
    unfold TDist.likelihood TDist.logLik
    simp only [y,add_sub_cancel_left,hx]
  obtain ⟨v,hv,_,hh⟩ := Student2Half.held_endpoint_one_magnitude_escape y hn hr (fun _ ↦ rfl)
    hq hs (show (0:ℝ) ≤ q from hq.le)
  unfold SkewLSHalfT.likelihood at hh
  rw [hEq q s,hEq v r] at hh
  exact ⟨v,hv,(mul_lt_mul_iff_right₀ (by positivity : 0 < (2:ℝ)^n)).mp hh⟩

/-- On samples with at most two observation levels, the fitted ordinary
Student chart has no finite maximizer. Unbalanced masses diverge and
the nonconstant balanced case strictly improves with shape. -/
theorem lst_at_most_two_levels_no_fit {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {a b : ℝ} (hx : ∀ i, x i = a ∨ x i = b) :
    let D := {p : ℝ × ℝ × ℝ | 0 < p.2.1 ∧ 1 ≤ p.2.2}
    let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
    ¬ ∃ p ∈ D, IsMaxOn L D p := by
  dsimp only
  let D := {p : ℝ × ℝ × ℝ | 0 < p.2.1 ∧ 1 ≤ p.2.2}
  let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
  intro h
  have hmass := ((Student2Free.lst_all_free_decision x hn).2.1.mp h).1
  have hab : a ≠ b := by
    intro he
    have hh : ∀ i, x i = a := fun i ↦ (hx i).elim id (fun k ↦ k.trans he.symm)
    have hN : TDist.multiplicity x a = n := by simp [TDist.multiplicity,hh]
    have H := hmass a
    omega
  have hCounts : TDist.multiplicity x a + TDist.multiplicity x b = n := by
    classical
    have hset : (Finset.univ.filter fun i ↦ x i ≠ a) =
        (Finset.univ.filter fun i ↦ x i = b) := by
      ext i
      simp only [Finset.mem_filter,Finset.mem_univ,true_and]
      exact ⟨fun h ↦ (hx i).resolve_left h,fun h ↦ by rw [h]; exact hab.symm⟩
    unfold TDist.multiplicity
    rw [← hset,Finset.card_filter_add_card_filter_not]
    simp
  have hA' := hmass a
  have hB' := hmass b
  have hA : 2*TDist.multiplicity x a = n := by omega
  have hB : 2*TDist.multiplicity x b = n := by omega
  have hE : TDist.multiplicity x a = TDist.multiplicity x b := by omega
  have hSum (g : ℝ → ℝ) :
      ∑ i, g (x i) = (TDist.multiplicity x a : ℝ)*g a+(TDist.multiplicity x b : ℝ)*g b := by
    classical
    have ht (i : Fin n) : g (x i) = if x i = a then g a else g b := by
      by_cases hk : x i = a
      · simp [hk]
      · simp [(hx i).resolve_left hk,hab.symm]
    simp_rw [ht]
    rw [Finset.sum_ite,Finset.sum_const,Finset.sum_const,nsmul_eq_mul,nsmul_eq_mul]
    have hset : (Finset.univ.filter fun i ↦ x i ≠ a) =
        (Finset.univ.filter fun i ↦ x i = b) := by
      ext i
      simp only [Finset.mem_filter,Finset.mem_univ,true_and]
      exact ⟨fun h ↦ (hx i).resolve_left h,fun h ↦ by rw [h]; exact hab.symm⟩
    rw [hset]
    rfl
  have reflect (m v s : ℝ) :
      TDist.logLik x (a+b-m) v s = TDist.logLik x m v s := by
    unfold TDist.logLik
    rw [hSum (fun z ↦ TDist.logNormalizer v-log s-(v+1)/2*log (1+(z-(a+b-m))^2/(v*s^2))),
      hSum (fun z ↦ TDist.logNormalizer v-log s-(v+1)/2*log (1+(z-m)^2/(v*s^2))),hE]
    have ha : a-(a+b-m) = -(b-m) := by ring
    have hb : b-(a+b-m) = -(a-m) := by ring
    rw [ha,hb,neg_sq,neg_sq]
    exact add_comm _ _
  obtain ⟨⟨m,s,v⟩,hp,hm⟩ := h
  have hs : 0 < s := hp.1
  have hv : 1 ≤ v := hp.2
  have hv0 := zero_lt_one.trans_le hv
  have pat (b' : ℝ) {s' v' : ℝ} (hs' : 0 < s') (hv' : 1 ≤ v') :
      TDist.logLik x b' v' s' ≤ TDist.logLik x m v s :=
    exp_le_exp.mp (@hm (b',s',v') (show (b',s',v') ∈ D from ⟨hs',hv'⟩))
  have hcond : IsMaxOn (fun p : ℝ × ℝ ↦ TDist.logLik x p.1 v p.2) (univ ×ˢ Ioi 0) (m,s) :=
    fun p hp ↦ pat p.1 hp.2 hv
  let c := (a+b)/2
  let r := |b-a|/2
  have hr : 0 < r := half_pos (abs_pos.mpr (sub_ne_zero.mpr hab.symm))
  have hmag (i : Fin n) : (x i-c)^2 = r^2 := by
    have ha : a-c = -(b-a)/2 := by unfold c; ring
    have hb : b-c = (b-a)/2 := by unfold c; ring
    unfold r
    rw [div_pow,sq_abs]
    rcases hx i with h | h
    · rw [h,ha]
      ring
    · rw [h,hb]
      ring
  obtain ⟨w,hw,hMV⟩ : ∃ w > 0, TDist.logLik x m v s ≤ TDist.logLik x c v w := by
    rcases hv.eq_or_lt with h | h
    · have h' : v = 1 := h.symm
      subst v
      refine ⟨r,hr,?_⟩
      have H : IsMaxOn (fun p : ℝ × ℝ ↦ TDist.logLik x p.1 1 p.2) Cauchy.domain (c,r) := by
        rcases lt_or_gt_of_ne hab with H | H
        · apply ((StudentCauchy.lst_df_one_balanced_argmax x hn H hA hx (c,r)).mpr ?_).2
          refine ⟨by dsimp [c]; linarith,by dsimp [c]; linarith,hr,?_⟩
          dsimp [r,c]
          rw [div_pow,sq_abs]
          ring
        · apply ((StudentCauchy.lst_df_one_balanced_argmax x hn H hB
            (fun i ↦ (hx i).symm) (c,r)).mpr ?_).2
          refine ⟨by dsimp [c]; linarith,by dsimp [c]; linarith,hr,?_⟩
          dsimp [r,c]
          rw [div_pow,sq_abs]
          ring
      exact @H (m,s) ⟨mem_univ _,hs⟩
    · have hsym : IsMaxOn (fun p : ℝ × ℝ ↦ TDist.logLik x p.1 v p.2) (univ ×ˢ Ioi 0)
          (a+b-m,s) := by
        intro p hp
        change TDist.logLik x p.1 v p.2 ≤ TDist.logLik x (a+b-m) v s
        rw [reflect]
        exact @hcond p hp
      have he := StudentGeodesic.lst_held_df_unique x hn h hs hs hcond hsym
      have hM : m = a+b-m := congrArg Prod.fst he
      have hmc : m = c := by unfold c; linarith
      exact ⟨s,hs,by rw [hmc]⟩
  obtain ⟨q,hq,hh⟩ := lst_held_location_one_magnitude_escape x hn hr hv0 hw hmag
  have H := exp_lt_exp.mp hh
  exact H.not_ge ((pat c hr (hv.trans hq.le)).trans hMV)


/-- Exact spatial Student density shape score at unit scale. -/
theorem ordinary_log_density_shape_deriv {q z : ℝ} (hq : 0 < q) :
    HasDerivAt (fun v ↦ log (TDist.density 0 v 1 z))
      (deriv TDist.logNormalizer q-log (1+z^2/q)/2+
        (q+1)*z^2/(2*q*(q+z^2))) q := by
  have hp : 0 < q+z^2 := by positivity
  have hl : HasDerivAt (fun v : ℝ ↦ log (1+z^2/v)) (-z^2/(q*(q+z^2))) q := by
    have he : (fun v : ℝ ↦ log (1+z^2/v)) =ᶠ[𝓝 q] (fun v ↦ log (v+z^2)-log v) := by
      filter_upwards [Ioi_mem_nhds hq] with v hv
      have hv' : 0 < v := hv
      rw [← log_div (by positivity : v+z^2 ≠ 0) hv'.ne']
      congr 1
      field_simp
    have h := ((Real.hasDerivAt_log hp.ne').comp q ((hasDerivAt_id q).add_const (z^2))).sub
      (Real.hasDerivAt_log hq.ne')
    have hh : HasDerivAt (fun v ↦ log (v+z^2)-log v) (-z^2/(q*(q+z^2))) q := by
      convert h using 1
      · rfl
      · field_simp
        ring
    exact hh.congr_of_eventuallyEq he
  have hn := (normalizer_differentiable hq).hasDerivAt
  have h := hn.sub ((((hasDerivAt_id q).add_const 1).div_const 2).mul hl)
  simp_rw [TDist.density,log_exp,log_one,sub_zero,one_pow,mul_one]
  convert h using 1
  · rfl
  · simp only [id_eq]
    field_simp
    ring

/-- On the compact shape interval used by the large-outlier comparisons,
the base Student density's shape score is at most the displayed
logarithmic bound. -/
theorem student_shape_derivative_bound_one_two {q z : ℝ} (h1 : 1 ≤ q) (h2 : q ≤ 2) :
    deriv (fun v ↦ log (TDist.density 0 v 1 z)) q ≤ 5/4-log (1+z^2/2)/2 := by
  have hq : 0 < q := zero_lt_one.trans_le h1
  rw [(ordinary_log_density_shape_deriv hq).deriv]
  have ha := (Student2Half.student_log_normalizer_score_bounds hq).2
  have hb : 1/(4*q^2) ≤ 1/4 := by
    apply one_div_le_one_div_of_le (by norm_num)
    nlinarith [pow_le_pow_left₀ zero_le_one h1 2]
  have hc : (q+1)*z^2/(2*q*(q+z^2)) ≤ 1 := by
    apply (div_le_one (by positivity)).2
    have hm := mul_le_mul_of_nonneg_right (show q+1 ≤ 2*q by linarith) (sq_nonneg z)
    nlinarith [sq_nonneg q]
  have hD : z^2/2 ≤ z^2/q := div_le_div_of_nonneg_left (sq_nonneg z) hq h2
  have hE := log_le_log (show 0 < 1+z^2/2 by positivity)
    (show 1+z^2/2 ≤ 1+z^2/q by linarith)
  linarith


/-- In squared kernel width coordinates the Student shape derivative
has the log-Gamma Laplace term and a fixed data-log sum. At the
conditional scale root this is the shape score used for the real
crossing comparisons. -/
theorem student_squared_width_shape_score {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {u q : ℝ} (hu : 0 < u) (hq : 0 < q) :
    HasDerivAt (fun v ↦ TDist.logLik x m v (sqrt (u/v)))
      ((n:ℝ)*(∫ t in Ioi (0:ℝ), exp (-q*t)/(1+exp (-t))) -
        (∑ i, log (1+(x i-m)^2/u))/2) q := by
  let G := ∑ i, log (1+(x i-m)^2/u)
  let B := fun v : ℝ ↦ log (Gamma ((v+1)/2))-log (Gamma (v/2))
  let H := fun v : ℝ ↦ (n:ℝ)*(B v-log (π*u)/2)-(v+1)/2*G
  have hd : HasDerivAt H
      ((n:ℝ)*(∫ t in Ioi (0:ℝ), exp (-q*t)/(1+exp (-t)))-G/2) q := by
    have h := (((Student2Half.log_gamma_half_step_derivative hq).sub_const
      (log (π*u)/2)).const_mul (n:ℝ)).sub
      ((((hasDerivAt_id q).add_const 1).div_const 2).mul_const G)
    convert h using 1
    · rfl
    · ring
  apply hd.congr_of_eventuallyEq
  filter_upwards [Ioi_mem_nhds hq] with v hv
  have hv' : 0 < v := hv
  have hW : v*(sqrt (u/v))^2 = u := by rw [sq_sqrt (by positivity)]; field_simp
  have hN : TDist.logNormalizer v-log (sqrt (u/v)) = B v-log (π*u)/2 := by
    unfold TDist.logNormalizer B
    rw [log_sqrt (by positivity),log_div hu.ne' hv'.ne',log_mul hv'.ne' pi_pos.ne',
      log_mul pi_pos.ne' hu.ne']
    ring
  unfold TDist.logLik H
  rw [← hN]
  simp_rw [hW]
  rw [Finset.sum_sub_distrib,Finset.sum_const,← Finset.mul_sum]
  simp [G]



end UnivariateML.Student2WidthWitness
end
