module

public import UnivariateML.Family.Student2Skew
public import UnivariateML.Family.Student2Crossing
public import UnivariateML.Family.Student2WidthWitness

import Mathlib.Tactic
import Mathlib.Analysis.Real.Pi.Bounds

set_option linter.style.header false

/-!
# Exact skew-Student scale witnesses

The outlier is carried by symbolic inequalities on its size. The
normalized objective is the fitted likelihood multiplied by a strictly
positive factor independent of the fitted scale.
-/

@[expose] public section

open Set Real
open UnivariateML.Student2Crossing

namespace UnivariateML.Student2SkewWitness

/-- The 201 observations used by the large-slant scale crossing. -/
def largeSlantSample (D R : ℝ) : Fin 201 → ℝ :=
  Fin.append (fun _ : Fin 100 ↦ 1)
    (Fin.append (fun _ : Fin 100 ↦ D) (fun _ : Fin 1 ↦ R))

private theorem positive_cauchy_skew_term {A w x : ℝ} (hw : 0 < w) (hx : 0 < x) :
    exp (Student2Skew.spatialLogDensity 0 1 A w x) =
      exp (TDist.logNormalizer 1)*(w/(w^2+x^2))*skewFactor A w x := by
  rw [Student2Skew.skew_cauchy_single_term hw,sub_zero]
  unfold skewFactor
  have he : w^2+(1+A^2)*x^2 = (1+A^2+(w/x)^2)*x^2 := by field_simp; ring
  rw [he,sqrt_mul (by positivity),sqrt_sq hx.le]
  have hp : sqrt (1+A^2+(w/x)^2) ≠ 0 := by positivity
  field_simp

private theorem largeSlantSample_normalized {A R D w : ℝ}
    (hD : 0 < D) (hR : 0 < R) (hw : 0 < w) :
    StudentSkew.likelihood (largeSlantSample D R) 0 1 A w =
      (exp (TDist.logNormalizer 1)^201 * 2^101/(D^200*R^2)) *
        normalizedSlantProfile A R D w := by
  rw [Student2Skew.skew_likelihood_spatial_product _ _ _ zero_lt_one]
  unfold largeSlantSample
  rw [Fin.prod_univ_add (a := 100) (b := 101)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const]
  rw [Fin.prod_univ_add (a := 100) (b := 1)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const,pow_one]
  rw [positive_cauchy_skew_term hw zero_lt_one,positive_cauchy_skew_term hw hD,
    positive_cauchy_skew_term hw hR,one_pow]
  unfold normalizedSlantProfile
  have h1 : w/(w^2+1) = (w^2/(w^2+1))/w := by field_simp
  have h2 : w/(w^2+D^2) = w/D^2*(D^2/(D^2+w^2)) := by field_simp; ring
  have h3 : w/(w^2+R^2) = w/R^2*(R^2/(R^2+w^2)) := by field_simp; ring
  rw [h1,h2,h3]
  simp only [mul_pow,div_pow]
  field_simp [hw.ne',hD.ne',hR.ne']

/-- At the atlas slant, shape and location, the exact 201-point sample
has two distinct positive-scale global maximizers for some interior
real value of the second level. -/
theorem skew_cauchy_large_slant_scale_crossing :
    let A : ℝ := 2^20
    let R : ℝ := 2^50000
    ∃ D ∈ Ioo (1024*A) ((2:ℝ)^150*A), ∃ u v : ℝ,
      0 < u ∧ u < 8*A ∧ 8*A < v ∧
      IsMaxOn (StudentSkew.likelihood (largeSlantSample D R) 0 1 A) (Ioi 0) u ∧
      IsMaxOn (StudentSkew.likelihood (largeSlantSample D R) 0 1 A) (Ioi 0) v := by
  dsimp only
  let A : ℝ := 2^20
  let R : ℝ := 2^50000
  have hA : 1024 ≤ A := by
    change (1024:ℝ) ≤ (2:ℝ)^20
    norm_num
  have hR : (2:ℝ)^150*A ≤ R := by
    change (2:ℝ)^150*(2:ℝ)^20 ≤ (2:ℝ)^50000
    rw [← pow_add]
    exact pow_le_pow_right₀ (by norm_num) (by decide)
  obtain ⟨D,hD,u,v,hu,huA,hvA,hm,hn⟩ := large_slant_normalized_crossing hA hR
  have hD0 : 0 < D := (show 0 < 1024*A by positivity).trans hD.1
  have hv : 0 < v := (show 0 < 8*A by positivity).trans hvA
  have hR0 : 0 < R := by unfold R; positivity
  have hK : 0 ≤ exp (TDist.logNormalizer 1)^201 * 2^101/(D^200*R^2) := by positivity
  refine ⟨D,hD,u,v,hu,huA,hvA,?_,?_⟩
  · intro w hw
    change StudentSkew.likelihood (largeSlantSample D R) 0 1 A w ≤
      StudentSkew.likelihood (largeSlantSample D R) 0 1 A u
    rw [largeSlantSample_normalized hD0 hR0 hw,largeSlantSample_normalized hD0 hR0 hu]
    exact mul_le_mul_of_nonneg_left (@hm w hw) hK
  · intro w hw
    change StudentSkew.likelihood (largeSlantSample D R) 0 1 A w ≤
      StudentSkew.likelihood (largeSlantSample D R) 0 1 A v
    rw [largeSlantSample_normalized hD0 hR0 hw,largeSlantSample_normalized hD0 hR0 hv]
    exact mul_le_mul_of_nonneg_left (@hn w hw) hK


/-- Nine observations at each inner sign and one at each outer sign. -/
def reflectedSample (R : ℝ) : Fin 20 → ℝ :=
  Fin.append (fun _ : Fin 9 ↦ -1)
    (Fin.append (fun _ : Fin 9 ↦ 1) ![-R,R])

private noncomputable def cauchyTerm (m α a x : ℝ) : ℝ :=
  a/(a^2+(x-m)^2)*(1+α*(x-m)/sqrt (a^2+(1+α^2)*(x-m)^2))

private noncomputable def cauchyConstant : ℝ := exp (TDist.logNormalizer 1)^20

private theorem reflected_skew_at_one {R m α a : ℝ} (ha : 0 < a) :
    StudentSkew.likelihood (reflectedSample R) m 1 α a =
      cauchyConstant*((cauchyTerm m α a (-1)*cauchyTerm m α a 1)^9 *
        (cauchyTerm m α a (-R)*cauchyTerm m α a R)) := by
  rw [Student2Skew.skew_likelihood_spatial_product _ _ _ zero_lt_one]
  unfold reflectedSample
  rw [Fin.prod_univ_add (a := 9) (b := 11)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const]
  rw [Fin.prod_univ_add (a := 9) (b := 2)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const,Fin.prod_univ_two,Matrix.cons_val_zero,Matrix.cons_val_one]
  let c := exp (TDist.logNormalizer 1)
  have term (x : ℝ) : exp (Student2Skew.spatialLogDensity m 1 α a x) =
      c*cauchyTerm m α a x := by
    rw [Student2Skew.skew_cauchy_single_term ha]
    unfold c cauchyTerm
    rw [mul_assoc]
  simp_rw [term,mul_pow]
  change _ = c^20*(cauchyTerm m α a (-1)^9*cauchyTerm m α a 1^9 *
    (cauchyTerm m α a (-R)*cauchyTerm m α a R))
  rw [show c^20 = c^9*c^9*c*c by ring]
  ac_rfl

private theorem reflected_base_at_one {R m a : ℝ} (ha : 0 < a) :
    TDist.likelihood (reflectedSample R) m 1 a =
      cauchyConstant*(((a/(a^2+(-1-m)^2))*(a/(a^2+(1-m)^2)))^9 *
        ((a/(a^2+(-R-m)^2))*(a/(a^2+(R-m)^2)))) := by
  rw [← Student2Skew.zero_slant_likelihood_eq _ _ _ zero_lt_one,reflected_skew_at_one ha]
  simp [cauchyTerm]

private noncomputable def centeredUpper : ℝ := (20/81)^9*(5/4)

private theorem centered_real_shape_one_bound {R a : ℝ} (hR : 0 < R) (ha : 0 < a) :
    R^4*StudentSkew.likelihood (reflectedSample R) 0 1 0 a ≤ cauchyConstant*centeredUpper := by
  let p := fun t : ℝ ↦ t^10/(1+t)^18
  have hpk (t : ℝ) (ht : 0 < t) : p t ≤ p (5/4) := by
    let G := fun u : ℝ ↦ (10:ℕ)*log u-(18:ℕ)*log (1+u)
    have hE (u : ℝ) (hu : 0 < u) : exp (G u) = p u := by
      unfold G p
      rw [exp_sub,exp_nat_mul,exp_nat_mul,exp_log hu,exp_log (by positivity)]
    have hd (u : ℝ) (hu : 0 < u) :
        HasDerivAt G ((10-8*u)/(u*(1+u))) u := by
      have hL := (Real.hasDerivAt_log hu.ne').const_mul (10:ℝ)
      have hR := (((Real.hasDerivAt_log (show 1+u ≠ 0 by positivity)).comp u
        ((hasDerivAt_id u).const_add (1:ℝ))).const_mul (18:ℝ))
      convert hL.sub hR using 1
      · rfl
      · field_simp
        ring
    rw [← hE t ht,← hE (5/4) (by norm_num),exp_le_exp]
    have hc : ContinuousOn G (Ioi 0) := fun u hu ↦ (hd u hu).continuousAt.continuousWithinAt
    by_cases h : t ≤ 5/4
    · have hM : MonotoneOn G (Ioc 0 (5/4)) := by
        apply monotoneOn_of_deriv_nonneg (convex_Ioc _ _)
        · exact hc.mono Ioc_subset_Ioi_self
        · intro u hu
          rw [interior_Ioc] at hu
          exact (hd u hu.1).differentiableAt.differentiableWithinAt
        · intro u hu
          rw [interior_Ioc] at hu
          have h0 : 0 < u := hu.1
          rw [(hd u h0).deriv]
          exact div_nonneg (by linarith [hu.2]) (by positivity)
      exact hM ⟨ht,h⟩ (by norm_num) h
    · have hM : AntitoneOn G (Ici (5/4)) := by
        apply antitoneOn_of_deriv_nonpos (convex_Ici _)
        · exact hc.mono (fun u hu ↦ (by norm_num : (0:ℝ)<5/4).trans_le hu)
        · intro u hu
          rw [interior_Ici] at hu
          exact (hd u ((by norm_num : (0:ℝ)<5/4).trans hu)).differentiableAt.differentiableWithinAt
        · intro u hu
          rw [interior_Ici] at hu
          have hp : 0 < u := (by norm_num : (0:ℝ)<5/4).trans hu
          rw [(hd u hp).deriv]
          exact div_nonpos_of_nonpos_of_nonneg (by linarith [show (5:ℝ)/4 < u from hu]) (by positivity)
      exact hM le_rfl (le_of_not_ge h) (le_of_not_ge h)
  have hp5 : p (5/4) = centeredUpper := by norm_num [p,centeredUpper]
  rw [Student2Skew.zero_slant_likelihood_eq _ _ _ zero_lt_one,reflected_base_at_one ha]
  simp only [sub_zero,neg_sq,one_pow]
  have hA : R^4 * ((a/(a^2+1)*(a/(a^2+1)))^9*(a/(a^2+R^2)*(a/(a^2+R^2)))) =
      p (a^2)*(R^2/(a^2+R^2))^2 := by
    unfold p
    field_simp
    ring
  rw [← mul_assoc,mul_comm (R^4) cauchyConstant,mul_assoc,hA]
  apply mul_le_mul_of_nonneg_left _ (by unfold cauchyConstant; positivity)
  have hrat : R^2/(a^2+R^2) ≤ 1 := (div_le_one (by positivity)).2 (by linarith [sq_nonneg a])
  exact (mul_le_of_le_one_right (by unfold p; positivity)
    (pow_le_one₀ (by positivity) hrat)).trans ((hpk (a^2) (by positivity)).trans (le_of_eq hp5))

private theorem reflected_near_factors {a : ℝ} (ha : 0 < a) (ha2 : a^2 = 5/4) :
    (136/525 : ℝ) ≤ cauchyTerm (1/2) (-1/2) a (-1) * cauchyTerm (1/2) (-1/2) a 1 := by
  unfold cauchyTerm
  rw [ha2]
  have h1 : sqrt (5/4+(1+(-1/2:ℝ)^2)*(1-1/2)^2) = 5/4 := by
    rw [show 5/4+(1+(-1/2:ℝ)^2)*(1-1/2)^2 = (5/4)^2 by norm_num,sqrt_sq (by norm_num)]
  have h2 : sqrt (5/4+(1+(-1/2:ℝ)^2)*(-1-1/2)^2) = sqrt 65/4 := by
    norm_num [show (65:ℝ)/16 = 65/(4:ℝ)^2 by norm_num, sqrt_div (by norm_num : (0:ℝ) ≤ 65)]
  rw [h1,h2]
  have hR : sqrt (65:ℝ) < 25/3 := (sqrt_lt' (by norm_num)).2 (by norm_num)
  have hp : 0 < sqrt (65:ℝ) := by positivity
  have hm : (9/25:ℝ) ≤ 3/sqrt 65 := (le_div_iff₀ hp).2 (by linarith)
  have he :
      a/(5/4+(-1-1/2)^2)*(1+(-1/2)*(-1-1/2)/(sqrt 65/4)) *
        (a/(5/4+(1-1/2)^2)*(1+(-1/2)*(1-1/2)/(5/4))) =
      (4/21)*(1+3/sqrt 65) := by
    norm_num
    field_simp
    nlinarith [ha2]
  rw [he]
  linarith

private theorem reflected_far_factors {R a : ℝ} (hR : 100 ≤ R) (ha : 0 < a) (ha2 : a^2 = 5/4) :
    (5/4 : ℝ)*(49/50)^2*(77/100) ≤
      R^4*(cauchyTerm (1/2) (-1/2) a (-R)*cauchyTerm (1/2) (-1/2) a R) := by
  let d := R+1/2
  let e := R-1/2
  have hd : 0 < d := by unfold d; linarith
  have he : 0 < e := by unfold e; linarith
  let S := fun z : ℝ ↦ sqrt (5/4+(5/4)*z^2)
  have hSp (z : ℝ) : 0 < S z := by unfold S; positivity
  have hA : S d ≤ 5/4*d := by
    unfold S
    apply sqrt_le_iff.mpr ⟨by positivity,?_⟩
    have hds : 4 ≤ d^2 := by
      have h : (2:ℝ) ≤ d := by unfold d; linarith
      exact (show (4:ℝ)=2^2 by norm_num).le.trans (pow_le_pow_left₀ (by norm_num) h 2)
    nlinarith
  have hB : (10/9)*e ≤ S e := by
    apply le_sqrt_of_sq_le
    have h : 0 ≤ e^2 := sq_nonneg _
    nlinarith
  have hd' : 7/5 ≤ 1+d/2/S d := by
    have H : (2/5:ℝ) ≤ d/2/S d := (le_div_iff₀ (hSp d)).2 (by linarith)
    linarith
  have he' : 11/20 ≤ 1-e/2/S e := by
    have H : e/2/S e ≤ 9/20 := (div_le_iff₀ (hSp e)).2 (by linarith)
    linarith
  have hD : (49/50:ℝ) ≤ R^2/(5/4+d^2) := by
    apply (le_div_iff₀ (by positivity)).2
    dsimp [d]
    nlinarith [mul_nonneg (show 0 ≤ R by linarith) (show 0 ≤ R-100 by linarith)]
  have hE : (49/50:ℝ) ≤ R^2/(5/4+e^2) := by
    apply (le_div_iff₀ (by positivity)).2
    dsimp [e]
    nlinarith [sq_nonneg R]
  have hh :
      R^4*(cauchyTerm (1/2) (-1/2) a (-R)*cauchyTerm (1/2) (-1/2) a R) =
        (5/4)*(R^2/(5/4+d^2))*(R^2/(5/4+e^2))*(1+d/2/S d)*(1-e/2/S e) := by
    unfold cauchyTerm
    rw [ha2,show -R-1/2 = -d by dsimp [d];ring,show R-1/2 = e from rfl,neg_sq]
    have h1 : (1+(-1/2:ℝ)^2) = 5/4 := by norm_num
    rw [h1]
    change _ = (5/4)*(R^2/(5/4+d^2))*(R^2/(5/4+e^2))*(1+d/2/S d)*(1-e/2/S e)
    unfold S
    have hp : a*a = (5/4:ℝ) := by simpa [pow_two] using ha2
    linear_combination (R^4/(5/4+d^2)/(5/4+e^2)*(1+d/2/sqrt (5/4+(5/4)*d^2))*
      (1-e/2/sqrt (5/4+(5/4)*e^2)))*hp
  rw [hh,show (5/4:ℝ)*(49/50)^2*(77/100) = (5/4)*(49/50)*(49/50)*(7/5)*(11/20) by norm_num]
  gcongr

private theorem reflected_finite_witness_gt {R : ℝ} (hR : 100 ≤ R) :
    cauchyConstant*centeredUpper <
      R^4*StudentSkew.likelihood (reflectedSample R) (1/2) 1 (-1/2) (sqrt 5/2) := by
  let b := sqrt (5:ℝ)/2
  have hb : 0 < b := by unfold b; positivity
  have hb2 : b^2 = 5/4 := by unfold b; rw [div_pow,sq_sqrt (by norm_num)]; norm_num
  rw [reflected_skew_at_one hb]
  rw [← mul_assoc,mul_comm (R^4) cauchyConstant,mul_assoc]
  apply mul_lt_mul_of_pos_left _ (by unfold cauchyConstant; positivity)
  have hN := reflected_near_factors hb hb2
  have hF := reflected_far_factors hR hb hb2
  rw [show R^4*((cauchyTerm (1/2) (-1/2) b (-1)*cauchyTerm (1/2) (-1/2) b 1)^9 *
      (cauchyTerm (1/2) (-1/2) b (-R)*cauchyTerm (1/2) (-1/2) b R)) =
      (cauchyTerm (1/2) (-1/2) b (-1)*cauchyTerm (1/2) (-1/2) b 1)^9 *
        (R^4*(cauchyTerm (1/2) (-1/2) b (-R)*cauchyTerm (1/2) (-1/2) b R)) by ac_rfl]
  refine lt_of_lt_of_le ?_ (mul_le_mul (pow_le_pow_left₀ (by norm_num) hN 9) hF (by norm_num) (by
    exact pow_nonneg ((by norm_num : (0:ℝ)≤136/525).trans hN) _))
  norm_num [centeredUpper]

/-- A finite skew-Cauchy witness beats every centered zero-slant fit
on the reflected sample, uniformly in the symbolic outlier. -/
theorem skew_cauchy_reflected_witness_beats_center {R : ℝ} (hR : 100 ≤ R) :
    ∀ a > 0, StudentSkew.likelihood (reflectedSample R) 0 1 0 a <
      StudentSkew.likelihood (reflectedSample R) (1/2) 1 (-1/2) (sqrt 5/2) := by
  intro a ha
  have hR0 : 0 < R := by linarith
  exact (mul_lt_mul_iff_right₀ (show 0 < R^4 by positivity)).mp
    ((centered_real_shape_one_bound hR0 ha).trans_lt (reflected_finite_witness_gt hR))


private theorem reflected_atom_count {R : ℝ} (hR : 100 ≤ R) (b : ℝ) :
    TDist.multiplicity (reflectedSample R) b ≤ 9 := by
  classical
  unfold TDist.multiplicity
  rw [Finset.card_eq_sum_ones,Finset.sum_filter,Fin.sum_univ_add (a := 9) (b := 11)]
  simp only [reflectedSample,Fin.append_left,Fin.append_right,Fin.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul]
  rw [Fin.sum_univ_add (a := 9) (b := 2)]
  simp only [Fin.append_left,Fin.append_right,Fin.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul,
    Fin.sum_univ_two,Matrix.cons_val_zero,Matrix.cons_val_one,Matrix.cons_val_succ]
  by_cases h₁ : b = -1
  · subst b
    norm_num [show -R ≠ -1 by linarith,show R ≠ -1 by linarith]
  by_cases h₂ : b = 1
  · subst b
    norm_num [show -R ≠ 1 by linarith,show R ≠ 1 by linarith]
  have hn₁ : (-1:ℝ) ≠ b := Ne.symm h₁
  have hn₂ : (1:ℝ) ≠ b := Ne.symm h₂
  by_cases h₃ : -R = b <;> by_cases h₄ : R = b <;> norm_num [hn₁,hn₂,h₃,h₄]
  all_goals split_ifs <;> norm_num

private theorem reflected_exp_one_positive {R : ℝ} (hR : 100 ≤ R) (b : ℝ) :
    0 < TDist.exponent (reflectedSample R) b 1 := by
  have h : (TDist.multiplicity (reflectedSample R) b : ℝ) ≤ 9 := by
    exact_mod_cast reflected_atom_count hR b
  unfold TDist.exponent
  norm_num
  linarith

private theorem orientedWeight_twenty_upper (x : Fin 20 → ℝ) (b ε : ℝ) :
    0 ≤ Student2Skew.orientedWeight x b ε 2 ∧ Student2Skew.orientedWeight x b ε 2 ≤ 2^20 := by
  unfold Student2Skew.orientedWeight
  constructor
  · apply Finset.prod_nonneg
    intro i _
    split_ifs <;> norm_num
  · rw [← Fin.prod_const]
    apply Finset.prod_le_prod₀
    · intro i _
      split_ifs <;> norm_num
    · intro i _
      split_ifs <;> norm_num

private theorem reflected_oriented_outside {R b ε : ℝ} (hε : ε^2 = 1)
    (hW : Student2Skew.orientedWeight (reflectedSample R) b ε 2 ≠ 0) :
    b ≤ -R ∨ R ≤ b := by
  have hsign (i : Fin 20) : 0 ≤ ε*(reflectedSample R i-b) := by
    by_contra! h
    have hne : reflectedSample R i ≠ b := by
      intro he
      rw [he,sub_self,mul_zero] at h
      exact (lt_irrefl _) h
    apply hW
    unfold Student2Skew.orientedWeight
    apply Finset.prod_eq_zero (Finset.mem_univ i)
    simp [not_lt_of_ge h.le,hne]
  have he : ε = 1 ∨ ε = -1 := (sq_eq_sq_iff_eq_or_eq_neg).mp (by simpa using hε)
  rcases he with h | h
  · left
    have hh := hsign (Fin.natAdd 9 (Fin.natAdd 9 (0 : Fin 2)))
    simpa only [reflectedSample,Fin.append_right,Matrix.cons_val_zero,h,one_mul,sub_nonneg] using hh
  · right
    have hh := hsign (Fin.natAdd 9 (Fin.natAdd 9 (1 : Fin 2)))
    simpa only [reflectedSample,Fin.append_right,Matrix.cons_val_one,Matrix.cons_val_succ,Matrix.cons_val_zero,h,neg_one_mul,neg_nonneg,sub_nonpos] using hh

private theorem base_outside_bound {R b a : ℝ} (hR : 100 ≤ R) (ha : 0 < a)
    (hb : b ≤ -R ∨ R ≤ b) :
    TDist.likelihood (reflectedSample R) b 1 a ≤ cauchyConstant/(R-1)^20 := by
  let C := a/(a^2+(R-1)^2)
  let f := fun t : ℝ ↦ a/(a^2+(t-b)^2)
  have hD : 0 < R-1 := by linarith
  have hc : 0 < C := by unfold C; positivity
  have hf (t : ℝ) : 0 < f t := by unfold f; positivity
  have fb (t : ℝ) : f t ≤ 1/a := by
    unfold f
    apply (div_le_div_iff₀ (by positivity) ha).2
    nlinarith [sq_nonneg (t-b)]
  have fTail (t : ℝ) (ht : R-1 ≤ |t-b|) : f t ≤ C := by
    unfold f C
    have hQ : (R-1)^2 ≤ (t-b)^2 := by
      simpa only [sq_abs] using pow_le_pow_left₀ hD.le ht 2
    exact div_le_div_of_nonneg_left ha.le (by positivity) (by linarith)
  have hA : f (-1) ≤ C := by
    apply fTail
    rcases hb with h | h
    · linarith [le_abs_self ((-1:ℝ)-b)]
    · linarith [neg_le_abs ((-1:ℝ)-b)]
  have hB : f 1 ≤ C := by
    apply fTail
    rcases hb with h | h
    · linarith [le_abs_self ((1:ℝ)-b)]
    · linarith [neg_le_abs ((1:ℝ)-b)]
  have hF : f (-R)*f R ≤ (1/a)*C := by
    rcases hb with h | h
    · exact mul_le_mul (fb _) (fTail R (by linarith [le_abs_self (R-b)]))
        (hf _).le (by positivity)
    · rw [mul_comm (1/a) C]
      exact mul_le_mul (fTail (-R) (by linarith [neg_le_abs (-R-b)])) (fb _)
        (hf _).le hc.le
  have hprod : (f (-1)*f 1)^9*(f (-R)*f R) ≤ (C*C)^9*(1/a*C) := by
    have H := mul_le_mul hA hB (hf _).le hc.le
    exact mul_le_mul (pow_le_pow_left₀ (by positivity) H 9) hF (by positivity) (by positivity)
  rw [reflected_base_at_one ha]
  change cauchyConstant*((f (-1)*f 1)^9*(f (-R)*f R)) ≤ _
  have c0 : 0 < cauchyConstant := by unfold cauchyConstant; positivity
  refine (mul_le_mul_of_nonneg_left hprod c0.le).trans ?_
  rw [show cauchyConstant/(R-1)^20 = cauchyConstant*(1/(R-1)^20) by rw [mul_one_div]]
  apply mul_le_mul_of_nonneg_left _ c0.le
  have hce : (C*C)^9*(1/a*C) = a^18/(a^2+(R-1)^2)^19 := by
    unfold C
    field_simp
  rw [hce]
  apply (div_le_div_iff₀ (by positivity) (by positivity)).2
  have h1 : (a^2)^9 ≤ (a^2+(R-1)^2)^9 :=
    pow_le_pow_left₀ (sq_nonneg _) (by linarith [sq_nonneg (R-1)]) 9
  have h2 : ((R-1)^2)^10 ≤ (a^2+(R-1)^2)^10 :=
    pow_le_pow_left₀ (sq_nonneg _) (by linarith [sq_nonneg a]) 10
  have h3 := mul_le_mul h1 h2 (by positivity) (by positivity)
  rw [← pow_add] at h3
  simp only [← pow_mul] at h3
  norm_num at h3
  simpa using h3

private theorem reflected_oriented_upper {R b s ε : ℝ} (hR : 100 ≤ R) (hs : 0 < s)
    (hε : ε^2 = 1) :
    R^4*(TDist.likelihood (reflectedSample R) b 1 s *
      Student2Skew.orientedWeight (reflectedSample R) b ε 2) ≤ cauchyConstant*centeredUpper := by
  have h0 : 0 < R := by linarith
  have hC : 0 < cauchyConstant := by unfold cauchyConstant; positivity
  have hU : 0 < centeredUpper := by unfold centeredUpper; positivity
  by_cases hW : Student2Skew.orientedWeight (reflectedSample R) b ε 2 = 0
  · rw [hW,mul_zero,mul_zero]
    exact mul_nonneg hC.le hU.le
  have hb := reflected_oriented_outside hε hW
  have hw := orientedWeight_twenty_upper (reflectedSample R) b ε
  have hL := base_outside_bound hR hs hb
  have hBound : (R^4*2^20)/(R-1)^20 ≤ centeredUpper := by
    have hD : 0 < R-1 := by linarith
    have hh : R/2 ≤ R-1 := by linarith
    have h1 : (R/2)^20 ≤ (R-1)^20 := pow_le_pow_left₀ (by positivity) hh 20
    have ht := div_le_div_of_nonneg_left (show 0 ≤ R^4*2^20 by positivity)
      (show 0 < (R/2)^20 by positivity) h1
    refine ht.trans ?_
    have he : R^4 * 2^20 / (R/2)^20 = 2^40/R^16 := by
      field_simp
    rw [he]
    have h2 : (100:ℝ)^16 ≤ R^16 := pow_le_pow_left₀ (by norm_num) hR 16
    exact (div_le_div_of_nonneg_left (by positivity) (by norm_num) h2).trans
      (by norm_num [centeredUpper])
  calc
    _ ≤ R^4*((cauchyConstant/(R-1)^20)*2^20) := by
      exact mul_le_mul_of_nonneg_left (mul_le_mul hL hw.2 hw.1 (by positivity)) (by positivity)
    _ = cauchyConstant*((R^4*2^20)/(R-1)^20) := by ring
    _ ≤ _ := mul_le_mul_of_nonneg_left hBound hC.le


private theorem reflected_skew_symmetry {R m a α : ℝ} (ha : 0 < a) :
    StudentSkew.likelihood (reflectedSample R) (-m) 1 (-α) a =
      StudentSkew.likelihood (reflectedSample R) m 1 α a := by
  rw [reflected_skew_at_one ha,reflected_skew_at_one ha]
  have h (x : ℝ) : cauchyTerm (-m) (-α) a (-x) = cauchyTerm m α a x := by
    unfold cauchyTerm
    rw [show -x- -m = -(x-m) by ring,neg_sq,neg_sq,neg_mul_neg]
  have h1 : cauchyTerm (-m) (-α) a 1 = cauchyTerm m α a (-1) := by simpa using h (-1)
  have hR : cauchyTerm (-m) (-α) a R = cauchyTerm m α a (-R) := by simpa using h (-R)
  rw [h 1,h R,h1,hR]
  ac_rfl

set_option maxHeartbeats 1000000 in
-- The chart projections are expanded for the joined escape comparison.
/-- With df one held, the reflected atlas sample has distinct reflected
finite global skew-Student winners for every sufficiently remote outlier. -/
theorem skew_held_df_one_reflected_maxima {R : ℝ} (hR : 100 ≤ R) :
    let D := Student2Skew.freeSlantDomain none (some 1) none
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood (reflectedSample R)
      p.1 p.2.2.1 p.2.2.2 p.2.1
    ∃ m s α : ℝ, 0 < s ∧ (m ≠ 0 ∨ α ≠ 0) ∧
      IsMaxOn L D (m,s,1,α) ∧ IsMaxOn L D (-m,s,1,-α) := by
  dsimp only
  let D := Student2Skew.freeSlantDomain none (some 1) none
  let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood (reflectedSample R)
    p.1 p.2.2.1 p.2.2.2 p.2.1
  let F := Student2Skew.freeSlantFaces (reflectedSample R) none (some 1) none
  let C := cauchyConstant*centeredUpper
  have r0 : 0 < R := by linarith
  have hC : 0 < C := by unfold C cauchyConstant centeredUpper; positivity
  have hbound (x : ℝ) (hx : x ∈ F) : x ≤ C/R^4 := by
    have H : R^4*x ≤ C := by
      unfold F Student2Skew.freeSlantFaces at hx
      rcases hx with h | h | h | h | h | h
      · rw [h,mul_zero]
        exact hC.le
      · exact (Option.some_ne_none _ h.1).elim
      · rcases h with ⟨b,_,s,_,q,hq,ε,hs,hε,rfl⟩
        have hq' : q = 1 := by simpa [Student2Skew.shapes] using hq
        subst q
        change R^4*(TDist.likelihood (reflectedSample R) b 1 s *
          Student2Skew.orientedWeight (reflectedSample R) b ε 2) ≤ C
        exact reflected_oriented_upper hR hs hε
      · exact (Option.some_ne_none _ h.1).elim
      · rcases h with ⟨_,b,_,β,d,hd,hc,hr⟩
        exact ((reflected_exp_one_positive hR b).ne' hc).elim
      · rcases h with ⟨_,b,_,ε,hc,he,hr⟩
        exact ((reflected_exp_one_positive hR b).ne' hc).elim
    exact (le_div_iff₀ (by positivity)).2 (by simpa only [mul_comm] using H)
  have hFne : F.Nonempty := ⟨0,Or.inl rfl⟩
  have hB := csSup_le hFne hbound
  let w : ℝ × ℝ × ℝ × ℝ := (1/2,sqrt 5/2,1,-1/2)
  have hw : w ∈ D := by
    exact ⟨mem_univ _,by dsimp [w]; positivity,mem_univ _,rfl⟩
  have H := Student2Skew.held_df_decision (reflectedSample R) (by decide) zero_lt_one
  have hmode : ∃ p ∈ D, IsMaxOn L D p := by
    by_contra hh
    have hh' := ((H.2.2 (fun _ b _ ↦ (reflected_exp_one_positive hR b).le)).2.2.mp hh).2 w hw
    have hD : L w ≤ C/R^4 := hh'.le.trans hB
    have h0 : R^4*L w ≤ C := by
      have K := (le_div_iff₀ (show 0 < R^4 by positivity)).1 hD
      simpa only [mul_comm] using K
    exact (reflected_finite_witness_gt hR).not_ge h0
  obtain ⟨⟨m,s,q,α⟩,hp,hm⟩ := hmode
  have hs : 0 < s := hp.2.1
  have hq : q = 1 := by simpa [Student2Skew.shapes] using hp.2.2.2
  subst q
  have hne : m ≠ 0 ∨ α ≠ 0 := by
    by_contra! h
    obtain ⟨rfl,rfl⟩ := h
    have hh := @hm w hw
    exact (skew_cauchy_reflected_witness_beats_center hR s hs).not_ge hh
  refine ⟨m,s,α,hs,hne,hm,?_⟩
  intro p hp'
  change L p ≤ StudentSkew.likelihood (reflectedSample R) (-m) 1 (-α) s
  rw [reflected_skew_symmetry hs]
  exact @hm p hp'

private theorem reflected_base_grouped {R m q s : ℝ} :
    TDist.likelihood (reflectedSample R) m q s =
      (TDist.density m q s (-1)*TDist.density m q s 1)^9 *
      (TDist.density m q s (-R)*TDist.density m q s R) := by
  unfold TDist.likelihood TDist.logLik TDist.density
  rw [exp_sum]
  unfold reflectedSample
  rw [Fin.prod_univ_add (a := 9) (b := 11)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const]
  rw [Fin.prod_univ_add (a := 9) (b := 2)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const,Fin.prod_univ_two,
    Matrix.cons_val_zero,Matrix.cons_val_one,mul_pow]
  ac_rfl

private theorem cauchyConstant_lower : (1/4:ℝ)^20 ≤ cauchyConstant := by
  apply pow_le_pow_left₀ (by norm_num)
  unfold TDist.logNormalizer
  norm_num [Gamma_one,Gamma_one_half_eq,log_sqrt pi_pos.le]
  have he : -(log π / 2) - log π / 2 = -log π := by ring
  rw [he,exp_neg,exp_log pi_pos]
  simpa only [one_div] using (one_div_le_one_div_of_le pi_pos pi_lt_four.le)

private theorem reflected_witness_numeric_floor : (2:ℝ)^(-70:ℤ) ≤ cauchyConstant*centeredUpper := by
  refine le_trans ?_ (mul_le_mul_of_nonneg_right cauchyConstant_lower (by unfold centeredUpper; positivity))
  norm_num [centeredUpper]

private theorem reflected_centered_closed_shape {R s q : ℝ} (hR : (2:ℝ)^128 ≤ R)
    (hs : 0 < s) (h1 : 1 ≤ q) :
    R^4*TDist.likelihood (reflectedSample R) 0 q s ≤ cauchyConstant*centeredUpper := by
  have r0 : 0 < R := (by positivity : (0:ℝ)<2^128).trans_le hR
  have r1 : 100 ≤ R := (by norm_num : (100:ℝ)≤2^128).trans hR
  let f := fun t ↦ TDist.density 0 q s t
  have hfn (t : ℝ) : 0 ≤ f t := by unfold f TDist.density; positivity
  have hf (t : ℝ) : f t ≤ 2*s/(s^2+t^2) := by
    simpa only [f,sub_zero] using
      (Student2Normal.student_density_rational_upper (m := 0) (y := t) h1 hs)
  have hf0 (t : ℝ) : f t ≤ 2/s := (hf t).trans (by
    apply (div_le_div_iff₀ (by positivity) hs).2
    nlinarith [sq_nonneg t])
  have hfar (t : ℝ) (ha : t^2=R^2) : f t ≤ 2*s/R^2 := by
    apply (hf t).trans
    rw [ha]
    exact div_le_div_of_nonneg_left (by positivity) (by positivity) (by linarith [sq_nonneg s])
  by_cases hw : 1024 ≤ s
  · apply le_trans _ reflected_witness_numeric_floor
    rw [reflected_base_grouped]
    change R^4*((f (-1)*f 1)^9*(f (-R)*f R)) ≤ _
    have H := mul_le_mul_of_nonneg_left
      (mul_le_mul (pow_le_pow_left₀ (mul_nonneg (hfn (-1)) (hfn 1))
        (mul_le_mul (hf0 (-1)) (hf0 1) (hfn 1) (by positivity : 0 ≤ 2/s)) 9)
        (mul_le_mul (hfar (-R) (neg_sq R)) (hfar R rfl) (hfn R) (by positivity : 0 ≤ 2*s/R^2))
        (mul_nonneg (hfn (-R)) (hfn R)) (by positivity)) (by positivity : 0 ≤ R^4)
    refine H.trans ?_
    have E : R^4*((2/s*(2/s))^9*(2*s/R^2*(2*s/R^2))) = 2^20/s^16 := by field_simp <;> ring
    rw [E]
    exact (div_le_div_of_nonneg_left (by positivity) (by norm_num : 0<(1024:ℝ)^16)
      (pow_le_pow_left₀ (by norm_num) hw 16)).trans (by norm_num)
  have hw' : s ≤ 1024 := le_of_not_ge hw
  by_cases h2 : 2 ≤ q
  · apply le_trans _ reflected_witness_numeric_floor
    rw [reflected_base_grouped]
    change R^4*((f (-1)*f 1)^9*(f (-R)*f R)) ≤ _
    have hnear (t : ℝ) (ht : t^2=1) : f t ≤ 1 := (hf t).trans (by
      rw [ht]
      exact (div_le_one (by positivity)).2 (by nlinarith [sq_nonneg (s-1)]))
    have hfar' (t : ℝ) (ht : |t|=R) : f t ≤ 6*s^2/R^3 := by
      have hne : t ≠ 0 := by intro h; exact r0.ne' (by simpa [h] using ht.symm)
      simpa only [sub_zero,ht,f] using
        (Student2Normal.student_density_cubic_remote_upper (m := 0) (y := t) h2 hs (by simpa))
    have H := mul_le_mul_of_nonneg_left
      (mul_le_mul (pow_le_pow_left₀ (mul_nonneg (hfn (-1)) (hfn 1))
        (mul_le_mul (hnear (-1) (by norm_num)) (hnear 1 (by norm_num)) (hfn 1) (by norm_num)) 9)
        (mul_le_mul (hfar' (-R) (by simp [abs_of_pos r0])) (hfar' R (abs_of_pos r0))
          (hfn R) (by positivity : 0 ≤ 6*s^2/R^3)) (mul_nonneg (hfn (-R)) (hfn R)) (by positivity))
      (by positivity : 0 ≤ R^4)
    refine H.trans ?_
    have E : R^4*((1*1)^9*(6*s^2/R^3*(6*s^2/R^3))) = 36*s^4/R^2 := by field_simp <;> ring
    rw [E]
    calc
      _ ≤ 36*1024^4/R^2 := by gcongr
      _ ≤ 36*1024^4/((2:ℝ)^128)^2 :=
        div_le_div_of_nonneg_left (by positivity) (by positivity) (pow_le_pow_left₀ (by positivity) hR 2)
      _ ≤ _ := by norm_num
  have hlog2 : (1:ℝ)/2 ≤ log 2 := by
    have h := log_le_sub_one_of_pos (show (0:ℝ)<1/2 by norm_num)
    have he : log ((1:ℝ)/2) = -log 2 := by rw [one_div,log_inv]
    rw [he] at h
    linarith
  have htail : 25 < log (1+(R/s)^2/2) := by
    have H : (2:ℝ)^100 ≤ 1+(R/s)^2/2 := by
      have hs2 : s^2 ≤ (1024:ℝ)^2 := pow_le_pow_left₀ hs.le hw' 2
      have hR2 : ((2:ℝ)^128)^2 ≤ R^2 := pow_le_pow_left₀ (by positivity) hR 2
      rw [div_pow,div_div]
      have hp : 0 < s^2*2 := by positivity
      have hP : (2:ℝ)^100 * (s^2*2) ≤ R^2 := by
        calc
          _ ≤ (2:ℝ)^100*((1024:ℝ)^2*2) := by gcongr
          _ ≤ ((2:ℝ)^128)^2 := by norm_num
          _ ≤ _ := hR2
      have he := (le_div_iff₀ hp).2 hP
      linarith
    have ht := log_le_log (show 0 < (2:ℝ)^100 by positivity) H
    rw [log_pow] at ht
    norm_num at ht
    linarith
  let G := fun v ↦ (18:ℝ)*log (TDist.density 0 v 1 (1/s)) +
    2*log (TDist.density 0 v 1 (R/s))
  have hD {v : ℝ} (hv : 0 < v) :
      DifferentiableAt ℝ G v := by
    have h1 := (Student2WidthWitness.ordinary_log_density_shape_deriv (z := 1/s) hv).differentiableAt
    have h2 := (Student2WidthWitness.ordinary_log_density_shape_deriv (z := R/s) hv).differentiableAt
    exact (h1.const_mul _).add (h2.const_mul _)
  have hAnti : AntitoneOn G (Icc 1 2) := by
    apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
    · exact fun v hv ↦ (hD (zero_lt_one.trans_le hv.1)).continuousAt.continuousWithinAt
    · intro v hv
      rw [interior_Icc] at hv
      exact (hD (zero_lt_one.trans hv.1)).differentiableWithinAt
    · intro v hv
      rw [interior_Icc] at hv
      have hb1 := Student2WidthWitness.student_shape_derivative_bound_one_two (z := 1/s) hv.1.le hv.2.le
      have hb2 := Student2WidthWitness.student_shape_derivative_bound_one_two (z := R/s) hv.1.le hv.2.le
      have hn : 0 ≤ log (1+((1:ℝ)/s)^2/2) := log_nonneg (by linarith [div_nonneg (sq_nonneg ((1:ℝ)/s)) (show (0:ℝ) ≤ 2 by norm_num)])
      have h1d := Student2WidthWitness.ordinary_log_density_shape_deriv (z := 1/s) (zero_lt_one.trans hv.1)
      have h2d := Student2WidthWitness.ordinary_log_density_shape_deriv (z := R/s) (zero_lt_one.trans hv.1)
      have h := ((h1d.const_mul (18:ℝ)).add (h2d.const_mul (2:ℝ))).deriv
      change deriv G v = _ at h
      rw [h,← h1d.deriv,← h2d.deriv]
      linarith
  have hEq (v : ℝ) (hv : 0 < v) :
      TDist.likelihood (reflectedSample R) 0 v s = exp (G v)/(s^20) := by
    rw [reflected_base_grouped]
    have hScale (t : ℝ) : TDist.density 0 v s t = TDist.density 0 v 1 (t/s)/s := by
      unfold TDist.density
      simp only [log_one,sub_zero,one_pow,mul_one]
      rw [show t^2/(v*s^2) = (t/s)^2/v by field_simp]
      calc
        _ = exp ((TDist.logNormalizer v-(v+1)/2*log (1+(t/s)^2/v))-log s) := by congr 1 <;> ring
        _ = _ := by rw [exp_sub,exp_log hs]
    simp_rw [hScale]
    unfold G
    rw [exp_add,show (18:ℝ)=(18:ℕ) by norm_num,show (2:ℝ)=(2:ℕ) by norm_num,
      exp_nat_mul,exp_nat_mul,exp_log (by unfold TDist.density; positivity),exp_log (by unfold TDist.density; positivity)]
    have hEven (r : ℝ) : TDist.density 0 v 1 ((-r)/s) = TDist.density 0 v 1 (r/s) := by
      simp [TDist.density,neg_div]
    rw [hEven,hEven]
    ring
  have hG := hAnti (by norm_num : (1:ℝ) ∈ Icc 1 2) ⟨h1,le_of_not_ge h2⟩ h1
  have H : TDist.likelihood (reflectedSample R) 0 q s ≤
      TDist.likelihood (reflectedSample R) 0 1 s := by
    rw [hEq q (zero_lt_one.trans_le h1),hEq 1 zero_lt_one]
    exact div_le_div_of_nonneg_right (exp_le_exp.mpr hG) (by positivity)
  have K := centered_real_shape_one_bound r0 hs
  rw [Student2Skew.zero_slant_likelihood_eq _ _ _ zero_lt_one] at K
  exact (mul_le_mul_of_nonneg_left H (by positivity)).trans K


private theorem reflected_tail_half_t {R b q s ε : ℝ} (hR : (2:ℝ)^128 ≤ R)
    (hq : 1 ≤ q) (hs : 0 < s) (hε : ε^2=1) :
    R^4*(TDist.likelihood (reflectedSample R) b q s *
      Student2Skew.orientedWeight (reflectedSample R) b ε 2) ≤ cauchyConstant*centeredUpper := by
  have r1 : 100 ≤ R := (by norm_num : (100:ℝ)≤2^128).trans hR
  by_cases hw : Student2Skew.orientedWeight (reflectedSample R) b ε 2 = 0
  · simp only [hw,mul_zero]
    unfold cauchyConstant centeredUpper
    positivity
  have hb := reflected_oriented_outside hε hw
  have hW := orientedWeight_twenty_upper (reflectedSample R) b ε
  let C := s/(s^2+(R-1)^2)
  have hC : 0 ≤ C := by unfold C; positivity
  have hD (t : ℝ) : 0 ≤ TDist.density b q s t := by unfold TDist.density; positivity
  have hCore (t : ℝ) : TDist.density b q s t ≤ 2*(1/s) := by
    apply (Student2Normal.student_density_rational_upper hq hs).trans
    have h := div_le_div_of_nonneg_left (show 0 ≤ 2*s by positivity) (show 0 < s^2 by positivity)
      (show s^2 ≤ s^2+(t-b)^2 by linarith [sq_nonneg (t-b)])
    refine h.trans (le_of_eq ?_)
    field_simp
  have hTail (t : ℝ) (ht : R-1 ≤ |t-b|) : TDist.density b q s t ≤ 2*C := by
    apply (Student2Normal.student_density_rational_upper hq hs).trans
    have hd : 0 ≤ R-1 := by linarith
    have hQ : (R-1)^2 ≤ (t-b)^2 := by simpa only [sq_abs] using pow_le_pow_left₀ hd ht 2
    unfold C
    rw [← mul_div_assoc]
    exact div_le_div_of_nonneg_left (by positivity) (by positivity) (by linarith)
  have hA : TDist.density b q s (-1) ≤ 2*C := by
    apply hTail
    rcases hb with h | h
    · linarith [le_abs_self ((-1:ℝ)-b)]
    · linarith [neg_le_abs ((-1:ℝ)-b)]
  have hB : TDist.density b q s 1 ≤ 2*C := by
    apply hTail
    rcases hb with h | h
    · linarith [le_abs_self ((1:ℝ)-b)]
    · linarith [neg_le_abs ((1:ℝ)-b)]
  have hF : TDist.density b q s (-R)*TDist.density b q s R ≤ (2*(1/s))*(2*C) := by
    rcases hb with h | h
    · exact mul_le_mul (hCore _) (hTail R (by linarith [le_abs_self (R-b)])) (hD _) (by positivity)
    · rw [mul_comm (2*(1/s)) (2*C)]
      exact mul_le_mul (hTail (-R) (by linarith [neg_le_abs (-R-b)])) (hCore _) (hD _) (by positivity)
  have hT : TDist.likelihood (reflectedSample R) b q s ≤ 2^20/(R-1)^20 := by
    rw [reflected_base_grouped]
    have hab := mul_le_mul hA hB (hD 1) (by positivity : 0 ≤ 2*C)
    have H := mul_le_mul (pow_le_pow_left₀ (mul_nonneg (hD (-1)) (hD 1)) hab 9)
      hF (mul_nonneg (hD (-R)) (hD R)) (by positivity)
    refine H.trans ?_
    have E : (2*C*(2*C))^9*(2*(1/s)*(2*C)) = 2^20*(s^18/(s^2+(R-1)^2)^19) := by
      unfold C
      field_simp <;> ring
    rw [E,show (2:ℝ)^20/(R-1)^20 = 2^20*(1/(R-1)^20) by ring]
    apply mul_le_mul_of_nonneg_left _ (by positivity)
    have hR1 : 0 < R-1 := by linarith
    apply (div_le_div_iff₀ (by positivity) (pow_pos hR1 20)).2
    have h1 : (s^2)^9 ≤ (s^2+(R-1)^2)^9 := pow_le_pow_left₀ (by positivity) (by linarith [sq_nonneg (R-1)]) 9
    have h2 : ((R-1)^2)^10 ≤ (s^2+(R-1)^2)^10 :=
      pow_le_pow_left₀ (by positivity) (by linarith [sq_nonneg s]) 10
    have H := mul_le_mul h1 h2 (by positivity) (by positivity)
    rw [← pow_add] at H
    simpa only [← pow_mul,show 2*9=18 by decide,show 2*10=20 by decide,show 9+10=19 by decide,
      one_mul] using H
  apply le_trans _ reflected_witness_numeric_floor
  have Rpos : 0 < R := by linarith
  have hR4 : 0 ≤ R^4 := by positivity
  calc
    _ ≤ R^4*(2^20/(R-1)^20*2^20) :=
      mul_le_mul_of_nonneg_left (mul_le_mul hT hW.2 hW.1 (by positivity)) hR4
    _ ≤ R^4*(2^20/(R/2)^20*2^20) := by
      have h := pow_le_pow_left₀ (show 0 ≤ R/2 by positivity) (show R/2 ≤ R-1 by linarith) 20
      gcongr
    _ = 2^60/R^16 := by field_simp <;> ring
    _ ≤ _ := (div_le_div_of_nonneg_left (by positivity) (by positivity : 0 < ((2:ℝ)^128)^16)
      (pow_le_pow_left₀ (by positivity) hR 16)).trans (by norm_num)

private theorem reflected_normal_bound {R b s : ℝ} (hR : (2:ℝ)^128 ≤ R) (hs : 0 < s) :
    R^4*((∏ i, TDist.normalDensity b s (reflectedSample R i))*2^20) ≤
      cauchyConstant*centeredUpper := by
  have r1 : 100 ≤ R := (by norm_num : (100:ℝ)≤2^128).trans hR
  have r0 : 0 < R := by linarith
  have hQ : ∑ i, (reflectedSample R i-b)^2 =
      9*(-1-b)^2+9*(1-b)^2+((-R-b)^2+(R-b)^2) := by
    unfold reflectedSample
    rw [Fin.sum_univ_add (a := 9) (b := 11)]
    simp only [Fin.append_left,Fin.append_right,Fin.sum_const]
    rw [Fin.sum_univ_add (a := 9) (b := 2)]
    simp only [Fin.append_left,Fin.append_right,Fin.sum_const,Fin.sum_univ_two,
      Matrix.cons_val_zero,Matrix.cons_val_one,nsmul_eq_mul,Fintype.card_fin,Nat.cast_ofNat]
    ring
  have hSq : 2*R^2 ≤ ∑ i, (reflectedSample R i-b)^2 := by rw [hQ]; nlinarith [sq_nonneg b]
  have hG : 0 ≤ log (2*π) := log_nonneg (by linarith [pi_gt_three])
  have hF : (∏ i, TDist.normalDensity b s (reflectedSample R i)) ≤
      exp (-(R^2/s^2))/s^20 := by
    unfold TDist.normalDensity
    rw [← exp_sum,← exp_log (show 0 < s^20 by positivity),← exp_sub,log_pow,exp_le_exp]
    rw [Finset.sum_sub_distrib,← Finset.sum_div,Finset.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul]
    have h := div_le_div_of_nonneg_right hSq (show 0 ≤ s^2 by positivity)
    have he : 2*R^2/s^2 = 2*(R^2/s^2) := by ring
    rw [he] at h
    have h' : 2*(R^2/s^2) ≤ ∑ i, (reflectedSample R i-b)^2/s^2 := by
      rw [← Finset.sum_div]
      exact h
    norm_num
    linarith only [hG,h']
  have he : exp (-(R^2/s^2))/s^20 ≤ (Nat.factorial 10:ℝ)/R^20 := by
    let u := R^2/s^2
    have hu : 0 < u := by unfold u; positivity
    have hE : u^10/(Nat.factorial 10:ℝ) ≤ exp u := by
      have H := sum_le_exp_of_nonneg hu.le 11
      refine (Finset.single_le_sum (f := fun i : ℕ ↦ u^i/(Nat.factorial i:ℝ))
        (fun i _ ↦ by positivity) (by simp : 10 ∈ Finset.range 11)).trans H
    change exp (-u)/s^20 ≤ _
    rw [exp_neg]
    apply (div_le_div_iff₀ (by positivity) (by positivity)).2
    have hT := (div_le_iff₀ (show 0 < (Nat.factorial 10:ℝ) by positivity)).1 hE
    have hT' := mul_le_mul_of_nonneg_left hT (show 0 ≤ s^20 by positivity)
    have hU : s^20*u^10=R^20 := by unfold u; field_simp <;> ring
    rw [hU] at hT'
    have hT'' : R^20 ≤ (Nat.factorial 10:ℝ)*exp u*s^20 := by
      convert hT' using 1 <;> ring
    calc
      _ = (exp u)⁻¹ * R^20 := by ring
      _ ≤ (exp u)⁻¹ * ((Nat.factorial 10:ℝ)*exp u*s^20) :=
        mul_le_mul_of_nonneg_left hT'' (by positivity)
      _ = _ := by field_simp <;> ring
  apply le_trans _ reflected_witness_numeric_floor
  calc
    _ ≤ R^4*(((Nat.factorial 10:ℝ)/R^20)*2^20) :=
      mul_le_mul_of_nonneg_left (mul_le_mul_of_nonneg_right (hF.trans he) (by positivity)) (by positivity)
    _ = ((Nat.factorial 10:ℝ)*2^20)/R^16 := by field_simp <;> ring
    _ ≤ _ := (div_le_div_of_nonneg_left (by positivity) (by positivity : 0 < ((2:ℝ)^128)^16)
      (pow_le_pow_left₀ (by positivity) hR 16)).trans (by norm_num)


private theorem reflected_skew_symmetry_fitted {R m q s α : ℝ} (hq : 0 < q) :
    StudentSkew.likelihood (reflectedSample R) (-m) q (-α) s =
      StudentSkew.likelihood (reflectedSample R) m q α s := by
  rw [Student2Skew.skew_likelihood_spatial_product _ _ _ hq,
    Student2Skew.skew_likelihood_spatial_product _ _ _ hq]
  have h (t : ℝ) : Student2Skew.spatialLogDensity (-m) q (-α) s (-t) =
      Student2Skew.spatialLogDensity m q α s t := by
    unfold Student2Skew.spatialLogDensity TDist.density
    rw [show -t- -m = -(t-m) by ring,neg_sq,neg_mul_neg]
  unfold reflectedSample
  rw [Fin.prod_univ_add (a := 9) (b := 11),Fin.prod_univ_add (a := 9) (b := 11)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const]
  rw [Fin.prod_univ_add (a := 9) (b := 2),Fin.prod_univ_add (a := 9) (b := 2)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const,Fin.prod_univ_two,Matrix.cons_val_zero,Matrix.cons_val_one]
  have h1 : Student2Skew.spatialLogDensity (-m) q (-α) s 1 =
      Student2Skew.spatialLogDensity m q α s (-1) := by simpa only [neg_neg] using h (-1)
  have hR : Student2Skew.spatialLogDensity (-m) q (-α) s R =
      Student2Skew.spatialLogDensity m q α s (-R) := by simpa only [neg_neg] using h (-R)
  rw [h 1,h R,h1,hR]
  ac_rfl

set_option maxHeartbeats 1000000 in
/-- On the fitted branch the symbolic reflected sample has distinct finite
global skew-Student winners, both with scale free and with the prescribed
scale held. The same witness dominates the face suprema in both charts. -/
theorem skew_fitted_reflected_maxima {R : ℝ} (hR : (2:ℝ)^128 ≤ R) (ω : Option ℝ)
    (hω : ω = none ∨ ω = some (sqrt 5/2)) :
    let D := Student2Skew.freeSlantDomain none none ω
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood (reflectedSample R)
      p.1 p.2.2.1 p.2.2.2 p.2.1
    ∃ m s q α : ℝ, (m,s,q,α) ∈ D ∧ (m ≠ 0 ∨ α ≠ 0) ∧
      IsMaxOn L D (m,s,q,α) ∧ IsMaxOn L D (-m,s,q,-α) := by
  dsimp only
  let D := Student2Skew.freeSlantDomain none none ω
  let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood (reflectedSample R)
    p.1 p.2.2.1 p.2.2.2 p.2.1
  let F := Student2Skew.freeSlantFaces (reflectedSample R) none none ω
  let C := cauchyConstant*centeredUpper
  have r1 : 100 ≤ R := (by norm_num : (100:ℝ)≤2^128).trans hR
  have r0 : 0 < R := by linarith
  have hC : 0 < C := by unfold C cauchyConstant centeredUpper; positivity
  have hbound (x : ℝ) (hx : x ∈ F) : x ≤ C/R^4 := by
    apply (le_div_iff₀ (show 0 < R^4 by positivity)).2
    rw [mul_comm x]
    unfold F Student2Skew.freeSlantFaces at hx
    rcases hx with h | h | h | h | h | h
    · rw [h,mul_zero]
      exact hC.le
    · obtain ⟨_,b,_,s,_,β,hs,rfl⟩ := h
      unfold Student2Skew.skewNormalHeight
      refine le_trans ?_ (reflected_normal_bound (b := b) hR hs)
      apply mul_le_mul_of_nonneg_left _ (by positivity)
      apply mul_le_mul_of_nonneg_left _ (Finset.prod_nonneg fun _ _ ↦ (exp_pos _).le)
      rw [← Fin.prod_const]
      apply Finset.prod_le_prod₀
      · intro i _
        exact mul_nonneg zero_le_two (SkewLSGaussian.cdf_pos _).le
      · intro i _
        apply mul_le_of_le_one_right zero_le_two
        exact (SkewLSGaussian.cdf_lt_one _).le
    · obtain ⟨b,_,s,_,q,hq,ε,hs,hε,rfl⟩ := h
      exact reflected_tail_half_t hR (by simpa [Student2Skew.shapes] using hq) hs hε
    · obtain ⟨_,b,_,s,_,ε,hs,_,rfl⟩ := h
      refine le_trans ?_ (reflected_normal_bound (b := b) hR hs)
      exact mul_le_mul_of_nonneg_left (mul_le_mul_of_nonneg_left (orientedWeight_twenty_upper _ _ _).2
        (Finset.prod_nonneg fun _ _ ↦ (exp_pos _).le)) (by positivity)
    · obtain ⟨_,b,_,β,d,_,hc,_⟩ := h
      exact ((reflected_exp_one_positive r1 b).ne' hc).elim
    · obtain ⟨_,b,_,ε,hc,_,_⟩ := h
      exact ((reflected_exp_one_positive r1 b).ne' hc).elim
  have hB := csSup_le (show F.Nonempty from ⟨0,Or.inl rfl⟩) hbound
  let w : ℝ × ℝ × ℝ × ℝ := (1/2,sqrt 5/2,1,-1/2)
  have hw : w ∈ D := by
    rcases hω with h | h
    · exact ⟨mem_univ _,by dsimp [w]; positivity,by simp [h,Student2Skew.scales],le_rfl⟩
    · exact ⟨mem_univ _,by dsimp [w]; positivity,by simp [h,w,Student2Skew.scales],le_rfl⟩
  have hω0 : 0 < ω.getD 1 := by rcases hω with rfl | rfl <;> simp <;> positivity
  have H := Student2Skew.free_slant_decision (reflectedSample R) (by decide) none none ω zero_lt_one hω0
  have hmode : ∃ p ∈ D, IsMaxOn L D p := by
    by_contra hh
    have hh' := ((H.2.2 (fun _ b _ ↦ (reflected_exp_one_positive r1 b).le)).2.2.mp hh).2 w hw
    have hD : L w ≤ C/R^4 := hh'.le.trans hB
    have h0 : R^4*L w ≤ C := by
      have K := (le_div_iff₀ (show 0 < R^4 by positivity)).1 hD
      simpa only [mul_comm] using K
    exact (reflected_finite_witness_gt r1).not_ge h0
  obtain ⟨⟨m,s,q,α⟩,hp,hm⟩ := hmode
  have hs : 0 < s := hp.2.1
  have hq : 1 ≤ q := hp.2.2.2
  have hne : m ≠ 0 ∨ α ≠ 0 := by
    by_contra! h
    obtain ⟨rfl,rfl⟩ := h
    have hh := @hm w hw
    change L w ≤ StudentSkew.likelihood (reflectedSample R) 0 q 0 s at hh
    rw [Student2Skew.zero_slant_likelihood_eq _ _ _ (zero_lt_one.trans_le hq)] at hh
    exact (reflected_finite_witness_gt r1).not_ge
      ((mul_le_mul_of_nonneg_left hh (by positivity : 0 ≤ R^4)).trans
        (reflected_centered_closed_shape hR hs hq))
  refine ⟨m,s,q,α,hp,hne,hm,?_⟩
  intro p hp'
  change L p ≤ StudentSkew.likelihood (reflectedSample R) (-m) q (-α) s
  rw [reflected_skew_symmetry_fitted (zero_lt_one.trans_le hq)]
  exact @hm p hp'


private theorem large_slant_grouped {q A D R s : ℝ} (hq : 0 < q) :
    StudentSkew.likelihood (largeSlantSample D R) 0 q A s =
      exp (100*Student2Skew.spatialLogDensity 0 q A s 1+
        100*Student2Skew.spatialLogDensity 0 q A s D+
        Student2Skew.spatialLogDensity 0 q A s R) := by
  rw [Student2Skew.skew_likelihood_spatial_product _ _ _ hq]
  unfold largeSlantSample
  rw [Fin.prod_univ_add (a := 100) (b := 101)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const]
  rw [Fin.prod_univ_add (a := 100) (b := 1)]
  simp only [Fin.append_left,Fin.append_right,Fin.prod_const,pow_one,exp_add]
  rw [show (100:ℝ)=(100:ℕ) by norm_num]
  simp only [exp_nat_mul]
  ac_rfl

private theorem large_slant_floor_pointwise {q A s x : ℝ} (h1 : 1 ≤ q) (h2 : q ≤ 2)
    (hA : 0 < A) (hs : 0 < s) (hx : 0 < x) :
    Student2Skew.spatialLogDensity 0 q A s x-Student2Skew.spatialLogDensity 0 1 A s x ≤
      (11/4-log (1+(x/s)^2/2)/2)*(q-1) := by
  let C := 5/4-log (1+(x/s)^2/2)/2
  let G := fun v ↦ log (TDist.density 0 v 1 (x/s))-C*v
  have H {v : ℝ} (hv : 0 < v) :
      HasDerivAt G (deriv (fun t ↦ log (TDist.density 0 t 1 (x/s))) v - C) v := by
    have h := (Student2WidthWitness.ordinary_log_density_shape_deriv (z := x/s) hv).differentiableAt.hasDerivAt
    have he := h.sub ((hasDerivAt_id v).const_mul C)
    change HasDerivAt G (_-C*1) v at he
    simpa only [mul_one] using he
  have hAnti : AntitoneOn G (Icc 1 q) := by
    apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
    · exact fun v hv ↦ (H (zero_lt_one.trans_le hv.1)).continuousAt.continuousWithinAt
    · intro v hv
      rw [interior_Icc] at hv
      exact (H (zero_lt_one.trans hv.1)).differentiableAt.differentiableWithinAt
    · intro v hv
      rw [interior_Icc] at hv
      rw [(H (zero_lt_one.trans hv.1)).deriv]
      exact sub_nonpos.mpr
        (Student2WidthWitness.student_shape_derivative_bound_one_two hv.1.le (hv.2.le.trans h2))
  have hB := hAnti ⟨le_rfl,h1⟩ ⟨h1,le_rfl⟩ h1
  let arg := fun v : ℝ ↦ A*x*sqrt ((v+1)/(v*s^2+x^2))
  have hCdf := Student2Skew.positive_slant_cdf_shape_floor_ratio h1 hA hs hx
  have hCdf' : StudentSkew.studentCDF (q+1) (arg q) ≤
      exp ((3/2:ℝ)*(q-1))*StudentSkew.studentCDF 2 (arg 1) := by
    convert hCdf using 1 <;> norm_num [arg]
  have hCdfPos : 0 < StudentSkew.studentCDF (q+1) (arg q) :=
    Student2Skew.cdf_pos (by linarith) _
  have hCdfPos' : 0 < StudentSkew.studentCDF 2 (arg 1) :=
    Student2Skew.cdf_pos (by norm_num) _
  have hL := log_le_log (mul_pos two_pos hCdfPos)
    (mul_le_mul_of_nonneg_left hCdf' (by norm_num : (0:ℝ) ≤ 2))
  rw [show 2*(exp ((3/2:ℝ)*(q-1))*StudentSkew.studentCDF 2 (arg 1)) =
    exp ((3/2:ℝ)*(q-1))*(2*StudentSkew.studentCDF 2 (arg 1)) by ring,
    log_mul (exp_pos _).ne' (by positivity),log_exp] at hL
  have hEq (v : ℝ) (hv : 0 < v) : Student2Skew.spatialLogDensity 0 v A s x =
      log (TDist.density 0 v 1 (x/s))-log s+log (2*StudentSkew.studentCDF (v+1) (arg v)) := by
    unfold Student2Skew.spatialLogDensity TDist.density arg
    simp only [log_exp,sub_zero,log_one,one_pow,mul_one]
    rw [show x^2/(v*s^2) = (x/s)^2/v by field_simp]
    ring
  rw [hEq q (zero_lt_one.trans_le h1),hEq 1 zero_lt_one]
  unfold G at hB
  unfold C at hB
  linarith


private theorem large_slant_floor_compact_width {q A s D R : ℝ} (h1 : 1 < q) (h2 : q ≤ 2)
    (hA : 0 < A) (hs : 0 < s) (hD : 0 < D)
    (hW : s ≤ (2:ℝ)^256) (hR : (2:ℝ)^50000 ≤ R) :
    StudentSkew.likelihood (largeSlantSample D R) 0 q A s <
      StudentSkew.likelihood (largeSlantSample D R) 0 1 A s := by
  have hR0 : 0 < R := (by positivity : (0:ℝ) < 2^50000).trans_le hR
  have hlog2 : (1:ℝ)/2 ≤ log 2 := by
    have h := log_le_sub_one_of_pos (show (0:ℝ)<1/2 by norm_num)
    rw [show log ((1:ℝ)/2) = -log 2 by rw [one_div,log_inv]] at h
    linarith
  have htail : 1200 < log (1+(R/s)^2/2) := by
    have H : (2:ℝ)^3000 ≤ 1+(R/s)^2/2 := by
      have hs2 : s^2 ≤ ((2:ℝ)^256)^2 := pow_le_pow_left₀ hs.le hW 2
      have hR2 : ((2:ℝ)^50000)^2 ≤ R^2 := pow_le_pow_left₀ (by positivity) hR 2
      rw [div_pow,div_div]
      have hP : (2:ℝ)^3000*(s^2*2) ≤ R^2 := by
        calc
          _ ≤ (2:ℝ)^3000*(((2:ℝ)^256)^2*2) := by gcongr
          _ ≤ ((2:ℝ)^50000)^2 := by
            rw [← pow_mul, ← pow_mul, ← pow_succ, ← pow_add]
            exact pow_le_pow_right₀ (by norm_num) (by decide)
          _ ≤ _ := hR2
      have he := (le_div_iff₀ (show 0 < s^2*2 by positivity)).2 hP
      linarith
    have ht := log_le_log (show 0 < (2:ℝ)^3000 by positivity) H
    rw [log_pow] at ht
    norm_num at ht
    linarith
  rw [large_slant_grouped (zero_lt_one.trans h1),large_slant_grouped zero_lt_one,exp_lt_exp]
  have hNear (x : ℝ) (hx : 0 < x) :
      Student2Skew.spatialLogDensity 0 q A s x-Student2Skew.spatialLogDensity 0 1 A s x ≤
        11/4*(q-1) := by
    refine (large_slant_floor_pointwise h1.le h2 hA hs hx).trans ?_
    have hL : 0 ≤ log (1+(x/s)^2/2) := log_nonneg (by linarith [div_nonneg (sq_nonneg (x/s)) (show (0:ℝ)≤2 by norm_num)])
    exact mul_le_mul_of_nonneg_right (by linarith) (sub_pos.mpr h1).le
  have hFar := large_slant_floor_pointwise h1.le h2 hA hs hR0
  have hTail : (11/4-log (1+(R/s)^2/2)/2)*(q-1) < (-(1100:ℝ)/2)*(q-1) :=
    mul_lt_mul_of_pos_right (by linarith) (sub_pos.mpr h1)
  linarith [hNear 1 zero_lt_one,hNear D hD]

private theorem large_slant_base_upper {A D R s q : ℝ} (hq : 1 ≤ q)
    (hs : 0 < s) :
    StudentSkew.likelihood (largeSlantSample D R) 0 q A s ≤ 2^201*
      (TDist.density 0 q s 1 ^100 * TDist.density 0 q s D ^100 *
        TDist.density 0 q s R) := by
  rw [Student2Skew.skew_likelihood_spatial_product _ _ _ (zero_lt_one.trans_le hq)]
  have H (x : ℝ) : exp (Student2Skew.spatialLogDensity 0 q A s x) ≤ 2*TDist.density 0 q s x := by
    unfold Student2Skew.spatialLogDensity
    have hp : 0 < StudentSkew.studentCDF (q+1) (A*(x-0)*sqrt ((q+1)/(q*s^2+(x-0)^2))) :=
      Student2Skew.cdf_pos (by linarith) _
    rw [exp_add,exp_log (by unfold TDist.density; positivity),exp_log (mul_pos two_pos hp),
      mul_comm 2 (TDist.density 0 q s x)]
    have hab : (2:ℝ)*StudentSkew.studentCDF (q+1) (A*(x-0)*sqrt ((q+1)/(q*s^2+(x-0)^2))) ≤ 2 :=
      mul_le_of_le_one_right zero_le_two (Student2Skew.cdf_bounds _ _).2
    have hDen : 0 ≤ TDist.density 0 q s x := (exp_pos _).le
    exact mul_le_mul_of_nonneg_left hab hDen
  refine le_trans (Finset.prod_le_prod₀ (fun _ _ ↦ (exp_pos _).le)
      (fun i _ ↦ H (largeSlantSample D R i))) ?_
  have hP : (∏ i, TDist.density 0 q s (largeSlantSample D R i)) =
      TDist.density 0 q s 1 ^100 * TDist.density 0 q s D ^100 * TDist.density 0 q s R := by
    unfold largeSlantSample
    rw [Fin.prod_univ_add (a := 100) (b := 101)]
    simp only [Fin.append_left,Fin.append_right,Fin.prod_const]
    rw [Fin.prod_univ_add (a := 100) (b := 1)]
    simp only [Fin.append_left,Fin.append_right,Fin.prod_const,pow_one]
    ring
  rw [Finset.prod_mul_distrib,hP,Fin.prod_const]

private theorem large_slant_fitted_floor_dominates {D R s q : ℝ}
    (hD : 1024*(2:ℝ)^20 ≤ D) (hDU : D ≤ (2:ℝ)^150*2^20)
    (hR : R = (2:ℝ)^50000) (hs : 0 < s) (hq : 1 < q) :
    StudentSkew.likelihood (largeSlantSample D R) 0 q (2^20) s <
      max (StudentSkew.likelihood (largeSlantSample D R) 0 1 (2^20) s)
        (StudentSkew.likelihood (largeSlantSample D R) 0 1 (2^20) (2^20/8)) := by
  let A : ℝ := 2^20
  have hA : (1024:ℝ) ≤ A := by norm_num [A]
  have hA0 : 0 < A := by positivity
  have hD0 : 0 < D := (by positivity : 0 < 1024*(2:ℝ)^20).trans_le hD
  have hR0 : 0 < R := hR.symm ▸ (by positivity : 0 < (2:ℝ)^50000)
  have hRwide : D ≤ R := by
    apply hDU.trans
    rw [hR,← pow_add]
    exact pow_le_pow_right₀ (by norm_num) (by decide)
  have hC : (1/4:ℝ) ≤ exp (TDist.logNormalizer 1) := by
    unfold TDist.logNormalizer
    norm_num [Gamma_one,Gamma_one_half_eq,log_sqrt pi_pos.le]
    rw [show -(log π/2)-log π/2 = -log π by ring,exp_neg,exp_log pi_pos]
    simpa only [one_div] using (one_div_le_one_div_of_le pi_pos pi_lt_four.le)
  let K := D^200*R^2
  have hK : 0 < K := by unfold K; positivity
  have hFloor : (2:ℝ)^(-500:ℤ) <
      K*StudentSkew.likelihood (largeSlantSample D R) 0 1 A (A/8) := by
    rw [largeSlantSample_normalized hD0 hR0 (by positivity)]
    have E : K*(exp (TDist.logNormalizer 1)^201*2^101/(D^200*R^2)*
      normalizedSlantProfile A R D (A/8)) =
      exp (TDist.logNormalizer 1)^201*2^101*normalizedSlantProfile A R D (A/8) := by
      unfold K
      field_simp <;> ring
    rw [E]
    have hW := large_slant_left_witness hA hD hRwide
    have hw : 1 < normalizedSlantProfile A R D (A/8) := by
      refine lt_of_le_of_lt ?_ hW
      have hG : (1:ℝ) ≤ (19/10:ℝ)^100 := one_le_pow₀ (by norm_num)
      exact (by linarith : (1:ℝ) ≤ A/512).trans (le_mul_of_one_le_right (by positivity) hG)
    calc
      _ ≤ (1/4:ℝ)^201*2^101 := by
        change ((2:ℝ)^(500:ℕ))⁻¹ ≤ (1/4:ℝ)^201*2^101
        calc
          _ ≤ (1/4:ℝ)^201 := by
            rw [one_div,inv_pow,show (4:ℝ)=2^2 by norm_num,← pow_mul]
            exact inv_anti₀ (by positivity) (pow_le_pow_right₀ (by norm_num) (by decide))
          _ ≤ _ := le_mul_of_one_le_right (by positivity) (one_le_pow₀ (by norm_num))
      _ ≤ exp (TDist.logNormalizer 1)^201*2^101 :=
        mul_le_mul_of_nonneg_right (pow_le_pow_left₀ (by norm_num) hC _) (by positivity)
      _ < _ := lt_mul_of_one_lt_right (by positivity) hw
  change _ < max (StudentSkew.likelihood (largeSlantSample D R) 0 1 A s)
    (StudentSkew.likelihood (largeSlantSample D R) 0 1 A (A/8))
  by_cases hW : s ≤ (2:ℝ)^256
  · by_cases h2 : q ≤ 2
    · exact (large_slant_floor_compact_width hq h2 hA0 hs hD0 hW hR.ge).trans_le (le_max_left _ _)
    · suffices H : K*StudentSkew.likelihood (largeSlantSample D R) 0 q A s ≤ (2:ℝ)^(-500:ℤ) by
        exact ((mul_lt_mul_iff_right₀ hK).1
          (by simpa only [mul_comm K] using H.trans_lt hFloor)).trans_le (le_max_right _ _)
      let f := fun x : ℝ ↦ TDist.density 0 q s x
      have hf (x : ℝ) : 0 ≤ f x := (exp_pos _).le
      have hnear : f 1 ≤ 1 := by
        apply (Student2Normal.student_density_rational_upper hq.le hs).trans
        simp only [sub_zero,one_pow]
        exact (div_le_one (by positivity)).2 (by nlinarith [sq_nonneg (s-1)])
      have hmid : f D ≤ 2*s/D^2 := by
        apply (Student2Normal.student_density_rational_upper hq.le hs).trans
        simp only [sub_zero]
        exact div_le_div_of_nonneg_left (by positivity) (by positivity) (by linarith [sq_nonneg s])
      have hfar : f R ≤ 6*s^2/R^3 := by
        simpa only [sub_zero,abs_of_pos hR0] using
          (Student2Normal.student_density_cubic_remote_upper (m := 0) (y := R)
            (le_of_not_ge h2) hs (by simpa using hR0.ne'))
      apply le_trans (mul_le_mul_of_nonneg_left (large_slant_base_upper hq.le hs) hK.le)
      change K*(2^201*(f 1^100*f D^100*f R)) ≤ _
      have H := mul_le_mul (mul_le_mul (pow_le_pow_left₀ (hf 1) hnear 100)
        (pow_le_pow_left₀ (hf D) hmid 100) (pow_nonneg (hf D) _) (by norm_num))
        hfar (hf R) (by positivity : 0 ≤ 1^100*(2*s/D^2)^100)
      calc
        _ ≤ K*(2^201*(1^100*(2*s/D^2)^100*(6*s^2/R^3))) := by gcongr
        _ = (6*2^301)*s^102/R := by unfold K; field_simp <;> ring
        _ ≤ (6*2^301)*((2:ℝ)^256)^102/R := by gcongr
        _ ≤ _ := by
          rw [hR,zpow_neg,← pow_mul]
          change 6*(2:ℝ)^301*2^(256*102)/2^50000 ≤ (2^500)⁻¹
          rw [← one_div,div_le_div_iff₀ (by positivity) (by positivity),one_mul]
          calc
            _ ≤ (2:ℝ)^3*2^301*2^(256*102)*2^500 := by gcongr <;> norm_num
            _ ≤ (2:ℝ)^50000 := by
              simp only [← pow_add]
              exact pow_le_pow_right₀ (by norm_num) (by decide)
  · have hW' : (2:ℝ)^256 ≤ s := le_of_not_ge hW
    suffices H : K*StudentSkew.likelihood (largeSlantSample D R) 0 q A s ≤ (2:ℝ)^(-500:ℤ) by
        exact ((mul_lt_mul_iff_right₀ hK).1
          (by simpa only [mul_comm K] using H.trans_lt hFloor)).trans_le (le_max_right _ _)
    let f := fun x : ℝ ↦ TDist.density 0 q s x
    have hf (x : ℝ) : 0 ≤ f x := (exp_pos _).le
    have hPeak (x : ℝ) : f x ≤ 2/s := by
      apply (Student2Normal.student_density_rational_upper hq.le hs).trans
      simp only [sub_zero]
      apply (div_le_div_iff₀ (by positivity) hs).2
      nlinarith [sq_nonneg x]
    have hfar : f R ≤ 2*s/R^2 := by
      apply (Student2Normal.student_density_rational_upper hq.le hs).trans
      simp only [sub_zero]
      exact div_le_div_of_nonneg_left (by positivity) (by positivity) (by linarith [sq_nonneg s])
    apply le_trans (mul_le_mul_of_nonneg_left (large_slant_base_upper hq.le hs) hK.le)
    change K*(2^201*(f 1^100*f D^100*f R)) ≤ _
    have H := mul_le_mul (mul_le_mul (pow_le_pow_left₀ (hf 1) (hPeak 1) 100)
      (pow_le_pow_left₀ (hf D) (hPeak D) 100) (pow_nonneg (hf D) _) (by positivity))
      hfar (hf R) (by positivity : 0 ≤ (2/s)^100*(2/s)^100)
    calc
      _ ≤ K*(2^201*((2/s)^100*(2/s)^100*(2*s/R^2))) := by gcongr
      _ = 2^402*D^200/s^199 := by unfold K; field_simp <;> ring
      _ ≤ 2^402*((2:ℝ)^150*2^20)^200/((2:ℝ)^256)^199 := by gcongr
      _ ≤ _ := by
        rw [← pow_add,← pow_mul,← pow_mul,← pow_add,zpow_neg,← one_div,
          div_le_div_iff₀ (by positivity) (by positivity),one_mul]
        change (2:ℝ)^(402+(150+20)*200)*2^(500:ℕ) ≤ (2:ℝ)^(256*199)
        rw [← pow_add]
        exact pow_le_pow_right₀ (by norm_num) (by decide)

/-- With positive location residuals and the atlas slant, the same actual
two-scale crossing remains a global tie when shape is free on the fitted
branch. Every shape strictly above one loses to a shape-one point. -/
theorem skew_large_slant_scale_shape_crossing :
    let A : ℝ := 2^20
    let R : ℝ := 2^50000
    ∃ D ∈ Ioo (1024*A) ((2:ℝ)^150*A), ∃ u v : ℝ,
      0 < u ∧ u < 8*A ∧ 8*A < v ∧
      IsMaxOn (fun p : ℝ × ℝ ↦ StudentSkew.likelihood (largeSlantSample D R) 0 p.2 A p.1)
        (Ioi 0 ×ˢ Ici 1) (u,1) ∧
      IsMaxOn (fun p : ℝ × ℝ ↦ StudentSkew.likelihood (largeSlantSample D R) 0 p.2 A p.1)
        (Ioi 0 ×ˢ Ici 1) (v,1) := by
  dsimp only
  obtain ⟨D,hD,u,v,hu,hua,hav,hm,hn⟩ := skew_cauchy_large_slant_scale_crossing
  refine ⟨D,hD,u,v,hu,hua,hav,?_,?_⟩
  all_goals
    intro ⟨s,q⟩ hq
    change 0 < s ∧ 1 ≤ q at hq
    rcases hq.2.eq_or_lt with h | h
    · subst q
      first | exact @hm s hq.1 | exact @hn s hq.1
    · refine (large_slant_fitted_floor_dominates hD.1.le hD.2.le rfl hq.1 h).le.trans ?_
      apply max_le
      · first | exact @hm s hq.1 | exact @hn s hq.1
      · have hpos : (0:ℝ) < 2^20/8 := by positivity
        first | exact @hm (2^20/8) hpos | exact @hn (2^20/8) hpos


end UnivariateML.Student2SkewWitness
end
