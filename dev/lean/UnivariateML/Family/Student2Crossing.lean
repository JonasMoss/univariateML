module

public import Mathlib.Topology.Order.Compact
public import Mathlib.Topology.Order.IntermediateValue

public import Mathlib.Analysis.SpecialFunctions.Pow.Real
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Exact crossings of separated likelihood maxima

A change in the order of two continuous compact branch maxima gives a
real sample with at least two global co-winners. The strict separator
excludes the overlap of the branch domains.
-/

@[expose] public section

open Set Real

namespace UnivariateML.Student2Crossing

/-- Two continuous compact profiles whose ordering reverses must tie at
an interior parameter. A strict separator forces distinct co-winners. -/
theorem separated_real_crossing {X : Type*} [TopologicalSpace X]
    (F : ℝ → X → ℝ) {a b B : ℝ} (hab : a < b) {L R : Set X}
    (hL : IsCompact L) (hR : IsCompact R)
    (nL : L.Nonempty) (nR : R.Nonempty)
    (hC : Continuous (fun p : ℝ × X ↦ F p.1 p.2))
    (hsep : ∀ t ∈ Set.Icc a b, ∀ u ∈ L ∩ R, F t u < B)
    (hhigh : ∀ t ∈ Set.Icc a b, B ≤ sSup (F t '' L))
    (ha : sSup (F a '' R) < sSup (F a '' L))
    (hb : sSup (F b '' L) < sSup (F b '' R)) :
    ∃ t ∈ Set.Ioo a b, ∃ u ∈ L \ R, ∃ v ∈ R \ L,
      IsMaxOn (F t) (L ∪ R) u ∧ IsMaxOn (F t) (L ∪ R) v := by
  have hcL : Continuous (fun t ↦ sSup (F t '' L)) := hL.continuous_sSup hC
  have hcR : Continuous (fun t ↦ sSup (F t '' R)) := hR.continuous_sSup hC
  let G := fun t ↦ sSup (F t '' L) - sSup (F t '' R)
  have hG : Continuous G := hcL.sub hcR
  have ha' : 0 < G a := sub_pos.mpr ha
  have hb' : G b < 0 := sub_neg.mpr hb
  obtain ⟨t,ht,hz⟩ := intermediate_value_Icc' hab.le hG.continuousOn ⟨hb'.le,ha'.le⟩
  have ht' : t ∈ Ioo a b := ⟨lt_of_le_of_ne ht.1 (by rintro rfl; exact ha'.ne' hz),
    lt_of_le_of_ne ht.2 (by rintro rfl; exact hb'.ne hz)⟩
  have hEq : sSup (F t '' L) = sSup (F t '' R) := sub_eq_zero.mp hz
  have hc : Continuous (F t) := hC.comp (continuous_const.prodMk continuous_id)
  obtain ⟨u,hu,huMax⟩ := hL.exists_isMaxOn nL hc.continuousOn
  obtain ⟨v,hv,hvMax⟩ := hR.exists_isMaxOn nR hc.continuousOn
  have hu' : sSup (F t '' L) = F t u := (huMax.isLUB hu).csSup_eq (nL.image _)
  have hv' : sSup (F t '' R) = F t v := (hvMax.isLUB hv).csSup_eq (nR.image _)
  have heq : F t u = F t v := hu'.symm.trans (hEq.trans hv')
  have hbest : B ≤ F t u := hu' ▸ hhigh t ht
  have hnotU : u ∉ R := fun h ↦ (hsep t ht u ⟨hu,h⟩).not_ge hbest
  have hnotV : v ∉ L := fun h ↦ (hsep t ht v ⟨h,hv⟩).not_ge (heq ▸ hbest)
  refine ⟨t,ht',u,⟨hu,hnotU⟩,v,⟨hv,hnotV⟩,?_,?_⟩
  · intro x hx
    rcases hx with hx | hx
    · exact @huMax x hx
    · exact heq.symm ▸ @hvMax x hx
  · intro x hx
    rcases hx with hx | hx
    · exact heq ▸ @huMax x hx
    · exact @hvMax x hx


/-- Skew-Cauchy multiplier at one positive residual. -/
noncomputable def skewFactor (A w x : ℝ) : ℝ :=
  1 + A / sqrt (1 + A^2 + (w/x)^2)

/-- Factored shape-one likelihood of the three positive levels used in
the large-slant example, after cancellation of the common normalization. -/
noncomputable def normalizedSlantProfile (A R D w : ℝ) : ℝ :=
  w*(w^2/(w^2+1))^100*(D^2/(D^2+w^2))^100*(skewFactor A w 1)^100 *
    (skewFactor A w D/2)^100*(R^2/(R^2+w^2))*(skewFactor A w R/2)

private theorem skewFactor_bounds {A w x : ℝ} (hA : 0 < A) :
    1 < skewFactor A w x ∧ skewFactor A w x < 2 := by
  have hp : 0 < 1+A^2+(w/x)^2 := by positivity
  have hsq : (sqrt (1+A^2+(w/x)^2))^2 = 1+A^2+(w/x)^2 := sq_sqrt hp.le
  have hb : A < sqrt (1+A^2+(w/x)^2) := by
    by_contra! h
    have hh := pow_le_pow_left₀ (sqrt_nonneg _) h 2
    nlinarith [sq_nonneg (w/x)]
  unfold skewFactor
  have hx := div_pos hA (sqrt_pos.mpr hp)
  have hy := (div_lt_one (sqrt_pos.mpr hp)).2 hb
  constructor <;> linarith

private theorem high_slant_factor_bound {A w : ℝ} (hA : 0 < A) (hw : 8*A ≤ w) :
    skewFactor A w 1 ≤ 9/8 := by
  have hp : 0 < w := (by positivity : 0 < 8*A).trans_le hw
  have he : w ≤ sqrt (1+A^2+(w/1)^2) := by
    apply le_sqrt_of_sq_le
    simp only [div_one]
    nlinarith [sq_nonneg A]
  unfold skewFactor
  have hy : A / sqrt (1+A^2+(w/1)^2) ≤ 1/8 := by
    apply (div_le_iff₀ (hp.trans_le he)).2
    linarith
  linarith

private theorem normalized_slant_bound {A R D w : ℝ}
    (hA : 0 < A) (hR : 0 < R) (hD : 0 < D) (hw : 0 < w) :
    normalizedSlantProfile A R D w ≤
      w*(D^2/(D^2+w^2))^100*(skewFactor A w 1)^100 := by
  let u := w^2/(w^2+1)
  let v := D^2/(D^2+w^2)
  let r := R^2/(R^2+w^2)
  let a := skewFactor A w 1
  let b := skewFactor A w D/2
  let c := skewFactor A w R/2
  have hu0 : 0 ≤ u := by unfold u; positivity
  have hu1 : u ≤ 1 := by unfold u; apply (div_le_one (by positivity)).2; linarith
  have hv0 : 0 ≤ v := by unfold v; positivity
  have hr0 : 0 ≤ r := by unfold r; positivity
  have hr1 : r ≤ 1 := by unfold r; apply (div_le_one (by positivity)).2; linarith [sq_nonneg w]
  have ha0 : 0 ≤ a := by unfold a; linarith [(skewFactor_bounds (w := w) (x := 1) hA).1]
  have hb0 : 0 ≤ b := by unfold b; linarith [(skewFactor_bounds (w := w) (x := D) hA).1]
  have hb1 : b ≤ 1 := by unfold b; linarith [(skewFactor_bounds (w := w) (x := D) hA).2]
  have hc0 : 0 ≤ c := by unfold c; linarith [(skewFactor_bounds (w := w) (x := R) hA).1]
  have hc1 : c ≤ 1 := by unfold c; linarith [(skewFactor_bounds (w := w) (x := R) hA).2]
  have H : u^100*b^100*r*c ≤ 1 := by
    calc
      _ ≤ (1:ℝ)*1*1*1 := by
        apply mul_le_mul (mul_le_mul (mul_le_mul (pow_le_one₀ hu0 hu1) (pow_le_one₀ hb0 hb1)
          (by positivity) zero_le_one) hr1 hr0 (by norm_num)) hc1 hc0 (by norm_num)
      _ = 1 := by norm_num
  change w*u^100*v^100*a^100*b^100*r*c ≤ w*v^100*a^100
  rw [show w*u^100*v^100*a^100*b^100*r*c = (w*v^100*a^100)*(u^100*b^100*r*c) by ac_rfl]
  exact mul_le_of_le_one_right (by positivity) H

/-- The separator and the entire high-scale branch at the lower sample
end admit bounds uniform in the outlier. No large outlier is evaluated. -/
theorem large_slant_high_branch_bounds {A R D w : ℝ}
    (hA : 0 < A) (hR : 0 < R) (hD : 0 < D) (hw : 8*A ≤ w) :
    normalizedSlantProfile A R D (8*A) ≤ 8*A*(9/8)^100 ∧
      normalizedSlantProfile A R D w ≤ D*(9/8)^100 := by
  have hw0 : 0 < w := (by positivity : 0 < 8*A).trans_le hw
  have base {y : ℝ} (hy : 8*A ≤ y) :
      normalizedSlantProfile A R D y ≤ y*(D^2/(D^2+y^2))^100*(9/8)^100 := by
    have hy0 : 0 < y := (by positivity : 0 < 8*A).trans_le hy
    have hA' : 0 ≤ skewFactor A y 1 := by
      linarith [(skewFactor_bounds (w := y) (x := 1) hA).1]
    exact (normalized_slant_bound hA hR hD hy0).trans
      (mul_le_mul_of_nonneg_left (pow_le_pow_left₀ hA' (high_slant_factor_bound hA hy) 100)
        (by positivity))
  have b0 (y : ℝ) : 0 ≤ D^2/(D^2+y^2) := by positivity
  have b1 (y : ℝ) : D^2/(D^2+y^2) ≤ 1 := by
    exact (div_le_one (by positivity)).2 (by linarith [sq_nonneg y])
  constructor
  · refine (base le_rfl).trans (mul_le_mul_of_nonneg_right ?_ (by norm_num))
    exact mul_le_of_le_one_right (by positivity) (pow_le_one₀ (b0 _) (b1 _))
  · refine (base hw).trans (mul_le_mul_of_nonneg_right ?_ (by norm_num))
    have hp : (D^2/(D^2+w^2))^100 ≤ D^2/(D^2+w^2) := by
      rw [show (100 : ℕ) = 99+1 by decide,pow_succ]
      exact mul_le_of_le_one_left (b0 _) (pow_le_one₀ (b0 _) (b1 _))
    refine (mul_le_mul_of_nonneg_left hp hw0.le).trans ?_
    rw [← mul_div_assoc]
    apply (div_le_iff₀ (by positivity)).2
    nlinarith [mul_nonneg hD.le (sq_nonneg (w-D))]

private theorem inverse_one_add_power_lower {z : ℝ} (hz : 0 ≤ z) (hz' : z ≤ 1)
    (p : ℕ) :
    1-(p:ℝ)*z ≤ (1/(1+z))^p := by
  have hz1 : 1-z ≤ 1/(1+z) := by
    apply (le_div_iff₀ (by positivity)).2
    nlinarith [sq_nonneg z]
  have hBern := one_add_mul_le_pow (a := -z) (by linarith : -2 ≤ -z) p
  rw [mul_neg,← sub_eq_add_neg,← sub_eq_add_neg] at hBern
  exact hBern.trans (pow_le_pow_left₀ (sub_nonneg.mpr hz') hz1 p)

private theorem moderate_slant_factor_lower {A w x : ℝ}
    (hA : 0 < A) (hx : 0 < x) (hw0 : 0 ≤ w) (hwx : w ≤ x) :
    2-1/A^2 ≤ skewFactor A w x := by
  have hrat : w/x ≤ 1 := (div_le_one hx).2 hwx
  have hsq : (w/x)^2 ≤ 1 := by simpa using pow_le_pow_left₀ (div_nonneg hw0 hx.le) hrat 2
  have hS : sqrt (1+A^2+(w/x)^2) ≤ A+1/A := by
    apply sqrt_le_iff.mpr
    refine ⟨by positivity,?_⟩
    have he : (A+1/A)^2 = A^2+2+(1/A)^2 := by field_simp; ring
    rw [he]
    nlinarith [sq_nonneg (1/A)]
  unfold skewFactor
  have hB : A/(A+1/A) ≤ A/sqrt (1+A^2+(w/x)^2) :=
    div_le_div_of_nonneg_left hA.le (by positivity) hS
  have hC : 1-1/A^2 ≤ A/(A+1/A) := by
    apply (le_div_iff₀ (by positivity)).2
    have hh : (1-1/A^2)*(A+1/A) = A-1/A^3 := by field_simp; ring
    rw [hh]
    linarith [show 0 ≤ 1/A^3 by positivity]
  linarith

private theorem left_skewFactor_gt {A : ℝ} (hA : 1024 ≤ A) :
    19/10 < skewFactor A (A/8) 1 := by
  have h0 : 0 < A := by linarith
  have hS : sqrt (1+A^2+(A/8/1)^2) < (10/9)*A := by
    apply (sqrt_lt' (by positivity)).mpr
    simp only [div_one]
    nlinarith [show 1024^2 ≤ A^2 from pow_le_pow_left₀ (by norm_num) hA 2]
  have hp : 0 < sqrt (1+A^2+(A/8/1)^2) := by positivity
  unfold skewFactor
  have hB : 9/10 < A/sqrt (1+A^2+(A/8/1)^2) := (lt_div_iff₀ hp).2 (by linarith)
  linarith


private theorem ratio_rewrite {x w : ℝ} (hx : 0 < x) :
    x^2/(x^2+w^2) = 1/(1+(w/x)^2) := by field_simp

private theorem ordinary_factors_lower {A R D w ℓ : ℝ} (hA : 1024 ≤ A)
    (hw : 128 ≤ w) (hD : 16*w ≤ D) (hR : D ≤ R)
    (hℓ0 : 0 < ℓ) (hℓ1 : ℓ ≤ 3/4) (hcomp : ℓ < 1-100*(w/D)^2) :
    w*(skewFactor A w 1)^100*ℓ^5 < normalizedSlantProfile A R D w := by
  have hA0 : 0 < A := by linarith
  have hw0 : 0 < w := by linarith
  have hD0 : 0 < D := by linarith
  have hR0 : 0 < R := hD0.trans_le hR
  have hq (x : ℝ) (hx : D ≤ x) : 0 ≤ w/x ∧ w/x ≤ 1/16 := by
    have hx0 := hD0.trans_le hx
    exact ⟨by positivity,(div_le_iff₀ hx0).2 (by linarith)⟩
  have sqbound (x : ℝ) (hx : D ≤ x) : (w/x)^2 ≤ (1/16)^2 :=
    pow_le_pow_left₀ (hq x hx).1 (hq x hx).2 2
  have hwi : 0 ≤ 1/w^2 := by positivity
  have hw2 : (128 : ℝ)^2 ≤ w^2 := pow_le_pow_left₀ (by norm_num) hw 2
  have hi : 1/w^2 ≤ 1/(128:ℝ)^2 := one_div_le_one_div_of_le (by norm_num) hw2
  have hB1 : ℓ < (w^2/(w^2+1))^100 := by
    rw [show w^2/(w^2+1) = 1/(1+1/w^2) by field_simp]
    have H := inverse_one_add_power_lower hwi (by linarith : 1/w^2 ≤ 1) 100
    simp only [Nat.cast_ofNat] at H
    have H' : 1-100*(1/w^2) ≤ (1/(1+1/w^2))^100 := H
    exact (by linarith : ℓ < 1-100*(1/w^2)).trans_le H'
  have hB2 : ℓ < (D^2/(D^2+w^2))^100 := by
    rw [ratio_rewrite hD0]
    exact hcomp.trans_le (inverse_one_add_power_lower (sq_nonneg _) (by
      linarith [sqbound D le_rfl]) 100)
  have hBR : ℓ < R^2/(R^2+w^2) := by
    rw [ratio_rewrite hR0]
    apply (lt_div_iff₀ (by positivity)).2
    have hz := sqbound R hR
    nlinarith [mul_nonneg hℓ0.le (sub_nonneg.mpr hz)]
  have hA2 : (1024 : ℝ)^2 ≤ A^2 := pow_le_pow_left₀ (by norm_num) hA 2
  have hAI : 1/A^2 ≤ 1/(1024:ℝ)^2 := one_div_le_one_div_of_le (by norm_num) hA2
  have hs (x : ℝ) (hx : D ≤ x) : 1-1/(2*A^2) ≤ skewFactor A w x/2 := by
    have hx0 := hD0.trans_le hx
    have hwx : w ≤ x := by linarith
    have H := moderate_slant_factor_lower hA0 hx0 hw0.le hwx
    rw [show 1/(2*A^2) = (1/A^2)/2 by ring]
    linarith
  have hSD : ℓ < (skewFactor A w D/2)^100 := by
    have hn : (0 : ℝ) ≤ 1-1/(2*A^2) := by
      rw [show 1/(2*A^2) = (1/A^2)/2 by ring]
      linarith
    refine lt_of_lt_of_le ?_ (pow_le_pow_left₀ hn (hs D le_rfl) 100)
    have H := one_add_mul_le_pow (a := -(1/(2*A^2))) (by linarith : -2 ≤ -(1/(2*A^2))) 100
    have HH : 1-100*(1/(2*A^2)) ≤ (1-1/(2*A^2))^100 := by simpa [sub_eq_add_neg] using H
    refine lt_of_lt_of_le ?_ HH
    rw [show 1/(2*A^2) = (1/A^2)/2 by ring]
    linarith
  have hSR : ℓ < skewFactor A w R/2 := by
    have H := hs R hR
    rw [show 1/(2*A^2) = (1/A^2)/2 by ring] at H
    linarith
  unfold normalizedSlantProfile
  have h0 : 0 < w*(skewFactor A w 1)^100 := by
    have hb := (skewFactor_bounds (w := w) (x := 1) hA0).1
    positivity
  rw [show w * (w^2/(w^2+1))^100 * (D^2/(D^2+w^2))^100 * (skewFactor A w 1)^100 *
      (skewFactor A w D/2)^100 * (R^2/(R^2+w^2)) * (skewFactor A w R/2) =
    (w*(skewFactor A w 1)^100) *
      ((w^2/(w^2+1))^100*(D^2/(D^2+w^2))^100*(skewFactor A w D/2)^100*
        (R^2/(R^2+w^2))*(skewFactor A w R/2)) by ac_rfl]
  apply mul_lt_mul_of_pos_left _ h0
  rw [show ℓ^5 = ℓ*ℓ*ℓ*ℓ*ℓ by ring]
  gcongr

/-- The first finite witness uniformly clears the separating width in
the large-slant normalized profile, for all sufficiently separated
levels and any symbolic outlier to their right. -/
theorem large_slant_left_witness {A R D : ℝ} (hA : 1024 ≤ A)
    (hD : 1024*A ≤ D) (hR : D ≤ R) :
    A/512*(19/10)^100 < normalizedSlantProfile A R D (A/8) := by
  have hA0 : 0 < A := by linarith
  have hw : 128 ≤ A/8 := by linarith
  have hD' : 0 < D := by nlinarith
  have hr : (A/8)/D ≤ 1/32 := (div_le_iff₀ hD').2 (by linarith)
  have hr2 : ((A/8)/D)^2 ≤ (1/32)^2 := pow_le_pow_left₀ (by positivity) hr 2
  have hb : (3:ℝ)/4 < 1-100*((A/8)/D)^2 := by linarith
  refine lt_trans ?_ (ordinary_factors_lower hA hw (by linarith) hR
    (show 0 < (3:ℝ)/4 by norm_num) le_rfl hb)
  have hpos : 0 < A/8*(3/4:ℝ)^5 := by positivity
  calc
    _ < A/8*(3/4)^5*(19/10)^100 := by
      apply mul_lt_mul_of_pos_right _ (by norm_num)
      nlinarith
    _ < _ := by
      rw [mul_assoc, mul_comm ((3/4:ℝ)^5), ← mul_assoc]
      have h := pow_lt_pow_left₀ (left_skewFactor_gt hA) (by norm_num) (by decide : 100 ≠ 0)
      gcongr

/-- At the far end of the sample band, the right-scale witness is
bounded below independently of the outlier. -/
theorem large_slant_right_witness {A R D : ℝ} (hA : 1024 ≤ A)
    (hD : 2048 ≤ D) (hR : D ≤ R) :
    D/512 < normalizedSlantProfile A R D (D/16) := by
  have hD0 : 0 < D := by linarith
  have hb : (1:ℝ)/2 < 1-100*((D/16)/D)^2 := by
    rw [show (D/16)/D = 1/16 by field_simp]
    norm_num
  refine lt_trans ?_ (ordinary_factors_lower hA (by linarith) (by linarith) hR
    (show 0 < (1:ℝ)/2 by norm_num) (by norm_num) hb)
  have hA0 : 0 < A := by linarith
  have h := (skewFactor_bounds (w := D/16) (x := 1) hA0).1
  rw [show D/512 = (D/16)*1*(1/2:ℝ)^5 by ring]
  gcongr
  exact one_lt_pow₀ h (by decide : 100 ≠ 0)


private noncomputable def compactRatio (x z : ℝ) : ℝ :=
  (x*(1-z))^2/((x*(1-z))^2+z^2)

private noncomputable def compactSlant (A x z : ℝ) : ℝ :=
  1+A*(x*(1-z))/sqrt ((1+A^2)*(x*(1-z))^2+z^2)

private noncomputable def compactSlantProfile (A R D z : ℝ) : ℝ :=
  (D^2*z*(1-z)/((D*(1-z))^2+z^2)) *
    (z^2/(z^2+(1-z)^2))^100 *
    (compactRatio D z)^99 *
    (compactSlant A 1 z)^100*(compactSlant A D z/2)^100 *
    compactRatio R z*(compactSlant A R z/2)

private theorem compact_denominator_pos {x c z : ℝ} (hx : 0 < x) (hc : 0 < c) :
    0 < c*(x*(1-z))^2+z^2 := by
  by_cases hz : z = 0
  · subst z; positivity
  · exact add_pos_of_nonneg_of_pos (by positivity) (by positivity)

private theorem compactSlantProfile_continuous {A R l : ℝ} (hR : 0 < R) (hl : 0 < l) :
    Continuous (fun p : ℝ × ℝ ↦ compactSlantProfile A R (max l p.1) p.2) := by
  let d := fun p : ℝ × ℝ ↦ max l p.1
  have hd : Continuous d := continuous_const.max continuous_fst
  have hp (p : ℝ × ℝ) : 0 < d p := hl.trans_le (le_max_left _ _)
  have h1 : Continuous (fun p : ℝ × ℝ ↦
      (d p)^2*p.2*(1-p.2)/((d p*(1-p.2))^2+p.2^2)) :=
    (by fun_prop : Continuous fun p : ℝ × ℝ ↦ (d p)^2*p.2*(1-p.2)).div
      (by fun_prop) (fun p ↦ by
        simpa only [one_mul] using (compact_denominator_pos (hp p) zero_lt_one (z := p.2)).ne')
  have h2 : Continuous (fun p : ℝ × ℝ ↦ p.2^2/(p.2^2+(1-p.2)^2)) :=
    (by fun_prop : Continuous fun p : ℝ × ℝ ↦ p.2^2).div (by fun_prop) (fun p ↦ by
      simpa only [one_mul,add_comm] using
        (compact_denominator_pos zero_lt_one zero_lt_one (z := p.2)).ne')
  have hRatio (x : ℝ × ℝ → ℝ) (hx : Continuous x) (hpos : ∀ p, 0 < x p) :
      Continuous (fun p : ℝ × ℝ ↦ compactRatio (x p) p.2) := by
    unfold compactRatio
    exact (by fun_prop : Continuous fun p : ℝ × ℝ ↦ (x p*(1-p.2))^2).div
      (by fun_prop) (fun p ↦ by
      simpa only [one_mul] using (compact_denominator_pos (hpos p) zero_lt_one (z := p.2)).ne')
  have hSlant (x : ℝ × ℝ → ℝ) (hx : Continuous x) (hpos : ∀ p, 0 < x p) :
      Continuous (fun p : ℝ × ℝ ↦ compactSlant A (x p) p.2) := by
    unfold compactSlant
    apply continuous_const.add ((by fun_prop : Continuous fun p : ℝ × ℝ ↦ A*(x p*(1-p.2))).div
      (by fun_prop) (fun p ↦ (sqrt_pos.mpr
        (compact_denominator_pos (hpos p) (by positivity) (z := p.2))).ne'))
  exact (((((h1.mul (h2.pow 100)).mul ((hRatio d hd hp).pow 99)).mul
    ((hSlant (fun _ ↦ 1) continuous_const (fun _ ↦ zero_lt_one)).pow 100)).mul
    (((hSlant d hd hp).div_const 2).pow 100)).mul
    (hRatio (fun _ ↦ R) continuous_const (fun _ ↦ hR))).mul
    ((hSlant (fun _ ↦ R) continuous_const (fun _ ↦ hR)).div_const 2)

private theorem compactSlant_eq {A R D z : ℝ} (hR : 0 < R) (hD : 0 < D)
    (hz : z ∈ Ioo 0 1) :
    compactSlantProfile A R D z = normalizedSlantProfile A R D (z/(1-z)) := by
  let w := z/(1-z)
  have ht : 0 < 1-z := sub_pos.mpr hz.2
  have hscale (x : ℝ) (hx : 0 < x) :
      compactRatio x z = x^2/(x^2+w^2) := by
    unfold compactRatio w
    field_simp
  have hs (x : ℝ) (hx : 0 < x) :
      compactSlant A x z = skewFactor A w x := by
    have hk : 0 < x*(1-z) := mul_pos hx ht
    have he : (1+A^2)*(x*(1-z))^2+z^2 =
      (1+A^2+(w/x)^2)*(x*(1-z))^2 := by
        unfold w
        field_simp
    unfold compactSlant skewFactor
    rw [he,sqrt_mul (by positivity),sqrt_sq hk.le]
    field_simp
  have h1 : z^2/(z^2+(1-z)^2) = w^2/(w^2+1) := by unfold w; field_simp
  have h2 : D^2*z*(1-z)/((D*(1-z))^2+z^2) = w*(D^2/(D^2+w^2)) := by
    unfold w
    field_simp
  unfold compactSlantProfile normalizedSlantProfile
  rw [h1,h2,hscale D hD,hscale R hR,hs 1 zero_lt_one,hs D hD,hs R hR,
    show ((D^2/(D^2+w^2))^100) = ((D^2/(D^2+w^2))^99)*(D^2/(D^2+w^2)) from pow_succ ..]
  ac_rfl

private theorem compactSlant_zero (A R D : ℝ) : compactSlantProfile A R D 0 = 0 := by
  simp [compactSlantProfile]

private theorem compactSlant_one (A R D : ℝ) : compactSlantProfile A R D 1 = 0 := by
  simp [compactSlantProfile]

private noncomputable def compactWidth (w : ℝ) : ℝ := w/(1+w)

private theorem compactWidth_mem {w : ℝ} (hw : 0 < w) : compactWidth w ∈ Ioo 0 1 := by
  unfold compactWidth
  exact ⟨by positivity,(div_lt_one (by positivity)).2 (by linarith)⟩

private theorem compactWidth_inv {w : ℝ} (hw : 0 < w) :
    compactWidth w / (1-compactWidth w) = w := by
  unfold compactWidth
  have he : 1-w/(1+w) = 1/(1+w) := by field_simp; ring
  rw [he]
  field_simp

private theorem compactWidth_strictMono : StrictMonoOn compactWidth (Ioi 0) := by
  intro a ha b hb hab
  unfold compactWidth
  exact (div_lt_div_iff₀ (show 0 < 1+a by linarith [mem_Ioi.mp ha])
    (show 0 < 1+b by linarith [mem_Ioi.mp hb])).2 (by nlinarith [hab])

private theorem width_compactWidth {z : ℝ} (hz : z ∈ Ioo 0 1) :
    compactWidth (z/(1-z)) = z := by
  have hp : 0 < 1-z := sub_pos.mpr hz.2
  unfold compactWidth
  have h : 1+z/(1-z) = 1/(1-z) := by field_simp; ring
  rw [h]
  field_simp

private theorem normalized_small_width_upper {A R D w : ℝ}
    (hA : 0 < A) (hR : 0 < R) (hD : 0 < D) (hw : 0 < w) (hsep : w ≤ 8*A) :
    normalizedSlantProfile A R D w ≤ 8*A*2^100 := by
  have ha := skewFactor_bounds (w := w) (x := 1) hA
  have hD' : D^2/(D^2+w^2) ≤ 1 := by
    exact (div_le_one (by positivity)).2 (by linarith [sq_nonneg w])
  calc
    _ ≤ w*(D^2/(D^2+w^2))^100*(skewFactor A w 1)^100 := normalized_slant_bound hA hR hD hw
    _ ≤ _ := by
      have hh := pow_le_one₀ (by positivity : 0 ≤ D^2/(D^2+w^2)) hD' (n := 100)
      have haf := pow_le_pow_left₀ (by linarith [ha.1]) ha.2.le 100
      calc
        _ ≤ (8*A*1)*2^100 := by gcongr
        _ = _ := by ring


set_option maxHeartbeats 1000000 in
-- Elaboration expands the compact branch definitions at their boundaries.
/-- On the specified sample band the normalized skew-Cauchy scale
profiles have two global co-winners at an interior sample parameter.
The two widths straddle the separating width. The outlier enters only
through an ordering inequality, so arbitrarily large outliers are covered. -/
theorem large_slant_normalized_crossing {A R : ℝ} (hA : 1024 ≤ A)
    (hR : (2:ℝ)^150*A ≤ R) :
    ∃ D ∈ Ioo (1024*A) ((2:ℝ)^150*A), ∃ u v : ℝ,
      0 < u ∧ u < 8*A ∧ 8*A < v ∧
      IsMaxOn (normalizedSlantProfile A R D) (Ioi 0) u ∧
      IsMaxOn (normalizedSlantProfile A R D) (Ioi 0) v := by
  let l := 1024*A
  let r := (2:ℝ)^150*A
  let c := compactWidth (8*A)
  let B := A/512*(19/10)^100
  let L := Icc 0 c
  let P := Icc c 1
  let F := fun t z ↦ compactSlantProfile A R (max l t) z
  have hA0 : 0 < A := by linarith
  have hl : 0 < l := by unfold l; positivity
  have hr : 0 < r := by unfold r; positivity
  have hrR : r ≤ R := hR
  have hR0 := hr.trans_le hrR
  have hlr : l < r := by
    have h : (1024:ℝ) < (2:ℝ)^150 := by
      calc
        (1024:ℝ) = (2:ℝ)^10 := by norm_num
        _ < (2:ℝ)^150 := pow_lt_pow_right₀ (by norm_num) (by decide)
    exact mul_lt_mul_of_pos_right h hA0
  have h8 : 0 < 8*A := by positivity
  have hc := compactWidth_mem h8
  have hB : 0 < B := by unfold B; positivity
  have hB1 : l*(9/8)^100 < B := by
    have h : (1024:ℝ)*(9/8)^100 < (19/10)^100/512 := by norm_num
    have H := mul_lt_mul_of_pos_right h hA0
    dsimp [l,B]
    linarith
  have hB2 : 8*A*(9/8)^100 < B := by
    apply lt_of_le_of_lt _ hB1
    apply mul_le_mul_of_nonneg_right (by dsimp [l]; linarith) (by norm_num)
  have hcomp (t : ℝ) (ht : t ∈ Icc l r) (z : ℝ) (hz : z ∈ Ioo 0 1) :
      F t z = normalizedSlantProfile A R t (z/(1-z)) := by
    unfold F
    rw [max_eq_right ht.1,compactSlant_eq hR0 (hl.trans_le ht.1) hz]
  have hl0 (t : ℝ) : F t 0 = 0 := by simp [F,compactSlant_zero]
  have hl1 (t : ℝ) : F t 1 = 0 := by simp [F,compactSlant_one]
  have hW (t w : ℝ) (ht : t ∈ Icc l r) (hw : 0 < w) :
      F t (compactWidth w) = normalizedSlantProfile A R t w := by
    rw [hcomp t ht _ (compactWidth_mem hw),compactWidth_inv hw]
  have hF : Continuous (fun p : ℝ × ℝ ↦ F p.1 p.2) := compactSlantProfile_continuous hR0 hl
  have hcb (t : ℝ) : Continuous (F t) :=
    hF.comp₂ (continuous_const (y := t)) continuous_id
  have hL : IsCompact L := isCompact_Icc
  have hP : IsCompact P := isCompact_Icc
  have nL : L.Nonempty := ⟨0,le_rfl,hc.1.le⟩
  have nP : P.Nonempty := ⟨1,hc.2.le,le_rfl⟩
  have h0 : 0 < A/8 := by positivity
  have hLeftMem : compactWidth (A/8) ∈ L :=
    ⟨(compactWidth_mem h0).1.le,(compactWidth_strictMono h0 h8 (by linarith)).le⟩
  have hLow (t : ℝ) (ht : t ∈ Icc l r) : B < F t (compactWidth (A/8)) := by
    rw [hW t _ ht h0]
    exact large_slant_left_witness hA ht.1 (ht.2.trans hrR)
  have hHigh (t : ℝ) (ht : t ∈ Icc l r) : B ≤ sSup (F t '' L) := by
    have hbb : BddAbove (F t '' L) := (hL.image (hcb t)).bddAbove
    exact (hLow t ht).le.trans (le_csSup hbb ⟨_,hLeftMem,rfl⟩)
  have hSep (t : ℝ) (ht : t ∈ Icc l r) (z : ℝ) (hz : z ∈ L ∩ P) : F t z < B := by
    have h : z = c := le_antisymm hz.1.2 hz.2.1
    rw [h]
    change F t (compactWidth (8*A)) < B
    rw [hW t _ ht h8]
    exact (large_slant_high_branch_bounds hA0 hR0 (hl.trans_le ht.1) le_rfl).1.trans_lt hB2
  have hturnL : sSup (F l '' P) < sSup (F l '' L) := by
    refine lt_of_le_of_lt (csSup_le (nP.image _) ?_) (hB1.trans_le (hHigh l ⟨le_rfl,hlr.le⟩))
    rintro _ ⟨z,hz,rfl⟩
    rcases hz.2.eq_or_lt with h | h
    · rw [h,hl1]
      positivity
    · have hz' : z ∈ Ioo 0 1 := ⟨hc.1.trans_le hz.1,h⟩
      have hw : 0 < z/(1-z) := div_pos hz'.1 (sub_pos.mpr hz'.2)
      have hs : 8*A ≤ z/(1-z) := by
        by_contra! hn
        have H := compactWidth_strictMono hw h8 hn
        rw [width_compactWidth hz'] at H
        exact (not_lt_of_ge hz.1) H
      rw [hcomp l ⟨le_rfl,hlr.le⟩ z hz']
      exact (large_slant_high_branch_bounds hA0 hR0 hl hs).2
  have hEq : 8*A*2^100 < r/512 := by
    have hh : (2:ℝ)^112 < (2:ℝ)^150 := pow_lt_pow_right₀ (by norm_num) (by decide)
    have H := mul_lt_mul_of_pos_right hh hA0
    change _ < (2:ℝ)^150*A/512
    rw [show (112:ℕ) = 9+3+100 by decide,pow_add,pow_add] at H
    norm_num at H ⊢
    linarith
  have hWr : 8*A < r/16 := by
    have hh : (2:ℝ)^7 < (2:ℝ)^150 := pow_lt_pow_right₀ (by norm_num) (by decide)
    have H := mul_lt_mul_of_pos_right hh hA0
    change _ < (2:ℝ)^150*A/16
    norm_num at H
    linarith
  have hWr0 : 0 < r/16 := by positivity
  have hturnR : sSup (F r '' L) < sSup (F r '' P) := by
    have hu : sSup (F r '' L) ≤ 8*A*2^100 := by
      refine csSup_le (nL.image _) ?_
      rintro _ ⟨z,hz,rfl⟩
      rcases hz.1.eq_or_lt with h | h
      · rw [← h,hl0]
        positivity
      · have hz' : z ∈ Ioo 0 1 := ⟨h,hz.2.trans_lt hc.2⟩
        have hw : 0 < z/(1-z) := div_pos hz'.1 (sub_pos.mpr hz'.2)
        have hs : z/(1-z) ≤ 8*A := by
          by_contra! hn
          have H := compactWidth_strictMono h8 hw hn
          rw [width_compactWidth hz'] at H
          exact (not_lt_of_ge hz.2) H
        rw [hcomp r ⟨hlr.le,le_rfl⟩ z hz']
        exact normalized_small_width_upper hA0 hR0 hr hw hs
    have hpM : compactWidth (r/16) ∈ P :=
      ⟨(compactWidth_strictMono h8 hWr0 hWr).le,(compactWidth_mem hWr0).2.le⟩
    have hBB : BddAbove (F r '' P) := (hP.image (hcb r)).bddAbove
    have hh := le_csSup hBB ⟨_,hpM,rfl⟩
    rw [hW r _ ⟨hlr.le,le_rfl⟩ hWr0] at hh
    have hR2 : 2048 ≤ r := by linarith [hlr,show 1024*A ≤ l from le_rfl]
    exact hu.trans_lt (hEq.trans (large_slant_right_witness hA hR2 hrR) |>.trans_le hh)
  obtain ⟨t,ht,z,hz,y,hy,hzBest,hyBest⟩ :=
    separated_real_crossing F hlr hL hP nL nP hF hSep hHigh hturnL hturnR
  have ht' : t ∈ Icc l r := Ioo_subset_Icc_self ht
  have hdom (x : ℝ) (hx : x ∈ Icc (0:ℝ) 1) : x ∈ L ∪ P := by
    rcases le_total x c with h | h
    · exact Or.inl ⟨hx.1,h⟩
    · exact Or.inr ⟨h,hx.2⟩
  have hzC : z < c := lt_of_le_of_ne hz.1.2 (fun e ↦ hz.2 ⟨e.ge,hc.2.le.trans' hz.1.2⟩)
  have hyC : c < y := lt_of_le_of_ne hy.1.1 (fun e ↦ hy.2 ⟨hc.1.le.trans hy.1.1,e.ge⟩)
  have hz0 : 0 < z := by
    by_contra! hn
    have he := le_antisymm hn hz.1.1
    have hbest := @hzBest (compactWidth (A/8)) (Or.inl hLeftMem)
    rw [he,hl0] at hbest
    exact (hLow t ht').not_ge (hbest.trans hB.le)
  have hy1 : y < 1 := by
    by_contra! hn
    have he := le_antisymm hy.1.2 hn
    have hbest := @hyBest (compactWidth (A/8)) (Or.inl hLeftMem)
    rw [he,hl1] at hbest
    exact (hLow t ht').not_ge (hbest.trans hB.le)
  have hz' : z ∈ Ioo 0 1 := ⟨hz0,hzC.trans hc.2⟩
  have hy' : y ∈ Ioo 0 1 := ⟨hc.1.trans hyC,hy1⟩
  let u := z/(1-z)
  let v := y/(1-y)
  have hu0 : 0 < u := div_pos hz'.1 (sub_pos.mpr hz'.2)
  have hv0 : 0 < v := div_pos hy'.1 (sub_pos.mpr hy'.2)
  have hu : u < 8*A := by
    by_contra! hn
    have h := compactWidth_strictMono.monotoneOn h8 hu0 hn
    rw [show compactWidth u = z from width_compactWidth hz'] at h
    exact hzC.not_ge h
  have hv : 8*A < v := by
    by_contra! hn
    have h := compactWidth_strictMono.monotoneOn hv0 h8 hn
    rw [show compactWidth v = y from width_compactWidth hy'] at h
    exact hyC.not_ge h
  refine ⟨t,ht,u,v,hu0,hu,hv,?_,?_⟩
  · intro w hw
    have hw0 : 0 < w := hw
    have hh := @hzBest (compactWidth w) (hdom _ (Ioo_subset_Icc_self (compactWidth_mem hw0)))
    change F t (compactWidth w) ≤ F t z at hh
    rw [hW t w ht' hw0,hcomp t ht' z hz'] at hh
    exact hh
  · intro w hw
    have hw0 : 0 < w := hw
    have hh := @hyBest (compactWidth w) (hdom _ (Ioo_subset_Icc_self (compactWidth_mem hw0)))
    change F t (compactWidth w) ≤ F t y at hh
    rw [hW t w ht' hw0,hcomp t ht' y hy'] at hh
    exact hh

end UnivariateML.Student2Crossing
end
