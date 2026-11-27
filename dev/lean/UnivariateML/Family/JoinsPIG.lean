module

public import UnivariateML.Family.HeldSweepPIG
public import Mathlib.Analysis.SpecialFunctions.Sqrt
import Mathlib.Analysis.Polynomial.Order
import Mathlib.Tactic

set_option linter.style.header false

/-!
The held-shape objective is written in the Bessel argument, with the
constant polynomial contributions for counts zero and one omitted.
The older `chartLogLik` definition does not expose its body. The
statements use the explicit objective from the held-shape derivation.
-/

@[expose] public section

open Set Real Filter Polynomial
open scoped Topology Polynomial
open UnivariateML.Discrete3PIG

namespace UnivariateML.JoinsPIG

noncomputable section

private theorem eval_ne_zero {k : ℕ} (hk : 2 ≤ k) {u : ℝ} (hu : 0 < u) :
    (besselPolynomial k).eval u ≠ 0 := by
  intro h
  have hv := bessel_variance_bound hk hu
  simp [degreeVariance, degreeMean, h] at hv

private theorem degree_mean_hasDerivAt {k : ℕ} (hk : 2 ≤ k) {u : ℝ} (hu : 0 < u) :
    HasDerivAt (degreeMean k) (degreeVariance k u / u) u := by
  let Q := besselPolynomial k
  have hQ := eval_ne_zero hk hu
  have hd := (hasDerivAt_id u).mul ((Q.derivative.hasDerivAt u).div (Q.hasDerivAt u) hQ)
  convert hd using 1
  · rfl
  · dsimp [degreeVariance, degreeMean, Q]
    field_simp [hu.ne', hQ]

private theorem degree_mean_pos {k : ℕ} (hk : 2 ≤ k) {u : ℝ} (hu : 0 < u) :
    0 < degreeMean k u := by
  let Q := besselPolynomial k
  let E := degreeMean k
  have hQ : Q ≠ 0 := by
    intro h
    exact eval_ne_zero hk hu (by simp [Q, h])
  have hd : Q.natDegree ≠ 0 := by
    intro h
    have hder : Q.derivative = 0 := (Polynomial.derivative_eq_zero).mpr h
    have hv := bessel_variance_bound hk hu
    change degreeVariance k u < _ at hv
    simp [degreeVariance, degreeMean, ← show Q = besselPolynomial k from rfl, hder] at hv
  let P := Q.derivative * Q
  have hl : 0 < P.leadingCoeff := by
    dsimp only [P]
    rw [leadingCoeff_mul, leadingCoeff_derivative]
    have hn : 0 < (Q.natDegree : ℝ) := Nat.cast_pos.mpr (Nat.pos_of_ne_zero hd)
    nlinarith only [mul_pos hn (sq_pos_of_ne_zero (leadingCoeff_ne_zero.mpr hQ))]
  have hf : ∀ᶠ v : ℝ in atTop, 0 < P.eval v := by
    by_cases he : P.natDegree = 0
    · have hp := eq_C_of_natDegree_eq_zero he
      filter_upwards [] with v
      have hlc : P.leadingCoeff = P.coeff 0 := by simp only [leadingCoeff, he]
      rw [hlc] at hl
      exact hl.trans_le (le_of_eq ((congrArg (fun R : ℝ[X] ↦ R.eval v) hp).trans eval_C).symm)
    · exact (P.tendsto_atTop_of_leadingCoeff_nonneg
        (by simpa [natDegree_eq_zero_iff_degree_le_zero] using he) hl.le).eventually_gt_atTop 0
  obtain ⟨v, huv, hv⟩ := ((eventually_gt_atTop u).and hf).exists
  have hv0 : 0 < v := hu.trans huv
  have hEv : 0 < E v := by
    unfold E degreeMean
    apply mul_pos hv0
    apply div_pos_iff.mpr
    apply mul_pos_iff.mp
    simpa [P, Q] using hv
  by_contra! hEu
  have hcont : ContinuousOn E (Icc u v) := fun w hw ↦
    (degree_mean_hasDerivAt hk (hu.trans_le hw.1)).continuousAt.continuousWithinAt
  have hne : (Icc u v ∩ E ⁻¹' {0}).Nonempty :=
    intermediate_value_Icc huv.le hcont ⟨hEu, hEv.le⟩
  have hc : IsCompact (Icc u v ∩ E ⁻¹' {0}) := isCompact_Icc.of_isClosed_subset
    ((IsClosed.inter_preimage_val_iff isClosed_Icc).mp
      (isClosed_singleton.preimage hcont.domRestrict)) inter_subset_left
  obtain ⟨w, hw, hmax⟩ := hc.exists_isMaxOn hne continuous_id.continuousOn
  have hw0 : 0 < w := hu.trans_le hw.1.1
  have hew : E w = 0 := hw.2
  have hwv : w < v := lt_of_le_of_ne hw.1.2 (fun h ↦ hEv.ne' (h ▸ hew))
  have hvw := bessel_variance_bound hk hw0
  change degreeMean k w = 0 at hew
  rw [hew, zero_mul, zero_div] at hvw
  have hs := (degree_mean_hasDerivAt hk hw0).tendsto_slope.mono_left (nhdsGT_le_nhdsNE w)
  have hneg := hs.eventually (Iio_mem_nhds (div_neg_of_neg_of_pos hvw hw0))
  have hnear : ∀ᶠ a in 𝓝[>] w, a ∈ Ioo w v := Ioo_mem_nhdsGT hwv
  obtain ⟨a, ha, hsa⟩ := (hnear.and hneg).exists
  have hEa : E a ≤ 0 := by
    rw [slope_def_field, hew, sub_zero] at hsa
    exact ((div_neg_iff.mp hsa).resolve_left (fun hh ↦ (sub_pos.mpr ha.1).not_gt hh.2)).1.le
  obtain ⟨b, hb, hb0⟩ := intermediate_value_Icc ha.2.le
    (hcont.mono (Icc_subset_Icc_left (hw.1.1.trans ha.1.le))) ⟨hEa, hEv.le⟩
  exact (ha.1.trans_le hb.1).not_ge (hmax ⟨⟨hw.1.1.trans (ha.1.le.trans hb.1), hb.2⟩, hb0⟩)

/-- The log-polynomial contribution for a count of at least two has
argument derivative `-E(1/z)/z`. -/
theorem log_polynomial_derivative {k : ℕ} (hk : 2 ≤ k) {z : ℝ} (hz : 0 < z) :
    HasDerivAt (fun v ↦ log ((besselPolynomial k).eval v⁻¹)) (-degreeMean k z⁻¹ / z) z := by
  let Q := besselPolynomial k
  have hQ := eval_ne_zero hk (inv_pos.mpr hz)
  have hd := ((Q.hasDerivAt z⁻¹).log hQ).comp z ((hasDerivAt_id z).inv hz.ne')
  convert hd using 1
  · rfl
  · dsimp [degreeMean, Q]
    ring

/-- The derivative of the argument-weighted degree contribution is
the degree mean minus its variance. -/
theorem weighted_degree_derivative {k : ℕ} (hk : 2 ≤ k) {z : ℝ} (hz : 0 < z) :
    HasDerivAt (fun v ↦ v * degreeMean k v⁻¹)
      (degreeMean k z⁻¹ - degreeVariance k z⁻¹) z := by
  have hd := (hasDerivAt_id z).mul
    ((degree_mean_hasDerivAt hk (inv_pos.mpr hz)).comp z ((hasDerivAt_id z).inv hz.ne'))
  convert hd using 1
  · rfl
  · dsimp only [id_eq, Function.comp_apply]
    field_simp [hz.ne']
    ring

/-- For arbitrary mixed counts, the derivative of the held-shape
log likelihood in the Bessel argument is the difference of the two
sides of the stationary equation divided by the squared argument. -/
theorem held_shape_derivative {n : ℕ} (x : Fin n → ℕ) {lam z : ℝ}
    (hl : 0 < lam) (hz : sqrt (2 * lam) < z) :
    let S := ∑ i, (x i : ℝ)
    let t := sqrt (z^2 - 2 * lam)
    let q := t / z
    let D := S + ∑ i, if 2 ≤ x i then degreeMean (x i) z⁻¹ else 0
    HasDerivAt
      (fun v ↦ -(n : ℝ) * (v - sqrt (v^2 - 2 * lam)) - S * log v +
        ∑ i, if 2 ≤ x i then log ((besselPolynomial (x i)).eval v⁻¹) else 0)
      ((2 * n * lam / (q * (1 + q)) - z * D) / z^2) z := by
  intro S t q D
  have hz0 : 0 < z := (sqrt_nonneg _).trans_lt hz
  have hrad : 0 < z^2 - 2 * lam := by
    have h := pow_lt_pow_left₀ hz (sqrt_nonneg _) (show 2 ≠ 0 by decide)
    rw [sq_sqrt (by positivity)] at h
    linarith
  have ht : 0 < t := sqrt_pos.mpr hrad
  have ht2 : t^2 = z^2 - 2 * lam := sq_sqrt hrad.le
  have hinner : HasDerivAt (fun v : ℝ ↦ v^2 - 2 * lam) (2 * z) z := by
    simpa using (((hasDerivAt_id z).pow 2).sub_const (2 * lam))
  have hroot : HasDerivAt (fun v : ℝ ↦ sqrt (v^2 - 2 * lam)) (z / t) z := by
    convert hinner.sqrt hrad.ne' using 1
    dsimp [t]
    ring
  have hcells (i : Fin n) :
      HasDerivAt
        (fun v : ℝ ↦ if 2 ≤ x i then log ((besselPolynomial (x i)).eval v⁻¹) else 0)
        (-(if 2 ≤ x i then degreeMean (x i) z⁻¹ else 0) / z) z := by
    by_cases h : 2 ≤ x i
    · simpa only [ite_eq_left h] using log_polynomial_derivative h hz0
    · simpa only [ite_eq_right h, neg_zero, zero_div] using hasDerivAt_const z (0 : ℝ)
  have hd := ((((hasDerivAt_id z).sub hroot).const_mul (-(n : ℝ))).sub
    (((hasDerivAt_id z).log hz0.ne').const_mul S)).add
    (HasDerivAt.fun_sum fun i (_ : i ∈ Finset.univ) ↦ hcells i)
  convert hd using 1
  · rfl
  · dsimp only [id_eq, Function.comp_apply]
    simp only [neg_div, Finset.sum_neg_distrib, ← Finset.sum_div]
    have he : (z - t) * (z + t) = 2 * lam := by nlinarith only [ht2]
    have hzt := (add_pos hz0 ht).ne'
    have hshape : 2 * (n : ℝ) * lam / (q * (1 + q)) / z^2 =
        (n : ℝ) * (z / t - 1) := by
      dsimp [q]
      field_simp [hz0.ne', ht.ne', hzt]
      linear_combination -(n : ℝ) * he
    rw [sub_div, hshape]
    dsimp [D]
    field_simp [hz0.ne', ht.ne']
    ring

/-- The left side of the held-shape stationary equation is strictly
increasing for every sample with at least one positive count. -/
theorem weighted_sample_degree_strictMono {n : ℕ} (x : Fin n → ℕ) (hx : ∃ i, 0 < x i) :
    StrictMonoOn
      (fun z : ℝ ↦ z * ((∑ i, (x i : ℝ)) +
        ∑ i, if 2 ≤ x i then degreeMean (x i) z⁻¹ else 0)) (Ioi 0) := by
  let S := ∑ i, (x i : ℝ)
  have hS : 0 < S := by
    obtain ⟨i, hi⟩ := hx
    exact Finset.sum_pos' (fun j _ ↦ Nat.cast_nonneg _) ⟨i, Finset.mem_univ _, Nat.cast_pos.mpr hi⟩
  let f := fun z : ℝ ↦ z * (S + ∑ i, if 2 ≤ x i then degreeMean (x i) z⁻¹ else 0)
  have hf (z : ℝ) (hz : 0 < z) :
      HasDerivAt f (S + ∑ i, if 2 ≤ x i then
        degreeMean (x i) z⁻¹ - degreeVariance (x i) z⁻¹ else 0) z := by
    have hc (i : Fin n) : HasDerivAt
        (fun v : ℝ ↦ v * (if 2 ≤ x i then degreeMean (x i) v⁻¹ else 0))
        (if 2 ≤ x i then degreeMean (x i) z⁻¹ - degreeVariance (x i) z⁻¹ else 0) z := by
      by_cases h : 2 ≤ x i
      · simpa only [ite_eq_left h] using weighted_degree_derivative h hz
      · simpa only [ite_eq_right h, mul_zero] using hasDerivAt_const z (0 : ℝ)
    convert ((hasDerivAt_id z).mul_const S).add
      (HasDerivAt.fun_sum fun i (_ : i ∈ Finset.univ) ↦ hc i) using 1
    · ext v
      simp only [f, Pi.add_apply, id_eq, mul_add, Finset.mul_sum]
    · simp only [one_mul]
  apply strictMonoOn_of_deriv_pos (convex_Ioi _)
    (fun z hz ↦ (hf z hz).continuousAt.continuousWithinAt)
  intro z hz
  have hz0 : 0 < z := interior_subset hz
  rw [(hf z hz0).deriv]
  apply add_pos_of_pos_of_nonneg hS
  apply Finset.sum_nonneg
  intro i _
  split_ifs with hi
  · have he := degree_mean_pos hi (inv_pos.mpr hz0)
    have hv := bessel_variance_bound hi (inv_pos.mpr hz0)
    have hu : degreeMean (x i) z⁻¹ * ((x i : ℝ)^2 - (degreeMean (x i) z⁻¹)^2) /
        ((x i : ℝ)^2 + (degreeMean (x i) z⁻¹)^2) ≤ degreeMean (x i) z⁻¹ := by
      apply (div_le_iff₀ (by positivity)).mpr
      nlinarith only [mul_nonneg he.le (sq_nonneg (degreeMean (x i) z⁻¹))]
    exact sub_nonneg.mpr (hv.le.trans hu)
  · exact le_rfl

/-- Every sample with a positive count has a unique maximum of the
held-shape log likelihood in the Bessel argument. -/
theorem held_shape_unique {n : ℕ} (x : Fin n → ℕ) (hx : ∃ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) :
    let S := ∑ i, (x i : ℝ)
    let b := sqrt (2 * lam)
    let L := fun z : ℝ ↦ -(n : ℝ) * (z - sqrt (z^2 - 2 * lam)) - S * log z +
      ∑ i, if 2 ≤ x i then log ((besselPolynomial (x i)).eval z⁻¹) else 0
    ∃! z, z ∈ Ioi b ∧ IsMaxOn L (Ioi b) z := by
  intro S b L
  let D := Ioi b
  let A := fun z : ℝ ↦ z * (S + ∑ i, if 2 ≤ x i then degreeMean (x i) z⁻¹ else 0)
  let q := fun z : ℝ ↦ sqrt (1 - 2 * lam / z^2)
  let H := fun z : ℝ ↦ A z * (q z * (1 + q z))
  let C : ℝ := 2 * n * lam
  have hb : 0 < b := sqrt_pos.mpr (by positivity)
  have hb2 : b^2 = 2 * lam := sq_sqrt (by positivity)
  have hn : 0 < n := by obtain ⟨i, _⟩ := hx; exact i.isLt.pos
  have hC : 0 < C := by dsimp [C]; positivity
  have hS : 0 < S := by
    obtain ⟨i, hi⟩ := hx
    exact Finset.sum_pos' (fun j _ ↦ Nat.cast_nonneg _) ⟨i, Finset.mem_univ _, Nat.cast_pos.mpr hi⟩
  have hAz {z : ℝ} (hz : 0 < z) : z * S ≤ A z := by
    apply mul_le_mul_of_nonneg_left _ hz.le
    apply le_add_of_nonneg_right
    apply Finset.sum_nonneg
    intro i _
    split_ifs with hi
    · exact (degree_mean_pos hi (inv_pos.mpr hz)).le
    · exact le_rfl
  have hA : StrictMonoOn A (Ioi 0) := weighted_sample_degree_strictMono x hx
  have hcA : ContinuousOn A (Ioi 0) := by
    apply continuousOn_id.mul (continuousOn_const.add (continuousOn_finsetSum _ _))
    intro i _
    by_cases hi : 2 ≤ x i
    · simp only [ite_eq_left hi]
      intro z hz
      exact ((degree_mean_hasDerivAt hi (inv_pos.mpr hz)).continuousAt.comp
        (continuousAt_inv₀ (show 0 < z from hz).ne')).continuousWithinAt
    · simp only [ite_eq_right hi]
      exact continuousOn_const
  have hcq : ContinuousOn q (Ioi 0) := by
    exact (continuousOn_const.sub
      (continuousOn_const.div ((continuousOn_id : ContinuousOn (fun z : ℝ ↦ z) (Ioi 0)).pow 2)
        (fun z hz ↦ pow_ne_zero _ (show 0 < z from hz).ne'))).sqrt
  have hcH : ContinuousOn H (Ioi 0) := hcA.mul (hcq.mul (continuousOn_const.add hcq))
  have hrad {z : ℝ} (hz : z ∈ D) : 0 < 1 - 2 * lam / z^2 := by
    rw [sub_pos, div_lt_iff₀ (sq_pos_of_pos (hb.trans hz)), one_mul, ← hb2]
    exact pow_lt_pow_left₀ hz hb.le (by decide)
  have hq {z : ℝ} (hz : z ∈ D) : 0 < q z := sqrt_pos.mpr (hrad hz)
  have hqinc : StrictMonoOn q D := by
    intro z hz w hw hzw
    apply sqrt_lt_sqrt (hrad hz).le
    exact sub_lt_sub_left (div_lt_div_of_pos_left (by positivity) (sq_pos_of_pos (hb.trans hz))
      (pow_lt_pow_left₀ hzw (hb.trans hz).le (by decide))) 1
  have hH : StrictMonoOn H D := by
    intro z hz w hw hzw
    have hAz0 := (mul_pos (hb.trans hz) hS).trans_le (hAz (hb.trans hz))
    have hi := hqinc hz hw hzw
    exact mul_lt_mul_of_pos_of_nonneg' (hA (hb.trans hz) (hb.trans hw) hzw)
      (mul_le_mul (hqinc.monotoneOn hz hw hzw.le) (add_le_add_right hi.le 1)
        (by linarith [hq hz]) (hq hw).le)
      (by positivity [hq hz]) (hAz0.le.trans (hA (hb.trans hz) (hb.trans hw) hzw).le)
  have hzero : H b = 0 := by
    have hh : q b = 0 := by simp [q, hb2, (show 2 * lam ≠ 0 by positivity)]
    simp [H, hh]
  have hstart : ∃ z ∈ D, H z < C := by
    have hc := hcH.continuousAt (Ioi_mem_nhds hb)
    have he := hc.eventually (Iio_mem_nhds (show H b < C from hzero ▸ hC))
    exact ((he.filter_mono nhdsWithin_le_nhds).and
      (self_mem_nhdsWithin : ∀ᶠ z in 𝓝[>] b, z ∈ D)).exists.imp
        (fun _ h ↦ ⟨h.2, h.1⟩)
  obtain ⟨a, ha, hsa⟩ := hstart
  let w := max (a + 1) (C / (S * q a) + 1)
  have haw : a < w := (lt_add_one a).trans_le (le_max_left _ _)
  have hw : w ∈ D := ha.trans haw
  have hsw : C < H w := by
    have hpos := mul_pos hS (hq ha)
    have hh : C / (S * q a) < w := (lt_add_one _).trans_le (le_max_right _ _)
    have h1 := (div_lt_iff₀ hpos).mp hh
    have h2 := hAz (hb.trans hw)
    have h3 : q a ≤ q w * (1 + q w) := by
      have h := hqinc ha hw haw
      nlinarith only [h, sq_nonneg (q w)]
    calc
      C < w * (S * q a) := h1
      _ = (w * S) * q a := by ring
      _ ≤ H w := mul_le_mul h2 h3 (hq ha).le ((mul_pos (hb.trans hw) hS).le.trans h2)
  obtain ⟨r, hr, hrC⟩ := intermediate_value_Icc haw.le
    (hcH.mono (fun z hz ↦ (hb.trans ha).trans_le hz.1)) ⟨hsa.le, hsw.le⟩
  have hrD : r ∈ D := ha.trans_le hr.1
  have hder {z : ℝ} (hz : z ∈ D) :
      HasDerivAt L ((C - H z) / (q z * (1 + q z) * z^2)) z := by
    have hz0 := hb.trans hz
    have hqeq : sqrt (z^2 - 2 * lam) / z = q z := by
      have he : 0 ≤ z^2 - 2 * lam := by
        have h := (div_lt_iff₀ (sq_pos_of_pos hz0)).mp (sub_pos.mp (hrad hz))
        linarith
      dsimp only [q]
      rw [show 1 - 2 * lam / z^2 = (z^2 - 2 * lam) / z^2 by field_simp]
      rw [sqrt_div he, sqrt_sq hz0.le]
    have hd := held_shape_derivative x hl hz
    dsimp only at hd
    rw [hqeq] at hd
    convert hd using 1
    dsimp only [C, H, A]
    field_simp [hz0.ne', (hq hz).ne', (show 1 + q z ≠ 0 by positivity [hq hz])]
    dsimp only [S]
    ring
  have hcL : ContinuousOn L D := fun z hz ↦ (hder hz).continuousAt.continuousWithinAt
  have hinc : StrictMonoOn L (D ∩ Iic r) := by
    apply strictMonoOn_of_deriv_pos ((convex_Ioi _).inter (convex_Iic _))
      (hcL.mono inter_subset_left)
    intro z hz
    have hd := (interior_subset hz).1
    have hzr : z < r := by simpa using interior_mono inter_subset_right hz
    rw [(hder hd).deriv]
    exact div_pos (sub_pos.mpr (hrC ▸ hH hd hrD hzr)) (by positivity [hq hd, hb.trans hd])
  have hdec : StrictAntiOn L (D ∩ Ici r) := by
    apply strictAntiOn_of_deriv_neg ((convex_Ioi _).inter (convex_Ici _))
      (hcL.mono inter_subset_left)
    intro z hz
    have hd := (interior_subset hz).1
    have hzr : r < z := by simpa using interior_mono inter_subset_right hz
    rw [(hder hd).deriv]
    exact div_neg_of_neg_of_pos (sub_neg.mpr (hrC ▸ hH hrD hd hzr))
      (by positivity [hq hd, hb.trans hd])
  have hstrict (z : ℝ) (hz : z ∈ D) (hne : z ≠ r) : L z < L r := by
    rcases lt_or_gt_of_ne hne with h | h
    · exact hinc ⟨hz, h.le⟩ ⟨hrD, le_rfl⟩ h
    · exact hdec ⟨hrD, le_rfl⟩ ⟨hz, h.le⟩ h
  refine ⟨r, ⟨hrD, ?_⟩, ?_⟩
  · intro z hz
    by_cases he : z = r
    · simp [he]
    · exact (hstrict z hz he).le
  · rintro z ⟨hz, hm⟩
    by_contra h
    exact (hstrict z hz h).not_ge (hm hrD)

private theorem argument_bijOn {lam : ℝ} (hl : 0 < lam) :
    BijOn (fun μ : ℝ ↦ sqrt (2 * lam + (lam / μ)^2)) (Ioi 0) (Ioi (sqrt (2 * lam))) := by
  refine ⟨?_, ?_, ?_⟩
  · intro μ hμ
    exact sqrt_lt_sqrt (by positivity) (lt_add_of_pos_right _ (sq_pos_of_pos (div_pos hl hμ)))
  · intro μ hμ ν hν he
    have hμt : 0 < lam / μ := div_pos hl hμ
    have hνt : 0 < lam / ν := div_pos hl hν
    have he' := congrArg (fun w : ℝ ↦ w^2) he
    simp only [sq_sqrt (show 0 ≤ 2 * lam + (lam / μ)^2 by positivity),
      sq_sqrt (show 0 ≤ 2 * lam + (lam / ν)^2 by positivity)] at he'
    have h : lam / μ = lam / ν := by
      rcases lt_trichotomy (lam / μ) (lam / ν) with h | h | h
      · nlinarith only [he', pow_lt_pow_left₀ h hμt.le (show 2 ≠ 0 by decide)]
      · exact h
      · nlinarith only [he', pow_lt_pow_left₀ h hνt.le (show 2 ≠ 0 by decide)]
    exact ((mul_right_inj' hl.ne').mp
      ((div_eq_div_iff (show 0 < μ from hμ).ne' (show 0 < ν from hν).ne').mp h)).symm
  · intro z hz
    have hz0 : 0 < z := (sqrt_nonneg _).trans_lt hz
    have hrad : 0 < z^2 - 2 * lam := by
      have hh := pow_lt_pow_left₀ hz (sqrt_nonneg _) (show 2 ≠ 0 by decide)
      rw [sq_sqrt (by positivity)] at hh
      linarith
    let t := sqrt (z^2 - 2 * lam)
    have ht : 0 < t := sqrt_pos.mpr hrad
    refine ⟨lam / t, div_pos hl ht, ?_⟩
    have he : lam / (lam / t) = t := by field_simp
    change sqrt (2 * lam + (lam / (lam / t))^2) = z
    rw [he, show t^2 = z^2 - 2 * lam from sq_sqrt hrad.le]
    rw [show 2 * lam + (z^2 - 2 * lam) = z^2 by ring, sqrt_sq hz0.le]

/-- The positive-shape likelihood in mean coordinates with its data-only
factor and positive, fixed power of shape omitted. -/
def heldShapeLikelihood {n : ℕ} (x : Fin n → ℕ) (lam μ : ℝ) : ℝ :=
  let z := sqrt (2 * lam + (lam / μ)^2)
  exp (-(n : ℝ) * (z - lam / μ) - (∑ i, (x i : ℝ)) * log z +
    ∑ i, if 2 ≤ x i then log ((besselPolynomial (x i)).eval z⁻¹) else 0)

/-- At positive held shape, a positive count gives a unique positive
mean fit. On all-zero samples the zero-mean limit is the strictly
unattained supremum. -/
theorem held_shape_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {lam : ℝ} (hl : 0 < lam) :
    let L := heldShapeLikelihood x lam
    ((∃ i, 0 < x i) → ∃! μ, μ ∈ Ioi (0 : ℝ) ∧ IsMaxOn L (Ioi 0) μ) ∧
    ((∀ i, x i = 0) →
      Tendsto L (𝓝[>] 0) (𝓝 1) ∧ IsLUB (L '' Ioi 0) 1 ∧ ∀ μ > 0, L μ < 1) := by
  intro L
  constructor
  · intro hx
    let Z := fun μ : ℝ ↦ sqrt (2 * lam + (lam / μ)^2)
    let D := Ioi (sqrt (2 * lam))
    let F := fun z : ℝ ↦ -(n : ℝ) * (z - sqrt (z^2 - 2 * lam)) -
      (∑ i, (x i : ℝ)) * log z +
        ∑ i, if 2 ≤ x i then log ((besselPolynomial (x i)).eval z⁻¹) else 0
    have hZ : BijOn Z (Ioi 0) D := argument_bijOn hl
    have hEq (μ : ℝ) (hμ : 0 < μ) : L μ = exp (F (Z μ)) := by
      have ht : 0 < lam / μ := div_pos hl hμ
      have hz : (Z μ)^2 = 2 * lam + (lam / μ)^2 := sq_sqrt (by positivity)
      have he : sqrt ((Z μ)^2 - 2 * lam) = lam / μ := by
        rw [hz, add_sub_cancel_left, sqrt_sq ht.le]
      dsimp only [F, L, heldShapeLikelihood]
      rw [he]
    have hmax (μ : ℝ) (hμ : μ ∈ Ioi 0) :
        IsMaxOn L (Ioi 0) μ ↔ IsMaxOn F D (Z μ) := by
      constructor
      · intro hm z hz
        obtain ⟨v, hv, rfl⟩ := hZ.surjOn hz
        have hv' : L v ≤ L μ := hm hv
        rw [hEq v hv, hEq μ hμ, exp_le_exp] at hv'
        exact hv'
      · intro hm v hv
        have hv' : F (Z v) ≤ F (Z μ) := hm (hZ.mapsTo hv)
        change L v ≤ L μ
        rw [hEq v hv, hEq μ hμ, exp_le_exp]
        exact hv'
    obtain ⟨z, ⟨hz, hm⟩, hu⟩ := held_shape_unique x hx hl
    obtain ⟨μ, hμ, hr⟩ := hZ.surjOn hz
    refine ⟨μ, ⟨hμ, (hmax μ hμ).mpr (hr.symm ▸ hm)⟩, ?_⟩
    rintro v ⟨hv, hmv⟩
    exact hZ.injOn hv hμ ((hu (Z v) ⟨hZ.mapsTo hv, (hmax v hv).mp hmv⟩).trans hr.symm)
  · intro hx
    have he : L = fun μ ↦ HeldSweepPIG.zeroLikelihood n μ lam := by
      funext μ
      simp [L, heldShapeLikelihood, HeldSweepPIG.zeroLikelihood, hx]
    rw [he]
    exact HeldSweepPIG.held_shape_all_zero hn hl

/-- The all-free PIG classification includes the unidentified mean at
shape zero for all-zero samples and the exact variance cut otherwise. -/
theorem all_free_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    ((∀ i, x i = 0) →
      let D := Ioi (0 : ℝ) ×ˢ Ici 0
      ∀ p ∈ D, IsMaxOn (fun q : ℝ × ℝ ↦ HeldSweepPIG.zeroLikelihood n q.1 q.2) D p ↔
        p.2 = 0) ∧
    ((∃ i, 0 < x i) →
      ((∃ p ∈ positiveChart, IsMaxOn (chartLogLik x) positiveChart p) ↔
        sampleMean x < sampleVariance x) ∧
      (sampleMean x < sampleVariance x →
        ∃! p, p ∈ positiveChart ∧ IsMaxOn (chartLogLik x) positiveChart p) ∧
      (sampleVariance x ≤ sampleMean x →
        IsLUB (chartLogLik x '' positiveChart) (poissonLogLik x (sampleMean x)) ∧
        ∀ p ∈ positiveChart, chartLogLik x p < poissonLogLik x (sampleMean x))) :=
  ⟨fun _ _ hp ↦ HeldSweepPIG.all_zero_argmax hn hp, all_free_positive⟩

end

end UnivariateML.JoinsPIG

end
