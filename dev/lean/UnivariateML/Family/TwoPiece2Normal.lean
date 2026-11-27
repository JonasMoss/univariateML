module

public import UnivariateML.Family.TwoPieceNormal
public import Mathlib.Analysis.SpecialFunctions.Sqrt
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Two-piece normal maximizing graphs and held-moment boundary traces

## Deviations

The held-moment criterion uses the derivative on the full finite skew
interval. Its algebraic decomposition into stationary pieces remains open.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TwoPiece2Normal

open TwoPieceNormal

variable {n : ℕ}

/-- A nonzero held-mean residual rules out collapse along every varying-skew
path. In the all-zero branch every fixed admissible skew gives divergence. -/
theorem heldMean_collapse (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    ((∃ i, x i ≠ μ) →
      ∀ {ι : Type*} {l : Filter ι} {u w : ι → ℝ},
        Tendsto u l atTop → (∀ᶠ t in l, w t ∈ Ioo (0 : ℝ) 1) →
        Tendsto (fun t ↦ heldMeanSkew x μ (w t) (u t)) l atBot) ∧
    ((∀ i, x i = μ) →
      ∀ w ∈ Ioo (0 : ℝ) 1, Tendsto (heldMeanSkew x μ w) atTop atTop) := by
  classical
  refine ⟨?_, fun hx w _ ↦ heldMeanSkew_collapse hn hx⟩
  rintro ⟨i, hi⟩ ι l u w hu hw
  let d := x i - μ
  have hd : 0 < |d| := abs_pos.mpr (sub_ne_zero.mpr hi)
  have hm : 0 < halfMean := sqrt_pos.2 (div_pos (by norm_num) pi_pos)
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let A := (|d| / 2)^2 / 2
  have hA : 0 < A := by positivity
  have ht : Tendsto (fun v : ℝ ↦ -(A / 2) * v^2) atTop atBot :=
    (tendsto_pow_atTop (by decide : 2 ≠ 0)).const_mul_atTop_of_neg (by linarith)
  apply tendsto_atBot_mono' _ _ (ht.comp hu)
  filter_upwards [hu.eventually (eventually_ge_atTop (2 * halfMean / |d|)),
    hu.eventually (eventually_ge_atTop (2 * n * halfMean / A)),
    hu.eventually (eventually_gt_atTop (0 : ℝ)), hw] with t ht₁ ht₂ ht₃ hwt
  change heldMeanSkew x μ (w t) (u t) ≤ -(A / 2) * (u t)^2
  let e := halfMean * (1 - 2 * w t)
  have he : |e| ≤ halfMean := by
    rw [abs_mul, abs_of_pos hm]
    exact mul_le_of_le_one_right hm.le (abs_le.mpr ⟨by linarith [hwt.2], by linarith [hwt.1]⟩)
  have hres : (|d| / 2) * u t ≤ |residual μ (w t) (u t) (x i)| := by
    have h := abs_sub_abs_le_abs_sub (d * u t) (-e)
    rw [sub_neg_eq_add, abs_neg, abs_mul, abs_of_pos ht₃] at h
    change (|d| / 2) * u t ≤ |d * u t + e|
    have hb := (div_le_iff₀ hd).mp ht₁
    nlinarith only [h, hb, he]
  let r := residual μ (w t) (u t) (x i)
  let s := if r < 0 then w t else 1 - w t
  have hs : s ∈ Ioo (0 : ℝ) 1 := by
    dsimp [s]
    split
    · exact hwt
    · exact ⟨sub_pos.mpr hwt.2, sub_lt_self _ hwt.1⟩
  have hterm : A * (u t)^2 ≤ r^2 / (2 * s^2) := by
    calc
      A * (u t)^2 = ((|d| / 2) * u t)^2 / 2 := by dsimp [A]; ring
      _ ≤ |r|^2 / 2 :=
        div_le_div_of_nonneg_right
          (pow_le_pow_left₀ (mul_nonneg (by positivity) ht₃.le) hres 2) (by norm_num)
      _ = r^2 / 2 := by rw [sq_abs]
      _ ≤ r^2 / (2 * s^2) :=
        div_le_div_of_nonneg_left (sq_nonneg _) (mul_pos (by norm_num) (pow_pos hs.1 _))
          (by nlinarith only [pow_le_pow_left₀ hs.1.le hs.2.le 2])
  let loss (j : Fin n) := (residual μ (w t) (u t) (x j))^2 /
    (2 * (if residual μ (w t) (u t) (x j) < 0 then w t else 1 - w t)^2)
  have hsum : A * (u t)^2 ≤ ∑ j, loss j :=
    hterm.trans (Finset.single_le_sum (f := loss) (fun j _ ↦ by dsimp [loss]; positivity)
      (Finset.mem_univ i))
  have hlog := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos (mul_pos hm ht₃)) hN.le
  have hlin := (div_le_iff₀ hA).mp ht₂
  have hquad := mul_le_mul_of_nonneg_right hlin ht₃.le
  unfold heldMeanSkew
  nlinarith only [hsum, hlog, hquad, hN]

/-- For an all-free sample, constants collapse. On nonconstant data
without a finite maximum, the supremum is a translated half-normal at
the corresponding sample extreme. -/
theorem allFree_failure (hn : 0 < n) (x : Fin n → ℝ) :
    (¬ LocationScale.Nonconstant x → ¬ BddAbove (allFreeValues x)) ∧
    (LocationScale.Nonconstant x →
      (¬ ∃ L, IsGreatest (allFreeValues x) L) →
      ∃ (i : Fin n) (left : Bool),
        (if left then ∀ j, x j ≤ x i else ∀ j, x i ≤ x j) ∧
        IsLUB (allFreeValues x)
          (halfLogLik x (x i) (sqrt (endpointSquares x (x i) / n)))) := by
  refine ⟨?_, ?_⟩
  · intro hx hb
    let i : Fin n := ⟨0, hn⟩
    have he (j : Fin n) : x j = x i := not_ne_iff.mp (fun h ↦ hx ⟨j, i, h⟩)
    obtain ⟨L, hL⟩ := hb
    obtain ⟨u, hu, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      ((heldSkew_collapse hn (1/2) he).eventually (eventually_gt_atTop L))).exists
    have hbound := hL (show heldSkew x (1/2) (x i, 1/u) ∈ allFreeValues x from
      ⟨x i, 1/2, 1/u, by norm_num, one_div_pos.mpr hu, rfl⟩)
    exact hgt.not_ge (by simpa only [heldSkew, mul_one_div] using hbound)
  · intro hx hnone
    apply allFree_endpoint_only hn hx
    intro hmode
    obtain ⟨c,w,s,hw,hs,hmax⟩ := (allFree_exists_iff hn x).mpr hmode
    apply hnone
    refine ⟨heldSkew x w (c,s), ⟨c,w,s,hw,hs,rfl⟩, ?_⟩
    rintro L ⟨d,v,t,hv,ht,rfl⟩
    exact hmax d v t hv ht

private theorem halfVariance_pos : 0 < 1 - halfMean^2 := by
  have he : halfMean^2 = 2/π := sq_sqrt (div_nonneg (by norm_num) pi_pos.le)
  rw [he, sub_pos, div_lt_iff₀ pi_pos]
  linarith [pi_gt_three]

/-- With mean and standard deviation held, only the left width fraction varies. -/
noncomputable def heldMeanSd (x : Fin n → ℝ) (μ σ w : ℝ) : ℝ :=
  heldMeanSkew x μ w (sqrt (varianceFraction w) / σ)

private theorem heldMeanSd_reflect (x : Fin n → ℝ) (μ σ w : ℝ) :
    heldMeanSd (fun i ↦ -x i) (-μ) σ (1-w) = heldMeanSd x μ σ w := by
  have hv : varianceFraction (1-w) = varianceFraction w := by unfold varianceFraction; ring
  simp only [heldMeanSd, heldMeanSkew, hv]
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  let u := sqrt (varianceFraction w) / σ
  let R := TwoPieceNormal.residual μ w u (x i)
  have h : TwoPieceNormal.residual (-μ) (1-w) u (-x i) = -R := by
    dsimp [R, TwoPieceNormal.residual]
    ring
  change (TwoPieceNormal.residual (-μ) (1-w) u (-x i))^2 /
      (2 * (if TwoPieceNormal.residual (-μ) (1-w) u (-x i) < 0 then 1-w else 1-(1-w))^2) =
    R^2 / (2 * (if R < 0 then w else 1-w)^2)
  rw [h, neg_sq, show 1-(1-w) = w by ring]
  rcases lt_trichotomy R 0 with hR | hR | hR
  · simp [hR, (neg_pos.mpr hR).not_gt]
  · simp [hR]
  · simp [hR.not_gt, neg_neg_of_pos hR]

private theorem heldMeanSd_rightTrace (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    let H := 1 - halfMean^2
    let b := σ / sqrt H
    let z := μ - halfMean*b
    let C := halfLogLik x z b -
      ((Finset.univ.filter fun i ↦ x i = z).card : ℝ) * (halfMean^2/(8*H^2))
    ((∀ i, z ≤ x i) → Tendsto (heldMeanSd x μ σ) (𝓝[Ioo 0 1] 0) (𝓝 C)) ∧
    ((∃ i, x i < z) → Tendsto (heldMeanSd x μ σ) (𝓝[Ioo 0 1] 0) atBot) := by
  classical
  intro H b z C
  let L := 𝓝[Ioo (0 : ℝ) 1] (0 : ℝ)
  let u (w : ℝ) := sqrt (varianceFraction w) / σ
  let R (i : Fin n) (w : ℝ) := TwoPieceNormal.residual μ w (u w) (x i)
  let P (i : Fin n) (w : ℝ) := (R i w)^2 / (2*(if R i w < 0 then w else 1-w)^2)
  let G (w : ℝ) := (n : ℝ) * log (halfMean * u w)
  have hm : 0 < halfMean := sqrt_pos.2 (div_pos (by norm_num) pi_pos)
  have hq : 0 < sqrt H := sqrt_pos.2 halfVariance_pos
  have hb : 0 < b := div_pos hσ hq
  have hu : Continuous u := by unfold u varianceFraction; fun_prop
  have hu0 : u 0 = 1/b := by simp [u, varianceFraction, H, b]
  have hR0 (i : Fin n) : R i 0 = (x i-z)/b := by
    simp only [R, TwoPieceNormal.residual, hu0, mul_zero, sub_zero, mul_one]
    dsimp only [z]
    field_simp [hb.ne']; ring
  have hR (i : Fin n) : Tendsto (R i) L (𝓝 ((x i-z)/b)) := by
    rw [← hR0]
    apply ContinuousAt.continuousWithinAt
    unfold R TwoPieceNormal.residual
    fun_prop
  have hG : Tendsto G L (𝓝 ((n : ℝ) * log (halfMean/b))) := by
    have hGc : ContinuousAt G 0 := by
      unfold G
      apply continuousAt_const.mul ((continuousAt_const.mul hu.continuousAt).log _)
      change halfMean * u 0 ≠ 0
      rw [hu0]
      positivity
    have he : G 0 = (n : ℝ) * log (halfMean/b) := by
      simp [G, hu0, div_eq_mul_inv]
    rw [← he]
    exact hGc.continuousWithinAt
  have hmem : ∀ᶠ w in L, w ∈ Ioo (0 : ℝ) 1 := self_mem_nhdsWithin
  have hw : Tendsto (fun w : ℝ ↦ w) L (𝓝 0) :=
    continuousWithinAt_id
  constructor
  · intro hs
    let D := halfMean^2/(8*H^2)
    let Q (i : Fin n) := (x i-z)^2 / (2*b^2) + if x i = z then D else 0
    have hP (i : Fin n) : Tendsto (P i) L (𝓝 (Q i)) := by
      rcases (hs i).lt_or_eq with hi | hi
      · have hpos : 0 < (x i-z)/b := div_pos (sub_pos.mpr hi) hb
        have hc : Tendsto (fun w : ℝ ↦ (1-w)^2) L (𝓝 1) := by
          simpa only [sub_zero, one_pow] using (tendsto_const_nhds.sub hw : Tendsto
            (fun w : ℝ ↦ (1-w)) L (𝓝 (1-0))).pow 2
        have ht := ((hR i).pow 2).div (hc.const_mul 2) (by norm_num)
        have hQ : ((x i-z)/b)^2/(2*1) = Q i := by
          simp only [Q, hi.ne', ite_false, add_zero, mul_one, div_pow]
          ring
        rw [← hQ]
        apply ht.congr'
        filter_upwards [(hR i).eventually (lt_mem_nhds hpos)] with w hw
        simp [P, hw.not_gt]
      · have he : Q i = D := by simp [Q, hi.symm]
        rw [he]
        simpa only [P, R, u, hi.symm] using heldMeanSd_rightContact μ hσ
    have hsum := tendsto_finsetSum Finset.univ (fun i _ ↦ hP i)
    have he : (n : ℝ)*log (halfMean/b) - ∑ i, Q i = C := by
      simp only [C, halfLogLik, endpointSquares, Q, Finset.sum_add_distrib, Finset.sum_div]
      rw [show (∑ i, if x i = z then D else 0) =
        ((Finset.univ.filter fun i ↦ x i = z).card : ℝ)*D by
          simp [Finset.sum_ite, nsmul_eq_mul]]
      ring
    rw [← he]
    exact hG.sub hsum
  · rintro ⟨i, hi⟩
    have hneg : (x i-z)/b < 0 := div_neg_of_neg_of_pos (sub_neg.mpr hi) hb
    have hwpos : Tendsto (fun w : ℝ ↦ w) L (𝓝[>] 0) :=
      tendsto_nhdsWithin_iff.mpr ⟨hw, hmem.mono fun _ hw ↦ hw.1⟩
    have hpi : Tendsto (P i) L atTop := by
      have htop := (tendsto_pow_atTop (by decide : (2 : ℕ) ≠ 0)).comp
        hwpos.inv_tendsto_nhdsGT_zero
      have hs := ((hR i).pow 2).div_const 2
      have hpos : 0 < ((x i-z)/b)^2/2 := div_pos (sq_pos_of_ne_zero hneg.ne) (by norm_num)
      apply (hs.pos_mul_atTop hpos htop).congr'
      filter_upwards [(hR i).eventually (gt_mem_nhds hneg)] with w hr
      simp only [P, ite_eq_left hr, Function.comp_def, Pi.inv_apply]
      ring
    have hS : Tendsto (fun w ↦ ∑ j, P j w) L atTop :=
      tendsto_atTop_mono (fun w ↦ Finset.single_le_sum (f := fun j ↦ P j w)
        (fun j _ ↦ by dsimp [P]; positivity) (Finset.mem_univ i)) hpi
    let M := (n : ℝ) * log (halfMean/b) + 1
    apply tendsto_atBot_mono' _ _
      (tendsto_atBot_add_const_right L M (tendsto_neg_atTop_atBot.comp hS))
    filter_upwards [hG.eventually (gt_mem_nhds (lt_add_one ((n : ℝ)*log (halfMean/b))))]
      with w hw
    change G w - (∑ j, P j w) ≤ -(∑ j, P j w) + M
    linarith

/-- Both held-moment half-normal traces include the forced penalty at every
exact contact. A datum on the excluded side makes that orientation lose. -/
theorem heldMeanSd_halfTraces (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ)
    (left : Bool) :
    let y := if left then fun i ↦ -x i else x
    let m := if left then -μ else μ
    let H := 1 - halfMean^2
    let b := σ / sqrt H
    let z := m - halfMean*b
    let C := halfLogLik y z b -
      ((Finset.univ.filter fun i ↦ y i = z).card : ℝ) * (halfMean^2/(8*H^2))
    let L := 𝓝[Ioo (0 : ℝ) 1] (if left then 1 else 0)
    ((∀ i, z ≤ y i) → Tendsto (heldMeanSd x μ σ) L (𝓝 C)) ∧
    ((∃ i, y i < z) → Tendsto (heldMeanSd x μ σ) L atBot) := by
  cases left
  · exact heldMeanSd_rightTrace x μ hσ
  · dsimp only [Bool.true_eq, ite_true]
    have ht : Tendsto (fun w : ℝ ↦ 1-w) (𝓝[Ioo (0 : ℝ) 1] 1) (𝓝[Ioo 0 1] 0) := by
      apply tendsto_nhdsWithin_iff.mpr
      refine ⟨?_, ?_⟩
      · have hc : Continuous (fun w : ℝ ↦ 1-w) := continuous_const.sub continuous_id
        have hh : Tendsto (fun w : ℝ ↦ 1-w) (𝓝[Ioo (0 : ℝ) 1] 1) (𝓝 (1-1)) :=
          hc.continuousAt.continuousWithinAt
        simpa only [sub_self] using hh
      · exact (show ∀ᶠ w in 𝓝[Ioo (0 : ℝ) 1] 1, w ∈ Ioo (0 : ℝ) 1
          from self_mem_nhdsWithin).mono fun w hw ↦ ⟨sub_pos.mpr hw.2, sub_lt_self _ hw.1⟩
    obtain ⟨hgood, hbad⟩ := heldMeanSd_rightTrace (fun i ↦ -x i) (-μ) hσ
    exact ⟨fun h ↦ (hgood h).comp ht |>.congr (heldMeanSd_reflect x μ σ),
      fun h ↦ (hbad h).comp ht |>.congr (heldMeanSd_reflect x μ σ)⟩

private theorem differentiableAt_positive_square (z : ℝ) :
    DifferentiableAt ℝ (fun r : ℝ ↦ (max r 0)^2) z := by
  rcases lt_trichotomy z 0 with hz | rfl | hz
  · apply (differentiableAt_const (c := (0 : ℝ))).congr_of_eventuallyEq
    filter_upwards [gt_mem_nhds hz] with r hr
    simp [max_eq_right hr.le]
  · have hl : HasDerivWithinAt (fun r : ℝ ↦ (max r 0)^2) 0 (Iic 0) 0 := by
      apply (hasDerivWithinAt_const (s := Iic (0 : ℝ)) (x := (0 : ℝ)) (c := (0 : ℝ))).congr
      · intro r hr
        simp [max_eq_right (show r ≤ 0 from hr)]
      · norm_num
    have hr : HasDerivWithinAt (fun r : ℝ ↦ (max r 0)^2) 0 (Ici 0) 0 := by
      have hp : HasDerivAt (fun r : ℝ ↦ r^2) 0 0 := by
        convert hasDerivAt_pow 2 (0 : ℝ) using 1
        norm_num
      apply hp.hasDerivWithinAt.congr
      · intro r hr
        simp [max_eq_left (show 0 ≤ r from hr)]
      · norm_num
    have hd : HasDerivAt (fun r : ℝ ↦ (max r 0)^2) 0 0 := by
      simpa only [Iic_union_Ici, hasDerivWithinAt_univ] using hl.union hr
    exact hd.differentiableAt
  · apply ((differentiableAt_id (x := z)).pow 2).congr_of_eventuallyEq
    filter_upwards [lt_mem_nhds hz] with r hr
    simp [max_eq_left hr.le]

private theorem heldMeanSd_differentiableAt {x : Fin n → ℝ} {μ σ w : ℝ}
    (hσ : 0 < σ) (hw : w ∈ Ioo (0 : ℝ) 1) :
    DifferentiableAt ℝ (heldMeanSd x μ σ) w := by
  let U := fun v : ℝ ↦ sqrt (varianceFraction v) / σ
  have hv : 0 < varianceFraction w := by
    unfold varianceFraction
    exact add_pos_of_nonneg_of_pos (mul_nonneg halfVariance_pos.le (sq_nonneg _))
      (mul_pos hw.1 (sub_pos.mpr hw.2))
  have hvd : DifferentiableAt ℝ varianceFraction w := by unfold varianceFraction; fun_prop
  have hud : DifferentiableAt ℝ U w := (hvd.sqrt hv.ne').div_const _
  let R (i : Fin n) := fun v : ℝ ↦ TwoPieceNormal.residual μ v (U v) (x i)
  have hRd (i : Fin n) : DifferentiableAt ℝ (R i) w := by
    unfold R TwoPieceNormal.residual
    fun_prop
  let P (i : Fin n) (v : ℝ) := (max (-R i v) 0)^2/(2*v^2) +
    (max (R i v) 0)^2/(2*(1-v)^2)
  have hPd (i : Fin n) : DifferentiableAt ℝ (P i) w := by
    have hL : DifferentiableAt ℝ (fun v ↦ (max (-R i v) 0)^2) w := by
      simpa only [Function.comp_def, Pi.neg_apply] using
        (differentiableAt_positive_square (-R i w)).comp w (hRd i).neg
    have hR : DifferentiableAt ℝ (fun v ↦ (max (R i v) 0)^2) w :=
      (differentiableAt_positive_square (R i w)).comp w (hRd i)
    unfold P
    exact (hL.div (by fun_prop) (by have := hw.1; positivity)).add
      (hR.div (by fun_prop) (by have := sub_pos.mpr hw.2; positivity))
  have hsum : DifferentiableAt ℝ (fun v ↦ ∑ i, P i v) w := by
    classical
    have h (s : Finset (Fin n)) : DifferentiableAt ℝ (fun v ↦ ∑ i ∈ s, P i v) w := by
      induction s using Finset.induction_on with
      | empty => simp only [Finset.sum_empty]; exact differentiableAt_const _
      | insert i s hi hs =>
        simp only [Finset.sum_insert hi]
        exact (hPd i).add hs
    exact h Finset.univ
  have hm : 0 < halfMean := sqrt_pos.2 (div_pos (by norm_num) pi_pos)
  have hu : 0 < U w := div_pos (sqrt_pos.2 hv) hσ
  have hbase : DifferentiableAt ℝ
      (fun v ↦ n * log (halfMean * U v) - ∑ i, P i v) w :=
    (((hud.const_mul halfMean).log (mul_pos hm hu).ne').const_mul (n : ℝ)).sub hsum
  apply hbase.congr_of_eventuallyEq
  apply Eventually.of_forall
  intro v
  unfold heldMeanSd heldMeanSkew
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  change (R i v)^2 / (2*(if R i v < 0 then v else 1-v)^2) = P i v
  by_cases h : R i v < 0
  · simp [P, h, max_eq_left (neg_nonneg.mpr h.le), max_eq_right h.le]
  · have hr := le_of_not_gt h
    simp [P, h, max_eq_left hr, max_eq_right (neg_nonpos.mpr hr)]

private theorem interval_max_of_strict_witness {f : ℝ → ℝ}
    (hc : ContinuousOn f (Ioo 0 1)) {v : ℝ} (hv : v ∈ Ioo 0 1)
    (h₀ : ∀ᶠ w in 𝓝[Ioo (0 : ℝ) 1] 0, f w < f v)
    (h₁ : ∀ᶠ w in 𝓝[Ioo (0 : ℝ) 1] 1, f w < f v) :
    ∃ w ∈ Ioo (0 : ℝ) 1, IsMaxOn f (Ioo 0 1) w := by
  obtain ⟨ε₀, he₀, h₀⟩ := Metric.eventually_nhds_iff.mp (eventually_nhdsWithin_iff.mp h₀)
  obtain ⟨ε₁, he₁, h₁⟩ := Metric.eventually_nhds_iff.mp (eventually_nhdsWithin_iff.mp h₁)
  let l := min v (ε₀/2)
  let r := max v (1-ε₁/2)
  have hl : 0 < l := lt_min hv.1 (half_pos he₀)
  have hr : r < 1 := max_lt hv.2 (by linarith)
  have hvr : v ∈ Icc l r := ⟨min_le_left _ _, le_max_left _ _⟩
  have hsub : Icc l r ⊆ Ioo 0 1 := fun _ hm ↦ ⟨hl.trans_le hm.1, hm.2.trans_lt hr⟩
  obtain ⟨w, hw, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨v,hvr⟩ (hc.mono hsub)
  refine ⟨w, hsub hw, fun t ht ↦ ?_⟩
  by_cases htl : l ≤ t
  · by_cases htr : t ≤ r
    · exact hmax ⟨htl,htr⟩
    · have hdist : dist t 1 < ε₁ := by
        rw [Real.dist_eq, abs_of_neg (sub_neg.mpr ht.2)]
        have hh := (le_max_right v (1-ε₁/2)).trans_lt (lt_of_not_ge htr)
        linarith
      exact (h₁ hdist ht).le.trans (hmax hvr)
  · have hdist : dist t 0 < ε₀ := by
      rw [Real.dist_eq, sub_zero, abs_of_pos ht.1]
      exact ((lt_of_not_ge htl).trans_le (min_le_right v _)).trans (half_lt_self he₀)
    exact (h₀ hdist ht).le.trans (hmax hvr)

/-- With both moments held, a finite optimum exists exactly when a
stationary skew reaches both compatible half-normal traces. A face
with an observation outside its support imposes no constraint. -/
theorem heldMeanSd_criterion (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    let f := heldMeanSd x μ σ
    let y := fun (left : Bool) ↦ if left then fun i ↦ -x i else x
    let H := 1 - halfMean^2
    let b := σ / sqrt H
    let z := fun (left : Bool) ↦ (if left then -μ else μ) - halfMean*b
    let C := fun (left : Bool) ↦ halfLogLik (y left) (z left) b -
      ((Finset.univ.filter fun i ↦ y left i = z left).card : ℝ) * (halfMean^2/(8*H^2))
    (∃ w ∈ Ioo (0 : ℝ) 1, IsMaxOn f (Ioo 0 1) w) ↔
      ∃ w ∈ Ioo (0 : ℝ) 1, deriv f w = 0 ∧
        ∀ left : Bool, (∀ i, z left ≤ y left i) → C left ≤ f w := by
  classical
  intro f y H b z C
  constructor
  · rintro ⟨w, hw, hm⟩
    have hz : deriv f w = 0 := (hm.isLocalMax (Ioo_mem_nhds hw.1 hw.2)).deriv_eq_zero
    refine ⟨w, hw, hz, ?_⟩
    intro left hs
    let t : ℝ := if left then 1 else 0
    have hclos : t ∈ closure (Ioo (0 : ℝ) 1) := by
      rw [closure_Ioo zero_ne_one]
      cases left <;> norm_num [t]
    let : (𝓝[Ioo (0 : ℝ) 1] t).NeBot := mem_closure_iff_nhdsWithin_neBot.mp hclos
    have ht : Tendsto f (𝓝[Ioo (0 : ℝ) 1] t) (𝓝 (C left)) :=
      (heldMeanSd_halfTraces x μ hσ left).1 hs
    have he : ∀ᶠ v in 𝓝[Ioo (0 : ℝ) 1] t, v ∈ Ioo (0 : ℝ) 1 := self_mem_nhdsWithin
    exact le_of_tendsto ht (he.mono fun _ h ↦ hm h)
  · rintro ⟨w, hw, _, hC⟩
    by_cases hwin : ∃ v ∈ Ioo (0 : ℝ) 1, f w < f v
    · obtain ⟨v,hv,hgt⟩ := hwin
      have hc : ContinuousOn f (Ioo 0 1) := fun t ht ↦
        (heldMeanSd_differentiableAt hσ ht).continuousAt.continuousWithinAt
      have hcut (left : Bool) :
          ∀ᶠ t in 𝓝[Ioo (0 : ℝ) 1] (if left then 1 else 0), f t < f v := by
        obtain ⟨hgood,hbad⟩ := heldMeanSd_halfTraces x μ hσ left
        by_cases hs : ∀ i, z left ≤ y left i
        · exact (hgood hs).eventually (gt_mem_nhds ((hC left hs).trans_lt hgt))
        · push Not at hs
          exact (hbad hs).eventually (eventually_lt_atBot _)
      exact interval_max_of_strict_witness hc hv (hcut false) (hcut true)
    · exact ⟨w, hw, fun v hv ↦ le_of_not_gt (fun h ↦ hwin ⟨v,hv,h⟩)⟩

private theorem modeProfile_pos {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (c : ℝ) : 0 < modeProfile x c := by
  have hL : 0 ≤ leftSquares x c := Finset.sum_nonneg (fun _ _ ↦ sq_nonneg _)
  have hR : 0 ≤ rightSquares x c := Finset.sum_nonneg (fun _ _ ↦ sq_nonneg _)
  have hx' : ∃ i, x i ≠ c := by
    by_contra! he
    obtain ⟨i,j,hij⟩ := hx
    exact hij ((he i).trans (he j).symm)
  obtain ⟨i, hi⟩ := hx'
  unfold modeProfile
  apply pow_pos
  rcases lt_or_gt_of_ne hi with h | h
  · have hp : 0 < leftSquares x c :=
      (sq_pos_of_ne_zero (show max (c-x i) 0 ≠ 0 by
        exact (lt_of_lt_of_le (sub_pos.mpr h) (le_max_left _ _)).ne')).trans_le
        (Finset.single_le_sum (f := fun j ↦ (max (c-x j) 0)^2)
          (fun _ _ ↦ sq_nonneg _) (Finset.mem_univ i))
    exact add_pos_of_pos_of_nonneg (rpow_pos_of_pos hp _) (rpow_nonneg hR _)
  · have hp : 0 < rightSquares x c :=
      (sq_pos_of_ne_zero (show max (x i-c) 0 ≠ 0 by
        exact (lt_of_lt_of_le (sub_pos.mpr h) (le_max_left _ _)).ne')).trans_le
        (Finset.single_le_sum (f := fun j ↦ (max (x j-c) 0)^2)
          (fun _ _ ↦ sq_nonneg _) (Finset.mem_univ i))
    exact add_pos_of_nonneg_of_pos (rpow_nonneg hL _) (rpow_pos_of_pos hp _)

/-- The finite maximizing graph contains exactly the interior
minimizing modes and their profiled parameters. The exact reflected
sample has two distinct points in that graph. -/
theorem allFree_maximizers (hn : 0 < n) (x : Fin n → ℝ) :
    let M := fun {k : ℕ} (y : Fin k → ℝ) (c w s : ℝ) ↦
      w ∈ Ioo 0 1 ∧ 0 < s ∧
        ∀ (d v t : ℝ), v ∈ Ioo 0 1 → 0 < t → heldSkew y v (d,t) ≤ heldSkew y w (c,s)
    (∀ c w s, M x c w s ↔
      LocationScale.Nonconstant x ∧
      IsMinOn (modeProfile x) univ c ∧
      w ∈ Ioo 0 1 ∧ w = modeFraction x c ∧ s = sqrt (modeProfile x c / n)) ∧
    (∃ c w w' s s', c ≠ 0 ∧ M reflectedSample c w s ∧ M reflectedSample (-c) w' s') := by
  intro M
  constructor
  · intro c w s
    constructor
    · rintro ⟨hw, hs, hm⟩
      have hx : LocationScale.Nonconstant x := by
        obtain ⟨d,⟨i,hi⟩,⟨j,hj⟩,_⟩ :=
          (allFree_exists_iff hn x).mp ⟨c,w,s,hw,hs,hm⟩
        exact ⟨i,j,(hi.trans hj).ne⟩
      obtain ⟨hmin, he, he'⟩ := (allFree_graph hn hx hw hs).mp hm
      exact ⟨hx, hmin, hw, he, he'⟩
    · rintro ⟨hx,hmin,hw,he,rfl⟩
      have hN : (0 : ℝ) < n := by exact_mod_cast hn
      have hs := sqrt_pos.2 (div_pos (modeProfile_pos hx c) hN)
      exact ⟨hw, hs, (allFree_graph hn hx hw hs).mpr ⟨hmin,he,rfl⟩⟩
  · obtain ⟨c,hc,hw,he,hmin,hmin'⟩ := allFree_reflected_tie
    have hx : LocationScale.Nonconstant reflectedSample :=
      ⟨0,21,by norm_num [reflectedSample]⟩
    have hs (d : ℝ) : 0 < sqrt (modeProfile reflectedSample d / 22) :=
      sqrt_pos.2 (div_pos (modeProfile_pos hx d) (by norm_num))
    have hw' : modeFraction reflectedSample (-c) ∈ Ioo 0 1 := by
      rw [he]
      exact ⟨sub_pos.mpr hw.2, sub_lt_self _ hw.1⟩
    exact ⟨c,_,_,_,_,hc,⟨hw,hs c,(allFree_graph (by decide) hx hw (hs c)).mpr
      ⟨hmin,rfl,rfl⟩⟩,⟨hw',hs (-c),(allFree_graph (by decide) hx hw' (hs (-c))).mpr
      ⟨hmin',rfl,rfl⟩⟩⟩

end UnivariateML.TwoPiece2Normal

end
