module

public import UnivariateML.Generic.LocationScale
public import Mathlib.Analysis.SpecialFunctions.Gamma.Basic
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
public import Mathlib.Analysis.Convex.SpecificFunctions.Pow
public import Mathlib.Analysis.MeanInequalitiesPow
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Two-piece generalized error likelihoods

Here `u` is inverse total kernel width and `w` is the left width fraction.
At fixed shape and skew, positive standard deviation is a positive constant
times total kernel width. The public mean is the join plus the half-kernel
mean times the right-minus-left width difference.

## Deviations

The atlas claims scale uniqueness at every positive held shape when mean
and skew are held. The dossier records separated ties below shape one.
The exact counterexample here uses shape one half and a sample at minus one.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TwoPieceSGED

noncomputable section

variable {n : ℕ}

/-- First moment of the unit half-GED kernel. -/
def halfMoment (p : ℝ) : ℝ := Gamma (2/p) / Gamma (1/p)

/-- The two-piece power penalty in signed residual coordinates. -/
def powerPenalty (p w r : ℝ) : ℝ :=
  (|r| / (if r < 0 then w else 1-w))^p

/-- Mean-and-skew held log likelihood, in inverse total kernel width. -/
def heldMeanSkew (x : Fin n → ℝ) (μ p w u : ℝ) : ℝ :=
  (n : ℝ) * (log u - log (Gamma (1+1/p))) -
    ∑ i, powerPenalty p w ((x i-μ)*u + halfMoment p*(1-2*w))

private theorem halfMoment_pos {p : ℝ} (hp : 0 < p) : 0 < halfMoment p :=
  div_pos (Gamma_pos_of_pos (div_pos (by norm_num) hp))
    (Gamma_pos_of_pos (div_pos one_pos hp))

private theorem penalty_nonneg {w r p : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) :
    0 ≤ powerPenalty p w r := by
  unfold powerPenalty
  split
  · exact rpow_nonneg (div_nonneg (abs_nonneg _) hw.1.le) _
  · exact rpow_nonneg (div_nonneg (abs_nonneg _) (sub_nonneg.mpr hw.2.le)) _

private def ratio (t : ℝ) : ℝ := log (1+t^2)/t

private theorem ratio_atTop : Tendsto ratio atTop (𝓝 0) := by
  have hlog : Tendsto (fun t : ℝ ↦ log t/t) atTop (𝓝 0) :=
    isLittleO_log_id_atTop.tendsto_div_nhds_zero
  have hc : Tendsto (fun t : ℝ ↦ log 2/t) atTop (𝓝 0) := by
    simpa only [div_eq_mul_inv, mul_zero] using tendsto_inv_atTop_zero.const_mul (log 2)
  have hb : Tendsto (fun t : ℝ ↦ (log 2 + 2*log t)/t) atTop (𝓝 0) := by
    simpa only [add_div, mul_div_assoc, mul_zero, add_zero] using
      hc.add (hlog.const_mul 2)
  apply squeeze_zero' _ _ hb
  · filter_upwards [eventually_ge_atTop (1 : ℝ)] with t ht
    exact div_nonneg (log_nonneg (by nlinarith [sq_nonneg t])) (by linarith)
  · filter_upwards [eventually_ge_atTop (1 : ℝ)] with t ht
    unfold ratio
    apply div_le_div_of_nonneg_right _ (by linarith)
    have ht' : 0 < t := by linarith
    have h2 : 1+t^2 ≤ 2*t^2 := by nlinarith [mul_nonneg (sub_nonneg.mpr ht) (show 0 ≤ t+1 by linarith)]
    have hl := log_le_log (show 0 < 1+t^2 by positivity) h2
    rw [log_mul (by norm_num : (2 : ℝ) ≠ 0) (pow_ne_zero 2 ht'.ne'), log_pow] at hl
    exact hl

private theorem ratio_attains :
    ∃ t > 0, 0 < ratio t ∧ ∀ v > 0, ratio v ≤ ratio t := by
  have h1 : ratio 1 = log 2 := by norm_num [ratio]
  have hp : 0 < ratio 1 := h1 ▸ log_pos (by norm_num)
  let lo := min 1 (ratio 1)
  have hlo : 0 < lo := lt_min one_pos hp
  have hs (v : ℝ) (hv : 0 < v) (hvl : v ≤ lo) : ratio v ≤ ratio 1 := by
    unfold ratio at ⊢
    rw [show log (1+(1:ℝ)^2)/1 = ratio 1 by rfl]
    have hl : log (1+v^2) ≤ v^2 := by
      linarith only [log_le_sub_one_of_pos (show 0 < 1+v^2 by positivity)]
    have hle : log (1+v^2)/v ≤ v := (div_le_iff₀ hv).mpr (by simpa [pow_two] using hl)
    exact hle.trans (hvl.trans (min_le_right _ _))
  obtain ⟨r,hr⟩ := eventually_atTop.mp (ratio_atTop.eventually (gt_mem_nhds hp))
  let hi := max 1 r
  have hi1 : 1 ≤ hi := le_max_left _ _
  have hh (v : ℝ) (hv : hi ≤ v) : ratio v ≤ ratio 1 :=
    (hr v ((le_max_right _ _).trans hv)).le
  have hI : (1 : ℝ) ∈ Icc lo hi := ⟨min_le_left _ _,hi1⟩
  have hc : ContinuousOn ratio (Icc lo hi) := by
    intro v hv
    have hv' := hlo.trans_le hv.1
    unfold ratio
    exact (((continuousAt_const.add (continuousAt_id.pow 2)).log
      (by simp only [id_eq]; positivity : (1 : ℝ) + (id v)^2 ≠ 0)).div
      continuousAt_id hv'.ne').continuousWithinAt
  obtain ⟨t,ht,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1,hI⟩ hc
  refine ⟨t,hlo.trans_le ht.1,hp.trans_le (hm hI),?_⟩
  intro v hv
  by_cases hv' : v ∈ Icc lo hi
  · exact hm hv'
  · by_cases hvl : lo ≤ v
    · exact (hh v (le_of_not_ge (fun hvh ↦ hv' ⟨hvl,hvh⟩))).trans (hm hI)
    · exact (hs v hv (le_of_not_ge hvl)).trans (hm hI)

/-- At shape one half and held mean zero there is a valid held skew
for which the sample at minus one has distinct global scale maximizers. -/
theorem heldMeanSkew_subunitTie :
    ∃ w ∈ Ioo (0 : ℝ) 1, ∃ u₁ u₂ : ℝ, 0 < u₁ ∧ u₁ < u₂ ∧
      IsMaxOn (heldMeanSkew (fun _ : Fin 1 ↦ -1) 0 (1/2) w) (Ioi 0) u₁ ∧
      IsMaxOn (heldMeanSkew (fun _ : Fin 1 ↦ -1) 0 (1/2) w) (Ioi 0) u₂ := by
  obtain ⟨t,ht,hα,hmax⟩ := ratio_attains
  let α := ratio t
  let k := halfMoment (1/2)
  let w := k/(α^2+2*k)
  let δ := k*(1-2*w)
  have hk : 0 < k := halfMoment_pos (by norm_num)
  have hden : 0 < α^2+2*k := by positivity
  have hw : w ∈ Ioo (0 : ℝ) 1 := ⟨div_pos hk hden,
    (div_lt_iff₀ hden).mpr (by nlinarith [sq_nonneg α])⟩
  have hd : δ = w*α^2 := by
    change k*(1-2*(k/(α^2+2*k))) = (k/(α^2+2*k))*α^2
    field_simp
    ring
  have hδ : 0 < δ := hd.symm ▸ mul_pos hw.1 (sq_pos_of_pos hα)
  let f := heldMeanSkew (fun _ : Fin 1 ↦ -1) 0 (1/2) w
  let C := log (Gamma (1+1/(1/2 : ℝ)))
  have hf (u : ℝ) : f u = log u-C-powerPenalty (1/2) w (δ-u) := by
    simp only [f, heldMeanSkew, Nat.cast_one, one_mul, Fin.sum_univ_one, sub_zero, neg_one_mul,
      C, δ, k]
    ring_nf
  have hbase : f δ = log δ-C := by
    rw [hf, sub_self]
    simp [powerPenalty]
  have hpos (u : ℝ) (hu : δ < u) :
      f u - f δ = log (u/δ) - α * sqrt (u/δ-1) := by
    rw [hf, hbase, log_div (hδ.trans hu).ne' hδ.ne']
    have hs : δ-u < 0 := sub_neg.mpr hu
    have he : -(δ-u)/w = α^2 * (u/δ-1) := by
      have h := congrArg (fun z : ℝ ↦ (u-δ)*z) hd
      field_simp [hw.1.ne', hδ.ne']
      nlinarith only [h]
    rw [powerPenalty, ite_eq_left hs, abs_of_neg hs, he, ← sqrt_eq_rpow,
      sqrt_mul (sq_nonneg α), sqrt_sq hα.le]
    ring
  have hm₁ : IsMaxOn f (Ioi 0) δ := by
    intro u hu
    change f u ≤ f δ
    rcases lt_trichotomy u δ with h | h | h
    · change 0 < u at hu
      have hlog := log_lt_log hu h
      rw [hf, hbase]
      linarith [penalty_nonneg (r := δ-u) (p := 1/2) hw]
    · rw [h]
    · let v := sqrt (u/δ-1)
      have hq : 0 < u/δ-1 := sub_pos.mpr ((lt_div_iff₀ hδ).mpr (by simpa using h))
      have hv : 0 < v := sqrt_pos.2 hq
      have he : 1+v^2 = u/δ := by unfold v; rw [sq_sqrt hq.le]; ring
      have hh := hmax v hv
      unfold ratio at hh
      rw [he] at hh
      have hh' := (div_le_iff₀ hv).mp hh
      have hpos' := hpos u h
      change log (u/δ) ≤ α*v at hh'
      change f u - f δ = log (u/δ) - α*v at hpos'
      linarith
  let u := δ*(1+t^2)
  have hu : δ < u := by unfold u; nlinarith only [mul_pos hδ (sq_pos_of_pos ht)]
  have hEq : f u = f δ := by
    have he : u/δ = 1+t^2 := by unfold u; field_simp
    have hf' := hpos u hu
    rw [he, add_sub_cancel_left, sqrt_sq ht.le] at hf'
    have hαeq : α*t = log (1+t^2) := by unfold α ratio; field_simp
    linarith
  refine ⟨w,hw,δ,u,hδ,hu,hm₁,fun r hr ↦ ?_⟩
  change f r ≤ f u
  rw [hEq]
  exact hm₁ hr

private theorem powerPenalty_lower {p w r : ℝ} (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) :
    |r|^p ≤ powerPenalty p w r := by
  unfold powerPenalty
  split
  · exact rpow_le_rpow (abs_nonneg _)
      ((le_div_iff₀ hw.1).2 (mul_le_of_le_one_right (abs_nonneg _) hw.2.le)) hp.le
  · have h := sub_pos.mpr hw.2
    exact rpow_le_rpow (abs_nonneg _)
      ((le_div_iff₀ h).2 (mul_le_of_le_one_right (abs_nonneg _) (by linarith [hw.1]))) hp.le

private theorem powerPenalty_hinges {p w : ℝ} (hp : 0 < p)
    (r : ℝ) :
    powerPenalty p w r = (max (-r) 0 / w)^p + (max r 0 / (1-w))^p := by
  unfold powerPenalty
  by_cases h : r < 0
  · simp only [ite_eq_left h, max_eq_left (neg_nonneg.mpr h.le), max_eq_right h.le,
      abs_of_neg h, zero_div, zero_rpow hp.ne', add_zero]
  · have hh : 0 ≤ r := le_of_not_gt h
    simp only [ite_eq_right h, max_eq_right (neg_nonpos.mpr hh), max_eq_left hh,
      abs_of_nonneg hh, zero_div, zero_rpow hp.ne', zero_add]

/-- With all observations at the held mean the held-shape, held-skew
likelihood diverges as width vanishes. -/
theorem heldMeanSkew_collapse (hn : 0 < n) {x : Fin n → ℝ} {μ p w : ℝ}
    (hx : ∀ i, x i = μ) : Tendsto (heldMeanSkew x μ p w) atTop atTop := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let C := -(n : ℝ) * log (Gamma (1+1/p)) -
    ∑ _i : Fin n, powerPenalty p w (halfMoment p*(1-2*w))
  have ht := tendsto_atTop_add_const_right atTop C
    ((tendsto_const_mul_atTop_of_pos hN).2 tendsto_log_atTop)
  convert ht using 1
  funext u
  simp only [heldMeanSkew, hx, sub_self, zero_mul, zero_add, C]
  ring

private theorem heldMeanSkew_continuousAt {x : Fin n → ℝ} {μ p w u : ℝ}
    (hp : 0 < p) (hu : 0 < u) :
    ContinuousAt (heldMeanSkew x μ p w) u := by
  let d (i : Fin n) := x i-μ
  let e := halfMoment p*(1-2*w)
  let r (i : Fin n) (v : ℝ) := d i*v+e
  have hterm (i : Fin n) : Continuous (fun v ↦ powerPenalty p w (r i v)) := by
    simp_rw [powerPenalty_hinges hp]
    have hr : Continuous (r i) := by unfold r; fun_prop
    exact (((hr.neg.max continuous_const).div_const w).rpow_const (fun _ ↦ Or.inr hp.le)).add
      (((hr.max continuous_const).div_const (1-w)).rpow_const (fun _ ↦ Or.inr hp.le))
  have hsum (s : Finset (Fin n)) :
      Continuous (fun v ↦ ∑ i ∈ s, powerPenalty p w (r i v)) := by
    classical
    induction s using Finset.induction_on with
    | empty => simpa only [Finset.sum_empty] using (continuous_const :
        Continuous (fun _ : ℝ ↦ (0 : ℝ)))
    | insert i s hi hs =>
      simp_rw [Finset.sum_insert hi]
      exact (hterm i).add hs
  change ContinuousAt (fun v ↦ (n : ℝ) * (log v - log (Gamma (1+1/p))) -
    ∑ i, powerPenalty p w (r i v)) u
  exact (continuousAt_const.mul ((continuousAt_log hu.ne').sub continuousAt_const)).sub
    (hsum Finset.univ).continuousAt

/-- A nonzero residual excludes both width ends under held mean, shape
and skew, at every positive held shape. -/
theorem heldMeanSkew_lossAtEnds (hn : 0 < n) {x : Fin n → ℝ} {μ p w : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) (hx : ∃ i, x i ≠ μ) :
    Tendsto (heldMeanSkew x μ p w) (𝓝[>] 0) atBot ∧
      Tendsto (heldMeanSkew x μ p w) atTop atBot := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let C := -(n : ℝ) * log (Gamma (1+1/p))
  let e := halfMoment p*(1-2*w)
  refine ⟨?_,?_⟩
  · have ht := tendsto_atBot_add_const_right (𝓝[>] (0 : ℝ)) C
      ((tendsto_const_mul_atBot_of_pos hN).2 tendsto_log_nhdsGT_zero)
    apply tendsto_atBot_mono _ ht
    intro u
    have hsum : 0 ≤ ∑ i, powerPenalty p w ((x i-μ)*u+e) :=
      Finset.sum_nonneg (fun _ _ ↦ penalty_nonneg hw)
    change heldMeanSkew x μ p w u ≤ n * log u + C
    unfold heldMeanSkew C
    change (n : ℝ)*(log u-log (Gamma (1+1/p))) -
      (∑ i, powerPenalty p w ((x i-μ)*u+e)) ≤ _
    nlinarith only [hsum]
  · obtain ⟨i,hi⟩ := hx
    let d := x i-μ
    have hd : 0 < |d| := abs_pos.mpr (sub_ne_zero.mpr hi)
    let A := (|d|/2)^p
    have hA : 0 < A := rpow_pos_of_pos (half_pos hd) _
    have hlog := ((isLittleO_log_rpow_atTop hp).const_mul_left (n : ℝ)).bound
      (show 0 < A/2 by positivity)
    have ht := tendsto_atBot_add_const_right atTop C
      (((tendsto_rpow_atTop hp).const_mul_atTop_of_neg
        (show -A/2 < 0 by linarith)))
    apply tendsto_atBot_mono' _ _ ht
    filter_upwards [eventually_gt_atTop (0 : ℝ),
      eventually_ge_atTop (2*|e|/|d|), hlog] with u hu he hl
    have he' := (div_le_iff₀ hd).mp he
    have ha : (|d|/2)*u ≤ |d*u+e| := by
      have h := abs_sub_abs_le_abs_sub (d*u) (-e)
      rw [sub_neg_eq_add, abs_neg, abs_mul, abs_of_pos hu] at h
      nlinarith only [h, he']
    have hA' : A*u^p ≤ powerPenalty p w (d*u+e) := by
      unfold A
      rw [← mul_rpow (div_nonneg hd.le (by norm_num)) hu.le]
      exact (rpow_le_rpow (mul_nonneg (div_nonneg hd.le (by norm_num)) hu.le) ha hp.le).trans
        (powerPenalty_lower hp hw)
    have hsum : A*u^p ≤ ∑ j, powerPenalty p w ((x j-μ)*u+e) :=
      hA'.trans (Finset.single_le_sum (f := fun j ↦ powerPenalty p w ((x j-μ)*u+e))
        (fun _ _ ↦ penalty_nonneg hw) (Finset.mem_univ i))
    have hlog' : (n : ℝ)*log u ≤ (A/2)*u^p := by
      rw [Real.norm_eq_abs, Real.norm_eq_abs, abs_of_pos (rpow_pos_of_pos hu _)] at hl
      exact (le_abs_self _).trans hl
    change heldMeanSkew x μ p w u ≤ -A/2*u^p+C
    unfold heldMeanSkew C
    change (n : ℝ)*(log u-log (Gamma (1+1/p))) -
      (∑ j, powerPenalty p w ((x j-μ)*u+e)) ≤ _
    linarith

/-- At every fixed positive shape and positive finite skew, the held-mean
fit attains exactly when some residual from the held mean is nonzero. -/
theorem heldMeanSkew_attains_iff (hn : 0 < n) {x : Fin n → ℝ} {μ p w : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) :
    (∃ u ∈ Ioi (0 : ℝ), IsMaxOn (heldMeanSkew x μ p w) (Ioi 0) u) ↔
      ∃ i, x i ≠ μ := by
  let f := heldMeanSkew x μ p w
  constructor
  · rintro ⟨u,_,hm⟩
    by_contra! h
    have ht := (heldMeanSkew_collapse (p := p) (w := w) hn h).eventually
      (eventually_gt_atTop (f u))
    obtain ⟨v,hv,hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and ht).exists
    exact hgt.not_ge (hm hv)
  · intro hx
    have hN : (0 : ℝ) < n := by exact_mod_cast hn
    let G := log (Gamma (1+1/p))
    let a := min 1 (exp (f 1 / n + G))
    have ha : 0 < a := lt_min one_pos (exp_pos _)
    have ha1 : a ≤ 1 := min_le_left _ _
    have hsmall (u : ℝ) (hu : 0 < u) (hua : u ≤ a) : f u ≤ f 1 := by
      have hlog := log_le_log hu (hua.trans (min_le_right _ _))
      rw [log_exp] at hlog
      have hh := mul_le_mul_of_nonneg_left hlog hN.le
      have hsum : 0 ≤ ∑ i, powerPenalty p w ((x i-μ)*u+halfMoment p*(1-2*w)) :=
        Finset.sum_nonneg (fun _ _ ↦ penalty_nonneg hw)
      have he : (n : ℝ)*(f 1 / n) = f 1 := by field_simp
      change (n : ℝ)*(log u - G) - _ ≤ _
      nlinarith only [hsum,hh,he]
    obtain ⟨t,ht⟩ := eventually_atTop.mp ((heldMeanSkew_lossAtEnds hn hp hw hx).2.eventually
      (eventually_le_atBot (f 1)))
    let b := max 1 t
    have hb1 : 1 ≤ b := le_max_left _ _
    have hc : ContinuousOn f (Icc a b) :=
      fun u hu ↦ (heldMeanSkew_continuousAt hp (ha.trans_le hu.1)).continuousWithinAt
    have h1 : (1 : ℝ) ∈ Icc a b := ⟨ha1,hb1⟩
    obtain ⟨u,hu,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1,h1⟩ hc
    refine ⟨u,ha.trans_le hu.1,fun v hv ↦ ?_⟩
    by_cases h : v ∈ Icc a b
    · exact hm h
    · by_cases hva : a ≤ v
      · have h' : b ≤ v := le_of_not_ge (fun hvb ↦ h ⟨hva,hvb⟩)
        exact (ht v ((le_max_right _ _).trans h')).trans (hm h1)
      · exact (hsmall v hv (le_of_not_ge hva)).trans (hm h1)

private theorem hingePower_Jensen {p s u v a b : ℝ}
    (hp : 1 ≤ p) (hs : 0 < s) (ha : 0 ≤ a) (hb : 0 ≤ b) (hab : a+b = 1) :
    (max (a*u+b*v) 0 / s)^p ≤ a*(max u 0 / s)^p + b*(max v 0 / s)^p := by
  have hp' := zero_lt_one.trans_le hp
  have hx : 0 ≤ max u 0 / s := div_nonneg (le_max_right _ _) hs.le
  have hy : 0 ≤ max v 0 / s := div_nonneg (le_max_right _ _) hs.le
  have h : max (a*u+b*v) 0 / s ≤ a*(max u 0 / s)+b*(max v 0 / s) := by
    rw [← mul_div_assoc, ← mul_div_assoc, ← add_div]
    apply div_le_div_of_nonneg_right _ hs.le
    exact max_le (add_le_add (mul_le_mul_of_nonneg_left (le_max_left _ _) ha)
      (mul_le_mul_of_nonneg_left (le_max_left _ _) hb))
      (add_nonneg (mul_nonneg ha (le_max_right _ _)) (mul_nonneg hb (le_max_right _ _)))
  have hr := rpow_le_rpow (div_nonneg (le_max_right _ _) hs.le) h hp'.le
  rcases eq_or_lt_of_le hp with he | he
  · rw [← he, rpow_one, rpow_one, rpow_one]
    exact h
  · exact hr.trans (by
      simpa only [smul_eq_mul] using (strictConvexOn_rpow he).convexOn.2
        (show max u 0 / s ∈ Ici 0 from hx) (show max v 0 / s ∈ Ici 0 from hy) ha hb hab)

private theorem hingePower_strict {p s u v a b : ℝ}
    (hp : 1 < p) (hs : 0 < s) (ha : 0 < a) (hb : 0 < b) (hab : a+b = 1)
    (hne : max u 0 ≠ max v 0) :
    (max (a*u+b*v) 0 / s)^p < a*(max u 0 / s)^p + b*(max v 0 / s)^p := by
  have hx : 0 ≤ max u 0 / s := div_nonneg (le_max_right _ _) hs.le
  have hy : 0 ≤ max v 0 / s := div_nonneg (le_max_right _ _) hs.le
  have h : max (a*u+b*v) 0 / s ≤ a*(max u 0 / s)+b*(max v 0 / s) := by
    rw [← mul_div_assoc, ← mul_div_assoc, ← add_div]
    apply div_le_div_of_nonneg_right _ hs.le
    exact max_le (add_le_add (mul_le_mul_of_nonneg_left (le_max_left _ _) ha.le)
      (mul_le_mul_of_nonneg_left (le_max_left _ _) hb.le))
      (add_nonneg (mul_nonneg ha.le (le_max_right _ _)) (mul_nonneg hb.le (le_max_right _ _)))
  have hne' : max u 0 / s ≠ max v 0 / s := fun h ↦ hne ((div_left_inj₀ hs.ne').mp h)
  have hr := rpow_le_rpow (div_nonneg (le_max_right _ _) hs.le) h (zero_lt_one.trans hp).le
  exact hr.trans_lt (by
    simpa only [smul_eq_mul] using (strictConvexOn_rpow hp).2
      (show max u 0 / s ∈ Ici 0 from hx) (show max v 0 / s ∈ Ici 0 from hy) hne' ha hb hab)

private theorem powerPenalty_convex {p w : ℝ} (hp : 1 ≤ p) (hw : w ∈ Ioo (0 : ℝ) 1) :
    ConvexOn ℝ univ (powerPenalty p w) := by
  refine ⟨convex_univ,fun u _ v _ a b ha hb hab ↦ ?_⟩
  simp only [smul_eq_mul, powerPenalty_hinges (zero_lt_one.trans_le hp)]
  rw [show -(a*u+b*v) = a*(-u)+b*(-v) by ring]
  linarith [hingePower_Jensen hp hw.1 ha hb hab (u := -u) (v := -v),
    hingePower_Jensen hp (sub_pos.mpr hw.2) ha hb hab (u := u) (v := v)]

private theorem powerPenalty_strictConvex {p w : ℝ} (hp : 1 < p)
    (hw : w ∈ Ioo (0 : ℝ) 1) : StrictConvexOn ℝ univ (powerPenalty p w) := by
  refine ⟨convex_univ,fun u _ v _ huv a b ha hb hab ↦ ?_⟩
  simp only [smul_eq_mul, powerPenalty_hinges (zero_lt_one.trans hp)]
  rw [show -(a*u+b*v) = a*(-u)+b*(-v) by ring]
  by_cases h : max u 0 = max v 0
  · have hne : max (-u) 0 ≠ max (-v) 0 := by
      intro h'
      apply huv
      have he (t : ℝ) : max t 0 - max (-t) 0 = t := by
        rcases le_total t 0 with ht | ht
        · simp [max_eq_right ht, max_eq_left (neg_nonneg.mpr ht)]
        · simp [max_eq_left ht, max_eq_right (neg_nonpos.mpr ht)]
      linarith [he u,he v]
    linarith [hingePower_strict hp hw.1 ha hb hab hne,
      hingePower_Jensen hp.le (sub_pos.mpr hw.2) ha.le hb.le hab (u := u) (v := v)]
  · linarith [hingePower_Jensen hp.le hw.1 ha.le hb.le hab (u := -u) (v := -v),
      hingePower_strict hp (sub_pos.mpr hw.2) ha hb hab h]

/-- At shapes at least one the held-mean and held-skew maximum, when it
exists, is unique. -/
theorem heldMeanSkew_unique {x : Fin n → ℝ} (hn : 0 < n) {μ p w : ℝ}
    (hp : 1 ≤ p) (hw : w ∈ Ioo (0 : ℝ) 1) (hx : ∃ i, x i ≠ μ) :
    ∃! u, u ∈ Ioi (0 : ℝ) ∧ IsMaxOn (heldMeanSkew x μ p w) (Ioi 0) u := by
  let f := heldMeanSkew x μ p w
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let e := halfMoment p*(1-2*w)
  let r (i : Fin n) (u : ℝ) := (x i-μ)*u+e
  have hsc : StrictConcaveOn ℝ (Ioi 0) f := by
    refine ⟨convex_Ioi _,fun u hu v hv huv a b ha hb hab ↦ ?_⟩
    have hl := strictConcaveOn_log_Ioi.2 hu hv huv ha hb hab
    simp only [smul_eq_mul] at hl ⊢
    have hsum : ∑ i, powerPenalty p w (r i (a*u+b*v)) ≤
        a*(∑ i, powerPenalty p w (r i u)) + b*(∑ i, powerPenalty p w (r i v)) := by
      rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
      apply Finset.sum_le_sum
      intro i _
      have he : r i (a*u+b*v) = a*r i u+b*r i v := by unfold r; linear_combination -e*hab
      rw [he]
      simpa only [smul_eq_mul] using (powerPenalty_convex hp hw).2
        (mem_univ (r i u)) (mem_univ (r i v)) ha.le hb.le hab
    let G := log (Gamma (1+1/p))
    have hg : a*(n*G)+b*(n*G) = n*G := by linear_combination (n*G)*hab
    change a*((n : ℝ)*(log u-G)-∑ i, powerPenalty p w (r i u)) +
      b*((n : ℝ)*(log v-G)-∑ i, powerPenalty p w (r i v)) <
      (n : ℝ)*(log (a*u+b*v)-G)-∑ i, powerPenalty p w (r i (a*u+b*v))
    nlinarith only [mul_lt_mul_of_pos_left hl hN, hg, hsum]
  obtain ⟨u,hu,hm⟩ := (heldMeanSkew_attains_iff hn (zero_lt_one.trans_le hp) hw).mpr hx
  exact ⟨u,⟨hu,hm⟩,fun _ ht ↦ hsc.eq_of_isMaxOn ht.2 hm ht.1 hu⟩

/-- Held shape and skew with a free mode and positive total kernel width.
The omitted term is `-n log Γ(1+1/p)`, constant under these holds. -/
def fixedSkewLogLik (x : Fin n → ℝ) (p w : ℝ) (cs : ℝ × ℝ) : ℝ :=
  LocationScale.logLik (powerPenalty p w) x cs

private theorem powerPenalty_continuous {p : ℝ} (hp : 0 < p) (w : ℝ) :
    Continuous (powerPenalty p w) := by
  change Continuous (fun r ↦ powerPenalty p w r)
  simp_rw [powerPenalty_hinges hp]
  exact (((continuous_id.neg.max continuous_const).div_const w).rpow_const
    (fun _ ↦ Or.inr hp.le)).add
    (((continuous_id.max continuous_const).div_const (1-w)).rpow_const (fun _ ↦ Or.inr hp.le))

/-- At fixed positive shape, skew and sd, likelihood tends to zero at
both location ends, a maximizer always exists, and above shape one it
is unique. Fixing sd fixes total width in these coordinates. -/
theorem heldWidth (hn : 0 < n) (x : Fin n → ℝ) {p w s : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) :
    Tendsto (fun c ↦ fixedSkewLogLik x p w (c,s)) (cocompact ℝ) atBot ∧
      (∃ c, IsMaxOn (fun d ↦ fixedSkewLogLik x p w (d,s)) univ c) ∧
      (1 < p → ∃! c, IsMaxOn (fun d ↦ fixedSkewLogLik x p w (d,s)) univ c) := by
  let f := fun c : ℝ ↦ fixedSkewLogLik x p w (c,s)
  let i : Fin n := ⟨0,hn⟩
  let C := (n : ℝ)*(-log s)
  let r (j : Fin n) (d : ℝ) := (x j-d)/s
  have he (d : ℝ) : f d = C - ∑ j, powerPenalty p w (r j d) := by
    simp only [f,C,r,fixedSkewLogLik,LocationScale.logLik,Finset.sum_sub_distrib,Finset.sum_const,
      Finset.card_univ,Fintype.card_fin,nsmul_eq_mul]
  have hbound (d : ℝ) : f d ≤ C - |r i d|^p := by
    rw [he]
    apply sub_le_sub_left
    exact (powerPenalty_lower hp hw).trans (Finset.single_le_sum (f := fun j ↦
      powerPenalty p w (r j d)) (fun _ _ ↦ penalty_nonneg hw) (Finset.mem_univ i))
  have hpos : Tendsto (fun d : ℝ ↦ x i-d) atTop atBot := by
    simpa only [sub_eq_add_neg, add_comm (x i)] using
      tendsto_atBot_add_const_right atTop (x i) tendsto_neg_atTop_atBot
  have hneg : Tendsto (fun d : ℝ ↦ x i-d) atBot atTop := by
    simpa only [sub_eq_add_neg, add_comm (x i)] using
      tendsto_atTop_add_const_right atBot (x i) tendsto_neg_atBot_atTop
  have finish {F : Filter ℝ} (h : Tendsto (fun d : ℝ ↦ |x i-d|) F atTop) :
      Tendsto f F atBot := by
    have hr : Tendsto (fun d ↦ |r i d|) F atTop := by
      simpa only [r, abs_div, abs_of_pos hs] using h.atTop_div_const hs
    have ht := (tendsto_rpow_atTop hp).comp hr
    apply tendsto_atBot_mono hbound
    simpa only [sub_eq_add_neg, add_comm C, Function.comp_def] using
      tendsto_atBot_add_const_right F C (tendsto_neg_atTop_atBot.comp ht)
  have htop := finish (tendsto_abs_atBot_atTop.comp hpos)
  have hbot := finish (tendsto_abs_atTop_atTop.comp hneg)
  refine ⟨?_,?_⟩
  · rw [cocompact_eq_atBot_atTop]
    exact hbot.sup htop
  obtain ⟨l,hl⟩ := eventually_atBot.mp (hbot.eventually (eventually_le_atBot (f 0)))
  obtain ⟨h,hh⟩ := eventually_atTop.mp (htop.eventually (eventually_le_atBot (f 0)))
  let I := Icc (min l 0) (max h 0)
  have hI : (0 : ℝ) ∈ I := ⟨min_le_right _ _,le_max_right _ _⟩
  have hc : Continuous f := by
    have hterm (j : Fin n) : Continuous (fun d ↦ powerPenalty p w (r j d)) :=
      (powerPenalty_continuous hp w).comp (by unfold r; fun_prop)
    have hsum (S : Finset (Fin n)) : Continuous (fun d ↦ ∑ j ∈ S, powerPenalty p w (r j d)) := by
      classical
      induction S using Finset.induction_on with
      | empty => simpa only [Finset.sum_empty] using (continuous_const :
          Continuous (fun _ : ℝ ↦ (0 : ℝ)))
      | insert j S hj hS =>
        simp_rw [Finset.sum_insert hj]
        exact (hterm j).add hS
    rw [show f = (fun d ↦ C - ∑ j, powerPenalty p w (r j d)) from funext he]
    exact continuous_const.sub (hsum Finset.univ)
  obtain ⟨c,_,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨0,hI⟩ hc.continuousOn
  have hmax : IsMaxOn f univ c := by
    intro d _
    by_cases hd : d ∈ I
    · exact hm hd
    · apply le_trans _ (hm hI)
      by_cases hlow : min l 0 ≤ d
      · exact hh d ((le_max_left _ _).trans (le_of_not_ge (fun hu ↦ hd ⟨hlow,hu⟩)))
      · exact hl d ((le_of_not_ge hlow).trans (min_le_left _ _))
  refine ⟨⟨c,hmax⟩,fun hp' ↦ ⟨c,hmax,?_⟩⟩
  have hsc : StrictConcaveOn ℝ univ f := by
    refine ⟨convex_univ,fun a _ b _ hab t u ht hu htu ↦ ?_⟩
    simp only [smul_eq_mul,he]
    have hz (j : Fin n) : r j (t*a+u*b) = t*r j a+u*r j b := by
      unfold r
      rw [← mul_div_assoc,← mul_div_assoc,← add_div]
      congr 1
      linear_combination -x j * htu
    have hsum : ∑ j, powerPenalty p w (r j (t*a+u*b)) <
        t*(∑ j, powerPenalty p w (r j a))+u*(∑ j, powerPenalty p w (r j b)) := by
      rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
      apply Finset.sum_lt_sum_of_nonempty ⟨i,Finset.mem_univ _⟩
      intro j _
      rw [hz]
      have hrne : r j a ≠ r j b := by
        intro hEq
        have hh := (div_left_inj₀ hs.ne').mp hEq
        exact hab (by linarith)
      simpa only [smul_eq_mul] using (powerPenalty_strictConvex hp' hw).2
        (mem_univ (r j a)) (mem_univ (r j b)) hrne ht hu htu
    have hC : t*C+u*C=C := by linear_combination C*htu
    linarith
  intro d hd
  exact hsc.eq_of_isMaxOn hd hmax (mem_univ d) (mem_univ c)

private def weightedLoss (x : Fin n → ℝ) (p w c : ℝ) : ℝ :=
  ∑ i, powerPenalty p w (x i-c)

private theorem fixedSkew_profile {x : Fin n → ℝ} {p w s : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) (c : ℝ) :
    fixedSkewLogLik x p w (c,s) = -(n : ℝ)*log s - weightedLoss x p w c / s^p := by
  have hterm (i : Fin n) : powerPenalty p w ((x i-c)/s) = powerPenalty p w (x i-c) / s^p := by
    have hsign : (x i-c)/s < 0 ↔ x i-c < 0 := by rw [div_lt_iff₀ hs, zero_mul]
    unfold powerPenalty
    simp only [hsign, abs_div, abs_of_pos hs]
    have h (z a : ℝ) (ha : 0 < a) : (|z|/s/a)^p = (|z|/a)^p / s^p := by
      rw [div_div, mul_comm s a, ← div_div, div_rpow (div_nonneg (abs_nonneg _) ha.le) hs.le]
    split
    · exact h _ _ hw.1
    · exact h _ _ (sub_pos.mpr hw.2)
  unfold fixedSkewLogLik LocationScale.logLik weightedLoss
  simp only [hterm,Finset.sum_sub_distrib,Finset.sum_const,Finset.card_univ,Fintype.card_fin,
    nsmul_eq_mul,← Finset.sum_div]
  ring

private theorem weightedLoss_pos {x : Fin n → ℝ} {p w c : ℝ} (hp : 0 < p)
    (hw : w ∈ Ioo (0 : ℝ) 1) (hx : LocationScale.Nonconstant x) :
    0 < weightedLoss x p w c := by
  have hx' : ∃ i, x i ≠ c := by
    by_contra! he
    obtain ⟨i,j,hij⟩ := hx
    exact hij ((he i).trans (he j).symm)
  obtain ⟨i,hi⟩ := hx'
  exact ((rpow_pos_of_pos (abs_pos.mpr (sub_ne_zero.mpr hi)) p).trans_le
    (powerPenalty_lower hp hw)).trans_le
    (Finset.single_le_sum (f := fun j ↦ powerPenalty p w (x j-c))
      (fun _ _ ↦ penalty_nonneg hw) (Finset.mem_univ i))

private theorem scaleForLoss_max (hn : (0 : ℝ) < n) {p T : ℝ} (hp : 0 < p)
    (hT : 0 < T) :
    let s := (p*T/n)^p⁻¹
    0 < s ∧ ∀ u > 0, -(n : ℝ)*log u-T/u^p ≤ -(n : ℝ)*log s-T/s^p := by
  intro s
  have hbase := div_pos (mul_pos hp hT) hn
  have hs : 0 < s := rpow_pos_of_pos hbase _
  have hs' : s^p = p*T/n := rpow_inv_rpow hbase.le hp.ne'
  refine ⟨hs,fun u hu ↦ ?_⟩
  have hrel (v : ℝ) (hv : 0 < v) : T/v^p = (n/p)*(s/v)^p := by
    rw [div_rpow hs.le hv.le, hs']
    field_simp [(rpow_pos_of_pos hv p).ne', hp.ne', hn.ne']
  have hsize : T/s^p = n/p := by rw [hrel s hs,div_self hs.ne',one_rpow,mul_one]
  have h := log_le_sub_one_of_pos (rpow_pos_of_pos (div_pos hs hu) p)
  rw [log_rpow (div_pos hs hu),log_div hs.ne' hu.ne'] at h
  have hl := mul_le_mul_of_nonneg_left h (div_pos hn hp).le
  have hh : n/p*(p*(log s-log u)) = n*(log s-log u) := by field_simp
  rw [hh] at hl
  nlinarith only [hl,hrel u hu,hsize]

/-- Constant samples collapse at the fixed skew, with the mode set at
the common observation and total kernel width tending to zero. -/
theorem heldSkew_collapse (hn : 0 < n) {x : Fin n → ℝ} {p w c : ℝ}
    (hp : 0 < p) (hx : ∀ i, x i = c) :
    Tendsto (fun u ↦ fixedSkewLogLik x p w (c,1/u)) atTop atTop := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have h := (tendsto_const_mul_atTop_of_pos hN).2 tendsto_log_atTop
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with u hu
  unfold fixedSkewLogLik LocationScale.logLik powerPenalty
  simp only [hx,sub_self,zero_div,abs_zero,zero_rpow hp.ne',sub_zero,one_div,
    log_inv,neg_neg,Finset.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul]

/-- Fixed positive shape and skew leave a finite joint mode and scale
fit exactly on nonconstant samples. At shapes above one it is unique. -/
theorem heldSkew_criterion (hn : 0 < n) (x : Fin n → ℝ) {p w : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) :
    ((∃ cs ∈ LocationScale.domain, IsMaxOn (fixedSkewLogLik x p w) LocationScale.domain cs) ↔
      LocationScale.Nonconstant x) ∧
    (LocationScale.Nonconstant x → 1 < p →
      ∃! cs, cs ∈ LocationScale.domain ∧
        IsMaxOn (fixedSkewLogLik x p w) LocationScale.domain cs) := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  refine ⟨⟨?_,?_⟩,?_⟩
  · rintro ⟨cs,_,hm⟩
    by_contra hx
    let i : Fin n := ⟨0,hn⟩
    have hc (j : Fin n) : x j = x i := not_ne_iff.mp (fun h ↦ hx ⟨j,i,h⟩)
    have ht := (heldSkew_collapse (w := w) hn hp hc).eventually
      (eventually_gt_atTop (fixedSkewLogLik x p w cs))
    obtain ⟨u,hu,hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and ht).exists
    exact hgt.not_ge (hm (show (x i,1/u) ∈ LocationScale.domain from ⟨mem_univ _,one_div_pos.mpr hu⟩))
  · intro hx
    obtain ⟨c,hc⟩ := (heldWidth hn x hp hw (s := 1) one_pos).2.1
    have hmin (d : ℝ) : weightedLoss x p w c ≤ weightedLoss x p w d := by
      have hh : fixedSkewLogLik x p w (d,1) ≤ fixedSkewLogLik x p w (c,1) :=
        hc (mem_univ d)
      rw [fixedSkew_profile hw one_pos d,fixedSkew_profile hw one_pos c] at hh
      simpa only [log_one,mul_zero,one_rpow,div_one,zero_sub,neg_le_neg_iff] using hh
    let s := (p * weightedLoss x p w c / n)^p⁻¹
    have hs := scaleForLoss_max hN hp (weightedLoss_pos (c := c) hp hw hx)
    refine ⟨(c,s),⟨mem_univ _,hs.1⟩,fun q hq ↦ ?_⟩
    have ht : 0 < q.2 := hq.2
    have hbound := div_le_div_of_nonneg_right (hmin q.1) (rpow_pos_of_pos ht p).le
    change fixedSkewLogLik x p w (q.1,q.2) ≤ fixedSkewLogLik x p w (c,s)
    rw [fixedSkew_profile hw ht, fixedSkew_profile hw hs.1]
    exact (sub_le_sub_left hbound _).trans (hs.2 q.2 ht)
  · intro hx hp'
    have hl : LocationScale.LinearLowerBound (powerPenalty p w) := by
      refine ⟨1,one_pos,1,fun z ↦ ?_⟩
      have h := powerPenalty_lower (r := z) hp hw
      by_cases hz : 1 ≤ |z|
      · have hh : |z| ≤ |z|^p := by
          simpa only [rpow_one] using rpow_le_rpow_of_exponent_le hz hp'.le
        linarith
      · have hh := penalty_nonneg (r := z) (p := p) hw
        linarith
    exact LocationScale.existsUnique_mle (powerPenalty_strictConvex hp' hw) hl hx

/-- The two observations minus one and one, with unit skew held. -/
def symmetricPair (i : Fin 2) : ℝ := if i.val = 0 then -1 else 1

/-- At every shape between zero and one inclusive, this sample with unit
skew gives two maximizing locations at a common positive width. -/
theorem heldSkew_lowShapeTie {p : ℝ} (hp : 0 < p) (hp' : p ≤ 1) :
    ∃ s > 0, IsMaxOn (fixedSkewLogLik symmetricPair p (1/2)) LocationScale.domain (-1,s) ∧
      IsMaxOn (fixedSkewLogLik symmetricPair p (1/2)) LocationScale.domain (1,s) := by
  have he (r : ℝ) : powerPenalty p (1/2) r = (2*|r|)^p := by
    unfold powerPenalty
    rw [show 1-(1/2 : ℝ) = 1/2 by norm_num, ite_self]
    congr 1
    ring
  have hsum (d : ℝ) : weightedLoss symmetricPair p (1/2) d =
      (2*|(-1 : ℝ)-d|)^p+(2*|(1 : ℝ)-d|)^p := by
    norm_num [weightedLoss, symmetricPair, Fin.sum_univ_two, he]
  have hbound (d : ℝ) : (4 : ℝ)^p ≤ weightedLoss symmetricPair p (1/2) d := by
    rw [hsum]
    have he : (4 : ℝ) ≤ 2*|(-1 : ℝ)-d| + 2*|(1 : ℝ)-d| := by
      linarith [neg_le_abs ((-1 : ℝ)-d), le_abs_self ((1 : ℝ)-d)]
    exact (rpow_le_rpow (by norm_num) he hp.le).trans
      (rpow_add_le_add_rpow (by positivity) (by positivity) hp.le hp')
  have hminus : weightedLoss symmetricPair p (1/2) (-1) = (4 : ℝ)^p := by
    rw [hsum]
    norm_num [zero_rpow hp.ne']
  have hplus : weightedLoss symmetricPair p (1/2) 1 = (4 : ℝ)^p := by
    rw [hsum]
    norm_num [zero_rpow hp.ne']
  have hw : (1/2 : ℝ) ∈ Ioo 0 1 := ⟨by norm_num,by norm_num⟩
  have hn : (0 : ℝ) < (↑(2 : ℕ) : ℝ) := by norm_num
  have hs := scaleForLoss_max hn hp (rpow_pos_of_pos (show 0 < (4 : ℝ) by norm_num) p)
  let s := (p*(4 : ℝ)^p/2)^p⁻¹
  refine ⟨s,hs.1, ?_, ?_⟩
  all_goals
    intro q hq
    have hu : 0 < q.2 := hq.2
    have hh := div_le_div_of_nonneg_right (hbound q.1) (rpow_pos_of_pos hu p).le
    change fixedSkewLogLik symmetricPair p (1/2) (q.1,q.2) ≤ _
    rw [fixedSkew_profile hw hu, fixedSkew_profile hw (show 0 < s from hs.1)]
    first | rw [hminus] | rw [hplus]
    exact (sub_le_sub_left hh _).trans (hs.2 q.2 hu)

end

end UnivariateML.TwoPieceSGED

end
