module

public import UnivariateML.Family.TDist
public import Mathlib.Algebra.Polynomial.Roots
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Two-piece Student collapse at fixed shape and skew

Use inverse total denominator width `u`, so the side denominators are
`w/u` and `(1-w)/u`. At concentration point `y`, the mode follows
`y-e/u`. The offset is zero on the exact-join path and the half-kernel
mean times `1-2*w` on the held-mean path.

## Deviations

The expansion concerns these fixed-offset paths. Optimization over a
moving mode or skew and the simultaneous shape limits require additional
uniform bounds.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TwoPieceStudent

noncomputable section

variable {n : ℕ}

/-- Mean of the half-kernel in Student denominator units. -/
def halfMoment (ν : ℝ) : ℝ :=
  Gamma ((ν-1)/2) / (sqrt π * Gamma (ν/2))

/-- Log of the Student normalizer for total denominator width one. -/
def logPeak (ν : ℝ) : ℝ :=
  log 2 + TDist.logNormalizer ν + log ν / 2

/-- Width fraction used on the observed side. A join observation has
zero residual and its side choice has no effect. -/
def side (w r : ℝ) : ℝ := if r < 0 then w else 1-w

/-- Log likelihood on the path whose mode is `y-e/u`. -/
def logAlongAtom (x : Fin n → ℝ) (y ν w e u : ℝ) : ℝ :=
  (n : ℝ) * (logPeak ν + log u) -
    (ν+1)/2 * ∑ i, log (1 + (((x i-y)*u+e) / side w ((x i-y)*u+e))^2)

/-- Critical log height after removing the multiplicity exponent. -/
def collapseLogHeight (x : Fin n → ℝ) (y ν w e : ℝ) : ℝ :=
  (n : ℝ) * logPeak ν - (ν+1)/2 * ∑ i,
    if x i = y then log (1 + (e / side w e)^2)
    else log (((x i-y) / side w (x i-y))^2)

private theorem side_pos {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) (r : ℝ) :
    0 < side w r := by
  unfold side
  split
  · exact hw.1
  · exact sub_pos.mpr hw.2

private theorem logKernel_scaled_limit {d w : ℝ} (hd : d ≠ 0)
    (hw : w ∈ Ioo (0 : ℝ) 1) (e : ℝ) :
    Tendsto (fun u : ℝ ↦ log (1 + ((d*u+e) / side w (d*u+e))^2) - 2*log u)
      atTop (𝓝 (log ((d / side w d)^2))) := by
  let F (u : ℝ) := d+e/u
  let s := side w d
  have hs : 0 < s := side_pos hw d
  have hF : Tendsto F atTop (𝓝 d) := by
    have h := (tendsto_const_nhds (x := d)).add (tendsto_inv_atTop_zero.const_mul e)
    simpa only [F, div_eq_mul_inv, mul_zero, add_zero] using h
  have he (u : ℝ) (hu : 0 < u) : d*u+e = F u*u := by
    unfold F
    field_simp
  have hw' : ∀ᶠ u : ℝ in atTop, side w (d*u+e) = s := by
    rcases lt_or_gt_of_ne hd with h | h
    · filter_upwards [eventually_gt_atTop (0 : ℝ), hF.eventually (gt_mem_nhds h)] with u hu hlu
      have hn : d*u+e < 0 := by rw [he u hu]; exact mul_neg_of_neg_of_pos hlu hu
      simp only [s,side,ite_eq_left h,ite_eq_left hn]
    · filter_upwards [eventually_gt_atTop (0 : ℝ), hF.eventually (lt_mem_nhds h)] with u hu hlu
      have hp : 0 < d*u+e := by rw [he u hu]; exact mul_pos hlu hu
      simp only [s,side,ite_eq_right h.not_gt,ite_eq_right hp.not_gt]
  have hg : Tendsto (fun u : ℝ ↦ u⁻¹^2 + (F u/s)^2) atTop (𝓝 ((d/s)^2)) := by
    simpa only [zero_pow (by decide : (2 : ℕ) ≠ 0), zero_add] using
      (tendsto_inv_atTop_zero.pow 2).add ((hF.div_const s).pow 2)
  have hpos : 0 < (d/s)^2 := sq_pos_of_ne_zero (div_ne_zero hd hs.ne')
  have hl := (continuousAt_log hpos.ne').tendsto.comp hg
  apply hl.congr'
  filter_upwards [hw',eventually_gt_atTop (0 : ℝ)] with u huw hu
  change log (u⁻¹^2 + (F u/s)^2) = log (1+((d*u+e)/side w (d*u+e))^2)-2*log u
  have hG : 0 < u⁻¹^2 + (F u/s)^2 :=
    add_pos_of_pos_of_nonneg (pow_pos (inv_pos.mpr hu) _) (sq_nonneg _)
  have hEq : 1+((d*u+e)/side w (d*u+e))^2 = u^2*(u⁻¹^2 + (F u/s)^2) := by
    rw [huw, he u hu]
    field_simp [hs.ne',hu.ne']
  rw [hEq,log_mul (pow_ne_zero 2 hu.ne') hG.ne',log_pow]
  push_cast
  ring

private theorem atom_count (x : Fin n → ℝ) (y ν : ℝ) :
    (n : ℝ) + TDist.exponent x y ν =
      (ν+1) * ((Finset.univ.filter (fun i ↦ x i ≠ y)).card : ℝ) := by
  have hn : TDist.multiplicity x y + (Finset.univ.filter (fun i ↦ x i ≠ y)).card = n := by
    unfold TDist.multiplicity
    rw [Finset.card_filter_add_card_filter_not, Finset.card_univ, Fintype.card_fin]
  have h' : (TDist.multiplicity x y : ℝ) +
      ((Finset.univ.filter (fun i ↦ x i ≠ y)).card : ℝ) = n := by exact_mod_cast hn
  unfold TDist.exponent
  nlinarith only [h', congrArg (ν * ·) h']

/-- The log likelihood after removal of the width exponent tends to the
sharp constant along the specified fixed-shape, fixed-skew path. This
includes no observations at the atom, any side count zero, and all ties. -/
theorem collapse_expansion (x : Fin n → ℝ) (y ν w e : ℝ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    Tendsto (fun u ↦ logAlongAtom x y ν w e u + TDist.exponent x y ν * log u)
      atTop (𝓝 (collapseLogHeight x y ν w e)) := by
  let g (i : Fin n) (u : ℝ) :=
    log (1+(((x i-y)*u+e)/side w ((x i-y)*u+e))^2) - (if x i = y then 0 else 2*log u)
  let G (i : Fin n) := if x i = y then log (1+(e/side w e)^2)
    else log (((x i-y)/side w (x i-y))^2)
  have ht (i : Fin n) : Tendsto (g i) atTop (𝓝 (G i)) := by
    by_cases h : x i = y
    · simpa only [g,G,h,ite_eq_left,sub_self,zero_mul,zero_add,sub_zero] using
        (tendsto_const_nhds (x := log (1+(e/side w e)^2)) (f := atTop))
    · simpa only [g,G,ite_eq_right h] using
        logKernel_scaled_limit (sub_ne_zero.mpr h) hw e
  have hs (s : Finset (Fin n)) : Tendsto (fun u ↦ ∑ i ∈ s, g i u) atTop (𝓝 (∑ i ∈ s, G i)) := by
    classical
    induction s using Finset.induction_on with
    | empty => simpa only [Finset.sum_empty] using
        (tendsto_const_nhds (x := (0 : ℝ)) (f := (atTop : Filter ℝ)))
    | insert i s hi hS =>
      simp_rw [Finset.sum_insert hi]
      exact (ht i).add hS
  have h := (tendsto_const_nhds (x := (n : ℝ)*logPeak ν) (f := (atTop : Filter ℝ))).sub
    ((hs Finset.univ).const_mul ((ν+1)/2))
  change Tendsto (fun u ↦ (n : ℝ)*logPeak ν - (ν+1)/2*∑ i, g i u)
    atTop (𝓝 (collapseLogHeight x y ν w e)) at h
  apply h.congr
  intro u
  have hc : ∑ i : Fin n, (if x i = y then (0 : ℝ) else 2*log u) =
      ((Finset.univ.filter (fun i ↦ x i ≠ y)).card : ℝ)*(2*log u) := by
    rw [← nsmul_eq_mul, ← Finset.sum_const]
    rw [Finset.sum_filter]
    apply Finset.sum_congr rfl
    intro i _
    by_cases h : x i = y <;> simp [h]
  unfold logAlongAtom
  simp only [g, Finset.sum_sub_distrib, hc]
  have he := atom_count x y ν
  nlinarith only [congrArg (fun z : ℝ ↦ z*log u) he]

/-- The sign of the collapse exponent classifies the fixed-width-fraction
path at either specified offset. At equality the product likelihood has
the stated strictly positive limit. -/
theorem collapse_cases (x : Fin n → ℝ) (y ν w e : ℝ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    let E := TDist.exponent x y ν
    let f := logAlongAtom x y ν w e
    (E < 0 → Tendsto f atTop atTop) ∧
    (0 < E → Tendsto f atTop atBot) ∧
    (E = 0 → Tendsto f atTop (𝓝 (collapseLogHeight x y ν w e)) ∧
      Tendsto (fun u ↦ exp (f u)) atTop (𝓝 (exp (collapseLogHeight x y ν w e)))) := by
  intro E f
  let g (u : ℝ) := f u + E * log u
  have hg : Tendsto g atTop (𝓝 (collapseLogHeight x y ν w e)) :=
    collapse_expansion x y ν w e hw
  have heq (u : ℝ) : -E * log u + g u = f u := by unfold g; ring
  refine ⟨?_,?_,?_⟩
  · intro he
    have h := ((tendsto_const_mul_atTop_of_pos (neg_pos.mpr he)).2 tendsto_log_atTop).atTop_add hg
    exact h.congr heq
  · intro he
    have h := (tendsto_log_atTop.const_mul_atTop_of_neg (neg_lt_zero.mpr he)).atBot_add hg
    exact h.congr heq
  · intro he
    have h : Tendsto f atTop (𝓝 (collapseLogHeight x y ν w e)) := by
      simpa only [g,he,zero_mul,add_zero] using hg
    exact ⟨h,continuous_exp.continuousAt.tendsto.comp h⟩

/-- Infinite total width sends the log likelihood to minus infinity
for every nonempty sample under fixed shape and skew. -/
theorem infiniteWidth_loses (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hν : 2 < ν) : Tendsto (logAlongAtom x y ν w e) (𝓝[>] 0) atBot := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have ht := tendsto_atBot_add_const_right (𝓝[>] (0 : ℝ)) ((n : ℝ)*logPeak ν)
    ((tendsto_const_mul_atBot_of_pos hN).2 tendsto_log_nhdsGT_zero)
  apply tendsto_atBot_mono _ ht
  intro u
  have hs : 0 ≤ ∑ i, log (1+(((x i-y)*u+e)/side w ((x i-y)*u+e))^2) :=
    Finset.sum_nonneg (fun _ _ ↦ log_nonneg (le_add_of_nonneg_right (sq_nonneg _)))
  have hc : 0 ≤ (ν+1)/2 := by linarith
  change logAlongAtom x y ν w e u ≤ n*log u + n*logPeak ν
  unfold logAlongAtom
  nlinarith only [mul_nonneg hc hs]

private theorem hasDerivAt_oneSide {s : ℝ} (hs : 0 < s) (r : ℝ) :
    HasDerivAt (fun z : ℝ ↦ log (1+(z/s)^2)) (2*r/(s^2+r^2)) r := by
  have h₀ := (((hasDerivAt_id r).div_const s).pow 2).const_add (1 : ℝ)
  have h₁ : 1 + ((fun x : ℝ ↦ id x / s)^2) r ≠ 0 := by
    change 1 + (r/s)^2 ≠ 0
    positivity
  have h := h₀.log h₁
  convert h using 1
  · rfl
  · simp only [Pi.pow_apply,id_eq,Nat.reduceSub,pow_one,Nat.cast_ofNat]
    have hh : s^2+r^2 ≠ 0 := (add_pos_of_pos_of_nonneg (pow_pos hs _) (sq_nonneg _)).ne'
    have hh' : 1+(r/s)^2 ≠ 0 := by positivity
    field_simp [hs.ne',hh,hh']

private theorem hasDerivAt_side {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) (r : ℝ) :
    HasDerivAt (fun z : ℝ ↦ log (1+(z/side w z)^2)) (2*r/((side w r)^2+r^2)) r := by
  rcases lt_trichotomy r 0 with hr | rfl | hr
  · rw [show side w r = w from ite_eq_left hr]
    apply (hasDerivAt_oneSide hw.1 r).congr_of_eventuallyEq
    filter_upwards [gt_mem_nhds hr] with z hz
    rw [show side w z = w from ite_eq_left hz]
  · have hL : HasDerivWithinAt (fun z : ℝ ↦ log (1+(z/side w z)^2)) 0 (Iic 0) 0 := by
      have hh := (hasDerivAt_oneSide hw.1 0).hasDerivWithinAt (s := Iic (0 : ℝ))
      simp only [mul_zero, zero_div] at hh
      apply hh.congr_of_mem _ (by simp)
      intro z hz
      rcases lt_or_eq_of_le hz with h | rfl
      · rw [show side w z = w from ite_eq_left h]
      · simp
    have hR : HasDerivWithinAt (fun z : ℝ ↦ log (1+(z/side w z)^2)) 0 (Ici 0) 0 := by
      have hh := (hasDerivAt_oneSide (sub_pos.mpr hw.2) 0).hasDerivWithinAt (s := Ici (0 : ℝ))
      simp only [mul_zero,zero_div] at hh
      apply hh.congr_of_mem _ (by simp)
      intro z hz
      rw [show side w z = 1-w from ite_eq_right (not_lt_of_ge hz)]
    simpa only [Iic_union_Ici,hasDerivWithinAt_univ,mul_zero,zero_div] using hL.union hR
  · rw [show side w r = 1-w from ite_eq_right hr.not_gt]
    apply (hasDerivAt_oneSide (sub_pos.mpr hw.2) r).congr_of_eventuallyEq
    filter_upwards [lt_mem_nhds hr] with z hz
    rw [show side w z = 1-w from ite_eq_right hz.not_gt]

/-- Score in inverse total denominator width. Its zeros agree with the
zeros of the log-width score on the positive-width domain. -/
def scaleScore (x : Fin n → ℝ) (y ν w e u : ℝ) : ℝ :=
  n/u - (ν+1)/2 * ∑ i,
    let r := (x i-y)*u+e
    2*r/((side w r)^2+r^2)*(x i-y)

/-- The score remains valid at observations crossing the moving mode. -/
theorem hasDerivAt_scale (x : Fin n → ℝ) (y ν w e : ℝ)
    (hw : w ∈ Ioo (0 : ℝ) 1) {u : ℝ} (hu : 0 < u) :
    HasDerivAt (logAlongAtom x y ν w e) (scaleScore x y ν w e u) u := by
  let R (i : Fin n) (u : ℝ) := (x i-y)*u+e
  have hterm (i : Fin n) :=
    (hasDerivAt_side hw (R i u)).comp u (((hasDerivAt_id u).const_mul (x i-y)).add_const e)
  have ht := HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ hterm i)
  have hl := ((hasDerivAt_log hu.ne').const_add (logPeak ν)).const_mul (n : ℝ)
  convert hl.sub (ht.const_mul ((ν+1)/2)) using 1
  · funext v
    simp only [logAlongAtom,Function.comp_def,R,Pi.sub_apply]
  · unfold scaleScore
    simp only [R,mul_one,div_eq_mul_inv]

private theorem ray_attains {f : ℝ → ℝ} (hc : ContinuousOn f (Ioi 0))
    (h0 : Tendsto f (𝓝[>] 0) atBot) {s : ℝ} (hs : 0 < s)
    (hEnd : ∀ᶠ u in atTop, f u ≤ f s) :
    ∃ u ∈ Ioi (0 : ℝ), IsMaxOn f (Ioi 0) u := by
  have hnear := h0.eventually (eventually_le_atBot (f s))
  rw [nhdsWithin, eventually_inf_principal] at hnear
  obtain ⟨ε,hε,hε'⟩ := Metric.eventually_nhds_iff.mp hnear
  obtain ⟨r,hr⟩ := eventually_atTop.mp hEnd
  let lo := min s (ε/2)
  let hi := max s r
  have hlo : 0 < lo := lt_min hs (half_pos hε)
  have hI : s ∈ Icc lo hi := ⟨min_le_left _ _,le_max_left _ _⟩
  have hsub : ∀ u ∈ Icc lo hi, u ∈ Ioi (0 : ℝ) :=
    fun _ hu ↦ hlo.trans_le hu.1
  obtain ⟨u,hu,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨s,hI⟩ (hc.mono hsub)
  refine ⟨u,hsub u hu,fun v hv ↦ ?_⟩
  by_cases hl : lo ≤ v
  · by_cases hh : v ≤ hi
    · exact hm (show v ∈ Icc lo hi from ⟨hl,hh⟩)
    · exact (hr v ((le_max_right _ _).trans (le_of_not_ge hh))).trans (hm hI)
  · have hlt : v < lo := lt_of_not_ge hl
    have hv' : 0 < v := hv
    have hd : dist v 0 < ε := by
      rw [Real.dist_eq,sub_zero,abs_of_pos hv']
      exact (hlt.trans_le (min_le_right _ _)).trans (half_lt_self hε)
    exact (hε' hd hv).trans (hm hI)

/-- The collapse test at an inverse width compares the log likelihood
against the finite collapse height when the exponent vanishes. -/
def meetsCollapse (x : Fin n → ℝ) (y ν w e u : ℝ) : Prop :=
  0 < TDist.exponent x y ν ∨
    TDist.exponent x y ν = 0 ∧ collapseLogHeight x y ν w e ≤ logAlongAtom x y ν w e u

private theorem score_of_max {x : Fin n → ℝ} {y ν w e u : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hu : 0 < u)
    (hm : IsMaxOn (logAlongAtom x y ν w e) (Ioi 0) u) :
    scaleScore x y ν w e u = 0 := by
  have h := (hm.isLocalMax (Ioi_mem_nhds hu)).deriv_eq_zero
  rwa [(hasDerivAt_scale x y ν w e hw hu).deriv] at h

private theorem meetsCollapse_of_max {x : Fin n → ℝ} {y ν w e u : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1)
    (hm : IsMaxOn (logAlongAtom x y ν w e) (Ioi 0) u) :
    meetsCollapse x y ν w e u := by
  let E := TDist.exponent x y ν
  let f := logAlongAtom x y ν w e
  have hh := collapse_cases x y ν w e hw
  rcases lt_trichotomy E 0 with h | h | h
  · have ht := (hh.1 h).eventually (eventually_gt_atTop (f u))
    obtain ⟨v,hv,hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and ht).exists
    exact (hgt.not_ge (hm hv)).elim
  · exact Or.inr ⟨h, le_of_tendsto ((hh.2.2 h).1)
      ((eventually_gt_atTop (0 : ℝ)).mono (fun v hv ↦ hm hv))⟩
  · exact Or.inl h

private theorem exists_of_meetsCollapse (hn : 0 < n) {x : Fin n → ℝ}
    {y ν w e u : ℝ} (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) (hu : 0 < u)
    (h : meetsCollapse x y ν w e u) :
    ∃ v ∈ Ioi (0 : ℝ), IsMaxOn (logAlongAtom x y ν w e) (Ioi 0) v := by
  let f := logAlongAtom x y ν w e
  let C := collapseLogHeight x y ν w e
  have hc : ContinuousOn f (Ioi 0) := fun v hv ↦
    (hasDerivAt_scale x y ν w e hw hv).continuousAt.continuousWithinAt
  have h0 := infiniteWidth_loses hn x y ν w e hν
  have hh := collapse_cases x y ν w e hw
  rcases h with h | ⟨hE,hcu⟩
  · exact ray_attains hc h0 hu ((hh.2.1 h).eventually (eventually_le_atBot (f u)))
  · by_cases hwin : ∃ v > 0, C < f v
    · obtain ⟨v,hv,hcv⟩ := hwin
      exact ray_attains hc h0 hv <| (((hh.2.2 hE).1).eventually (gt_mem_nhds hcv)).mono
        (fun _ h ↦ h.le)
    · have hv : ∀ v > 0, f v ≤ C := by
        intro v hv
        exact le_of_not_gt (fun ht ↦ hwin ⟨v,hv,ht⟩)
      exact ⟨u,hu,fun v hv' ↦ (hv v hv').trans hcu⟩

/-- The held-mean, held-shape, held-skew fit exists exactly when a finite
score zero reaches the collapse envelope. For a general fixed offset
this is the same criterion on the resulting inverse-width ray. -/
theorem fixedOffset_criterion (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) :
    (∃ u ∈ Ioi (0 : ℝ), IsMaxOn (logAlongAtom x y ν w e) (Ioi 0) u) ↔
      ∃ u > 0, scaleScore x y ν w e u = 0 ∧ meetsCollapse x y ν w e u := by
  constructor
  · rintro ⟨u,hu,hm⟩
    exact ⟨u,hu,score_of_max hw hu hm,meetsCollapse_of_max hw hm⟩
  · rintro ⟨u,hu,_,hm⟩
    exact exists_of_meetsCollapse hn hν hw hu hm

/-- All maximizing scales are the stationary points that meet the
collapse envelope and tie for the greatest stationary value. -/
theorem fixedOffset_maximizers (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) {u : ℝ} (hu : 0 < u) :
    IsMaxOn (logAlongAtom x y ν w e) (Ioi 0) u ↔
      scaleScore x y ν w e u = 0 ∧ meetsCollapse x y ν w e u ∧
      ∀ v > 0, scaleScore x y ν w e v = 0 →
        logAlongAtom x y ν w e v ≤ logAlongAtom x y ν w e u := by
  constructor
  · intro h
    exact ⟨score_of_max hw hu h, meetsCollapse_of_max hw h, fun _ hv _ ↦ h hv⟩
  · rintro ⟨_,h,hm⟩
    obtain ⟨v,hv,hmax⟩ := exists_of_meetsCollapse hn hν hw hu h
    exact fun r hr ↦ (hmax hr).trans (hm v hv (score_of_max hw hv hmax))

/-- The inverse-width score has only finitely many positive roots.
Clearing denominators on every sign assignment gives a polynomial
with nonzero constant term, including assignments with zero side counts. -/
theorem scaleScore_roots_finite (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    {u : ℝ | 0 < u ∧ scaleScore x y ν w e u = 0}.Finite := by
  classical
  let d (i : Fin n) := x i-y
  let B (h : Fin n → Bool) (i : Fin n) : ℝ := if h i then w else 1-w
  let R (i : Fin n) : Polynomial ℝ := Polynomial.C (d i) * Polynomial.X + Polynomial.C e
  let D (h : Fin n → Bool) (i : Fin n) : Polynomial ℝ :=
    Polynomial.C (B h i)^2 + R i^2
  let P (h : Fin n → Bool) : Polynomial ℝ :=
    Polynomial.C (n : ℝ) * (∏ i, D h i) -
      Polynomial.C ((ν+1)/2) * Polynomial.X *
        (∑ i, Polynomial.C (2*d i) * R i *
          ∏ j ∈ Finset.univ.erase i, D h j)
  have hB (h : Fin n → Bool) (i : Fin n) : 0 < B h i := by
    unfold B
    split
    · exact hw.1
    · exact sub_pos.mpr hw.2
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hP (h : Fin n → Bool) : P h ≠ 0 := by
    have h0 : (P h).eval 0 = (n : ℝ)*∏ i, ((B h i)^2+e^2) := by
      simp [P,D,R,Polynomial.eval_prod]
    have hp : 0 < (P h).eval 0 := by
      rw [h0]
      exact mul_pos hn' (Finset.prod_pos (fun i _ ↦
        add_pos_of_pos_of_nonneg (pow_pos (hB h i) _) (sq_nonneg _)))
    intro he
    rw [he,Polynomial.eval_zero] at hp
    exact (lt_irrefl _ hp)
  have hF : (⋃ h : Fin n → Bool, {u : ℝ | (P h).IsRoot u}).Finite :=
    Set.finite_iUnion (fun h ↦ Polynomial.finite_setOfPred_isRoot (hP h))
  apply hF.subset
  rintro u ⟨hu,hscore⟩
  let r (i : Fin n) := d i*u+e
  let h (i : Fin n) := decide (r i < 0)
  let A (i : Fin n) := (side w (r i))^2+(r i)^2
  have hE (i : Fin n) : (D h i).eval u = A i := by simp [D,R,A,r,B,h,side]
  have hA (i : Fin n) : 0 < A i :=
    add_pos_of_pos_of_nonneg (pow_pos (side_pos hw (r i)) _) (sq_nonneg _)
  have hS :
      (∑ i, 2*r i/A i*d i) * (∏ i, A i) =
        ∑ i, 2*d i*r i * (∏ j ∈ Finset.univ.erase i, A j) := by
    rw [Finset.sum_mul]
    apply Finset.sum_congr rfl
    intro i _
    rw [← Finset.prod_erase_mul Finset.univ A (Finset.mem_univ i)]
    field_simp [(hA i).ne']
  have hEq : (P h).eval u =
      (n : ℝ)*(∏ i, A i) - ((ν+1)/2)*u*
        (∑ i, 2*d i*r i*(∏ j ∈ Finset.univ.erase i, A j)) := by
    simp only [P, Polynomial.eval_sub, Polynomial.eval_mul, Polynomial.eval_C, Polynomial.eval_X,
      Polynomial.eval_prod, Polynomial.eval_finsetSum, hE, R, r, Polynomial.eval_add]
  change (n : ℝ)/u - (ν+1)/2*(∑ i, 2*r i/A i*d i) = 0 at hscore
  have hscore' : (n : ℝ) = u*((ν+1)/2)*(∑ i, 2*r i/A i*d i) := by
    exact (div_eq_iff hu.ne').mp (sub_eq_zero.mp hscore) |>.trans (by ring)
  refine mem_iUnion_of_mem h ?_
  change (P h).eval u = 0
  rw [hEq, ← hS]
  calc
    _ = ((n : ℝ)-u*((ν+1)/2)*(∑ i,2*r i/A i*d i))*(∏ i,A i) := by ring
    _ = 0 := by rw [← hscore',sub_self,zero_mul]

/-- At critical multiplicity on the exact-join path, the likelihood
strictly increases as width shrinks and its finite supremum is unattained.
Unit skew makes this the held-mean path as well. -/
theorem exactJoin_critical (hn : 0 < n) (x : Fin n → ℝ) (y ν w : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) (hE : TDist.exponent x y ν = 0) :
    let f := logAlongAtom x y ν w 0
    let C := collapseLogHeight x y ν w 0
    StrictMonoOn f (Ioi 0) ∧
      IsLUB (f '' Ioi 0) C ∧ ∀ u > 0, f u < C := by
  intro f C
  let off := Finset.univ.filter (fun i : Fin n ↦ x i ≠ y)
  have hcount : (n : ℝ) = (ν+1)*(off.card : ℝ) := by
    simpa only [hE,add_zero] using atom_count x y ν
  have hne : off.Nonempty := by
    by_contra h
    rw [Finset.not_nonempty_iff_eq_empty.mp h] at hcount
    simp only [Finset.card_empty,Nat.cast_zero,mul_zero] at hcount
    exact (Nat.ne_of_gt hn) (by exact_mod_cast hcount)
  have hp : 0 < ν+1 := by linarith
  have hscore (u : ℝ) (hu : 0 < u) : 0 < scaleScore x y ν w 0 u := by
    let r (i : Fin n) := (x i-y)*u
    let D (i : Fin n) := (side w (r i))^2+(r i)^2
    let t (i : Fin n) := (r i)^2 / D i
    have hD (i : Fin n) : 0 < D i :=
      add_pos_of_pos_of_nonneg (pow_pos (side_pos hw (r i)) _) (sq_nonneg _)
    have ht (i : Fin n) : t i < 1 :=
      (div_lt_one (hD i)).mpr (by
        unfold D
        linarith [pow_pos (side_pos hw (r i)) 2])
    have ht0 (i : Fin n) (hi : x i = y) : t i = 0 := by simp [t,r,hi]
    have hsum : ∑ i, t i < (off.card : ℝ) := by
      have he : ∑ i ∈ off, t i = ∑ i, t i :=
        Finset.sum_filter_of_ne (fun i _ h ↦ fun hh ↦ h (ht0 i hh))
      rw [← he]
      simpa only [Finset.sum_const, nsmul_eq_mul, mul_one] using
        Finset.sum_lt_sum_of_nonempty hne (fun i _ ↦ ht i)
    have hterm : (∑ i, 2*r i/D i*(x i-y))*u = 2*∑ i, t i := by
      rw [Finset.sum_mul,Finset.mul_sum]
      apply Finset.sum_congr rfl
      intro i _
      unfold t r
      ring
    have hpos := mul_lt_mul_of_pos_left hsum hp
    apply pos_of_mul_pos_left _ hu.le
    unfold scaleScore
    simp only [add_zero]
    change 0 < ((n : ℝ)/u-(ν+1)/2*(∑ i,2*r i/D i*(x i-y)))*u
    rw [sub_mul,div_mul_cancel₀ _ hu.ne',mul_assoc,hterm]
    nlinarith only [hpos,hcount]
  have hmono : StrictMonoOn f (Ioi 0) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioi _)
    · intro u hu
      exact (hasDerivAt_scale x y ν w 0 hw hu).continuousAt.continuousWithinAt
    · intro u hu
      have hu' : 0 < u := by simpa only [interior_Ioi,mem_Ioi] using hu
      rw [(hasDerivAt_scale x y ν w 0 hw hu').deriv]
      exact hscore u hu'
  have hlim : Tendsto f atTop (𝓝 C) := ((collapse_cases x y ν w 0 hw).2.2 hE).1
  have hub (u : ℝ) (hu : 0 < u) : f u ≤ C := by
    apply ge_of_tendsto hlim
    filter_upwards [eventually_ge_atTop u] with t ht
    exact hmono.monotoneOn hu (hu.trans_le ht) ht
  refine ⟨hmono,⟨?_,?_⟩,?_⟩
  · rintro _ ⟨u,hu,rfl⟩
    exact hub u hu
  · intro L hL
    exact le_of_tendsto hlim ((eventually_gt_atTop (0 : ℝ)).mono
      (fun u hu ↦ hL (mem_image_of_mem f hu)))
  · intro u hu
    have h : u < u+1 := by linarith
    exact (hmono hu (hu.trans h) h).trans_le (hub _ (hu.trans h))

/-- A moving join and even a vanishing side cannot increase the power
of fixed-shape collapse. The width fractions may vary arbitrarily while
positive, and the concentration point may be any finite real number. -/
theorem movingCollapse_upper {ι : Type*} {l : Filter ι} (x : Fin n → ℝ) (y ν : ℝ)
    (hν : 2 < ν) {u w e : ι → ℝ}
    (hu : Tendsto u l atTop) (hw : ∀ᶠ t in l, w t ∈ Ioo (0 : ℝ) 1)
    (he : Tendsto (fun t ↦ e t/u t) l (𝓝 (0 : ℝ))) :
    ∃ C : ℝ, ∀ᶠ t in l, logAlongAtom x y ν (w t) (e t) (u t) ≤
      -TDist.exponent x y ν * log (u t) + C := by
  let off := Finset.univ.filter (fun i : Fin n ↦ x i ≠ y)
  let d (i : Fin n) := x i-y
  let C := (n : ℝ) * logPeak ν - (ν+1)/2 * ∑ i ∈ off, 2*log (|d i|/2)
  let R (t : ι) (i : Fin n) := d i*u t+e t
  let G (t : ι) (i : Fin n) := log (1+(R t i/side (w t) (R t i))^2)
  have hd (i : Fin n) (hi : i ∈ off) : 0 < |d i|/2 :=
    half_pos (abs_pos.mpr (sub_ne_zero.mpr (Finset.mem_filter.mp hi).2))
  have hsep : ∀ᶠ t in l, ∀ i ∈ off, |e t/u t| < |d i|/2 := by
    rw [eventually_all_finset]
    intro i hi
    have ht := he.abs
    rw [abs_zero] at ht
    exact ht.eventually (gt_mem_nhds (hd i hi))
  refine ⟨C,?_⟩
  filter_upwards [hu.eventually (eventually_gt_atTop (0 : ℝ)), hw, hsep] with t hut hwt hsep
  have hwt' (r : ℝ) : side (w t) r ≤ 1 := by
    unfold side
    split
    · exact hwt.2.le
    · linarith [hwt.1]
  have hterm (i : Fin n) (hi : i ∈ off) : 2*log (u t)+2*log (|d i|/2) ≤ G t i := by
    have hR : (d i + e t/u t)*u t = R t i := by
      unfold R
      field_simp
    have h : |d i|/2 ≤ |d i+e t/u t| := by
      have hh := abs_sub_abs_le_abs_sub (d i) (-(e t/u t))
      rw [abs_neg, sub_neg_eq_add] at hh
      linarith [hsep i hi]
    have h' : (|d i|/2)*u t ≤ |R t i| := by
      rw [← hR,abs_mul,abs_of_pos hut]
      exact mul_le_mul_of_nonneg_right h hut.le
    have hside := side_pos hwt (R t i)
    have h'' : |R t i| ≤ |R t i / side (w t) (R t i)| := by
      rw [abs_div,abs_of_pos hside]
      exact (le_div_iff₀ hside).mpr
        (mul_le_of_le_one_right (abs_nonneg _) (hwt' _))
    have hs := pow_le_pow_left₀ (mul_nonneg (hd i hi).le hut.le) (h'.trans h'') 2
    rw [sq_abs] at hs
    have hlog := log_le_log (pow_pos (mul_pos (hd i hi) hut) 2)
      (hs.trans (le_add_of_nonneg_left zero_le_one))
    rw [log_pow,log_mul (hd i hi).ne' hut.ne'] at hlog
    unfold G
    nlinarith only [hlog]
  have hsum : ∑ i ∈ off, (2*log (u t)+2*log (|d i|/2)) ≤ ∑ i, G t i := by
    refine (Finset.sum_le_sum hterm).trans ?_
    exact Finset.sum_le_sum_of_subset_of_nonneg (Finset.filter_subset _ _)
      (fun _ _ _ ↦ log_nonneg (le_add_of_nonneg_right (sq_nonneg _)))
  rw [Finset.sum_add_distrib,Finset.sum_const,nsmul_eq_mul] at hsum
  have hc : 0 ≤ (ν+1)/2 := by linarith
  have h := mul_le_mul_of_nonneg_left hsum hc
  have hcount := congrArg (fun b : ℝ ↦ b*log (u t)) (atom_count x y ν)
  change (n : ℝ)*(logPeak ν+log (u t))-(ν+1)/2*∑ i, G t i ≤ _
  unfold C
  nlinarith only [h,hcount]

/-- The held-mean, held-shape, held-skew likelihood is unbounded
precisely at strict collapse multiplicity. The conclusion also holds
on the more general fixed-offset ray. -/
theorem fixedOffset_unbounded_iff (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) :
    ¬ BddAbove (logAlongAtom x y ν w e '' Ioi 0) ↔ TDist.exponent x y ν < 0 := by
  let f := logAlongAtom x y ν w e
  let C := collapseLogHeight x y ν w e
  have hmax_bdd : (∃ u ∈ Ioi (0 : ℝ), IsMaxOn f (Ioi 0) u) → BddAbove (f '' Ioi 0) := by
    rintro ⟨u,_,hm⟩
    refine ⟨f u,?_⟩
    rintro _ ⟨v,hv,rfl⟩
    exact hm hv
  constructor
  · intro h
    by_contra! hE
    apply h
    rcases hE.lt_or_eq with hp | he
    · exact hmax_bdd (exists_of_meetsCollapse hn hν hw one_pos (Or.inl hp))
    · by_cases hC : ∃ v > 0, C ≤ f v
      · obtain ⟨v,hv,hcv⟩ := hC
        exact hmax_bdd (exists_of_meetsCollapse hn hν hw hv (Or.inr ⟨he.symm,hcv⟩))
      · refine ⟨C,?_⟩
        rintro _ ⟨v,hv,rfl⟩
        exact (lt_of_not_ge (fun hh ↦ hC ⟨v,hv,hh⟩)).le
  · intro he
    rintro ⟨L,hL⟩
    have ht := ((collapse_cases x y ν w e hw).1 he).eventually (eventually_gt_atTop L)
    obtain ⟨u,hu,hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and ht).exists
    exact hgt.not_ge (hL (mem_image_of_mem f hu))

end

end UnivariateML.TwoPieceStudent

end
