module

public import UnivariateML.Generic.AE3WeightedChart
public import UnivariateML.Family.ExpSumSeparation
public import UnivariateML.Family.Beta
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Analysis.Calculus.DSlope
import Mathlib.Tactic

set_option linter.style.header false

/-! Fixed-endpoint generalized beta likelihood on strictly interior data. -/

@[expose] public section

open Set Filter MeasureTheory Real
open scoped Topology
open UnivariateML.AE3WeightedChart (domain likelihood)
open UnivariateML.ExpSumSeparation

namespace UnivariateML.AE3GenBeta

noncomputable section

variable {n : ℕ}

/-- Lower shape, upper shape, and power. -/
abbrev Parameters := ℝ × ℝ × ℝ

/-- Positive parameters and observations strictly between zero and the endpoint. -/
def support {P : Type*} (s : ℝ) (param : P → Parameters) : Set (P × ℝ) :=
  {z | 0 < (param z.1).1 ∧ 0 < (param z.1).2.1 ∧ 0 < (param z.1).2.2 ∧
    0 < z.2 ∧ z.2 < s}

/-- The observation-dependent part of the fixed-endpoint likelihood. -/
def kernel {P : Type*} (s : ℝ) (param : P → Parameters) (z : P × ℝ) : ℝ :=
  genbetaKernel s (param z.1).1 (param z.1).2.1 (param z.1).2.2 z.2

/-- The observation-independent part of the fixed-endpoint likelihood. -/
def normalizer {P : Type*} (s : ℝ) (param : P → Parameters) (v : P) : ℝ :=
  log (param v).2.2 - log s - Beta.logBeta ((param v).1, (param v).2.1)

private theorem analytic_chart {P : Type*} [NormedAddCommGroup P] [NormedSpace ℝ P]
    (s : ℝ) (param : P → Parameters) (ha : AnalyticOnNhd ℝ param univ) :
    IsOpen (support s param) ∧ AnalyticOnNhd ℝ (kernel s param) (support s param) ∧
      ContinuousOn (fun z : P × ℝ ↦ normalizer s param z.1) (support s param) := by
  have hc := continuousOn_univ.mp ha.continuousOn
  let v : P × ℝ → Parameters := param ∘ Prod.fst
  have cv : Continuous v := hc.comp continuous_fst
  refine ⟨?_, ?_, ?_⟩
  · have he : support s param = ({z | 0 < (v z).1} ∩ {z | 0 < (v z).2.1} ∩
        {z | 0 < (v z).2.2} ∩ {z | 0 < z.2} ∩ {z | z.2 < s} : Set (P × ℝ)) := by
      ext z
      simp only [support, mem_ofPred_eq, mem_inter_iff, and_assoc]
      rfl
    rw [he]
    exact ((((isOpen_lt continuous_const cv.fst).inter
      (isOpen_lt continuous_const cv.snd.fst)).inter
      (isOpen_lt continuous_const cv.snd.snd)).inter
      (isOpen_lt continuous_const continuous_snd)).inter
      (isOpen_lt continuous_snd continuous_const)
  · intro z hz
    have av : AnalyticAt ℝ v z := (ha z.1 (mem_univ _)).comp
      ((ContinuousLinearMap.fst ℝ P ℝ).analyticAt z)
    have ax := (ContinuousLinearMap.snd ℝ P ℝ).analyticAt z
    have al : AnalyticAt ℝ (fun y ↦ (v y).1) z :=
      ((ContinuousLinearMap.fst ℝ ℝ (ℝ × ℝ)).analyticAt (v z)).comp (x := z) av
    have ar : AnalyticAt ℝ (fun y ↦ (v y).2) z :=
      ((ContinuousLinearMap.snd ℝ ℝ (ℝ × ℝ)).analyticAt (v z)).comp (x := z) av
    have au : AnalyticAt ℝ (fun y ↦ (v y).2.1) z :=
      ((ContinuousLinearMap.fst ℝ ℝ ℝ).analyticAt (v z).2).comp (x := z) ar
    have ap : AnalyticAt ℝ (fun y ↦ (v y).2.2) z :=
      ((ContinuousLinearMap.snd ℝ ℝ ℝ).analyticAt (v z).2).comp (x := z) ar
    have hx : 0 < z.2 := hz.2.2.2.1
    have hxs : z.2 < s := hz.2.2.2.2
    have hs := hx.trans hxs
    have apow : 0 < (v z).2.2 := hz.2.2.1
    have at' : AnalyticAt ℝ (fun y : P × ℝ ↦ log (y.2 / s)) z :=
      (analyticAt_log (div_pos hx hs)).comp (g := log) (x := z)
        (ax.div analyticAt_const hs.ne')
    have hn : 0 < 1 - exp ((v z).2.2 * log (z.2 / s)) := sub_pos.mpr
      (exp_lt_one_iff.mpr (mul_neg_of_pos_of_neg apow
        (log_neg (div_pos hx hs) ((div_lt_one hs).mpr hxs))))
    have hlog : AnalyticAt ℝ (fun y : P × ℝ ↦
        log (1 - exp ((v y).2.2 * log (y.2 / s)))) z :=
      (analyticAt_log hn).comp (g := log) (x := z) (analyticAt_const.sub (ap.mul at').rexp')
    exact (((al.mul ap).sub analyticAt_const).mul at').add ((au.sub analyticAt_const).mul hlog)
  · intro z hz
    have hl : 0 < (v z).1 := hz.1
    have hu : 0 < (v z).2.1 := hz.2.1
    have hp : 0 < (v z).2.2 := hz.2.2.1
    have cg {g : P × ℝ → ℝ} (hg : Continuous g) (hpos : 0 < g z) :
        ContinuousAt (fun y ↦ Gamma (g y)) z :=
      (hasDerivAt_Gamma_of_pos hpos).continuousAt.comp (g := Gamma) (x := z) hg.continuousAt
    have hB : ContinuousAt (fun y : P × ℝ ↦ Beta.logBeta ((v y).1, (v y).2.1)) z := by
      unfold Beta.logBeta Beta.normalizer
      exact (((cg cv.fst hl).mul (cg cv.snd.fst hu)).div
        (cg (cv.fst.add cv.snd.fst) (add_pos hl hu))
          (Gamma_pos_of_pos (add_pos hl hu)).ne').log
          (div_pos (mul_pos (Gamma_pos_of_pos hl) (Gamma_pos_of_pos hu))
            (Gamma_pos_of_pos (add_pos hl hu))).ne'
    exact (((cv.snd.snd.continuousAt.log hp.ne').sub continuousAt_const).sub hB).continuousWithinAt

/-- Nonunit fixed upper shape and endpoint give at most one finite winner
almost everywhere on four or more levels. -/
theorem held_scale_shape2_ae_unique (s u : ℝ) (hu : u ≠ 1) (w : Fin n → ℕ)
    (hw : ∀ i, 0 < w i) :
    let param : ℝ × ℝ → Parameters := fun z ↦ (z.1, u, z.2)
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain (support s param))
          (likelihood (kernel s param) (normalizer s param) w) x p →
        UniqueAE.Winner (domain (support s param))
          (likelihood (kernel s param) (normalizer s param) w) x q → p = q := by
  let param : ℝ × ℝ → Parameters := fun z ↦ (z.1, u, z.2)
  change ∀ᵐ x : Fin n → ℝ ∂volume, _ → ∀ p q,
    UniqueAE.Winner (domain (support s param))
      (likelihood (kernel s param) (normalizer s param) w) x p →
    UniqueAE.Winner (domain (support s param))
      (likelihood (kernel s param) (normalizer s param) w) x q →
    p = q
  obtain ⟨ho, ha, hc⟩ := analytic_chart s param (fun z _ ↦
    ((ContinuousLinearMap.fst ℝ ℝ ℝ).analyticAt z).prod
      (analyticAt_const.prod ((ContinuousLinearMap.snd ℝ ℝ ℝ).analyticAt z)))
  refine AE3WeightedChart.ae_winning_class_analytic ho ha hc w hw id _ ?_
  apply Eventually.of_forall
  intro x hlevels p q hp hq he
  have hpos : ∀ t ∈ Finset.univ.image x, 0 < t ∧ t < s := by
    intro t ht
    obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ht
    exact (hp.1 i).2.2.2
  have hsc : ∀ t ∈ Finset.univ.image x,
      deriv (genbetaKernel s p.1 u p.2) t = deriv (genbetaKernel s q.1 u q.2) t := by
    intro t ht
    obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ht
    exact he i
  obtain ⟨t, ht⟩ := Finset.card_pos.mp (show 0 < (Finset.univ.image x).card by omega)
  obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ht
  obtain ⟨hl, hp'⟩ := genbeta_held_scale_shape2_separation hu
    (hp.1 i).2.2.1 (hq.1 i).2.2.1 _ hlevels hpos hsc
  exact Prod.ext hl hp'

private theorem beta_one {l : ℝ} (hl : 0 < l) :
    Beta.normalizer (l, 1) = 1 / l := by
  unfold Beta.normalizer
  simp only [Gamma_one, Gamma_add_one hl.ne']
  have hg := (Gamma_pos_of_pos hl).ne'
  field_simp

private theorem beta_two {l : ℝ} (hl : 0 < l) :
    Beta.normalizer (l, 2) = 1 / (l * (l + 1)) := by
  unfold Beta.normalizer
  change Gamma l * Gamma 2 / Gamma (l + 2) = _
  rw [show (2 : ℝ) = 1 + 1 by norm_num, Gamma_add_one one_ne_zero, Gamma_one,
    ← add_assoc, Gamma_add_one (add_pos hl zero_lt_one).ne', Gamma_add_one hl.ne']
  have hg := (Gamma_pos_of_pos hl).ne'
  field_simp

private theorem eventual_unit_improvement {r d : ℝ} (hr : 0 < r) (hd : 0 < d) :
    ∀ᶠ p : ℝ in atTop, 1 < (1 + r / p) * (1 - exp (-(p * d))) := by
  have hlim : Tendsto (fun p : ℝ ↦ exp (p * d) / p) atTop atTop := by
    have hm := (tendsto_exp_div_pow_atTop 1).comp (tendsto_id.const_mul_atTop hd)
    apply (hm.const_mul_atTop hd).congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
    dsimp
    rw [pow_one, mul_comm p d]
    field_simp
  filter_upwards [hlim.eventually_gt_atTop (1 + r⁻¹), eventually_gt_atTop (1 : ℝ)]
    with p he hp
  have hp' : 0 < p := zero_lt_one.trans hp
  have hd' : 1 + p / r < exp (p * d) := by
    have hh := (lt_div_iff₀ hp').mp he
    rw [add_mul, one_mul, div_eq_inv_mul] at *
    linarith
  have hexp := exp_pos (p * d)
  have hgt : p + r < r * exp (p * d) := by
    have hh : (p + r) / r < exp (p * d) := by
      rwa [add_div, div_self hr.ne', add_comm]
    simpa [mul_comm] using (div_lt_iff₀ hr).mp hh
  rw [exp_neg, show 1 + r / p = (p + r) / p by
    rw [add_div, div_self hp'.ne'], show 1 - (exp (p * d))⁻¹ =
      (exp (p * d) - 1) / exp (p * d) by
        rw [sub_div, div_self hexp.ne', one_div], div_mul_div_comm]
  apply (lt_div_iff₀ (mul_pos hp' hexp)).2
  nlinarith only [hgt]


private theorem no_unit_winner (s : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    {x : Fin n → ℝ} (hn : 0 < n) {l p : ℝ}
    (h : UniqueAE.Winner (domain (support s id))
      (likelihood (kernel s id) (normalizer s id) w) x (l, 1, p)) : False := by
  let j : Fin n := ⟨0, hn⟩
  have hl : 0 < l := (h.1 j).1
  have hp : 0 < p := (h.1 j).2.2.1
  have hx (i : Fin n) : 0 < x i := (h.1 i).2.2.2.1
  have hxs (i : Fin n) : x i < s := (h.1 i).2.2.2.2
  have hs : 0 < s := (hx j).trans (hxs j)
  let r := l * p
  have hr : 0 < r := mul_pos hl hp
  let d (i : Fin n) := -log (x i / s)
  have hd (i : Fin n) : 0 < d i :=
    neg_pos.mpr (log_neg (div_pos (hx i) hs) ((div_lt_one hs).mpr (hxs i)))
  have he : ∀ᶠ a : ℝ in atTop, ∀ i, 1 < (1 + r / a) * (1 - exp (-(a * d i))) :=
    eventually_all.mpr (fun i ↦ eventual_unit_improvement hr (hd i))
  obtain ⟨a, ha, he⟩ := ((eventually_gt_atTop (0 : ℝ)).and he).exists
  have hla : 0 < r / a := div_pos hr ha
  have hmul : r / a * a = r := div_mul_cancel₀ _ ha.ne'
  have hlog : log a + log (r / a) = log p + log l := by
    rw [← log_mul ha.ne' hla.ne', mul_comm a, hmul, ← log_mul hp.ne' hl.ne', mul_comm p l]
  have hterm (i : Fin n) :
      normalizer s id (l, 1, p) + kernel s id ((l, 1, p), x i) <
      normalizer s id (r / a, 2, a) + kernel s id ((r / a, 2, a), x i) := by
    have hlogpos : 0 < log (1 + r / a) + log (1 - exp (-(a * d i))) := by
      rw [← log_mul (by positivity) (sub_pos.mpr (exp_lt_one_iff.mpr
        (neg_neg_of_pos (mul_pos ha (hd i))))).ne']
      exact log_pos (he i)
    simp only [kernel, normalizer, id_eq, genbetaKernel, Beta.logBeta, beta_one hl, beta_two hla,
      log_div one_ne_zero hl.ne', log_div one_ne_zero (mul_pos hla (add_pos hla zero_lt_one)).ne',
      log_one, zero_sub, sub_neg_eq_add, sub_self, zero_mul, add_zero, hmul]
    rw [log_mul hla.ne' (add_pos hla zero_lt_one).ne']
    have arg : -(a * d i) = a * log (x i / s) := by simp [d]
    rw [arg, add_comm (1 : ℝ) (r / a)] at hlogpos
    linarith only [hlogpos, hlog]
  have hlt : likelihood (kernel s id) (normalizer s id) w (l, 1, p) x <
      likelihood (kernel s id) (normalizer s id) w (r / a, 2, a) x := by
    unfold likelihood
    exact Finset.sum_lt_sum
      (fun i _ ↦ mul_le_mul_of_nonneg_left (hterm i).le (by positivity))
      ⟨j, Finset.mem_univ _, mul_lt_mul_of_pos_left (hterm j) (by exact_mod_cast hw j)⟩
  exact hlt.not_ge (h.2 _ (fun i ↦ ⟨hla, by norm_num, ha, hx i, hxs i⟩))

/-- At fixed endpoint, an attained finite fit is almost everywhere unique on
four or more levels, including when the upper shape is free. -/
theorem held_scale_ae_unique (s : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain (support s id))
          (likelihood (kernel s id) (normalizer s id) w) x p →
        UniqueAE.Winner (domain (support s id))
          (likelihood (kernel s id) (normalizer s id) w) x q → p = q := by
  obtain ⟨ho, ha, hc⟩ := analytic_chart s id (fun _ _ ↦ analyticAt_id)
  refine AE3WeightedChart.ae_winning_class_analytic ho ha hc w hw id _ ?_
  apply Eventually.of_forall
  intro x hlevels p q hp hq he
  have hpos : ∀ t ∈ Finset.univ.image x, 0 < t ∧ t < s := by
    intro t ht
    obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ht
    exact (hp.1 i).2.2.2
  have hsc : ∀ t ∈ Finset.univ.image x,
      deriv (genbetaKernel s p.1 p.2.1 p.2.2) t =
      deriv (genbetaKernel s q.1 q.2.1 q.2.2) t := by
    intro t ht
    obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ht
    exact he i
  obtain ⟨t, ht⟩ := Finset.card_pos.mp (show 0 < (Finset.univ.image x).card by omega)
  obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ht
  rcases genbeta_held_scale_separation
      (hp.1 i).2.2.1 (hq.1 i).2.2.1 _ hlevels hpos hsc with ⟨hl, hu, hpower⟩ | ⟨hu, _, _⟩
  · exact Prod.ext hl (Prod.ext hu hpower)
  · have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
    rcases p with ⟨l, u, a⟩
    change u = 1 at hu
    subst u
    exact (no_unit_winner s w hw hn hp).elim

/-- Lower shape, power, and upper endpoint are the free coordinates. -/
def movingSupport (u : ℝ) : Set ((Fin 3 → ℝ) × ℝ) :=
  {z | 0 < z.1 0 ∧ 0 < z.1 1 ∧ 0 < z.2 ∧ z.2 < z.1 2 ∧ 1 < u}

/-- Scalar log density in the free-endpoint chart. -/
def movingDensity (u : ℝ) (z : (Fin 3 → ℝ) × ℝ) : ℝ :=
  log (z.1 1) - log (z.1 2) - Beta.logBeta (z.1 0, u) +
    genbetaKernel (z.1 2) (z.1 0) u (z.1 1) z.2

private def transform (s p x : ℝ) : ℝ := exp (p * log (x / s))

private def movingScore (s l u p x : ℝ) : ℝ :=
  (l * p - 1 - (u - 1) * p * transform s p x / (1 - transform s p x)) / x

private theorem transform_lt_one {s p x : ℝ} (hp : 0 < p) (hx : 0 < x)
    (hs : x < s) : transform s p x < 1 :=
  exp_lt_one_iff.mpr (mul_neg_of_pos_of_neg hp
    (log_neg (div_pos hx (hx.trans hs)) ((div_lt_one (hx.trans hs)).mpr hs)))

private theorem transform_hasDeriv {s p x : ℝ} (hs : 0 < s) (hx : 0 < x) :
    HasDerivAt (transform s p) (p * transform s p x / x) x := by
  have hh := ((((hasDerivAt_id x).div_const s).log
    (div_ne_zero hx.ne' hs.ne')).const_mul p).exp
  convert hh using 1
  · rfl
  · simp only [transform, id_eq]
    field_simp [hs.ne', hx.ne']

private theorem moving_hasScore {s l u p x : ℝ} (hp : 0 < p) (hx : 0 < x) (hs : x < s) :
    HasDerivAt (genbetaKernel s l u p) (movingScore s l u p x) x := by
  have hpos := hx.trans hs
  have hlog := (((hasDerivAt_id x).div_const s).log (div_ne_zero hx.ne' hpos.ne'))
  have ht := (transform_hasDeriv (p := p) hpos hx).const_sub 1
  have hh := (hlog.const_mul (l * p - 1)).add
    ((ht.log (sub_pos.mpr (transform_lt_one hp hx hs)).ne').const_mul (u - 1))
  convert hh using 1
  · rfl
  · simp only [movingScore, id_eq]
    field_simp [hx.ne', hpos.ne', sub_ne_zero.mpr (transform_lt_one hp hx hs).ne']
    ring

private theorem scaled_score_at_endpoint {s l u p : ℝ} (hs : 0 < s) (hp : 0 < p) :
    Tendsto (fun x ↦ (s - x) * movingScore s l u p x) (𝓝[<] s) (𝓝 (-(u - 1))) := by
  have hE := transform_hasDeriv (p := p) hs hs
  have hself : transform s p s = 1 := by simp [transform, hs.ne']
  rw [hself, mul_one] at hE
  have hd := (continuousAt_dslope_same.mpr hE.differentiableAt).continuousWithinAt
    (s := Iio s) |>.tendsto
  rw [dslope_same, hE.deriv] at hd
  have hE' := hE.continuousAt.continuousWithinAt (s := Iio s) |>.tendsto
  rw [hself] at hE'
  have hnum := (((continuous_const (y := s)).sub continuous_id).continuousAt
      (x := s)).continuousWithinAt (s := Iio s) |>.tendsto
  have hden := (continuous_id.continuousAt (x := s)).continuousWithinAt (s := Iio s) |>.tendsto
  have hh := ((hnum.mul_const (l * p - 1)).sub
    ((hE'.const_mul ((u - 1) * p)).div hd (div_ne_zero hp.ne' hs.ne'))).div hden hs.ne'
  have hlim : ((s - s) * (l * p - 1) - (u - 1) * p * 1 / (p / s)) / s =
      -(u - 1) := by field_simp [hp.ne', hs.ne']; ring
  simp only [Pi.div_apply, Pi.sub_apply, id_eq] at hh
  rw [hlim] at hh
  refine hh.congr' ?_
  filter_upwards [self_mem_nhdsWithin, (Ioi_mem_nhds hs : Ioi 0 ∈ 𝓝 s)
    |> mem_nhdsWithin_of_mem_nhds]
    with x hxs hx
  have hx' : 0 < x := hx
  have hxs' : x < s := hxs
  rw [Pi.div_apply, id_eq, dslope_of_ne _ hxs'.ne, slope_def_field, hself]
  dsimp only [movingScore]
  have hne := sub_ne_zero.mpr (transform_lt_one hp hx' hxs').ne'
  field_simp [hx'.ne', sub_ne_zero.mpr hxs'.ne, hne,
    sub_ne_zero.mpr (transform_lt_one hp hx' hxs').ne]
  ring

private theorem moving_identify {s s' p p' l l' u : ℝ}
    (hs : 0 < s) (hs' : 0 < s') (hp : 0 < p) (hp' : 0 < p') (hu : 1 < u)
    (he : ∀ x ∈ Ioo 0 (min s s'), movingScore s l u p x = movingScore s' l' u p' x) :
    s = s' ∧ p = p' ∧ l = l' := by
  have no_cross {s s' l l' p p' : ℝ} (hs : 0 < s) (hp : 0 < p) (hp' : 0 < p') (hss : s < s')
      (he : ∀ x ∈ Ioo 0 s, movingScore s l u p x = movingScore s' l' u p' x) : False := by
    have ht := (transform_hasDeriv (p := p') (hs.trans hss) hs).continuousAt
    have hne := sub_ne_zero.mpr (transform_lt_one hp' hs hss).ne'
    have hc : ContinuousAt (movingScore s' l' u p') s :=
      ((continuousAt_const.sub ((ht.const_mul ((u - 1) * p')).div
        (continuousAt_const.sub ht) hne)).div continuous_id.continuousAt hs.ne')
    have hh : ContinuousAt (fun x ↦ (s - x) * movingScore s' l' u p' x) s :=
      (continuous_const.sub continuous_id).continuousAt.mul hc
    have hzero : Tendsto (fun x ↦ (s - x) * movingScore s' l' u p' x) (𝓝[<] s) (𝓝 0) := by
      simpa only [sub_self, zero_mul] using hh.continuousWithinAt (s := Iio s) |>.tendsto
    have heq : (fun x ↦ (s - x) * movingScore s l u p x) =ᶠ[𝓝[<] s]
        (fun x ↦ (s - x) * movingScore s' l' u p' x) := by
      filter_upwards [self_mem_nhdsWithin, mem_nhdsWithin_of_mem_nhds (Ioi_mem_nhds hs)]
        with x hxs hx
      rw [he x ⟨hx, hxs⟩]
    have := tendsto_nhds_unique_of_eventuallyEq
      (scaled_score_at_endpoint (l := l) (u := u) hs hp) hzero heq
    linarith
  have hss : s = s' := by
    rcases lt_trichotomy s s' with h | h | h
    · exact (no_cross hs hp hp' h (by simpa only [min_eq_left h.le] using he)).elim
    · exact h
    · exact (no_cross hs' hp' hp h (fun x hx ↦
        (he x (by simpa only [min_eq_right h.le] using hx)).symm)).elim
  subst s'
  let v (i : Fin 4) := (i.val + 1 : ℝ) * s / 5
  have hinj : Function.Injective v := by
    intro i j h
    apply Fin.ext
    dsimp [v] at h
    have h' : (i.val : ℝ) = (j.val : ℝ) := by
      apply mul_right_cancel₀ hs.ne'
      linarith
    exact_mod_cast h'
  let z := Finset.univ.image v
  have hz : z.card = 4 := by simp [z, Finset.card_image_of_injective _ hinj]
  have hv (x : ℝ) (hx : x ∈ z) : 0 < x ∧ x < s := by
    obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp hx
    have hi₀ : 0 ≤ (i.val : ℝ) := by positivity
    have hi₁ : (i.val : ℝ) < 4 := by exact_mod_cast i.isLt
    constructor
    · dsimp [v]
      positivity
    · dsimp [v]
      apply (div_lt_iff₀ (by norm_num : (0 : ℝ) < 5)).mpr
      nlinarith [mul_pos hs (show 0 < 4 - (i.val : ℝ) by linarith)]
  have hsc : ∀ x ∈ z, deriv (genbetaKernel s l u p) x = deriv (genbetaKernel s l' u p') x := by
    intro x hx
    obtain ⟨hx₀, hxs⟩ := hv x hx
    rw [(moving_hasScore hp hx₀ hxs).deriv, (moving_hasScore hp' hx₀ hxs).deriv]
    exact he x ⟨hx₀, by simpa using hxs⟩
  obtain ⟨hl, hpow⟩ := genbeta_held_scale_shape2_separation (ne_of_gt hu) hp hp'
    z hz.ge hv hsc
  exact ⟨rfl, hpow, hl⟩


set_option maxHeartbeats 650000 in
-- Elaboration combines the analytic chart, its scalar score, and the incidence application.
/-- At held upper shape above one, finite generalized beta winners with a
free endpoint agree almost everywhere on strata with at least seven levels. -/
theorem held_shape2_ae_unique (u : ℝ) (hn : 7 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (movingSupport u))
        (likelihood (movingDensity u) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (movingSupport u))
        (likelihood (movingDensity u) (fun _ ↦ 0) w) x q → p = q := by
  let T := (Fin 3 → ℝ) × ℝ
  let U := movingSupport u
  let coord (i : Fin 3) : T →L[ℝ] ℝ :=
    (ContinuousLinearMap.proj i).comp (ContinuousLinearMap.fst ℝ (Fin 3 → ℝ) ℝ)
  let l := coord 0
  let p := coord 1
  let s := coord 2
  let x := ContinuousLinearMap.snd ℝ (Fin 3 → ℝ) ℝ
  let d (z : T) := movingScore (s z) (l z) u (p z) (x z)
  have ho : IsOpen U := by
    have h : U = ({z | 0 < l z} ∩ {z | 0 < p z} ∩ {z | 0 < x z} ∩
        {z | x z < s z} ∩ {z | 1 < u} : Set T) := by
      ext z
      simp [U, movingSupport, l, p, s, x, coord, and_assoc]
      rfl
    rw [h]
    exact ((((isOpen_lt continuous_const l.continuous).inter
      (isOpen_lt continuous_const p.continuous)).inter
      (isOpen_lt continuous_const x.continuous)).inter
      (isOpen_lt x.continuous s.continuous)).inter isOpen_const
  have ala (z : T) (hz : z ∈ U) :
      AnalyticAt ℝ (fun z : T ↦ log (x z / s z)) z :=
    (analyticAt_log (div_pos hz.2.2.1 (hz.2.2.1.trans hz.2.2.2.1))).comp
      (g := log) (x := z) ((x.analyticAt z).div (s.analyticAt z)
        (hz.2.2.1.trans hz.2.2.2.1).ne')
  have ae (z : T) (hz : z ∈ U) :
      AnalyticAt ℝ (fun z : T ↦ transform (s z) (p z) (x z)) z :=
    ((p.analyticAt z).mul (ala z hz)).rexp'
  have hE (z : T) (hz : z ∈ U) : 0 < 1 - transform (s z) (p z) (x z) :=
    sub_pos.mpr (transform_lt_one hz.2.1 hz.2.2.1 hz.2.2.2.1)
  have hd : AnalyticOnNhd ℝ d U := by
    intro z hz
    have h₁ := ((l.analyticAt z).mul (p.analyticAt z)).sub (analyticAt_const (v := (1 : ℝ)))
    have h₂ := (((analyticAt_const (v := u - 1)).mul (p.analyticAt z)).mul (ae z hz)).div
      (analyticAt_const.sub (ae z hz)) (hE z hz).ne'
    exact (h₁.sub h₂).div (x.analyticAt z) hz.2.2.1.ne'
  have hf : ContinuousOn (movingDensity u) U := by
    intro z hz
    have hs := hz.2.2.1.trans hz.2.2.2.1
    have hu : 0 < u := zero_lt_one.trans hz.2.2.2.2
    have cg {g : T → ℝ} (hg : Continuous g) (hpos : 0 < g z) :
        ContinuousAt (fun y ↦ Gamma (g y)) z :=
      (hasDerivAt_Gamma_of_pos hpos).continuousAt.comp (g := Gamma) (x := z) hg.continuousAt
    have hB : ContinuousAt (fun z : T ↦ Beta.logBeta (l z, u)) z := by
      unfold Beta.logBeta Beta.normalizer
      exact (((cg l.continuous hz.1).mul continuousAt_const).div
        (cg (l.continuous.add continuous_const) (add_pos hz.1 hu))
          (Gamma_pos_of_pos (add_pos hz.1 hu)).ne').log
          (div_pos (mul_pos (Gamma_pos_of_pos hz.1) (Gamma_pos_of_pos hu))
            (Gamma_pos_of_pos (add_pos hz.1 hu))).ne'
    have hlog : AnalyticAt ℝ (fun z : T ↦ log (1 - transform (s z) (p z) (x z))) z :=
      (analyticAt_log (hE z hz)).comp (g := log) (x := z) (analyticAt_const.sub (ae z hz))
    have hk := ((((l.analyticAt z).mul (p.analyticAt z)).sub
      (analyticAt_const (v := (1 : ℝ)))).mul (ala z hz)).add
        ((analyticAt_const (v := u - 1)).mul hlog)
    exact ((((p.continuous.continuousAt.log hz.2.1.ne').sub
      (s.continuous.continuousAt.log hs.ne')).sub hB).add hk.continuousAt).continuousWithinAt
  refine AE3WeightedChart.ae_unique_of_analytic_score (by omega : 3 + 3 < n)
    ho hf hd continuousOn_const ?_ ?_ ?_ w hw
  · intro a t ht
    exact (moving_hasScore (s := a 2) (l := a 0) (u := u)
      ht.2.1 ht.2.2.1 ht.2.2.2.1).const_add
        (log (a 1) - log (a 2) - Beta.logBeta (a 0, u))
  · intro a b
    by_cases h : 0 < a 0 ∧ 0 < a 1 ∧ 0 < b 0 ∧ 0 < b 1 ∧ 1 < u
    · convert (convex_Ioo (0 : ℝ) (min (a 2) (b 2))).isPreconnected using 1
      ext t
      simp [U, movingSupport, h.1, h.2.1, h.2.2.1, h.2.2.2.1, h.2.2.2.2, lt_min_iff]
      tauto
    · convert isPreconnected_empty using 1
      ext t
      simp only [mem_ofPred_eq, mem_empty_iff_false, iff_false]
      intro ht
      exact h ⟨ht.1.1, ht.1.2.1, ht.2.1, ht.2.2.1, ht.1.2.2.2.2⟩
  · rintro a b hne ⟨t, ha, hb⟩
    by_contra! h
    obtain ⟨hs, hp', hl⟩ := moving_identify (s := a 2) (s' := b 2)
      (ha.2.2.1.trans ha.2.2.2.1) (hb.2.2.1.trans hb.2.2.2.1)
      ha.2.1 hb.2.1 ha.2.2.2.2 (fun t ht ↦
        h t ⟨ha.1, ha.2.1, ht.1, (lt_min_iff.mp ht.2).1, ha.2.2.2.2⟩
          ⟨hb.1, hb.2.1, ht.1, (lt_min_iff.mp ht.2).2, hb.2.2.2.2⟩)
    apply hne
    funext i
    fin_cases i
    · exact hl
    · exact hp'
    · exact hs

end

end UnivariateML.AE3GenBeta

end
