module

public import UnivariateML.Generic.AE3WeightedChart
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Tactic

set_option linter.style.header false

/-! Finite exponentiated Weibull winner uniqueness on positive levels. -/

@[expose] public section

open Set Filter MeasureTheory Real
open scoped Topology
open UnivariateML.AE3WeightedChart (domain likelihood)

namespace UnivariateML.AE3ExpWeibull

noncomputable section

variable {n m : ℕ}

/-- Weibull shape, scale, and outer exponent. -/
abbrev Parameters := ℝ × ℝ × ℝ

/-- Positive parameter and sample chart. -/
def support (param : (Fin m → ℝ) → Parameters) : Set ((Fin m → ℝ) × ℝ) :=
  {z | 0 < (param z.1).1 ∧ 0 < (param z.1).2.1 ∧ 0 < (param z.1).2.2 ∧ 0 < z.2}

/-- Exponentiated Weibull scalar log density. -/
def density (param : (Fin m → ℝ) → Parameters) (z : (Fin m → ℝ) × ℝ) : ℝ :=
  let (b, a, c) := param z.1
  let y := b * log (z.2 / a)
  let v := exp y
  log b + log c - log z.2 + y - v + (c - 1) * log (1 - exp (-v))

private def functionV (c v : ℝ) : ℝ := 1 - v + (c - 1) * v / (exp v - 1)

private def score (b a c x : ℝ) : ℝ :=
  (b * functionV c (exp (b * log (x / a))) - 1) / x

private theorem hasScore {m : ℕ} {b a c x : ℝ} (ha : 0 < a) (hx : 0 < x) :
    HasDerivAt (fun y ↦ density (fun _ : Fin m → ℝ ↦ (b, a, c)) (0, y))
      (score b a c x) x := by
  have hlog := (hasDerivAt_id x).log hx.ne'
  have hy := ((((hasDerivAt_id x).div_const a).log (div_ne_zero hx.ne' ha.ne')).const_mul b)
  let v := exp (b * log (x / a))
  have hv : 0 < v := exp_pos _
  have hL := ((hy.exp.neg.exp).const_sub 1).log
    (sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos hv))).ne'
  have hh := ((((hlog.const_sub (log b + log c)).add hy).sub hy.exp).add
    (hL.const_mul (c - 1)))
  convert hh using 1
  · rfl
  · simp only [score, functionV, id_eq]
    have he : exp (-v) * (exp v - 1) = 1 - exp (-v) := by
      rw [mul_sub, mul_one, ← exp_add, neg_add_cancel, exp_zero]
    change (b * (1 - v + (c - 1) * v / (exp v - 1)) - 1) / x =
      -(1 / x) + b * (1 / a / (x / a)) - v * (b * (1 / a / (x / a))) +
        (c - 1) * (-(exp (-v) * -(v * (b * (1 / a / (x / a))))) / (1 - exp (-v)))
    rw [← he]
    field_simp [hx.ne', ha.ne', exp_ne_zero _, sub_ne_zero.mpr
      (one_lt_exp_iff.mpr hv).ne']
    ring

private theorem normalized_limit (c : ℝ) :
    Tendsto (fun v : ℝ ↦ functionV c v / v) atTop (𝓝 (-1)) := by
  have hinv : Tendsto (fun v : ℝ ↦ v⁻¹) atTop (𝓝 0) := tendsto_inv_atTop_zero
  have he := (tendsto_exp_atTop.atTop_add (tendsto_const_nhds (x := (-1 : ℝ)))).inv_tendsto_atTop
  have h := (hinv.sub (tendsto_const_nhds (x := (1 : ℝ)))).add (he.const_mul (c - 1))
  simp only [zero_sub, mul_zero, add_zero] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with v hv
  dsimp [functionV]
  rw [show exp v + -1 = exp v - 1 by ring]
  field_simp [hv.ne', sub_ne_zero.mpr (one_lt_exp_iff.mpr hv).ne']

private theorem identify {b a c b' a' c' : ℝ}
    (hb : 0 < b) (ha : 0 < a) (hb' : 0 < b') (ha' : 0 < a')
    (he : ∀ x > 0, score b a c x = score b' a' c' x) :
    b = b' ∧ a = a' ∧ c = c' := by
  let v (b a t : ℝ) := exp (b * (t - log a))
  have h (t : ℝ) : b * functionV c (v b a t) = b' * functionV c' (v b' a' t) := by
    have hh := sub_left_inj.mp ((div_left_inj₀ (exp_pos t).ne').mp (he (exp t) (exp_pos t)))
    simpa only [v, log_div (exp_ne_zero t) ha.ne', log_div (exp_ne_zero t) ha'.ne', log_exp]
      using hh
  have htop (b a : ℝ) (hb : 0 < b) : Tendsto (v b a) atTop atTop :=
    tendsto_exp_atTop.comp
      ((tendsto_id.atTop_add (tendsto_const_nhds (x := -log a))).const_mul_atTop hb)
  have hnorm (b a c : ℝ) (hb : 0 < b) :
      Tendsto (fun t ↦ b * (functionV c (v b a t) / v b a t)) atTop (𝓝 (-b)) := by
    simpa only [mul_neg_one, Function.comp_apply]
      using ((normalized_limit c).comp (htop b a hb)).const_mul b
  have ratio (B A D E t : ℝ) :
      v B A t / v D E t =
        exp ((B - D) * t + (D * log E - B * log A)) := by
    unfold v
    rw [← exp_sub]
    congr 1
    ring
  have no_less {b a c b' a' c' : ℝ} (hb : 0 < b) (hb' : 0 < b') (hlt : b < b')
      (h : ∀ t, b * functionV c (v b a t) = b' * functionV c' (v b' a' t)) : False := by
    have hzero : Tendsto (fun t ↦ v b a t / v b' a' t) atTop (𝓝 0) := by
      simp only [ratio]
      exact tendsto_exp_atBot.comp
        (((tendsto_id.const_mul_atTop_of_neg (sub_neg.mpr hlt))).atBot_add
          (tendsto_const_nhds (x := b' * log a' - b * log a)))
    have hh := (hnorm b a c hb).mul hzero
    simp only [mul_zero] at hh
    have heq : (fun t ↦ (b * (functionV c (v b a t) / v b a t)) * (v b a t / v b' a' t)) =
        (fun t ↦ b' * (functionV c' (v b' a' t) / v b' a' t)) := by
      funext t
      have hh : v b a t ≠ 0 := exp_ne_zero _
      rw [show b * (functionV c (v b a t) / v b a t) * (v b a t / v b' a' t) =
        (b * functionV c (v b a t)) / v b' a' t by field_simp [hh],
        h t, mul_div_assoc]
    rw [heq] at hh
    have := tendsto_nhds_unique hh (hnorm b' a' c' hb')
    linarith
  have hbb : b = b' := by
    rcases lt_trichotomy b b' with hlt | he | hgt
    · exact (no_less hb hb' hlt h).elim
    · exact he
    · exact (no_less hb' hb hgt (fun t ↦ (h t).symm)).elim
  subst b'
  let R := exp (b * log a' - b * log a)
  have heq : (fun t ↦ (b * (functionV c (v b a t) / v b a t)) * R) =
      (fun t ↦ b * (functionV c' (v b a' t) / v b a' t)) := by
    funext t
    have hr : R = v b a t / v b a' t := by rw [ratio, sub_self, zero_mul, zero_add]
    rw [hr, show b * (functionV c (v b a t) / v b a t) * (v b a t / v b a' t) =
      (b * functionV c (v b a t)) / v b a' t by
        field_simp [show v b a t ≠ 0 from exp_ne_zero _], h t, mul_div_assoc]
  have hh := (hnorm b a c hb).mul_const R
  rw [heq] at hh
  have hr : R = 1 := mul_left_cancel₀ (neg_ne_zero.mpr hb.ne')
    ((tendsto_nhds_unique hh (hnorm b a' c' hb)).trans (mul_one _).symm)
  have haa : a = a' := by
    have hz := exp_injective (hr.trans exp_zero.symm)
    exact strictMonoOn_log.injOn ha ha'
      (mul_left_cancel₀ hb.ne' (sub_eq_zero.mp hz).symm)
  subst a'
  have hv := mul_left_cancel₀ hb.ne' (h (log a))
  simp only [v, sub_self, mul_zero, exp_zero, functionV, mul_one, zero_add] at hv
  have hh : c - 1 = c' - 1 :=
    (div_left_inj₀ (sub_ne_zero.mpr (one_lt_exp_iff.mpr zero_lt_one).ne')).mp hv
  exact ⟨rfl, rfl, sub_left_inj.mp hh⟩

set_option maxHeartbeats 650000 in
-- Elaboration combines the analytic chart, its scalar score, and the incidence application.
private theorem chart_unique (param : (Fin m → ℝ) → Parameters)
    (hinj : Function.Injective param)
    (h₁ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).1) p)
    (h₂ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).2.1) p)
    (h₃ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).2.2) p)
    (hn : m + m < n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q := by
  let T := (Fin m → ℝ) × ℝ
  let b (z : T) := (param z.1).1
  let a (z : T) := (param z.1).2.1
  let c (z : T) := (param z.1).2.2
  let sc (z : T) := score (b z) (a z) (c z) z.2
  have ab (z : T) : AnalyticAt ℝ b z :=
    (h₁ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin m → ℝ) ℝ).analyticAt z)
  have aa (z : T) : AnalyticAt ℝ a z :=
    (h₂ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin m → ℝ) ℝ).analyticAt z)
  have ac (z : T) : AnalyticAt ℝ c z :=
    (h₃ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin m → ℝ) ℝ).analyticAt z)
  have ax (z : T) := (ContinuousLinearMap.snd ℝ (Fin m → ℝ) ℝ).analyticAt z
  let y (z : T) := b z * log (z.2 / a z)
  let v (z : T) := exp (y z)
  let L (z : T) := log (1 - exp (-v z))
  have ay (z : T) (hz : z ∈ support param) : AnalyticAt ℝ y z :=
    (ab z).mul ((analyticAt_log (div_pos hz.2.2.2 hz.2.1)).comp (g := log) (x := z)
      ((ax z).div (aa z) hz.2.1.ne'))
  have av (z : T) (hz : z ∈ support param) : AnalyticAt ℝ v z := (ay z hz).rexp'
  have aL (z : T) (hz : z ∈ support param) : AnalyticAt ℝ L z :=
    (analyticAt_log (sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos (exp_pos _))))).comp
      (g := log) (x := z) (analyticAt_const.sub (av z hz).neg.rexp')
  have hopen : IsOpen (support param) := by
    have he : support param = ({z | 0 < b z} ∩ {z | 0 < a z} ∩ {z | 0 < c z} ∩
        {z | 0 < z.2} : Set T) := by
      ext z
      simp [support, and_assoc, b, a, c]
      rfl
    exact he ▸ (((isOpen_lt continuous_const (continuous_iff_continuousAt.mpr
      (fun z ↦ (ab z).continuousAt))).inter
      (isOpen_lt continuous_const (continuous_iff_continuousAt.mpr
        (fun z ↦ (aa z).continuousAt)))).inter
      (isOpen_lt continuous_const (continuous_iff_continuousAt.mpr
        (fun z ↦ (ac z).continuousAt)))).inter (isOpen_lt continuous_const continuous_snd)
  have ha : AnalyticOnNhd ℝ (density param) (support param) := by
    intro z hz
    have hlogb : AnalyticAt ℝ (fun y : T ↦ log (b y)) z :=
      (analyticAt_log hz.1).comp (g := log) (x := z) (ab z)
    have hlogc : AnalyticAt ℝ (fun y : T ↦ log (c y)) z :=
      (analyticAt_log hz.2.2.1).comp (g := log) (x := z) (ac z)
    have hlogx : AnalyticAt ℝ (fun y : T ↦ log y.2) z :=
      (analyticAt_log hz.2.2.2).comp (g := log) (x := z) (ax z)
    exact (((((hlogb.add hlogc).sub hlogx).add (ay z hz)).sub (av z hz)).add
      (((ac z).sub analyticAt_const).mul (aL z hz)))
  have hscore : AnalyticOnNhd ℝ sc (support param) := by
    intro z hz
    have hne : exp (v z) - 1 ≠ 0 := sub_ne_zero.mpr (one_lt_exp_iff.mpr (exp_pos _)).ne'
    have h₁ := ((ac z).sub (analyticAt_const (v := (1 : ℝ))))
    have h₂ := (h₁.mul (av z hz)).div ((av z hz).rexp'.sub analyticAt_const) hne
    exact (((ab z).mul ((analyticAt_const.sub (av z hz)).add h₂)).sub analyticAt_const).div
      (ax z) hz.2.2.2.ne'
  refine AE3WeightedChart.ae_unique_of_analytic_score hn
    hopen ha.continuousOn hscore continuousOn_const
    (fun p t ht ↦ hasScore (m := m) (b := (param p).1) (c := (param p).2.2) ht.2.1 ht.2.2.2)
    ?_ ?_ w hw
  · intro p q
    by_cases h : 0 < (param p).1 ∧ 0 < (param p).2.1 ∧ 0 < (param p).2.2 ∧
        0 < (param q).1 ∧ 0 < (param q).2.1 ∧ 0 < (param q).2.2
    · convert (convex_Ioi (0 : ℝ)).isPreconnected using 1
      ext t
      simp [support, h.1, h.2.1, h.2.2.1, h.2.2.2.1, h.2.2.2.2.1, h.2.2.2.2.2]
    · convert isPreconnected_empty using 1
      ext t
      simp only [mem_ofPred_eq, mem_empty_iff_false, iff_false]
      intro ht
      exact h ⟨ht.1.1, ht.1.2.1, ht.1.2.2.1, ht.2.1, ht.2.2.1, ht.2.2.2.1⟩
  · rintro p q hne ⟨t, hp, hq⟩
    by_contra! h
    obtain ⟨h₁, h₂, h₃⟩ := identify (c := (param p).2.2) (c' := (param q).2.2)
      hp.1 hp.2.1 hq.1 hq.2.1 (fun t ht ↦
      h t ⟨hp.1, hp.2.1, hp.2.2.1, ht⟩ ⟨hq.1, hq.2.1, hq.2.2.1, ht⟩)
    exact hne (hinj (Prod.ext h₁ (Prod.ext h₂ h₃)))

/-- With shape, scale and exponent free, at most one finite exponentiated Weibull winner
exists for almost every fixed-count stratum with at least seven levels. -/
theorem all_free_ae_unique (hn : 7 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : (Fin 3 → ℝ) → Parameters := fun p ↦ (p 0, p 1, p 2)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q := by
  dsimp only
  apply chart_unique _ ?_ ?_ ?_ ?_ (by omega) w hw
  · intro p q he
    funext i
    fin_cases i
    · exact congrArg Prod.fst he
    · exact congrArg (fun z : Parameters ↦ z.2.1) he
    · exact congrArg (fun z : Parameters ↦ z.2.2) he
  · exact fun p ↦ (ContinuousLinearMap.proj (0 : Fin 3) : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt p
  · exact fun p ↦ (ContinuousLinearMap.proj (1 : Fin 3) : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt p
  · exact fun p ↦ (ContinuousLinearMap.proj (2 : Fin 3) : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt p

/-- With scale held, at most one finite exponentiated Weibull winner exists for almost
every fixed-count stratum with at least five levels. -/
theorem held_scale_ae_unique (s : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : (Fin 2 → ℝ) → Parameters := fun p ↦ (p 0, s, p 1)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q := by
  dsimp only
  apply chart_unique _ ?_ ?_ ?_ ?_ (by omega) w hw
  · intro p q he
    funext i
    fin_cases i
    · exact congrArg Prod.fst he
    · exact congrArg (fun z : Parameters ↦ z.2.2) he
  · exact fun p ↦ (ContinuousLinearMap.proj (0 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p
  · exact fun _ ↦ analyticAt_const
  · exact fun p ↦ (ContinuousLinearMap.proj (1 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p

end

end UnivariateML.AE3ExpWeibull

end
