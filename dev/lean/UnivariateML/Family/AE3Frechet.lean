module

public import UnivariateML.Generic.AE3WeightedChart
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Tactic

set_option linter.style.header false

/-! Finite Fréchet winners on open support, including shape one. -/

@[expose] public section

open Set Filter MeasureTheory Real
open scoped Topology
open UnivariateML.AE3WeightedChart (domain likelihood)

namespace UnivariateML.AE3Frechet

noncomputable section

variable {n m : ℕ}

/-- The coordinates of a Fréchet candidate are shape, threshold and scale. -/
abbrev Parameters := ℝ × ℝ × ℝ

/-- Finite positive-density support. -/
def support {P : Type*} (param : P → Parameters) : Set (P × ℝ) :=
  {z | 0 < (param z.1).1 ∧ 0 < (param z.1).2.2 ∧ (param z.1).2.1 < z.2}

/-- Fréchet log density in physical values. -/
def density {P : Type*} (param : P → Parameters) (z : P × ℝ) : ℝ :=
  let (b, a, s) := param z.1
  let t := log ((z.2 - a) / s)
  log b - log s - (1 + b) * t - exp (-b * t)

private def score (b a s x : ℝ) : ℝ :=
  (-1 - b + b * exp (-b * log ((x - a) / s))) / (x - a)

private theorem hasScore {b a s x : ℝ} (hs : 0 < s) (hx : a < x) :
    HasDerivAt (fun y ↦ density id ((b, a, s), y)) (score b a s x) x := by
  have ht := sub_pos.mpr hx
  have hlog := (((hasDerivAt_id x).sub_const a).div_const s).log
    (div_ne_zero ht.ne' hs.ne')
  have hh := ((hlog.const_mul (1 + b)).const_sub (log b - log s)).sub
    (hlog.const_mul (-b)).exp
  convert hh using 1
  · rfl
  · simp only [score, id_eq]
    field_simp [ht.ne', hs.ne']
    ring

private theorem score_scaled_at_threshold {b a s : ℝ} (hb : 0 < b) (hs : 0 < s) :
    Tendsto (fun t : ℝ ↦ t * score b a s (a + t)) (𝓝[>] 0) atTop := by
  have hdiv : Tendsto (fun t : ℝ ↦ t / s) (𝓝[>] 0) (𝓝[>] 0) := by
    refine tendsto_nhdsWithin_iff.mpr ⟨?_, ?_⟩
    · simpa using (continuous_id.div_const s).continuousAt.continuousWithinAt
          (x := (0 : ℝ)) |>.tendsto
    · filter_upwards [self_mem_nhdsWithin] with t ht
      exact div_pos ht hs
  have hlog := tendsto_log_nhdsGT_zero.comp hdiv
  have hex := tendsto_exp_atTop.comp (hlog.const_mul_atBot_of_neg (neg_neg_of_pos hb))
  apply ((hex.const_mul_atTop hb).atTop_add
    (tendsto_const_nhds (x := -1 - b))).congr'
  filter_upwards [self_mem_nhdsWithin] with t ht
  have ht' : 0 < t := ht
  dsimp [Function.comp_def, score]
  rw [add_sub_cancel_left]
  rw [add_comm (-1 - b)]
  field_simp

private theorem threshold_eq {b a s b' a' s' : ℝ}
    (hb : 0 < b) (hs : 0 < s) (hb' : 0 < b') (hs' : 0 < s')
    (h : ∀ x > max a a', score b a s x = score b' a' s' x) : a = a' := by
  have no_cross {b a s b' a' s' : ℝ}
      (hb : 0 < b) (hs : 0 < s) (hs' : 0 < s') (hgt : a' < a)
      (h : ∀ x > a, score b a s x = score b' a' s' x) : False := by
    have hz : Tendsto (fun t : ℝ ↦ t * score b' a' s' (a + t)) (𝓝[>] 0) (𝓝 0) := by
      have ht : ContinuousAt (fun t : ℝ ↦ a + t - a') 0 := by fun_prop
      have hh := ((continuousAt_const (y := -1 - b')).add
        ((continuousAt_const (y := b')).mul
          (Real.continuous_exp.continuousAt.comp
            ((continuousAt_const (y := -b')).mul
              ((ht.div_const s').log (by
                exact div_ne_zero (by simpa using (sub_pos.mpr hgt).ne')
                  hs'.ne')))))).div ht
        (by simpa using (sub_pos.mpr hgt).ne')
      have hh' : ContinuousAt (fun t : ℝ ↦ score b' a' s' (a + t)) 0 := hh
      have hc : ContinuousAt (fun t : ℝ ↦ t * score b' a' s' (a + t)) 0 :=
        continuous_id.continuousAt.mul hh'
      simpa only [zero_mul] using hc.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
    have ha := (score_scaled_at_threshold hb hs).congr' (f₂ := fun t ↦ t * score b' a' s' (a + t))
      (by
        filter_upwards [self_mem_nhdsWithin] with t ht
        rw [h (a + t) (lt_add_of_pos_right _ (show 0 < t from ht))])
    exact not_tendsto_nhds_of_tendsto_atTop ha _ hz
  rcases lt_trichotomy a a' with hl | he | hg
  · exact (no_cross hb' hs' hs hl (fun x hx ↦ (h x (max_lt_iff.mpr ⟨hl.trans hx, hx⟩)).symm)).elim
  · exact he
  · exact (no_cross hb hs hs' hg (fun x hx ↦ h x (max_lt_iff.mpr ⟨hx, hg.trans hx⟩))).elim

private theorem identify {b a s b' a' s' : ℝ}
    (hb : 0 < b) (hs : 0 < s) (hb' : 0 < b') (hs' : 0 < s')
    (h : ∀ x > max a a', score b a s x = score b' a' s' x) :
    b = b' ∧ a = a' ∧ s = s' := by
  have ha := threshold_eq hb hs hb' hs' h
  subst a'
  have he (t : ℝ) (ht : 0 < t) :
      -1 - b + b * exp (-b * log (t / s)) =
      -1 - b' + b' * exp (-b' * log (t / s')) := by
    have hh := h (a + t) (by simpa using ht)
    apply (div_left_inj₀ ht.ne').mp
    simpa [score] using hh
  have he' := he s hs
  rw [div_self hs.ne', log_one, mul_zero, exp_zero, mul_one] at he'
  have hbex : b' * (exp (-b' * log (s / s')) - 1) = 0 := by linear_combination -he'
  have hx := (mul_eq_zero.mp hbex).resolve_left hb'.ne'
  have hz : log (s / s') = 0 := by
    have hf := exp_injective ((sub_eq_zero.mp hx).trans exp_zero.symm)
    exact (mul_eq_zero.mp hf).resolve_left (neg_ne_zero.mpr hb'.ne')
  have hss : s = s' :=
    (div_eq_one_iff_eq hs'.ne').mp (strictMonoOn_log.injOn (div_pos hs hs') zero_lt_one
      (hz.trans log_one.symm))
  have hlim (b s : ℝ) (hb : 0 < b) (hs : 0 < s) :
      Tendsto (fun t : ℝ ↦ -1 - b + b * exp (-b * log (t / s))) atTop (𝓝 (-1 - b)) := by
    have ht := tendsto_log_atTop.comp (tendsto_id.atTop_div_const hs)
    have hh := tendsto_exp_atBot.comp (ht.const_mul_atTop_of_neg (neg_neg_of_pos hb))
    simpa using (hh.const_mul b).const_add (-1 - b)
  have hh := tendsto_nhds_unique_of_eventuallyEq (hlim b s hb hs) (hlim b' s' hb' hs')
    (by filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht using he t ht)
  exact ⟨by linarith, rfl, hss⟩

set_option maxHeartbeats 650000 in
-- Elaboration combines the analytic chart, its scalar score, and the incidence application.
private theorem chart_unique (param : (Fin m → ℝ) → Parameters)
    (h₁ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).1) p)
    (h₂ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).2.1) p)
    (h₃ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).2.2) p)
    (hn : m + m < n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      param p = param q := by
  let T := (Fin m → ℝ) × ℝ
  let b (z : T) := (param z.1).1
  let a (z : T) := (param z.1).2.1
  let s (z : T) := (param z.1).2.2
  let sc (z : T) := score (b z) (a z) (s z) z.2
  have ab (z : T) : AnalyticAt ℝ b z :=
    (h₁ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin m → ℝ) ℝ).analyticAt z)
  have aa (z : T) : AnalyticAt ℝ a z :=
    (h₂ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin m → ℝ) ℝ).analyticAt z)
  have as' (z : T) : AnalyticAt ℝ s z :=
    (h₃ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin m → ℝ) ℝ).analyticAt z)
  have hx (z : T) := (ContinuousLinearMap.snd ℝ (Fin m → ℝ) ℝ).analyticAt z
  have hlog (z : T) (hz : z ∈ support param) :
      AnalyticAt ℝ (fun y : T ↦ log ((y.2 - a y) / s y)) z :=
    (analyticAt_log (div_pos (sub_pos.mpr hz.2.2) hz.2.1)).comp (g := log) (x := z)
      (((hx z).sub (aa z)).div (as' z) hz.2.1.ne')
  have hopen : IsOpen (support param) := by
    have he : support param = ({z | 0 < b z} ∩ {z | 0 < s z} ∩ {z | a z < z.2} : Set T) := by
      ext z
      simp [support, and_assoc, b, s, a]
      rfl
    rw [he]
    exact ((isOpen_lt continuous_const (continuous_iff_continuousAt.mpr
      (fun z ↦ (ab z).continuousAt))).inter
      (isOpen_lt continuous_const (continuous_iff_continuousAt.mpr
        (fun z ↦ (as' z).continuousAt)))).inter
      (isOpen_lt (continuous_iff_continuousAt.mpr (fun z ↦ (aa z).continuousAt)) continuous_snd)
  have ha : AnalyticOnNhd ℝ (density param) (support param) := by
    intro z hz
    have hlb : AnalyticAt ℝ (fun y : T ↦ log (b y)) z :=
      (analyticAt_log hz.1).comp (g := log) (x := z) (ab z)
    have hls : AnalyticAt ℝ (fun y : T ↦ log (s y)) z :=
      (analyticAt_log hz.2.1).comp (g := log) (x := z) (as' z)
    exact (((hlb.sub hls).sub ((analyticAt_const.add (ab z)).mul (hlog z hz))).sub
      (((ab z).neg.mul (hlog z hz)).rexp'))
  have hscore : AnalyticOnNhd ℝ sc (support param) := by
    intro z hz
    have hE := ((ab z).neg.mul (hlog z hz)).rexp'
    have hConst : AnalyticAt ℝ (fun _ : T ↦ (-1 : ℝ)) z := analyticAt_const
    exact ((hConst.sub (ab z)).add ((ab z).mul hE)).div
      ((hx z).sub (aa z)) (sub_pos.mpr hz.2.2).ne'
  refine AE3WeightedChart.ae_unique_class_of_analytic_score hn hopen ha.continuousOn hscore
    continuousOn_const (fun p t ht ↦ hasScore (b := (param p).1) ht.2.1 ht.2.2) ?_ param ?_ w hw
  · intro p q
    by_cases h : 0 < (param p).1 ∧ 0 < (param p).2.2 ∧
        0 < (param q).1 ∧ 0 < (param q).2.2
    · convert (convex_Ioi (max (param p).2.1 (param q).2.1)).isPreconnected using 1
      ext t
      simp [support, h.1, h.2.1, h.2.2.1, h.2.2.2, max_lt_iff]
    · convert isPreconnected_empty using 1
      ext t
      simp only [mem_ofPred_eq, mem_empty_iff_false, iff_false]
      intro ht
      exact h ⟨ht.1.1, ht.1.2.1, ht.2.1, ht.2.2.1⟩
  · rintro p q hne ⟨t, hp, hq⟩
    by_contra! h
    obtain ⟨h₁, h₂, h₃⟩ := identify (a := (param p).2.1) (a' := (param q).2.1)
      hp.1 hp.2.1 hq.1 hq.2.1 (fun t ht ↦
      h t ⟨hp.1, hp.2.1, (le_max_left _ _).trans_lt ht⟩
        ⟨hq.1, hq.2.1, (le_max_right _ _).trans_lt ht⟩)
    exact hne (Prod.ext h₁ (Prod.ext h₂ h₃))

/-- The fitted chart includes shape one and omits shapes below one. -/
def fittedDomain {P : Type*} (param : P → Parameters) : Set (P × (Fin n → ℝ)) :=
  {z | 1 ≤ (param z.1).1 ∧ (∀ i, (z.1, z.2 i) ∈ support param)}

private theorem transfer {P : Type*} (param : P → Parameters)
    (chart : (Fin m → ℝ) → P) (hchart : ∀ p, 1 ≤ (param (chart p)).1)
    (hsurj : ∀ p, 1 ≤ (param p).1 → ∃ q, chart q = p)
    (hinj : Function.Injective param) (w : Fin n → ℕ)
    (h : ∀ᵐ x : Fin n → ℝ ∂volume, ∀ a b,
      UniqueAE.Winner (domain (support (param ∘ chart)))
        (likelihood (density (param ∘ chart)) (fun _ ↦ 0) w) x a →
      UniqueAE.Winner (domain (support (param ∘ chart)))
        (likelihood (density (param ∘ chart)) (fun _ ↦ 0) w) x b →
      (param ∘ chart) a = (param ∘ chart) b) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ a b,
      UniqueAE.Winner (fittedDomain param) (likelihood (density param) (fun _ ↦ 0) w) x a →
      UniqueAE.Winner (fittedDomain param) (likelihood (density param) (fun _ ↦ 0) w) x b →
      a = b := by
  filter_upwards [h] with x hx a b ha hb
  have lift (p : P)
      (hp : UniqueAE.Winner (fittedDomain param) (likelihood (density param) (fun _ ↦ 0) w) x p) :
      ∃ r, chart r = p ∧ UniqueAE.Winner (domain (support (param ∘ chart)))
        (likelihood (density (param ∘ chart)) (fun _ ↦ 0) w) x r := by
    obtain ⟨r, hr⟩ := hsurj p hp.1.1
    refine ⟨r, hr, ?_, ?_⟩
    · intro i
      change (chart r, x i) ∈ support param
      rw [hr]
      exact hp.1.2 i
    · intro q hq
      have hfit : (chart q, x) ∈ fittedDomain param := ⟨hchart q, hq⟩
      have he : likelihood (density (param ∘ chart)) (fun _ ↦ 0) w r x =
          likelihood (density param) (fun _ ↦ 0) w p x := by
        have heq : likelihood (density (param ∘ chart)) (fun _ ↦ 0) w r x =
            likelihood (density param) (fun _ ↦ 0) w (chart r) x := rfl
        exact heq.trans (congrArg (fun p ↦ likelihood (density param) (fun _ ↦ 0) w p x) hr)
      rw [he]
      exact hp.2 (chart q) hfit
  obtain ⟨r, hr, hfr⟩ := lift a ha
  obtain ⟨s, hs, hfs⟩ := lift b hb
  have he := hx r s hfr hfs
  change param (chart r) = param (chart s) at he
  exact hr.symm.trans ((hinj he).trans hs)

/-- With shape at least one and all coefficients free, finite Fréchet winners
agree almost everywhere on each fixed-count stratum with at least seven levels. -/
theorem all_free_ae_unique (hn : 7 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (fittedDomain id) (likelihood (density id) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (fittedDomain id) (likelihood (density id) (fun _ ↦ 0) w) x q →
      p = q := by
  let chart (p : Fin 3 → ℝ) : Parameters := (1 + p 0 ^ 2, p 1, p 2)
  refine transfer id chart (fun p ↦ le_add_of_nonneg_right (sq_nonneg (p 0))) ?_
    Function.injective_id w ?_
  · intro p hp
    change 1 ≤ p.1 at hp
    let q : Fin 3 → ℝ := ![sqrt (p.1 - 1), p.2.1, p.2.2]
    refine ⟨q, Prod.ext ?_ rfl⟩
    change 1 + sqrt (p.1 - 1) ^ 2 = p.1
    rw [sq_sqrt (sub_nonneg.mpr hp)]
    ring
  · apply chart_unique _ ?_ ?_ ?_ (by omega) w hw
    · intro p
      exact analyticAt_const.add
        (((ContinuousLinearMap.proj (0 : Fin 3) : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt p).pow 2)
    · exact fun p ↦ (ContinuousLinearMap.proj (1 : Fin 3) : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt p
    · exact fun p ↦ (ContinuousLinearMap.proj (2 : Fin 3) : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt p

/-- With scale held and shape at least one, finite Fréchet winners
agree almost everywhere on each fixed-count stratum with at least five levels. -/
theorem held_sigma_ae_unique (s : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : ℝ × ℝ → Parameters := fun p ↦ (p.1, p.2, s)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (fittedDomain param) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (fittedDomain param) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q := by
  let param : ℝ × ℝ → Parameters := fun p ↦ (p.1, p.2, s)
  let chart (p : Fin 2 → ℝ) : ℝ × ℝ := (1 + p 0 ^ 2, p 1)
  refine transfer param chart (fun p ↦ le_add_of_nonneg_right (sq_nonneg (p 0))) ?_ ?_ w ?_
  · intro p hp
    let q : Fin 2 → ℝ := ![sqrt (p.1 - 1), p.2]
    refine ⟨q, Prod.ext ?_ rfl⟩
    change 1 + sqrt (p.1 - 1) ^ 2 = p.1
    rw [sq_sqrt (sub_nonneg.mpr hp)]
    ring
  · intro a b h
    have h₁ : a.1 = b.1 := congrArg (fun p : Parameters ↦ p.1) h
    have h₂ : a.2 = b.2 := congrArg (fun p : Parameters ↦ p.2.1) h
    exact Prod.ext h₁ h₂
  · apply chart_unique _ ?_ ?_ ?_ (by omega) w hw
    · intro p
      exact analyticAt_const.add
        (((ContinuousLinearMap.proj (0 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p).pow 2)
    · exact fun p ↦ (ContinuousLinearMap.proj (1 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p
    · exact fun _ ↦ analyticAt_const

end

end UnivariateML.AE3Frechet

end
