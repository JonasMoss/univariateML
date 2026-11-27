module

public import UnivariateML.Generic.AE3WeightedChart
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Tactic

set_option linter.style.header false

/-! Five-level finite winner uniqueness for the two held PGW charts. -/

@[expose] public section

open Set Filter MeasureTheory Real
open scoped Topology
open UnivariateML.AE3WeightedChart (domain likelihood)

namespace UnivariateML.AE3PGW

noncomputable section

variable {n : ℕ}

/-- Shape, scale and exponent. -/
abbrev Parameters := ℝ × ℝ × ℝ

/-- Positivity conditions for the PGW density. -/
def support (param : (Fin 2 → ℝ) → Parameters) : Set ((Fin 2 → ℝ) × ℝ) :=
  {z | 0 < (param z.1).1 ∧ 0 < (param z.1).2.1 ∧ 0 < (param z.1).2.2 ∧ 0 < z.2}

/-- PGW log density written with exponential powers on the positive support. -/
def density (param : (Fin 2 → ℝ) → Parameters) (z : (Fin 2 → ℝ) × ℝ) : ℝ :=
  let (b, a, c) := param z.1
  let y := b * log (z.2 / a)
  let L := log (1 + exp y)
  log b + log c - log z.2 + y + (c - 1) * L - (exp (c * L) - 1)

private def functionV (c v : ℝ) : ℝ :=
  1 + (c - 1) * v / (1 + v) - c * v * exp ((c - 1) * log (1 + v))

private def score (b a c x : ℝ) : ℝ :=
  (b * functionV c (exp (b * log (x / a))) - 1) / x

private theorem hasScore {b a c x : ℝ} (ha : 0 < a) (hx : 0 < x) :
    HasDerivAt (fun y ↦ density (fun _ ↦ (b, a, c)) (0, y)) (score b a c x) x := by
  have hlog := (hasDerivAt_id x).log hx.ne'
  have hy := ((((hasDerivAt_id x).div_const a).log (div_ne_zero hx.ne' ha.ne')).const_mul b)
  have hL := ((hy.exp.const_add 1).log (by positivity : 1 + exp (b * log (x / a)) ≠ 0))
  have hh := (((hlog.const_sub (log b + log c)).add hy).add
    (hL.const_mul (c - 1))).sub (((hL.const_mul c).exp).sub_const 1)
  convert hh using 1
  · rfl
  · simp only [score, functionV, id_eq]
    have he : exp (c * log (1 + exp (b * log (x / a)))) =
        exp ((c - 1) * log (1 + exp (b * log (x / a)))) *
          (1 + exp (b * log (x / a))) := by
      rw [show c * log (1 + exp (b * log (x / a))) =
        (c - 1) * log (1 + exp (b * log (x / a))) + log (1 + exp (b * log (x / a))) by ring,
        exp_add, exp_log (by positivity)]
    rw [he]
    field_simp [hx.ne', ha.ne', show 1 + exp (b * log (x / a)) ≠ 0 by positivity]
    ring

private theorem functionV_deriv_zero (c : ℝ) : HasDerivAt (functionV c) (-1) 0 := by
  have hv := hasDerivAt_id (0 : ℝ)
  have h1 := hv.const_add 1
  have he := ((h1.log (by norm_num)).const_mul (c - 1)).exp
  convert ((((hv.const_mul (c - 1)).div h1 (by norm_num)).const_add 1).sub
    ((hv.const_mul c).mul he)) using 1
  · rfl
  · norm_num

private theorem functionV_one_inj {c d : ℝ} (hc : 0 < c) (hd : 0 < d)
    (h : functionV c 1 = functionV d 1) : c = d := by
  have strict (a b : ℝ) (ha : 0 < a) (hab : a < b) : functionV b 1 < functionV a 1 := by
    have he : exp ((a - 1) * log 2) < exp ((b - 1) * log 2) :=
      exp_lt_exp.mpr (mul_lt_mul_of_pos_right (sub_lt_sub_right hab _) (log_pos (by norm_num)))
    have hE : 0 < exp ((a - 1) * log 2) - 1 / 2 := by
      have hzero : exp ((0 - 1) * log 2) = 1 / 2 := by
        rw [zero_sub, neg_mul, one_mul, exp_neg, exp_log (by norm_num), one_div]
      rw [sub_pos, ← hzero]
      exact exp_lt_exp.mpr (mul_lt_mul_of_pos_right (sub_lt_sub_right ha _) (log_pos (by norm_num)))
    have hm := (mul_lt_mul_of_pos_left (sub_lt_sub_right he (1 / 2)) ha).trans_le
      (mul_le_mul_of_nonneg_right hab.le (hE.trans (sub_lt_sub_right he (1 / 2))).le)
    unfold functionV
    norm_num at *
    nlinarith
  rcases lt_trichotomy c d with hlt | he | hgt
  · exact ((strict c d hc hlt).ne h.symm).elim
  · exact he
  · exact ((strict d c hd hgt).ne h).elim

private theorem identify {b a c b' a' c' : ℝ}
    (hb : 0 < b) (ha : 0 < a) (hc : 0 < c)
    (hb' : 0 < b') (ha' : 0 < a') (hc' : 0 < c')
    (he : ∀ x > 0, score b a c x = score b' a' c' x) :
    b = b' ∧ a = a' ∧ c = c' := by
  have he' (x : ℝ) (hx : 0 < x) :
      b * functionV c (exp (b * log (x / a))) =
      b' * functionV c' (exp (b' * log (x / a'))) :=
    sub_left_inj.mp ((div_left_inj₀ hx.ne').mp (he x hx))
  have ht (b a c : ℝ) (hb : 0 < b) :
      Tendsto (fun t : ℝ ↦ b * functionV c (exp (b * (t - log a)))) atBot (𝓝 b) := by
    have h := tendsto_exp_atBot.comp
      ((tendsto_id.atBot_add (tendsto_const_nhds (x := -log a))).const_mul_atBot hb)
    have hh := ((functionV_deriv_zero c).continuousAt.tendsto.comp h).const_mul b
    simpa only [Function.comp_apply, sub_eq_add_neg, functionV, mul_zero, zero_div,
      sub_zero, add_zero, mul_one, neg_zero, id_eq, zero_mul] using hh
  have hbb := tendsto_nhds_unique_of_eventuallyEq (ht b a c hb) (ht b' a' c' hb')
    (Eventually.of_forall fun t ↦ by
      have hh := he' (exp t) (exp_pos t)
      rw [log_div (exp_ne_zero _) ha.ne', log_div (exp_ne_zero _) ha'.ne', log_exp] at hh
      exact hh)
  subst b'
  let A := exp (-b * log a)
  let A' := exp (-b * log a')
  have hv (v : ℝ) (hv : 0 < v) : functionV c (A * v) = functionV c' (A' * v) := by
    let x := exp (log v / b)
    have hh := mul_left_cancel₀ hb.ne' (he' x (exp_pos _))
    dsimp [x] at hh
    rw [log_div (exp_ne_zero _) ha.ne', log_div (exp_ne_zero _) ha'.ne', log_exp] at hh
    have he (t : ℝ) : exp (b * (log v / b - log t)) = exp (-b * log t) * v := by
      rw [mul_sub, mul_div_cancel₀ _ hb.ne', exp_sub, exp_log hv, neg_mul,
        exp_neg, div_eq_mul_inv, mul_comm]
    rwa [he, he] at hh
  have hD : -A = -A' := by
    have h₁ := HasDerivAt.comp (0 : ℝ) (h := fun v : ℝ ↦ A * v) (h₂ := functionV c) (h₂' := -1)
      (by simpa using functionV_deriv_zero c)
      ((hasDerivAt_id (0 : ℝ)).const_mul A)
    have h₂ := HasDerivAt.comp (0 : ℝ) (h := fun v : ℝ ↦ A' * v) (h₂ := functionV c') (h₂' := -1)
      (by simpa using functionV_deriv_zero c')
      ((hasDerivAt_id (0 : ℝ)).const_mul A')
    have he : (fun v ↦ functionV c (A * v)) =ᶠ[nhdsWithin 0 (Ici 0)]
        (fun v ↦ functionV c' (A' * v)) := by
      filter_upwards [self_mem_nhdsWithin] with v hv'
      rcases eq_or_lt_of_le (show 0 ≤ v from hv') with h | h
      · simp [← h, functionV]
      · exact hv v h
    have hd : UniqueDiffWithinAt ℝ (Ici (0 : ℝ)) 0 :=
      uniqueDiffOn_Ici _ _ (by simp)
    have h₁' : HasDerivWithinAt (fun v ↦ functionV c (A * v)) (-A) (Ici 0) 0 := by
      simpa [Function.comp_def, id_eq] using h₁.hasDerivWithinAt (s := Ici 0)
    have h₂' : HasDerivWithinAt (fun v ↦ functionV c' (A' * v)) (-A') (Ici 0) 0 := by
      simpa [Function.comp_def, id_eq] using h₂.hasDerivWithinAt (s := Ici 0)
    exact (h₁'.derivWithin hd).symm.trans
      ((h₂'.congr_of_eventuallyEq he (by simp [functionV])).derivWithin hd)
  have haa : a = a' := by
    have he := exp_injective (neg_injective hD)
    exact strictMonoOn_log.injOn ha ha'
      (mul_left_cancel₀ (neg_ne_zero.mpr hb.ne') he)
  subst a'
  have hh := mul_left_cancel₀ hb.ne' (he' a ha)
  rw [div_self ha.ne', log_one, mul_zero, exp_zero] at hh
  exact ⟨rfl, rfl, functionV_one_inj hc hc' hh⟩

set_option maxHeartbeats 650000 in
-- Elaboration combines the analytic chart, its scalar score, and the incidence application.
private theorem chart_unique (param : (Fin 2 → ℝ) → Parameters)
    (hinj : Function.Injective param)
    (h₁ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).1) p)
    (h₂ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).2.1) p)
    (h₃ : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).2.2) p)
    (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q := by
  let T := (Fin 2 → ℝ) × ℝ
  let b (z : T) := (param z.1).1
  let a (z : T) := (param z.1).2.1
  let c (z : T) := (param z.1).2.2
  let sc (z : T) := score (b z) (a z) (c z) z.2
  have ab (z : T) : AnalyticAt ℝ b z :=
    (h₁ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin 2 → ℝ) ℝ).analyticAt z)
  have aa (z : T) : AnalyticAt ℝ a z :=
    (h₂ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin 2 → ℝ) ℝ).analyticAt z)
  have ac (z : T) : AnalyticAt ℝ c z :=
    (h₃ z.1).comp ((ContinuousLinearMap.fst ℝ (Fin 2 → ℝ) ℝ).analyticAt z)
  have ax (z : T) := (ContinuousLinearMap.snd ℝ (Fin 2 → ℝ) ℝ).analyticAt z
  let y (z : T) := b z * log (z.2 / a z)
  let v (z : T) := exp (y z)
  let L (z : T) := log (1 + v z)
  have ay (z : T) (hz : z ∈ support param) : AnalyticAt ℝ y z :=
    (ab z).mul ((analyticAt_log (div_pos hz.2.2.2 hz.2.1)).comp (g := log) (x := z)
      ((ax z).div (aa z) hz.2.1.ne'))
  have av (z : T) (hz : z ∈ support param) : AnalyticAt ℝ v z := (ay z hz).rexp'
  have aL (z : T) (hz : z ∈ support param) : AnalyticAt ℝ L z :=
    (analyticAt_log (show 0 < 1 + v z by dsimp [v]; positivity)).comp (g := log) (x := z)
      (analyticAt_const.add (av z hz))
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
    exact (((((hlogb.add hlogc).sub hlogx).add (ay z hz)).add
      (((ac z).sub analyticAt_const).mul (aL z hz))).sub
      (((ac z).mul (aL z hz)).rexp'.sub analyticAt_const))
  have hscore : AnalyticOnNhd ℝ sc (support param) := by
    intro z hz
    have hne : 1 + v z ≠ 0 := by dsimp [v]; positivity
    have h₁ := ((ac z).sub (analyticAt_const (v := (1 : ℝ))))
    have h₂ := ((h₁.mul (av z hz)).div (analyticAt_const.add (av z hz)) hne)
    have h₃ := ((ac z).mul (av z hz)).mul ((h₁.mul (aL z hz)).rexp')
    exact (((ab z).mul ((analyticAt_const.add h₂).sub h₃)).sub analyticAt_const).div
      (ax z) hz.2.2.2.ne'
  refine AE3WeightedChart.ae_unique_of_analytic_score (by omega : 2 + 2 < n)
    hopen ha.continuousOn hscore continuousOn_const
    (fun p t ht ↦ hasScore (b := (param p).1) (c := (param p).2.2) ht.2.1 ht.2.2.2)
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
    obtain ⟨h₁, h₂, h₃⟩ := identify hp.1 hp.2.1 hp.2.2.1 hq.1 hq.2.1 hq.2.2.1 (fun t ht ↦
      h t ⟨hp.1, hp.2.1, hp.2.2.1, ht⟩ ⟨hq.1, hq.2.1, hq.2.2.1, ht⟩)
    exact hne (hinj (Prod.ext h₁ (Prod.ext h₂ h₃)))

/-- With scale held, at most one finite PGW winner exists on almost
every fixed-count stratum with five or more levels. -/
theorem held_scale_ae_unique (a : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : (Fin 2 → ℝ) → Parameters := fun p ↦ (p 0, a, p 1)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q := by
  dsimp only
  apply chart_unique _ ?_ ?_ ?_ ?_ hn w hw
  · intro p q h
    funext i
    fin_cases i
    · exact congrArg Prod.fst h
    · exact congrArg (fun z : Parameters ↦ z.2.2) h
  · exact fun p ↦ (ContinuousLinearMap.proj (0 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p
  · exact fun _ ↦ analyticAt_const
  · exact fun p ↦ (ContinuousLinearMap.proj (1 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p

/-- With shape held, at most one finite PGW winner exists on almost
every fixed-count stratum with five or more levels. -/
theorem held_shape_ae_unique (b : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : (Fin 2 → ℝ) → Parameters := fun p ↦ (b, p 0, p 1)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q := by
  dsimp only
  apply chart_unique _ ?_ ?_ ?_ ?_ hn w hw
  · intro p q h
    funext i
    fin_cases i
    · exact congrArg (fun z : Parameters ↦ z.2.1) h
    · exact congrArg (fun z : Parameters ↦ z.2.2) h
  · exact fun _ ↦ analyticAt_const
  · exact fun p ↦ (ContinuousLinearMap.proj (0 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p
  · exact fun p ↦ (ContinuousLinearMap.proj (1 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p

end

end UnivariateML.AE3PGW

end
