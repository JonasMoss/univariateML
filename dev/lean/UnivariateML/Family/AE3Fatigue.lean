module

public import UnivariateML.Family.AE2Fatigue
public import UnivariateML.Generic.AE3WeightedChart
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Tactic

set_option linter.style.header false

/-!
Five-level almost-everywhere uniqueness for finite Birnbaum–Saunders fits.
The chart uses strict support. Stationary-component and face-height
classifications are separate from the finite winner assertion.
-/

@[expose] public section

open Set Filter MeasureTheory Real
open scoped Topology

namespace UnivariateML.AE3Fatigue

noncomputable section

open AE2Fatigue (Coordinates)

variable {n : ℕ}

/-- The chart with fixed shape, in free scale and threshold coordinates. -/
def heldAlpha (a : ℝ) (p : Fin 2 → ℝ) : Coordinates := ⟨a, p 0, p 1⟩

/-- The chart with fixed scale, in free shape and threshold coordinates. -/
def heldBeta (b : ℝ) (p : Fin 2 → ℝ) : Coordinates := ⟨p 0, b, p 1⟩

private def support (param : (Fin 2 → ℝ) → Coordinates) :
    Set ((Fin 2 → ℝ) × ℝ) :=
  {z | 0 < (param z.1).shape ∧ 0 < (param z.1).scale ∧ (param z.1).threshold < z.2}

private def kernel (a b t : ℝ) : ℝ :=
  log (t + b) - 3 * log t / 2 - log b / 2 - log a -
    (t / b + b / t - 2) / (2 * a ^ 2)

private def score (a b t : ℝ) : ℝ :=
  1 / (t + b) - 3 / (2 * t) - (1 / b - b / t ^ 2) / (2 * a ^ 2)

private theorem score_hasDeriv {a b c x : ℝ} (ha : 0 < a) (hb : 0 < b) (hx : c < x) :
    HasDerivAt (fun y ↦ kernel a b (y - c)) (score a b (x - c)) x := by
  have ht := sub_pos.mpr hx
  have h := (hasDerivAt_id x).sub_const c
  have hl := (h.add_const b).log (add_pos ht hb).ne'
  have hl' := ((h.log ht.ne').const_mul 3).div_const 2
  have hr := (((h.div_const b).add ((hasDerivAt_const x b).div h ht.ne')).sub_const 2).div_const
    (2 * a ^ 2)
  convert ((((hl.sub hl').sub_const (log b / 2)).sub_const (log a)).sub hr) using 1
  · rfl
  · simp only [score, id_eq, zero_mul, zero_sub]
    field_simp [ht.ne', hb.ne', ha.ne', (add_pos ht hb).ne']; ring

private def numerator (a b t : ℝ) : ℝ :=
  t ^ 2 - (3 / 2) * t * (t + b) - t ^ 2 * (t + b) / (b * (2 * a ^ 2)) +
    b * (t + b) / (2 * a ^ 2)

private def denominator (b t : ℝ) : ℝ := t ^ 2 * (t + b)

private theorem score_cleared {a b t : ℝ} (ha : 0 < a) (hb : 0 < b) (ht : 0 < t) :
    score a b t * denominator b t = numerator a b t := by
  unfold score numerator denominator
  field_simp [ht.ne', ha.ne', hb.ne', (add_pos ht hb).ne']
  ring

private theorem analytic_identity {f : ℝ → ℝ} (h : AnalyticOnNhd ℝ f univ)
    (c : ℝ) (hz : ∀ x > c, f x = 0) : ∀ x, f x = 0 := by
  have hm : Ioi c ∈ 𝓝 (c + 1) := Ioi_mem_nhds (by linarith)
  have he : f =ᶠ[𝓝 (c + 1)] 0 := by
    filter_upwards [hm] with x hx
    exact hz x hx
  exact fun x ↦ h.eqOn_zero_of_preconnected_of_eventuallyEq_zero
    isPreconnected_univ (mem_univ _) he (mem_univ x)

private theorem identify {a b c a' b' c' : ℝ}
    (ha : 0 < a) (hb : 0 < b) (ha' : 0 < a') (hb' : 0 < b')
    (he : ∀ x > max c c', score a b (x - c) = score a' b' (x - c')) :
    a = a' ∧ b = b' ∧ c = c' := by
  have ht (x : ℝ) (hx : max c c' < x) : 0 < x - c :=
    sub_pos.mpr ((le_max_left _ _).trans_lt hx)
  have ht' (x : ℝ) (hx : max c c' < x) : 0 < x - c' :=
    sub_pos.mpr ((le_max_right _ _).trans_lt hx)
  have hcleared : ∀ x, numerator a b (x - c) * denominator b' (x - c') =
      numerator a' b' (x - c') * denominator b (x - c) := by
    have h := analytic_identity
      (f := fun x ↦ numerator a b (x - c) * denominator b' (x - c') -
        numerator a' b' (x - c') * denominator b (x - c)) ?_ (max c c') ?_
    · exact fun x ↦ sub_eq_zero.mp (h x)
    · intro x _
      unfold numerator denominator
      simp only [div_eq_mul_inv, mul_inv_rev]
      have ca : AnalyticAt ℝ (fun _ : ℝ ↦ (a ^ 2)⁻¹) x := analyticAt_const
      have ca' : AnalyticAt ℝ (fun _ : ℝ ↦ (a' ^ 2)⁻¹) x := analyticAt_const
      have cb : AnalyticAt ℝ (fun _ : ℝ ↦ b⁻¹) x := analyticAt_const
      have cb' : AnalyticAt ℝ (fun _ : ℝ ↦ b'⁻¹) x := analyticAt_const
      fun_prop
    · intro x hx
      rw [← score_cleared ha hb (ht x hx), ← score_cleared ha' hb' (ht' x hx), he x hx]
      ring
  have hc : c = c' := by
    rcases lt_trichotomy c c' with h | h | h
    · have hpos : 0 < numerator a' b' 0 := by
        simp [numerator]
        positivity
      have hd : 0 < denominator b (c' - c) := by
        unfold denominator
        have := sub_pos.mpr h
        positivity
      have hh := hcleared c'
      simp only [sub_self, show denominator b' 0 = 0 by simp [denominator], mul_zero] at hh
      exact ((mul_pos hpos hd).ne' hh.symm).elim
    · exact h
    · have hpos : 0 < numerator a b 0 := by
        simp [numerator]
        positivity
      have hd : 0 < denominator b' (c - c') := by
        unfold denominator
        have := sub_pos.mpr h
        positivity
      have hh := hcleared c
      simp only [sub_self, show denominator b 0 = 0 by simp [denominator], mul_zero] at hh
      exact ((mul_pos hpos hd).ne' hh).elim
  subst c'
  have hred : ∀ t, numerator a b t * (t + b') = numerator a' b' t * (t + b) := by
    have h := analytic_identity
      (f := fun t ↦ numerator a b t * (t + b') - numerator a' b' t * (t + b)) ?_ 0 ?_
    · exact fun t ↦ sub_eq_zero.mp (h t)
    · intro x _
      unfold numerator
      simp only [div_eq_mul_inv, mul_inv_rev]
      have ca : AnalyticAt ℝ (fun _ : ℝ ↦ (a ^ 2)⁻¹) x := analyticAt_const
      have ca' : AnalyticAt ℝ (fun _ : ℝ ↦ (a' ^ 2)⁻¹) x := analyticAt_const
      have cb : AnalyticAt ℝ (fun _ : ℝ ↦ b⁻¹) x := analyticAt_const
      have cb' : AnalyticAt ℝ (fun _ : ℝ ↦ b'⁻¹) x := analyticAt_const
      fun_prop
    · intro t ht
      have hh := hcleared (t + c)
      rw [add_sub_cancel_right] at hh
      unfold denominator at hh
      apply (mul_right_cancel₀ (pow_ne_zero 2 ht.ne') (show
        (numerator a b t * (t + b') - numerator a' b' t * (t + b)) * t ^ 2 = 0 * t ^ 2 by
          linear_combination hh))
  have hbb : b = b' := by
    have hh := hred (-b)
    have hnum : numerator a b (-b) = b ^ 2 := by simp [numerator]
    rw [hnum, neg_add_cancel, mul_zero] at hh
    linarith [(mul_eq_zero.mp hh).resolve_left (pow_ne_zero 2 hb.ne')]
  subst b'
  have hh := hred 0
  simp only [numerator, zero_pow (by decide : 2 ≠ 0), zero_mul, zero_add, zero_div,
    sub_zero] at hh
  field_simp [ha.ne', ha'.ne'] at hh
  have hsq : a' ^ 2 = a ^ 2 := by
    apply mul_left_cancel₀ (show b ^ 2 ≠ 0 from pow_ne_zero 2 hb.ne')
    linear_combination hh
  exact ⟨by nlinarith [sq_nonneg (a - a')], rfl, rfl⟩

private theorem score_analytic {E : Type*} [NormedAddCommGroup E] [NormedSpace ℝ E]
    {a b t : E → ℝ} {z : E} (ha : AnalyticAt ℝ a z) (hb : AnalyticAt ℝ b z)
    (ht : AnalyticAt ℝ t z) (pa : 0 < a z) (pb : 0 < b z) (pt : 0 < t z) :
    AnalyticAt ℝ (fun y ↦ score (a y) (b y) (t y)) z := by
  have hab : 2 * a z ^ 2 ≠ 0 := by positivity
  exact ((analyticAt_const.div (ht.add hb) (add_pos pt pb).ne').sub
    (analyticAt_const.div (analyticAt_const.mul ht) (mul_pos (by norm_num) pt).ne')).sub
    (((analyticAt_const.div hb pb.ne').sub (hb.div (ht.pow 2) (pow_ne_zero _ pt.ne'))).div
      (analyticAt_const.mul (ha.pow 2)) hab)

private theorem kernel_analytic {E : Type*} [NormedAddCommGroup E] [NormedSpace ℝ E]
    {a b t : E → ℝ} {z : E} (ha : AnalyticAt ℝ a z) (hb : AnalyticAt ℝ b z)
    (ht : AnalyticAt ℝ t z) (pa : 0 < a z) (pb : 0 < b z) (pt : 0 < t z) :
    AnalyticAt ℝ (fun y ↦ kernel (a y) (b y) (t y)) z := by
  have hab : 2 * a z ^ 2 ≠ 0 := by positivity
  have htwo : AnalyticAt ℝ (fun _ : E ↦ (2 : ℝ)) z := analyticAt_const
  have hthree : AnalyticAt ℝ (fun _ : E ↦ (3 : ℝ)) z := analyticAt_const
  have htb : AnalyticAt ℝ (fun y ↦ log (t y + b y)) z :=
    (analyticAt_log (add_pos pt pb)).comp (g := log) (x := z) (ht.add hb)
  have hlogt : AnalyticAt ℝ (fun y ↦ log (t y)) z :=
    (analyticAt_log pt).comp (g := log) (x := z) ht
  have hlogb : AnalyticAt ℝ (fun y ↦ log (b y)) z :=
    (analyticAt_log pb).comp (g := log) (x := z) hb
  have hloga : AnalyticAt ℝ (fun y ↦ log (a y)) z :=
    (analyticAt_log pa).comp (g := log) (x := z) ha
  have hlog := ((htb).sub
    ((hthree.mul (hlogt)).div htwo (by norm_num))).sub
    ((hlogb).div htwo (by norm_num))
  have hrat := ((((ht.div hb pb.ne').add (hb.div ht pt.ne')).sub htwo).div
    (htwo.mul (ha.pow 2)) hab)
  exact (hlog.sub (hloga)).sub hrat

set_option maxHeartbeats 650000 in
-- Elaboration combines the analytic chart, its scalar score, and the incidence application.
private theorem chart_unique (param : (Fin 2 → ℝ) → Coordinates)
    (hinj : Function.Injective param)
    (ha : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).shape) p)
    (hb : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).scale) p)
    (hc : ∀ p, AnalyticAt ℝ (fun q ↦ (param q).threshold) p)
    (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (AE2Fatigue.domain param) (AE2Fatigue.likelihood param w) x p →
      UniqueAE.Winner (AE2Fatigue.domain param) (AE2Fatigue.likelihood param w) x q →
      p = q := by
  let f (z : (Fin 2 → ℝ) × ℝ) :=
    kernel (param z.1).shape (param z.1).scale (z.2 - (param z.1).threshold)
  have hf : AnalyticOnNhd ℝ f (support param) := by
    intro z hz
    have hs := (ContinuousLinearMap.fst ℝ (Fin 2 → ℝ) ℝ).analyticAt z
    have ht := (ContinuousLinearMap.snd ℝ (Fin 2 → ℝ) ℝ).analyticAt z
    exact kernel_analytic ((ha z.1).comp hs) ((hb z.1).comp hs)
      (ht.sub ((hc z.1).comp hs)) hz.1 hz.2.1 (sub_pos.mpr hz.2.2)
  have ho : IsOpen (support param) := by
    have ca : Continuous (fun z : (Fin 2 → ℝ) × ℝ ↦ (param z.1).shape) :=
      (continuous_iff_continuousAt.mpr fun p ↦ (ha p).continuousAt).comp continuous_fst
    have cb : Continuous (fun z : (Fin 2 → ℝ) × ℝ ↦ (param z.1).scale) :=
      (continuous_iff_continuousAt.mpr fun p ↦ (hb p).continuousAt).comp continuous_fst
    have cc : Continuous (fun z : (Fin 2 → ℝ) × ℝ ↦ (param z.1).threshold) :=
      (continuous_iff_continuousAt.mpr fun p ↦ (hc p).continuousAt).comp continuous_fst
    have he : support param = {z | 0 < (param z.1).shape} ∩ {z | 0 < (param z.1).scale} ∩
        {z | (param z.1).threshold < z.2} := by
      ext z
      simp [support, and_assoc]
    rw [he]
    exact ((isOpen_lt continuous_const ca).inter (isOpen_lt continuous_const cb)).inter
      (isOpen_lt cc continuous_snd)
  let d (z : (Fin 2 → ℝ) × ℝ) :=
    score (param z.1).shape (param z.1).scale (z.2 - (param z.1).threshold)
  have hd : AnalyticOnNhd ℝ d (support param) := by
    intro z hz
    have hs := (ContinuousLinearMap.fst ℝ (Fin 2 → ℝ) ℝ).analyticAt z
    have ht := (ContinuousLinearMap.snd ℝ (Fin 2 → ℝ) ℝ).analyticAt z
    exact score_analytic ((ha z.1).comp hs) ((hb z.1).comp hs)
      (ht.sub ((hc z.1).comp hs)) hz.1 hz.2.1 (sub_pos.mpr hz.2.2)
  have hdiff (p : Fin 2 → ℝ) (t : ℝ) (ht : (p, t) ∈ support param) :
      HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t :=
    score_hasDeriv ht.1 ht.2.1 ht.2.2
  have hcommon (p q : Fin 2 → ℝ) :
      IsPreconnected {t | (p, t) ∈ support param ∧ (q, t) ∈ support param} := by
    by_cases h : 0 < (param p).shape ∧ 0 < (param p).scale ∧
        0 < (param q).shape ∧ 0 < (param q).scale
    · convert (convex_Ioi (max (param p).threshold (param q).threshold)).isPreconnected using 1
      ext t
      simp [support, h.1, h.2.1, h.2.2.1, h.2.2.2, max_lt_iff]
    · convert isPreconnected_empty using 1
      ext t
      simp only [mem_ofPred_eq, mem_empty_iff_false, iff_false]
      intro ht
      exact h ⟨ht.1.1, ht.1.2.1, ht.2.1, ht.2.2.1⟩
  have hident (p q : Fin 2 → ℝ) (hne : p ≠ q)
      (he : ∃ t, (p, t) ∈ support param ∧ (q, t) ∈ support param) :
      ∃ t, (p, t) ∈ support param ∧ (q, t) ∈ support param ∧ d (p, t) ≠ d (q, t) := by
    obtain ⟨t, hp, hq⟩ := he
    by_contra! h
    obtain ⟨h₁, h₂, h₃⟩ := identify (c := (param p).threshold) (c' := (param q).threshold)
      hp.1 hp.2.1 hq.1 hq.2.1 (fun t ht ↦
      h t ⟨hp.1, hp.2.1, (le_max_left _ _).trans_lt ht⟩
        ⟨hq.1, hq.2.1, (le_max_right _ _).trans_lt ht⟩)
    apply hne (hinj ?_)
    exact Coordinates.mk.injEq _ _ _ _ _ _ |>.mpr ⟨h₁, h₂, h₃⟩
  have hh := AE3WeightedChart.ae_unique_of_analytic_score (by omega : 2 + 2 < n)
    (c := fun _ ↦ 0) ho hf.continuousOn hd continuousOn_const hdiff hcommon hident w hw
  have hdom : AE3WeightedChart.domain (n := n) (support param) = AE2Fatigue.domain param := by
    ext z
    simp only [AE3WeightedChart.domain, support, AE2Fatigue.domain, mem_ofPred_eq]
    refine ⟨fun hz ↦ ?_, fun hz i ↦ ⟨hz.1, hz.2.1, hz.2.2 i⟩⟩
    let i : Fin n := ⟨0, by omega⟩
    exact ⟨(hz i).1, (hz i).2.1, fun j ↦ (hz j).2.2⟩
  have hl : AE3WeightedChart.likelihood f (fun _ ↦ 0) w = AE2Fatigue.likelihood param w := by
    funext p x
    unfold AE3WeightedChart.likelihood AE2Fatigue.likelihood
    simp only [zero_add, f, kernel]
  rw [hdom, hl] at hh
  exact hh

/-- With shape held, finite winners agree for almost every fixed-count five-level or larger
stratum. -/
theorem held_alpha_ae_unique (a : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (AE2Fatigue.domain (heldAlpha a))
        (AE2Fatigue.likelihood (heldAlpha a) w) x p →
      UniqueAE.Winner (AE2Fatigue.domain (heldAlpha a))
        (AE2Fatigue.likelihood (heldAlpha a) w) x q →
      p = q := by
  refine chart_unique (heldAlpha a) ?_ (fun _ ↦ analyticAt_const)
    (fun p ↦ (ContinuousLinearMap.proj (0 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p)
    (fun p ↦ (ContinuousLinearMap.proj (1 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p) hn w hw
  intro p q h
  funext i
  fin_cases i
  · exact congrArg Coordinates.scale h
  · exact congrArg Coordinates.threshold h

/-- With scale held, finite winners agree for almost every fixed-count five-level or larger
stratum. -/
theorem held_beta_ae_unique (b : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (AE2Fatigue.domain (heldBeta b)) (AE2Fatigue.likelihood (heldBeta b) w) x p →
      UniqueAE.Winner (AE2Fatigue.domain (heldBeta b)) (AE2Fatigue.likelihood (heldBeta b) w) x q →
      p = q := by
  refine chart_unique (heldBeta b) ?_
    (fun p ↦ (ContinuousLinearMap.proj (0 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p)
      (fun _ ↦ analyticAt_const)
    (fun p ↦ (ContinuousLinearMap.proj (1 : Fin 2) : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt p) hn w hw
  intro p q h
  funext i
  fin_cases i
  · exact congrArg Coordinates.shape h
  · exact congrArg Coordinates.threshold h

end

end UnivariateML.AE3Fatigue

end
