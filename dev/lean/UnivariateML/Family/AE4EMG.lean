module

public import UnivariateML.Family.AE1EMG
public import UnivariateML.Generic.AE4Analytic
public import UnivariateML.Generic.AE4Gaussian

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Finite EMG winners on fixed-count charts

## Deviations

These comparisons concern finite parameters with freely varying levels
and any finite collection of fixed observations. Stationary lists and
attainment criteria are separate.
-/

@[expose] public section

open Set Filter MeasureTheory ContinuousLinearMap Real
open scoped Topology
open UnivariateML.SkewLSGaussian UnivariateML.AE4Gaussian

namespace UnivariateML.AE4EMG

noncomputable section

private def arg (m s r t : ℝ) := (t-m)/s-r*s

private def score (m s r t : ℝ) :=
  -r + (φ (arg m s r t) / Φ (arg m s r t)) / s

private theorem arg_atTop (m r : ℝ) {s : ℝ} (hs : 0 < s) :
    Tendsto (arg m s r) atTop atTop := by
  unfold arg
  have h := (tendsto_atTop_add_const_right atTop (-m) tendsto_id)
  simpa only [sub_eq_add_neg, div_eq_mul_inv, id_eq] using
    (tendsto_atTop_add_const_right atTop (-(r*s)) (h.atTop_mul_const (inv_pos.mpr hs)))

private theorem log_cdf_atTop : Tendsto (fun t ↦ log (Φ t)) atTop (𝓝 0) := by
  simpa only [log_one] using cdf_atTop.log (by norm_num : (1 : ℝ) ≠ 0)

private theorem phi_atTop : Tendsto φ atTop (𝓝 0) := by
  have h := (tendsto_id.atTop_pow₀ (by decide : (2 : ℕ) ≠ 0) :
    Tendsto (fun t : ℝ ↦ t^2) atTop atTop).atTop_mul_const
      (show (0 : ℝ) < 1/2 by norm_num)
  have hp := (tendsto_exp_neg_atTop_nhds_zero.comp h).const_mul (1 / sqrt (2*π))
  rw [mul_zero] at hp
  apply hp.congr
  intro x
  simp [phi_formula, div_eq_mul_inv]

private theorem score_atTop (m r : ℝ) {s : ℝ} (hs : 0 < s) :
    Tendsto (score m s r) atTop (𝓝 (-r)) := by
  have harg := arg_atTop m r hs
  have h := ((phi_atTop.comp harg).div (cdf_atTop.comp harg) (by norm_num)).div_const s
  have h' := h.const_add (-r)
  rw [zero_div, zero_div, add_zero] at h'
  exact h'

private theorem score_identifies {m s r m' s' r' : ℝ} (hs : 0 < s) (hs' : 0 < s')
    (he : ∀ t, score m s r t = score m' s' r' t) : m = m' ∧ s = s' ∧ r = r' := by
  have hr : r = r' := neg_inj.mp (tendsto_nhds_unique (score_atTop m r hs)
    ((score_atTop m' r' hs').congr (fun t ↦ (he t).symm)))
  subst r'
  let F (t : ℝ) := log (Φ (arg m s r t)) - log (Φ (arg m' s' r t))
  have dx (a b : ℝ) (t : ℝ) :
      HasDerivAt (fun u ↦ log (Φ (arg a b r u)))
        ((φ (arg a b r t) / Φ (arg a b r t)) / b) t := by
    have harg : HasDerivAt (arg a b r) (1/b) t :=
      (((hasDerivAt_id t).sub_const a).div_const b).sub_const (r*b)
    simpa only [div_eq_mul_inv, one_mul, Function.comp_def] using
      (((cdf_hasDerivAt _).log (cdf_pos _).ne').comp t harg)
  have hd (t : ℝ) : HasDerivAt F 0 t := by
    have h := he t
    unfold score at h
    have h' := add_left_cancel h
    have hd := (dx m s t).sub (dx m' s' t)
    rw [h', sub_self] at hd
    exact hd
  have hconst (t : ℝ) : F t = F 0 :=
    is_const_of_deriv_eq_zero (fun u ↦ (hd u).differentiableAt) (fun u ↦ (hd u).deriv) t 0
  have hlim : Tendsto F atTop (𝓝 0) := by
    simpa using (log_cdf_atTop.comp (arg_atTop m r hs)).sub
      (log_cdf_atTop.comp (arg_atTop m' r hs'))
  have hzero : F 0 = 0 :=
    tendsto_nhds_unique (tendsto_const_nhds (x := F 0)) (hlim.congr hconst)
  have ha (t : ℝ) : arg m s r t = arg m' s' r t := by
    apply cdf_strictMono.injective
    apply strictMonoOn_log.injOn (cdf_pos _) (cdf_pos _)
    exact sub_eq_zero.mp ((hconst t).trans hzero)
  have h₀ := ha 0
  have h₁ := ha 1
  unfold arg at h₀ h₁
  have hsc : 1/s = 1/s' := by
    linear_combination h₁ - h₀
  have hss : s = s' := by
    have h := (div_eq_div_iff hs.ne' hs'.ne').mp hsc
    linarith
  subst s'
  have hmm : m/s = m'/s := by
    simpa only [zero_sub, neg_div, sub_left_inj, neg_inj] using h₀
  exact ⟨(div_left_inj₀ hs.ne').mp hmm, rfl, rfl⟩

private theorem score_analytic {E : Type*} [NormedAddCommGroup E] [NormedSpace ℝ E]
    {m s r t : E → ℝ} {z : E}
    (hm : AnalyticAt ℝ m z) (hs : AnalyticAt ℝ s z) (hr : AnalyticAt ℝ r z)
    (ht : AnalyticAt ℝ t z) (hs₀ : s z ≠ 0) :
    AnalyticAt ℝ (fun y ↦ score (m y) (s y) (r y) (t y)) z := by
  have ha : AnalyticAt ℝ (fun y ↦ arg (m y) (s y) (r y) (t y)) z :=
    ((ht.sub hm).div hs hs₀).sub (hr.mul hs)
  exact hr.neg.add (((AnalyticAt.comp (analyticAt_phi _) ha).div
    (AnalyticAt.comp (analyticAt_Phi _) ha) (cdf_pos _).ne').div hs hs₀)


private def term (m s r t : ℝ) :=
  log r + (r*s)^2/2 - r*(t-m) + log (Φ (arg m s r t))

private theorem term_derivative (m s r t : ℝ) :
    HasDerivAt (term m s r) (score m s r t) t := by
  have hz : HasDerivAt (arg m s r) (1/s) t :=
    (((hasDerivAt_id t).sub_const m).div_const s).sub_const (r*s)
  have hΦ := ((cdf_hasDerivAt _).log (cdf_pos _).ne').comp t hz
  have hrem := ((((hasDerivAt_id t).sub_const m).const_mul r).const_sub
    (log r + (r*s)^2/2))
  convert hrem.add hΦ using 1
  · rfl
  · simp [score, div_eq_mul_inv]

variable {d n kfixed : ℕ}

private def component (j : Option (Fin d)) (c : ℝ) (p : Fin d → ℝ) : ℝ :=
  match j with
  | none => c
  | some i => p i

private theorem component_analytic (j : Option (Fin d)) (c : ℝ) (p : Fin d → ℝ) :
    AnalyticAt ℝ (component j c) p := by
  cases j with
  | none => exact analyticAt_const
  | some i => exact (proj i : (Fin d → ℝ) →L[ℝ] ℝ).analyticAt p

/-- The allowed parameters in a chart supplied by a location, a Gaussian
scale and a rate map. -/
def domain (s r : (Fin d → ℝ) → ℝ) : Set (Fin d → ℝ) :=
  {p | 0 < s p ∧ 0 < r p}

/-- A weighted EMG log likelihood with free levels `x` and fixed levels `y`. -/
def logLik (w : Fin n → ℕ) (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (m s r : ℝ) (x : Fin n → ℝ) : ℝ :=
  AE1EMG.logLik v m s r y + AE1EMG.logLik w m s r x

private theorem chart (hdn : d + d < n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (j k l : Option (Fin d)) (a b c : ℝ)
    (hinj : Function.Injective (fun p ↦ (component j a p, component k b p, component l c p))) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ domain (component k b) (component l c),
      ∀ q ∈ domain (component k b) (component l c),
        IsMaxOn (fun r ↦ logLik w v y (component j a r) (component k b r) (component l c r) x)
          (domain (component k b) (component l c)) p →
        IsMaxOn (fun r ↦ logLik w v y (component j a r) (component k b r) (component l c r) x)
          (domain (component k b) (component l c)) q → p = q := by
  let m := component j a
  let s := component k b
  let r := component l c
  have hm (p) : AnalyticAt ℝ m p := component_analytic j a p
  have hs (p) : AnalyticAt ℝ s p := component_analytic k b p
  have hr (p) : AnalyticAt ℝ r p := component_analytic l c p
  have hmc : Continuous m := continuous_iff_continuousAt.mpr (fun p ↦ (hm p).continuousAt)
  have hsc : Continuous s := continuous_iff_continuousAt.mpr (fun p ↦ (hs p).continuousAt)
  have hrc : Continuous r := continuous_iff_continuousAt.mpr (fun p ↦ (hr p).continuousAt)
  have hO : IsOpen (domain s r) := (isOpen_Ioi.preimage hsc).inter (isOpen_Ioi.preimage hrc)
  let T : Set ((Fin d → ℝ) × ℝ) := domain s r ×ˢ univ
  have hL : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ term (m z.1) (s z.1) (r z.1) z.2) T := by
    have hc (f : (Fin d → ℝ) → ℝ) (hf : Continuous f) :
        ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ f z.1) T :=
      (hf.comp continuous_fst).continuousOn
    have ht : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ z.2) T :=
      continuous_snd.continuousOn
    have harg : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ arg (m z.1) (s z.1) (r z.1) z.2) T :=
      ((ht.sub (hc m hmc)).div (hc s hsc) (fun _ hz ↦ hz.1.1.ne')).sub
        ((hc r hrc).mul (hc s hsc))
    have hlog := (hc r hrc).log (fun _ hz ↦ hz.1.2.ne')
    have hΦ := (cdf_continuous.comp_continuousOn harg).log (fun _ _ ↦ (cdf_pos _).ne')
    exact (((hlog.add (((hc r hrc).mul (hc s hsc)).pow 2 |>.div_const 2)).sub
      ((hc r hrc).mul (ht.sub (hc m hmc)))).add hΦ)
  apply AE4Analytic.ae_unique_analytic_scores_with_offset hdn w hw (domain s r) hO
    (fun p ↦ term (m p) (s p) (r p)) (fun p ↦ score (m p) (s p) (r p))
    (fun p ↦ AE1EMG.logLik v (m p) (s p) (r p) y) hL
  · unfold AE1EMG.logLik
    apply continuousOn_finsetSum Finset.univ
    intro i _
    have hmap : Continuous (fun p : Fin d → ℝ ↦ (p, y i)) := by fun_prop
    exact (hL.comp hmap.continuousOn (fun p (hp : p ∈ domain s r) ↦
      ⟨hp, mem_univ _⟩)).const_mul (v i : ℝ)
  · intro p hp t
    exact score_analytic ((hm p).comp analyticAt_fst) ((hs p).comp analyticAt_fst)
      ((hr p).comp analyticAt_fst) analyticAt_snd hp.1.ne'
  · intro p _ t
    exact term_derivative (m p) (s p) (r p) t
  · intro p hp q hq he
    obtain ⟨h₁,h₂,h₃⟩ := score_identifies hp.1 hq.1 he
    exact hinj (Prod.ext h₁ (Prod.ext h₂ h₃))

/-- Seven freely varying levels suffice for uniqueness of any attained
finite EMG fit almost everywhere. Parameter coordinates are location,
Gaussian scale, and rate, in that order. -/
theorem all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ) (hn : 7 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ domain (fun p : Fin 3 → ℝ ↦ p 1) (fun p ↦ p 2),
      ∀ q ∈ domain (fun p : Fin 3 → ℝ ↦ p 1) (fun p ↦ p 2),
        IsMaxOn (fun r ↦ logLik w v y (r 0) (r 1) (r 2) x)
          (domain (fun p : Fin 3 → ℝ ↦ p 1) (fun p ↦ p 2)) p →
        IsMaxOn (fun r ↦ logLik w v y (r 0) (r 1) (r 2) x)
          (domain (fun p : Fin 3 → ℝ ↦ p 1) (fun p ↦ p 2)) q → p = q := by
  apply chart (d := 3) (by omega) w hw v y (some 0) (some 1) (some 2) 0 0 0
  intro p q h
  funext i
  fin_cases i
  · exact congrArg Prod.fst h
  · exact congrArg (fun z ↦ z.2.1) h
  · exact congrArg (fun z ↦ z.2.2) h

/-- At held rate, any attained finite EMG fit is almost everywhere
unique on fixed-count charts with at least 5 freely varying levels. -/
theorem held_lambda_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (r : ℝ) (hn : 5 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ (domain (fun p : Fin 2 → ℝ ↦ p 1) (fun _ ↦ r)),
      ∀ q ∈ (domain (fun p : Fin 2 → ℝ ↦ p 1) (fun _ ↦ r)),
        IsMaxOn (fun p ↦ logLik w v y (p 0) (p 1) r x)
          (domain (fun p : Fin 2 → ℝ ↦ p 1) (fun _ ↦ r)) p →
        IsMaxOn (fun p ↦ logLik w v y (p 0) (p 1) r x)
          (domain (fun p : Fin 2 → ℝ ↦ p 1) (fun _ ↦ r)) q → p = q := by
  apply chart (d := 2) (by omega) w hw v y (some 0) (some 1) none 0 0 r
  intro p q h
  funext i
  fin_cases i
  · exact congrArg Prod.fst h
  · exact congrArg (fun z ↦ z.2.1) h

/-- At held location, any attained finite EMG fit is almost everywhere
unique on fixed-count charts with at least 5 freely varying levels. -/
theorem held_mu_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (m : ℝ) (hn : 5 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ (domain (fun p : Fin 2 → ℝ ↦ p 0) (fun p ↦ p 1)),
      ∀ q ∈ (domain (fun p : Fin 2 → ℝ ↦ p 0) (fun p ↦ p 1)),
        IsMaxOn (fun p ↦ logLik w v y m (p 0) (p 1) x)
          (domain (fun p : Fin 2 → ℝ ↦ p 0) (fun p ↦ p 1)) p →
        IsMaxOn (fun p ↦ logLik w v y m (p 0) (p 1) x)
          (domain (fun p : Fin 2 → ℝ ↦ p 0) (fun p ↦ p 1)) q → p = q := by
  apply chart (d := 2) (by omega) w hw v y none (some 0) (some 1) m 0 0
  intro p q h
  funext i
  fin_cases i
  · exact congrArg (fun z ↦ z.2.1) h
  · exact congrArg (fun z ↦ z.2.2) h

/-- At held Gaussian scale, any attained finite EMG fit is almost everywhere
unique on fixed-count charts with at least 5 freely varying levels. -/
theorem held_sigma_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (s : ℝ) (hn : 5 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ (domain (fun _ : Fin 2 → ℝ ↦ s) (fun p ↦ p 1)),
      ∀ q ∈ (domain (fun _ : Fin 2 → ℝ ↦ s) (fun p ↦ p 1)),
        IsMaxOn (fun p ↦ logLik w v y (p 0) s (p 1) x)
          (domain (fun _ : Fin 2 → ℝ ↦ s) (fun p ↦ p 1)) p →
        IsMaxOn (fun p ↦ logLik w v y (p 0) s (p 1) x)
          (domain (fun _ : Fin 2 → ℝ ↦ s) (fun p ↦ p 1)) q → p = q := by
  apply chart (d := 2) (by omega) w hw v y (some 0) none (some 1) 0 s 0
  intro p q h
  funext i
  fin_cases i
  · exact congrArg Prod.fst h
  · exact congrArg (fun z ↦ z.2.2) h

/-- At held rate and location, any attained finite EMG fit is almost everywhere
unique on fixed-count charts with at least 3 freely varying levels. -/
theorem held_lambda_mu_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (r m : ℝ) (hn : 3 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ (domain (fun p : Fin 1 → ℝ ↦ p 0) (fun _ ↦ r)),
      ∀ q ∈ (domain (fun p : Fin 1 → ℝ ↦ p 0) (fun _ ↦ r)),
        IsMaxOn (fun p ↦ logLik w v y m (p 0) r x)
          (domain (fun p : Fin 1 → ℝ ↦ p 0) (fun _ ↦ r)) p →
        IsMaxOn (fun p ↦ logLik w v y m (p 0) r x)
          (domain (fun p : Fin 1 → ℝ ↦ p 0) (fun _ ↦ r)) q → p = q := by
  apply chart (d := 1) (by omega) w hw v y none (some 0) none m 0 r
  intro p q h
  funext i
  fin_cases i
  · exact congrArg (fun z ↦ z.2.1) h

end

end UnivariateML.AE4EMG

end
