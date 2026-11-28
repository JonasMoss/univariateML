module

public import UnivariateML.Family.LocScaleHuber
public import UnivariateML.Generic.RBProfile

import Mathlib.Tactic
import Mathlib.MeasureTheory.Integral.IntegralEqImproper

set_option linter.style.header false

/-! # Huber escape with location and scale held -/

@[expose] public section

open Set Filter Real
open UnivariateML.Huber
open scoped Topology Interval
open UnivariateML.LocScaleHuber

namespace UnivariateML.RBHuber

/-- Small cutoff gives vanishing likelihood, and failure of attainment means
that the matched normal is the exact, strictly unbeaten supremum. -/
theorem held_location_scale_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    let L := fun c ↦ logLikelihood x c m s
    let B := Normal.logLik x m s
    Tendsto L (𝓝[>] 0) atBot ∧ Tendsto L atTop (𝓝 B) ∧
      ((¬ ∃ c ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) c) ↔
        (∀ c ∈ Ioi (0 : ℝ), L c < B) ∧ IsLUB (L '' Ioi 0) B) :=
  ⟨held_scale_small_shape hn x m hs, normal_face_path x m hs,
    held_location_scale_nonattainment hn x m hs⟩

private theorem rho_lower {c a z : ℝ} (ha : 0 < a) (hc : a ≤ c) :
    a * |z| - a^2/2 ≤ rho c z := by
  rw [rho_eq_piecewise (ha.le.trans hc)]
  split_ifs with h
  · nlinarith [sq_nonneg (|z|-a), sq_abs z]
  · have h₁ := mul_nonneg (sub_nonneg.mpr hc)
      (show 0 ≤ |z|-(c+a)/2 by linarith)
    nlinarith only [h₁]

private theorem rho_nonneg {c z : ℝ} (hc : 0 < c) : 0 ≤ rho c z := by
  rw [rho_eq_piecewise hc.le]
  split_ifs with h
  · positivity
  · exact mul_nonneg hc.le (by linarith [le_of_not_ge h])

private theorem normal_formula {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {s : ℝ}
    (hs : 0 < s) :
    Normal.logLik x m s = ∑ j, (-log (s*sqrt (2*π))-((x j-m)/s)^2/2) := by
  have hπ : 0 < 2*π := by positivity
  rw [Normal.logLik_formula x m hs]
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, log_mul hs.ne' (sqrt_pos.mpr hπ).ne', log_sqrt hπ.le, div_pow,
    Normal.residual]
  rw [← Finset.sum_div, ← Finset.sum_div]
  ring

private theorem log_normalizer_lt {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    log (s * sqrt (2*π)) < log (s * normalizer c) := by
  have h := held_scale_constant_strict (n := 1) (by decide) 0 hs c hc 0
  rw [normal_formula _ _ hs] at h
  simpa [logLikelihood, rho, clip, max_eq_right (neg_nonpos.mpr hc.le),
    min_eq_right hc.le] using h

/-- For any log likelihood level strictly above the held-scale Gaussian
value at the mean, one compact rectangle contains the joint superlevel.
The bounds apply to both coordinates independently of any choice of
conditional location maximizer. -/
theorem held_scale_superlevel {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s J : ℝ} (hs : 0 < s) (hJ : Normal.logLik x (Normal.mean x) s < J) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Ioi 0 ×ˢ univ ∧
      ∀ p ∈ Ioi (0 : ℝ) ×ˢ (univ : Set ℝ),
        J ≤ logLikelihood x p.1 p.2 s → p ∈ K := by
  let i : Fin n := ⟨0, hn⟩
  let P := log (s * sqrt (2*π))
  have hb (c m : ℝ) (hc : 0 < c) :
      logLikelihood x c m s ≤ logLikelihood (fun _ : Fin n ↦ (0 : ℝ)) c 0 s := by
    unfold logLikelihood
    apply Finset.sum_le_sum
    intro j _
    have h := rho_nonneg (z := (x j-m)/s) hc
    have hzero : rho c 0 = 0 := by
      simp [rho, clip, min_eq_right hc.le, max_eq_right (neg_nonpos.mpr hc.le)]
    simpa [hzero] using sub_le_self (-log (s * normalizer c)) h
  obtain ⟨a,ha,ha'⟩ := Metric.mem_nhdsWithin_iff.mp
    ((held_scale_small_shape (n := n) hn (fun _ ↦ 0) 0 hs).eventually_lt_atBot J)
  let d := a/2
  have hd : 0 < d := half_pos ha
  have hloc (c m : ℝ) (hc : d ≤ c) :
      logLikelihood x c m s ≤ -(n : ℝ)*P - (d*|(x i-m)/s|-d^2/2) := by
    have hc' := hd.trans_le hc
    unfold logLikelihood
    rw [Finset.sum_sub_distrib]
    have hsum : rho c ((x i-m)/s) ≤ ∑ j, rho c ((x j-m)/s) :=
      Finset.single_le_sum (fun j _ ↦ rho_nonneg (z := (x j-m)/s) hc') (Finset.mem_univ i)
    have hnorm : (n : ℝ)*P ≤ n*log (s*normalizer c) :=
      mul_le_mul_of_nonneg_left (log_normalizer_lt hc' hs).le (by positivity)
    simpa only [Finset.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul] using
      (show (n : ℝ) * -log (s*normalizer c) - (∑ j, rho c ((x j-m)/s)) ≤ _ by
        linarith only [hsum, hnorm, rho_lower (z := (x i-m)/s) hd hc])
  let M := |x i| + s * max 0 ((-(n : ℝ)*P + d^2/2 - J)/d)
  let X := ∑ j, |x j|
  let C := max d ((X+M)/s)
  refine ⟨Icc d C ×ˢ Icc (-M) M, isCompact_Icc.prod isCompact_Icc,
    fun p hp ↦ ⟨hd.trans_le hp.1.1, mem_univ _⟩, ?_⟩
  intro p hp h
  have hc : 0 < p.1 := hp.1
  have hc₁ : d ≤ p.1 := by
    by_contra! ht
    have he : p.1 ∈ Metric.ball (0 : ℝ) a ∩ Ioi 0 := by
      refine ⟨?_, hc⟩
      rw [Metric.mem_ball, Real.dist_eq, sub_zero, abs_of_pos hc]
      dsimp [d] at ht
      linarith
    exact ((hb p.1 p.2 hc).trans_lt (ha' he)).not_ge h
  have hm : |p.2| ≤ M := by
    have hh := hloc p.1 p.2 hc₁
    have hr : |(x i-p.2)/s| ≤ (-(n : ℝ)*P + d^2/2 - J)/d :=
      (le_div_iff₀ hd).mpr (by linarith)
    have ht : |p.2| ≤ |x i| + |x i-p.2| := by
      simpa [abs_sub_comm p.2 (x i), add_comm] using abs_sub_le p.2 (x i) 0
    rw [abs_div,abs_of_pos hs] at hr
    have hg := (div_le_iff₀ hs).mp (hr.trans (le_max_right 0 _))
    dsimp [M]
    linarith
  refine ⟨⟨hc₁, ?_⟩, abs_le.mp hm⟩
  by_contra! hbig
  have hcore (j : Fin n) : |(x j-p.2)/s| ≤ p.1 := by
    rw [abs_div, abs_of_pos hs]
    refine (div_le_div_of_nonneg_right ?_ hs.le).trans
      ((le_max_right d _).trans hbig.le)
    have hx : |x j| ≤ X := Finset.single_le_sum (fun k _ ↦ abs_nonneg (x k)) (Finset.mem_univ j)
    exact (abs_sub _ _).trans (add_le_add hx hm)
  have hN : logLikelihood x p.1 p.2 s ≤ Normal.logLik x p.2 s := by
    rw [normal_formula x p.2 hs]
    unfold logLikelihood
    apply Finset.sum_le_sum
    intro j _
    rw [rho_eq_piecewise hc.le, ite_eq_left (hcore j)]
    linarith only [log_normalizer_lt hc hs]
  have hmean := (Normal.heldSd hn x hs (Normal.mean x)).mpr rfl
  exact hJ.not_ge (h.trans (hN.trans (hmean (mem_univ p.2))))

private theorem held_scale_cont {n : ℕ} (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    ContinuousOn (fun p : ℝ × ℝ ↦ logLikelihood x p.1 p.2 s) (Ioi 0 ×ˢ univ) := by
  have he : Continuous (fun t : ℝ ↦ exp (-(t^2/2))) := by fun_prop
  have hi : Continuous (fun c : ℝ ↦ ∫ t in (0 : ℝ)..c, exp (-(t^2/2))) :=
    intervalIntegral.continuous_primitive he.intervalIntegrable 0
  have hZ : ContinuousOn normalizer (Ioi 0) := (hi.const_mul 2).continuousOn.add
    ((he.const_mul 2).continuousOn.div continuous_id.continuousOn
      (fun t hc ↦ (show 0 < t from hc).ne'))
  have hp {c : ℝ} (hc : c ∈ Ioi (0 : ℝ)) : 0 < normalizer c := by
    unfold normalizer
    have h : 0 ≤ ∫ t in (0 : ℝ)..c, exp (-(t^2/2)) :=
      intervalIntegral.integral_nonneg_of_forall hc.le (fun t ↦ (exp_pos _).le)
    exact add_pos_of_nonneg_of_pos (mul_nonneg (by norm_num) h)
      (div_pos (mul_pos (by norm_num) (exp_pos _)) (show 0 < c from hc))
  have hlog := (hZ.const_mul s).log (fun _ hc ↦ (mul_pos hs (hp hc)).ne')
  unfold logLikelihood
  apply continuousOn_finsetSum Finset.univ
  intro j _
  have hR : Continuous (fun p : ℝ × ℝ ↦ rho p.1 ((x j-p.2)/s)) := by
    unfold rho clip
    fun_prop
  have hf : MapsTo (Prod.fst : ℝ × ℝ → ℝ) (Ioi 0 ×ˢ (univ : Set ℝ)) (Ioi 0) :=
    fun _ h ↦ h.1
  exact (hlog.neg.comp continuous_fst.continuousOn hf).sub hR.continuousOn

private theorem held_scale_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ logLikelihood x p.1 p.2 s
    let N := Normal.logLik x (Normal.mean x) s
    (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ q ∈ D, N ≤ L q := by
  dsimp only
  constructor
  · rintro ⟨p,hp,hm⟩
    refine ⟨p,hp,le_of_tendsto (normal_face_path x (Normal.mean x) hs) ?_⟩
    exact (eventually_gt_atTop (0 : ℝ)).mono fun c hc ↦
      isMaxOn_iff.mp hm (c, Normal.mean x)
        (show (c, Normal.mean x) ∈ Ioi (0 : ℝ) ×ˢ univ from ⟨hc, mem_univ _⟩)
  · rintro ⟨p,hp,h⟩
    by_cases hb : ∃ q ∈ Ioi (0 : ℝ) ×ˢ (univ : Set ℝ),
        Normal.logLik x (Normal.mean x) s < logLikelihood x q.1 q.2 s
    · obtain ⟨q,hq,hq'⟩ := hb
      obtain ⟨K,hK,hsub,hcut⟩ := held_scale_superlevel hn x hs hq'
      exact RBProfile.exists_isMaxOn_of_compact_superlevel hK hsub (held_scale_cont x hs) hq hcut
    · exact ⟨p,hp,fun z hz ↦ (le_of_not_gt (fun h₁ ↦ hb ⟨z,hz,h₁⟩)).trans h⟩

/-- A finite conditional location optimum with zero profile derivative and
height at least the held-scale normal value exists exactly when the
joint likelihood attains a maximum. -/
theorem held_scale_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ logLikelihood x p.1 p.2 s
    (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ c > 0, ∃ m : ℝ,
      IsMaxOn (fun u ↦ logLikelihood x c u s) univ m ∧
      HasDerivAt (fun t ↦ sSup ((fun u ↦ logLikelihood x t u s) '' univ)) 0 c ∧
      Normal.logLik x (Normal.mean x) s ≤ logLikelihood x c m s := by
  dsimp only
  constructor
  · rintro ⟨p,hp,hm⟩
    have hc : 0 < p.1 := hp.1
    have hmax : IsMaxOn (fun c ↦ logLikelihood x c p.2 s) (Ioi 0) p.1 :=
      fun c ht ↦ isMaxOn_iff.mp hm (c,p.2) (show (c,p.2) ∈ Ioi (0 : ℝ) ×ˢ univ from ⟨ht,mem_univ _⟩)
    have hd := held_location_scale_hasDerivAt x p.2 hs hc
    have hz := (hmax.isLocalMax (Ioi_mem_nhds hc)).hasDerivAt_eq_zero hd
    have hb := le_of_tendsto (normal_face_path x (Normal.mean x) hs)
      ((eventually_gt_atTop (0 : ℝ)).mono fun c ht ↦
        isMaxOn_iff.mp hm (c, Normal.mean x)
          (show (c, Normal.mean x) ∈ Ioi (0 : ℝ) ×ˢ univ from ⟨ht, mem_univ _⟩))
    have hprofile := RBProfile.profile_hasDerivAt_zero
      (f := fun t u ↦ logLikelihood x t u s) (mem_univ p.2) (Ioi_mem_nhds hc)
      (fun t ht u hu ↦ isMaxOn_iff.mp hm (t,u) (show (t,u) ∈ Ioi (0 : ℝ) ×ˢ univ from ⟨ht,hu⟩))
      (hz ▸ hd)
    exact ⟨p.1,hc,p.2,fun m _ ↦
      isMaxOn_iff.mp hm (p.1,m) (show (p.1,m) ∈ Ioi (0 : ℝ) ×ˢ univ from ⟨hc,mem_univ _⟩),
      hprofile,hb⟩
  · rintro ⟨c,hc,m,_,_,hb⟩
    exact (held_scale_attainment hn x hs).mpr ⟨(c,m),⟨hc,mem_univ _⟩,hb⟩

/-- At held scale only the normal can be an unattained supremum.
For constant data every finite point strictly loses to that face. -/
theorem held_scale_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ logLikelihood x p.1 p.2 s
    let N := Normal.logLik x (Normal.mean x) s
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ p ∈ D, L p < N) ∧ IsLUB (L '' D) N) ∧
    (∀ m : ℝ, (∀ j, x j = m) → ∀ p ∈ D, L p < N) := by
  refine ⟨?_, ?_⟩
  · exact LocScaleCommon.nonattainment_iff_of_face
      (l := atTop) (path := fun c ↦ (c, Normal.mean x))
      ((eventually_gt_atTop (0 : ℝ)).mono fun _ hc ↦ ⟨hc,mem_univ _⟩)
      (normal_face_path x (Normal.mean x) hs) (held_scale_attainment hn x hs)
  · intro m hm p hp
    have hx : x = fun _ ↦ m := funext hm
    have hmean := (Normal.heldSd hn x hs (Normal.mean x)).mpr rfl
    have hlt : logLikelihood x p.1 p.2 s < Normal.logLik x m s := by
      rw [hx]
      exact held_scale_constant_strict hn m hs p.1 hp.1 p.2
    exact hlt.trans_le (hmean (mem_univ m))

private theorem normalizer_positive {c : ℝ} (hc : 0 < c) : 0 < normalizer c := by
  unfold normalizer
  have h : 0 ≤ ∫ t in (0 : ℝ)..c, exp (-(t^2/2)) :=
    intervalIntegral.integral_nonneg_of_forall hc.le (fun t ↦ (exp_pos _).le)
  positivity

/-- Uniform small-cutoff comparison in the Laplace scale chart. It
holds for arbitrary location, arbitrary positive scale, and all ties. -/
theorem laplace_envelope {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {c b : ℝ} (hc : 0 < c) (hb : 0 < b) :
    logLikelihood x c m (c*b) ≤ Laplace.logLik x m b + (n : ℝ)*c^2 := by
  have hZ : 2*exp (-(c^2/2)) ≤ c*normalizer c := by
    have hI : 0 ≤ ∫ t in (0 : ℝ)..c, exp (-(t^2/2)) :=
      intervalIntegral.integral_nonneg_of_forall hc.le (fun t ↦ (exp_pos _).le)
    unfold normalizer
    rw [mul_add, mul_comm c (_ / c), div_mul_cancel₀ _ hc.ne']
    exact le_add_of_nonneg_left (by positivity)
  have hlog : log (2*b) - c^2/2 ≤ log (c*b*normalizer c) := by
    have h := log_le_log (show 0 < (2*b)*exp (-(c^2/2)) by positivity)
      (show (2*b)*exp (-(c^2/2)) ≤ c*b*normalizer c by
        convert mul_le_mul_of_nonneg_right hZ hb.le using 1 <;> ring)
    rwa [log_mul (by positivity : 2*b ≠ 0) (exp_pos _).ne',log_exp,
      ← sub_eq_add_neg] at h
  unfold logLikelihood Laplace.logLik Laplace.logDensity
  calc
    _ ≤ ∑ i, (-log (2*b) - |x i-m|/b + c^2) := by
      apply Finset.sum_le_sum
      intro i _
      have ht := rho_lower (z := (x i-m)/(c*b)) hc le_rfl
      have he : c * |(x i-m)/(c*b)| = |x i-m|/b := by
        rw [abs_div,abs_of_pos (mul_pos hc hb)]
        field_simp [hc.ne',hb.ne']
      rw [he] at ht
      linarith
    _ = _ := by rw [Finset.sum_add_distrib]; simp [nsmul_eq_mul]

/-- Coercive upper bound in an effective scale that equals the Gaussian
scale at large cutoff and the Laplace scale at small cutoff. -/
theorem effective_scale_bound {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    let t := s / min c 1
    let A := min (log (sqrt (2*π))) (log 2 - 1/2)
    logLikelihood x c m s ≤
      -(n : ℝ)*log t + (n : ℝ)*(1/2-A) - (∑ j, |x j - m|)/t := by
  let a := min c 1
  let t := s / a
  let A := min (log (sqrt (2*π))) (log 2 - 1/2)
  have ha : 0 < a := lt_min hc zero_lt_one
  have ht : 0 < t := div_pos hs ha
  have he : a*t = s := by dsimp [t]; field_simp [ha.ne']
  have hN : log t + A ≤ log (s*normalizer c) := by
    by_cases h : c ≤ 1
    · have hca : a = c := min_eq_left h
      have he' : c*t = s := hca ▸ he
      have hI : 0 ≤ ∫ v in (0 : ℝ)..c, exp (-(v^2/2)) :=
        intervalIntegral.integral_nonneg_of_forall hc.le (fun v ↦ (exp_pos _).le)
      have hz : 2 * exp (-(c^2/2)) ≤ c*normalizer c := by
        unfold normalizer
        rw [mul_add,mul_comm c (_ / c),div_mul_cancel₀ _ hc.ne']
        exact le_add_of_nonneg_left (by positivity)
      have hlog := log_le_log (show 0 < 2*t*exp (-(c^2/2)) by positivity)
        (show 2*t*exp (-(c^2/2)) ≤ s*normalizer c by
          rw [← he']
          convert mul_le_mul_of_nonneg_right hz ht.le using 1 <;> ring)
      rw [log_mul (by positivity : 2*t ≠ 0) (exp_pos _).ne',log_exp,
        log_mul (by norm_num : (2 : ℝ) ≠ 0) ht.ne'] at hlog
      have hh := mul_nonneg (sub_nonneg.mpr h) (show 0 ≤ 1+c by positivity)
      linarith only [hlog, hh, min_le_right (log (sqrt (2*π))) (log 2-1/2)]
    · have ht' : t = s := by simp [t,a,min_eq_right (le_of_not_ge h)]
      rw [ht']
      have hπ : 0 < sqrt (2*π) := by positivity
      have hlt := log_normalizer_lt hc hs
      rw [log_mul hs.ne' hπ.ne'] at hlt
      linarith only [hlt,min_le_left (log (sqrt (2*π))) (log 2-1/2)]
  have hterm (j : Fin n) :
      -log (s*normalizer c)-rho c ((x j-m)/s) ≤
        -log t + (1/2-A) - |x j - m|/t := by
    have hr := rho_lower (z := (x j-m)/s) ha (show a ≤ c from min_le_left _ _)
    have hc' : a ≤ 1 := min_le_right _ _
    have hh := mul_nonneg (sub_nonneg.mpr hc') (show 0 ≤ 1+a by positivity)
    have hscale : a * |(x j-m)/s| = |x j - m|/t := by
      rw [abs_div,abs_of_pos hs,← he]
      field_simp [ha.ne',ht.ne']
    rw [hscale] at hr
    linarith only [hr, hN, hh]
  calc
    logLikelihood x c m s ≤ ∑ j, (-log t + (1/2-A) - |x j - m|/t) :=
      Finset.sum_le_sum (fun j _ ↦ hterm j)
    _ = _ := by
      rw [Finset.sum_sub_distrib,← Finset.sum_div]
      simp only [Finset.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul]
      ring

private theorem scale_interval (N D C J : ℝ) (hN : 0 < N) (hD : 0 < D) :
    ∃ l u : ℝ, 0 < l ∧ 0 < u ∧ ∀ t > 0,
      J ≤ -N*log t + C - D/t → l ≤ t ∧ t ≤ u := by
  have ht : Tendsto (fun v : ℝ ↦ N*(log v/v)) atTop (𝓝 0) := by
    simpa using (tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero).const_mul N
  obtain ⟨R,hR⟩ := (eventually_atTop.mp
    (ht.eventually (eventually_lt_nhds (half_pos hD))))
  let V := max 1 (max R (2*(C-J)/D+1))
  have hV : 0 < V := zero_lt_one.trans_le (le_max_left _ _)
  refine ⟨V⁻¹, exp ((C-J)/N), inv_pos.mpr hV, exp_pos _, ?_⟩
  intro t ht' hh
  constructor
  · by_contra! h
    have hv : V < t⁻¹ := (lt_inv_comm₀ ht' hV).mp h
    have hR' : R ≤ t⁻¹ := ((le_max_left R _).trans (le_max_right 1 _)).trans hv.le
    have hev := hR _ hR'
    rw [log_inv] at hev
    have hmul := (div_lt_iff₀ (inv_pos.mpr ht')).mp
      (show (N * -log t) / t⁻¹ < D/2 by
        simpa [mul_div_assoc, mul_assoc] using hev)
    have hV' : 2*(C-J)/D+1 ≤ V := (le_max_right R _).trans (le_max_right _ _)
    have hdiv : 2*(C-J)/D < t⁻¹ := by linarith
    have harr := (div_lt_iff₀ hD).mp hdiv
    rw [div_eq_mul_inv] at hh
    nlinarith only [hh, hmul, harr]
  · rw [← log_le_log_iff ht' (exp_pos _), log_exp]
    apply (le_div_iff₀ hN).mpr
    linarith [div_nonneg hD.le ht'.le]

/-- A common compact box contains the joint superlevel for all feasible
locations, once the level is strictly above both compatible faces.
The location set can be the whole line or a held singleton. -/
theorem free_scale_superlevel {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {S : Set ℝ} (hS : IsClosed S) {D N E J : ℝ} (hD : 0 < D)
    (hdev : ∀ m ∈ S, D ≤ ∑ j, |x j - m|)
    (hnorm : ∀ m ∈ S, ∀ s > 0, Normal.logLik x m s ≤ N)
    (hlap : ∀ m ∈ S, ∀ b > 0, Laplace.logLik x m b ≤ E)
    (hN : N < J) (hE : E < J) :
    ∃ K : Set (ℝ × (ℝ × ℝ)), IsCompact K ∧ K ⊆ Ioi 0 ×ˢ (S ×ˢ Ioi 0) ∧
      ∀ p ∈ Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi 0),
        J ≤ logLikelihood x p.1 p.2.1 p.2.2 → p ∈ K := by
  let A := min (log (sqrt (2*π))) (log 2 - 1/2)
  let C := (n : ℝ)*(1/2-A)
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  obtain ⟨l,u,hl,hu,hscale⟩ := scale_interval n D C J hn' hD
  let d := sqrt ((J-E)/(2*n))
  have hd : 0 < d := sqrt_pos.mpr (div_pos (sub_pos.mpr hE) (by positivity))
  have hd2 : d^2 = (J-E)/(2*n) := sq_sqrt (by positivity)
  let i : Fin n := ⟨0,hn⟩
  let M := |x i| + u*max 0 (-(n : ℝ)*log l + C - J)
  let X := ∑ j, |x j|
  let T := max 1 ((X+M)/l)
  let v := min d 1*l
  have hv : 0 < v := mul_pos (lt_min hd zero_lt_one) hl
  refine ⟨Icc d T ×ˢ ((Icc (-M) M ∩ S) ×ˢ Icc v u),
    isCompact_Icc.prod ((isCompact_Icc.inter_right hS).prod isCompact_Icc),
    fun _ hp ↦ ⟨hd.trans_le hp.1.1,hp.2.1.2,hv.trans_le hp.2.2.1⟩, ?_⟩
  rintro ⟨c,m,s⟩ ⟨hc,hm,hs⟩ hval
  change 0 < c at hc
  change 0 < s at hs
  let a := min c 1
  let t := s/a
  have ha : 0 < a := lt_min hc zero_lt_one
  have ht : 0 < t := div_pos hs ha
  have he : a*t = s := by dsimp [t]; field_simp [ha.ne']
  have hbound : logLikelihood x c m s ≤
      -(n : ℝ)*log t + C - (∑ j, |x j - m|)/t := effective_scale_bound x m hc hs
  have htu := hscale t ht (hval.trans (hbound.trans
    (sub_le_sub_left (div_le_div_of_nonneg_right (hdev m hm) ht.le) _)))
  have hsmall : d ≤ c := by
    by_contra! h
    let b := s/c
    have hb : 0 < b := div_pos hs hc
    have he' : c*b=s := by dsimp [b]; field_simp [hc.ne']
    have hf := laplace_envelope x m hc hb
    rw [he'] at hf
    have hsq : c^2 ≤ d^2 := by
      nlinarith [mul_nonneg (sub_nonneg.mpr h.le) (show 0 ≤ d+c by positivity)]
    have hδ : (n : ℝ)*d^2 = (J-E)/2 := by rw [hd2]; field_simp [hn'.ne']
    nlinarith only [hval, hf, hlap m hm b hb, hδ, mul_le_mul_of_nonneg_left hsq hn'.le, hE]
  have hM : |m| ≤ M := by
    have hlog := mul_le_mul_of_nonneg_left (log_le_log hl htu.1) hn'.le
    have hti : |x i-m| ≤ ∑ j, |x j - m| := Finset.single_le_sum
      (fun j _ ↦ abs_nonneg (x j-m)) (Finset.mem_univ i)
    have hi : |x i-m|/t ≤ max 0 (-(n : ℝ)*log l+C-J) := by
      have hh := div_le_div_of_nonneg_right hti ht.le
      linarith only [hval,hbound,hlog,hh,le_max_right (0 : ℝ) (-(n : ℝ)*log l+C-J)]
    have htri : |m| ≤ |x i|+|x i-m| := by
      simpa [abs_sub_comm m (x i),add_comm] using abs_sub_le m (x i) 0
    have hmul := mul_le_mul_of_nonneg_right htu.2
      (le_max_left (0 : ℝ) (-(n : ℝ)*log l+C-J))
    dsimp [M]
    nlinarith only [htri, (div_le_iff₀ ht).mp hi, hmul]
  refine ⟨⟨hsmall,?_⟩,⟨abs_le.mp hM,hm⟩,?_⟩
  · by_contra! hbig
    have ha1 : a = 1 := min_eq_right (le_of_lt ((le_max_left _ _).trans_lt hbig))
    have hst : s = t := by rw [← he,ha1,one_mul]
    have hcore (j : Fin n) : |(x j-m)/s| ≤ c := by
      rw [abs_div,abs_of_pos hs,hst]
      have hx : |x j| ≤ X :=
        Finset.single_le_sum (fun k _ ↦ abs_nonneg (x k)) (Finset.mem_univ j)
      have hnum := (abs_sub (x j) m).trans (add_le_add hx hM)
      exact ((div_le_div_of_nonneg_right hnum ht.le).trans
        (div_le_div_of_nonneg_left ((abs_nonneg _).trans hnum) hl htu.1)).trans
        ((le_max_right _ _).trans hbig.le)
    have hGauss : logLikelihood x c m s ≤ Normal.logLik x m s := by
      rw [normal_formula x m hs]
      unfold logLikelihood
      apply Finset.sum_le_sum
      intro j _
      rw [rho_eq_piecewise hc.le,ite_eq_left (hcore j)]
      linarith only [log_normalizer_lt hc hs]
    exact hN.not_ge (hval.trans (hGauss.trans (hnorm m hm s hs)))
  · rw [← he]
    exact ⟨mul_le_mul (min_le_min_right 1 hsmall) htu.1 hl.le ha.le,
      (mul_le_mul_of_nonneg_right (min_le_right c 1) ht.le).trans (by simpa using htu.2)⟩

private theorem free_scale_cont {n : ℕ} (x : Fin n → ℝ) (S : Set ℝ) :
    ContinuousOn (fun p : ℝ × (ℝ × ℝ) ↦ logLikelihood x p.1 p.2.1 p.2.2)
      (Ioi 0 ×ˢ (S ×ˢ Ioi 0)) := by
  let D := Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi (0 : ℝ))
  have he : Continuous (fun t : ℝ ↦ exp (-(t^2/2))) := by fun_prop
  have hi : Continuous (fun c : ℝ ↦ ∫ t in (0 : ℝ)..c, exp (-(t^2/2))) :=
    intervalIntegral.continuous_primitive he.intervalIntegrable 0
  have hZ : ContinuousOn normalizer (Ioi 0) := (hi.const_mul 2).continuousOn.add
    ((he.const_mul 2).continuousOn.div continuous_id.continuousOn
      (fun t hc ↦ (show 0 < t from hc).ne'))
  have hs : ContinuousOn (fun p : ℝ × (ℝ × ℝ) ↦ p.2.2) D :=
    (continuous_snd.comp continuous_snd).continuousOn
  have hF := hs.mul (hZ.comp continuous_fst.continuousOn (fun _ h ↦ h.1))
  have hlog := hF.log (fun p hp ↦ (mul_pos
    (show 0 < p.2.2 from hp.2.2) (normalizer_positive hp.1)).ne')
  unfold logLikelihood
  apply continuousOn_finsetSum Finset.univ
  intro j _
  have hR : ContinuousOn (fun p : ℝ × (ℝ × ℝ) ↦ rho p.1 ((x j-p.2.1)/p.2.2)) D := by
    have hquot := (show ContinuousOn (fun p : ℝ × (ℝ × ℝ) ↦ x j-p.2.1) D by fun_prop).div
      hs (fun p hp ↦ (show 0 < p.2.2 from hp.2.2).ne')
    have hc : ContinuousOn (Prod.fst : ℝ × (ℝ × ℝ) → ℝ) D := continuous_fst.continuousOn
    have hclip := hc.neg.sup (hc.inf hquot)
    exact (hclip.mul hquot).sub ((hclip.pow 2).div_const 2)
  exact hlog.neg.sub hR

private theorem free_scale_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {S : Set ℝ} (hS : IsClosed S) {D : ℝ} (hD : 0 < D)
    (hdev : ∀ m ∈ S, D ≤ ∑ j, |x j - m|)
    {mN sN mE bE : ℝ} (hmN : mN ∈ S) (hsN : 0 < sN) (hmE : mE ∈ S) (hbE : 0 < bE)
    (hN : ∀ m ∈ S, ∀ s > 0, Normal.logLik x m s ≤ Normal.logLik x mN sN)
    (hE : ∀ m ∈ S, ∀ b > 0, Laplace.logLik x m b ≤ Laplace.logLik x mE bE) :
    let A := Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi (0 : ℝ))
    let L := fun p : ℝ × (ℝ × ℝ) ↦ logLikelihood x p.1 p.2.1 p.2.2
    let B := max (Normal.logLik x mN sN) (Laplace.logLik x mE bE)
    ((∃ p ∈ A, IsMaxOn L A p) ↔ ∃ p ∈ A, B ≤ L p) ∧
    ((¬ ∃ p ∈ A, IsMaxOn L A p) ↔ (∀ p ∈ A, L p < B) ∧ IsLUB (L '' A) B) := by
  let A := Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi (0 : ℝ))
  let L (p : ℝ × (ℝ × ℝ)) := logLikelihood x p.1 p.2.1 p.2.2
  let B := max (Normal.logLik x mN sN) (Laplace.logLik x mE bE)
  have hface {u : ℝ} (hu : ∀ p ∈ A, L p ≤ u) : B ≤ u := by
    apply max_le
    · apply le_of_tendsto (normal_face_path x mN hsN)
      exact (eventually_gt_atTop (0 : ℝ)).mono fun c hc ↦
        hu (c,mN,sN) ⟨hc,hmN,hsN⟩
    · apply le_of_tendsto (laplace_face_path x mE hbE)
      have he : ∀ᶠ c : ℝ in 𝓝[>] 0, c ∈ Ioi 0 := self_mem_nhdsWithin
      exact he.mono fun c hc ↦
        hu (c,mE,c*bE) ⟨hc,hmE,mul_pos hc hbE⟩
  have hcrit : (∃ p ∈ A, IsMaxOn L A p) ↔ ∃ p ∈ A, B ≤ L p := by
    constructor
    · rintro ⟨p,hp,hm⟩
      exact ⟨p,hp,hface (fun _ hq ↦ hm hq)⟩
    · rintro ⟨p,hp,h⟩
      by_cases hb : ∃ q ∈ A, B < L q
      · obtain ⟨q,hq,hq'⟩ := hb
        obtain ⟨K,hK,hsub,hcut⟩ := free_scale_superlevel hn x hS hD hdev hN hE
          ((le_max_left _ _).trans_lt hq') ((le_max_right _ _).trans_lt hq')
        exact RBProfile.exists_isMaxOn_of_compact_superlevel hK hsub (free_scale_cont x S) hq hcut
      · exact ⟨p,hp,fun z hz ↦ (le_of_not_gt (fun h₁ ↦ hb ⟨z,hz,h₁⟩)).trans h⟩
  refine ⟨hcrit, ?_⟩
  rw [hcrit]
  constructor
  · intro h
    have hlt (p) (hp : p ∈ A) : L p < B := lt_of_not_ge (fun hq ↦ h ⟨p,hp,hq⟩)
    exact ⟨hlt, ⟨fun _ ⟨p,hp,he⟩ ↦ he ▸ (hlt p hp).le,
      fun _ hu ↦ hface (fun p hp ↦ hu (mem_image_of_mem L hp))⟩⟩
  · rintro ⟨hlt,_⟩ ⟨p,hp,h⟩
    exact (hlt p hp).not_ge h

private theorem stationary_comparison {n : ℕ} (x : Fin n → ℝ) (S : Set ℝ) (B : ℝ)
    (hcrit : (∃ p ∈ Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi 0),
        IsMaxOn (fun q : ℝ × (ℝ × ℝ) ↦ logLikelihood x q.1 q.2.1 q.2.2)
          (Ioi 0 ×ˢ (S ×ˢ Ioi 0)) p) ↔
      ∃ p ∈ Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi 0), B ≤ logLikelihood x p.1 p.2.1 p.2.2) :
    (∃ p ∈ Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi 0),
        IsMaxOn (fun q : ℝ × (ℝ × ℝ) ↦ logLikelihood x q.1 q.2.1 q.2.2)
          (Ioi 0 ×ˢ (S ×ˢ Ioi 0)) p) ↔
      ∃ c > 0, ∃ m ∈ S, ∃ s > 0,
        IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x c q.1 q.2) (S ×ˢ Ioi 0) (m,s) ∧
        HasDerivAt (fun t ↦ sSup ((fun q : ℝ × ℝ ↦ logLikelihood x t q.1 q.2) ''
          (S ×ˢ Ioi 0))) 0 c ∧ B ≤ logLikelihood x c m s := by
  constructor
  · rintro ⟨⟨c,m,s⟩,hp,hm⟩
    have hc : 0 < c := hp.1
    have hs : 0 < s := hp.2.2
    have hb : B ≤ logLikelihood x c m s := by
      obtain ⟨q,hq,hq'⟩ := hcrit.mp ⟨(c,m,s),hp,hm⟩
      exact hq'.trans (hm hq)
    have hcmax : IsMaxOn (fun t ↦ logLikelihood x t m s) (Ioi 0) c :=
      fun t ht ↦ hm (show (t,m,s) ∈ Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi 0) from ⟨ht,hp.2⟩)
    have hd := held_location_scale_hasDerivAt x m hs hc
    have hz := (hcmax.isLocalMax (Ioi_mem_nhds hc)).hasDerivAt_eq_zero hd
    have hprofile := RBProfile.profile_hasDerivAt_zero
      (f := fun t (q : ℝ × ℝ) ↦ logLikelihood x t q.1 q.2) hp.2 (Ioi_mem_nhds hc)
      (fun t ht q hq ↦ hm (show (t,q) ∈ Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi 0) from ⟨ht,hq⟩))
      (hz ▸ hd)
    exact ⟨c,hc,m,hp.2.1,s,hs,fun q hq ↦
      hm (show (c,q) ∈ Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi 0) from ⟨hc,hq⟩),hprofile,hb⟩
  · rintro ⟨c,hc,m,hm,s,hs,_,_,hb⟩
    exact hcrit.mpr ⟨(c,m,s),⟨hc,hm,hs⟩,hb⟩

private theorem absolute_residual_floor {n : ℕ} (x : Fin n → ℝ)
    {i j : Fin n} (m : ℝ) :
    |x i-x j|/2 ≤ ∑ k, |x k-m| := by
  have hi := Finset.single_le_sum (f := fun k ↦ |x k-m|)
    (fun _ _ ↦ abs_nonneg _) (Finset.mem_univ i)
  have hj := Finset.single_le_sum (f := fun k ↦ |x k-m|)
    (fun _ _ ↦ abs_nonneg _) (Finset.mem_univ j)
  have ht : |x i-x j| ≤ |x i-m| + |x j - m| := by
    simpa [abs_sub_comm m (x j)] using abs_sub_le (x i) m (x j)
  linarith

/-- The all-free Huber criterion and failure classification. On
nonconstant samples it compares a conditional optimum with zero profile
derivative to the two fitted faces. Constant samples collapse unboundedly. -/
theorem all_free_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) :
    let A := Ioi (0 : ℝ) ×ˢ LocationScale.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ logLikelihood x p.1 p.2.1 p.2.2
    let N := Normal.logLik x (Normal.mean x) (Normal.sd x (Normal.mean x))
    let mE := Laplace.lowerMedian x hn
    let E := Laplace.logLik x mE (Laplace.meanAbsDev x mE)
    let B := max N E
    ((∃ p ∈ A, IsMaxOn L A p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ c > 0, ∃ m s : ℝ, 0 < s ∧
        IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x c q.1 q.2) LocationScale.domain (m,s) ∧
        HasDerivAt (fun t ↦ sSup ((fun q : ℝ × ℝ ↦ logLikelihood x t q.1 q.2) ''
          LocationScale.domain)) 0 c ∧ B ≤ logLikelihood x c m s) ∧
    ((¬ ∃ p ∈ A, IsMaxOn L A p) ↔
      ((¬ ∃ i j, x i ≠ x j) ∧ ¬ BddAbove (L '' A)) ∨
      ((∃ i j, x i ≠ x j) ∧ (∀ p ∈ A, L p < B) ∧ IsLUB (L '' A) B)) := by
  dsimp only
  by_cases hx : ∃ i j, x i ≠ x j
  · obtain ⟨i,j,hij⟩ := hx
    have hx' : ∃ i j, x i ≠ x j := ⟨i,j,hij⟩
    let mN := Normal.mean x
    let mE := Laplace.lowerMedian x hn
    have ⟨hpN,hmaxN⟩ := (Normal.jointMLE hn hx' (mN,Normal.sd x mN)).mpr rfl
    have hd : 0 < Laplace.meanAbsDev x mE := by
      apply lt_of_le_of_ne (by unfold Laplace.meanAbsDev; positivity)
      intro he
      obtain ⟨v,hv⟩ := (Laplace.minMeanAbsDev_zero_iff hn x).mp he.symm
      exact hij ((hv i).trans (hv j).symm)
    have hpE : (mE,Laplace.meanAbsDev x mE) ∈ Laplace.jointMaximizers x := by
      rw [Laplace.joint hn hd]
      have hmE : mE ∈ Laplace.medianSet x hn :=
        ⟨le_rfl, Tuple.monotone_sort x (show (n - 1) / 2 ≤ n / 2 by omega)⟩
      exact ⟨hmE, mem_singleton _⟩
    have h := free_scale_comparison hn x (S := univ) isClosed_univ
      (half_pos (abs_pos.mpr (sub_ne_zero.mpr hij)))
      (fun m _ ↦ absolute_residual_floor x (i := i) (j := j) m)
      (mem_univ mN) hpN.2 (mem_univ mE) hd
      (fun m _ s hs ↦ hmaxN (show (m,s) ∈ Normal.domain from ⟨mem_univ _,hs⟩))
      (fun m _ s hs ↦ hpE.2 (show (m,s) ∈ Laplace.domain from ⟨mem_univ _,hs⟩))
    have hst := stationary_comparison x univ _ h.1
    simpa only [hx', true_and, not_true_eq_false, false_and, false_or,
      mem_univ, mem_Ioi, LocationScale.domain] using
      And.intro hst h.2
  · let i : Fin n := ⟨0,hn⟩
    have heq : x = fun _ ↦ x i :=
      funext fun j ↦ not_not.mp (fun h ↦ hx ⟨j,i,h⟩)
    have hf := all_free_constant_failure hn (x i)
    have hu : ¬ BddAbove ((fun p : ℝ × (ℝ × ℝ) ↦ logLikelihood x p.1 p.2.1 p.2.2) ''
        (Ioi 0 ×ˢ LocationScale.domain)) := by
      rw [heq]
      exact hf.1
    have hm : ¬ ∃ p ∈ Ioi 0 ×ˢ LocationScale.domain,
        IsMaxOn (fun q : ℝ × (ℝ × ℝ) ↦ logLikelihood x q.1 q.2.1 q.2.2)
          (Ioi 0 ×ˢ LocationScale.domain) p := by
      rintro ⟨p,hp,hmax⟩
      exact hu ⟨_, (hmax.isLUB hp).1⟩
    simp only [hx,false_and,or_false,not_false_eq_true,true_and,hu,iff_true,iff_false]
    exact ⟨hm,hm⟩

/-- The held-location Huber criterion and failure classification. At
nonzero residuals the finite conditional scale and zero profile derivative
are compared to the matched-location faces. Zero residuals collapse. -/
theorem held_location_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    let A := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ logLikelihood x p.1 m p.2
    let N := Normal.logLik x m (Normal.sd x m)
    let E := Laplace.logLik x m (Laplace.meanAbsDev x m)
    let B := max N E
    ((∃ p ∈ A, IsMaxOn L A p) ↔
      (∃ i, x i ≠ m) ∧ ∃ c > 0, ∃ s > 0,
        IsMaxOn (logLikelihood x c m) (Ioi 0) s ∧
        HasDerivAt (fun t ↦ sSup (logLikelihood x t m '' Ioi 0)) 0 c ∧
        B ≤ logLikelihood x c m s) ∧
    ((¬ ∃ p ∈ A, IsMaxOn L A p) ↔
      ((∀ j, x j = m) ∧ ¬ BddAbove (L '' A)) ∨
      ((∃ j, x j ≠ m) ∧ (∀ p ∈ A, L p < B) ∧ IsLUB (L '' A) B)) := by
  dsimp only
  by_cases hx : ∃ i, x i ≠ m
  · obtain ⟨i,hi⟩ := hx
    have hx' : ∃ i, x i ≠ m := ⟨i,hi⟩
    have hz : ¬ ∀ j, x j = m := fun h ↦ hi (h i)
    have ⟨hsN,hmaxN⟩ := (Normal.heldMean hn hx' (Normal.sd x m)).mpr rfl
    have hd : 0 < Laplace.meanAbsDev x m := by
      apply lt_of_le_of_ne (by unfold Laplace.meanAbsDev; positivity)
      intro he
      exact hz ((Laplace.meanAbsDev_zero_iff hn x m).mp he.symm)
    have hmaxE : IsMaxOn (Laplace.logLik x m) (Ioi 0) (Laplace.meanAbsDev x m) := by
      have hm : Laplace.meanAbsDev x m ∈
          {b ∈ Ioi 0 | IsMaxOn (Laplace.logLik x m) (Ioi 0) b} := by
        rw [Laplace.heldLocation hn hd]
        exact mem_singleton _
      exact hm.2
    let B := max (Normal.logLik x m (Normal.sd x m))
      (Laplace.logLik x m (Laplace.meanAbsDev x m))
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let D' := Ioi (0 : ℝ) ×ˢ ({m} ×ˢ Ioi (0 : ℝ))
    let L (p : ℝ × ℝ) := logLikelihood x p.1 m p.2
    let L' (p : ℝ × (ℝ × ℝ)) := logLikelihood x p.1 p.2.1 p.2.2
    let e (p : ℝ × ℝ) := (p.1,m,p.2)
    have hbij : BijOn e D D' := by
      refine ⟨fun p hp ↦ ⟨hp.1,mem_singleton _,hp.2⟩, ?_, ?_⟩
      · intro p _ q _ h
        exact Prod.ext (congrArg (fun v : ℝ × (ℝ × ℝ) ↦ v.1) h)
          (congrArg (fun v : ℝ × (ℝ × ℝ) ↦ v.2.2) h)
      · rintro ⟨c,u,s⟩ ⟨hc,hu,hs⟩
        have he : u = m := hu
        subst u
        exact ⟨(c,s),⟨hc,hs⟩,rfl⟩
    have hmiff : (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ D', IsMaxOn L' D' p := by
      constructor
      · rintro ⟨p,hp,hp'⟩
        exact ⟨e p,hbij.mapsTo hp,
          (LocScaleCommon.isMaxOn_iff_of_bijOn hbij (fun _ _ ↦ rfl) hp).mp hp'⟩
      · rintro ⟨q,hq,hq'⟩
        obtain ⟨p,hp,rfl⟩ := hbij.surjOn hq
        exact ⟨p,hp,(LocScaleCommon.isMaxOn_iff_of_bijOn hbij (fun _ _ ↦ rfl) hp).mpr hq'⟩
    have hL : L '' D = L' '' D' := by
      ext v
      constructor
      · rintro ⟨p,hp,rfl⟩
        exact ⟨e p,hbij.mapsTo hp,rfl⟩
      · rintro ⟨q,hq,rfl⟩
        obtain ⟨p,hp,rfl⟩ := hbij.surjOn hq
        exact ⟨p,hp,rfl⟩
    have h := free_scale_comparison hn x (isClosed_singleton (x := m))
      (abs_pos.mpr (sub_ne_zero.mpr hi))
      (fun u hu ↦ by
        have he : u = m := hu
        subst u
        exact Finset.single_le_sum (fun j _ ↦ abs_nonneg (x j-m)) (Finset.mem_univ i))
      (mem_singleton m) hsN (mem_singleton m) hd
      (fun u hu s hs ↦ by
        have he : u = m := hu
        subst u
        exact hmaxN hs)
      (fun u hu s hs ↦ by
        have he : u = m := hu
        subst u
        exact hmaxE hs)
    have hst := stationary_comparison x {m} B h.1
    have heq (c s : ℝ) :
        IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x c q.1 q.2) ({m} ×ˢ Ioi 0) (m,s) ↔
          IsMaxOn (logLikelihood x c m) (Ioi 0) s := by
      constructor
      · intro hm t ht
        exact hm (show (m,t) ∈ {m} ×ˢ Ioi 0 from ⟨mem_singleton _,ht⟩)
      · rintro hm ⟨u,t⟩ ⟨hu,ht⟩
        have h0 : u = m := hu
        subst u
        exact hm ht
    have hprofileEq :
        (fun t ↦ sSup ((fun q : ℝ × ℝ ↦ logLikelihood x t q.1 q.2) '' ({m} ×ˢ Ioi 0))) =
          (fun t ↦ sSup (logLikelihood x t m '' Ioi 0)) := by
      funext t
      congr 1
      ext y
      constructor
      · rintro ⟨⟨u,s⟩,⟨hu,hs⟩,rfl⟩
        have he : u = m := hu
        subst u
        exact ⟨s,hs,rfl⟩
      · rintro ⟨s,hs,rfl⟩
        exact ⟨(m,s),⟨mem_singleton m,hs⟩,rfl⟩
    change (((∃ p ∈ D, IsMaxOn L D p) ↔ _) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ _))
    simp only [hx',hz,true_and,false_and,false_or]
    rw [hmiff,hL]
    refine ⟨?_,?_⟩
    · rw [hst]
      constructor
      · rintro ⟨c,hc,u,hu,s,hs,hm,hd,hb⟩
        have he : u = m := hu
        subst u
        exact ⟨c,hc,s,hs,(heq c s).mp hm,hprofileEq ▸ hd,hb⟩
      · rintro ⟨c,hc,s,hs,hm,hd,hb⟩
        exact ⟨c,hc,m,mem_singleton _,s,hs,(heq c s).mpr hm,hprofileEq.symm ▸ hd,hb⟩
    · refine h.2.trans ⟨?_,?_⟩
      · rintro ⟨hlt,hu⟩
        exact ⟨fun p hp ↦ hlt (e p) (hbij.mapsTo hp),hu⟩
      · rintro ⟨hlt,hu⟩
        refine ⟨?_,hu⟩
        intro p hp
        obtain ⟨q,hq,rfl⟩ := hbij.surjOn hp
        exact hlt q hq
  · have he : ∀ j, x j = m := fun j ↦ not_not.mp (fun h ↦ hx ⟨j,h⟩)
    have hu : ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLikelihood x p.1 m p.2) ''
        (Ioi 0 ×ˢ Ioi 0)) := by
      rintro ⟨u,hu⟩
      obtain ⟨s,hs,h⟩ := constant_scale_collapse hn x zero_lt_one he u
      exact h.not_ge (hu ⟨(1,s),⟨zero_lt_one,hs⟩,rfl⟩)
    have hm : ¬ ∃ p ∈ Ioi 0 ×ˢ Ioi 0,
        IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x q.1 m q.2) (Ioi 0 ×ˢ Ioi 0) p := by
      rintro ⟨p,hp,hmax⟩
      exact hu ⟨_, (hmax.isLUB hp).1⟩
    simp only [hx,false_and,or_false,iff_false]
    simp only [he,implies_true,true_and,hu,not_false_eq_true,iff_true]
    exact ⟨hm,hm⟩

end UnivariateML.RBHuber

end
