module

public import UnivariateML.Family.SkewLSEMG
public import UnivariateML.Family.ClosedForm
public import UnivariateML.Generic.FinishProfile
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Compactification of the all-free EMG profile

## Deviations

The chart uses `k = (1-t)/t` and total log likelihood, as in
`SkewLSEMG.critical_value_criterion`. The criterion quantifies over interior
stationary points. It needs no finite enumeration of that set.
-/

@[expose] public section

open Set Filter Topology Real
open UnivariateML.SkewLSGaussian UnivariateML.SkewLSEMG

namespace UnivariateML.FinishEMG

noncomputable section

def rateProfile {n : ℕ} (x : Fin n → ℝ) (k : ℝ) : ℝ :=
  sSup (shapeLogLik x k '' LocationScale.domain)

private theorem profile_max {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    {k : ℝ} (hk : 0 < k) :
    ∃ p ∈ LocationScale.domain, IsMaxOn (shapeLogLik x k) LocationScale.domain p ∧
      rateProfile x k = shapeLogLik x k p := by
  obtain ⟨p, ⟨hp, hm⟩, _⟩ := fixed_shape_fit hx hk
  exact ⟨p, hp, hm, (hm.isLUB hp).csSup_eq (Nonempty.image _ ⟨p, hp⟩)⟩

private theorem log_formula {r s : ℝ} (hr : 0 < r) (μ v : ℝ) :
    log (density r μ s v) =
      log r + ((r*s)^2/2-r*(v-μ)) + log (Φ ((v-μ)/s-r*s)) := by
  unfold density
  rw [log_mul (mul_ne_zero hr.ne' (exp_pos _).ne') (cdf_pos _).ne',
    log_mul hr.ne' (exp_pos _).ne', log_exp]

private theorem log_cdf_quadratic {z : ℝ} (hz : z ≤ 0) :
    log (Φ z) ≤ -(z^2/2) := by
  let f (u : ℝ) := log (Φ u) + u^2/2
  have hd (u : ℝ) : HasDerivAt f (φ u / Φ u + u) u := by
    convert ((cdf_hasDerivAt u).log (cdf_pos u).ne').add
      (((hasDerivAt_id u).pow 2).div_const 2) using 1
    · rfl
    change φ u / Φ u + u = φ u / Φ u + (2 * u ^ 1 * 1) / 2
    ring
  have hm : Monotone f := monotone_of_deriv_nonneg
    (fun u ↦ (hd u).differentiableAt) (fun u ↦ by
      rw [(hd u).deriv]
      exact (le_of_lt (by
        rw [show φ u / Φ u + u = (φ u + u*Φ u)/Φ u by field_simp [(cdf_pos u).ne']]
        exact div_pos (phi_add_mul_cdf_pos u) (cdf_pos u))))
  have h := hm hz
  dsimp [f] at h
  rw [cdf_zero, zero_pow (by decide : 2 ≠ 0), zero_div, add_zero] at h
  linarith [log_nonpos (by norm_num : (0 : ℝ) ≤ 1/2) (by norm_num : (1/2 : ℝ) ≤ 1)]

private theorem small_shape_bound {n : ℕ} (x : Fin n → ℝ) (i : Fin n)
    {k : ℝ} (hk : 0 < k) (p : ℝ × ℝ) (hp : p ∈ LocationScale.domain) :
    shapeLogLik x k p ≤ Exponential.logLik (fun j ↦ x j - x i) (k / p.2) +
      (n : ℝ)^2*k^2/2 := by
  have hs : 0 < p.2 := hp.2
  let r := k/p.2
  let z := (x i-p.1)/p.2-k
  have hr : 0 < r := div_pos hk hs
  have hz : r * (x i-p.1) = k*(z+k) := by
    dsimp [r,z]
    ring
  have hsum : (∑ j : Fin n, log (Φ ((x j-p.1)/p.2-k))) ≤ log (Φ z) := by
    rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i)]
    exact add_le_of_nonpos_left (Finset.sum_nonpos fun j _ ↦
      log_nonpos (cdf_pos _).le (cdf_lt_one _).le)
  have he : shapeLogLik x k p =
      Exponential.logLik (fun j ↦ x j-x i) r - n*k*z - n*k^2/2 +
        ∑ j : Fin n, log (Φ ((x j-p.1)/p.2-k)) := by
    unfold shapeLogLik locationLogLik
    change (∑ j : Fin n, log (density r p.1 p.2 (x j))) = _
    simp only [log_formula hr, show r*p.2 = k from div_mul_cancel₀ _ hs.ne']
    rw [Finset.sum_add_distrib]
    congr 1
    have hterm (j : Fin n) : log r + (k^2/2 - r*(x j-p.1)) =
        log r - r*(x j-x i) - k*z - k^2/2 := by nlinarith only [hz]
    simp only [hterm, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum, Exponential.logLik]
    ring
  rw [he]
  have hn : 0 ≤ (n : ℝ) := Nat.cast_nonneg _
  rcases le_or_gt 0 z with h | h
  · have hlog := log_nonpos (cdf_pos z).le (cdf_lt_one z).le
    nlinarith only [hsum, hlog, mul_nonneg (mul_nonneg hn hk.le) h,
      mul_nonneg hn (sq_nonneg k), sq_nonneg ((n : ℝ)*k)]
  · have hlog := log_cdf_quadratic h.le
    nlinarith only [hsum, hlog, sq_nonneg (z+(n : ℝ)*k), mul_nonneg hn (sq_nonneg k)]

/-- At the exponential endpoint the all-free profile converges to the
optimized exponential log likelihood of the sample translated by its minimum. -/
theorem exponential_endpoint {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    Tendsto (rateProfile x) (𝓝[>] 0)
      (𝓝 (Exponential.logLik (fun j ↦ x j-x i) (Exponential.rate (fun j ↦ x j-x i)))) := by
  let y (j : Fin n) := x j-x i
  let r := Exponential.rate y
  let E := Exponential.logLik y r
  have hn : 0 < n := i.isLt.pos
  have hy : ∀ j, 0 ≤ y j := fun j ↦ sub_nonneg.mpr (hi j)
  have hp : ∃ j, 0 < y j := by
    by_contra! h
    obtain ⟨a,b,hab⟩ := hx
    exact hab (by linarith [hy a, hy b, h a, h b])
  have hm := (Exponential.mle hn hy hp r).mpr rfl
  have hr : 0 < r := hm.1
  let T (k : ℝ) := (sqrt (k/r))⁻¹
  have hs : Tendsto (fun k : ℝ ↦ sqrt (k/r)) (𝓝[>] 0) (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    refine ⟨?_, ?_⟩
    · simpa [Function.comp_def] using continuous_sqrt.continuousAt.tendsto.comp
        ((show Tendsto id (𝓝[>] (0 : ℝ)) (𝓝 0) from nhdsWithin_le_nhds).div_const r)
    · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with k hk
      exact sqrt_pos_of_pos (div_pos hk hr)
  have hT : Tendsto T (𝓝[>] 0) atTop := tendsto_inv_nhdsGT_zero.comp hs
  let witness (k : ℝ) := shapeLogLik x k (x i-(T k)⁻¹, ((T k)⁻¹)^2)
  have hws : Tendsto witness (𝓝[>] 0) (𝓝 E) := by
    have hprod := (moving_exponential_face x hi r).comp hT
    have he : (∏ j, r * exp (-r*(x j-x i))) = exp E := by
      simp only [Finset.prod_mul_distrib, Finset.prod_const, Finset.card_univ, Fintype.card_fin,
        ← Real.exp_sum, ← Finset.mul_sum, E, Exponential.logLik,
        y]
      rw [sub_eq_add_neg, exp_add, exp_nat_mul, exp_log hr]
      simp only [sub_eq_neg_add, neg_mul]
    rw [he] at hprod
    have ht := ((continuousAt_log (exp_pos E).ne').tendsto.comp hprod)
    rw [log_exp] at ht
    refine ht.congr' ?_
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with k (hk : 0 < k)
    have hh : ((T k)⁻¹)^2 = k/r := by simp [T, sq_sqrt (div_nonneg hk.le hr.le)]
    have hh' : k / ((T k)⁻¹)^2 = r := by rw [hh]; field_simp
    simp only [Function.comp_apply, witness, shapeLogLik, locationLogLik, hh']
    exact log_prod (fun j _ ↦ (mul_pos (mul_pos hr (exp_pos _)) (cdf_pos _)).ne')
  have hup : Tendsto (fun k : ℝ ↦ E + (n : ℝ)^2*k^2/2) (𝓝[>] 0) (𝓝 E) := by
    have h : Continuous (fun k : ℝ ↦ E + (n : ℝ)^2*k^2/2) := by fun_prop
    simpa using (h.continuousAt.continuousWithinAt (s := Ioi (0 : ℝ)) (x := 0)).tendsto
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hws hup
  · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with k (hk : 0 < k)
    obtain ⟨p, hp, hm', hval⟩ := profile_max hx hk
    rw [hval]
    apply hm'
    exact ⟨mem_univ _, sq_pos_of_pos (by simpa [T] using sqrt_pos_of_pos (div_pos hk hr))⟩
  · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with k (hk : 0 < k)
    obtain ⟨p, hp, _, hval⟩ := profile_max hx hk
    rw [hval]
    exact (small_shape_bound x i hk p hp).trans (add_le_add_left (hm.2 (div_pos hk hp.2)) _)

private def loss (k z : ℝ) : ℝ := k*z-log (Φ (z-k))

private def objective {n : ℕ} (x : Fin n → ℝ) (k : ℝ) (p : ℝ × ℝ) :=
  LocationScale.inverseLogLik (loss k) x p + n*(log k+k^2/2)

private theorem objective_transfer {n : ℕ} (x : Fin n → ℝ) {k : ℝ} (hk : 0 < k)
    (p : LocationScale.domain) :
    objective x k (LocationScale.inverseEquiv p) = shapeLogLik x k p := by
  rw [objective, LocationScale.inverseLogLik_inverseEquiv]
  have hs : 0 < p.val.2 := p.property.2
  have hks : k/p.val.2*p.val.2 = k := div_mul_cancel₀ _ hs.ne'
  unfold shapeLogLik locationLogLik LocationScale.logLik
  have hc : (∑ _ : Fin n, (log k + k^2/2)) = n*(log k+k^2/2) := by
    simp [mul_add]
  rw [← hc, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro j _
  rw [log_formula (div_pos hk hs), hks, log_div hk.ne' hs.ne']
  unfold loss
  ring

private theorem loss_convex (k : ℝ) : StrictConvexOn ℝ univ (loss k) := by
  refine ⟨convex_univ, ?_⟩
  intro x _ y _ hxy a b ha hb hab
  have hc := cdf_strictLogConcave.2 (mem_univ (x-k)) (mem_univ (y-k))
    (fun h ↦ hxy (sub_left_inj.mp h)) ha hb hab
  simp only [smul_eq_mul] at hc ⊢
  have he : a*(x-k)+b*(y-k) = a*x+b*y-k := by
    rw [show b = 1-a by linarith]
    ring
  rw [he] at hc
  unfold loss
  linarith

private theorem objective_concave {n : ℕ} {x : Fin n → ℝ} (hn : 0 < n) (k : ℝ) :
    ConcaveOn ℝ LocationScale.inverseDomain (objective x k) :=
  (LocationScale.strictConcave_inverseLogLik (loss_convex k) x hn).concaveOn.add_const _

private theorem inverseDomain_open : IsOpen LocationScale.inverseDomain :=
  isOpen_Ioi.prod isOpen_univ

private theorem objective_max {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    {k : ℝ} (hk : 0 < k) :
    ∃ p ∈ LocationScale.inverseDomain, IsMaxOn (objective x k) LocationScale.inverseDomain p ∧
      rateProfile x k = objective x k p ∧
      ∀ q ∈ LocationScale.inverseDomain, q ≠ p → objective x k q < objective x k p := by
  obtain ⟨p, ⟨hp, hm⟩, hu⟩ := fixed_shape_fit hx hk
  let v := LocationScale.inverseEquiv ⟨p,hp⟩
  have htrans (q : LocationScale.inverseDomain) :
      objective x k q = shapeLogLik x k (LocationScale.inverseEquiv.symm q) := by
    rw [← objective_transfer x hk (LocationScale.inverseEquiv.symm q),
      Equiv.apply_symm_apply]
  refine ⟨v, v.property, ?_, ?_, ?_⟩
  · intro q hq
    change objective x k (⟨q,hq⟩ : LocationScale.inverseDomain) ≤ objective x k v
    rw [htrans, objective_transfer x hk]
    exact hm (LocationScale.inverseEquiv.symm ⟨q,hq⟩).property
  · rw [objective_transfer x hk]
    exact (hm.isLUB hp).csSup_eq (Nonempty.image _ ⟨p,hp⟩)
  · intro q hq hne
    have hq' := (LocationScale.inverseEquiv.symm ⟨q,hq⟩).property
    change objective x k (⟨q,hq⟩ : LocationScale.inverseDomain) < objective x k v
    rw [htrans, objective_transfer x hk]
    apply lt_of_le_of_ne (hm hq')
    intro he
    have hh := hu _ ⟨hq', fun y hy ↦ (hm hy).trans_eq he.symm⟩
    exact hne (congrArg Subtype.val (LocationScale.inverseEquiv.symm_apply_eq.mp
      (Subtype.ext hh : LocationScale.inverseEquiv.symm ⟨q,hq⟩ = ⟨p,hp⟩)))

private theorem smooth_objective {n : ℕ} (x : Fin n → ℝ) {k : ℝ} {p : ℝ × ℝ}
    (hk : 0 < k) (hp : p ∈ LocationScale.inverseDomain) :
    ContDiffAt ℝ 1 (fun av : ℝ × (ℝ × ℝ) ↦ objective x av.1 av.2) (k,p) := by
  unfold objective LocationScale.inverseLogLik loss
  have h₁ : k ≠ 0 := hk.ne'
  have h₂ : p.1 ≠ 0 := (show 0 < p.1 from hp.1).ne'
  have hcdf : ContDiff ℝ 1 Φ := by
    rw [contDiff_one_iff_deriv, show deriv Φ = φ from funext fun u ↦ (cdf_hasDerivAt u).deriv]
    exact ⟨fun u ↦ (cdf_hasDerivAt u).differentiableAt,
      continuous_iff_continuousAt.mpr fun u ↦ (phi_hasDerivAt u).continuousAt⟩
  apply ContDiffAt.add
  · apply ContDiffAt.sum
    intro i _
    have harg : ContDiffAt ℝ 1 (fun av : ℝ × (ℝ × ℝ) ↦
        av.2.1 * x i - av.2.2 - av.1) (k,p) := by fun_prop
    have hlog := (hcdf.contDiffAt.comp (k,p) harg).log (cdf_pos (p.1*x i-p.2-k)).ne'
    apply ContDiffAt.sub (by fun_prop) (ContDiffAt.sub (by fun_prop) hlog)
  · fun_prop

private theorem profile_derivative {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    {k : ℝ} (hk : 0 < k) :
    DifferentiableAt ℝ (rateProfile x) k := by
  have hn : 0 < n := by obtain ⟨i,_,_⟩ := hx; exact i.isLt.pos
  obtain ⟨p,hp,hm,hval,hstrict⟩ := objective_max hx hk
  let q (a : ℝ) := if ha : 0 < a then (objective_max hx ha).choose else p
  have hq (a : ℝ) (ha : 0 < a) :
      q a ∈ LocationScale.inverseDomain ∧ IsMaxOn (objective x a) LocationScale.inverseDomain (q a)
      ∧ rateProfile x a = objective x a (q a) := by
    have h := (objective_max hx ha).choose_spec
    simpa only [q, dite_eq_left ha] using And.intro h.1 (And.intro h.2.1 h.2.2.1)
  have hpos : ∀ᶠ a in 𝓝 k, 0 < a := eventually_gt_nhds hk
  have hlim : Tendsto q (𝓝 k) (𝓝 p) :=
    FinishProfile.tendsto_optimizer inverseDomain_open hp
      (fun y hy ↦ by
        rw [← nhds_prod_eq]
        exact (smooth_objective x hk hy).continuousAt.tendsto)
      hstrict (Eventually.of_forall fun a ↦ objective_concave hn a)
      (hpos.mono fun a ha ↦ ⟨(hq a ha).1, (hq a ha).2.1 hp⟩)
  exact (FinishProfile.hasDerivAt_profile
    ((smooth_objective x hk hp).hasStrictFDerivAt_fderiv one_ne_zero)
    hlim hval (hpos.mono fun a ha ↦
      ⟨((hq a ha).2.1 hp).trans_eq (hq a ha).2.2.symm,
        (hq a ha).2.2, (hm (hq a ha).1).trans_eq hval.symm⟩)).differentiableAt

private def gaussianObjective {n : ℕ} (x : Fin n → ℝ) (p : ℝ × ℝ) :=
  ∑ i, log (p.1 * φ (p.1*x i-p.2))

private theorem gaussian_transfer {n : ℕ} (x : Fin n → ℝ) (p : LocationScale.domain) :
    gaussianObjective x (LocationScale.inverseEquiv p) = Normal.logLik x p.val.1 p.val.2 := by
  have hs : 0 < p.val.2 := p.property.2
  rw [Normal.logLik_formula x p.val.1 hs, gaussianObjective]
  change (∑ i, log (p.val.2⁻¹ * φ (p.val.2⁻¹*x i-p.val.1/p.val.2))) = _
  calc
    _ = ∑ i, (-log p.val.2 - (1/2:ℝ)*log (2*π) -
          (x i-p.val.1)^2/(2*p.val.2^2)) := by
      apply Finset.sum_congr rfl
      intro i _
      rw [log_mul (inv_ne_zero hs.ne') (phi_pos _).ne', log_inv, phi_formula,
        one_div, log_mul (inv_ne_zero (sqrt_pos_of_pos (by positivity : 0 < 2*π)).ne')
          (exp_pos _).ne', log_inv, log_exp, log_sqrt (by positivity : 0 ≤ 2*π)]
      field_simp [hs.ne']
      ring
    _ = _ := by
      simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
        Fintype.card_fin, nsmul_eq_mul, Normal.residual, Finset.sum_div]
      ring

private theorem gaussian_joint_limit {n : ℕ} (x : Fin n → ℝ) {p : ℝ × ℝ}
    (hp : p ∈ LocationScale.inverseDomain) :
    Tendsto (fun av : ℝ × (ℝ × ℝ) ↦ objective x av.1 av.2)
      ((atTop : Filter ℝ) ×ˢ 𝓝 p) (𝓝 (gaussianObjective x p)) := by
  let l := (atTop : Filter ℝ) ×ˢ 𝓝 p
  let k (av : ℝ × (ℝ × ℝ)) := av.1
  let b (av : ℝ × (ℝ × ℝ)) := av.2.1
  have hk : Tendsto k l atTop := tendsto_fst
  have hb : Tendsto b l (𝓝 p.1) := continuous_fst.continuousAt.tendsto.comp tendsto_snd
  have hb' : 0 < p.1 := hp.1
  have hpos := hb.eventually (eventually_gt_nhds hb')
  have ht (i : Fin n) :
      Tendsto (fun av : ℝ × (ℝ × ℝ) ↦ log (b av) - loss (k av) (b av*x i-av.2.2)
        + (log (k av)+(k av)^2/2)) l (𝓝 (log (p.1*φ (p.1*x i-p.2)))) := by
    let z (av : ℝ × (ℝ × ℝ)) := b av*x i-av.2.2
    let y (av : ℝ × (ℝ × ℝ)) := k av-z av
    have hz : Tendsto z l (𝓝 (p.1*x i-p.2)) :=
      (hb.mul_const _).sub (continuous_snd.continuousAt.tendsto.comp tendsto_snd)
    have hy : Tendsto y l atTop := by
      simpa [y, sub_eq_add_neg] using hk.atTop_add hz.neg
    have hratio : Tendsto (fun av ↦ 1+z av*(y av)⁻¹) l (𝓝 1) := by
      simpa using (hz.mul (tendsto_inv_atTop_zero.comp hy)).const_add 1
    have hg := (continuous_exp.continuousAt.tendsto.comp ((hz.pow 2).div_const 2).neg)
    have hm := ((((mills.comp hy).mul hratio).mul hb).mul hg)
    have hf : (1 / sqrt (2*π))*1*p.1*exp (-((p.1*x i-p.2)^2/2)) =
        p.1*φ (p.1*x i-p.2) := by rw [phi_formula]; ring
    rw [hf] at hm
    refine ((continuousAt_log (mul_pos hb' (phi_pos _)).ne').tendsto.comp hm).congr' ?_
    filter_upwards [hpos, hk.eventually (eventually_gt_atTop 0),
      hy.eventually (eventually_gt_atTop 0)] with av hbav hkav hyav
    change log ((y av * exp ((y av)^2/2) * Φ (-y av)) *
      (1+z av*(y av)⁻¹) * b av * exp (-(z av^2/2))) = _
    have hr : y av*(1+z av*(y av)⁻¹) = k av := by
      field_simp [hyav.ne']
      dsimp [y]
      ring
    rw [show y av * exp (y av ^ 2 / 2) * Φ (-y av) * (1 + z av * (y av)⁻¹) *
      b av * exp (-(z av ^ 2 / 2)) = b av * k av *
        (exp (y av^2/2)*exp (-(z av^2/2))) * Φ (-y av) by
          rw [← hr]
          ring, ← exp_add,
      log_mul (mul_ne_zero (mul_ne_zero hbav.ne' hkav.ne') (exp_pos _).ne')
        (cdf_pos _).ne', log_mul (mul_ne_zero hbav.ne' hkav.ne') (exp_pos _).ne',
      log_mul hbav.ne' hkav.ne', log_exp]
    have he : -y av = z av-k av := by dsimp [y]; ring
    rw [he]
    unfold loss
    change _ = log (b av) - (k av*z av-log (Φ (z av-k av))) + (log (k av)+(k av)^2/2)
    dsimp [y]
    ring
  refine (tendsto_finsetSum Finset.univ fun i _ ↦ ht i).congr ?_
  intro av
  simp only [objective, LocationScale.inverseLogLik, Finset.sum_add_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, b, k, mul_add]

/-- At infinite dimensionless shape the maximized EMG log likelihood
converges to the log likelihood of the ordinary all-free normal fit. -/
theorem normal_endpoint {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) :
    Tendsto (rateProfile x) atTop (𝓝 (Normal.logLik x (Normal.mean x)
      (Normal.sd x (Normal.mean x)))) := by
  have hn : 0 < n := by obtain ⟨i,_,_⟩ := hx; exact i.isLt.pos
  let p₀ := (Normal.mean x, Normal.sd x (Normal.mean x))
  have hm := (Normal.jointMLE hn hx p₀).mpr rfl
  let p := LocationScale.inverseEquiv ⟨p₀,hm.1⟩
  have hstrict : ∀ v ∈ LocationScale.inverseDomain, v ≠ (p : ℝ × ℝ) →
      gaussianObjective x v < gaussianObjective x p := by
    intro v hv hne
    let q := LocationScale.inverseEquiv.symm ⟨v,hv⟩
    change gaussianObjective x (⟨v,hv⟩ : LocationScale.inverseDomain) < _
    rw [← LocationScale.inverseEquiv.apply_symm_apply ⟨v,hv⟩, gaussian_transfer,
      gaussian_transfer]
    apply lt_of_le_of_ne (hm.2 q.property)
    intro he
    have hh := (Normal.jointMLE hn hx q).mp
      ⟨q.property, fun a ha ↦ (hm.2 ha).trans_eq he.symm⟩
    exact hne (congrArg Subtype.val (LocationScale.inverseEquiv.symm_apply_eq.mp
      (Subtype.ext hh : LocationScale.inverseEquiv.symm ⟨v,hv⟩ = ⟨p₀,hm.1⟩)))
  let q (k : ℝ) := if hk : 0 < k then (objective_max hx hk).choose else p.val
  have hq (k : ℝ) (hk : 0 < k) :
      q k ∈ LocationScale.inverseDomain ∧ IsMaxOn (objective x k) LocationScale.inverseDomain (q k)
      ∧ rateProfile x k = objective x k (q k) := by
    have h := (objective_max hx hk).choose_spec
    simpa only [q, dite_eq_left hk] using And.intro h.1 (And.intro h.2.1 h.2.2.1)
  have hq_lim : Tendsto q atTop (𝓝 p.val) :=
    FinishProfile.tendsto_optimizer inverseDomain_open p.property
      (fun _ hv ↦ gaussian_joint_limit x hv) hstrict
      (Eventually.of_forall fun k ↦ objective_concave hn k)
      ((eventually_gt_atTop 0).mono fun k hk ↦ ⟨(hq k hk).1, (hq k hk).2.1 p.property⟩)
  have ht := (gaussian_joint_limit x p.property).comp (tendsto_id.prodMk hq_lim)
  rw [gaussian_transfer] at ht
  exact ht.congr' ((eventually_gt_atTop 0).mono fun k hk ↦ (hq k hk).2.2.symm)

/-- The continuously compactified EMG profile in the chart
`k = (1-t)/t`. The index supplies the location of the exponential face. -/
def compactProfile {n : ℕ} (x : Fin n → ℝ) (i : Fin n) (t : ℝ) : ℝ :=
  if t = 0 then Normal.logLik x (Normal.mean x) (Normal.sd x (Normal.mean x))
  else if t = 1 then Exponential.logLik (fun j ↦ x j - x i)
    (Exponential.rate (fun j ↦ x j - x i))
  else interiorProfile x t

private theorem profile_eq {n : ℕ} (x : Fin n → ℝ) (i : Fin n) :
    EqOn (compactProfile x i) (interiorProfile x) (Ioo 0 1) :=
  fun _ ht ↦ by simp [compactProfile, (show (0 : ℝ) < _ from ht.1).ne',
    (show _ < (1 : ℝ) from ht.2).ne]

private theorem differentiable_profile {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) : DifferentiableOn ℝ (compactProfile x i) (Ioo 0 1) := by
  intro t ht
  have hk : 0 < (1-t)/t := div_pos (sub_pos.mpr ht.2) ht.1
  have htne : t ≠ 0 := ht.1.ne'
  have hd : DifferentiableAt ℝ (fun t : ℝ ↦ (1-t)/t) t := by fun_prop
  have hc := (profile_derivative hx hk).comp (f := fun u : ℝ ↦ (1 - u) / u) t hd
  exact hc.differentiableWithinAt.congr
    (fun _ h ↦ profile_eq x i h) (profile_eq x i ht)

private theorem continuous_profile {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    ContinuousOn (compactProfile x i) (Icc 0 1) := by
  intro t ht
  by_cases h0 : t = 0
  · subst t
    have hf : ∀ᶠ t : ℝ in 𝓝[>] 0, t ∈ Ioo 0 1 := by
      filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
        nhdsWithin_le_nhds (eventually_lt_nhds (show (0 : ℝ) < 1 by norm_num))]
        with t hp hl
      exact ⟨hp,hl⟩
    have hk : Tendsto (fun t : ℝ ↦ (1-t)/t) (𝓝[>] 0) atTop := by
      have hv : Tendsto (fun t : ℝ ↦ t⁻¹ - 1) (𝓝[>] 0) atTop :=
        tendsto_atTop_add_const_right _ (-1) tendsto_inv_nhdsGT_zero
      apply hv.congr'
      filter_upwards [hf] with t ht
      have hp : 0 < t := ht.1
      field_simp [hp.ne']
    have h := ((normal_endpoint hx).comp hk).congr'
      (hf.mono fun t ht ↦ (profile_eq x i ht).symm)
    rw [← continuousWithinAt_sdiff_self]
    have hs : Icc (0 : ℝ) 1 \ {0} = Ioc 0 1 := by ext u; simp [lt_iff_le_and_ne, ne_comm]
    rw [hs, ContinuousWithinAt, nhdsWithin_Ioc_eq_nhdsGT (by norm_num : (0 : ℝ) < 1)]
    simpa [compactProfile] using h
  by_cases h1 : t = 1
  · subst t
    have hf : ∀ᶠ t : ℝ in 𝓝[<] 1, t ∈ Ioo 0 1 := by
      filter_upwards [nhdsWithin_le_nhds (eventually_gt_nhds (show (0 : ℝ) < 1 by norm_num)),
        self_mem_nhdsWithin (s := Iio (1 : ℝ))] with t hp hl
      exact ⟨hp,hl⟩
    have hk : Tendsto (fun t : ℝ ↦ (1-t)/t) (𝓝[<] 1) (𝓝[>] 0) := by
      apply tendsto_nhdsWithin_iff.mpr
      refine ⟨?_, hf.mono fun t ht ↦ div_pos (sub_pos.mpr ht.2) ht.1⟩
      have hc : ContinuousAt (fun t : ℝ ↦ (1-t)/t) 1 := by fun_prop
      simpa using hc.continuousWithinAt (s := Iio (1 : ℝ)) |>.tendsto
    have h := ((exponential_endpoint hx i hi).comp hk).congr'
      (hf.mono fun t ht ↦ (profile_eq x i ht).symm)
    rw [← continuousWithinAt_sdiff_self]
    have hs : Icc (0 : ℝ) 1 \ {1} = Ico 0 1 := by ext u; simp [lt_iff_le_and_ne]
    rw [hs, ContinuousWithinAt, nhdsWithin_Ico_eq_nhdsLT (by norm_num : (0 : ℝ) < 1)]
    simpa [compactProfile] using h
  have ht' : t ∈ Ioo (0 : ℝ) 1 := ⟨lt_of_le_of_ne ht.1 (Ne.symm h0), lt_of_le_of_ne ht.2 h1⟩
  exact ((differentiable_profile hx i t ht').differentiableAt
    (Ioo_mem_nhds ht'.1 ht'.2)).continuousAt.continuousWithinAt

/-- The profile has the optimized normal and translated-exponential
endpoint values and is differentiable at every interior shape. -/
theorem compactification {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    ContinuousOn (compactProfile x i) (Icc 0 1) ∧
      DifferentiableOn ℝ (compactProfile x i) (Ioo 0 1) ∧
      EqOn (compactProfile x i) (interiorProfile x) (Ioo 0 1) :=
  ⟨continuous_profile hx i hi, differentiable_profile hx i, profile_eq x i⟩

/-- An interior stationary value reaching both optimized faces is
equivalent to finite attainment. Equality with either excluded face is allowed. -/
theorem all_free_criterion {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    (∃ t ∈ Ioo 0 1, HasDerivAt (compactProfile x i) 0 t ∧
      max (compactProfile x i 0) (compactProfile x i 1) ≤ compactProfile x i t) ↔
      ∃ q ∈ (Ioo 0 1 ×ˢ LocationScale.domain),
        IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
          (Ioo 0 1 ×ˢ LocationScale.domain) q :=
  critical_value_criterion hx (continuous_profile hx i hi) (differentiable_profile hx i)
    (profile_eq x i)


/-- Every nonempty constant sample has an unbounded all-free log likelihood. -/
theorem constant_log_unbounded {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove ((fun p : ℝ × (ℝ × ℝ) ↦
      shapeLogLik (fun _ : Fin n ↦ m) ((1-p.1)/p.1) p.2) ''
        (Ioo 0 1 ×ˢ LocationScale.domain)) := by
  rintro ⟨B,hB⟩
  have ht := tendsto_log_atTop.comp (constant_sample_unbounded hn m)
  have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
  obtain ⟨s,hs,hlt⟩ := (hs.and (ht.eventually_gt_atTop B)).exists
  have hm : ((1/2,m,s) : ℝ × (ℝ × ℝ)) ∈ Ioo 0 1 ×ˢ LocationScale.domain :=
    ⟨by norm_num,⟨mem_univ _,hs⟩⟩
  have hle := hB (mem_image_of_mem _ hm)
  change shapeLogLik (fun _ : Fin n ↦ m) ((1-1/2)/(1/2)) (m,s) ≤ B at hle
  rw [show (1-(1:ℝ)/2)/(1/2) = 1 by norm_num] at hle
  change B < log (∏ _ : Fin n, density s⁻¹ m s m) at hlt
  have hden : density s⁻¹ m s m ≠ 0 :=
    (mul_pos (mul_pos (inv_pos.mpr hs) (exp_pos _)) (cdf_pos _)).ne'
  rw [log_prod (fun _ _ ↦ hden)] at hlt
  exact hlt.not_ge (by simpa [shapeLogLik, locationLogLik] using hle)

/-- On a nonconstant sample with no finite maximizer, the optimized faces
give the unattained least upper bound of the actual all-free likelihood. -/
theorem unattained_face_supremum {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j)
    (h : ¬ ∃ q ∈ (Ioo 0 1 ×ˢ LocationScale.domain),
      IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
        (Ioo 0 1 ×ˢ LocationScale.domain) q) :
    IsLUB ((fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2) ''
      (Ioo 0 1 ×ˢ LocationScale.domain)) (max (compactProfile x i 0) (compactProfile x i 1)) := by
  let P := compactProfile x i
  let J := Icc (0 : ℝ) 1
  have hc := continuous_profile hx i hi
  obtain ⟨t,ht,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨0,by norm_num⟩ hc
  have hboundary : t = 0 ∨ t = 1 := by
    by_contra! hne
    have ht' : t ∈ Ioo (0 : ℝ) 1 := ⟨lt_of_le_of_ne ht.1 (Ne.symm hne.1),
      lt_of_le_of_ne ht.2 hne.2⟩
    have hl : IsLocalMax P t := hm.isLocalMax (Icc_mem_nhds ht'.1 ht'.2)
    have hd := (differentiable_profile hx i t ht').differentiableAt
      (Ioo_mem_nhds ht'.1 ht'.2)
    exact h ((all_free_criterion hx i hi).mp
      ⟨t,ht',hl.deriv_eq_zero ▸ hd.hasDerivAt,max_le (hm (by norm_num)) (hm (by norm_num))⟩)
  have htval : P t ≤ max (P 0) (P 1) := by
    rcases hboundary with rfl | rfl
    · exact le_max_left _ _
    · exact le_max_right _ _
  constructor
  · rintro _ ⟨⟨a,p⟩,⟨ha,hp⟩,rfl⟩
    obtain ⟨q,hq,hmax,he⟩ := profile_max hx (div_pos (sub_pos.mpr ha.2) ha.1)
    have hval : P a = shapeLogLik x ((1-a)/a) q := (profile_eq x i ha).trans he
    exact (((isMaxOn_iff.mp hmax p hp).trans_eq hval.symm).trans
      (hm (Ioo_subset_Icc_self ha))).trans htval
  · intro L hL
    have hb : Ioo (0 : ℝ) 1 ⊆ P ⁻¹' Iic L := by
      intro a ha
      obtain ⟨q,hq,_,he⟩ := profile_max hx (div_pos (sub_pos.mpr ha.2) ha.1)
      change compactProfile x i a ≤ L
      rw [profile_eq x i ha]
      change rateProfile x ((1-a)/a) ≤ L
      rw [he]
      exact hL ⟨(a,q),⟨ha,hq⟩,rfl⟩
    have hends (a : ℝ) (ha : a ∈ J) : P a ≤ L := by
      have he : a ∈ closure (Ioo (0 : ℝ) 1) := by
        rwa [closure_Ioo zero_ne_one]
      let _ : (𝓝[Ioo (0 : ℝ) 1] a).NeBot := mem_closure_iff_nhdsWithin_neBot.mp he
      have hevent : ∀ᶠ b in 𝓝[Ioo (0 : ℝ) 1] a, b ∈ Ioo 0 1 := self_mem_nhdsWithin
      exact le_of_tendsto ((hc a ha).mono Ioo_subset_Icc_self).tendsto
        (hevent.mono fun _ hb' ↦ hb hb')
    exact max_le (hends 0 (by norm_num [J])) (hends 1 (by norm_num [J]))

end

end UnivariateML.FinishEMG
