module

public import UnivariateML.Family.SkewLSEMG
public import UnivariateML.Family.LocScaleCommon
public import UnivariateML.Generic.RBProfile
public import Mathlib.Analysis.Calculus.MeanValue
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics

set_option linter.style.header false

/-! # EMG nonattainment and endpoint contact -/

@[expose] public section

open Set Filter Real
open scoped Topology
open UnivariateML.SkewLSEMG UnivariateML.SkewLSGaussian

namespace UnivariateML.RBEMG

/-- On approaching zero scale, a bounded limiting location contributes the
exponential prefactor. An observation strictly above or below that location
contributes a CDF factor of one or zero, respectively. At contact the factor
is the limiting CDF evaluated at the scaled location displacement. -/
theorem held_rate_contact_limit {α : Type*} {f : Filter α} {n : ℕ} (x : Fin n → ℝ)
    (r : ℝ) {m s : α → ℝ} {c w : ℝ} (hm : Tendsto m f (𝓝 c))
    (hs : Tendsto s f (𝓝[>] 0))
    (hw : Tendsto (fun t ↦ Φ ((c - m t) / s t)) f (𝓝 w)) :
    Tendsto (fun t ↦ ∏ j, density r (m t) (s t) (x j)) f
      (𝓝 (∏ j, r * exp (-r * (x j - c)) *
        (if c < x j then 1 else if x j < c then 0 else w))) := by
  have hs₀ := hs.mono_right nhdsWithin_le_nhds
  have hinv := tendsto_inv_nhdsGT_zero.comp hs
  have hk := hs₀.const_mul r
  have harg (v : ℝ) :
      Tendsto (fun t ↦ Φ ((v - m t) / s t - r * s t)) f
        (𝓝 (if c < v then 1 else if v < c then 0 else w)) := by
    have hres := hm.const_sub v
    rcases lt_trichotomy c v with h | h | h
    · rw [ite_eq_left h]
      have H := (hres.pos_mul_atTop (sub_pos.mpr h) hinv).atTop_add hk.neg
      simpa [Function.comp_def, sub_eq_add_neg, div_eq_mul_inv] using cdf_atTop.comp H
    · subst v
      simp only [lt_irrefl, ite_false]
      have hb : ∀ t : α, |Φ ((c - m t) / s t - r * s t) - Φ ((c - m t) / s t)| ≤
          φ 0 * |r * s t| := by
        intro t
        have hp (z : ℝ) : |deriv Φ z| ≤ φ 0 := by
          rw [(cdf_hasDerivAt z).deriv, abs_of_pos (phi_pos z), phi_formula, phi_formula]
          simp only [zero_pow (by decide : (2 : ℕ) ≠ 0), neg_zero, zero_div, exp_zero]
          exact mul_le_mul_of_nonneg_left
            (show Real.exp (-(z^2/2)) ≤ 1 from exp_le_one_iff.mpr (neg_nonpos.mpr (by positivity)))
            (show 0 ≤ 1/sqrt (2*π) by positivity)
        have H := Convex.norm_image_sub_le_of_norm_deriv_le
          (fun z _ ↦ (cdf_hasDerivAt z).differentiableAt)
          (fun z _ ↦ by simpa [Real.norm_eq_abs] using hp z)
          (convex_univ : Convex ℝ (univ : Set ℝ))
          (mem_univ ((c - m t) / s t)) (mem_univ ((c - m t) / s t - r * s t))
        simpa [Real.norm_eq_abs] using H
      have hzero : Tendsto (fun t ↦ Φ ((c - m t) / s t - r * s t) -
          Φ ((c - m t) / s t)) f (𝓝 0) := by
        apply squeeze_zero_norm (fun t ↦ by simpa [Real.norm_eq_abs] using hb t)
        simpa using hk.abs.const_mul (φ 0)
      simpa using hzero.add hw
    · rw [ite_eq_right h.not_gt, ite_eq_left h]
      have hneg := hres.neg.pos_mul_atTop (neg_pos.mpr (sub_neg.mpr h)) hinv
      have H := (tendsto_neg_atTop_atBot.comp hneg).atBot_add hk.neg
      refine (cdf_atBot.comp H).congr fun t ↦ ?_
      apply congrArg Φ
      dsimp only [Function.comp_apply]
      ring
  apply tendsto_finsetProd Finset.univ
  intro j _
  have hexp := (Real.continuous_exp.continuousAt.tendsto.comp
    (((hk.pow 2).div_const 2).sub ((hm.const_sub (x j)).const_mul r)))
  simpa only [Function.comp_def, density, mul_zero, zero_pow (by decide : (2 : ℕ) ≠ 0),
    zero_div, zero_sub, ← neg_mul] using (hexp.const_mul r).mul (harg (x j))

private theorem density_nonneg {r m s x : ℝ} (hr : 0 < r) :
    0 ≤ density r m s x := by
  unfold density
  exact (mul_pos (mul_pos hr (exp_pos _)) (cdf_pos _)).le

private theorem rate_peak {r m s x : ℝ} (hr : 0 < r) (hs : 0 < s) :
    density r m s x ≤ r := ((held_rate_at_sample hr x).2 m s hs).le

private theorem scale_left_far_endpoint {rate c ε : ℝ} (hr : 0 < rate) (he : 0 < ε) :
    ∀ μ s : ℝ, c+ε ≤ μ → 0 < s →
      density rate μ s c ≤ (2*φ 0/ε^2)*s := by
  intro μ s hμ hs
  have hr' : c ≤ μ := by linarith
  have hsq : (ε/s)^2 ≤ ((c-μ)/s)^2 := by
    have h := (div_le_div_iff_of_pos_right hs).mpr (show ε ≤ μ-c by linarith)
    have hh := pow_le_pow_left₀ (show 0 ≤ ε/s by positivity) h 2
    rw [show (μ-c)/s = -((c-μ)/s) by ring, neg_sq] at hh
    exact hh
  have hφ : φ ((c-μ)/s) ≤ φ 0*exp (-((ε/s)^2/2)) := by
    simp only [phi_formula, zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div, neg_zero,
      exp_zero, mul_one]
    exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr (by linarith)) (by positivity)
  have hE : exp (-((ε/s)^2/2)) ≤ 2/(ε/s)^2 := by
    rw [exp_neg]
    have h := inv_anti₀ (show 0 < (ε/s)^2/2 by positivity)
      (show (ε/s)^2/2 ≤ exp ((ε/s)^2/2) by linarith [add_one_le_exp ((ε/s)^2/2)])
    simpa only [inv_div] using h
  calc
    _ ≤ φ ((c-μ)/s)/s := (normal_dominates_at_held_location hr hs hr').le
    _ ≤ (φ 0*(2/(ε/s)^2))/s := div_le_div_of_nonneg_right
      (hφ.trans (mul_le_mul_of_nonneg_left hE (phi_pos _).le)) hs.le
    _ = _ := by field_simp [he.ne', hs.ne']

private theorem exponential_below_width_bound {rate μ c s ε v : ℝ}
    (hr : 0 < rate) (hμ : μ ≤ c + ε) :
    density rate μ s v ≤ (rate*exp (-rate*(v-c)))*exp ((rate*s)^2/2+rate*ε) := by
  unfold density
  calc
    _ ≤ rate*exp ((rate*s)^2/2-rate*(v-μ)) :=
      mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (cdf_lt_one _).le
    _ ≤ rate*exp (-rate*(v-c)+((rate*s)^2/2+rate*ε)) := by
      apply mul_le_mul_of_nonneg_left _ hr.le
      apply exp_le_exp.mpr
      nlinarith only [mul_le_mul_of_nonneg_left hμ hr.le]
    _ = _ := by rw [exp_add, mul_assoc]

private theorem product_bound_one {n : ℕ} (f : Fin n → ℝ) (M : ℝ)
    (hpos : ∀ i, 0 ≤ f i) (hM : ∀ i, f i ≤ M) (i : Fin n) :
    (∏ j, f j) ≤ M^(n-1)*f i := by
  classical
  rw [← Finset.prod_erase_mul Finset.univ f (Finset.mem_univ i)]
  have h := Finset.prod_le_prod₀ (s := Finset.univ.erase i)
    (fun j _ ↦ hpos j) (fun j _ ↦ hM j)
  exact mul_le_mul_of_nonneg_right (by simpa using h) (hpos i)

private theorem rate_likelihood_bound {n : ℕ} (x : Fin n → ℝ)
    {r m c s d : ℝ} (hr : 0 < r) (hm : m ≤ c + d) :
    (∏ j, density r m s (x j)) ≤
      (∏ j, r*exp (-r*(x j-c))) * exp ((n : ℝ)*((r*s)^2/2+r*d)) := by
  have h := Finset.prod_le_prod₀ (s := Finset.univ)
    (fun j _ ↦ density_nonneg (m := m) (s := s) (x := x j) hr)
    (fun j _ ↦ exponential_below_width_bound (s := s) (v := x j) hr hm)
  simpa only [Finset.prod_mul_distrib,Finset.prod_const,Finset.card_univ,
    Fintype.card_fin,exp_nat_mul] using h

/-- At fixed positive rate all joint superlevels above the exponential
face are confined to a compact location and positive-scale box. -/
theorem held_rate_superlevel {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    {r J : ℝ} (hr : 0 < r) (hJ : (∏ j, r * exp (-r * (x j - x i))) < J) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ univ ×ˢ Ioi 0 ∧
      ∀ p ∈ (univ : Set ℝ) ×ˢ Ioi (0 : ℝ),
        J ≤ (∏ j, density r p.1 p.2 (x j)) → p ∈ K := by
  let c := x i
  let B := ∏ j, r*exp (-r*(x j-c))
  let C := fun s e : ℝ ↦ B*exp ((n : ℝ)*((r*s)^2/2+r*e))
  have hB : 0 < B := Finset.prod_pos (fun _ _ ↦ mul_pos hr (exp_pos _))
  have hJ₀ : 0 < J := hB.trans hJ
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hpos : ∃ e > 0, C 0 e < J := by
    have H : Tendsto (fun e : ℝ ↦ C 0 e) (𝓝[>] 0) (𝓝 B) := by
      have hc : ContinuousAt (fun e : ℝ ↦ C 0 e) 0 := by unfold C; fun_prop
      simpa [C] using (hc.continuousWithinAt (s := Ioi (0 : ℝ))).tendsto
    have he : ∀ᶠ e : ℝ in 𝓝[>] 0, 0 < e := self_mem_nhdsWithin
    exact (he.and (H.eventually (eventually_lt_nhds hJ))).exists
  obtain ⟨e,he,heJ⟩ := hpos
  have hsmall : ∀ᶠ s : ℝ in 𝓝[>] 0, C s e < J := by
    have hc : ContinuousAt (fun s : ℝ ↦ C s e) 0 := by unfold C; fun_prop
    exact (hc.continuousWithinAt (s := Ioi (0 : ℝ))).tendsto.eventually
      (eventually_lt_nhds heJ)
  obtain ⟨δ,hδ,hδ'⟩ := Metric.mem_nhdsWithin_iff.mp hsmall
  let P := r^(n-1)* (2*φ 0/e^2)
  have hP : 0 < P := by dsimp [P]; have := phi_pos 0; positivity
  let l := min (δ/2) (J/P)
  have hl : 0 < l := lt_min (half_pos hδ) (div_pos hJ₀ hP)
  have hTop : Tendsto (fun s : ℝ ↦ (φ 0/s)^n) atTop (𝓝 0) := by
    simpa only [div_eq_mul_inv,mul_zero,zero_pow hn.ne'] using
      ((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul (φ 0)).pow n
  obtain ⟨U₀,hU₀⟩ := eventually_atTop.mp (hTop.eventually (eventually_lt_nhds hJ₀))
  let U := max 1 U₀
  have hU : 0 < U := zero_lt_one.trans_le (le_max_left _ _)
  have hLeft : Tendsto (fun v : ℝ ↦ C U v) atBot (𝓝 0) := by
    have h := tendsto_atBot_add_const_left atBot ((r*U)^2/2)
      (tendsto_id.const_mul_atBot hr)
    simpa only [C,mul_zero,Function.comp_apply,id_eq] using (tendsto_exp_atBot.comp
      (h.const_mul_atBot hn')).const_mul B
  obtain ⟨A,hA⟩ := eventually_atBot.mp (hLeft.eventually (eventually_lt_nhds hJ₀))
  let V := sqrt (r^(n-1)*(2*φ 0)*U/J)+1
  have hV : 0 < V := by dsimp [V]; positivity
  have hV' : r^(n-1)*(2*φ 0)*U < J*V^2 := by
    let Q := r^(n-1)*(2*φ 0)*U/J
    have hQ : 0 ≤ Q := by dsimp [Q]; have := phi_pos 0; positivity
    have h := sq_sqrt hQ
    have hGt : Q < V^2 := by dsimp [V]; nlinarith [sqrt_nonneg Q]
    exact (div_lt_iff₀ hJ₀).mp hGt |>.trans_eq (mul_comm _ _)
  refine ⟨Icc (c+A) (c+V) ×ˢ Icc l U,isCompact_Icc.prod isCompact_Icc,
    fun p hp ↦ ⟨mem_univ _,hl.trans_le hp.2.1⟩,?_⟩
  rintro ⟨m,s⟩ ⟨_,hs⟩ h
  change 0 < s at hs
  change J ≤ ∏ j, density r m s (x j) at h
  have hhigh : s ≤ U := by
    by_contra! hgt
    have hbound : (∏ j, density r m s (x j)) ≤ (φ 0/s)^n := by
      simpa only [Finset.prod_const,Finset.card_univ,Fintype.card_fin] using
        (Finset.prod_le_prod₀ (s := Finset.univ)
          (fun j _ ↦ density_nonneg (m := m) (s := s) (x := x j) hr)
          (fun j _ ↦ (gaussian_peak_strict (μ := m) (x := x j) hr hs).le))
    exact (hbound.trans_lt (hU₀ s ((le_max_right _ _).trans hgt.le))).not_ge h
  have hright {d : ℝ} (hd : 0 < d) (hm : c + d ≤ m) :
      (∏ j, density r m s (x j)) ≤ r^(n-1)*((2*φ 0/d^2)*s) := by
    exact (product_bound_one _ r (fun _ ↦ density_nonneg hr)
      (fun _ ↦ rate_peak hr hs) i).trans
      (mul_le_mul_of_nonneg_left (scale_left_far_endpoint hr hd m s hm hs) (by positivity))
  have hlow : l ≤ s := by
    by_contra! hlt
    by_cases hm : m ≤ c+e
    · have hsδ : s < δ := hlt.trans_le ((min_le_left _ _).trans (by linarith))
      have hball : s ∈ Metric.ball (0 : ℝ) δ ∩ Ioi 0 := by
        refine ⟨?_,hs⟩
        rwa [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]
      exact ((rate_likelihood_bound x hr hm).trans_lt (hδ' hball)).not_ge h
    · have hle := hright he (le_of_not_ge hm)
      have hh := (lt_div_iff₀ hP).mp (hlt.trans_le (min_le_right _ _))
      unfold P at hh
      nlinarith only [hle,hh,h]
  have hleft : c+A ≤ m := by
    by_contra! hlt
    have hle : (∏ j, density r m s (x j)) ≤ C s A :=
      rate_likelihood_bound x hr hlt.le
    have hmon : C s A ≤ C U A := by
      unfold C
      apply mul_le_mul_of_nonneg_left _ hB.le
      apply exp_le_exp.mpr
      have hp := pow_le_pow_left₀ (mul_pos hr hs).le (mul_le_mul_of_nonneg_left hhigh hr.le) 2
      exact mul_le_mul_of_nonneg_left (add_le_add_left
        (div_le_div_of_nonneg_right hp (by norm_num : (0 : ℝ) ≤ 2)) (r*A)) hn'.le
    exact ((hle.trans hmon).trans_lt (hA A le_rfl)).not_ge h
  have hright' : m ≤ c+V := by
    by_contra! hgt
    have hle := hright hV hgt.le
    have hprod := mul_le_mul_of_nonneg_left hhigh
      (show 0 ≤ r^(n-1)*(2*φ 0/V^2) by have := phi_pos 0; positivity)
    have hcut := (div_lt_iff₀ (sq_pos_of_pos hV)).mpr
      (show r^(n-1)*(2*φ 0)*U < J*V^2 from hV')
    have heq : r^(n-1)*(2*φ 0/V^2)*U = r^(n-1)*(2*φ 0)*U/V^2 := by ring
    nlinarith only [hle,hprod,hcut,heq,h]
  exact ⟨⟨hleft,hright'⟩,⟨hlow,hhigh⟩⟩

/-- At the sample minimum the joint contact penalty is one copy
of the limiting Gaussian CDF value per observation tied there. -/
theorem held_rate_minimum_contact {α : Type*} {f : Filter α} {n : ℕ}
    (x : Fin n → ℝ) {i : Fin n} (hi : ∀ j, x i ≤ x j) (r : ℝ)
    {m s : α → ℝ} {w : ℝ} (hm : Tendsto m f (𝓝 (x i)))
    (hs : Tendsto s f (𝓝[>] 0))
    (hw : Tendsto (fun t ↦ Φ ((x i - m t) / s t)) f (𝓝 w)) :
    Tendsto (fun t ↦ ∏ j, density r (m t) (s t) (x j)) f
      (𝓝 ((∏ j, r * exp (-r * (x j - x i))) *
        w^((Finset.univ.filter (fun j ↦ x j = x i)).card))) := by
  have H := held_rate_contact_limit x r hm hs hw
  have hw' (j : Fin n) : (if x i < x j then 1 else if x j < x i then 0 else w) =
      if x j = x i then w else 1 := by
    by_cases h : x j = x i
    · simp [h]
    · simp [h,lt_of_le_of_ne (hi j) (Ne.symm h)]
  simp_rw [hw'] at H
  simpa only [Finset.prod_mul_distrib,Finset.prod_ite,Finset.prod_const,Finset.prod_const_one,
    one_pow,mul_one] using H

/-- At fixed positive rate, the scale-zero face gives the only possible
unattained supremum. The joined statement includes the full moving path
and the joint contact penalty for sequences approaching the minimum. -/
theorem held_rate_failure {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) {r : ℝ} (hr : 0 < r) :
    let D := univ ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density r p.1 p.2 (x j)
    let B := ∏ j, r * exp (-r * (x j - x i))
    Tendsto (fun t : ℝ ↦ L (x i - t⁻¹, (t⁻¹)^2)) atTop (𝓝 B) ∧
    (∀ (m s : ℕ → ℝ) (w : ℝ), Tendsto m atTop (𝓝 (x i)) →
      Tendsto s atTop (𝓝[>] 0) → Tendsto (fun t ↦ Φ ((x i - m t) / s t)) atTop (𝓝 w) →
      Tendsto (fun t ↦ L (m t,s t)) atTop
        (𝓝 (B*w^((Finset.univ.filter (fun j ↦ x j = x i)).card)))) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B) := by
  let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
  let L (p : ℝ × ℝ) := ∏ j, density r p.1 p.2 (x j)
  let B := ∏ j, r * exp (-r * (x j - x i))
  refine ⟨moving_exponential_face x hi r, fun _ _ _ hm hs hw ↦
    held_rate_minimum_contact x hi r hm hs hw, ?_⟩
  apply LocScaleCommon.nonattainment_iff_of_face
    (l := atTop) (path := fun t : ℝ ↦ (x i - t⁻¹, (t⁻¹)^2))
  · exact (eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦
      ⟨mem_univ _, pow_pos (inv_pos.mpr ht) _⟩
  · exact moving_exponential_face x hi r
  · change (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ D, B ≤ L p
    constructor
    · intro h
      obtain ⟨m,s,hs,hms⟩ := (held_rate_criterion x hi hr).mp h
      exact ⟨(m,s),⟨mem_univ _,hs⟩,hms⟩
    · rintro ⟨p,hp,h⟩
      by_cases hb : ∃ q ∈ D, B < L q
      · obtain ⟨q,hq,hbetter⟩ := hb
        obtain ⟨K,hK,hsub,hcut⟩ := held_rate_superlevel x (i := i) hr hbetter
        have hc : ContinuousOn L D := by
          refine continuousOn_finsetProd Finset.univ fun j _ ↦ ?_
          unfold density
          refine ContinuousOn.mul (by fun_prop) (cdf_continuous.comp_continuousOn ?_)
          refine ContinuousOn.sub (ContinuousOn.div (by fun_prop) continuous_snd.continuousOn
            (fun p hp ↦ (show 0 < p.2 from hp.2).ne')) (by fun_prop)
        exact RBProfile.exists_isMaxOn_of_compact_superlevel hK hsub hc hq hcut
      · exact ⟨p,hp,fun q hq ↦ (le_of_not_gt (fun hq' ↦ hb ⟨q,hq,hq'⟩)).trans h⟩

private noncomputable def weightedTail (y : ℝ) : ℝ := exp (y^2/2) * Φ (-y)

private theorem weightedTail_strictAnti : StrictAnti weightedTail := by
  have hd (y : ℝ) : HasDerivAt weightedTail
      (exp (y^2/2) * (y * Φ (-y) - φ (-y))) y := by
    have he : HasDerivAt (fun y : ℝ ↦ exp (y^2/2)) (exp (y^2/2)*y) y := by
      convert (((hasDerivAt_id y).pow 2).div_const 2).exp using 1
      · rfl
      · change exp (y^2/2)*y = exp (y^2/2)*(2*y^1*1/2)
        ring
    have hF : HasDerivAt (fun y ↦ Φ (-y)) (-φ (-y)) y := by
      simpa [Function.comp_def] using (cdf_hasDerivAt (-y)).comp y (hasDerivAt_id y).neg
    convert he.mul hF using 1
    · rfl
    · ring
  apply strictAnti_of_deriv_neg
  intro y
  rw [(hd y).deriv]
  exact mul_neg_of_pos_of_neg (exp_pos _) (by linarith [phi_add_mul_cdf_pos (-y)])

private theorem density_weightedTail (rate μ x : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    density rate μ σ x = rate * exp (-((x-μ)/σ)^2/2) *
      weightedTail (rate*σ - (x-μ)/σ) := by
  unfold density weightedTail
  rw [show -(rate*σ-(x-μ)/σ) = (x-μ)/σ-rate*σ by ring]
  have h := div_mul_cancel₀ (x-μ) hσ.ne'
  have he : (rate*σ)^2/2 - rate*(x-μ) =
      -((x-μ)/σ)^2/2 + (rate*σ-(x-μ)/σ)^2/2 := by
    conv_lhs => rw [← h]
    ring
  rw [he, exp_add]
  ring

private theorem scaled_mills_upper {y : ℝ} (hy : 0 < y) :
    weightedTail y ≤ φ 0/y := by
  have h := phi_add_mul_cdf_pos (-y)
  have hc : y*Φ (-y) ≤ φ (-y) := by linarith
  apply (le_div_iff₀ hy).mpr
  have hmul := mul_le_mul_of_nonneg_left hc (exp_pos (y^2/2)).le
  have he : exp (y^2/2)*φ (-y) = φ 0 := by
    simp only [phi_formula, neg_sq, zero_pow (by decide : (2 : ℕ) ≠ 0),
      zero_div, neg_zero, exp_zero, mul_one]
    calc
      _ = (1/sqrt (2*π))*(exp (y^2/2)*exp (-(y^2/2))) := by ring
      _ = _ := by rw [← exp_add, add_neg_cancel, exp_zero, mul_one]
  rw [he] at hmul
  unfold weightedTail
  nlinarith only [hmul]

private theorem exp_power_tail {d : ℝ} (hd : 0 < d) (n : ℕ) :
    Tendsto (fun r : ℝ ↦ r^n*exp (-(r*d))) atTop (𝓝 0) := by
  have ht := (tendsto_pow_mul_exp_neg_atTop_nhds_zero n).comp
    (tendsto_id.atTop_mul_const hd)
  have h := ht.div_const (d^n)
  simp only [zero_div] at h
  refine h.congr fun r ↦ ?_
  change (r*d)^n*exp (-(r*d))/d^n = r^n*exp (-(r*d))
  rw [mul_pow]
  field_simp

private theorem gauss_power_tail {d : ℝ} (hd : 0 < d) (n : ℕ) :
    Tendsto (fun s : ℝ ↦ (s⁻¹)^n*exp (-((d/s)^2/2))) (𝓝[>] 0) (𝓝 0) := by
  have ht : Tendsto (fun w : ℝ ↦ w^n*exp (-(d^2*w^2/2))) atTop (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
      (tendsto_pow_mul_exp_neg_atTop_nhds_zero n)
    · exact (eventually_ge_atTop (0 : ℝ)).mono (fun w hw ↦
        mul_nonneg (pow_nonneg hw _) (exp_pos _).le)
    · filter_upwards [eventually_ge_atTop (2/d^2), eventually_ge_atTop (0 : ℝ)] with w hw hw0
      apply mul_le_mul_of_nonneg_left _ (pow_nonneg hw0 _)
      apply exp_le_exp.mpr
      have h := (div_le_iff₀ (sq_pos_of_pos hd)).mp hw
      nlinarith only [mul_le_mul_of_nonneg_left h hw0]
  have h := ht.comp tendsto_inv_nhdsGT_zero
  convert h using 1
  ext s
  simp only [Function.comp_apply, div_eq_mul_inv, mul_pow, inv_pow]

private theorem gauss_bound_if_left {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i < μ) {rate s : ℝ} (hr : 0 < rate) (hs : 0 < s) :
    (∏ j, density rate μ s (x j)) ≤
      (φ 0)^n*(s⁻¹)^n*exp (-(((μ-x i)/s)^2/2)) := by
  have hb := product_bound_one (fun j ↦ density rate μ s (x j)) (φ 0/s)
    (fun _ ↦ density_nonneg hr) (fun _ ↦ (gaussian_peak_strict hr hs).le) i
  calc
    _ ≤ (φ 0/s)^(n-1)*(φ ((x i-μ)/s)/s) := hb.trans
      (mul_le_mul_of_nonneg_left (normal_dominates_at_held_location hr hs hi.le).le
        (by have := phi_pos 0; positivity))
    _ = _ := by
      have he : (x i-μ)/s = -((μ-x i)/s) := by ring
      have hφ : φ ((x i-μ)/s) = φ 0*exp (-(((μ-x i)/s)^2/2)) := by
        rw [he]
        simp [phi_formula]
      rw [hφ, div_eq_mul_inv (φ 0), mul_pow]
      simp only [div_eq_mul_inv]
      have hsucc : (n-1)+1 = n := Nat.sub_add_cancel (Nat.zero_lt_of_lt i.isLt)
      rw [← hsucc]
      simp only [Nat.add_sub_cancel_right,pow_succ]
      ring

private theorem joint_collapse_bound {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : μ < x i) {rate s : ℝ} (hr : 0 < rate) (hs : 0 < s) :
    let d := x i-μ
    (∏ j, density rate μ s (x j)) ≤
      max (rate^n*exp (-(rate*(d/2))))
        (max ((φ 0)^(n-1)*d*(s⁻¹)^(n+1)*exp (-((d/s)^2/2)))
             (2*(φ 0)^n*(s⁻¹)^n*exp (-((d/s)^2/2)))) := by
  intro d
  let z := d/s
  let k := rate*s
  have hd : 0 < d := sub_pos.mpr hi
  have hz : 0 < z := div_pos hd hs
  have hzs : z*s = d := div_mul_cancel₀ _ hs.ne'
  have hk : 0 < k := mul_pos hr hs
  have hC := phi_pos 0
  by_cases hle : k ≤ z
  · apply le_trans ?_ (le_max_left _ _)
    have harg : (rate*s)^2/2-rate*(x i-μ) ≤ -(rate*(d/2)) := by
      change k^2/2-rate*d ≤ _
      have h := mul_le_mul_of_nonneg_left hle hk.le
      rw [← hzs]
      dsimp [k] at h ⊢
      nlinarith only [h]
    have hf : density rate μ s (x i) ≤ rate*exp (-(rate*(d/2))) := by
      unfold density
      exact (mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (cdf_lt_one _).le).trans
        (mul_le_mul_of_nonneg_left (exp_le_exp.mpr harg) hr.le)
    have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
    calc
      _ ≤ rate^(n-1)*density rate μ s (x i) := product_bound_one _ rate
        (fun _ ↦ density_nonneg hr) (fun _ ↦ rate_peak hr hs) i
      _ ≤ rate^(n-1)*(rate*exp (-(rate*(d/2)))) :=
        mul_le_mul_of_nonneg_left hf (by positivity)
      _ = _ := by rw [← mul_assoc, ← pow_succ, Nat.sub_add_cancel hn]
  · have hy : 0 < k-z := sub_pos.mpr (lt_of_not_ge hle)
    have hb := product_bound_one (fun j ↦ density rate μ s (x j)) (φ 0/s)
      (fun _ ↦ density_nonneg hr) (fun _ ↦ (gaussian_peak_strict hr hs).le) i
    by_cases h2 : k ≤ 2*z
    · apply le_trans ?_ ((le_max_left _ _).trans (le_max_right _ _))
      have htail : weightedTail (k-z) ≤ 1/2 := by
        have h := weightedTail_strictAnti.antitone hy.le
        simpa [weightedTail,cdf_zero] using h
      have hrate : rate/2 ≤ z/s := by
        apply (le_div_iff₀ hs).mpr
        dsimp [k] at h2
        nlinarith only [h2]
      have hf : density rate μ s (x i) ≤ (z/s)*exp (-(z^2/2)) := by
        rw [density_weightedTail rate μ (x i) hs, neg_div]
        calc
          _ ≤ rate*exp (-(z^2/2))*(1/2) :=
            mul_le_mul_of_nonneg_left htail (mul_pos hr (exp_pos _)).le
          _ = (rate/2)*exp (-(z^2/2)) := by ring
          _ ≤ _ := mul_le_mul_of_nonneg_right hrate (exp_pos _).le
      calc
        _ ≤ (φ 0/s)^(n-1)*((z/s)*exp (-(z^2/2))) := hb.trans
          (mul_le_mul_of_nonneg_left hf (by positivity))
        _ = _ := by
          have he : (n-1)+2 = n+1 := by have := Nat.sub_add_cancel (Nat.zero_lt_of_lt i.isLt); omega
          rw [← he, pow_add]
          dsimp [z]
          simp only [div_eq_mul_inv,mul_pow]
          ring
    · apply le_trans ?_ ((le_max_right _ _).trans (le_max_right _ _))
      have hrate : rate/(k-z) ≤ 2/s := by
        apply (div_le_div_iff₀ hy hs).mpr
        change k ≤ 2*(k-z)
        linarith [lt_of_not_ge h2]
      have hf : density rate μ s (x i) ≤ (2/s)*φ 0*exp (-(z^2/2)) := by
        rw [density_weightedTail rate μ (x i) hs, neg_div]
        calc
          _ ≤ rate*exp (-(z^2/2))*(φ 0/(k-z)) :=
            mul_le_mul_of_nonneg_left (scaled_mills_upper hy) (mul_pos hr (exp_pos _)).le
          _ = (rate/(k-z))*φ 0*exp (-(z^2/2)) := by ring
          _ ≤ _ := mul_le_mul_of_nonneg_right
            (mul_le_mul_of_nonneg_right hrate hC.le) (exp_pos _).le
      calc
        _ ≤ (φ 0/s)^(n-1)*((2/s)*φ 0*exp (-(z^2/2))) :=
          hb.trans (mul_le_mul_of_nonneg_left hf (by positivity))
        _ = _ := by
          have hsucc : (n-1)+1 = n := Nat.sub_add_cancel (Nat.zero_lt_of_lt i.isLt)
          rw [← hsucc]
          dsimp [z]
          simp only [pow_succ,div_eq_mul_inv,mul_pow]
          ring

private theorem small_scale_large_rate_bound {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i ≠ μ) {J : ℝ} (hJ : 0 < J) :
    ∃ a > 0, ∃ R > 0, ∀ s rate : ℝ, 0 < s → s ≤ a → R ≤ rate →
      (∏ j, density rate μ s (x j)) ≤ J := by
  rcases lt_or_gt_of_ne hi with hi | hi
  · have h : Tendsto (fun s : ℝ ↦ (φ 0)^n*(s⁻¹)^n*exp (-(((μ-x i)/s)^2/2)))
        (𝓝[>] 0) (𝓝 0) := by
      simpa only [mul_zero, mul_assoc] using
        (gauss_power_tail (sub_pos.mpr hi) n).const_mul ((φ 0)^n)
    obtain ⟨δ,hδ,hb⟩ := Metric.mem_nhdsWithin_iff.mp (h.eventually (eventually_lt_nhds hJ))
    refine ⟨δ/2,half_pos hδ,1,zero_lt_one,fun s rate hs hsa hr ↦ ?_⟩
    refine (gauss_bound_if_left x μ hi (zero_lt_one.trans_le hr) hs).trans ?_
    apply (hb ⟨?_,hs⟩).le
    rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]
    linarith
  · let d := x i-μ
    have hd : 0 < d := sub_pos.mpr hi
    obtain ⟨R,hR⟩ := eventually_atTop.mp
      ((exp_power_tail (half_pos hd) n).eventually (eventually_lt_nhds hJ))
    have h₁ : Tendsto (fun s : ℝ ↦ (φ 0)^(n-1)*d*(s⁻¹)^(n+1)*exp (-((d/s)^2/2)))
        (𝓝[>] 0) (𝓝 0) := by
      simpa only [mul_zero,mul_assoc] using (gauss_power_tail hd (n+1)).const_mul ((φ 0)^(n-1)*d)
    have h₂ : Tendsto (fun s : ℝ ↦ 2*(φ 0)^n*(s⁻¹)^n*exp (-((d/s)^2/2)))
        (𝓝[>] 0) (𝓝 0) := by
      simpa only [mul_zero,mul_assoc] using (gauss_power_tail hd n).const_mul (2*(φ 0)^n)
    obtain ⟨δ,hδ,hb⟩ := Metric.mem_nhdsWithin_iff.mp
      ((h₁.eventually (eventually_lt_nhds hJ)).and (h₂.eventually (eventually_lt_nhds hJ)))
    refine ⟨δ/2, half_pos hδ, max 1 R, zero_lt_one.trans_le (le_max_left _ _),
      fun s rate hs hsa hr ↦ ?_⟩
    have hb' := hb ⟨by rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]; linarith,hs⟩
    exact (joint_collapse_bound x μ hi (zero_lt_one.trans_le ((le_max_left _ _).trans hr)) hs).trans
      (max_le (hR _ ((le_max_right _ _).trans hr)).le (max_le hb'.1.le hb'.2.le))

/-- The small-Gaussian-scale, large-rate corner has zero limiting
likelihood whenever a residual at the held location is nonzero. -/
theorem held_location_joint_collapse {α : Type*} {f : Filter α} {n : ℕ}
    (x : Fin n → ℝ) (m : ℝ) {i : Fin n} (hi : x i ≠ m) {r s : α → ℝ}
    (hr : Tendsto r f atTop) (hs : Tendsto s f (𝓝[>] 0)) :
    Tendsto (fun t ↦ ∏ j, density (r t) m (s t) (x j)) f (𝓝 0) := by
  apply tendsto_order.mpr
  constructor
  · intro v hv
    filter_upwards [hr.eventually (eventually_gt_atTop (0 : ℝ))] with t ht
    exact hv.trans_le (Finset.prod_nonneg fun _ _ ↦ density_nonneg ht)
  · intro v hv
    obtain ⟨a,ha,R,_,hcut⟩ := small_scale_large_rate_bound x m hi (half_pos hv)
    have hS : ∀ᶠ z : ℝ in 𝓝[>] 0, z ∈ Ioo 0 a := Ioo_mem_nhdsGT ha
    filter_upwards [hr.eventually (eventually_ge_atTop R),hs.eventually hS] with t ht ht'
    exact (hcut _ _ ht'.1 ht'.2.le ht).trans_lt (half_lt_self hv)

/-- The fixed-location Gaussian face remains valid for jointly moving
rate and scale when the Gaussian scale has a positive limit. -/
theorem held_location_normal_limit {α : Type*} {f : Filter α} {n : ℕ}
    (x : Fin n → ℝ) (m : ℝ) {r s : α → ℝ} {s₀ : ℝ} (hs₀ : 0 < s₀)
    (hr : Tendsto r f atTop) (hs : Tendsto s f (𝓝 s₀)) :
    Tendsto (fun t ↦ ∏ j, density (r t) m (s t) (x j)) f (𝓝 (normalTrace x m s₀)) := by
  apply tendsto_finsetProd Finset.univ
  intro j _
  let z (t : α) := (x j-m)/s t
  let Z := (x j-m)/s₀
  let y (t : α) := r t*s t-z t
  have hz : Tendsto z f (𝓝 Z) := tendsto_const_nhds.div hs hs₀.ne'
  have hy : Tendsto y f atTop := (hr.atTop_mul_pos hs₀ hs).atTop_add hz.neg
  have hscale : Tendsto (fun t ↦ (1+z t*(y t)⁻¹)/s t) f (𝓝 (1/s₀)) := by
    simpa only [mul_zero,add_zero,Pi.div_def,Function.comp_apply] using
      ((hz.mul (tendsto_inv_atTop_zero.comp hy)).const_add (1 : ℝ)).div hs hs₀.ne'
  have hΦ : Tendsto (fun t ↦ y t * weightedTail (y t)) f (𝓝 (φ 0)) := by
    simpa [weightedTail,Function.comp_def,phi_formula,mul_assoc] using mills.comp hy
  have hExp := Real.continuous_exp.continuousAt.tendsto.comp ((hz.pow 2).div_const 2).neg
  have h := (hΦ.mul hscale).mul hExp
  have htarget : (φ 0*(1/s₀))*exp (-(Z^2/2)) = φ ((x j-m)/s₀)/s₀ := by
    simp only [Z,phi_formula,zero_pow (by decide : (2 : ℕ) ≠ 0),zero_div,neg_zero,exp_zero,mul_one]
    ring
  rw [htarget] at h
  refine h.congr' ?_
  filter_upwards [hy.eventually (eventually_gt_atTop (0 : ℝ)),
    hs.eventually (eventually_gt_nhds hs₀)] with t ht ht'
  rw [density_weightedTail (r t) m (x j) ht',neg_div]
  have he : y t*((1+z t*(y t)⁻¹)/s t) = r t := by
    field_simp [ht.ne',ht'.ne']
    dsimp [y]
    ring
  change y t * weightedTail (y t) * ((1+z t*(y t)⁻¹)/s t) * exp (-(z t^2/2)) =
    r t * exp (-(z t^2/2)) * weightedTail (y t)
  linear_combination (weightedTail (y t)*exp (-(z t^2/2)))*he

/-- The matched-location exponential trace is joint in Gaussian scale
and rate. Observations at the held location retain half weight. -/
theorem held_location_exponential_limit {α : Type*} {f : Filter α} {n : ℕ}
    (x : Fin n → ℝ) (m : ℝ) {r s : α → ℝ} {r₀ : ℝ}
    (hr : Tendsto r f (𝓝 r₀)) (hs : Tendsto s f (𝓝[>] 0)) :
    Tendsto (fun t ↦ ∏ j, density (r t) m (s t) (x j)) f (𝓝 (exponentialTrace x r₀ m)) := by
  apply tendsto_finsetProd Finset.univ
  intro j _
  have hk := hr.mul (hs.mono_right nhdsWithin_le_nhds)
  have harg : Tendsto (fun t ↦ Φ ((x j-m)/s t-r t*s t)) f
      (𝓝 (if m < x j then 1 else if x j < m then 0 else 1/2)) := by
    rcases lt_trichotomy m (x j) with hj | hj | hj
    · rw [ite_eq_left hj]
      have H := ((tendsto_inv_nhdsGT_zero.comp hs).const_mul_atTop
        (sub_pos.mpr hj)).atTop_add hk.neg
      simpa only [Function.comp_def,div_eq_mul_inv,sub_eq_add_neg] using cdf_atTop.comp H
    · simp only [← hj,lt_irrefl,ite_false,sub_self,zero_div,zero_sub]
      simpa only [Function.comp_def, mul_zero, neg_zero, cdf_zero] using
        cdf_continuous.continuousAt.tendsto.comp hk.neg
    · rw [ite_eq_right hj.not_gt,ite_eq_left hj]
      have H := ((tendsto_inv_nhdsGT_zero.comp hs).const_mul_atTop_of_neg
        (sub_neg.mpr hj)).atBot_add hk.neg
      simpa only [Function.comp_def,div_eq_mul_inv,sub_eq_add_neg] using cdf_atBot.comp H
  have hExp := Real.continuous_exp.continuousAt.tendsto.comp
    (((hk.pow 2).div_const 2).sub (hr.mul_const (x j-m)))
  simpa only [Function.comp_def,density,mul_zero,zero_pow (by decide : (2 : ℕ) ≠ 0),
    zero_div,zero_sub,← neg_mul] using (hr.mul hExp).mul harg

private theorem trace_bdd_of_ends {f : ℝ → ℝ} (hf : ContinuousOn f (Ioi 0))
    (hsmall : Tendsto f (𝓝[>] 0) (𝓝 0)) (hlarge : Tendsto f atTop (𝓝 0)) :
    BddAbove (f '' Ioi 0) := by
  obtain ⟨δ,hδ,hδ'⟩ := Metric.mem_nhdsWithin_iff.mp
    (hsmall.eventually (eventually_lt_nhds (zero_lt_one : (0 : ℝ) < 1)))
  obtain ⟨U,hU⟩ := eventually_atTop.mp
    (hlarge.eventually (eventually_lt_nhds (zero_lt_one : (0 : ℝ) < 1)))
  have hpos : Icc (δ/2) U ⊆ Ioi 0 := fun _ h ↦ (half_pos hδ).trans_le h.1
  have hK : IsCompact (f '' Icc (δ/2) U) :=
    isCompact_Icc.image_of_continuousOn (hf.mono hpos)
  obtain ⟨M,hM⟩ := hK.bddAbove
  refine ⟨max 1 M, ?_⟩
  rintro _ ⟨s,hs,rfl⟩
  change 0 < s at hs
  by_cases hl : δ/2 ≤ s
  · by_cases hu : s ≤ U
    · exact (hM ⟨s,⟨hl,hu⟩,rfl⟩).trans (le_max_right _ _)
    · exact (hU s (le_of_not_ge hu)).le.trans (le_max_left _ _)
  · have hb : s ∈ Metric.ball (0 : ℝ) δ ∩ Ioi 0 := by
      refine ⟨?_,hs⟩
      rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]
      linarith [lt_of_not_ge hl]
    exact (show f s < 1 from hδ' hb).le.trans (le_max_left _ _)

private theorem phi_at_most_peak (z : ℝ) : φ z ≤ φ 0 := by
  rw [phi_formula,phi_formula]
  simp only [zero_pow (by decide : (2 : ℕ) ≠ 0),zero_div,neg_zero,exp_zero]
  exact mul_le_mul_of_nonneg_left (exp_le_one_iff.mpr (neg_nonpos.mpr (by positivity)))
    (by positivity)

private theorem exp_trace_nonneg {n : ℕ} (x : Fin n → ℝ) {rate μ : ℝ} (hr : 0 < rate) :
    0 ≤ exponentialTrace x rate μ := by
  unfold exponentialTrace
  refine Finset.prod_nonneg fun i _ ↦ mul_nonneg (mul_pos hr (exp_pos _)).le ?_
  split_ifs <;> norm_num

private theorem normalTrace_scale_bounds {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i ≠ μ) :
    Tendsto (normalTrace x μ) (𝓝[>] 0) (𝓝 0) ∧
      Tendsto (normalTrace x μ) atTop (𝓝 0) ∧
      BddAbove (normalTrace x μ '' Ioi 0) := by
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  let d := |x i-μ|
  have hd : 0 < d := abs_pos.mpr (sub_ne_zero.mpr hi)
  have hsmall : Tendsto (normalTrace x μ) (𝓝[>] 0) (𝓝 0) := by
    have ht := (gauss_power_tail hd n).const_mul ((φ 0)^n)
    simp only [mul_zero] at ht
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ht ?_ ?_
    · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s (hs : 0 < s)
      exact Finset.prod_nonneg fun _ _ ↦ div_nonneg (phi_pos _).le hs.le
    · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s (hs : 0 < s)
      have hbound := product_bound_one (fun j ↦ φ ((x j-μ)/s)/s) (φ 0/s)
        (fun _ ↦ div_nonneg (phi_pos _).le hs.le)
        (fun _ ↦ div_le_div_of_nonneg_right (phi_at_most_peak _) hs.le) i
      refine hbound.trans_eq ?_
      have hφ : φ ((x i-μ)/s) = φ 0 * exp (-((d/s)^2/2)) := by
        simp only [phi_formula, d, div_pow, sq_abs, zero_pow (by decide : (2 : ℕ) ≠ 0),
          zero_div, neg_zero, exp_zero, mul_one]
      rw [hφ, div_eq_mul_inv (φ 0), mul_pow]
      have hsucc : (n-1)+1 = n := Nat.sub_add_cancel hn
      rw [← hsucc]
      simp only [Nat.add_sub_cancel_right, pow_succ]
      ring
  have hlarge : Tendsto (normalTrace x μ) atTop (𝓝 0) := by
    have ht (j : Fin n) : Tendsto (fun s ↦ φ ((x j-μ)/s)/s) atTop (𝓝 0) := by
      have h := tendsto_inv_atTop_zero.const_mul (φ 0)
      simp only [mul_zero] at h
      refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds h ?_ ?_
      · exact (eventually_gt_atTop (0 : ℝ)).mono fun _ hs ↦ div_nonneg (phi_pos _).le hs.le
      · exact (eventually_gt_atTop (0 : ℝ)).mono fun _ hs ↦
          by simpa only [div_eq_mul_inv] using div_le_div_of_nonneg_right (phi_at_most_peak _) hs.le
    change Tendsto (fun s ↦ ∏ j, φ ((x j-μ)/s)/s) atTop (𝓝 0)
    simpa only [Finset.prod_const, Finset.card_univ, Fintype.card_fin, zero_pow hn.ne'] using
      tendsto_finsetProd Finset.univ (fun j _ ↦ ht j)
  have hc : ContinuousOn (normalTrace x μ) (Ioi 0) := by
    unfold normalTrace
    refine continuousOn_finsetProd Finset.univ (fun j _ ↦ ?_)
    have hφ : Continuous φ := continuous_iff_continuousAt.mpr
      (fun t ↦ (phi_hasDerivAt t).continuousAt)
    have hs : ContinuousOn (fun s : ℝ ↦ s) (Ioi 0) := continuous_id.continuousOn
    exact (hφ.comp_continuousOn (continuous_const.continuousOn.div hs
      (fun s hs ↦ (show 0 < s from hs).ne'))).div hs (fun s hs ↦ (show 0 < s from hs).ne')
  exact ⟨hsmall,hlarge,trace_bdd_of_ends hc hsmall hlarge⟩

private theorem exponential_trace_bounds {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i ≠ μ) :
    Tendsto (fun r ↦ exponentialTrace x r μ) (𝓝[>] 0) (𝓝 0) ∧
      Tendsto (fun r ↦ exponentialTrace x r μ) atTop (𝓝 0) ∧
      BddAbove ((fun r ↦ exponentialTrace x r μ) '' Ioi 0) := by
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  let w (j : Fin n) : ℝ := if μ < x j then 1 else if x j < μ then 0 else 1/2
  have hw (j : Fin n) : 0 ≤ w j ∧ w j ≤ 1 := by dsimp [w]; split_ifs <;> norm_num
  have hf (r : ℝ) : exponentialTrace x r μ = ∏ j, r*exp (-r*(x j-μ))*w j := rfl
  have hsmall : Tendsto (fun r ↦ exponentialTrace x r μ) (𝓝[>] 0) (𝓝 0) := by
    simp_rw [hf]
    have h (j : Fin n) : Tendsto (fun r ↦ r*exp (-r*(x j-μ))*w j) (𝓝[>] 0) (𝓝 0) := by
      have hcont : ContinuousAt (fun r : ℝ ↦ r*exp (-r*(x j-μ))*w j) 0 := by fun_prop
      simpa using hcont.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
    simpa [zero_pow hn.ne'] using tendsto_finsetProd Finset.univ (fun j _ ↦ h j)
  have hlarge : Tendsto (fun r ↦ exponentialTrace x r μ) atTop (𝓝 0) := by
    by_cases hside : ∃ j, x j < μ
    · obtain ⟨j,hj⟩ := hside
      have hz (r : ℝ) : exponentialTrace x r μ = 0 := by
        rw [hf]
        exact Finset.prod_eq_zero (Finset.mem_univ j)
          (by simp [w,hj,not_lt_of_ge hj.le])
      simp_rw [hz]
      exact tendsto_const_nhds
    · push Not at hside
      have hi' : μ < x i := lt_of_le_of_ne (hside i) hi.symm
      let d := x i-μ
      have hd : 0 < d := sub_pos.mpr hi'
      refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
        (exp_power_tail hd n) ?_ ?_
      · exact (eventually_gt_atTop (0 : ℝ)).mono (fun r hr ↦ exp_trace_nonneg x hr)
      · filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
        rw [hf]
        have hnonneg (j : Fin n) : 0 ≤ r*exp (-r*(x j-μ))*w j :=
          mul_nonneg (mul_pos hr (exp_pos _)).le (hw j).1
        have hpoint (j : Fin n) : r*exp (-r*(x j-μ))*w j ≤ r :=
          (mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (hw j).2).trans
            (mul_le_of_le_one_right hr.le (exp_le_one_iff.mpr
              (mul_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr hr.le) (sub_nonneg.mpr (hside j)))))
        calc
          _ ≤ r^(n-1)*(r*exp (-r*(x i-μ))*w i) := product_bound_one _ r hnonneg hpoint i
          _ ≤ r^(n-1)*(r*exp (-(r*d))) := by
            rw [show -r*(x i-μ) = -(r*d) by dsimp [d]; ring]
            exact mul_le_mul_of_nonneg_left
              (mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (hw i).2) (by positivity)
          _ = _ := by rw [← mul_assoc, ← pow_succ, Nat.sub_add_cancel hn]
  have hc : Continuous (fun r ↦ exponentialTrace x r μ) := by
    simp_rw [hf]
    exact continuous_finsetProd Finset.univ (fun j _ ↦ by fun_prop)
  exact ⟨hsmall,hlarge,trace_bdd_of_ends hc.continuousOn hsmall hlarge⟩

private theorem gaussian_relative_bound {rate μ σ v ε Z : ℝ}
    (hr : 0 < rate) (hσ : 0 < σ) (hε : 0 < ε)
    (hz : (v - μ) / σ ≤ Z) (hR : (1 + ε) * Z < ε * (rate * σ)) :
    density rate μ σ v ≤ (1+ε)*(φ ((v-μ)/σ)/σ) := by
  let k := rate*σ
  let z := (v-μ)/σ
  change (1+ε)*Z < ε*k at hR
  have h : (1+ε)*z < ε*k := (mul_le_mul_of_nonneg_left hz (by positivity)).trans_lt hR
  have hk : 0 < k := mul_pos hr hσ
  have hy : 0 < k-z := by
    by_contra! hn
    have h' := mul_le_mul_of_nonneg_left (show k ≤ z by linarith) (show 0 ≤ 1+ε by positivity)
    linarith only [h, h', hk]
  have hf : rate/(k-z) ≤ (1+ε)/σ := by
    apply (div_le_div_iff₀ hy hσ).mpr
    change k ≤ (1+ε)*(k-z)
    nlinarith only [h]
  have hφ : φ z = φ 0 * exp (-(z^2/2)) := by simp [phi_formula]
  rw [density_weightedTail rate μ v hσ, neg_div]
  change rate*exp (-(z^2/2))*weightedTail (k-z) ≤ (1+ε)*(φ z/σ)
  rw [hφ]
  calc
    _ ≤ rate*exp (-(z^2/2))*(φ 0/(k-z)) :=
      mul_le_mul_of_nonneg_left (scaled_mills_upper hy) (mul_pos hr (exp_pos _)).le
    _ = (rate/(k-z))*(φ 0*exp (-(z^2/2))) := by ring
    _ ≤ ((1+ε)/σ)*(φ 0*exp (-(z^2/2))) :=
      mul_le_mul_of_nonneg_right hf (mul_pos (phi_pos _) (exp_pos _)).le
    _ = _ := by ring

private theorem bounded_rate_small_scale {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {E J R : ℝ} (hE : 0 ≤ E) (hJE : E < J)
    (hbound : ∀ rate > 0, exponentialTrace x rate μ ≤ E) :
    ∃ a > 0, ∀ s rate : ℝ, 0 < s → s ≤ a → 0 < rate → rate ≤ R →
      (∏ j, density rate μ s (x j)) ≤ J := by
  by_cases hside : ∃ j, x j < μ
  · obtain ⟨j,hj⟩ := hside
    have ht : Tendsto (fun s : ℝ ↦ (φ 0)^n*(s⁻¹)^n*exp (-(((μ-x j)/s)^2/2)))
        (𝓝[>] 0) (𝓝 0) := by
      simpa only [mul_zero,mul_assoc] using
        (gauss_power_tail (sub_pos.mpr hj) n).const_mul ((φ 0)^n)
    obtain ⟨a,ha,ha'⟩ := Metric.mem_nhdsWithin_iff.mp
      (ht.eventually (eventually_lt_nhds (hE.trans_lt hJE)))
    refine ⟨a/2, half_pos ha, fun s rate hs hs' hr _ ↦
      (gauss_bound_if_left x μ hj hr hs).trans ?_⟩
    apply (ha' ⟨?_,hs⟩).le
    rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]
    linarith
  · push Not at hside
    have ht : Tendsto (fun s : ℝ ↦ E*exp ((n : ℝ)*((R*s)^2/2))) (𝓝[>] 0) (𝓝 E) := by
      have h : ContinuousAt (fun s : ℝ ↦ E*exp ((n : ℝ)*((R*s)^2/2))) 0 := by fun_prop
      simpa using h.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
    obtain ⟨a,ha,ha'⟩ := Metric.mem_nhdsWithin_iff.mp (ht.eventually (eventually_lt_nhds hJE))
    refine ⟨a/2,half_pos ha,fun s rate hs hsa hr hrR ↦ ?_⟩
    let D := exp ((rate*s)^2/2)
    let f (j : Fin n) := rate*exp (-rate*(x j-μ))*(if μ < x j then 1 else 1/2)
    have htrace : (∏ j, f j) = exponentialTrace x rate μ := by
      simp only [f,exponentialTrace,not_lt_of_ge (hside _),ite_false]
    have hterm (j : Fin n) : density rate μ s (x j) ≤ f j*D := by
      unfold density
      have hΦ : Φ ((x j-μ)/s-rate*s) ≤ (if μ < x j then 1 else 1/2) := by
        split_ifs with hj
        · exact (cdf_lt_one _).le
        · have he : x j=μ := le_antisymm (le_of_not_gt hj) (hside j)
          rw [he,sub_self,zero_div,zero_sub,← cdf_zero]
          exact cdf_strictMono.monotone (neg_nonpos.mpr (mul_pos hr hs).le)
      have he : f j*D =
          rate * exp ((rate*s)^2/2-rate*(x j-μ))*(if μ<x j then 1 else 1/2) := by
        rw [sub_eq_add_neg,exp_add]
        unfold f D
        ring_nf
      rw [he]
      exact mul_le_mul_of_nonneg_left hΦ (mul_pos hr (exp_pos _)).le
    have hprod := Finset.prod_le_prod₀ (s := Finset.univ)
      (fun j _ ↦ density_nonneg hr) (fun j _ ↦ hterm j)
    rw [Finset.prod_mul_distrib,Finset.prod_const,Finset.card_univ,Fintype.card_fin,htrace] at hprod
    have hpow : D^n ≤ exp ((n : ℝ)*((R*s)^2/2)) := by
      change exp ((rate*s)^2/2)^n ≤ _
      rw [← exp_nat_mul]
      apply exp_le_exp.mpr
      have h := pow_le_pow_left₀ (mul_pos hr hs).le (mul_le_mul_of_nonneg_right hrR hs.le) 2
      exact mul_le_mul_of_nonneg_left (div_le_div_of_nonneg_right h (by norm_num)) (by positivity)
    have hb := mul_le_mul (hbound rate hr) hpow (by unfold D; positivity) hE
    exact (hprod.trans hb).trans (ha' ⟨by
      rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]; linarith,hs⟩).le

/-- With location held and a nonzero residual, every likelihood level
above both optimized faces has a compact rate-scale superlevel. -/
theorem held_location_superlevel {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {i : Fin n}
    (hi : x i ≠ m) {J : ℝ}
    (hJ : max (sSup (normalTrace x m '' Ioi 0))
      (sSup ((fun r ↦ exponentialTrace x r m) '' Ioi 0)) < J) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Ioi 0 ×ˢ Ioi 0 ∧
      ∀ p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ),
        J ≤ (∏ j, density p.1 m p.2 (x j)) → p ∈ K := by
  let N := sSup (normalTrace x m '' Ioi 0)
  let E := sSup ((fun r ↦ exponentialTrace x r m) '' Ioi 0)
  have hN (s : ℝ) (hs : 0 < s) : normalTrace x m s ≤ N :=
    le_csSup (normalTrace_scale_bounds x m hi).2.2 (mem_image_of_mem _ hs)
  have hE (r : ℝ) (hr : 0 < r) : exponentialTrace x r m ≤ E :=
    le_csSup (exponential_trace_bounds x m hi).2.2 (mem_image_of_mem _ hr)
  have hE₀ : 0 ≤ E := (exp_trace_nonneg x zero_lt_one).trans (hE 1 zero_lt_one)
  have hJ₀ : 0 < J := hE₀.trans_lt ((le_max_right N E).trans_lt hJ)
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  let e := min 1 (J/2)
  have he : 0 < e := lt_min zero_lt_one (half_pos hJ₀)
  have hePow : e^n < J := by
    have h : e^n ≤ e := by
      obtain ⟨k,hk⟩ := Nat.exists_eq_succ_of_ne_zero hn.ne'
      rw [hk,pow_succ]
      exact (mul_le_mul_of_nonneg_right (pow_le_one₀ he.le (min_le_left _ _)) he.le).trans_eq
        (one_mul e)
    exact (h.trans (min_le_right _ _)).trans_lt (half_lt_self hJ₀)
  obtain ⟨a,ha,R₀,hR₀,hcorner⟩ := small_scale_large_rate_bound x m hi (half_pos hJ₀)
  have hε : ∃ ε > 0, (1+ε)^n*N < J := by
    have hc : Tendsto (fun ε : ℝ ↦ (1+ε)^n*N) (𝓝[>] 0) (𝓝 N) := by
      have hbase : ContinuousAt (fun ε : ℝ ↦ (1+ε)^n*N) 0 := by fun_prop
      simpa using hbase.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
    have hd : ∀ᶠ ε : ℝ in 𝓝[>] 0, 0 < ε := self_mem_nhdsWithin
    exact (hd.and (hc.eventually (eventually_lt_nhds ((le_max_left N E).trans_lt hJ)))).exists
  obtain ⟨ε,hε,hgap⟩ := hε
  let X := ∑ j, |x j-m|
  have hX : 0 ≤ X := Finset.sum_nonneg fun _ _ ↦ abs_nonneg _
  let Z := X/a
  let R := max R₀ (1+(1+ε)*Z/(ε*a))
  have hR : 0 < R := hR₀.trans_le (le_max_left _ _)
  let T := (J+E)/2
  have hET : E < T := by dsimp [T]; linarith only [(le_max_right N E).trans_lt hJ]
  have hTJ : T < J := by dsimp [T]; linarith only [(le_max_right N E).trans_lt hJ]
  obtain ⟨δ,hδ,hfloor⟩ := bounded_rate_small_scale x m (R := R) hE₀ hET hE
  let S := φ 0/e
  refine ⟨Icc e R ×ˢ Icc δ S,isCompact_Icc.prod isCompact_Icc,
    fun _ hp ↦ ⟨he.trans_le hp.1.1,hδ.trans_le hp.2.1⟩,?_⟩
  rintro ⟨r,s⟩ ⟨hr,hs⟩ h
  change 0 < r at hr
  change 0 < s at hs
  change J ≤ ∏ j, density r m s (x j) at h
  have hall (hb : ∀ j, density r m s (x j) ≤ e) : (∏ j, density r m s (x j)) < J := by
    have ht := Finset.prod_le_prod₀ (s := Finset.univ)
      (fun j _ ↦ density_nonneg hr) (fun j _ ↦ hb j)
    exact (show (∏ j, density r m s (x j)) ≤ e^n by simpa using ht).trans_lt hePow
  have hlo : e ≤ r := by
    by_contra! hlt
    exact (hall fun j ↦ (rate_peak hr hs).trans hlt.le).not_ge h
  have hhi : s ≤ S := by
    by_contra! hgt
    apply (hall fun j ↦ (gaussian_peak_strict (μ := m) (x := x j) hr hs).le.trans ?_).not_ge h
    exact (div_le_iff₀ hs).mpr (by
      simpa only [mul_comm] using (div_le_iff₀ he).mp hgt.le)
  have hR' : r ≤ R := by
    by_contra! hgt
    by_cases ha' : a ≤ s
    · have hineq : (1+ε)*Z < ε*(r*s) := by
        have hbig : 1+(1+ε)*Z/(ε*a) < r := (le_max_right _ _).trans_lt hgt
        have h₁ := (div_lt_iff₀ (mul_pos hε ha)).mp
          (show (1+ε)*Z/(ε*a) < r by linarith)
        have h₂ := mul_le_mul_of_nonneg_left ha' (mul_pos hε hr).le
        nlinarith only [h₁,h₂]
      have hterm (j : Fin n) : density r m s (x j) ≤ (1+ε)*(φ ((x j-m)/s)/s) := by
        apply gaussian_relative_bound hr hs hε ?_ hineq
        calc
          _ ≤ |x j-m|/s := div_le_div_of_nonneg_right (le_abs_self _) hs.le
          _ ≤ X/s := div_le_div_of_nonneg_right
            (Finset.single_le_sum (f := fun j ↦ |x j-m|) (fun _ _ ↦ abs_nonneg _)
              (Finset.mem_univ j)) hs.le
          _ ≤ _ := div_le_div_of_nonneg_left hX ha ha'
      have hp := Finset.prod_le_prod₀ (s := Finset.univ)
        (fun j _ ↦ density_nonneg hr) (fun j _ ↦ hterm j)
      have hh : (∏ j, density r m s (x j)) ≤ (1+ε)^n*normalTrace x m s := by
        simpa [Finset.prod_mul_distrib,normalTrace] using hp
      have hle := mul_le_mul_of_nonneg_left (hN s hs) (by positivity : 0 ≤ (1+ε)^n)
      exact ((hh.trans hle).trans_lt hgap).not_ge h
    · exact ((hcorner s r hs (le_of_not_ge ha')
        ((le_max_left _ _).trans hgt.le)).trans_lt (half_lt_self hJ₀)).not_ge h
  have hδ' : δ ≤ s := by
    by_contra! hlt
    exact ((hfloor s r hs hlt.le hr hR').trans_lt hTJ).not_ge h
  exact ⟨⟨hlo,hR'⟩,⟨hδ',hhi⟩⟩

private theorem held_location_attainment {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {i : Fin n} (hi : x i ≠ m) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 m p.2 (x j)
    let B := max (sSup (normalTrace x m '' Ioi 0))
      (sSup ((fun r ↦ exponentialTrace x r m) '' Ioi 0))
    (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ r s : ℝ, 0 < r ∧ 0 < s ∧ B ≤ L (r,s) := by
  dsimp only
  refine ⟨(held_location_criterion x m hi).mp, ?_⟩
  rintro ⟨r,s,hr,hs,h⟩
  let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
  let L (p : ℝ × ℝ) := ∏ j, density p.1 m p.2 (x j)
  let B := max (sSup (normalTrace x m '' Ioi 0))
    (sSup ((fun r ↦ exponentialTrace x r m) '' Ioi 0))
  by_cases hb : ∃ p ∈ D, B < L p
  · obtain ⟨p,hp,hbetter⟩ := hb
    obtain ⟨K,hK,hsub,hcut⟩ := held_location_superlevel x m hi hbetter
    have hc : ContinuousOn L D := by
      refine continuousOn_finsetProd Finset.univ fun j _ ↦ ?_
      unfold density
      refine ContinuousOn.mul (by fun_prop) (cdf_continuous.comp_continuousOn ?_)
      refine ContinuousOn.sub (ContinuousOn.div (by fun_prop) continuous_snd.continuousOn
        (fun p hp ↦ (show 0 < p.2 from hp.2).ne')) (by fun_prop)
    exact RBProfile.exists_isMaxOn_of_compact_superlevel hK hsub hc hp hcut
  · exact ⟨(r,s),⟨hr,hs⟩,fun q hq ↦
      (le_of_not_gt (fun hv ↦ hb ⟨q,hq,hv⟩)).trans h⟩

/-- At held location failure is exactly unbounded concentration when all
residuals vanish, or strict loss to the larger of the two boundary heights. -/
theorem held_location_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 m p.2 (x j)
    let B := max (sSup (normalTrace x m '' Ioi 0))
      (sSup ((fun r ↦ exponentialTrace x r m) '' Ioi 0))
    (¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      ((∀ j, x j = m) ∧
        Tendsto (fun s : ℝ ↦ L (s⁻¹,s)) (𝓝[>] 0) atTop) ∨
      ((∃ j, x j ≠ m) ∧ (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B) := by
  dsimp only
  by_cases h : ∃ j, x j ≠ m
  · obtain ⟨i, hi⟩ := h
    have hnz : ¬ ∀ j, x j = m := fun hc ↦ hi (hc i)
    have hc := held_location_attainment x m hi
    dsimp only at hc
    rw [hc]
    have h : ∃ j, x j ≠ m := ⟨i,hi⟩
    simp only [hnz, false_and, false_or, h, true_and]
    constructor
    · intro hnone
      have hlt (p : ℝ × ℝ) (hp : p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) :
          (∏ j, density p.1 m p.2 (x j)) <
            max (sSup (normalTrace x m '' Ioi 0))
              (sSup ((fun r ↦ exponentialTrace x r m) '' Ioi 0)) :=
        lt_of_not_ge (fun hb ↦ hnone ⟨p.1,p.2,hp.1,hp.2,hb⟩)
      refine ⟨hlt, ⟨?_, ?_⟩⟩
      · rintro _ ⟨p,hp,rfl⟩
        exact (hlt p hp).le
      · intro u hu
        apply max_le
        · apply csSup_le ⟨_, mem_image_of_mem _ (show (1 : ℝ) ∈ Ioi 0 from zero_lt_one)⟩
          rintro _ ⟨s, hs, rfl⟩
          apply le_of_tendsto (tendsto_finsetProd Finset.univ
            (fun j _ ↦ normal_path m (x j) hs))
          exact (eventually_gt_atTop (0 : ℝ)).mono fun r hr ↦ hu ⟨(r,s), ⟨hr,hs⟩, rfl⟩
        · apply csSup_le ⟨_, mem_image_of_mem _ (show (1 : ℝ) ∈ Ioi 0 from zero_lt_one)⟩
          rintro _ ⟨r, hr, rfl⟩
          apply le_of_tendsto (tendsto_finsetProd Finset.univ
            (fun j _ ↦ exponential_path r m (x j)))
          have he : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
          exact he.mono fun s hs ↦ hu ⟨(r,s), ⟨hr,hs⟩, rfl⟩
    · rintro ⟨hlt, _⟩ ⟨r,s,hr,hs,hb⟩
      exact (hlt (r,s) ⟨hr,hs⟩).not_ge hb
  · have hx : ∀ j, x j = m := fun j ↦ not_not.mp (fun hj ↦ h ⟨j,hj⟩)
    have hc : x = fun _ ↦ m := funext hx
    have ht : Tendsto (fun s : ℝ ↦ ∏ j, density s⁻¹ m s (x j)) (𝓝[>] 0) atTop := by
      rw [hc]
      exact constant_sample_unbounded hn m
    simp only [h, false_and, or_false]
    simp only [hx, implies_true, true_and]
    refine ⟨fun _ ↦ constant_sample_unbounded hn m, fun _ ↦ ?_⟩
    rintro ⟨p, hp, hm⟩
    have he : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
    obtain ⟨s, hs, hv⟩ := (he.and
      (ht.eventually_gt_atTop (∏ j, density p.1 m p.2 (x j)))).exists
    apply hv.not_ge
    simpa only [hx] using (isMaxOn_iff.mp hm (s⁻¹,s) (show (s⁻¹,s) ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) from
      ⟨inv_pos.mpr hs, hs⟩))

end UnivariateML.RBEMG

end
