module

public import UnivariateML.Family.LocScaleBhatt
public import UnivariateML.Family.LocScaleCommon
public import UnivariateML.Generic.RBProfile
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics

import Mathlib.Tactic

set_option linter.style.header false

/-! # Bhattacharjee failure with held width -/

@[expose] public section

open Set Filter Real MeasureTheory
open scoped Topology Interval
open UnivariateML.SkewLSBhatt UnivariateML.LocScaleBhatt UnivariateML.SkewLSGaussian

namespace UnivariateML.RBBhatt

/-- The held-centre problem either collapses on the normal seam, has a
strictly unattained penalized uniform envelope, or has a finite maximum. -/
theorem held_width_centre_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a m : ℝ} (ha : 0 ≤ a) :
    (¬ ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (likelihood x a m) (Ioi 0) s) ↔
      (a = 0 ∧ (∀ i, x i = m) ∧
        Tendsto (likelihood x a m) (𝓝[>] 0) atTop) ∨
      (0 < a ∧ (∀ i, |x i - m| ≤ a) ∧
        let u := ∏ i, (if |x i - m| = a then (1 : ℝ)/2 else 1)/(2*a)
        IsLUB (likelihood x a m '' Ioi 0) u ∧ ∀ s > 0, likelihood x a m s < u) := by
  rw [LocScaleBhatt.held_width_centre_failure hn x ha]
  constructor
  · rintro (⟨rfl, hx⟩ | ⟨hp, hx⟩)
    · exact Or.inl ⟨rfl, hx, funext hx ▸ held_zero_width_centre_collapse hn m⟩
    · exact Or.inr ⟨hp, hx, HeldSweepBhatt.held_centre_supremum hn hp hx⟩
  · rintro (⟨hz, hx, _⟩ | ⟨hp, hx, _⟩)
    · exact Or.inl ⟨hz, hx⟩
    · exact Or.inr ⟨hp, hx⟩

/-- Width zero uses precisely the normal argmax. Its attained location
and scale are unique. -/
theorem held_zero_width_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {p : ℝ × ℝ} (hp : p ∈ Normal.domain)
    (hm : IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x 0 q.1 q.2) Normal.domain p) :
    ∃! q, q ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x 0 q.1 q.2) Normal.domain q := by
  have hN := (HeldSweepBhatt.normal_member_argmax x hp).mp hm
  have hx : ∃ i j, x i ≠ x j := by
    by_contra h
    exact Normal.constant_unbounded hn h ⟨_, (hN.isLUB hp).1⟩
  obtain ⟨q, ⟨hq, hqmax⟩, huniq⟩ := (Normal.existsUnique_mle_iff hn x).mpr hx
  exact ⟨q, ⟨hq, (HeldSweepBhatt.normal_member_argmax x hq).mpr hqmax⟩,
    fun z hz ↦ huniq z ⟨hz.1, (HeldSweepBhatt.normal_member_argmax x hz.1).mp hz.2⟩⟩

/-- For held positive width, the exact unattained height uses the
unpenalized uniform in strict coverage and endpoint fractions at equality.
At width zero constant data collapse through the normal family. -/
theorem held_width_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 ≤ a) :
    let D := Normal.domain
    let L := fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2
    (¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (a = 0 ∧ ¬ BddAbove (L '' D)) ∨
      (0 < a ∧ ∃ i j : Fin n, (∀ k, x i ≤ x k) ∧ (∀ k, x k ≤ x j) ∧
        x j - x i ≤ 2*a ∧
        let z := (Finset.univ.filter (fun k ↦ x k = x i)).card
        let o := (Finset.univ.filter (fun k ↦ x k = x j)).card
        let w := (z : ℝ) / ((z : ℝ) + o)
        let u := if x j - x i = 2*a then w^z * (1-w)^o / (2*a)^n
          else (1/(2*a))^n
        IsLUB (L '' D) u ∧ ∀ p ∈ D, L p < u) := by
  dsimp only
  rcases ha.eq_or_lt with he | hpos
  · subst a
    simp only [lt_irrefl,false_and,or_false,true_and]
    constructor
    · intro h
      have hc : ¬ ∃ i j, x i ≠ x j := by
        intro hx
        exact h ((held_width_criterion hn x (show (0 : ℝ) ≤ 0 from le_rfl)).mpr
          (by simpa using hx))
      let i : Fin n := ⟨0,hn⟩
      have hx : x = fun _ ↦ x i := funext fun j ↦ not_not.mp (fun ht ↦ hc ⟨j,i,ht⟩)
      have hsub : likelihood x 0 (x i) '' Ioi 0 ⊆
          (fun p : ℝ × ℝ ↦ likelihood x 0 p.1 p.2) '' Normal.domain := by
        rintro _ ⟨s,hs,rfl⟩
        exact ⟨(x i,s),⟨mem_univ _,hs⟩,rfl⟩
      have hc' : ¬ BddAbove (likelihood x 0 (x i) '' Ioi 0) := by
        have heq : likelihood x 0 (x i) = likelihood (fun _ : Fin n ↦ x i) 0 (x i) :=
          congrArg (fun y ↦ likelihood y 0 (x i)) hx
        rw [heq]
        exact constant_normal_seam hn (x i)
      exact fun hb ↦ hc' (hb.mono hsub)
    · rintro hb ⟨p,hp,hm⟩
      exact hb ⟨_, (hm.isLUB hp).1⟩
  · simp only [hpos.ne',false_and,false_or,true_and,hpos]
    constructor
    · intro h
      have hd : ¬ ∃ i j, 2*a < x j - x i :=
        fun hx ↦ h ((positive_width_iff hn x hpos).mpr hx)
      have hne : (Finset.univ : Finset (Fin n)).Nonempty :=
        ⟨⟨0,hn⟩,Finset.mem_univ _⟩
      obtain ⟨i,_,hi⟩ := Finset.exists_min_image Finset.univ x hne
      obtain ⟨j,_,hj⟩ := Finset.exists_max_image Finset.univ x hne
      have hlo : ∀ k, x i ≤ x k := fun k ↦ hi k (Finset.mem_univ k)
      have hhi : ∀ k, x k ≤ x j := fun k ↦ hj k (Finset.mem_univ k)
      have hij : x j - x i ≤ 2*a := le_of_not_gt (fun hh ↦ hd ⟨i,j,hh⟩)
      refine ⟨i,j,hlo,hhi,hij,?_⟩
      rcases hij.lt_or_eq with hlt | he
      · simpa only [ite_eq_right hlt.ne] using
          held_width_strict_cover_failure hn x hpos hlo hhi hlt
      · obtain ⟨hB,hlt⟩ := HeldSweepBhatt.range_equality_height hn hpos i j hlo hhi he
        rw [ite_eq_left he]
        exact ⟨hB, fun p hp ↦ hlt p.1 p.2 hp.2⟩
    · rintro ⟨i,j,_,_,_,hB,hlt⟩ ⟨p,hp,hm⟩
      exact (hlt p hp).not_ge (hB.2 (hm.isLUB hp).1)

/-- The attained held-centre normal scale is unique. -/
theorem held_zero_width_centre_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (m : ℝ) (h : ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (likelihood x 0 m) (Ioi 0) s) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (likelihood x 0 m) (Ioi 0) s := by
  apply (held_zero_width_centre_criterion hn x m).mpr
  by_contra! hc
  exact ((held_width_centre_failure hn x (show (0 : ℝ) ≤ 0 from le_rfl)).mpr
    (Or.inl ⟨rfl,hc,funext hc ▸ held_zero_width_centre_collapse hn m⟩)) h

private noncomputable def pointDensity (a r s : ℝ) : ℝ :=
  if a = 0 then φ (r / s) / s else
    (Φ ((r + a) / s) - Φ ((r - a) / s)) / (2 * a)

private theorem phi_cont : Continuous φ := continuous_iff_continuousAt.mpr
  (fun x ↦ (phi_hasDerivAt x).continuousAt)

private theorem interval_mass (u v : ℝ) : Φ v - Φ u = ∫ z in u..v, φ z := by
  rw [cdf_integral, cdf_integral, intervalIntegral.integral_Iic_sub_Iic
    phi_integrable.integrableOn phi_integrable.integrableOn]

private theorem density_average {a r s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    pointDensity a r s = (∫ z in (-1 : ℝ)..1, φ ((r + a * z) / s)) / (2*s) := by
  rcases ha.eq_or_lt with h | h
  · subst a
    simp [pointDensity, intervalIntegral.integral_const, smul_eq_mul]
    ring
  · simp only [pointDensity, h.ne', ite_false, interval_mass]
    have he (z : ℝ) : (r + a*z)/s = (a/s)*z + r/s := by ring
    simp_rw [he]
    rw [intervalIntegral.integral_comp_mul_add φ (div_ne_zero h.ne' hs.ne')]
    simp only [smul_eq_mul]
    rw [show a / s * (-1) + r / s = (r-a)/s by ring,
        show a / s * 1 + r / s = (r+a)/s by ring]
    field_simp

private theorem density_pos {a r s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    0 < pointDensity a r s := by
  rcases ha.eq_or_lt with h | h
  · subst a
    simpa only [pointDensity, ite_true] using div_pos (phi_pos (r/s)) hs
  · simp only [pointDensity, h.ne', ite_false]
    exact div_pos (sub_pos.mpr (cdf_strictMono
      ((div_lt_div_iff_of_pos_right hs).mpr (by linarith)))) (by positivity)

private theorem density_bound_width {a r s : ℝ} (ha : 0 < a) :
    pointDensity a r s < 1 / (2*a) := by
  simp only [pointDensity, ha.ne', ite_false]
  apply (div_lt_div_iff_of_pos_right (show 0 < 2*a by positivity)).mpr
  linarith [cdf_lt_one ((r+a)/s), cdf_pos ((r-a)/s)]

private theorem density_bound_local {a r s M : ℝ} (ha : 0 ≤ a) (hs : 0 < s)
    (hM : ∀ z ∈ Icc (-1 : ℝ) 1, φ ((r + a * z) / s) ≤ M) :
    pointDensity a r s ≤ M / s := by
  rw [density_average ha hs]
  have hc : Continuous (fun z : ℝ ↦ φ ((r + a * z) / s)) :=
    phi_cont.comp (by fun_prop)
  have hb := intervalIntegral.integral_mono_on (μ := volume) (show (-1 : ℝ) ≤ 1 by norm_num)
    (hc.intervalIntegrable _ _) (continuous_const.intervalIntegrable _ _) hM
  rw [intervalIntegral.integral_const, smul_eq_mul] at hb
  apply (div_le_iff₀ (show 0 < 2*s by positivity)).mpr
  rw [show M / s * (2 * s) = 2*M by field_simp]
  linarith

private theorem gaussian_peak (z : ℝ) : φ z ≤ φ 0 := by
  simp only [phi_formula, zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div, neg_zero,
    exp_zero, mul_one]
  exact mul_le_of_le_one_right (by positivity) (exp_le_one_iff.mpr (neg_nonpos.mpr (by positivity)))

private theorem density_bound_scale {a r s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    pointDensity a r s ≤ φ 0 / s :=
  density_bound_local ha hs (fun _ _ ↦ gaussian_peak _)

private theorem likelihood_pos {n : ℕ} (x : Fin n → ℝ) {a m s : ℝ}
    (ha : 0 ≤ a) (hs : 0 < s) : 0 < likelihood x a m s :=
  Finset.prod_pos (fun i _ ↦ density_pos (r := x i - m) ha hs)

private theorem density_tail {a r s d : ℝ} (ha : 0 ≤ a) (hs : 0 < s)
    (hd : 0 ≤ d) (hr : a + d ≤ |r|) :
    pointDensity a r s ≤ (φ 0 / s) * exp (-((d/s)^2/2)) := by
  rw [show (φ 0 / s) * exp (-((d/s)^2/2)) = φ 0 * exp (-((d/s)^2/2)) / s by ring]
  apply density_bound_local ha hs
  intro z hz
  have hmul : |a*z| ≤ a := by
    rw [abs_mul,abs_of_nonneg ha]
    exact mul_le_of_le_one_right ha (abs_le.mpr hz)
  have htri : |r| ≤ |r+a*z| + |a*z| := by
    simpa using abs_sub (r+a*z) (a*z)
  have hmag : d/s ≤ |(r+a*z)/s| := by
    rw [abs_div,abs_of_pos hs]
    exact div_le_div_of_nonneg_right (by linarith) hs.le
  have hsq : (d/s)^2 ≤ ((r+a*z)/s)^2 := by
    have H := mul_nonneg (sub_nonneg.mpr hmag)
      (show 0 ≤ |(r+a*z)/s|+d/s by positivity)
    nlinarith only [H,sq_abs ((r+a*z)/s)]
  rw [phi_formula,phi_formula]
  simp only [zero_pow (by decide : (2 : ℕ) ≠ 0),zero_div,neg_zero,exp_zero,mul_one]
  exact mul_le_mul_of_nonneg_left (Real.exp_le_exp.mpr (by linarith)) (by positivity)

private theorem likelihood_gap_bound {n : ℕ} (x : Fin n → ℝ) {a m s d : ℝ}
    (ha : 0 ≤ a) (hs : 0 < s) (hd : 0 ≤ d) {i : Fin n} (hi : a + d ≤ |x i - m|) :
    likelihood x a m s ≤ (φ 0/s)^n * exp (-((d/s)^2/2)) := by
  classical
  let f (j : Fin n) := pointDensity a (x j-m) s
  let G := φ 0/s
  let e := exp (-((d/s)^2/2))
  change (∏ j, f j) ≤ G^n*e
  rw [← Finset.prod_erase_mul Finset.univ f (Finset.mem_univ i)]
  have hprod : (∏ j ∈ Finset.univ.erase i, f j) ≤ G^(n-1) := by
    simpa [f,G,div_pow] using (Finset.prod_le_prod₀ (s := Finset.univ.erase i)
      (fun j _ ↦ (density_pos (r := x j-m) ha hs).le)
      (fun j _ ↦ density_bound_scale (r := x j-m) ha hs))
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  calc
    _ ≤ G^(n-1)*(G*e) := mul_le_mul hprod (density_tail ha hs hd hi)
      (density_pos ha hs).le (pow_nonneg (div_nonneg (phi_pos 0).le hs.le) _)
    _ = _ := by rw [← mul_assoc,← pow_succ,Nat.sub_add_cancel hn]

private theorem likelihood_width_bound {n : ℕ} (x : Fin n → ℝ) {a m s : ℝ}
    (ha : 0 < a) (hs : 0 < s) :
    likelihood x a m s ≤ (1/(2*a))^n := by
  change (∏ j, pointDensity a (x j-m) s) ≤ _
  simpa only [Finset.prod_const,Finset.card_univ,Fintype.card_fin] using
    (Finset.prod_le_prod₀ (s := Finset.univ)
      (fun j _ ↦ (density_pos (r := x j-m) ha.le hs).le)
      (fun j _ ↦ (density_bound_width (r := x j-m) (s := s) ha).le))

private theorem likelihood_scale_bound {n : ℕ} (x : Fin n → ℝ) {a m s : ℝ}
    (ha : 0 ≤ a) (hs : 0 < s) :
    likelihood x a m s ≤ (φ 0/s)^n := by
  change (∏ j, pointDensity a (x j-m) s) ≤ _
  simpa only [Finset.prod_const,Finset.card_univ,Fintype.card_fin] using
    (Finset.prod_le_prod₀ (s := Finset.univ)
      (fun j _ ↦ (density_pos (r := x j-m) ha hs).le)
      (fun j _ ↦ density_bound_scale (r := x j-m) ha hs))

private theorem likelihood_cont {n : ℕ} (x : Fin n → ℝ) (S : Set ℝ) :
    ContinuousOn (fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2)
      (Ici 0 ×ˢ (S ×ˢ Ioi 0)) := by
  let f (i : Fin n) (p : ℝ × (ℝ × ℝ)) (z : ℝ) :=
    φ ((x i-p.2.1+p.1*z)/exp p.2.2)
  have hcont (i : Fin n) : Continuous (fun p : ℝ × (ℝ × ℝ) ↦
      (∫ z in (-1 : ℝ)..1, f i p z)/(2*exp p.2.2)) := by
    have h : Continuous (fun q : (ℝ × (ℝ × ℝ)) × ℝ ↦ f i q.1 q.2) := by
      unfold f
      refine phi_cont.comp (Continuous.div (by fun_prop) (by fun_prop) ?_)
      exact fun q ↦ (exp_pos q.1.2.2).ne'
    simp only [intervalIntegral.integral_of_le (by norm_num : (-1 : ℝ) ≤ 1)]
    simp_rw [← MeasureTheory.integral_Icc_eq_integral_Ioc]
    exact (continuous_parametric_integral_of_continuous (μ := volume) h isCompact_Icc).div
      (by fun_prop) (fun p ↦ mul_ne_zero (by norm_num) (exp_pos p.2.2).ne')
  have hlog : ContinuousOn (fun p : ℝ × (ℝ × ℝ) ↦ (p.1,p.2.1, log p.2.2))
      (Ici 0 ×ˢ (S ×ˢ Ioi 0)) := by
    have hs : Continuous (fun p : ℝ × (ℝ × ℝ) ↦ p.2.2) :=
      continuous_snd.comp continuous_snd
    exact continuous_fst.continuousOn.prodMk
      ((continuous_fst.comp continuous_snd).continuousOn.prodMk
        (hs.continuousOn.log (fun p hp ↦ (show 0 < p.2.2 from hp.2.2).ne')))
  refine ((continuous_finsetProd Finset.univ (fun i _ ↦ hcont i)).comp_continuousOn hlog).congr ?_
  intro p hp
  apply Finset.prod_congr rfl
  intro i _
  change pointDensity p.1 (x i-p.2.1) p.2.2 =
    (∫ z in (-1 : ℝ)..1, f i (p.1,p.2.1,log p.2.2) z)/(2*exp (log p.2.2))
  simpa only [f, exp_log (show 0 < p.2.2 from hp.2.2)] using
    density_average (r := x i-p.2.1) hp.1 hp.2.2

private theorem gap_bound_small_scale (n : ℕ) {d J : ℝ} (hd : 0 < d) (hJ : 0 < J) :
    ∃ l > 0, ∀ s ∈ Ioo 0 l, (φ 0/s)^n * exp (-((d/s)^2/2)) < J := by
  let b := d^2/2
  have hb : 0 < b := by dsimp [b]; positivity
  have ht : Tendsto (fun v : ℝ ↦ (φ 0)^n * (v^n * exp (-b*v))) atTop (𝓝 0) := by
    simpa only [rpow_natCast, mul_zero] using
      (tendsto_rpow_mul_exp_neg_mul_atTop_nhds_zero (n : ℝ) b hb).const_mul ((φ 0)^n)
  have hclose := (ht.comp tendsto_inv_nhdsGT_zero).eventually (eventually_lt_nhds hJ)
  obtain ⟨l,hl,hl'⟩ := Metric.mem_nhdsWithin_iff.mp hclose
  refine ⟨min l 1,lt_min hl zero_lt_one,?_⟩
  intro s hs
  have hball : s ∈ Metric.ball (0 : ℝ) l ∩ Ioi 0 := by
    refine ⟨?_, hs.1⟩
    rw [Metric.mem_ball, Real.dist_eq,sub_zero,abs_of_pos hs.1]
    exact hs.2.trans_le (min_le_left _ _)
  have hpow : 1 ≤ s⁻¹ :=
    (one_le_inv₀ hs.1).mpr (hs.2.le.trans (min_le_right _ _))
  have hsquare : s⁻¹ ≤ (s⁻¹)^2 := by
    nlinarith [mul_nonneg (sub_nonneg.mpr hpow) (inv_nonneg.mpr hs.1.le)]
  have he : -((d/s)^2/2) ≤ -b*s⁻¹ := by
    rw [show (d/s)^2 = d^2*(s⁻¹)^2 by rw [div_eq_mul_inv,mul_pow] ]
    have h := mul_le_mul_of_nonneg_left hsquare hb.le
    dsimp [b] at h ⊢
    linarith only [h]
  calc
    _ ≤ (φ 0/s)^n * exp (-b*s⁻¹) :=
      mul_le_mul_of_nonneg_left (Real.exp_le_exp.mpr he)
        (pow_nonneg (div_nonneg (phi_pos 0).le hs.1.le) _)
    _ < J := by
      have hex : (φ 0)^n * ((s⁻¹)^n * exp (-b*s⁻¹)) < J := hl' hball
      simpa only [div_eq_mul_inv,mul_pow,mul_assoc] using hex

/-- Every level strictly above the compatible uniform height has its
joint superlevel in a compact box, including at the normal seam.
The radius lower bound applies to all eligible centres. -/
theorem free_width_superlevel {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {S : Set ℝ} (hS : IsClosed S) {r J : ℝ} (hr : 0 < r)
    (hradius : ∀ m ∈ S, ∃ i, r ≤ |x i - m|) (hJ : (1 / (2 * r)) ^ n < J) :
    ∃ K : Set (ℝ × (ℝ × ℝ)), IsCompact K ∧ K ⊆ Ici 0 ×ˢ (S ×ˢ Ioi 0) ∧
      ∀ p ∈ Ici (0 : ℝ) ×ˢ (S ×ˢ Ioi 0),
        J ≤ likelihood x p.1 p.2.1 p.2.2 → p ∈ K := by
  have hJpos : 0 < J := (show 0 < (1 / (2 * r)) ^ n by positivity).trans hJ
  have hct : ContinuousAt (fun a : ℝ ↦ (1/(2*a))^n) r := by
    exact ((continuousAt_const.div (continuousAt_id.const_mul (2 : ℝ))
      (show 2*r ≠ 0 by positivity)).pow n)
  have hLeft : ∀ᶠ a : ℝ in 𝓝[<] r, a ∈ Iio r := self_mem_nhdsWithin
  have hRight : ∀ᶠ a : ℝ in 𝓝[<] r, 0 < a :=
    eventually_nhdsWithin_of_eventually_nhds (eventually_gt_nhds hr)
  obtain ⟨A,hAr,hAp,hAJ⟩ := (hLeft.and (hRight.and
    (hct.continuousWithinAt.tendsto.eventually (eventually_lt_nhds hJ)))).exists
  have hd : 0 < r-A := sub_pos.mpr hAr
  obtain ⟨l,hl,hl'⟩ := gap_bound_small_scale n hd hJpos
  have hsTop : Tendsto (fun s : ℝ ↦ (φ 0/s)^n) atTop (𝓝 0) := by
    simpa only [div_eq_mul_inv, mul_zero, zero_pow hn.ne'] using
      ((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul (φ 0)).pow n
  obtain ⟨S₀,hS₀⟩ := eventually_atTop.mp (hsTop.eventually (eventually_lt_nhds hJpos))
  let U := max 1 S₀
  have hU : 0 < U := zero_lt_one.trans_le (le_max_left _ _)
  have hExp : Tendsto (fun v : ℝ ↦ exp (-(v^2/2))) atTop (𝓝 0) :=
    tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp
      ((tendsto_pow_atTop (by decide : 2 ≠ 0)).atTop_div_const (by norm_num : (0 : ℝ) < 2)))
  have hExp' : Tendsto (fun v : ℝ ↦ (φ 0/l)^n * exp (-(v^2/2))) atTop (𝓝 0) := by
    simpa only [mul_zero] using hExp.const_mul ((φ 0/l)^n)
  obtain ⟨R,hR⟩ := eventually_atTop.mp (hExp'.eventually (eventually_lt_nhds hJpos))
  let i : Fin n := ⟨0,hn⟩
  let M := |x i|+A+U*(max 1 R)
  refine ⟨Icc 0 A ×ˢ ((Icc (-M) M ∩ S) ×ˢ Icc l U),
    isCompact_Icc.prod ((isCompact_Icc.inter_right hS).prod isCompact_Icc),
    fun _ hp ↦ ⟨hp.1.1,hp.2.1.2,hl.trans_le hp.2.2.1⟩, ?_⟩
  rintro ⟨a,m,s⟩ ⟨ha,hm,hs⟩ h
  change 0 ≤ a at ha
  change 0 < s at hs
  have hA : a ≤ A := by
    by_contra! hgt
    have ha' := hAp.trans hgt
    have hle : (1/(2*a))^n ≤ (1/(2*A))^n :=
      pow_le_pow_left₀ (by positivity) (one_div_le_one_div_of_le (by positivity) (by linarith)) _
    exact ((likelihood_width_bound x ha' hs).trans_lt (hle.trans_lt hAJ)).not_ge h
  have hlow : l ≤ s := by
    by_contra! hlt
    obtain ⟨j,hj⟩ := hradius m hm
    have hgap : a+(r-A) ≤ |x j - m| := by linarith
    exact ((likelihood_gap_bound x ha hs hd.le (i := j) hgap).trans_lt
      (hl' s ⟨hs,hlt⟩)).not_ge h
  have hhigh : s ≤ U := by
    by_contra! hgt
    exact ((likelihood_scale_bound x ha hs).trans_lt
      (hS₀ s ((le_max_right _ _).trans hgt.le))).not_ge h
  have hfar : |m| ≤ M := by
    by_contra! hgt
    let V := max 1 R
    let d := U*V
    have hV : 0 < V := zero_lt_one.trans_le (le_max_left _ _)
    have hd' : 0 < d := mul_pos hU hV
    have htri : |m| ≤ |x i|+|x i - m| := by
      simpa [abs_sub_comm m (x i),add_comm] using abs_sub_le m (x i) 0
    have hgap : a + d ≤ |x i - m| := by dsimp [d,V,M] at *; linarith
    have hr' : R ≤ V := le_max_right _ _
    have hVs : V ≤ d/s := by
      exact (le_div_iff₀ hs).mpr (by
        dsimp [d]
        rw [mul_comm V s]
        exact mul_le_mul_of_nonneg_right hhigh hV.le)
    have hsq : V^2 ≤ (d/s)^2 := by
      nlinarith [mul_nonneg (sub_nonneg.mpr hVs) (show 0 ≤ d/s+V by positivity)]
    have he : exp (-((d/s)^2/2)) ≤ exp (-(V^2/2)) :=
      Real.exp_le_exp.mpr (by linarith)
    have hp : (φ 0/s)^n ≤ (φ 0/l)^n := pow_le_pow_left₀ (by have := phi_pos 0; positivity)
      (div_le_div_of_nonneg_left (phi_pos 0).le hl hlow) _
    have hprod := mul_le_mul hp he (exp_pos _).le
      (pow_nonneg (div_nonneg (phi_pos 0).le hl.le) _)
    exact (((likelihood_gap_bound x ha hs hd'.le (i := i) hgap).trans hprod).trans_lt
      (hR V hr')).not_ge h
  exact ⟨⟨ha,hA⟩,⟨abs_le.mp hfar,hm⟩,⟨hlow,hhigh⟩⟩

/-- A conditional optimum at the dimensionless width, with the
normal seam included and zero width score at every positive candidate. -/
def ConditionalCandidate {n : ℕ} (x : Fin n → ℝ) (S : Set ℝ) (a m s : ℝ) : Prop :=
  IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x ((a/s)*p.2) p.1 p.2) (S ×ˢ Ioi 0) (m,s) ∧
    (a = 0 ∨ HasDerivAt (fun k ↦ sSup
      ((fun p : ℝ × ℝ ↦ likelihood x (k*p.2) p.1 p.2) '' (S ×ˢ Ioi 0))) 0 (a/s))

private theorem free_width_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {S : Set ℝ} (hS : IsClosed S) {c r : ℝ} (hr : 0 < r) (hc : c ∈ S)
    (hcover : ∀ i, |x i - c| ≤ r) (hradius : ∀ m ∈ S, ∃ i, r ≤ |x i - m|) :
    let D := Ici (0 : ℝ) ×ˢ (S ×ˢ Ioi (0 : ℝ))
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    let B := (1 / (2 * r)) ^ n
    ((∃ p ∈ D, IsMaxOn L D p) ↔
      ∃ a ≥ 0, ∃ m ∈ S, ∃ s > 0, ConditionalCandidate x S a m s ∧
        B ≤ likelihood x a m s) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B) := by
  let D := Ici (0 : ℝ) ×ˢ (S ×ˢ Ioi (0 : ℝ))
  let L (p : ℝ × (ℝ × ℝ)) := likelihood x p.1 p.2.1 p.2.2
  let B := (1 / (2 * r)) ^ n
  let path (t : ℝ) := (r+t⁻¹,c,(t⁻¹)^2)
  have hpath : ∀ᶠ t in atTop, path t ∈ D :=
    (eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦
      ⟨(add_pos hr (inv_pos.mpr ht)).le,hc,pow_pos (inv_pos.mpr ht) _⟩
  have hlim : Tendsto (L ∘ path) atTop (𝓝 B) := moving_width_uniform_path x hr hcover
  have hcrit : (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ D, B ≤ L p := by
    constructor
    · rintro ⟨p,hp,hm⟩
      exact ⟨p,hp,le_of_tendsto hlim (hpath.mono fun t ht ↦ hm ht)⟩
    · rintro ⟨p,hp,h⟩
      by_cases hb : ∃ q ∈ D, B < L q
      · obtain ⟨q,hq,hq'⟩ := hb
        obtain ⟨K,hK,hsub,hcut⟩ := free_width_superlevel hn x hS hr hradius hq'
        exact RBProfile.exists_isMaxOn_of_compact_superlevel hK hsub (likelihood_cont x S) hq hcut
      · exact ⟨p,hp,fun z hz ↦ (le_of_not_gt (fun h₁ ↦ hb ⟨z,hz,h₁⟩)).trans h⟩
  refine ⟨?_,LocScaleCommon.nonattainment_iff_of_face hpath hlim hcrit⟩
  constructor
  · rintro ⟨⟨a,m,s⟩,hp,hm⟩
    have ha : 0 ≤ a := hp.1
    have hs : 0 < s := hp.2.2
    have he : a/s*s=a := div_mul_cancel₀ a hs.ne'
    have hheight := le_of_tendsto hlim (hpath.mono fun t ht ↦ hm ht)
    refine ⟨a,ha,m,hp.2.1,s,hs,⟨?_,?_⟩,hheight⟩
    · intro p hP
      change likelihood x (a/s*p.2) p.1 p.2 ≤ likelihood x (a/s*s) m s
      rw [he]
      exact hm (show (a/s*p.2,p.1,p.2) ∈ D from
        ⟨mul_nonneg (div_nonneg ha hs.le) (show 0 < p.2 from hP.2).le,hP⟩)
    · rcases ha.eq_or_lt with hz | ha'
      · exact Or.inl hz.symm
      · right
        have hloc : IsMaxOn (fun k ↦ likelihood x (k*s) m s) (Ioi 0) (a/s) := by
          intro k hk
          change likelihood x (k*s) m s ≤ likelihood x (a/s*s) m s
          rw [he]
          exact hm (show (k*s,m,s) ∈ D from ⟨mul_nonneg (show 0 < k from hk).le hs.le,hp.2⟩)
        have hk : 0 < a/s := div_pos ha' hs
        have hdiff : DifferentiableAt ℝ (fun k ↦ likelihood x (k*s) m s) (a/s) := by
          let f (k : ℝ) := ∏ j, (Φ ((x j-m+k*s)/s)-Φ ((x j-m-k*s)/s))/(2*(k*s))
          have hf : DifferentiableAt ℝ f (a/s) := by
            have hΦ : Differentiable ℝ Φ := fun v ↦ (cdf_hasDerivAt v).differentiableAt
            unfold f
            refine DifferentiableAt.fun_finsetProd (fun j _ ↦ ?_)
            refine DifferentiableAt.div ?_ (by fun_prop) (by positivity)
            exact (hΦ.differentiableAt.comp (a/s) (by fun_prop)).sub
              (hΦ.differentiableAt.comp (a/s) (by fun_prop))
          apply hf.congr_of_eventuallyEq
          filter_upwards [Ioi_mem_nhds hk] with t ht
          simp only [f,likelihood,(mul_pos (show 0 < t from ht) hs).ne',ite_false]
        have hz := (hloc.isLocalMax (Ioi_mem_nhds hk)).deriv_eq_zero
        have hderiv : HasDerivAt (fun k ↦ likelihood x (k*s) m s) 0 (a/s) :=
          hz ▸ hdiff.hasDerivAt
        apply RBProfile.profile_hasDerivAt_zero
          (f := fun k (q : ℝ × ℝ) ↦ likelihood x (k*q.2) q.1 q.2)
          hp.2 (Ioi_mem_nhds hk) ?_ hderiv
        intro t ht q hq
        rw [he]
        exact hm (show (t*q.2,q.1,q.2) ∈ D from
          ⟨mul_nonneg (show 0 < t from ht).le (show 0 < q.2 from hq.2).le,hq⟩)
  · rintro ⟨a,ha,m,hm,s,hs,_,h⟩
    exact hcrit.mpr ⟨(a,m,s),⟨ha,hm,hs⟩,h⟩

private theorem radius_of_extrema {n : ℕ} (x : Fin n → ℝ) {i j : Fin n}
    (hlo : ∀ k, x i ≤ x k) (hhi : ∀ k, x k ≤ x j) :
    (∀ k, |x k-(x i+x j)/2| ≤ (x j-x i)/2) ∧
      ∀ m : ℝ, ∃ k, (x j-x i)/2 ≤ |x k-m| := by
  refine ⟨fun k ↦ abs_le.mpr ⟨by linarith [hlo k],by linarith [hhi k]⟩,?_⟩
  intro m
  by_cases hm : m ≤ (x i+x j)/2
  · exact ⟨j,(show (x j-x i)/2 ≤ x j-m by linarith).trans (le_abs_self _)⟩
  · exact ⟨i,(show (x j-x i)/2 ≤ -(x i-m) by linarith [le_of_not_ge hm]).trans (neg_le_abs _)⟩

/-- Free centre and width compare the conditional stationary heights
with the uniform covering the sample range. Constant data collapse
on the included normal seam. -/
theorem all_free_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    let r := (x j-x i)/2
    let D := Ici (0 : ℝ) ×ˢ Normal.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    let B := (1 / (2 * r)) ^ n
    ((∃ p ∈ D, IsMaxOn L D p) ↔
      0 < r ∧ ∃ a ≥ 0, ∃ m s : ℝ, 0 < s ∧ ConditionalCandidate x univ a m s ∧
        B ≤ likelihood x a m s) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (r = 0 ∧ ¬ BddAbove (L '' D)) ∨
      (0 < r ∧ (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B)) := by
  dsimp only
  have hspan : 0 ≤ (x j-x i)/2 := by linarith [hi j]
  rcases hspan.eq_or_lt with hz | hp
  · have he : x = fun _ ↦ x i := funext fun k ↦ by linarith [hi k,hj k]
    have hu : ¬ BddAbove ((fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2) ''
        (Ici 0 ×ˢ Normal.domain)) := by
      rw [he]
      exact all_free_constant_failure hn (x i)
    have hm : ¬ ∃ p ∈ Ici 0 ×ˢ Normal.domain,
        IsMaxOn (fun q : ℝ × (ℝ × ℝ) ↦ likelihood x q.1 q.2.1 q.2.2)
          (Ici 0 ×ˢ Normal.domain) p := by
      rintro ⟨p,hp,hm⟩
      exact hu ⟨_,(hm.isLUB hp).1⟩
    simp only [← hz,lt_irrefl,false_and,or_false,true_and,hu,iff_false,not_false_eq_true,iff_true]
    exact ⟨hm,hm⟩
  · have hb := radius_of_extrema x hi hj
    have h := free_width_comparison hn x (S := univ) isClosed_univ hp
      (mem_univ ((x i+x j)/2)) hb.1 (fun m _ ↦ hb.2 m)
    simpa only [Normal.domain,hp,hp.ne',false_and,false_or,mem_univ,true_and] using h

/-- At held centre, the covering radius is the largest absolute
residual, including constant samples displaced from the held centre. -/
theorem held_centre_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    (i : Fin n) (hi : ∀ j, |x j - m| ≤ |x i - m|) :
    let r := |x i - m|
    let D := Ici (0 : ℝ) ×ˢ ({m} ×ˢ Ioi (0 : ℝ))
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    let B := (1 / (2 * r)) ^ n
    ((∃ p ∈ D, IsMaxOn L D p) ↔
      0 < r ∧ ∃ a ≥ 0, ∃ s > 0, ConditionalCandidate x {m} a m s ∧
        B ≤ likelihood x a m s) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (r = 0 ∧ ¬ BddAbove (L '' D)) ∨
      (0 < r ∧ (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B)) := by
  dsimp only
  rcases (abs_nonneg (x i-m)).eq_or_lt with hz | hp
  · have he : x = fun _ ↦ m := funext fun j ↦ by
      have h := hi j
      rw [← hz] at h
      exact sub_eq_zero.mp (abs_eq_zero.mp (le_antisymm h (abs_nonneg _)))
    let D := Ici (0 : ℝ) ×ˢ ({m} ×ˢ Ioi (0 : ℝ))
    let L (p : ℝ × (ℝ × ℝ)) := likelihood x p.1 p.2.1 p.2.2
    have hsub : likelihood x 0 m '' Ioi 0 ⊆ L '' D := by
      rintro _ ⟨s,hs,rfl⟩
      exact ⟨(0,m,s),⟨le_rfl,mem_singleton m,hs⟩,rfl⟩
    have hu : ¬ BddAbove (L '' D) := by
      intro hb
      have h := hb.mono hsub
      rw [he] at h
      exact constant_normal_seam hn m h
    have hm : ¬ ∃ p ∈ D, IsMaxOn L D p := by
      rintro ⟨p,hp,hm⟩
      exact hu ⟨_,(hm.isLUB hp).1⟩
    change (((∃ p ∈ D, IsMaxOn L D p) ↔ _) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        (|x i - m| = 0 ∧ ¬ BddAbove (L '' D)) ∨ _))
    simp only [← hz,lt_irrefl,false_and,or_false,true_and,hu,iff_false,not_false_eq_true,iff_true]
    exact ⟨hm,hm⟩
  · have h := free_width_comparison hn x (isClosed_singleton (x := m)) hp (mem_singleton m) hi
      (fun u hu ↦ by
        have he : u = m := hu
        subst u
        exact ⟨i,le_rfl⟩)
    simpa only [mem_singleton_iff, exists_eq_left, hp,hp.ne',false_and,false_or,true_and]
      using h

private theorem gaussian_positive_interval {l u : ℝ} (hlu : l < u) :
    l * (Φ u-Φ l) ≤ φ l-φ u := by
  have hd (t : ℝ) : HasDerivAt (fun v ↦ φ v+l*Φ v) ((l-t)*φ t) t := by
    convert (phi_hasDerivAt t).add ((cdf_hasDerivAt t).const_mul l) using 1
    ring
  have hanti : AntitoneOn (fun v ↦ φ v+l*Φ v) (Ici l) := by
    apply antitoneOn_of_deriv_nonpos (convex_Ici l)
      ((continuous_iff_continuousAt.mpr (fun t ↦ (hd t).continuousAt)).continuousOn)
      (fun t _ ↦ (hd t).differentiableAt.differentiableWithinAt)
    intro t ht
    rw [(hd t).deriv]
    have ht' : l ≤ t := interior_subset ht
    exact mul_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr ht') (phi_pos t).le
  have h := hanti (show l ∈ Ici l from le_rfl) (show u ∈ Ici l from hlu.le) hlu.le
  linarith

private theorem interval_curvature {l u p q : ℝ} (hlu : l < u) (hpq : p ≠ 0 ∨ q ≠ 0) :
    (Φ u-Φ l)*(-u*φ u*q^2+l*φ l*p^2)-(φ u*q-φ l*p)^2 < 0 := by
  let A := φ l
  let B := φ u
  let M := Φ u-Φ l
  have hA : 0 < A := phi_pos l
  have hB : 0 < B := phi_pos u
  have hM : 0 < M := sub_pos.mpr (cdf_strictMono hlu)
  have hP : 0 < A-l*M := by
    by_cases h : 0 ≤ l
    · have hb := gaussian_positive_interval hlu
      change l*M ≤ A-B at hb
      linarith
    · exact sub_pos.mpr ((mul_nonpos_of_nonpos_of_nonneg (le_of_not_ge h) hM.le).trans_lt hA)
  have hT : 0 < u*A-l*B-l*u*M := by
    rcases le_or_gt 0 l with hl | hl
    · have hb := gaussian_positive_interval hlu
      change l*M ≤ A-B at hb
      have hu : 0 ≤ u := hl.trans hlu.le
      have hle := mul_le_mul_of_nonneg_left (show B ≤ A-l*M by linarith) hu
      have hlt := mul_lt_mul_of_pos_right hlu hB
      nlinarith only [hle,hlt]
    · rcases le_or_gt u 0 with hu | hu
      · have h := gaussian_positive_interval (neg_lt_neg hlu)
        have hphi (z : ℝ) : φ (-z) = φ z := by simp [phi_formula]
        rw [hphi,hphi,cdf_reflect,cdf_reflect] at h
        have hh : -u*M ≤ B-A := by dsimp [A,B,M]; linarith only [h]
        have hle := mul_le_mul_of_nonneg_left (show A ≤ B+u*M by linarith)
          (neg_nonneg.mpr hl.le)
        have hlt := mul_lt_mul_of_pos_right (neg_lt_neg hlu) hA
        nlinarith only [hle,hlt]
      · have h₁ := mul_pos hu hA
        have h₂ := mul_nonpos_of_nonpos_of_nonneg hl.le hB.le
        have h₃ := mul_nonpos_of_nonpos_of_nonneg
          (mul_nonpos_of_nonpos_of_nonneg hl.le hu.le) hM.le
        linarith only [h₁,h₂,h₃]
  let P := A*(A-l*M)
  let Q := B*(B+u*M)
  have hP' : 0 < P := mul_pos hA hP
  have hdet : 0 < P*Q-(A*B)^2 := by
    rw [show P*Q-(A*B)^2 = A*B*M*(u*A-l*B-l*u*M) by dsimp [P,Q]; ring]
    positivity
  have hneg : -(P*p-A*B*q)^2-(P*Q-(A*B)^2)*q^2 < 0 := by
    by_cases hq : q=0
    · subst q
      have hp : p ≠ 0 := hpq.resolve_right (not_not_intro rfl)
      simp only [mul_zero,sub_zero,zero_pow (by decide : (2 : ℕ) ≠ 0)]
      exact neg_neg_of_pos (sq_pos_of_ne_zero (mul_ne_zero hP'.ne' hp))
    · linarith only [sq_nonneg (P*p-A*B*q),mul_pos hdet (sq_pos_of_ne_zero hq)]
  have hid : P*(M*(-u*B*q^2+l*A*p^2)-(B*q-A*p)^2) =
      -(P*p-A*B*q)^2-(P*Q-(A*B)^2)*q^2 := by dsimp [P,Q]; ring
  exact neg_of_mul_neg_right (hid ▸ hneg) hP'.le

private theorem log_mass_line {l u p q : ℝ} (hpq : p ≠ 0 ∨ q ≠ 0)
    (horder : ∀ t ∈ Icc (0 : ℝ) 1, l + p * t < u + q * t) :
    StrictConcaveOn ℝ (Icc (0 : ℝ) 1)
      (fun t ↦ log (Φ (u+q*t)-Φ (l+p*t))) := by
  let M (t : ℝ) := Φ (u+q*t)-Φ (l+p*t)
  let N (t : ℝ) := φ (u+q*t)*q-φ (l+p*t)*p
  have hM (t : ℝ) : HasDerivAt M (N t) t := by
    have h₁ := (cdf_hasDerivAt (u+q*t)).comp t (((hasDerivAt_id t).const_mul q).const_add u)
    have h₂ := (cdf_hasDerivAt (l+p*t)).comp t (((hasDerivAt_id t).const_mul p).const_add l)
    convert h₁.sub h₂ using 1
    · rfl
    · simp only [mul_one,N]
  have hN (t : ℝ) : HasDerivAt N
      (-(u+q*t)*φ (u+q*t)*q^2+(l+p*t)*φ (l+p*t)*p^2) t := by
    have h₁ := (phi_hasDerivAt (u+q*t)).comp t (((hasDerivAt_id t).const_mul q).const_add u)
    have h₂ := (phi_hasDerivAt (l+p*t)).comp t (((hasDerivAt_id t).const_mul p).const_add l)
    convert (h₁.mul_const q).sub (h₂.mul_const p) using 1
    · rfl
    · ring
  have hpos (t : ℝ) (ht : t ∈ Icc (0 : ℝ) 1) : 0 < M t :=
    sub_pos.mpr (cdf_strictMono (horder t ht))
  have hd (t : ℝ) (ht : 0 < M t) :
      HasDerivAt (fun t ↦ log (M t)) (N t/M t) t := (hM t).log ht.ne'
  apply strictConcaveOn_of_deriv2_neg (convex_Icc 0 1)
    (fun t ht ↦ (hd t (hpos t ht)).continuousAt.continuousWithinAt)
  intro t ht
  have ht' := hpos t (interior_subset ht)
  have hEq : deriv (fun t ↦ log (M t)) =ᶠ[𝓝 t] (fun t ↦ N t/M t) := by
    have H := (hM t).continuousAt.tendsto.eventually (eventually_gt_nhds ht')
    exact H.mono fun y hy ↦ (hd y hy).deriv
  change deriv (deriv (fun t ↦ log (M t))) t < 0
  rw [hEq.deriv_eq]
  change deriv (N / M) t < 0
  rw [((hN t).div (hM t) ht'.ne').deriv]
  apply div_neg_of_neg_of_pos _ (sq_pos_of_pos ht')
  have h := interval_curvature (horder t (interior_subset ht)) hpq
  dsimp [M,N]
  linarith only [h]

private theorem positive_width_log_formula {n : ℕ} (x : Fin n → ℝ) {a m s : ℝ}
    (ha : 0 < a) (hs : 0 < s) :
    log (likelihood x a m s) =
      ∑ i, (log (Φ ((x i-m+a)/s)-Φ ((x i-m-a)/s))-log (2*a)) := by
  change log (∏ i, pointDensity a (x i-m) s) = _
  rw [log_prod (fun i _ ↦ (density_pos (r := x i-m) ha.le hs).ne')]
  apply Finset.sum_congr rfl
  intro i _
  simp only [pointDensity,ha.ne',ite_false]
  exact log_div
    (sub_pos.mpr (cdf_strictMono (div_lt_div_of_pos_right (by linarith) hs))).ne'
    (by positivity)

/-- At positive held physical width, a midpoint in inverse scale and
scaled location strictly increases the log likelihood above the mean
of the two distinct endpoint fits. -/
theorem held_positive_width_strict_midpoint {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 < a) {S : Set ℝ} (hS : Convex ℝ S)
    {p q : ℝ × ℝ} (hp : p ∈ S ×ˢ Ioi (0 : ℝ)) (hq : q ∈ S ×ˢ Ioi (0 : ℝ))
    (hne : p ≠ q) :
    ∃ z ∈ S ×ˢ Ioi (0 : ℝ),
      (log (likelihood x a p.1 p.2)+log (likelihood x a q.1 q.2))/2 <
        log (likelihood x a z.1 z.2) := by
  have hp₂ : 0 < p.2 := hp.2
  have hq₂ : 0 < q.2 := hq.2
  let b := p.2⁻¹+q.2⁻¹
  have hb : 0 < b := add_pos (inv_pos.mpr hp₂) (inv_pos.mpr hq₂)
  let w := p.2⁻¹/b
  let v := q.2⁻¹/b
  have hw : 0 ≤ w := div_nonneg (inv_nonneg.mpr hp₂.le) hb.le
  have hv : 0 ≤ v := div_nonneg (inv_nonneg.mpr hq₂.le) hb.le
  have hwv : w+v=1 := by dsimp [w,v]; rw [← add_div]; exact div_self hb.ne'
  let z : ℝ × ℝ := (w*p.1+v*q.1,2/b)
  have hz₂ : 0 < z.2 := div_pos (by norm_num) hb
  have hz₁ : z.1 ∈ S := hS hp.1 hq.1 hw hv hwv
  refine ⟨z,⟨hz₁,hz₂⟩,?_⟩
  rw [positive_width_log_formula x ha hp₂,positive_width_log_formula x ha hq₂,
    positive_width_log_formula x ha hz₂,← Finset.sum_add_distrib,Finset.sum_div]
  let : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro i _
  let l₀ := (x i-p.1-a)/p.2
  let u₀ := (x i-p.1+a)/p.2
  let l₁ := (x i-q.1-a)/q.2
  let u₁ := (x i-q.1+a)/q.2
  have h₀ : l₀ < u₀ := div_lt_div_of_pos_right (by linarith) hp₂
  have h₁ : l₁ < u₁ := div_lt_div_of_pos_right (by linarith) hq₂
  have hneq : l₁-l₀ ≠ 0 ∨ u₁-u₀ ≠ 0 := by
    by_contra! h
    have hl := h.1
    have hu := h.2
    dsimp [l₀,l₁,u₀,u₁] at hl hu
    have he₁ := sub_eq_zero.mp hl
    have he₂ := sub_eq_zero.mp hu
    field_simp [hp₂.ne',hq₂.ne'] at he₁ he₂
    have hs : p.2 = q.2 := by nlinarith only [he₁,he₂,ha]
    have hm : p.1 = q.1 := by
      rw [hs] at he₁
      rw [mul_comm q.2 (x i-p.1-a)] at he₁
      have h := mul_right_cancel₀ hq₂.ne' he₁
      linarith
    exact hne (Prod.ext hm hs)
  have horder (t : ℝ) (ht : t ∈ Icc (0 : ℝ) 1) :
      l₀+(l₁-l₀)*t < u₀+(u₁-u₀)*t := by
    have ht₀ : 0 ≤ t := ht.1
    have ht₁ : 0 ≤ 1-t := sub_nonneg.mpr ht.2
    by_cases h : t = 0
    · simpa [h] using h₀
    · have ht' : 0 < t := lt_of_le_of_ne ht₀ (Ne.symm h)
      nlinarith only [mul_nonneg (sub_nonneg.mpr h₀.le) ht₁,
        mul_pos (sub_pos.mpr h₁) ht']
  have hconc := log_mass_line hneq horder
  have hmid := hconc.2 (show (0 : ℝ) ∈ Icc 0 1 by norm_num)
    (show (1 : ℝ) ∈ Icc 0 1 by norm_num) zero_ne_one
    (show (0 : ℝ) < 1/2 by norm_num) (show (0 : ℝ) < 1/2 by norm_num)
    (show (1 : ℝ)/2+1/2=1 by norm_num)
  have hl : (x i-z.1-a)/z.2 = l₀+(l₁-l₀)*(1/2) := by
    dsimp [z,l₀,l₁,w,v,b]
    field_simp [hp₂.ne',hq₂.ne',show p.2+q.2 ≠ 0 from (add_pos hp₂ hq₂).ne']
    ring
  have hu : (x i-z.1+a)/z.2 = u₀+(u₁-u₀)*(1/2) := by
    dsimp [z,u₀,u₁,w,v,b]
    field_simp [hp₂.ne',hq₂.ne',show p.2+q.2 ≠ 0 from (add_pos hp₂ hq₂).ne']
    ring
  rw [hl,hu]
  simp only [smul_eq_mul,mul_zero,mul_one,zero_add,add_zero,add_sub_cancel] at hmid
  change (log (Φ u₀-Φ l₀)-log (2*a)+(log (Φ u₁-Φ l₁)-log (2*a)))/2 < _
  linarith only [hmid]

private theorem held_positive_width_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 < a) {S : Set ℝ} (hS : Convex ℝ S)
    {p q : ℝ × ℝ} (hp : p ∈ S ×ˢ Ioi (0 : ℝ)) (hq : q ∈ S ×ˢ Ioi (0 : ℝ))
    (hpmax : IsMaxOn (fun z : ℝ × ℝ ↦ likelihood x a z.1 z.2) (S ×ˢ Ioi 0) p)
    (hqmax : IsMaxOn (fun z : ℝ × ℝ ↦ likelihood x a z.1 z.2) (S ×ˢ Ioi 0) q) :
    p = q := by
  by_contra hne
  obtain ⟨z,hz,h⟩ := held_positive_width_strict_midpoint hn x ha hS hp hq hne
  have hp' := log_le_log (likelihood_pos x ha.le hz.2) (hpmax hz)
  have hq' := log_le_log (likelihood_pos x ha.le hz.2) (hqmax hz)
  dsimp only at hp' hq'
  linarith

/-- Every attained held-width fit is unique, including the width-zero
normal member and every sample with endpoint ties. -/
theorem held_width_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ} (ha : 0 ≤ a)
    (h : ∃ p ∈ Normal.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x a q.1 q.2) Normal.domain p) :
    ∃! p, p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x a q.1 q.2) Normal.domain p := by
  obtain ⟨p,hp,hm⟩ := h
  rcases ha.eq_or_lt with he | hpos
  · subst a
    exact held_zero_width_unique hn x hp hm
  · exact ⟨p,⟨hp,hm⟩,fun q hq ↦
      held_positive_width_unique hn x hpos convex_univ hq.1 hp hq.2 hm⟩

/-- Every attained scale fit at held width and centre is unique. -/
theorem held_width_centre_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 ≤ a) (m : ℝ)
    (h : ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (likelihood x a m) (Ioi 0) s) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (likelihood x a m) (Ioi 0) s := by
  rcases ha.eq_or_lt with he | hpos
  · subst a
    exact held_zero_width_centre_unique hn x m h
  · obtain ⟨s,hs,hm⟩ := h
    refine ⟨s,⟨hs,hm⟩,?_⟩
    rintro t ⟨ht,htm⟩
    have heq (v : ℝ) (hv : IsMaxOn (likelihood x a m) (Ioi 0) v) :
        IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x a q.1 q.2) ({m} ×ˢ Ioi 0) (m,v) := by
      rintro ⟨u,k⟩ ⟨hu,hk⟩
      have h : u = m := hu
      subst u
      exact hv hk
    exact congrArg (fun q : ℝ × ℝ ↦ q.2)
      (held_positive_width_unique hn x hpos (convex_singleton m)
        (show (m,t) ∈ {m} ×ˢ Ioi 0 from ⟨mem_singleton _,ht⟩)
        (show (m,s) ∈ {m} ×ˢ Ioi 0 from ⟨mem_singleton _,hs⟩)
        (heq t htm) (heq s hm))

end UnivariateML.RBBhatt

end
