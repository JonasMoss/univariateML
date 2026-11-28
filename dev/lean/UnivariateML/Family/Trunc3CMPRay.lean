module

public import UnivariateML.Family.TruncCount
public import UnivariateML.Family.CMP
public import Mathlib.Analysis.Calculus.Deriv.MeanValue

/-! Held-rate CMP on the positive ray.

Deviations: attainment and uniqueness are stated through the centered
normalizer. The admitted geometric boundary is decided by its centered
factorial moment. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc3CMPRay

noncomputable section

def domain (η : ℝ) : Set ℝ := if η < 0 then Iic 0 else Iio 0

def term (η m t : ℝ) (k : ℕ) : ℝ :=
  exp (η * (k+1 : ℕ) + t * (CMP.logFactorial (k+1)-m))

def centered (η m t : ℝ) : ℝ := ∑' k, term η m t k

private theorem summable_term {η m t : ℝ} (ht : t ∈ domain η) :
    Summable (term η m t) := by
  have H : 0 < -t ∨ -t = 0 ∧ η < 0 := by
    unfold domain at ht
    split_ifs at ht with h
    · obtain ht | ht := lt_or_eq_of_le ht
      · exact Or.inl (neg_pos.mpr ht)
      · exact Or.inr ⟨by simp [ht], h⟩
    · exact Or.inl (neg_pos.mpr ht)
  have hs := ((summable_nat_add_iff 1).mpr (CMP.summable_weight H)).mul_right (exp (-(t*m)))
  convert hs using 1
  ext k
  rw [term, CMP.weight, ← exp_add]
  congr 1
  ring

private theorem strict_convex (η m : ℝ) : StrictConvexOn ℝ (domain η) (centered η m) := by
  have hD : Convex ℝ (domain η) := by
    unfold domain
    split_ifs <;> first | exact convex_Iic _ | exact convex_Iio _
  refine ⟨hD, fun x hx y hy hxy a b ha hb hab ↦ ?_⟩
  have hj : ∃ j : ℕ, CMP.logFactorial (j+1) ≠ m := by
    by_cases hm : m = 0
    · refine ⟨1, ?_⟩
      rw [hm]
      exact (show 0 < CMP.logFactorial 2 by
        apply log_pos
        norm_num [Nat.factorial]).ne'
    · exact ⟨0, by simpa [CMP.logFactorial] using Ne.symm hm⟩
  obtain ⟨j,hj⟩ := hj
  have hs (k : ℕ) :
      η * (k+1 : ℕ) + (a • x+b • y)*(CMP.logFactorial (k+1)-m) =
        a*(η*(k+1 : ℕ)+x*(CMP.logFactorial (k+1)-m)) +
        b*(η*(k+1 : ℕ)+y*(CMP.logFactorial (k+1)-m)) := by
    simp only [smul_eq_mul]
    linear_combination η * ((k+1 : ℕ) : ℝ) * (-hab)
  have hbnd (k : ℕ) : term η m (a • x+b • y) k ≤
      a*term η m x k + b*term η m y k := by
    unfold term
    rw [hs]
    exact convexOn_exp.2 (mem_univ _) (mem_univ _) ha.le hb.le hab
  have hlt : term η m (a • x+b • y) j < a*term η m x j + b*term η m y j := by
    unfold term
    rw [hs]
    refine strictConvexOn_exp.2 (mem_univ _) (mem_univ _) (fun H ↦ hxy ?_) ha hb hab
    exact (mul_left_inj' (sub_ne_zero.mpr hj)).mp (add_left_cancel H)
  have hx' := (summable_term (m := m) hx).mul_left a
  have hy' := (summable_term (m := m) hy).mul_left b
  have H := Summable.tsum_lt_tsum hbnd hlt
    (summable_term (hD hx hy ha.le hb.le hab)) (hx'.add hy')
  rw [hx'.tsum_add hy', tsum_mul_left, tsum_mul_left] at H
  exact H

private theorem continuous_interval {η m l u : ℝ}
    (hl : l ∈ domain η) (hu : u ∈ domain η) :
    ContinuousOn (centered η m) (Icc l u) := by
  intro t ht
  have hlim (k : ℕ) : Tendsto (fun v ↦ term η m v k) (𝓝[Icc l u] t)
      (𝓝 (term η m t k)) :=
    (show Continuous (fun v ↦ term η m v k) by unfold term; fun_prop).continuousAt.continuousWithinAt
  apply tendsto_tsum_of_dominated_convergence
    ((summable_term (m := m) hl).add (summable_term (m := m) hu)) hlim
  filter_upwards [self_mem_nhdsWithin] with v hv k
  rw [Real.norm_of_nonneg (show 0 ≤ term η m v k from (exp_pos _).le)]
  change term η m v k ≤ term η m l k + term η m u k
  by_cases h : 0 ≤ CMP.logFactorial (k+1)-m
  · refine le_trans ?_ (le_add_of_nonneg_left (exp_pos _).le)
    exact exp_le_exp.mpr (add_le_add_right (mul_le_mul_of_nonneg_right hv.2 h) _)
  · refine le_trans ?_ (le_add_of_nonneg_right (exp_pos _).le)
    exact exp_le_exp.mpr (add_le_add_right (mul_le_mul_of_nonpos_right hv.1 (le_of_not_ge h)) _)

private theorem excluded_zero_top {η : ℝ} (hη : 0 ≤ η) (m : ℝ) :
    Tendsto (centered η m) (𝓝[<] 0) atTop := by
  apply tendsto_atTop.mpr
  intro B
  obtain ⟨n, hn⟩ := exists_nat_gt B
  have H : B < ∑ k ∈ Finset.range n, term η m 0 k := by
    apply hn.trans_le
    simpa [term] using Finset.sum_le_sum (s := Finset.range n) (fun k _ ↦
      one_le_exp (show 0 ≤ η*(k+1 : ℕ) by positivity))
  have hc : Continuous (fun u ↦ ∑ k ∈ Finset.range n, term η m u k) := by
    unfold term
    fun_prop
  have hNear : ∀ᶠ u : ℝ in 𝓝[<] 0, B < ∑ k ∈ Finset.range n, term η m u k := by
    apply (hc.continuousAt.tendsto.mono_left nhdsWithin_le_nhds).eventually
    exact Ioi_mem_nhds H
  have hneg : ∀ᶠ u : ℝ in 𝓝[<] 0, u < 0 := self_mem_nhdsWithin
  filter_upwards [hneg,hNear] with u hu hlt
  have hD : u ∈ domain η := by simpa [domain, not_lt.mpr hη]
  exact hlt.le.trans ((summable_term hD).sum_le_tsum _ fun _ _ ↦ (exp_pos _).le)

private theorem right_barrier {η m t : ℝ} (hη : 0 ≤ η) (ht : t < 0) :
    ∃ u < 0, t < u ∧ centered η m t < centered η m u := by
  have hneg : ∀ᶠ u : ℝ in 𝓝[<] 0, u < 0 := self_mem_nhdsWithin
  have htNear : ∀ᶠ u : ℝ in 𝓝[<] 0, t < u := mem_nhdsWithin_of_mem_nhds (Ioi_mem_nhds ht)
  have hBig := (excluded_zero_top hη m).eventually (eventually_gt_atTop (centered η m t))
  exact (hneg.and (htNear.and hBig)).exists

/-- At a held rate at least one, each positive conditional cell loses
all mass as dispersion tends to zero. -/
theorem held_rate_excluded_geometric {η : ℝ} (hη : 0 ≤ η) (j : ℕ) :
    Tendsto (fun t ↦ term η 0 t j / centered η 0 t) (𝓝[<] 0) (𝓝 0) := by
  have h : Continuous (fun t ↦ term η 0 t j) := by unfold term; fun_prop
  have H : Tendsto (fun t ↦ term η 0 t j) (𝓝[<] 0) (𝓝 (term η 0 0 j)) :=
    h.continuousAt.tendsto.mono_left nhdsWithin_le_nhds
  simpa only [Pi.div_def] using H.div_atTop (excluded_zero_top hη 0)

/-- At a held rate below one the positive conditional masses tend to
the normalized shifted geometric carrier as dispersion tends to zero. -/
theorem held_rate_admitted_geometric {η : ℝ} (hη : η < 0) (j : ℕ) :
    Tendsto (fun t ↦ term η 0 t j / centered η 0 t) (𝓝[<] 0)
      (𝓝 (term η 0 0 j / centered η 0 0)) := by
  have h0 : 0 ∈ domain η := by simp [domain,hη]
  have hsum := summable_term (m := 0) h0
  have h (k : ℕ) : Tendsto (fun t ↦ term η 0 t k) (𝓝[<] 0) (𝓝 (term η 0 0 k)) :=
    (show Continuous (fun t ↦ term η 0 t k) by unfold term; fun_prop).continuousAt.tendsto.mono_left
      nhdsWithin_le_nhds
  have hs : Tendsto (centered η 0) (𝓝[<] 0) (𝓝 (centered η 0 0)) := by
    apply tendsto_tsum_of_dominated_convergence hsum h
    filter_upwards [show ∀ᶠ t : ℝ in 𝓝[<] 0, t < 0 from self_mem_nhdsWithin] with t ht k
    have hk : 0 ≤ CMP.logFactorial (k+1) := by
      apply log_nonneg
      exact_mod_cast Nat.factorial_pos (k+1)
    unfold term
    rw [Real.norm_of_nonneg (exp_pos _).le, exp_le_exp]
    simpa using mul_nonpos_of_nonpos_of_nonneg ht.le hk
  have hp : 0 < centered η 0 0 :=
    (exp_pos _).trans_le (hsum.le_tsum 0 (fun _ _ ↦ (exp_pos _).le))
  simpa only [Pi.div_def] using (h j).div hs hp.ne'

/-- The held-rate positive ray has exactly one optimum for a strictly
positive mean log factorial and none for an all-ones sample. -/
theorem held_rate_positive_fit (η : ℝ) {m : ℝ} (hm : 0 ≤ m) :
    (∃! t, t ∈ domain η ∧ IsMinOn (centered η m) (domain η) t) ↔ 0 < m := by
  have htail : ∀ᶠ t : ℝ in atBot, t ∈ domain η :=
    (eventually_lt_atBot 0).mono fun t ht ↦ by
      unfold domain
      split_ifs
      · exact ht.le
      · exact ht
  constructor
  · rintro ⟨t,⟨ht,H⟩,_⟩
    by_contra! h
    obtain rfl : m = 0 := le_antisymm h hm
    obtain ⟨v,hv,hvt⟩ := (htail.and (eventually_lt_atBot t)).exists
    have hmono (k : ℕ) : 0 ≤ CMP.logFactorial (k+1) := by
      apply log_nonneg
      exact_mod_cast Nat.factorial_pos (k+1)
    have hlt : 0 < CMP.logFactorial 2 := by
      apply log_pos
      norm_num [Nat.factorial]
    have hle (k : ℕ) : term η 0 v k ≤ term η 0 t k := by
      unfold term
      apply exp_le_exp.mpr
      simpa only [sub_zero] using
        (add_le_add_right (mul_le_mul_of_nonneg_right hvt.le (hmono k)) (η*(k+1 : ℕ)))
    have hs : term η 0 v 1 < term η 0 t 1 := by
      unfold term
      apply exp_lt_exp.mpr
      simpa only [sub_zero] using
        (add_lt_add_right (mul_lt_mul_of_pos_right hvt hlt) (η * (2 : ℕ)))
    exact (Summable.tsum_lt_tsum hle hs (summable_term hv) (summable_term ht)).not_ge (H hv)
  · intro hm'
    let F := centered η m
    have hconv := strict_convex η m
    have hb (t : ℝ) (ht : t ∈ domain η) :
        ∃ u ∈ domain η, t ≤ u ∧ ((∀ v ∈ domain η, v ≤ u) ∨ F t < F u) := by
      by_cases hη : η < 0
      · refine ⟨0, by simp [domain,hη], by simpa [domain,hη] using ht, Or.inl ?_⟩
        exact fun v hv ↦ by simpa [domain,hη] using hv
      · obtain ⟨u,hu,htu,hr⟩ := right_barrier (not_lt.mp hη) (by simpa [domain,hη] using ht)
        exact ⟨u,by simpa [domain,hη] using hu,htu.le,Or.inr hr⟩
    obtain ⟨t,ht⟩ := htail.exists
    obtain ⟨u,hu,htu,hupper⟩ := hb t ht
    have hlimit : Tendsto (fun v ↦ term η m v 0) atBot atTop := by
      have H : Tendsto (fun v : ℝ ↦ v * (-m)) atBot atTop :=
        tendsto_id.atBot_mul_const_of_neg (neg_lt_zero.mpr hm')
      have H' := tendsto_exp_atTop.comp (tendsto_atTop_add_const_left atBot η H)
      simpa [term, CMP.logFactorial, Function.comp_def] using H'
    obtain ⟨l,hl,hlt,hlow⟩ := (htail.and ((eventually_lt_atBot t).and
      (hlimit.eventually (eventually_gt_atTop (F t))))).exists
    have hleft : F t < F l := hlow.trans_le
      ((summable_term hl).le_tsum 0 fun _ _ ↦ (exp_pos _).le)
    have hsub : Icc l u ⊆ domain η := by
      rw [← segment_eq_Icc (hlt.le.trans htu)]
      exact hconv.1.segment_subset hl hu
    obtain ⟨s,hs,hmin⟩ := isCompact_Icc.exists_isMinOn ⟨t,hlt.le,htu⟩
      (continuous_interval (m := m) hl hu)
    have hst : F s ≤ F t := hmin ⟨hlt.le,htu⟩
    have H : IsMinOn F (domain η) s := by
      intro v hv
      change F s ≤ F v
      by_cases hvi : v ∈ Icc l u
      · exact hmin hvi
      · by_contra! hvs
        have hh : max (F t) (F v) ≤ F t := max_le le_rfl (hvs.le.trans hst)
        by_cases hvl : v < l
        · have hseg : l ∈ segment ℝ t v := by
            rw [segment_symm, segment_eq_Icc (hvl.le.trans hlt.le)]
            exact ⟨hvl.le,hlt.le⟩
          exact hleft.not_ge ((hconv.convexOn.le_max_of_mem_segment ht hv hseg).trans hh)
        · have huv : u < v := lt_of_not_ge (fun h ↦ hvi ⟨le_of_not_gt hvl,h⟩)
          rcases hupper with hupper | hupper
          · exact huv.not_ge (hupper v hv)
          · have hseg : u ∈ segment ℝ t v := by
              rw [segment_eq_Icc (htu.trans huv.le)]
              exact ⟨htu,huv.le⟩
            exact hupper.not_ge ((hconv.convexOn.le_max_of_mem_segment ht hv hseg).trans hh)
    exact ⟨s,⟨hsub hs,H⟩,fun v hv ↦ hconv.eq_of_isMinOn hv.2 H hv.1 (hsub hs)⟩

private theorem log_factorial_bound (k : ℕ) :
    CMP.logFactorial (k+1) ≤ ((k+1 : ℕ) : ℝ)^2 := by
  have hk : (0 : ℝ) < (k+1 : ℕ) := by positivity
  have hf : (((k+1).factorial : ℕ) : ℝ) ≤ ((k+1 : ℕ) : ℝ)^(k+1) := by
    exact_mod_cast Nat.factorial_le_pow (k+1)
  unfold CMP.logFactorial
  refine (log_le_log (by exact_mod_cast Nat.factorial_pos (k+1)) hf).trans ?_
  rw [log_pow, pow_two]
  exact mul_le_mul_of_nonneg_left ((log_le_sub_one_of_pos hk).trans (sub_le_self _ zero_le_one))
    hk.le

private theorem geometric_slope {η m : ℝ} (hη : η < 0) (hm : 0 ≤ m) :
    Tendsto (fun t ↦ (centered η m t - centered η m 0) / t) (𝓝[<] 0)
      (𝓝 (∑' k : ℕ, exp (η*(k+1 : ℕ)) * (CMP.logFactorial (k+1)-m))) := by
  let d := fun k : ℕ ↦ CMP.logFactorial (k+1)-m
  let derivTerm := fun t k ↦ term η m t k * d k
  let q := fun t k ↦ (term η m t k - term η m 0 k) / t
  have hDer (k : ℕ) (t : ℝ) : HasDerivAt (fun v ↦ term η m v k) (derivTerm t k) t := by
    dsimp [term, derivTerm]
    exact (((hasDerivAt_id t).mul_const (d k)).const_add (η*(k+1 : ℕ))).exp.congr_deriv
      (by simp [d])
  have hterm (k : ℕ) : Tendsto (q · k) (𝓝[<] 0) (𝓝 (exp (η*(k+1 : ℕ)) * d k)) := by
    simpa [q, derivTerm, term, div_eq_mul_inv, mul_comm] using (hDer k 0).tendsto_slope_zero_left
  have he : ‖exp η‖ < 1 := by rw [Real.norm_of_nonneg (exp_pos _).le]; simpa
  have hs0 := summable_pow_mul_geometric_of_norm_lt_one 0 he
  have hs2 := summable_pow_mul_geometric_of_norm_lt_one 2 he
  have hsum : Summable (fun k : ℕ ↦ exp m * (exp (η*(k+1 : ℕ)) * (((k+1 : ℕ) : ℝ)^2+m))) := by
    have hs := (summable_nat_add_iff 1).mpr (hs2.add (hs0.mul_left m))
    convert hs.mul_left (exp m) using 1
    ext k
    rw [mul_comm η _, exp_nat_mul]
    simp only [pow_zero, one_mul]
    ring
  have hlt : ∀ᶠ t : ℝ in 𝓝[<] 0, t < 0 := self_mem_nhdsWithin
  have hgt : ∀ᶠ t : ℝ in 𝓝[<] 0, -1 < t :=
    mem_nhdsWithin_of_mem_nhds (Ioi_mem_nhds (by norm_num))
  have hbound : ∀ᶠ t in 𝓝[<] (0 : ℝ), ∀ k,
      ‖q t k‖ ≤ exp m * (exp (η*(k+1 : ℕ)) * (((k+1 : ℕ) : ℝ)^2+m)) := by
    filter_upwards [hlt,hgt] with t ht ht' k
    obtain ⟨c,hc,H⟩ := exists_hasDerivAt_eq_slope (fun v ↦ term η m v k)
      (derivTerm · k) ht ((show Differentiable ℝ (fun v ↦ term η m v k) from
        fun v ↦ (hDer k v).differentiableAt).continuous.continuousOn)
      (fun v _ ↦ hDer k v)
    have hq : q t k = derivTerm c k := by
      rw [H]
      dsimp [q]
      rw [zero_sub, div_neg, ← neg_div, neg_sub]
    have hfact : 0 ≤ CMP.logFactorial (k+1) := by
      apply log_nonneg
      exact_mod_cast Nat.factorial_pos (k+1)
    have habs : |d k| ≤ ((k+1 : ℕ) : ℝ)^2+m := by
      rw [abs_le]
      dsimp [d]
      constructor <;> nlinarith [log_factorial_bound k]
    have hc0 := hc.2
    have hc1 : -1 ≤ c := ht'.le.trans hc.1.le
    have hcd : c * d k ≤ m := by
      dsimp [d]
      have h₁ := mul_nonpos_of_nonpos_of_nonneg hc0.le hfact
      have h₂ := mul_nonneg (show 0 ≤ c+1 by linarith) hm
      nlinarith
    rw [hq]
    change ‖exp (η*(k+1 : ℕ) + c*d k) * d k‖ ≤ _
    rw [Real.norm_eq_abs, abs_mul, abs_of_nonneg (exp_pos _).le, exp_add]
    calc
      _ ≤ (exp (η*(k+1 : ℕ)) * exp m) * (((k+1 : ℕ) : ℝ)^2+m) :=
        mul_le_mul (mul_le_mul_of_nonneg_left (exp_le_exp.mpr hcd) (exp_pos _).le) habs
          (abs_nonneg _) (by positivity)
      _ = _ := by ring
  apply (tendsto_tsum_of_dominated_convergence hsum hterm hbound).congr'
  filter_upwards [hlt] with t ht
  have htD : t ∈ domain η := by simpa [domain,hη] using ht.le
  have hzero : 0 ∈ domain η := by simp [domain,hη]
  simp only [q, div_eq_mul_inv, tsum_mul_right, (summable_term (m := m) htD).tsum_sub
    (summable_term (m := m) hzero), centered]

/-- At a held rate below one, the admitted geometric boundary wins
exactly when its centered factorial moment is nonpositive. -/
theorem held_rate_geometric_edge {η m : ℝ} (hη : η < 0) (hm : 0 ≤ m) :
    IsMinOn (centered η m) (domain η) 0 ↔
      (∑' k : ℕ, exp (η*(k+1 : ℕ)) * (CMP.logFactorial (k+1)-m)) ≤ 0 := by
  let M := ∑' k : ℕ, exp (η*(k+1 : ℕ)) * (CMP.logFactorial (k+1)-m)
  have H := geometric_slope hη hm
  constructor
  · intro hmin
    apply le_of_tendsto H
    filter_upwards [show ∀ᶠ t : ℝ in 𝓝[<] 0, t < 0 from self_mem_nhdsWithin] with t ht
    exact div_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr
      (hmin (by simpa [domain,hη] using ht.le : t ∈ domain η))) ht.le
  · intro hM t ht
    have ht' : t ≤ 0 := by simpa [domain,hη] using ht
    rcases ht'.eq_or_lt with rfl | hlt
    · exact le_rfl
    · have hslope : (centered η m t - centered η m 0)/t ≤ M := by
        apply ge_of_tendsto H
        have he : ∀ᶠ u : ℝ in 𝓝[<] 0, t < u :=
          mem_nhdsWithin_of_mem_nhds (Ioi_mem_nhds hlt)
        filter_upwards [he, show ∀ᶠ u : ℝ in 𝓝[<] 0, u < 0 from self_mem_nhdsWithin]
          with u htu hu
        have huD : u ∈ domain η := by simpa [domain,hη] using hu.le
        have hzero : 0 ∈ domain η := by simp [domain,hη]
        have HH := (strict_convex η m).convexOn.secant_mono hzero ht huD hlt.ne hu.ne htu.le
        simpa only [sub_zero] using HH
      have he := hslope.trans hM
      exact sub_nonneg.mp (by simpa only [zero_mul] using (div_le_iff_of_neg hlt).mp he)

end

end UnivariateML.Trunc3CMPRay

end
