module

public import UnivariateML.Family.AllFreeBetaPrime
public import UnivariateML.Generic.LocationScale

set_option linter.style.header false

/-!
The log-data likelihood with both beta shapes held.

## Deviations

The single-held-shape comparisons cover the constant branches. The signed nonconstant tail
tests and the all-free compact ratio comparison are outside this module.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.Shape2BetaPrime

noncomputable section

def logisticLoss (p q z : ℝ) : ℝ := (p + q) * log (1 + exp z) - p * z

private theorem softplus_strictConvex :
    StrictConvexOn ℝ univ (fun z ↦ log (1 + exp z)) := by
  let f := fun z : ℝ ↦ log (1 + exp z)
  let g := fun z : ℝ ↦ exp z / (1 + exp z)
  have hd (z : ℝ) : HasDerivAt f (g z) z :=
    (((hasDerivAt_exp z).const_add 1).log (by positivity))
  have hg (z : ℝ) : HasDerivAt g (exp z / (1 + exp z) ^ 2) z := by
    convert (hasDerivAt_exp z).div ((hasDerivAt_exp z).const_add 1) (by positivity) using 1
    ring
  apply strictConvexOn_of_deriv2_pos' convex_univ
    (fun z _ ↦ (hd z).continuousAt.continuousWithinAt)
  intro z _
  change 0 < deriv (deriv f) z
  rw [show deriv f = g from funext fun z ↦ (hd z).deriv, (hg z).deriv]
  positivity

/-- In log-data coordinates the fixed-beta-shape loss is strictly convex
and grows at least linearly in both directions. -/
theorem beta_logistic_loss_geometry {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    StrictConvexOn ℝ univ (logisticLoss p q) ∧
      ∀ z, min p q * |z| ≤ logisticLoss p q z := by
  have hs : 0 < p + q := add_pos hp hq
  constructor
  · refine ⟨convex_univ, ?_⟩
    intro x _ y _ hxy a b ha hb hab
    have h := softplus_strictConvex.2 (mem_univ x) (mem_univ y) hxy ha hb hab
    simp only [smul_eq_mul] at h ⊢
    unfold logisticLoss
    nlinarith only [mul_lt_mul_of_pos_left h hs]
  · intro z
    have hlog : 0 ≤ log (1 + exp z) := log_nonneg (by linarith [exp_pos z])
    have hzlog : z ≤ log (1 + exp z) := by
      calc
        z = log (exp z) := (log_exp _).symm
        _ ≤ _ := log_le_log (exp_pos _) (by linarith)
    rcases le_total z 0 with hz | hz
    · rw [abs_of_nonpos hz]
      unfold logisticLoss
      nlinarith only [mul_le_mul_of_nonneg_right (min_le_left p q) (neg_nonneg.mpr hz),
        mul_nonneg hs.le hlog]
    · rw [abs_of_nonneg hz]
      unfold logisticLoss
      nlinarith only [mul_le_mul_of_nonneg_right (min_le_right p q) hz,
        mul_le_mul_of_nonneg_left hzlog hs.le]

variable {n : ℕ}

/-- Any nonempty positive sample has a unique finite beta-prime scale
when both beta shapes are positive and held. -/
theorem held_shapes_scale_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ BetaPrime.logLik x t (p, q)) (Ioi 0) s := by
  let y := fun i ↦ log (x i)
  let g := logisticLoss p q
  let L := fun m ↦ -∑ i, g (y i - m)
  obtain ⟨hconv, hbound⟩ := beta_logistic_loss_geometry hp hq
  let i : Fin n := ⟨0, hn⟩
  have hL : StrictConcaveOn ℝ univ L := by
    refine ⟨convex_univ, ?_⟩
    intro a _ b _ hab u v hu hv huv
    have h (j : Fin n) : g (y j - (u * a + v * b)) <
        u * g (y j - a) + v * g (y j - b) := by
      rw [show y j - (u * a + v * b) = u * (y j - a) + v * (y j - b) by
        linear_combination -(y j) * huv]
      exact hconv.2 (mem_univ _) (mem_univ _)
        (fun h ↦ hab (by linarith only [h])) hu hv huv
    have hsum := Finset.sum_lt_sum_of_nonempty (s := Finset.univ)
      ⟨i, Finset.mem_univ i⟩ (fun j _ ↦ h j)
    simp only [smul_eq_mul, L, Finset.sum_add_distrib, ← Finset.mul_sum] at hsum ⊢
    linarith only [hsum]
  let d := min p q
  have hd : 0 < d := lt_min hp hq
  have hb (m : ℝ) : L m ≤ -(d * |y i - m|) := by
    have h (j : Fin n) : 0 ≤ g (y j - m) :=
      (mul_nonneg hd.le (abs_nonneg _)).trans (hbound _)
    have hs := Finset.single_le_sum (s := Finset.univ) (fun j _ ↦ h j) (Finset.mem_univ i)
    exact neg_le_neg ((hbound _).trans hs)
  let R := -L (y i) / d
  have hR : 0 ≤ R := div_nonneg (by simpa only [sub_self, abs_zero, mul_zero, neg_zero,
    neg_nonneg] using (hb (y i))) hd.le
  have hi : y i ∈ Icc (y i - R) (y i + R) := ⟨by linarith, by linarith⟩
  obtain ⟨m, hm, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨y i, hi⟩
    ((hL.concaveOn.continuousOn isOpen_univ).mono (subset_univ _))
  have hM : IsMaxOn L univ m := by
    intro t _
    by_cases ht : L (y i) ≤ L t
    · have h' : |y i - t| ≤ R := (le_div_iff₀ hd).2 (by linarith only [ht, hb t])
      exact hmax ⟨by linarith [(abs_le.mp h').2], by linarith [(abs_le.mp h').1]⟩
    · exact (le_of_not_ge ht).trans (hmax hi)
  have hbij : BijOn log (Ioi (0 : ℝ)) univ :=
    ⟨fun _ _ ↦ mem_univ _, log_injOn_pos, fun t _ ↦ ⟨exp t, exp_pos _, log_exp _⟩⟩
  apply (Transfer.existsUnique_max_iff (f := fun s ↦ BetaPrime.logLik x s (p, q))
    (g := L) (c := -(∑ i, (y i + Beta.logBeta (p, q)))) hbij ?_).2
  · exact ⟨m, ⟨mem_univ _, hM⟩, fun t ht ↦
      hL.eq_of_isMaxOn ht.2 hM ht.1 (mem_univ _)⟩
  · intro s hs
    have he (j : Fin n) : exp (y j - log s) = x j / s := by
      rw [exp_sub, exp_log (hx j), exp_log hs]
    simp only [L, g, logisticLoss, he]
    simp only [BetaPrime.logLik, log_div (hx _).ne' (show 0 < s from hs).ne', y,
      Finset.sum_sub_distrib, Finset.sum_add_distrib, ← Finset.mul_sum,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    ring


private theorem gamma_chord_bound {s : ℝ} (hs : 0 < s) :
    deriv (log ∘ Real.Gamma) s ≤ log s := by
  have hd := (Real.hasDerivAt_Gamma_of_pos hs).log (Gamma_pos_of_pos hs).ne'
  have h := convexOn_log_Gamma.deriv_le_slope (show s ∈ Ioi (0 : ℝ) from hs)
    (show s + 1 ∈ Ioi (0 : ℝ) from add_pos hs one_pos) (lt_add_one s) hd.differentiableAt
  have he : log (Real.Gamma (s + 1)) = log s + log (Real.Gamma s) := by
    rw [Real.Gamma_add_one hs.ne', log_mul hs.ne' (Gamma_pos_of_pos hs).ne']
  simpa only [slope_def_field, Function.comp_apply, he, add_sub_cancel_right,
    add_sub_cancel_left, div_one] using h

private theorem gamma_log_deriv_lt {s : ℝ} (hs : 0 < s) :
    deriv (log ∘ Real.Gamma) s < log s := by
  let F := log ∘ Real.Gamma
  have hd (t : ℝ) (ht : 0 < t) : HasDerivAt F (deriv F t) t :=
    ((Real.hasDerivAt_Gamma_of_pos ht).log (Gamma_pos_of_pos ht).ne').differentiableAt.hasDerivAt
  have h1 := (hd (s + 1) (by positivity)).comp s ((hasDerivAt_id s).add_const 1)
  have h2 := (hasDerivAt_log hs.ne').add (hd s hs)
  have he : (fun t ↦ log t + F t) =ᶠ[nhds s] F ∘ fun t ↦ t + 1 := by
    filter_upwards [Ioi_mem_nhds hs] with t (ht : 0 < t)
    simp only [F, Function.comp_apply]
    rw [Real.Gamma_add_one ht.ne', log_mul ht.ne' (Gamma_pos_of_pos ht).ne']
  have hstep : deriv F (s + 1) = s⁻¹ + deriv F s := by
    simpa using h1.unique (h2.congr_of_eventuallyEq he.symm)
  have hlog := log_lt_sub_one_of_pos (div_pos (by positivity : 0 < s + 1) hs)
    (fun h ↦ by have := (div_eq_one_iff_eq hs.ne').1 h; linarith)
  rw [log_div (by positivity : s + 1 ≠ 0) hs.ne', add_div, div_self hs.ne', one_div] at hlog
  linarith only [hlog, hstep, gamma_chord_bound (show 0 < s + 1 by positivity)]

private theorem gamma_remainder_anti :
    StrictAntiOn (fun s : ℝ ↦ log (Real.Gamma s) - s * log s + s) (Ioi 0) := by
  have hh (s : ℝ) (hs : 0 < s) : HasDerivAt
      (fun t : ℝ ↦ log (Real.Gamma t) - t * log t + t)
      (deriv (log ∘ Real.Gamma) s - log s) s := by
    have hd := ((Real.hasDerivAt_Gamma_of_pos hs).log (Gamma_pos_of_pos hs).ne')
    have hd' : HasDerivAt (log ∘ Real.Gamma) (deriv (log ∘ Real.Gamma) s) s :=
      hd.differentiableAt.hasDerivAt
    have h := (hd'.sub ((hasDerivAt_id s).mul (hasDerivAt_log hs.ne'))).add (hasDerivAt_id s)
    convert h using 1
    · rfl
    · simp only [id_eq, one_mul, mul_inv_cancel₀ hs.ne']
      ring
  apply strictAntiOn_of_deriv_neg (convex_Ioi _)
    (fun s hs ↦ (hh s hs).continuousAt.continuousWithinAt)
  rw [interior_Ioi]
  intro s hs
  rw [(hh s hs).deriv]
  exact sub_neg.mpr (gamma_log_deriv_lt hs)

private theorem constant_upper {p q s t : ℝ} (hp : 0 < p) (hq : 0 < q)
    (hs : 0 < s) (ht : 0 < t) :
    (p - 1) * log (t / s) - log s - (p + q) * log (1 + t / s) - Beta.logBeta (p, q) <
      Gamma.logDensity p (p / t) t := by
  let w := log (p / q)
  let g := logisticLoss p q
  obtain ⟨hstrict, _⟩ := beta_logistic_loss_geometry hp hq
  have hE : exp w = p / q := exp_log (div_pos hp hq)
  have hw : HasDerivAt g 0 w := by
    have hd := ((((hasDerivAt_exp w).const_add 1).log (by positivity)).const_mul (p + q)).sub
      ((hasDerivAt_id w).const_mul p)
    convert hd using 1
    · rfl
    · simp only [mul_one, hE]
      field_simp
      ring
  have hmin : IsMinOn g univ w :=
    hstrict.convexOn.isMinOn_of_rightDeriv_eq_zero (by simp : w ∈ interior (univ : Set ℝ))
      (hw.hasDerivWithinAt.derivWithin (uniqueDiffWithinAt_Ioi _))
  have hmin' : g w ≤ g (log (t / s)) := hmin (mem_univ _)
  have hgw : g w = (p + q) * log (p + q) - p * log p - q * log q := by
    dsimp only [g, logisticLoss]
    rw [hE, show 1 + p / q = (p + q) / q by field_simp; ring]
    rw [log_div (add_pos hp hq).ne' hq.ne', show w = log p - log q from
      log_div hp.ne' hq.ne']
    ring
  have hβ : Beta.logBeta (p, q) =
      log (Real.Gamma p) + log (Real.Gamma q) - log (Real.Gamma (p + q)) := by
    simp only [Beta.logBeta, Beta.normalizer]
    rw [log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hq)).ne'
       (Gamma_pos_of_pos (add_pos hp hq)).ne',
       log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne']
  have hg := gamma_remainder_anti (show q ∈ Ioi (0 : ℝ) from hq)
    (show p + q ∈ Ioi (0 : ℝ) from add_pos hp hq) (by linarith : q < p + q)
  change log (Real.Gamma (p + q)) - (p + q) * log (p + q) + (p + q) <
    log (Real.Gamma q) - q * log q + q at hg
  rw [hgw] at hmin'
  dsimp only [g, logisticLoss] at hmin'
  rw [exp_log (div_pos ht hs)] at hmin'
  rw [hβ, Gamma.logDensity, log_div hp.ne' ht.ne',
    show p / t * t = p by field_simp]
  have h := log_div ht.ne' hs.ne'
  rw [h] at hmin' ⊢
  nlinarith only [hmin', hg]

/-- Holding the first beta shape on a constant sample gives the optimized
gamma face as the strict, unattained supremum. -/
theorem held_first_constant_gamma (hn : 0 < n) {t p : ℝ} (ht : 0 < t) (hp : 0 < p) :
    let x := fun _ : Fin n ↦ t
    let V := (n : ℝ) * Gamma.logDensity p (p / t) t
    IsLUB {y | ∃ s > 0, ∃ q > 0, BetaPrime.logLik x s (p, q) = y} V ∧
      ∀ s > 0, ∀ q > 0, BetaPrime.logLik x s (p, q) < V := by
  intro x V
  have hlt (s : ℝ) (hs : 0 < s) (q : ℝ) (hq : 0 < q) :
      BetaPrime.logLik x s (p, q) < V := by
    have hN : (0 : ℝ) < n := by exact_mod_cast hn
    simp only [x, V, BetaPrime.logLik, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    exact mul_lt_mul_of_pos_left (constant_upper hp hq hs ht) hN
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro y ⟨s, hs, q, hq, rfl⟩
    exact (hlt s hs q hq).le
  · intro y hy
    have hT := AllFreeBetaPrime.gamma_path (x := x) (fun _ ↦ ht) (div_pos ht hp) hp
    simp only [inv_div, x, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at hT
    apply le_of_tendsto hT
    filter_upwards [self_mem_nhdsWithin] with a (ha : 0 < a)
    exact hy ⟨t / p / a, div_pos (div_pos ht hp) ha, a⁻¹, inv_pos.mpr ha, rfl⟩


/-- Holding the second beta shape on a constant sample gives the optimized
inverse-gamma face as the strict, unattained supremum. -/
theorem held_second_constant_inverse_gamma (hn : 0 < n) {t q : ℝ}
    (ht : 0 < t) (hq : 0 < q) :
    let x := fun _ : Fin n ↦ t
    let V := (n : ℝ) * (Gamma.logDensity q (q * t) t⁻¹ - 2 * log t)
    IsLUB {y | ∃ s > 0, ∃ p > 0, BetaPrime.logLik x s (p, q) = y} V ∧
      ∀ s > 0, ∀ p > 0, BetaPrime.logLik x s (p, q) < V := by
  intro x V
  have hr := (held_first_constant_gamma (n := n) hn (inv_pos.mpr ht) hq).2
  have hrate : q / t⁻¹ = q * t := by rw [div_inv_eq_mul]
  have hc : (∑ i, 2 * log (x i)) = (n : ℝ) * (2 * log t) := by
    simp [x, nsmul_eq_mul]
  have hlt (s : ℝ) (hs : 0 < s) (p : ℝ) (hp : 0 < p) :
      BetaPrime.logLik x s (p, q) < V := by
    have h := hr s⁻¹ (inv_pos.mpr hs) p hp
    rw [hrate] at h
    rw [AllFreeBetaPrime.reciprocal_logLik (fun _ ↦ ht : ∀ i, 0 < x i) hs, hc]
    change BetaPrime.logLik (fun _ : Fin n ↦ t⁻¹) s⁻¹ (q, p) -
      (n : ℝ) * (2 * log t) < (n : ℝ) * (Gamma.logDensity q (q * t) t⁻¹ - 2 * log t)
    linarith only [h]
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨s, hs, p, hp, rfl⟩
    exact (hlt s hs p hp).le
  · intro y hy
    let r := t⁻¹ / q
    have hr : 0 < r := div_pos (inv_pos.mpr ht) hq
    have htend := AllFreeBetaPrime.inverse_gamma_path (x := x) (fun _ ↦ ht) hr hq
    have he : r⁻¹ = q * t := by dsimp [r]; rw [inv_div, hrate]
    simp only [he, hc, x, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
      at htend
    have htend' : Tendsto (fun h : ℝ ↦ BetaPrime.logLik x (h / r) (h⁻¹, q))
        (𝓝[>] 0) (𝓝 V) := by
      convert htend using 1
      congr 1
      dsimp [V]
      ring
    apply le_of_tendsto htend'
    filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
    exact hy ⟨h / r, div_pos hh hr, h⁻¹, inv_pos.mpr hh, rfl⟩

end

end UnivariateML.Shape2BetaPrime

end
