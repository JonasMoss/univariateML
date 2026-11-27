module

public import UnivariateML.Family.Shape2ExpWeibull
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp

set_option linter.style.header false

/-! # Exponentiated Weibull at held scale

The two held-scale nonattainment branches use the concentration path and
optimized power trace, including the factor at exact contact. At held
shape, pointwise curvature and Cauchy–Schwarz give strict concavity of
the exponent-profiled objective.
-/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.Shape3ExpWeibull
open Shape2ExpWeibull ShapeFacesExpWeibull
noncomputable section

/-- All held-scale outcomes, with the strict upper bound on the power branch
and an explicit diverging path on the constant branch. -/
theorem held_scale_classification {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    let L := fun p : ℝ × ℝ ↦ logLik x p.1 a p.2
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, a < x i) ∧
    (∀ v ≥ a, (∀ i, x i = v) →
      Tendsto (fun b ↦ L (b, exp (exp (b * log (v / a))))) atTop atTop) ∧
    ((∀ i, x i ≤ a) → (∃ i, x i < a) →
      let D := ∑ i : Fin n, -log (x i / a)
      let P := heldPowerTrace x a ((n : ℝ) / D)
      IsLUB (L '' Weibull.domain) P ∧ ∀ p ∈ Weibull.domain, L p < P) := by
  refine ⟨held_scale_attains_iff hn hx ha, ?_, ?_⟩
  · intro v hv h
    have he : x = fun _ ↦ v := funext h
    simpa only [he] using held_scale_constant_concentration hn ha hv
  · intro hle hlt
    obtain ⟨hB, hstrict⟩ := held_scale_power_supremum hn hx ha hle hlt
    exact ⟨hB, fun p hp ↦ hstrict p.1 p.2 hp.1 hp.2⟩

private def H₀ (r t : ℝ) : ℝ := -log (1 - exp (-r * t))
private def H₁ (r t : ℝ) : ℝ := -r * exp (-r * t) / (1 - exp (-r * t))
private def H₂ (r t : ℝ) : ℝ := r ^ 2 * exp (-r * t) / (1 - exp (-r * t)) ^ 2

private theorem point_derivs {r t : ℝ} (hr : 0 < r) (ht : 0 < t) :
    HasDerivAt (H₀ r) (H₁ r t) t ∧ HasDerivAt (H₁ r) (H₂ r t) t := by
  have h := (((hasDerivAt_id t).const_mul (-r)).exp)
  have hp := h.const_sub 1
  have hP : 0 < 1 - exp (-r * t) :=
    sub_pos.mpr (exp_lt_one_iff.mpr (mul_neg_of_neg_of_pos (neg_lt_zero.mpr hr) ht))
  constructor
  · have hh := (hp.log hP.ne').neg
    have he : (-fun y : ℝ ↦ log (1 - exp (-r * y))) = H₀ r := by
      funext y
      rfl
    simpa only [he, H₁, id_eq, mul_one, neg_neg, neg_div, mul_comm (exp _) (-r)] using hh
  · have hq := (h.const_mul (-r)).div hp hP.ne'
    refine hq.congr_deriv ?_
    simp only [H₂, id_eq, mul_one]
    ring

private theorem point_bounds {r t : ℝ} (hr : 0 < r) (ht : 0 < t) :
    0 < H₀ r t ∧ 0 < H₂ r t ∧ H₁ r t ^ 2 ≤ H₀ r t * H₂ r t ∧
      H₂ r t < t⁻¹ ^ 2 := by
  let q := exp (-r * t)
  let P := 1 - q
  have hq : 0 < q := exp_pos _
  have hq1 : q < 1 := exp_lt_one_iff.mpr (mul_neg_of_neg_of_pos (neg_lt_zero.mpr hr) ht)
  have hP : 0 < P := sub_pos.mpr hq1
  have hP1 : P < 1 := sub_lt_self _ hq
  have hH : q ≤ H₀ r t := by
    have h := log_le_sub_one_of_pos hP
    change log (1 - q) ≤ 1 - q - 1 at h
    change q ≤ -log (1 - q)
    linarith
  refine ⟨(hq.trans_le hH), by unfold H₂; positivity, ?_, ?_⟩
  · change (-r * q / P) ^ 2 ≤ H₀ r t * (r ^ 2 * q / P ^ 2)
    rw [div_pow, ← mul_div_assoc]
    apply div_le_div_of_nonneg_right _ (sq_nonneg P)
    nlinarith only [mul_le_mul_of_nonneg_left hH (show 0 ≤ r ^ 2 * q by positivity)]
  · have ht' : 0 < r * t / 2 := by positivity
    have he : P = 2 * exp (-r * t / 2) * sinh (r * t / 2) := by
      dsimp only [P, q]
      rw [sinh_eq]
      have h1 : exp (-r * t / 2) * exp (r * t / 2) = 1 := by
        rw [← exp_add, show -r * t / 2 + r * t / 2 = 0 by ring, exp_zero]
      have h2 : exp (-r * t / 2) * exp (-(r * t / 2)) = exp (-r * t) := by
        rw [← exp_add]
        congr 1
        ring
      linear_combination -h1 + h2
    have hlt : r * t * exp (-r * t / 2) < P := by
      rw [he]
      have hh := mul_lt_mul_of_pos_left (self_lt_sinh_iff.mpr ht')
        (show 0 < 2 * exp (-r * t / 2) by positivity)
      linarith only [hh]
    have hsq := sq_lt_sq' (show -P < r * t * exp (-r * t / 2) by
      exact (neg_lt_zero.mpr hP).trans (by positivity)) hlt
    have hExp : exp (-r * t / 2) ^ 2 = q := by
      rw [pow_two, ← exp_add]
      congr 1
      ring
    rw [mul_pow, mul_pow, hExp] at hsq
    change r ^ 2 * q / P ^ 2 < t⁻¹ ^ 2
    rw [inv_pow, inv_eq_one_div, div_lt_div_iff₀ (sq_pos_of_pos hP) (sq_pos_of_pos ht)]
    nlinarith only [hsq]

private def H {n : ℕ} (r : Fin n → ℝ) (t : ℝ) : ℝ :=
  (∑ i, H₀ (r i) t) / n
private def D {n : ℕ} (r : Fin n → ℝ) (t : ℝ) : ℝ :=
  (∑ i, H₁ (r i) t) / n
private def E {n : ℕ} (r : Fin n → ℝ) (t : ℝ) : ℝ :=
  (∑ i, H₂ (r i) t) / n

/-- The held-shape, exponent-profiled objective is strictly concave in the
positive scale coordinate t. The additive sample constant is omitted. -/
theorem held_shape_profile_strictConcave {n : ℕ} (hn : 0 < n) (r : Fin n → ℝ)
    (hr : ∀ i, 0 < r i) (b : ℝ) :
    let h := fun t : ℝ ↦ (∑ i : Fin n, -log (1 - exp (-(r i) * t))) / n
    StrictConcaveOn ℝ (Ioi 0) (fun t ↦ log t - b * t + h t - log (h t)) := by
  change StrictConcaveOn ℝ (Ioi 0) (fun t ↦ log t - b * t + H r t - log (H r t))
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hne : (Finset.univ : Finset (Fin n)).Nonempty := ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hpos (t : ℝ) (ht : 0 < t) : 0 < H r t :=
    div_pos (Finset.sum_pos (fun i _ ↦ (point_bounds (hr i) ht).1) hne) hnR
  have hD (t : ℝ) (ht : 0 < t) : HasDerivAt (H r) (D r t) t := by
    unfold H D
    exact (HasDerivAt.fun_sum (u := Finset.univ)
      (fun i _ ↦ (point_derivs (hr i) ht).1)).div_const _
  have hE (t : ℝ) (ht : 0 < t) : HasDerivAt (D r) (E r t) t := by
    unfold D E
    exact (HasDerivAt.fun_sum (u := Finset.univ)
      (fun i _ ↦ (point_derivs (hr i) ht).2)).div_const _
  let L := fun t ↦ log t - b * t + H r t - log (H r t)
  let G := fun t ↦ t⁻¹ - b + D r t - D r t / H r t
  have hL (t : ℝ) (ht : 0 < t) : HasDerivAt L (G t) t := by
    have h := (((hasDerivAt_log ht.ne').sub ((hasDerivAt_id t).const_mul b)).add
      (hD t ht)).sub ((hD t ht).log (hpos t ht).ne')
    convert h using 1
    · funext u
      rfl
    · simp only [G, mul_one]
  apply strictConcaveOn_of_deriv2_neg' (convex_Ioi 0)
  · exact fun t ht ↦ (hL t ht).continuousAt.continuousWithinAt
  · intro t ht
    have hC := Finset.sum_sq_le_sum_mul_sum_of_sq_le_mul Finset.univ
      (fun i _ ↦ ((point_bounds (hr i) ht).1).le)
      (fun i _ ↦ ((point_bounds (hr i) ht).2.1).le)
      (fun i _ ↦ (point_bounds (hr i) ht).2.2.1)
    have hC' : D r t ^ 2 ≤ H r t * E r t := by
      unfold D H E
      rw [div_pow, div_mul_div_comm, ← sq]
      exact div_le_div_of_nonneg_right hC (sq_nonneg _)
    have hlt : E r t < t⁻¹ ^ 2 := by
      apply (div_lt_iff₀ hnR).2
      have h := Finset.sum_lt_sum_of_nonempty hne fun i _ ↦ (point_bounds (hr i) ht).2.2.2
      simpa [nsmul_eq_mul, mul_comm] using h
    have hG : HasDerivAt G
        (-t⁻¹ ^ 2 + E r t - (E r t * H r t - D r t * D r t) / H r t ^ 2) t := by
      have hinv := (hasDerivAt_inv ht.ne')
      refine (((hinv.sub_const b).add (hE t ht)).sub
        ((hE t ht).div (hD t ht) (hpos t ht).ne')).congr_deriv ?_
      simp only [inv_pow]
    have he : deriv L =ᶠ[𝓝 t] G := by
      filter_upwards [Ioi_mem_nhds ht] with u hu
      exact (hL u hu).deriv
    change deriv (deriv L) t < 0
    rw [Filter.EventuallyEq.deriv_eq he, hG.deriv]
    have hnon : 0 ≤ (E r t * H r t - D r t * D r t) / H r t ^ 2 :=
      div_nonneg (by nlinarith only [hC']) (sq_nonneg _)
    linarith only [hlt, hnon]

/-- For each positive held shape, a nonconstant positive sample has a
unique scale and exponent fit. Constants have no finite maximizer. -/
theorem held_shape_unique_iff_nonconstant {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    (∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x b q.1 q.2) Weibull.domain p) ↔
      ∃ i j, x i ≠ x j := by
  let r := fun i ↦ x i ^ b
  let t := fun a : ℝ ↦ exp (-(b * log a))
  let a := fun u : ℝ ↦ exp (-log u / b)
  let B := (∑ i : Fin n, r i) / (n : ℝ)
  let F := fun u ↦ log u - B * u + H r u - log (H r u)
  let N := (n : ℝ) * (log b - 1) + (b - 1) * ∑ i : Fin n, log (x i)
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hr (i) : 0 < r i := rpow_pos_of_pos (hx i) _
  have hpos (u : ℝ) (hu : 0 < u) : 0 < H r u :=
    div_pos (Finset.sum_pos (fun i _ ↦ (point_bounds (hr i) hu).1)
      ⟨⟨0, hn⟩, Finset.mem_univ _⟩) hnR
  have hat (u : ℝ) (hu : 0 < u) : t (a u) = u := by
    dsimp [t, a]
    rw [log_exp, show -(b * (-log u / b)) = log u by field_simp, exp_log hu]
  have hinj : InjOn t (Ioi 0) := by
    intro s hs u hu h
    apply (log_injOn_pos hs hu)
    apply mul_left_cancel₀ hb.ne'
    exact neg_injective (exp_injective h)
  have hlik (s c : ℝ) (hs : 0 < s) (hc : 0 < c) :
      logLik x b s c = N + (n : ℝ) * (F (t s) +
        (log (c * H r (t s)) - c * H r (t s) + 1)) := by
    have hu (i : Fin n) : (x i / s) ^ b = r i * t s := by
      dsimp [r, t]
      rw [div_rpow (hx i).le hs.le, rpow_def_of_pos hs, exp_neg, div_eq_mul_inv, mul_comm (log s) b]
    have hH := hpos (t s) (exp_pos _)
    rw [logLik, log_mul hc.ne' hH.ne']
    simp only [Weibull.logLik, Weibull.logDensity, hu, H, H₀, N, F, B, t,
      log_exp, log_div (hx _).ne' hs.ne', Finset.sum_sub_distrib, ← Finset.mul_sum,
      Finset.sum_neg_distrib, neg_mul, mul_comm (r _) (exp _)]
    field_simp
    simp only [Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum]
    ring
  have hprofile (s : ℝ) (hs : 0 < s) :
      logLik x b s (H r (t s))⁻¹ = N + (n : ℝ) * F (t s) := by
    rw [hlik s _ hs (inv_pos.mpr (hpos _ (exp_pos _))), inv_mul_cancel₀ (hpos _ (exp_pos _)).ne']
    ring_nf
    simp
  constructor
  · intro h
    exact (held_shape_attains_iff hn hx hb).mp (by
      obtain ⟨p, ⟨hp, hm⟩, _⟩ := h
      exact ⟨p, hp, hm⟩)
  · intro hne
    obtain ⟨p, hp, hm⟩ := (held_shape_attains_iff hn hx hb).mpr hne
    have hbest (q : ℝ × ℝ) (hq : q ∈ Weibull.domain)
        (hqm : IsMaxOn (fun q : ℝ × ℝ ↦ logLik x b q.1 q.2) Weibull.domain q) :
        q.2 = (H r (t q.1))⁻¹ ∧ IsMaxOn F (Ioi 0) (t q.1) := by
      have hH := hpos (t q.1) (exp_pos _)
      have hq2 : 0 < q.2 := hq.2
      have hq1 : 0 < q.1 := hq.1
      have hc : q.2 = (H r (t q.1))⁻¹ := by
        have hle := hqm (show (q.1, (H r (t q.1))⁻¹) ∈ Weibull.domain from
          ⟨hq.1, inv_pos.mpr hH⟩)
        change logLik x b q.1 (H r (t q.1))⁻¹ ≤ logLik x b q.1 q.2 at hle
        rw [hprofile _ hq1, hlik _ _ hq1 hq2] at hle
        by_contra hne'
        have hne1 : q.2 * H r (t q.1) ≠ 1 := by
          intro h
          exact hne' ((eq_div_iff hH.ne').2 h |>.trans (one_div _))
        have hlt := log_lt_sub_one_of_pos (mul_pos hq2 hH) hne1
        nlinarith only [hle, mul_lt_mul_of_pos_left hlt hnR]
      refine ⟨hc, fun u hu ↦ ?_⟩
      have hs : 0 < a u := exp_pos _
      have hle := hqm (show (a u, (H r (t (a u)))⁻¹) ∈ Weibull.domain from
        ⟨hs, inv_pos.mpr (hpos _ (exp_pos _))⟩)
      change logLik x b (a u) (H r (t (a u)))⁻¹ ≤ logLik x b q.1 q.2 at hle
      rw [hc, hprofile _ hs, hprofile _ hq1, hat u hu] at hle
      exact (mul_le_mul_iff_right₀ hnR).mp (le_of_add_le_add_left hle)
    refine ⟨p, ⟨hp, hm⟩, ?_⟩
    rintro q ⟨hq, hqm⟩
    have hconc : StrictConcaveOn ℝ (Ioi 0) F :=
      held_shape_profile_strictConcave hn r hr B
    obtain ⟨hp₂, hpF⟩ := hbest p hp hm
    obtain ⟨hq₂, hqF⟩ := hbest q hq hqm
    have h1 : q.1 = p.1 := hinj hq.1 hp.1
      (hconc.eq_of_isMaxOn hqF hpF (exp_pos _) (exp_pos _))
    exact Prod.ext h1 (by rw [hq₂, hp₂, h1])

end
end UnivariateML.Shape3ExpWeibull
end
