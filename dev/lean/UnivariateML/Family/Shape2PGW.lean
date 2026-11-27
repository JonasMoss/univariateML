module

public import UnivariateML.Family.AllFreePGW

set_option linter.style.header false

/-!
Complete sample predicates for the PGW held exponent masks and the
constant threshold-collapse branch.

## Deviations

The Gompertz supremum theorem addresses the narrow held-shape predicate. The held-scale
concentration result addresses the constant branch at or below the hold. The finite
stationary lists and the all-free two-face comparison are outside this module.
-/

@[expose] public section

open Real Set Filter
namespace UnivariateML.Shape2PGW

open ShapeFacesPGW

variable {n : ℕ}

theorem held_exponent_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {c : ℝ} (hc : 0 < c) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q ↦ logLik x q.1 q.2 c) Weibull.domain p) ↔
      ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨p, _, hm⟩
    by_contra h
    push Not at h
    let i : Fin n := ⟨0, hn⟩
    have he : x = fun _ ↦ x i := funext fun j ↦ h j i
    have ht := held_exponent_constant_unbounded hn (hx i) hc
    obtain ⟨b, hb, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (ht.eventually (eventually_gt_atTop (logLik x p.1 p.2 c)))).exists
    have hle : logLik x b (x i) c ≤ logLik x p.1 p.2 c :=
      isMaxOn_iff.mp hm (b, x i) ⟨hb, hx i⟩
    rw [he] at hle hgt
    exact hgt.not_ge hle
  · intro h
    exact (held_exponent_mle hn hx hc h).exists

theorem held_exponent_scale_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a c : ℝ} (ha : 0 < a) (hc : 0 < c) :
    (∃ b ∈ Ioi (0 : ℝ), IsMaxOn (fun b ↦ logLik x b a c) (Ioi 0) b) ↔
      ∃ i, x i ≠ a := by
  constructor
  · rintro ⟨b, _, hm⟩
    by_contra h
    push Not at h
    have ht := held_exponent_scale_unbounded hn ha hc h
    obtain ⟨d, hd, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (ht.eventually (eventually_gt_atTop (logLik x b a c)))).exists
    exact hgt.not_ge (hm hd)
  · intro h
    exact (held_exponent_scale_unique hn hx ha hc h).exists

/-- With scale held, every constant sample at or below it admits a
concentration path through positive shape and exponent. -/
theorem held_scale_constant_at_or_below (hn : 0 < n) {t a : ℝ}
    (ht : 0 < t) (hle : t ≤ a) :
    ∃ c : ℝ → ℝ, (∀ b, 0 < c b) ∧
      Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ t) b a (c b)) atTop atTop := by
  rcases hle.eq_or_lt with rfl | hlt
  · exact ⟨fun _ ↦ 1, fun _ ↦ one_pos, held_exponent_constant_unbounded hn ht one_pos⟩
  · exact ⟨fun b ↦ ((t / a) ^ b)⁻¹,
      fun b ↦ inv_pos.mpr (rpow_pos_of_pos (div_pos ht (ht.trans hlt)) b),
      AllFreePGW.held_scale_constant_below hn ht hlt⟩

/-- On the narrow held-shape branch, the optimized Gompertz value
is the supremum and strictly beats every finite point. -/
theorem held_shape_narrow_supremum (hn : 0 < n) {x : Fin n → ℝ} {b X : ℝ}
    (hb : 0 < b) (hx : ∀ i, 0 < x i) (hX : ∀ i, x i ≤ X)
    (hnarrow : ∀ i, 1 / 2 ≤ (x i / X) ^ b) :
    ∃ r > 0,
      IsLUB ((fun p : ℝ × ℝ ↦ logLik x b p.1 p.2) '' Weibull.domain)
        (AllFreePGW.gompertzLogLik x b r) ∧
      ∀ p ∈ Weibull.domain, logLik x b p.1 p.2 < AllFreePGW.gompertzLogLik x b r := by
  obtain ⟨r, ⟨hr, hm⟩, _⟩ := AllFreePGW.gompertz_unique hn hx (b := b)
  have hlt (p : ℝ × ℝ) (hp : p ∈ Weibull.domain) :
      logLik x b p.1 p.2 < AllFreePGW.gompertzLogLik x b r := by
    by_contra! h
    exact AllFreePGW.held_shape_narrow_no_max hn hb hx hX hnarrow
      ((AllFreePGW.held_shape_attains_iff_face hn hx hb).2
        ⟨p.1, hp.1, p.2, hp.2, fun s hs ↦ (hm hs).trans h⟩)
  refine ⟨r, hr, ⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hlt p hp).le
  · intro u hu
    apply le_of_tendsto (AllFreePGW.gompertz_path hb hr hx)
    filter_upwards [self_mem_nhdsWithin] with t (ht : 0 < t)
    exact hu ⟨((t * r) ^ (-1 / b), t⁻¹),
      ⟨rpow_pos_of_pos (mul_pos ht hr) _, inv_pos.mpr ht⟩, rfl⟩

end UnivariateML.Shape2PGW

end
