module

public import UnivariateML.Generic.ExpSum
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! # Four - level separation on strict support

The kernels omit terms independent of the observation. The inverse Burr kernel
uses the equivalent positive - power GB2 chart, with unit upper beta shape.
Generalized beta is identified up to the product of power and lower shape on
the unit-upper-shape slice.
-/

@[expose] public section

namespace UnivariateML.ExpSumSeparation

private theorem three_coefficients (z : Finset ℝ) (hz : 4 ≤ z.card)
    {p A B C : ℝ} (hp : 0 < p)
    (h : ∀ t ∈ z, A + B * Real.exp (p * t) + C * Real.exp ((2 * p) * t) = 0) :
    A = 0 ∧ B = 0 ∧ C = 0 := by
  classical
  let s : Finset ℝ := {0, p, 2 * p}
  let c : ℝ → ℝ := fun r ↦ if r = 0 then A else if r = p then B else C
  have h01 : p ≠ 0 := by linarith
  have h02 : 2 * p ≠ 0 := by linarith
  have h12 : p ≠ 2 * p := by linarith
  have hc := ExpSum.identity_of_card_le s z c
    (Finset.card_le_three.trans (by omega : 3 ≤ z.card)) (by
      intro t ht
      simpa [s, c, Finset.sum_insert, h01, h01.symm, h02, h02.symm, h12,
        h12.symm, add_assoc] using h t ht)
  exact ⟨by simpa [c] using hc 0 (by simp [s]),
    by simpa [c, h01] using hc p (by simp [s]),
    by simpa [c, h02, h12.symm] using hc (2 * p) (by simp [s])⟩

private theorem four_coefficients (z : Finset ℝ) (hz : 4 ≤ z.card)
    {p q A B C D : ℝ} (hp : 0 < p) (hq : 0 < q) (hpq : p ≠ q)
    (h : ∀ t ∈ z, A + B * Real.exp (p * t) + C * Real.exp (q * t) +
      D * Real.exp ((p + q) * t) = 0) :
    A = 0 ∧ B = 0 ∧ C = 0 ∧ D = 0 := by
  classical
  let s : Finset ℝ := {0, p, q, p + q}
  let c : ℝ → ℝ := fun r ↦ if r = 0 then A else if r = p then B else if r = q then C else D
  have h01 : p ≠ 0 := by linarith
  have h02 : q ≠ 0 := by linarith
  have h03 : p + q ≠ 0 := by linarith
  have h13 : p ≠ p + q := by linarith
  have h23 : q ≠ p + q := by linarith
  have hc := ExpSum.identity_of_card_le s z c (Finset.card_le_four.trans hz) (by
    intro t ht
    simpa [s, c, Finset.sum_insert, h01, h01.symm, h02, h02.symm, h03, h03.symm,
      hpq, hpq.symm, h13, h13.symm, h23, h23.symm, add_assoc] using h t ht)
  exact ⟨by simpa [c] using hc 0 (by simp [s]),
    by simpa [c, h01] using hc p (by simp [s]),
    by simpa [c, h02, hpq.symm] using hc q (by simp [s]),
    by simpa [c, h03, h13.symm, h23.symm] using hc (p + q) (by simp [s])⟩

private theorem rational_separation (z : Finset ℝ) (hz : 4 ≤ z.card)
    {p q A A' D D' S S' : ℝ} (hp : 0 < p) (hq : 0 < q)
    (hd : ∀ t ∈ z, 1 + S * Real.exp (p * t) ≠ 0 ∧ 1 + S' * Real.exp (q * t) ≠ 0)
    (h : ∀ t ∈ z, A + D * Real.exp (p * t) / (1 + S * Real.exp (p * t)) =
      A' + D' * Real.exp (q * t) / (1 + S' * Real.exp (q * t))) :
    A = A' ∧ ((D = 0 ∧ D' = 0) ∨ (p = q ∧ D = D' ∧ S = S')) := by
  have cross (t : ℝ) (ht : t ∈ z) :
      (A - A') + ((A - A') * S + D) * Real.exp (p * t) + ((A - A') * S' - D') * Real.exp (q * t) +
      ((A - A') * S * S' + D * S' - D' * S) * Real.exp ((p + q) * t) = 0 := by
    have he := h t ht
    obtain ⟨h₁, h₂⟩ := hd t ht
    rw [add_mul p q t, Real.exp_add]
    rw [add_div_eq_mul_add_div _ _ h₁, add_div_eq_mul_add_div _ _ h₂] at he
    linear_combination (div_eq_div_iff h₁ h₂).mp he
  by_cases hep : p = q
  · subst q
    obtain ⟨hA, hD, hDS⟩ := three_coefficients z hz hp (A := A - A')
      (B := (A - A') * S + D + ((A - A') * S' - D'))
      (C := (A - A') * S * S' + D * S' - D' * S) (fun t ht ↦ by
        rw [show 2 * p = p + p by ring]
        linear_combination cross t ht)
    have hAA := sub_eq_zero.mp hA
    have hDD : D = D' := by
      rw [hA] at hD
      linarith
    refine ⟨hAA, ?_⟩
    by_cases hd0 : D = 0
    · exact Or.inl ⟨hd0, hDD ▸ hd0⟩
    · refine Or.inr ⟨rfl, hDD, ?_⟩
      have he : D * (S' - S) = 0 := by
        rw [hAA, ← hDD] at hDS
        linear_combination hDS
      exact (sub_eq_zero.mp ((mul_eq_zero.mp he).resolve_left hd0)).symm
  · obtain ⟨hA, hD, hD', _⟩ := four_coefficients z hz hp hq hep cross
    have hAA := sub_eq_zero.mp hA
    exact ⟨hAA, Or.inl ⟨by simpa [hAA] using hD, by simpa [hAA] using hD'⟩⟩

/-- Observation-dependent part of the shifted GB2 log density on strict support.
Arguments are threshold, upper shape, power, lower shape, scale, observation. -/
noncomputable def gb2Kernel (m u p l s x : ℝ) : ℝ :=
  (l * p - 1) * Real.log (x - m) -
    (l + u) * Real.log (1 + Real.exp ((-p) * Real.log s) * Real.exp (p * Real.log (x - m)))

/-- Observation-dependent part of the fixed-endpoint generalized - beta log density.
Arguments are endpoint, lower shape, upper shape, power, observation. -/
noncomputable def genbetaKernel (s l u p x : ℝ) : ℝ :=
  (l * p - 1) * Real.log (x / s) + (u - 1) * Real.log (1 - Real.exp (p * Real.log (x / s)))

private theorem kernel_derivative {f : ℝ → ℝ} {v w x A B S p : ℝ}
    (hf : HasDerivAt f w x) (hv : f x = v)
    (hd : 1 + S * Real.exp (p * v) ≠ 0) :
    HasDerivAt (fun y ↦ A * f y + B * Real.log (1 + S * Real.exp (p * f y)))
      ((A + (B * S * p) * Real.exp (p * v) / (1 + S * Real.exp (p * v))) * w) x := by
  have he := ((((hf.const_mul p).exp.const_mul S).const_add 1).log (hv ▸ hd))
  have hh := (hf.const_mul A).add (he.const_mul B)
  rw [hv] at hh
  convert hh using 1
  ring

private theorem gb2_derivative {m u p l s x : ℝ} (hx : m < x) :
    HasDerivAt (gb2Kernel m u p l s)
      ((l * p - 1 + (-(l + u) * Real.exp ((-p) * Real.log s) * p) *
        Real.exp (p * Real.log (x - m)) /
        (1 + Real.exp ((-p) * Real.log s) * Real.exp (p * Real.log (x - m)))) * (x - m)⁻¹) x := by
  have hlog := (((hasDerivAt_id x).sub_const m).log (sub_ne_zero.mpr hx.ne'))
  convert kernel_derivative hlog rfl (by positivity : 1 +
    Real.exp ((-p) * Real.log s) * Real.exp (p * Real.log (x - m)) ≠ 0)
    (A := l * p - 1) (B := -(l + u)) using 1
  · ext y
    unfold gb2Kernel
    simp only [id_eq, neg_mul, sub_eq_add_neg]
  · simp [one_div]

private theorem genbeta_derivative {s l u p x : ℝ}
    (hx : 0 < x) (hxs : x < s) (hp : 0 < p) :
    HasDerivAt (genbetaKernel s l u p)
      ((l * p - 1 + ((u - 1) * (-1) * p) * Real.exp (p * Real.log (x / s)) /
        (1 + (-1) * Real.exp (p * Real.log (x / s)))) * x⁻¹) x := by
  have hs : 0 < s := hx.trans hxs
  have hlog := (((hasDerivAt_id x).div_const s).log (div_ne_zero hx.ne' hs.ne'))
  have hlog' : HasDerivAt (fun y : ℝ ↦ Real.log (y / s)) x⁻¹ x := by
    convert hlog using 1
    · rfl
    · change x⁻¹ = 1 / s / (x / s)
      field_simp
  have hpow : Real.exp (p * Real.log (x / s)) < 1 := by
    rw [← Real.exp_zero]
    apply Real.exp_lt_exp.mpr
    exact mul_neg_of_pos_of_neg hp (Real.log_neg (div_pos hx hs) ((div_lt_one hs).mpr hxs))
  unfold genbetaKernel
  simpa [sub_eq_add_neg] using
    (kernel_derivative hlog' rfl (by linarith : 1 + (-1) * Real.exp (p * Real.log (x / s)) ≠ 0)
      (A := l * p - 1) (B := u - 1))

private theorem shifted_separation {m u p l s u' p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l) (hs : 0 < s)
    (_hu' : 0 < u') (hp' : 0 < p') (_hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p l s) x = deriv (gb2Kernel m u' p' l' s') x) :
    u = u' ∧ p = p' ∧ l = l' ∧ s = s' := by
  classical
  let f := fun x : ℝ ↦ Real.log (x - m)
  let S := Real.exp ((-p) * Real.log s)
  let S' := Real.exp ((-p') * Real.log s')
  have hi : Set.InjOn f (↑z : Set ℝ) := by
    intro x hxx y hyy he
    have hy := Real.strictMonoOn_log.injOn (sub_pos.mpr (hx x hxx))
      (sub_pos.mpr (hx y hyy)) he
    linarith
  have hc : 4 ≤ (z.image f).card := (Finset.card_image_of_injOn hi).symm ▸ hz
  obtain ⟨hA, hd | ⟨rfl, hD, hS⟩⟩ := rational_separation (z.image f) hc hp hp'
    (A := l * p - 1) (A' := l' * p' - 1) (D := -(l + u) * S * p) (D' := -(l' + u') * S' * p')
    (S := S) (S' := S')
    (fun t _ ↦ ⟨by dsimp [S]; positivity, by dsimp [S']; positivity⟩)
    (fun t ht ↦ by
      obtain ⟨x, hxz, rfl⟩ := Finset.mem_image.mp ht
      have he := h x hxz
      rw [(gb2_derivative (u := u) (p := p) (l := l) (s := s) (hx x hxz)).deriv,
        (gb2_derivative (u := u') (p := p') (l := l') (s := s') (hx x hxz)).deriv] at he
      exact mul_right_cancel₀ (inv_ne_zero (sub_ne_zero.mpr (hx x hxz).ne')) he)
  · exact False.elim ((show -(l + u) * S * p ≠ 0 by
      exact mul_ne_zero (mul_ne_zero (neg_ne_zero.mpr (by positivity))
        (Real.exp_pos _).ne') hp.ne') hd.1)
  · have hll : l = l' := by
      apply mul_right_cancel₀ hp.ne'
      linarith
    have hss : s = s' := by
      apply Real.strictMonoOn_log.injOn hs hs'
      exact mul_left_cancel₀ (neg_ne_zero.mpr hp.ne') (Real.exp_injective hS)
    have hsum : -(l + u) = -(l' + u') := by
      rw [← hS] at hD
      exact mul_right_cancel₀ (Real.exp_pos _).ne' (mul_right_cancel₀ hp.ne' hD)
    exact ⟨by linarith, rfl, hll, hss⟩

/-- Four strict interior levels identify a fixed-endpoint generalized - beta
candidate, with the sole alias given by the common rate at unit upper shape. -/
theorem genbeta_held_scale_separation {s l u p l' u' p' : ℝ}
    (hp : 0 < p) (hp' : 0 < p')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x ∧ x < s)
    (h : ∀ x ∈ z, deriv (genbetaKernel s l u p) x = deriv (genbetaKernel s l' u' p') x) :
    (l = l' ∧ u = u' ∧ p = p') ∨ (u = 1 ∧ u' = 1 ∧ l * p = l' * p') := by
  classical
  have hs : 0 < s := by
    obtain ⟨x, hxz⟩ := Finset.card_pos.mp (by omega : 0 < z.card)
    exact (hx x hxz).1.trans (hx x hxz).2
  let f := fun x : ℝ ↦ Real.log (x / s)
  have hi : Set.InjOn f (↑z : Set ℝ) := by
    intro x hxx y hyy he
    exact (div_left_inj₀ hs.ne').mp
      (Real.strictMonoOn_log.injOn (div_pos (hx x hxx).1 hs)
        (div_pos (hx y hyy).1 hs) he)
  have hc : 4 ≤ (z.image f).card := (Finset.card_image_of_injOn hi).symm ▸ hz
  obtain ⟨hA, hD | ⟨rfl, hD, _⟩⟩ := rational_separation (z.image f) hc hp hp'
    (A := l * p - 1) (A' := l' * p' - 1) (D := (u - 1) * (-1) * p) (D' := (u' - 1) * (-1) * p')
    (S := -1) (S' := -1)
    (fun t ht ↦ by
      obtain ⟨x, hxz, rfl⟩ := Finset.mem_image.mp ht
      have hf : f x < 0 :=
        Real.log_neg (div_pos (hx x hxz).1 hs) ((div_lt_one hs).mpr (hx x hxz).2)
      have h₁ := Real.exp_lt_one_iff.mpr (mul_neg_of_pos_of_neg hp hf)
      have h₂ := Real.exp_lt_one_iff.mpr (mul_neg_of_pos_of_neg hp' hf)
      exact ⟨by linarith, by linarith⟩)
    (fun t ht ↦ by
      obtain ⟨x, hxz, rfl⟩ := Finset.mem_image.mp ht
      have he := h x hxz
      rw [(genbeta_derivative (hx x hxz).1 (hx x hxz).2 hp).deriv,
        (genbeta_derivative (hx x hxz).1 (hx x hxz).2 hp').deriv] at he
      exact mul_right_cancel₀ (inv_ne_zero (hx x hxz).1.ne') he)
  · right
    have identify {v a : ℝ} (ha : 0 < a) (h₀ : (v - 1) * (-1) * a = 0) : v = 1 := by
      have he := (mul_eq_zero.mp h₀).resolve_right ha.ne'
      exact sub_eq_zero.mp ((mul_eq_zero.mp he).resolve_right (by norm_num))
    exact ⟨identify hp hD.1, identify hp' hD.2, by linarith⟩
  · left
    have hl : l = l' := mul_right_cancel₀ hp.ne' (by linarith)
    have hu : u = u' := by
      have he := mul_right_cancel₀ hp.ne' hD
      linarith
    exact ⟨hl, hu, rfl⟩

/-- With endpoint and a nonunit upper shape held, four strict interior levels
identify lower shape and power. -/
theorem genbeta_held_scale_shape2_separation {s l u p l' p' : ℝ}
    (hu : u ≠ 1) (hp : 0 < p) (hp' : 0 < p')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x ∧ x < s)
    (h : ∀ x ∈ z, deriv (genbetaKernel s l u p) x = deriv (genbetaKernel s l' u p') x) :
    l = l' ∧ p = p' := by
  rcases genbeta_held_scale_separation hp hp' z hz hx h with ⟨hl, _, hpow⟩ | ⟨he, _, _⟩
  · exact ⟨hl, hpow⟩
  · exact (hu he).elim

/-- Four strict-support levels separate GB2 candidates. -/
theorem gb2_separation {u p l s u' p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l) (hs : 0 < s)
    (hu' : 0 < u') (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel 0 u p l s) x = deriv (gb2Kernel 0 u' p' l' s') x) :
    u = u' ∧ p = p' ∧ l = l' ∧ s = s' := by
  have he := shifted_separation hu hp hl hs hu' hp' hl' hs' z hz hx h
  exact he

/-- Four strict-support levels separate GB2 with upper shape held candidates. -/
theorem gb2_held_shape1_separation {u p l s p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel 0 u p l s) x = deriv (gb2Kernel 0 u p' l' s') x) :
    p = p' ∧ l = l' ∧ s = s' := by
  have he := shifted_separation hu hp hl hs hu hp' hl' hs' z hz hx h
  exact he.2

/-- Four strict-support levels separate GB2 with lower shape held candidates. -/
theorem gb2_held_shape3_separation {u p l s u' p' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (hu' : 0 < u') (hp' : 0 < p') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel 0 u p l s) x = deriv (gb2Kernel 0 u' p' l s') x) :
    u = u' ∧ p = p' ∧ s = s' := by
  have he := shifted_separation hu hp hl hs hu' hp' hl hs' z hz hx h
  exact ⟨he.1, he.2.1, he.2.2.2⟩

/-- Four strict-support levels separate Feller–Pareto with minimum held candidates. -/
theorem fpareto_held_min_separation {m u p l s u' p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l) (hs : 0 < s)
    (hu' : 0 < u') (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p l s) x = deriv (gb2Kernel m u' p' l' s') x) :
    u = u' ∧ p = p' ∧ l = l' ∧ s = s' := by
  have he := shifted_separation hu hp hl hs hu' hp' hl' hs' z hz hx h
  exact he

/-- Four strict-support levels separate Feller–Pareto candidates with
minimum and upper shape held. -/
theorem fpareto_held_min_shape1_separation {m u p l s p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p l s) x = deriv (gb2Kernel m u p' l' s') x) :
    p = p' ∧ l = l' ∧ s = s' := by
  have he := shifted_separation hu hp hl hs hu hp' hl' hs' z hz hx h
  exact he.2

/-- Four strict-support levels separate Feller–Pareto candidates with
minimum and lower shape held. -/
theorem fpareto_held_min_shape3_separation {m u p l s u' p' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (hu' : 0 < u') (hp' : 0 < p') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p l s) x = deriv (gb2Kernel m u' p' l s') x) :
    u = u' ∧ p = p' ∧ s = s' := by
  have he := shifted_separation hu hp hl hs hu' hp' hl hs' z hz hx h
  exact ⟨he.1, he.2.1, he.2.2.2⟩

/-- Four strict-support levels separate Burr XII candidates. -/
theorem burr_separation {u p s u' p' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hs : 0 < s)
    (hu' : 0 < u') (hp' : 0 < p') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel 0 u p 1 s) x = deriv (gb2Kernel 0 u' p' 1 s') x) :
    u = u' ∧ p = p' ∧ s = s' := by
  have he := shifted_separation hu hp (by norm_num) hs hu' hp' (by norm_num) hs' z hz hx h
  exact ⟨he.1, he.2.1, he.2.2.2⟩

/-- Observation-dependent part of the inverse Burr log density in the
reciprocal-power parametrization of the dossier. -/
noncomputable def invburrKernel (l p s x : ℝ) : ℝ :=
  (-p - 1) * Real.log x - (l + 1) * Real.log (1 + Real.exp (p * (Real.log s - Real.log x)))

private theorem invburr_deriv_eq {l p s x : ℝ} (hx : 0 < x) :
    deriv (invburrKernel l p s) x = deriv (gb2Kernel 0 1 p l s) x := by
  have hsame : invburrKernel l p s =ᶠ[nhds x]
      (fun y ↦ gb2Kernel 0 1 p l s y - (l + 1) * p * Real.log s) := by
    filter_upwards [Ioi_mem_nhds hx] with y _
    let v := p * (Real.log y - Real.log s)
    have he : 1 + Real.exp (-v) = (1 + Real.exp v) * Real.exp (-v) := by
      rw [add_mul, one_mul, ← Real.exp_add, add_neg_cancel, Real.exp_zero, add_comm]
    rw [invburrKernel, gb2Kernel]
    simp only [sub_zero, ← Real.exp_add]
    rw [show p * (Real.log s - Real.log y) = -v by dsimp [v]; ring,
      show (-p) * Real.log s + p * Real.log y = v by dsimp [v]; ring,
      he, Real.log_mul (by positivity : 1 + Real.exp v ≠ 0) (Real.exp_pos _).ne',
      Real.log_exp]
    dsimp only [v]
    ring
  rw [hsame.deriv_eq]
  exact ((gb2_derivative hx).sub_const _).deriv.trans (gb2_derivative hx).deriv.symm

/-- Four strict-support levels separate inverse Burr candidates. -/
theorem invburr_separation {p l s p' l' s' : ℝ}
    (hp : 0 < p) (hl : 0 < l) (hs : 0 < s)
    (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (invburrKernel l p s) x = deriv (invburrKernel l' p' s') x) :
    l = l' ∧ p = p' ∧ s = s' := by
  have he := shifted_separation (u := 1) (u' := 1)
    (by norm_num) hp hl hs (by norm_num) hp' hl' hs' z hz hx (fun x hxz ↦ by
    simpa only [invburr_deriv_eq (hx x hxz)] using h x hxz)
  exact ⟨he.2.2.1, he.2.1, he.2.2.2⟩

/-- Four strict-support levels separate Pareto IV with minimum held candidates. -/
theorem pareto4_held_min_separation {m u p s u' p' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hs : 0 < s)
    (hu' : 0 < u') (hp' : 0 < p') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p 1 s) x = deriv (gb2Kernel m u' p' 1 s') x) :
    u = u' ∧ p = p' ∧ s = s' := by
  have he := shifted_separation hu hp (by norm_num) hs hu' hp' (by norm_num) hs' z hz hx h
  exact ⟨he.1, he.2.1, he.2.2.2⟩

end UnivariateML.ExpSumSeparation

end
