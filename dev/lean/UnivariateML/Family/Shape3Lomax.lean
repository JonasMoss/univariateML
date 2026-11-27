module

public import UnivariateML.Family.TiesLomax

set_option linter.style.header false

/-! # Lomax observation-score separation

The scale chart uses the reciprocal of the rate in `Lomax.logLik`.
The two-level singleton and the exact three-level tie are available in
the imported modules. The measure-theoretic uniqueness bridge is separate.
-/

@[expose] public section

open Real
namespace UnivariateML.Shape3Lomax

/-- Two distinct positive observations distinguish Lomax coefficients,
and no constant exponential-face score agrees at both observations. -/
theorem score_separation {k s k' s' x y : ℝ} (hk : 0 < k)
    (hs : 0 < s) (hs' : 0 < s') (hx : 0 < x) (hxy : x < y) :
    ((-(1 + k) / (s + x) = -(1 + k') / (s' + x) ∧
      -(1 + k) / (s + y) = -(1 + k') / (s' + y)) → k = k' ∧ s = s') ∧
    (∀ c : ℝ, ¬ (-(1 + k) / (s + x) = c ∧ -(1 + k) / (s + y) = c)) := by
  have hy := hx.trans hxy
  constructor
  · rintro ⟨h₁, h₂⟩
    have h₁' := (div_eq_div_iff (add_pos hs hx).ne' (add_pos hs' hx).ne').mp h₁
    have h₂' := (div_eq_div_iff (add_pos hs hy).ne' (add_pos hs' hy).ne').mp h₂
    have hK : k = k' := by
      have h : (k - k') * (y - x) = 0 := by linear_combination h₁' - h₂'
      exact sub_eq_zero.mp ((mul_eq_zero.mp h).resolve_right (sub_pos.mpr hxy).ne')
    subst k'
    have h : (1 + k) * (s - s') = 0 := by linear_combination h₁'
    exact ⟨rfl, sub_eq_zero.mp ((mul_eq_zero.mp h).resolve_left (by positivity))⟩
  · rintro c ⟨h₁, h₂⟩
    have h := (div_eq_div_iff (add_pos hs hx).ne' (add_pos hs hy).ne').mp (h₁.trans h₂.symm)
    have h' : (1 + k) * (y - x) = 0 := by linear_combination -h
    exact (mul_pos (by positivity : 0 < 1 + k) (sub_pos.mpr hxy)).ne' h'

end UnivariateML.Shape3Lomax
end
