module

public import UnivariateML.Family.HeldSweepDoublePareto

set_option linter.style.header false

@[expose] public section

open Set Filter Topology Real
open UnivariateML.DoublePareto UnivariateML.HeldSweepDoublePareto

namespace UnivariateML.JoinsDoublePareto

private theorem finite_of_strict_face {f : ℝ → ℝ} {B : ℝ}
    (h : IsLUB (f '' Ioi 0) B) (hs : ∀ r > 0, f r < B) :
    ¬ ∃ r ∈ Ioi 0, IsMaxOn f (Ioi 0) r := by
  rintro ⟨r, hr, hm⟩
  exact (hs r hr).not_ge (h.2 (by
    rintro _ ⟨t, ht, rfl⟩
    exact hm ht))

/-- With kink and left rate held, the right-rate maximum is unique
when the right deviation is positive. At zero the power face is the
unattained supremum. -/
theorem held_left_rate {L R a : ℝ} (ha : 0 < a) (hR : 0 ≤ R) :
    ((∃ b ∈ Ioi (0 : ℝ), IsMaxOn (objective L R a) (Ioi 0) b) ↔ 0 < R) ∧
    (0 < R → ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (objective L R a) (Ioi 0) b) ∧
    (R = 0 → IsLUB (objective L R a '' Ioi 0) (log a - a * L)) := by
  refine ⟨⟨?_, fun h ↦ (held_left_rate_unique ha h).exists⟩,
    held_left_rate_unique ha, ?_⟩
  · intro h
    by_contra! hc
    have : R = 0 := le_antisymm hc hR
    subst R
    have hf := (held_left_rate_zero_deviation (L := L) ha).2
    exact finite_of_strict_face hf.1 hf.2 h
  · rintro rfl
    exact (held_left_rate_zero_deviation ha).2.1

/-- With kink and right rate held, the left-rate maximum is unique
when the left deviation is positive. At zero the Pareto face is the
unattained supremum. -/
theorem held_right_rate {L R b : ℝ} (hb : 0 < b) (hL : 0 ≤ L) :
    let f := fun a ↦ objective L R a b
    ((∃ a ∈ Ioi (0 : ℝ), IsMaxOn f (Ioi 0) a) ↔ 0 < L) ∧
    (0 < L → ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn f (Ioi 0) a) ∧
    (L = 0 → IsLUB (f '' Ioi 0) (log b - b * R)) := by
  intro f
  refine ⟨⟨?_, fun h ↦ (held_right_rate_unique hb h).exists⟩,
    held_right_rate_unique hb, ?_⟩
  · intro h
    by_contra! hc
    have : L = 0 := le_antisymm hc hL
    subst L
    have hf := (held_right_rate_zero_deviation (R := R) hb).2
    exact finite_of_strict_face hf.1 hf.2 h
  · rintro rfl
    exact (held_right_rate_zero_deviation hb).2.1

end UnivariateML.JoinsDoublePareto

end
