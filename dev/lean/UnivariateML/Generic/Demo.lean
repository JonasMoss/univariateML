module

public import UnivariateML.Family.Lomax
public import UnivariateML.Generic.Score

set_option linter.style.header false

/-! # Held-shape Lomax via the generic log-score theorem -/

@[expose] public section

open Set Real

namespace UnivariateML.Generic.Demo

/-- Holding the Lomax shape fixed gives a unique maximizing positive rate. -/
theorem heldShape_unique {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {κ : ℝ} (hk : 0 < κ) :
    ∃! rate, rate ∈ Ioi (0 : ℝ) ∧ IsMaxOn (Lomax.logLik x κ) (Ioi 0) rate := by
  obtain ⟨hd, hs⟩ := Lomax.heldShape_score_strictAnti hn hx hk
  obtain ⟨hl, hr⟩ := Lomax.heldShape_score_limits hx κ
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  have hc : Continuous (Lomax.logRateScore x κ) := by
    have hsum : Continuous (fun t : ℝ ↦ ∑ i, (1 - (1 + exp t * x i)⁻¹)) := by
      refine continuous_finsetSum Finset.univ (fun i _ ↦ ?_)
      exact continuous_const.sub ((continuous_const.add (continuous_exp.mul continuous_const)).inv₀
        (fun t ↦ (add_pos zero_lt_one (mul_pos (exp_pos t) (hx i))).ne'))
    exact continuous_const.sub (continuous_const.mul hsum)
  obtain ⟨⟨rate, ⟨hpos, hz⟩, huniq⟩, hmax⟩ := existsUnique_log_score_zero hd hc hs hnR
    (mul_neg_of_neg_of_pos (neg_neg_of_pos hnR) hk) hl hr
  exact ⟨rate, ⟨hpos, (hmax rate hpos).2 hz⟩, fun r hm ↦
    huniq r ⟨hm.1, (hmax r hm.1).1 hm.2⟩⟩

end UnivariateML.Generic.Demo
