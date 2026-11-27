module

public import UnivariateML.Family.AE1GED
public import UnivariateML.Family.TwoPieceSGED

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Fixed-centre SGED at unit skew

Unit skew makes the join equal to the held mean. Its two kernel widths
coincide, giving the corresponding GED chart.

## Deviations

These results cover the
unit-skew subcases of the two held-mean claims.
-/

@[expose] public section

open Set Filter MeasureTheory Real

namespace UnivariateML.AE1SGED

noncomputable section

variable {n : ℕ}

/-- Unit-skew log likelihood in (shape, inverse total width) coordinates. -/
def unitSkewLogLik (w : Fin n → ℕ) (m : ℝ) (p : ℝ × ℝ) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (log p.2 - log (Gamma (1 + 1 / p.1)) -
    TwoPieceSGED.powerPenalty p.1 (1 / 2)
      ((x i - m) * p.2 + TwoPieceSGED.halfMoment p.1 * (1 - 2 * (1 / 2))))

private theorem symmetric_eq (w : Fin n → ℕ) (m a b : ℝ) (ha : 0 < a) (hb : 0 < b)
    (x : Fin n → ℝ) :
    unitSkewLogLik w m (a, 1 / (2 * b)) x = AE1GED.logLik w m (a, b) x := by
  unfold unitSkewLogLik AE1GED.logLik
  apply Finset.sum_congr rfl
  intro i _
  have hγ : 0 < Gamma (1 + 1 / a) := Gamma_pos_of_pos (by positivity)
  simp only [TwoPieceSGED.powerPenalty]
  have hs (z : ℝ) : (if z < 0 then (1 : ℝ) / 2 else 1 - 1 / 2) = 1 / 2 := by
    split <;> norm_num
  rw [hs]
  have hz : (1 : ℝ) - 2 * (1 / 2) = 0 := by norm_num
  simp only [hz, mul_zero, add_zero]
  rw [abs_mul, abs_of_pos (one_div_pos.mpr (mul_pos two_pos hb))]
  have hh : |x i - m| * (1 / (2 * b)) / (1 / 2) = |x i - m| / b := by
    field_simp [hb.ne']
  rw [hh, log_div one_ne_zero (mul_pos two_pos hb).ne', log_one, zero_sub,
    log_mul (mul_pos two_pos hb).ne' hγ.ne']
  ring

private def switchScale (p : ℝ × ℝ) : ℝ × ℝ := (p.1, 1 / (2 * p.2))

private theorem switch_mem {p : ℝ × ℝ} (hp : p ∈ Ici 1 ×ˢ Ioi 0) :
    switchScale p ∈ Ici 1 ×ˢ Ioi 0 :=
  ⟨hp.1, one_div_pos.mpr (mul_pos two_pos hp.2)⟩

private theorem switch_twice {p : ℝ × ℝ} (hp : p ∈ Ici 1 ×ˢ Ioi 0) :
    switchScale (switchScale p) = p := by
  refine Prod.ext ?_ ?_
  · rfl
  show 1 / (2 * (1 / (2 * p.2))) = p.2
  field_simp [(show 0 < p.2 from hp.2).ne']

/-- At unit skew and held mean, two free levels suffice for almost-everywhere
uniqueness of any attained fitted SGED shape and scale. -/
theorem held_mean_unit_skew_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (m : ℝ) (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ Ici 1 ×ˢ Ioi 0, ∀ q ∈ Ici 1 ×ˢ Ioi 0,
      IsMaxOn (fun r ↦ unitSkewLogLik w m r x) (Ici 1 ×ˢ Ioi 0) p →
      IsMaxOn (fun r ↦ unitSkewLogLik w m r x) (Ici 1 ×ˢ Ioi 0) q → p = q := by
  filter_upwards [AE1GED.held_mean_ae_unique w hw m hn] with x hx p hp q hq hmp hmq
  have hlik (r : ℝ × ℝ) (hr : r ∈ Ici 1 ×ˢ Ioi 0) :
      unitSkewLogLik w m (switchScale r) x = AE1GED.logLik w m r x :=
    symmetric_eq w m r.1 r.2 (zero_lt_one.trans_le hr.1) hr.2 x
  have hmax (r : ℝ × ℝ) (hr : r ∈ Ici 1 ×ˢ Ioi 0)
      (hm : IsMaxOn (fun t ↦ unitSkewLogLik w m t x) (Ici 1 ×ˢ Ioi 0) r) :
      IsMaxOn (fun t ↦ AE1GED.logLik w m t x) (Ici 1 ×ˢ Ioi 0) (switchScale r) := by
    intro t ht
    change AE1GED.logLik w m t x ≤ AE1GED.logLik w m (switchScale r) x
    rw [← hlik t ht, ← hlik (switchScale r) (switch_mem hr), switch_twice hr]
    exact hm (switch_mem ht)
  have h := congrArg switchScale (hx (switchScale p) (switchScale q)
    (switch_mem hp) (switch_mem hq) (hmax p hp hmp) (hmax q hq hmq))
  exact (switch_twice hp).symm.trans (h.trans (switch_twice hq))

/-- Unit-skew likelihood at held mean and standard deviation. -/
def unitSkewHeldSdLogLik (w : Fin n → ℕ) (m sd a : ℝ) (x : Fin n → ℝ) : ℝ :=
  unitSkewLogLik w m (a, 1 / (2 * (sd / Ged.sdFactor a))) x

/-- At unit skew with mean and standard deviation held, two free levels
suffice for almost-everywhere uniqueness of any attained fitted shape. -/
theorem held_mean_sd_unit_skew_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (m sd : ℝ) (hsd : 0 < sd) (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : ℝ, p ∈ Ici 1 → q ∈ Ici 1 →
      IsMaxOn (fun a ↦ unitSkewHeldSdLogLik w m sd a x) (Ici 1) p →
      IsMaxOn (fun a ↦ unitSkewHeldSdLogLik w m sd a x) (Ici 1) q → p = q := by
  filter_upwards [AE1GED.held_mean_sd_ae_unique w hw m sd hsd hn] with x hx p q hp hq hmp hmq
  have hlik (a : ℝ) (ha : a ∈ Ici 1) :
      unitSkewHeldSdLogLik w m sd a x = AE1GED.heldSdLogLik w m sd a x := by
    have ha' : 0 < a := zero_lt_one.trans_le ha
    have hb : 0 < sd / Ged.sdFactor a := by
      unfold Ged.sdFactor
      exact div_pos hsd (sqrt_pos_of_pos (div_pos (Gamma_pos_of_pos (by positivity))
        (Gamma_pos_of_pos (by positivity))))
    exact symmetric_eq w m a _ ha' hb x
  have pull {r : ℝ} (hr : r ∈ Ici 1)
      (hm : IsMaxOn (fun a ↦ unitSkewHeldSdLogLik w m sd a x) (Ici 1) r) :
      IsMaxOn (fun a ↦ AE1GED.heldSdLogLik w m sd a x) (Ici 1) r := by
    intro a ha
    change AE1GED.heldSdLogLik w m sd a x ≤ AE1GED.heldSdLogLik w m sd r x
    rw [← hlik a ha, ← hlik r hr]
    exact hm ha
  exact hx p q hp hq (pull hp hmp) (pull hq hmq)

end
end UnivariateML.AE1SGED

end
