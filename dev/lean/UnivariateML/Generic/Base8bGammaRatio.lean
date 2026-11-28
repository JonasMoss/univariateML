module

public import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Base8bGammaRatio

private noncomputable def gammaRatio (b a : ℝ) := Gamma (a+b) / Gamma a

private theorem gammaRatio_bounds {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (max (a-1) 0)^b ≤ gammaRatio b a ∧ gammaRatio b a ≤ (a+b)^b := by
  let G : ℝ → ℝ := fun x ↦ log (Gamma x)
  have cv : ConvexOn ℝ (Ioi 0) G := Real.convexOn_log_Gamma
  have hab : 0 < a+b := add_pos ha hb
  have hstep (x : ℝ) (hx : 0 < x) : G (x+1) = G x + log x := by
    dsimp [G]
    rw [Gamma_add_one hx.ne', log_mul hx.ne' (Gamma_pos_of_pos hx).ne', add_comm]
  have hQ : 0 < gammaRatio b a :=
    div_pos (Gamma_pos_of_pos hab) (Gamma_pos_of_pos ha)
  have hlog : log (gammaRatio b a) = G (a+b) - G a :=
    log_div (Gamma_pos_of_pos hab).ne' (Gamma_pos_of_pos ha).ne'
  constructor
  · by_cases h : 1 < a
    · have ha1 := sub_pos.mpr h
      rw [max_eq_left ha1.le, ← log_le_log_iff (rpow_pos_of_pos ha1 b) hQ,
        log_rpow ha1, hlog]
      have H := cv.slope_mono_adjacent (x := a-1) (y := a) (z := a+b)
        ha1 hab (by linarith) (lt_add_of_pos_right _ hb)
      rw [show a-(a-1) = 1 by ring, div_one,
        show a+b-a = b by ring] at H
      have step := hstep (a-1) ha1
      rw [sub_add_cancel] at step
      have HH := (le_div_iff₀ hb).mp H
      rw [step] at HH ⊢
      linarith only [HH]
    · rw [max_eq_right (sub_nonpos.mpr (le_of_not_gt h)), zero_rpow hb.ne']
      exact hQ.le
  · rw [← log_le_log_iff hQ (rpow_pos_of_pos hab b), log_rpow hab, hlog]
    have H := cv.slope_mono_adjacent (x := a) (y := a+b) (z := a+b+1)
      ha (by positivity : 0 < a+b+1) (lt_add_of_pos_right _ hb) (by linarith)
    rw [show a+b-a = b by ring,
      show a+b+1-(a+b) = 1 by ring, div_one, hstep _ hab] at H
    have HH := (div_le_iff₀ hb).mp (show (G (a+b)-G a)/b ≤ log (a+b) by
      linarith only [H])
    linarith

private theorem scaled_limits {ι : Type*} {l : Filter ι} {a T : ι → ℝ} {b p : ℝ}
    (hb : 0 < b) (hp : 0 ≤ p) (ha : ∀ᶠ t in l, 0 < a t)
    (hT : Tendsto T l atTop)
    (hr : Tendsto (fun t ↦ a t / T t) l (𝓝 p)) :
    Tendsto (fun t ↦ gammaRatio b (a t) / gammaRatio b (T t)) l (𝓝 (p^b)) := by
  have hi := tendsto_inv_atTop_zero.comp hT
  have hc (c : ℝ) : Tendsto (fun t ↦ (a t+c)/T t) l (𝓝 p) := by
    simpa [div_eq_mul_inv, add_mul] using hr.add (hi.const_mul c)
  have hd (c : ℝ) : Tendsto (fun t ↦ 1+c*(T t)⁻¹) l (𝓝 1) := by
    simpa using (hi.const_mul c).const_add 1
  have hNum : Tendsto (fun t ↦ max ((a t-1)/T t) 0) l (𝓝 p) := by
    simpa [max_eq_left hp, sub_eq_add_neg] using (hc (-1)).max (tendsto_const_nhds (x := (0 : ℝ)))
  have hLo0 := hNum.div (hd b) one_ne_zero
  have hHi0 := (hc b).div (hd (-1)) one_ne_zero
  have hpow : ContinuousAt (fun x : ℝ ↦ x^b) p :=
    (continuousAt_rpow_const p b (Or.inr hb.le))
  rw [div_one] at hLo0 hHi0
  have low := hpow.tendsto.comp hLo0
  have high := hpow.tendsto.comp hHi0
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' low high
  · filter_upwards [ha, hT.eventually_gt_atTop 1] with t hat ht
    have ht0 : 0 < T t := zero_lt_one.trans ht
    have hs : 0 < T t+b := add_pos ht0 hb
    have hG : 0 < gammaRatio b (T t) :=
      div_pos (Gamma_pos_of_pos hs) (Gamma_pos_of_pos ht0)
    have hD := ((gammaRatio_bounds ht0 hb).2).trans_eq (rfl : (T t+b)^b = _)
    have eq : max ((a t-1)/T t) 0 / (1+b*(T t)⁻¹) =
        max (a t-1) 0 / (T t+b) := by
      rw [← show max (a t-1) 0 / T t = max ((a t-1)/T t) 0 by
        simp [← max_div_div_right ht0.le]]
      field_simp
    change (max ((a t-1)/T t) 0 / (1+b*(T t)⁻¹))^b ≤ _
    rw [eq, div_rpow (le_max_right _ _) hs.le]
    apply div_le_div₀
    · exact (div_pos (Gamma_pos_of_pos (add_pos hat hb)) (Gamma_pos_of_pos hat)).le
    · exact (gammaRatio_bounds hat hb).1
    · exact hG
    · exact hD
  · filter_upwards [ha, hT.eventually_gt_atTop 1] with t hat ht
    have ht0 : 0 < T t := zero_lt_one.trans ht
    have hs : 0 < T t-1 := sub_pos.mpr ht
    have eq : ((a t+b)/T t) / (1+(-1)*(T t)⁻¹) = (a t+b)/(T t-1) := by
      have he : 1+(-1)*(T t)⁻¹ = (T t-1)/T t := by
        field_simp [ht0.ne']
        ring
      rw [he]
      field_simp [ht0.ne', hs.ne']
    change _ ≤ (((a t+b)/T t) / (1+(-1)*(T t)⁻¹))^b
    rw [eq, div_rpow (add_pos hat hb).le hs.le]
    apply div_le_div₀
    · positivity
    · exact (gammaRatio_bounds hat hb).2
    · exact rpow_pos_of_pos hs b
    · simpa [max_eq_left hs.le] using (gammaRatio_bounds ht0 hb).1

/-- For a fixed positive gamma increment, the quotient of the two
gamma ratios converges to the corresponding power of the limiting ratio.
The smaller gamma argument may remain bounded when that ratio is zero. -/
theorem scaled_limit {ι : Type*} {l : Filter ι} {a T : ι → ℝ} {b p : ℝ}
    (hb : 0 < b) (hp : 0 ≤ p) (ha : ∀ᶠ t in l, 0 < a t)
    (hT : Tendsto T l atTop)
    (hr : Tendsto (fun t ↦ a t / T t) l (𝓝 p)) :
    Tendsto (fun t ↦ (Gamma (a t+b)/Gamma (a t)) /
      (Gamma (T t+b)/Gamma (T t))) l (𝓝 (p^b)) :=
  scaled_limits hb hp ha hT hr

end UnivariateML.Base8bGammaRatio

end
