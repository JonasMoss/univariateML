module

public import UnivariateML.Family.HeldSweepBNBinom

import Mathlib.Tactic

set_option linter.style.header false

/-!
# A diagonal exclusion at held beta-negative-binomial alpha

The two-count example uses exact integer comparisons on closed intervals.
The diagonal envelope decreases after the last checked interval.
Separate bounds on the escaping parameters prove attainment for the
explicit likelihood.

## Deviations

The example uses two observations and a different off-diagonal witness
from the dossier's golden sample. The theorem uses the explicit mass
formula. The imported log-mass body is opaque in the public API, so its
identification with this formula remains separate.
-/

@[expose] public section

namespace UnivariateML.TiesBNBinom

open Set Real

noncomputable section

private def zeroMass (r b : ℝ) : ℝ := Gamma (2 + r) * Gamma (2 + b) / Gamma (2 + r + b)

private def rise (s : ℝ) (k : ℕ) : ℝ := ∏ j ∈ Finset.range k, (s + j)

private def ratio (r b : ℝ) (k : ℕ) : ℝ :=
  rise r k * rise b k / (rise (2 + r + b) k * k.factorial)

private def mass (r b : ℝ) (k : ℕ) : ℝ := zeroMass r b * ratio r b k

private theorem gamma_two : Gamma (2 : ℝ) = 1 := by
  simp

private theorem zeroMass_pos {r b : ℝ} (hr : 0 ≤ r) (hb : 0 ≤ b) : 0 < zeroMass r b := by
  unfold zeroMass
  exact div_pos (mul_pos (Gamma_pos_of_pos (by positivity)) (Gamma_pos_of_pos (by positivity)))
    (Gamma_pos_of_pos (by positivity))

private theorem rise_pos {s : ℝ} (hs : 0 < s) (k : ℕ) : 0 < rise s k := by
  exact Finset.prod_pos (fun j _ ↦ by positivity)

private theorem ratio_pos {r b : ℝ} (hr : 0 < r) (hb : 0 < b) (k : ℕ) :
    0 < ratio r b k := by
  unfold ratio
  exact div_pos (mul_pos (rise_pos hr k) (rise_pos hb k))
    (mul_pos (rise_pos (by positivity) k) (by positivity))

private theorem mass_zero (r b : ℝ) : mass r b 0 = zeroMass r b := by
  simp [mass, ratio, rise]

private theorem rise_succ (s : ℝ) (k : ℕ) :
    rise s (k + 1) = rise s k * (s + k) := by
  unfold rise
  rw [Finset.prod_range_succ]

private theorem Gamma_rise {s : ℝ} (hs : 0 < s) (k : ℕ) :
    Gamma (s + (k : ℝ)) = Gamma s * rise s k := by
  induction k with
  | zero => simp [rise]
  | succ k ih =>
    simp only [Nat.cast_succ, ← add_assoc]
    rw [Gamma_add_one (by positivity), ih, rise_succ]
    ring

private theorem mass_eq_gamma {r b : ℝ} (hr : 0 < r) (hb : 0 < b) (k : ℕ) :
    mass r b k =
      (Gamma (2 + r) * Gamma (2 + b) * Gamma (r + k) * Gamma (b + k)) /
      (Gamma (2 + r + b + k) * Gamma r * Gamma b * k.factorial) := by
  rw [Gamma_rise hr, Gamma_rise hb, Gamma_rise (show 0 < 2 + r + b by positivity)]
  have h₁ := (Gamma_pos_of_pos hr).ne'
  have h₂ := (Gamma_pos_of_pos hb).ne'
  have h₃ := (Gamma_pos_of_pos (show 0 < 2 + r + b by positivity)).ne'
  have h₄ := (rise_pos (show 0 < 2 + r + b by positivity) k).ne'
  have h₅ : (k.factorial : ℝ) ≠ 0 := by positivity
  unfold mass zeroMass ratio
  field_simp [h₁, h₂, h₃, h₄, h₅]

private def logG (s : ℝ) := log (Gamma s)

private theorem logG_diff {s : ℝ} (hs : 0 < s) : DifferentiableAt ℝ logG s := by
  exact (Real.differentiableAt_Gamma (fun j ↦ by
    have : 0 ≤ (j : ℝ) := by positivity
    linarith only [hs, this])).log (Gamma_pos_of_pos hs).ne'

private theorem zeroMass_swap (a b : ℝ) : zeroMass a b = zeroMass b a := by
  unfold zeroMass
  rw [mul_comm, show 2 + a + b = 2 + b + a by ring]

private theorem zeroMass_antitone {b : ℝ} (hb : 0 ≤ b) :
    AntitoneOn (fun r ↦ zeroMass r b) (Ici 0) := by
  let f (r : ℝ) := logG (2 + r) + logG (2 + b) - logG (2 + r + b)
  have hd {r : ℝ} (hr : 0 ≤ r) :
      HasDerivAt f (deriv logG (2 + r) - deriv logG (2 + r + b)) r := by
    have ha : HasDerivAt (fun t : ℝ ↦ 2 + t + b) 1 r :=
      ((hasDerivAt_id r).const_add 2).add_const b
    have h₁ := (logG_diff (s := 2 + r) (by positivity)).hasDerivAt.comp r
      ((hasDerivAt_id r).const_add 2)
    have hg : HasDerivAt logG (deriv logG (2 + r + b)) ((fun t : ℝ ↦ 2 + t + b) r) :=
      (logG_diff (s := 2 + r + b) (by positivity)).hasDerivAt
    have h₂ := hg.comp (h := fun t : ℝ ↦ 2 + t + b) r ha
    convert (h₁.add_const (logG (2 + b))).sub h₂ using 1
    · rfl
    · simp
  have hm : AntitoneOn f (Ici 0) := by
    apply antitoneOn_of_deriv_nonpos (convex_Ici _)
      (fun r hr ↦ (hd hr).continuousAt.continuousWithinAt)
      (fun _ hr ↦ (hd (interior_subset hr)).differentiableAt.differentiableWithinAt)
    intro r hr
    have hr' : 0 ≤ r := interior_subset hr
    have h := Real.convexOn_log_Gamma.monotoneOn_deriv
      (fun x hx ↦ logG_diff hx)
      (show 2 + r ∈ Ioi (0 : ℝ) by change 0 < _; positivity)
      (show 2 + r + b ∈ Ioi (0 : ℝ) by change 0 < _; positivity)
      (le_add_of_nonneg_right hb)
    rw [(hd hr').deriv]
    exact sub_nonpos.mpr h
  have he {r : ℝ} (hr : 0 ≤ r) : log (zeroMass r b) = f r := by
    have h₁ := (Gamma_pos_of_pos (show 0 < 2 + r by positivity)).ne'
    have h₂ := (Gamma_pos_of_pos (show 0 < 2 + b by positivity)).ne'
    have h₃ := (Gamma_pos_of_pos (show 0 < 2 + r + b by positivity)).ne'
    unfold zeroMass f logG
    rw [log_div (mul_ne_zero h₁ h₂) h₃, log_mul h₁ h₂]
  intro r hr s hs hrs
  apply (log_le_log_iff (zeroMass_pos hs hb) (zeroMass_pos hr hb)).mp
  rw [he hs, he hr]
  exact hm hr hs hrs

private theorem diag_zero_antitone : AntitoneOn (fun s ↦ zeroMass s s) (Ici 0) := by
  intro u hu v hv huv
  change zeroMass v v ≤ zeroMass u u
  calc
    _ ≤ zeroMass u v := zeroMass_antitone hv hu hv huv
    _ = zeroMass v u := zeroMass_swap u v
    _ ≤ _ := zeroMass_antitone hu hu hv huv

private theorem ratio_prod (s : ℝ) (k : ℕ) :
    ratio s s k = ∏ j ∈ Finset.range k, ((s + j) ^ 2 / ((2 + s + s + j) * (j + 1))) := by
  rw [Finset.prod_div_distrib, Finset.prod_mul_distrib]
  unfold ratio rise
  rw [← Finset.prod_mul_distrib, Nat.factorial_eq_prod_range_add_one, Nat.cast_prod]
  simp only [pow_two, Nat.cast_add, Nat.cast_one]

private theorem diag_ratio_monotone (k : ℕ) : MonotoneOn (fun s ↦ ratio s s k) (Ici 0) := by
  intro u hu v hv huv
  change ratio u u k ≤ ratio v v k
  rw [ratio_prod, ratio_prod]
  have hu' : 0 ≤ u := hu
  have hv' : 0 ≤ v := hv
  apply Finset.prod_le_prod₀
  · intro j _
    positivity
  · intro j _
    have h₁ : 0 < (2 + u + u + j) * ((j : ℝ) + 1) := by positivity
    have h₂ : 0 < (2 + v + v + j) * ((j : ℝ) + 1) := by positivity
    apply (div_le_div_iff₀ h₁ h₂).mpr
    have hbase : (u + (j : ℝ)) ^ 2 * (2 + v + v + j) ≤
        (v + (j : ℝ)) ^ 2 * (2 + u + u + j) := by
      have hnon : 0 ≤ (v - u) * (2 * u * v + (2 + j) * (u + v) + 4 * j) := by
        exact mul_nonneg (sub_nonneg.mpr huv) (by positivity)
      linear_combination hnon
    linear_combination ((j : ℝ) + 1) * hbase

private def envelope (m : ℝ) : ℝ := zeroMass m m ^ 2 * ratio (m + 1) (m + 1) 200

private theorem diag_product_le_envelope {d m : ℝ} (hd : 0 < d)
    (hm : 0 ≤ m) (hlo : m ≤ d) (hhi : d ≤ m + 1) :
    mass d d 0 * mass d d 200 ≤ envelope m := by
  have hz := diag_zero_antitone hm hd.le hlo
  have hr := diag_ratio_monotone 200 hd.le (show 0 ≤ m + 1 by positivity) hhi
  rw [mass_zero]
  unfold mass envelope
  rw [← mul_assoc, ← pow_two]
  exact mul_le_mul (pow_le_pow_left₀ (zeroMass_pos hd.le hd.le).le hz 2) hr
    (ratio_pos hd hd 200).le (sq_nonneg _)

private theorem rise_shift (s : ℝ) (k : ℕ) :
    rise (s + 1) k * s = rise s k * (s + k) := by
  induction k with
  | zero => simp [rise]
  | succ k ih =>
    rw [rise_succ, rise_succ, Nat.cast_succ]
    calc
      rise (s + 1) k * (s + 1 + (k : ℝ)) * s =
          rise (s + 1) k * s * (s + 1 + (k : ℝ)) := by ring
      _ = _ := by rw [ih]; ring

private theorem rise_shift_eq {s : ℝ} (hs : 0 < s) (k : ℕ) :
    rise (s + 1) k = rise s k * (s + k) / s :=
  (eq_div_iff hs.ne').mpr (rise_shift s k)

private theorem ratio_shift_le {s : ℝ} (hs : 0 < s) (k : ℕ) :
    ratio (s + 1) (s + 1) k ≤ ratio s s k * ((2 + s + s) * (3 + s + s) / s ^ 2) := by
  let u := 2 + s + s
  have hu : 0 < u := by dsimp [u]; positivity
  have hre : 2 + (s + 1) + (s + 1) = (u + 1) + 1 := by dsimp [u]; ring
  have hden : rise (2 + (s + 1) + (s + 1)) k =
      rise u k * (u + k) * (u + 1 + k) / (u * (u + 1)) := by
    rw [hre, rise_shift_eq (by positivity : 0 < u + 1), rise_shift_eq hu]
    field_simp [hu.ne', show u + 1 ≠ 0 by positivity]
  have hR : ratio (s + 1) (s + 1) k =
      ratio s s k * (u * (u + 1) / s ^ 2) *
        ((s + k) ^ 2 / ((u + k) * (u + 1 + k))) := by
    unfold ratio
    rw [hden, rise_shift_eq hs]
    change _ = (rise s k * rise s k / (rise u k * (k.factorial : ℝ))) *
      (u * (u + 1) / s ^ 2) * ((s + k) ^ 2 / ((u + k) * (u + 1 + k)))
    field_simp [hs.ne', hu.ne', (rise_pos hu k).ne', show (u + k) ≠ 0 by positivity,
      show (u + 1 + k) ≠ 0 by positivity, show u + 1 ≠ 0 by positivity,
      show (k.factorial : ℝ) ≠ 0 by positivity]
  have hfrac : (s + k) ^ 2 / ((u + k) * (u + 1 + k)) ≤ 1 := by
    apply (div_le_iff₀ (by positivity)).mpr
    rw [one_mul, pow_two]
    have hk : 0 ≤ (k : ℝ) := by positivity
    have hu₁ : s + (k : ℝ) ≤ u + k := by dsimp [u]; linarith only [hs]
    exact mul_le_mul hu₁ (by linarith only [hu₁]) (by positivity) (by positivity)
  rw [hR, show 3 + s + s = u + 1 by dsimp [u]; ring]
  exact mul_le_of_le_one_right
    (by have := ratio_pos hs hs k; positivity) hfrac

private theorem zeroMass_shift {s : ℝ} (hs : 0 ≤ s) :
    zeroMass (s + 1) (s + 1) =
      zeroMass s s * ((s + 2) ^ 2 / ((2 + s + s) * (3 + s + s))) := by
  have he : 2 + (s + 1) + (s + 1) = ((2 + s + s) + 1) + 1 := by ring
  have hG := Gamma_add_one (s := 2 + s) (by positivity)
  unfold zeroMass
  rw [he, Gamma_add_one (by positivity), Gamma_add_one (by positivity),
    show 2 + (s + 1) = (2 + s) + 1 by ring, hG]
  field_simp [(Gamma_pos_of_pos (show 0 < 2 + s + s by positivity)).ne',
    show 2 + s + s ≠ 0 by positivity, show 3 + s + s ≠ 0 by positivity,
    show 2 + s + s + 1 ≠ 0 by positivity]
  ring

private theorem envelope_step {s : ℝ} (hs : 4 ≤ s) : envelope (s + 1) ≤ envelope s := by
  have hs₀ : 0 ≤ s := by linarith only [hs]
  let u := 2 + s + s
  let v := 3 + s + s
  have hu : 0 < u := by dsimp [u]; positivity
  have hv : 0 < v := by dsimp [v]; positivity
  have hp : 0 < s + 1 := by positivity
  have hratio := ratio_shift_le hp 200
  have he₁ : 2 + (s + 1) + (s + 1) = u + 2 := by dsimp [u]; ring
  have he₂ : 3 + (s + 1) + (s + 1) = v + 2 := by dsimp [v]; ring
  rw [he₁, he₂] at hratio
  have hfactor : ((s + 2) ^ 2 / (u * v)) ^ 2 *
      ((u + 2) * (v + 2) / (s + 1) ^ 2) ≤ 1 := by
    have hn : (s + 2) ^ 4 * (u + 2) * (v + 2) ≤ (u * v) ^ 2 * (s + 1) ^ 2 := by
      let x := s - 4
      have hx : 0 ≤ x := sub_nonneg.mpr hs
      have h := show 0 ≤ 12 * x ^ 6 + 350 * x ^ 5 + 4184 * x ^ 4 + 26080 * x ^ 3 +
        88520 * x ^ 2 + 152416 * x + 100324 by positivity
      dsimp [x, u, v] at h ⊢
      linear_combination h
    rw [div_pow, ← mul_div_mul_comm, div_le_iff₀ (by positivity), one_mul]
    linear_combination hn
  unfold envelope
  rw [zeroMass_shift hs₀]
  change (zeroMass s s * ((s + 2) ^ 2 / (u * v))) ^ 2 *
      ratio (s + 1 + 1) (s + 1 + 1) 200 ≤ _
  calc
    _ ≤ (zeroMass s s * ((s + 2) ^ 2 / (u * v))) ^ 2 *
        (ratio (s + 1) (s + 1) 200 * ((u + 2) * (v + 2) / (s + 1) ^ 2)) :=
      mul_le_mul_of_nonneg_left hratio (sq_nonneg _)
    _ = (zeroMass s s ^ 2 * ratio (s + 1) (s + 1) 200) *
        (((s + 2) ^ 2 / (u * v)) ^ 2 * ((u + 2) * (v + 2) / (s + 1) ^ 2)) := by ring
    _ ≤ _ := mul_le_of_le_one_right (mul_nonneg (sq_nonneg _) (ratio_pos hp hp 200).le) hfactor

private theorem Gamma_nat (n : ℕ) : Gamma ((n : ℝ) + 1) = n.factorial :=
  Real.Gamma_nat_eq_factorial n

private theorem envelope_nat (m : ℕ) :
    envelope m = (((m + 1).factorial : ℝ) ^ 4 * ((m + 200).factorial : ℝ) ^ 2 *
        ((2 * m + 3).factorial : ℝ)) /
      (((2 * m + 1).factorial : ℝ) ^ 2 * (m.factorial : ℝ) ^ 2 *
        ((2 * m + 203).factorial : ℝ) * ((200 : ℕ).factorial : ℝ)) := by
  have hr {n k : ℕ} :
      rise ((n : ℝ) + 1) k = ((n + k).factorial : ℝ) / n.factorial := by
    have h := Gamma_rise (s := (n : ℝ) + 1) (by positivity) k
    rw [show (n : ℝ) + 1 + k = ((n + k : ℕ) : ℝ) + 1 by push_cast; ring,
      Gamma_nat, Gamma_nat] at h
    rw [eq_div_iff (by positivity)]
    linear_combination -h
  unfold envelope zeroMass ratio
  rw [show 2 + (m : ℝ) + m = ((2 * m + 1 : ℕ) : ℝ) + 1 by push_cast; ring,
    show 2 + (m : ℝ) = ((m + 1 : ℕ) : ℝ) + 1 by push_cast; ring,
    show 2 + ((m : ℝ) + 1) + ((m : ℝ) + 1) = ((2 * m + 3 : ℕ) : ℝ) + 1 by
      push_cast; ring, Gamma_nat, Gamma_nat, hr, hr]
  have h : 2 * m + 3 + 200 = 2 * m + 203 := by omega
  rw [h]
  field_simp [show ((m + 1).factorial : ℝ) ≠ 0 by positivity,
    show ((2 * m + 1).factorial : ℝ) ≠ 0 by positivity,
    show ((2 * m + 3).factorial : ℝ) ≠ 0 by positivity,
    show ((2 * m + 203).factorial : ℝ) ≠ 0 by positivity,
    show (m.factorial : ℝ) ≠ 0 by positivity, show ((200 : ℕ).factorial : ℝ) ≠ 0 by positivity]

private theorem envelope_small_iff (m : ℕ) :
    envelope m < 1 / 70000 ↔
      70000 * (m + 1).factorial ^ 4 * (m + 200).factorial ^ 2 * (2 * m + 3).factorial <
        (2 * m + 1).factorial ^ 2 * m.factorial ^ 2 * (2 * m + 203).factorial *
          (200 : ℕ).factorial := by
  rw [envelope_nat, div_lt_div_iff₀ (by positivity) (by norm_num)]
  rw [one_mul]
  rw [show ((m + 1).factorial : ℝ) ^ 4 * ((m + 200).factorial : ℝ) ^ 2 *
      ((2 * m + 3).factorial : ℝ) * 70000 =
        70000 * ((m + 1).factorial : ℝ) ^ 4 * ((m + 200).factorial : ℝ) ^ 2 *
           ((2 * m + 3).factorial : ℝ) by ring]
  norm_cast

set_option maxRecDepth 4000 in
set_option maxHeartbeats 1000000 in
-- Kernel reduction checks five integer inequalities with factorials above 200.
private theorem envelope_small (m : Fin 5) : envelope m.val < 1 / 70000 := by
  apply (envelope_small_iff m.val).mpr
  fin_cases m <;> decide

private theorem envelope_all (m : ℕ) : envelope m < 1 / 70000 := by
  induction m with
  | zero => exact envelope_small 0
  | succ m ih =>
    by_cases h : 4 ≤ m
    · exact (by exact_mod_cast envelope_step (s := m) (by exact_mod_cast h) :
        envelope ((m + 1 : ℕ) : ℝ) ≤ envelope (m : ℝ)).trans_lt ih
    · exact envelope_small ⟨m + 1, by omega⟩

private theorem mass_one {b : ℝ} (hb : 0 < b) (k : ℕ) :
    mass 1 b k = 2 * b * (b + 1) / ((b + k) * (b + k + 1) * (b + k + 2)) := by
  rw [mass_eq_gamma (by norm_num) hb, show (2 : ℝ) + 1 = 3 by norm_num,
    show Gamma (3 : ℝ) = 2 by simp,
    add_comm (1 : ℝ) (k : ℝ), Gamma_nat, Gamma_one]
  rw [show 2 + b = (b + 1) + 1 by ring, Gamma_add_one (by positivity),
    Gamma_add_one hb.ne']
  rw [show (3 : ℝ) + b + k = (((b + k) + 1) + 1) + 1 by ring,
    Gamma_add_one (by positivity), Gamma_add_one (by positivity), Gamma_add_one (by positivity)]
  field_simp [(Gamma_pos_of_pos hb).ne',
    (Gamma_pos_of_pos (show 0 < b + (k : ℝ) by positivity)).ne',
    show b + (k : ℝ) ≠ 0 by positivity, show b + (k : ℝ) + 1 ≠ 0 by positivity,
    show b + (k : ℝ) + 2 ≠ 0 by positivity, show (k.factorial : ℝ) ≠ 0 by positivity]
  ring

/-- For observations 0 and 200 at held alpha 2, the shapes (1, 100) strictly
beat every diagonal shape in the explicit product of beta-negative-binomial
probabilities. -/
theorem explicit_diagonal_exclusion :
    let p (r b : ℝ) (k : ℕ) :=
      (Gamma (2 + r) * Gamma (2 + b) * Gamma (r + k) * Gamma (b + k)) /
        (Gamma (2 + r + b + k) * Gamma r * Gamma b * k.factorial)
    ∀ d > 0, p d d 0 * p d d 200 < p 1 100 0 * p 1 100 200 := by
  dsimp only
  intro d hd
  rw [← mass_eq_gamma hd hd, ← mass_eq_gamma hd hd,
    ← mass_eq_gamma (by norm_num : (0 : ℝ) < 1) (by norm_num : (0 : ℝ) < 100),
    ← mass_eq_gamma (by norm_num : (0 : ℝ) < 1) (by norm_num : (0 : ℝ) < 100)]
  have h := diag_product_le_envelope hd (by positivity : (0 : ℝ) ≤ (⌊d⌋₊ : ℝ))
    (Nat.floor_le hd.le) (Nat.lt_floor_add_one d).le
  exact (h.trans_lt (envelope_all ⌊d⌋₊)).trans (by
    rw [mass_one (by norm_num), mass_one (by norm_num)]
    norm_num)

private theorem product_witness :
    (1 / 70000 : ℝ) < mass 1 100 0 * mass 1 100 200 := by
  rw [mass_one (by norm_num), mass_one (by norm_num)]
  norm_num

private theorem zeroMass_le_one {r b : ℝ} (hr : 0 ≤ r) (hb : 0 ≤ b) : zeroMass r b ≤ 1 := by
  have h := zeroMass_antitone hb (show (0 : ℝ) ∈ Ici 0 from le_rfl) hr hr
  have h0 : zeroMass 0 b = 1 := by
    unfold zeroMass
    rw [add_zero, gamma_two, one_mul, div_self (Gamma_pos_of_pos (by positivity)).ne']
  exact h.trans_eq h0

private theorem logG_succ {s : ℝ} (hs : 0 < s) : logG (s + 1) = log s + logG s := by
  unfold logG
  rw [Gamma_add_one hs.ne', log_mul hs.ne' (Gamma_pos_of_pos hs).ne']

private theorem zeroMass_decay {r b : ℝ} (hr : 0 ≤ r) (hb : 0 < b) :
    zeroMass r b ≤ exp (logG (2 + b) - b * log (1 + r)) := by
  have hlo : 1 + r ∈ Ioi (0 : ℝ) := by change 0 < _; positivity
  have hmid : 2 + r ∈ Ioi (0 : ℝ) := by change 0 < _; positivity
  have hhi : 2 + r + b ∈ Ioi (0 : ℝ) := by change 0 < _; positivity
  have hc : ConvexOn ℝ (Ioi 0) logG := Real.convexOn_log_Gamma
  have h₁ := hc.slope_le_deriv hlo hmid (by linarith : 1 + r < 2 + r) (logG_diff hmid)
  have h₂ := hc.deriv_le_slope hmid hhi (lt_add_of_pos_right _ hb) (logG_diff hmid)
  rw [slope_def_field, show 2 + r + b - (2 + r) = b by ring] at h₂
  rw [slope_def_field, show 2 + r - (1 + r) = 1 by ring, div_one] at h₁
  have hG : logG (2 + r) = log (1 + r) + logG (1 + r) := by
    rw [show 2 + r = (1 + r) + 1 by ring, logG_succ hlo]
  rw [hG, add_sub_cancel_right] at h₁
  have h := (le_div_iff₀ hb).mp (h₁.trans h₂)
  apply (log_le_log_iff (zeroMass_pos hr hb.le) (exp_pos _)).mp
  rw [log_exp, zeroMass, log_div (mul_ne_zero
    (Gamma_pos_of_pos hmid).ne' (Gamma_pos_of_pos (by positivity)).ne')
      (Gamma_pos_of_pos hhi).ne', log_mul (Gamma_pos_of_pos hmid).ne'
        (Gamma_pos_of_pos (by positivity)).ne']
  change logG (2 + r) + logG (2 + b) - logG (2 + r + b) ≤ _
  linarith only [h]

private theorem rise_mono {s t : ℝ} (hs : 0 ≤ s) (hst : s ≤ t) (k : ℕ) :
    rise s k ≤ rise t k := by
  unfold rise
  exact Finset.prod_le_prod₀ (fun _ _ ↦ by positivity) (fun _ _ ↦ add_le_add_left hst _)

private theorem ratio_le_rise {r b : ℝ} (hr : 0 < r) (hb : 0 < b) (k : ℕ) :
    ratio r b k ≤ rise r k / k.factorial := by
  have h := rise_mono hb.le (show b ≤ 2 + r + b by linarith only [hr]) k
  have hs := rise_pos (show 0 < 2 + r + b by positivity) k
  unfold ratio
  have hstep := (div_le_div_of_nonneg_right (mul_le_mul_of_nonneg_left h (rise_pos hr k).le)
    (by positivity : 0 ≤ rise (2 + r + b) k * (k.factorial : ℝ)))
  exact hstep.trans_eq (by
    field_simp [hs.ne', show (k.factorial : ℝ) ≠ 0 by positivity])

private theorem ratio_le_power {r b : ℝ} (hr : 0 < r) (hb : 0 < b) :
    ratio r b 200 ≤ (r + 200) ^ 200 := by
  have h : rise r 200 ≤ (r + 200) ^ 200 := by
    unfold rise
    calc
      _ ≤ (∏ _ ∈ Finset.range 200, (r + 200)) := Finset.prod_le_prod₀ (fun _ _ ↦ by positivity)
        (fun j hj ↦ add_le_add_right
          (show (j : ℝ) ≤ 200 from Nat.cast_le.mpr (Nat.le_of_lt (Finset.mem_range.mp hj))) r)
      _ = _ := by simp
  have hf : (1 : ℝ) ≤ ((200 : ℕ).factorial : ℝ) := by exact_mod_cast Nat.factorial_pos 200
  exact (ratio_le_rise hr hb 200).trans ((div_le_self (rise_pos hr 200).le hf).trans h)

private theorem ratio_le_small {r b : ℝ} (hr : 0 < r) (hb : 0 < b) (hr' : r ≤ 1) :
    ratio r b 200 ≤ r := by
  have h := ratio_le_rise hr hb 200
  have he : rise r 200 / ((200 : ℕ).factorial : ℝ) =
      ∏ j ∈ Finset.range 200, (r + j) / (j + 1 : ℝ) := by
    rw [Finset.prod_div_distrib, rise, Nat.factorial_eq_prod_range_add_one, Nat.cast_prod]
    simp only [Nat.cast_add, Nat.cast_one]
  rw [he, show (200 : ℕ) = 199 + 1 by norm_num, Finset.prod_range_succ'] at h
  have hp : (∏ j ∈ Finset.range 199, (r + ((j + 1 : ℕ) : ℝ)) / (((j + 1 : ℕ) : ℝ) + 1)) ≤ 1 := by
    apply Finset.prod_le_one₀
    · intro j _
      positivity
    · intro j _
      apply (div_le_one (by positivity)).mpr
      linarith only [hr']
  simp only [Nat.cast_zero, add_zero, zero_add, div_one] at h
  exact h.trans ((mul_comm _ _).le.trans (mul_le_of_le_one_right hr.le hp))

private def product (r b : ℝ) : ℝ := mass r b 0 * mass r b 200

private theorem product_eq (r b : ℝ) : product r b = zeroMass r b ^ 2 * ratio r b 200 := by
  unfold product
  rw [mass_zero, mass, pow_two, mul_assoc]

private theorem product_swap (r b : ℝ) : product r b = product b r := by
  rw [product_eq, product_eq, zeroMass_swap]
  unfold ratio
  rw [mul_comm (rise r 200), show 2 + r + b = 2 + b + r by ring]

private theorem product_small {r b : ℝ} (hr : 0 < r) (hb : 0 < b) (hr' : r ≤ 1 / 100000) :
    product r b ≤ 1 / 70000 := by
  have h := ratio_le_small hr hb (hr'.trans (by norm_num))
  have h₁ := pow_le_pow_left₀ (zeroMass_pos hr.le hb.le).le
    (zeroMass_le_one hr.le hb.le) 2
  rw [product_eq]
  calc
    _ ≤ 1 ^ 2 * r := mul_le_mul h₁ h (ratio_pos hr hb 200).le (by norm_num)
    _ ≤ _ := by linarith only [hr']


private abbrev δ : ℝ := 1 / 100000
private def smallTail : ℝ := 1 / (70000 * 301 ^ 200)
private def boundSmall : ℝ := exp ((logG (2 + δ) - log smallTail) / δ)
private def bigTail : ℝ := Gamma 103 ^ 2 * 2 ^ 200
private def cutoff : ℝ := max 200 (max boundSmall (70000 * bigTail))

private theorem sorted_tail_small {r b : ℝ} (hr : δ ≤ r) (hb : 0 < b)
    (hr' : r ≤ 101) (hcut : cutoff ≤ b) : product r b ≤ 1 / 70000 := by
  have hδ : 0 < δ := by norm_num [δ]
  have hrp := hδ.trans_le hr
  have hsmall : 0 < smallTail := by unfold smallTail; positivity
  have hcl : boundSmall ≤ b :=
    (le_max_left _ _).trans ((le_max_right (200 : ℝ) _).trans hcut)
  have hlog := log_le_log (exp_pos _ : 0 < boundSmall)
    (hcl.trans (by linarith : b ≤ 1 + b))
  change log (exp ((logG (2 + δ) - log smallTail) / δ)) ≤ log (1 + b) at hlog
  rw [log_exp, div_le_iff₀ hδ, mul_comm (log (1 + b)) δ] at hlog
  have hz' : zeroMass b δ ≤ smallTail := by
    have he := exp_le_exp.mpr
      (show logG (2 + δ) - δ * log (1 + b) ≤ log smallTail by linarith only [hlog])
    rw [exp_log hsmall] at he
    exact (zeroMass_decay hb.le hδ).trans he
  have hz := (zeroMass_antitone hb.le hδ.le hrp.le hr).trans
    (le_of_eq_of_le (zeroMass_swap _ _) hz')
  have hR := (ratio_le_power hrp hb).trans
    (pow_le_pow_left₀ (by positivity : 0 ≤ r + 200) (by linarith only [hr'] : r + 200 ≤ 301) 200)
  have hz₀ := (zeroMass_pos hrp.le hb.le).le
  have hzsq : zeroMass r b ^ 2 ≤ zeroMass r b := by
    rw [pow_two]
    simpa only [mul_one] using mul_le_mul_of_nonneg_left (zeroMass_le_one hrp.le hb.le) hz₀
  rw [product_eq]
  refine (mul_le_mul (hzsq.trans hz) hR (ratio_pos hrp hb 200).le hsmall.le).trans_eq ?_
  unfold smallTail
  field_simp

private theorem sorted_tail_large {r b : ℝ} (hr : 101 ≤ r) (hrb : r ≤ b)
    (hcut : cutoff ≤ b) : product r b ≤ 1 / 70000 := by
  have hbp := (by norm_num : (0 : ℝ) < 101).trans_le (hr.trans hrb)
  have hrp := (by norm_num : (0 : ℝ) < 101).trans_le hr
  have hb₂ : 200 ≤ b := (le_max_left _ _).trans hcut
  let x := 1 + b
  have hx : 0 < x := by dsimp [x]; positivity
  have hx₁ : 1 ≤ x := le_add_of_nonneg_right hbp.le
  have hB : 70000 * bigTail ≤ x :=
    (le_max_right _ _).trans ((le_max_right (200 : ℝ) _).trans
      (hcut.trans (by dsimp [x]; linarith)))
  have hΓ := Gamma_pos_of_pos (by norm_num : (0 : ℝ) < 103)
  have hz : zeroMass r b ≤ Gamma 103 / x ^ 101 := by
    have h₀ := zeroMass_antitone hbp.le (by norm_num : (101 : ℝ) ∈ Ici 0) hrp.le hr
    change zeroMass r b ≤ zeroMass 101 b at h₀
    rw [zeroMass_swap 101 b] at h₀
    refine h₀.trans ((zeroMass_decay hbp.le (by norm_num : (0 : ℝ) < 101)).trans_eq ?_)
    rw [show (2 : ℝ) + 101 = 103 by norm_num]
    change exp (logG 103 - 101 * log x) = _
    unfold logG
    rw [exp_sub, exp_log hΓ, show (101 : ℝ) * log x = ((101 : ℕ) : ℝ) * log x by norm_num,
      exp_nat_mul, exp_log hx]
  have hR : ratio r b 200 ≤ (2 * x) ^ 200 := by
    have h := show r + 200 ≤ 2 * x by
      dsimp [x]
      linarith only [hrb, hb₂]
    exact (ratio_le_power hrp hbp).trans (pow_le_pow_left₀ (by positivity) h 200)
  have hBig : 0 < bigTail := by unfold bigTail; positivity
  have hsq : x ≤ x ^ 2 := by
    have h := mul_le_mul_of_nonneg_left hx₁ hx.le
    linarith only [h]
  rw [product_eq]
  calc
    _ ≤ (Gamma 103 / x ^ 101) ^ 2 * (2 * x) ^ 200 :=
      mul_le_mul (pow_le_pow_left₀ (zeroMass_pos hrp.le hbp.le).le hz 2) hR
        (ratio_pos hrp hbp 200).le (sq_nonneg _)
    _ = bigTail / x ^ 2 := by
      unfold bigTail
      field_simp [hx.ne']
    _ ≤ _ := (div_le_iff₀ (pow_pos hx 2)).mpr
      (by linarith only [hB.trans hsq])

private def jointDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
private def rect : Set (ℝ × ℝ) := Icc δ cutoff ×ˢ Icc δ cutoff

private theorem witness_rect : (1, 100) ∈ rect := by
  have hc : (200 : ℝ) ≤ cutoff := le_max_left _ _
  refine ⟨⟨by norm_num [δ], (by norm_num : (1 : ℝ) ≤ 200).trans hc⟩,
    ⟨by norm_num [δ], (by norm_num : (100 : ℝ) ≤ 200).trans hc⟩⟩

private theorem outside_bound {p : ℝ × ℝ} (hp : p ∈ jointDomain) (hp' : p ∉ rect) :
    product p.1 p.2 ≤ 1 / 70000 := by
  have sorted {r b : ℝ} (hr : 0 < r) (hb : 0 < b) (hrb : r ≤ b) (hout : (r, b) ∉ rect) :
      product r b ≤ 1 / 70000 := by
    by_cases hδ : δ ≤ r
    · have hcut : cutoff ≤ b := le_of_not_ge fun hh ↦
        hout ⟨⟨hδ, hrb.trans hh⟩, ⟨hδ.trans hrb, hh⟩⟩
      rcases le_total r 101 with hr' | hr'
      · exact sorted_tail_small hδ hb hr' hcut
      · exact sorted_tail_large hr' hrb hcut
    · exact product_small hr hb (le_of_not_ge hδ)
  rcases le_total p.1 p.2 with h | h
  · exact sorted hp.1 hp.2 h hp'
  · rw [product_swap]
    exact sorted hp.2 hp.1 h (fun q ↦ hp' ⟨q.2, q.1⟩)

private theorem product_continuousOn : ContinuousOn (fun p : ℝ × ℝ ↦ product p.1 p.2) rect := by
  have hΓ : ContinuousOn Gamma (Ioi 0) := by
    intro t ht
    have htp : 0 < t := ht
    exact (Real.differentiableAt_Gamma (fun n ↦ by
      have h : (0 : ℝ) ≤ (n : ℝ) := by positivity
      linarith only [h, htp])).continuousAt.continuousWithinAt
  have hpos {p : ℝ × ℝ} (hp : p ∈ rect) : 0 < p.1 ∧ 0 < p.2 := by
    exact ⟨(by norm_num [δ] : (0 : ℝ) < δ).trans_le hp.1.1,
      (by norm_num [δ] : (0 : ℝ) < δ).trans_le hp.2.1⟩
  have hG₁ : ContinuousOn (fun p : ℝ × ℝ ↦ Gamma (2 + p.1)) rect :=
    hΓ.comp (by fun_prop) (fun p hp ↦ show 0 < 2 + p.1 by have := (hpos hp).1; positivity)
  have hG₂ : ContinuousOn (fun p : ℝ × ℝ ↦ Gamma (2 + p.2)) rect :=
    hΓ.comp (by fun_prop) (fun p hp ↦ show 0 < 2 + p.2 by have := (hpos hp).2; positivity)
  have hG₃ : ContinuousOn (fun p : ℝ × ℝ ↦ Gamma (2 + p.1 + p.2)) rect :=
    hΓ.comp (by fun_prop) (fun p hp ↦ show 0 < 2 + p.1 + p.2 by
      have := (hpos hp).1
      have := (hpos hp).2
      positivity)
  have hsum {p : ℝ × ℝ} (hp : p ∈ rect) : 0 < 2 + p.1 + p.2 := by
    have := (hpos hp).1
    have := (hpos hp).2
    positivity
  have hZero : ContinuousOn (fun p : ℝ × ℝ ↦ zeroMass p.1 p.2) rect :=
    (hG₁.mul hG₂).div hG₃ (fun _ hp ↦ (Gamma_pos_of_pos (hsum hp)).ne')
  have hRise : Continuous (fun x : ℝ ↦ rise x 200) := by
    unfold rise
    exact continuous_finsetProd _ (fun j _ ↦ continuous_id.add continuous_const)
  have h₁ := (hRise.comp continuous_fst).continuousOn (s := rect)
  have h₂ := (hRise.comp continuous_snd).continuousOn (s := rect)
  have h₃ : ContinuousOn (fun p : ℝ × ℝ ↦ rise (2 + p.1 + p.2) 200 * ((200 : ℕ).factorial : ℝ))
      rect := ((hRise.comp (by fun_prop)).mul continuous_const).continuousOn
  have hRatio : ContinuousOn (fun p : ℝ × ℝ ↦ ratio p.1 p.2 200) rect :=
    (h₁.mul h₂).div h₃ (fun p hp ↦ mul_ne_zero (rise_pos (hsum hp) _).ne' (by positivity))
  simp only [product_eq]
  exact (hZero.pow 2).mul hRatio

private theorem product_attains :
    ∃ p ∈ jointDomain, IsMaxOn (fun p : ℝ × ℝ ↦ product p.1 p.2) jointDomain p := by
  have hc : IsCompact rect := isCompact_Icc.prod isCompact_Icc
  obtain ⟨p, hp, hmax⟩ := hc.exists_isMaxOn ⟨_, witness_rect⟩ product_continuousOn
  have hp' : p ∈ jointDomain := ⟨(by norm_num [δ] : (0 : ℝ) < δ).trans_le hp.1.1,
    (by norm_num [δ] : (0 : ℝ) < δ).trans_le hp.2.1⟩
  refine ⟨p, hp', fun q hq ↦ ?_⟩
  by_cases hBox : q ∈ rect
  · exact hmax hBox
  · exact (outside_bound hq hBox).trans (product_witness.le.trans (hmax witness_rect))

set_option maxHeartbeats 1000000 in
-- Rewriting both maximizing inequalities unfolds the explicit Gamma ratios.
/-- The explicit beta-negative-binomial likelihood of the sample (0, 200)
at held alpha 2 attains its maximum at two distinct swapped shape pairs. -/
theorem explicit_labeled_global_tie :
    let mass (r b : ℝ) (k : ℕ) :=
      (Gamma (2 + r) * Gamma (2 + b) * Gamma (r + k) * Gamma (b + k)) /
        (Gamma (2 + r + b + k) * Gamma r * Gamma b * k.factorial)
    let S : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let f := fun p : ℝ × ℝ ↦ mass p.1 p.2 0 * mass p.1 p.2 200
    ∃ p ∈ S, p.1 ≠ p.2 ∧ IsMaxOn f S p ∧ p.swap ∈ S ∧ IsMaxOn f S p.swap := by
  dsimp only
  obtain ⟨p, hp, hm⟩ := product_attains
  have hw : (1, 100) ∈ jointDomain := ⟨by norm_num, by norm_num⟩
  have hneq : p.1 ≠ p.2 := by
    intro he
    have hb := diag_product_le_envelope hp.1 (by positivity : (0 : ℝ) ≤ (⌊p.1⌋₊ : ℝ))
      (Nat.floor_le (show 0 ≤ p.1 from hp.1.le)) (Nat.lt_floor_add_one p.1).le
    have hd : product p.1 p.2 < 1 / 70000 := by
      rw [← he]
      exact hb.trans_lt (envelope_all ⌊p.1⌋₊)
    exact (hd.trans product_witness).not_ge (hm hw)
  have hswap : p.swap ∈ jointDomain := ⟨hp.2, hp.1⟩
  refine ⟨p, hp, hneq, ?_, hswap, ?_⟩
  · intro q hq
    have hq' : q ∈ jointDomain := hq
    simp only [mem_ofPred_eq]
    rw [← mass_eq_gamma hq'.1 hq'.2, ← mass_eq_gamma hq'.1 hq'.2,
      ← mass_eq_gamma hp.1 hp.2, ← mass_eq_gamma hp.1 hp.2]
    exact hm hq
  · intro q hq
    have hq' : q ∈ jointDomain := hq
    simp only [mem_ofPred_eq]
    rw [← mass_eq_gamma hq'.1 hq'.2, ← mass_eq_gamma hq'.1 hq'.2,
      ← mass_eq_gamma hswap.1 hswap.2, ← mass_eq_gamma hswap.1 hswap.2]
    exact (hm hq).trans_eq (product_swap p.1 p.2)


end
end UnivariateML.TiesBNBinom

end
