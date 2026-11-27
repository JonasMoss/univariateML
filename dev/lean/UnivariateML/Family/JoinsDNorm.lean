module

public import UnivariateML.Family.HeldSweepDNorm
import Mathlib.Tactic

set_option linter.style.header false

/-!
## Deviations

The normalized CDF cell formula and the normalization of the existing
working product are proved separately. Equating the explicit Gaussian
interval integral with the existing opaque `intervalMass` still needs
a public interface lemma in `Discrete3DNorm`.
-/

@[expose] public section

open Set Real MeasureTheory Filter Topology
open UnivariateML.Discrete3DNorm UnivariateML.SkewLSGaussian

namespace UnivariateML.JoinsDNorm

private theorem total_eq : gaussianTotal = sqrt (2 * π) := by
  unfold gaussianTotal
  convert integral_gaussian (1 / 2 : ℝ) using 1
  · congr 1
    ext z
    unfold kernel
    congr 1
    ring
  · congr 1
    ring

/-- In the working coordinates, a normalized Gaussian CDF difference
is the standardized Gaussian interval integral divided by `sqrt (2π)`. -/
theorem working_cdf_integral (k : ℝ) (p : ℝ × ℝ) :
    let a := k * p.2 - p.1
    let b := (k + 1) * p.2 - p.1
    Φ b - Φ a = (∫ z in a..b, kernel z) / sqrt (2 * π) := by
  intro a b
  have hc : Continuous φ := continuous_iff_continuousAt.mpr fun z ↦
    (phi_hasDerivAt z).continuousAt
  rw [← intervalIntegral.integral_eq_sub_of_hasDerivAt
    (fun z _ ↦ cdf_hasDerivAt z) (hc.intervalIntegrable a b)]
  simp_rw [phi_formula, show ∀ z : ℝ, exp (-(z^2 / 2)) = kernel z by
    intro z; unfold kernel; congr 1; ring]
  rw [intervalIntegral.integral_const_mul]
  ring

private theorem ratio_strictMono : StrictMono (fun z : ℝ ↦ z / kernel z) := by
  have he (z : ℝ) : z / kernel z = z * exp (z^2 / 2) := by
    unfold kernel
    rw [show -(z^2) / 2 = -(z^2 / 2) by ring, exp_neg, div_inv_eq_mul]
  simp_rw [he]
  apply strictMono_of_deriv_pos
  intro z
  have hd : HasDerivAt (fun v : ℝ ↦ v^2 / 2) z z := by
    convert (((hasDerivAt_id z).pow 2).div_const 2) using 1
    · rfl
    · norm_num [id_eq]
  have hh : HasDerivAt (fun v : ℝ ↦ v * exp (v^2 / 2))
      (1 * exp (z^2 / 2) + z * (exp (z^2 / 2) * z)) z :=
    (hasDerivAt_id z).mul hd.exp
  rw [hh.deriv]
  change 0 < 1 * exp (z^2 / 2) + z * (exp (z^2 / 2) * z)
  nlinarith only [exp_pos (z^2 / 2), mul_nonneg (sq_nonneg z) (exp_pos (z^2 / 2)).le]

private theorem mass_pos {a b : ℝ} (hab : a < b) : 0 < intervalMass a b := by
  have ha : 0 < kernel a := exp_pos _
  have hb : 0 < kernel b := exp_pos _
  have ht := (div_lt_div_iff₀ ha hb).mp (ratio_strictMono hab)
  have h := endpoint_information_pos (va := kernel b) (vb := kernel a) hab
    (Or.inl hb.ne')
  have hn : 0 < b * kernel b * kernel a ^ 2 - a * kernel a * kernel b ^ 2 := by
    nlinarith only [mul_pos (mul_pos ha hb) (sub_pos.mpr ht)]
  unfold intervalScore at h
  rw [mul_comm (kernel b), sub_self, zero_div, zero_pow (by decide : 2 ≠ 0), zero_add] at h
  exact ((div_pos_iff.mp h).resolve_right (fun h' ↦ hn.not_gt h'.1)).2

/-- The log of the normalized working likelihood differs from the
existing working log likelihood by the Gaussian normalization only. -/
theorem working_log_normalization {n : ℕ} (x : Fin n → ℤ)
    {p : ℝ × ℝ} (hp : p ∈ workingDomain) :
    log (workingLikelihood x p) = workingLogLik x p - n * log (sqrt (2 * π)) := by
  have hB : 0 < p.2 := hp.2
  have ht : 0 < gaussianTotal := by rw [total_eq]; positivity
  have hc (i : Fin n) :
      0 < intervalMass ((x i : ℝ) * p.2 - p.1) (((x i : ℝ) + 1) * p.2 - p.1) :=
    mass_pos (by linarith)
  have hprob (i : Fin n) : 0 < workingCellProb (x i : ℝ) p := div_pos (hc i) ht
  unfold workingLikelihood workingLogLik
  rw [log_prod (fun i _ ↦ (hprob i).ne')]
  have he (i : Fin n) : log (workingCellProb (x i : ℝ) p) =
      workingCellLog (x i : ℝ) p - log (sqrt (2 * π)) := by
    rw [workingCellProb, intervalProb, workingCellLog, log_div (hc i).ne' ht.ne', total_eq]
  simp only [he, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]

/-- Maximization of the existing normalized working product is
equivalent to maximization of the working log objective on any fiber. -/
theorem working_argmax_iff {n : ℕ} (x : Fin n → ℤ) {D : Set (ℝ × ℝ)}
    (hD : D ⊆ workingDomain) {p : ℝ × ℝ} (hp : p ∈ D) :
    IsMaxOn (workingLikelihood x) D p ↔ IsMaxOn (workingLogLik x) D p := by
  have ht : 0 < gaussianTotal := by rw [total_eq]; positivity
  have hpos {q : ℝ × ℝ} (hq : q ∈ D) : 0 < workingLikelihood x q := by
    have hB : 0 < q.2 := (hD hq).2
    apply Finset.prod_pos
    intro i _
    exact div_pos (mass_pos (by linarith : (x i : ℝ)*q.2-q.1 < ((x i : ℝ)+1)*q.2-q.1)) ht
  have hcompare {q : ℝ × ℝ} (hq : q ∈ D) :
      workingLikelihood x q ≤ workingLikelihood x p ↔ workingLogLik x q ≤ workingLogLik x p := by
    rw [← log_le_log_iff (hpos hq) (hpos hp), working_log_normalization x (hD hq),
      working_log_normalization x (hD hp), sub_le_sub_iff_right]
  exact ⟨fun hm q hq ↦ (hcompare hq).mp (hm hq), fun hm q hq ↦ (hcompare hq).mpr (hm hq)⟩

/-- At a held location, a positive scale fits exactly when some
observed closed cell excludes that location. Otherwise the contact
product gives the strictly unattained zero-scale supremum. -/
theorem held_location_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (m : ℝ) :
    let L := HeldSweepDNorm.heldLocationLikelihood x m
    ((∃ B ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) B) ↔
      ∃ i, m ∉ Icc (x i : ℝ) ((x i : ℝ) + 1)) ∧
    ((∀ i, m ∈ Icc (x i : ℝ) ((x i : ℝ) + 1)) →
      let u := ∏ i, (if m = (x i : ℝ) ∨ m = (x i : ℝ) + 1 then (1 : ℝ) / 2 else 1)
      Tendsto L atTop (𝓝 u) ∧ IsLUB (L '' Ioi 0) u ∧ ∀ B > 0, L B < u) := by
  intro L
  refine ⟨⟨?_, HeldSweepDNorm.held_location_away_attainment x m⟩,
    HeldSweepDNorm.held_location_contact_supremum hn x m⟩
  rintro ⟨B,hB,hm⟩
  by_contra h
  push Not at h
  obtain ⟨_, hsup, hstrict⟩ := HeldSweepDNorm.held_location_contact_supremum hn x m h
  exact (hstrict B hB).not_ge (hsup.2 (by
    rintro _ ⟨q,hq,rfl⟩
    exact hm hq))

end UnivariateML.JoinsDNorm

end
