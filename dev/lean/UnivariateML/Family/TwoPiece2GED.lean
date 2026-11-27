module

public import UnivariateML.Family.TwoPieceGED
public import Mathlib.Analysis.SpecialFunctions.Pow.Deriv
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Exact fixed-shape GED maximizing sets and shape-end comparisons

## Deviations

The held-mean uniform trace concerns likelihood. The weak-law limit and
the signed off-contact comparison at held standard deviation remain open.
-/

@[expose] public section

open Set Filter Real
open scoped Topology
namespace UnivariateML.TwoPiece2GED

variable {n : ℕ}

/-- The three shape regimes give the complete maximizing set at held standard deviation. -/
theorem heldSd_maximizers {ν σ : ℝ} (hν : 0 < ν) (hσ : 0 < σ)
    (hn : 0 < n) (x : Fin n → ℝ) :
    let M := {μ | IsMaxOn (fun m ↦ Ged.logLik ν x (m, σ)) univ μ}
    (1 < ν → ∃ μ, M = {μ}) ∧
    (ν = 1 → M = Laplace.medianSet x hn) ∧
    (ν < 1 → M = {m | ∃ i, m = x i ∧
      ∀ j, Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x (x j)}) := by
  dsimp only
  refine ⟨?_, ?_, ?_⟩
  · intro h
    obtain ⟨μ, hμ, hu⟩ := Ged.heldScale_existsUnique h hn x hσ
    exact ⟨μ, Set.ext fun m ↦ ⟨fun hm ↦ hu m hm, fun hm ↦ (mem_singleton_iff.mp hm).symm ▸ hμ⟩⟩
  · rintro rfl
    exact Ged.heldScale_one hn x hσ
  · intro h
    exact Set.ext fun m ↦ TwoPieceGED.heldSd_belowOne hν h hσ hn x m

/-- Fixed-shape GED maximizers, including collapse on constant data and the
common profiled scale on the subunit branch. -/
theorem heldShape_maximizers {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) (x : Fin n → ℝ) :
    (¬ LocationScale.Nonconstant x → Ged.jointMaximizers ν x = ∅) ∧
    (LocationScale.Nonconstant x →
      (1 < ν → ∃ p, Ged.jointMaximizers ν x = {p}) ∧
      (ν = 1 → Ged.jointMaximizers ν x =
        Laplace.medianSet x hn ×ˢ {Real.sqrt 2 * Laplace.meanAbsDev x (Laplace.lowerMedian x hn)}) ∧
      (ν < 1 → ∃ s > 0, Ged.jointMaximizers ν x =
        {m | ∃ i, m = x i ∧
          ∀ j, Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x (x j)} ×ˢ {s})) := by
  constructor
  · intro hx
    apply Set.eq_empty_iff_forall_notMem.mpr
    intro p hp
    let i : Fin n := ⟨0, hn⟩
    have he (j : Fin n) : x j = x i := not_ne_iff.mp (fun h ↦ hx ⟨j, i, h⟩)
    obtain ⟨s, hs, hgt⟩ := ((show ∀ᶠ s in 𝓝[>] (0 : ℝ), s ∈ Ioi (0 : ℝ)
        from self_mem_nhdsWithin).and
      ((Ged.collapse hν hn he).eventually (Filter.eventually_gt_atTop (Ged.logLik ν x p)))).exists
    exact hgt.not_ge (hp.2 (show (x i, s) ∈ LocationScale.domain from ⟨mem_univ _, hs⟩))
  · intro hx
    refine ⟨?_, ?_, ?_⟩
    · intro h
      obtain ⟨p, hp, hu⟩ := Ged.joint_existsUnique h hx
      exact ⟨p, Set.ext fun q ↦ ⟨fun hq ↦ hu q hq, fun hq ↦ (mem_singleton_iff.mp hq).symm ▸ hp⟩⟩
    · rintro rfl
      exact Ged.joint_one hn hx
    · intro h
      let i₀ : Fin n := ⟨0, hn⟩
      obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ
        (fun i ↦ Ged.residualSum ν x (x i)) ⟨i₀, Finset.mem_univ _⟩
      have hx' : ∃ j, x j ≠ x i := by
        by_contra! he
        obtain ⟨j, k, hjk⟩ := hx
        exact hjk ((he j).trans (he k).symm)
      refine ⟨Ged.bestScale ν x (x i), (Ged.heldLocation hν hn hx').1, ?_⟩
      rw [TwoPieceGED.heldShape_belowOne hν h hn hx]
      ext p
      simp only [mem_ofPred_eq, mem_prod, mem_singleton_iff]
      apply and_congr_right
      rintro ⟨j, hj, hjmin⟩
      have he : Ged.residualSum ν x p.1 = Ged.residualSum ν x (x i) := by
        rw [hj]
        exact le_antisymm (hjmin i) (hi j (Finset.mem_univ _))
      simp only [Ged.bestScale, he]

private noncomputable def logGammaWidth (t : ℝ) : ℝ :=
  (log (Gamma (1+t)) - log (Gamma (1+3*t))) / 2

private theorem logGammaWidth_zero : logGammaWidth 0 = 0 := by
  simp [logGammaWidth]

private theorem logGammaWidth_deriv :
    HasDerivAt logGammaWidth eulerMascheroniConstant 0 := by
  have h (k : ℝ) :
      HasDerivAt (fun t ↦ log (Gamma (1+k*t))) (-eulerMascheroniConstant*k) 0 := by
    have hG := hasDerivAt_Gamma_one.log (by simp)
    have hk := ((hasDerivAt_id (0 : ℝ)).const_mul k).const_add 1
    have he : (1 : ℝ) + k*0 = 1 := by simp
    have hd := (he.symm ▸ hG).comp 0 hk
    simpa only [Function.comp_def, id_eq, mul_zero, add_zero, Gamma_one, div_one, mul_one] using hd
  convert ((h 1).sub (h 3)).div_const 2 using 1
  · funext t
    simp only [logGammaWidth, Pi.sub_apply, one_mul]
  · ring

private theorem held_kernel_width {p σ : ℝ} (hp : 0 < p) (hσ : 0 < σ) :
    (sqrt 3 * σ * exp (logGammaWidth p⁻¹)) * Ged.sdFactor p = σ := by
  let U := Gamma (1+p⁻¹)
  let V := Gamma (1+3*p⁻¹)
  have hi : 0 < p⁻¹ := inv_pos.mpr hp
  have hU : 0 < U := Gamma_pos_of_pos (by positivity)
  have hV : 0 < V := Gamma_pos_of_pos (by positivity)
  have hG : Ged.sdFactor p = sqrt (V/(3*U)) := by
    simp only [Ged.sdFactor]
    congr 1
    dsimp [U, V]
    rw [show 1/p = p⁻¹ by simp, div_eq_mul_inv 3 p,
      add_comm (1 : ℝ), add_comm (1 : ℝ),
      Gamma_add_one hi.ne', Gamma_add_one (mul_pos (by norm_num) hi).ne']
    field_simp [hi.ne', (Gamma_pos_of_pos hi).ne']
  have hpow : exp (logGammaWidth p⁻¹)^2 = U/V := by
    rw [sq, ← exp_add]
    rw [show logGammaWidth p⁻¹ + logGammaWidth p⁻¹ = log U - log V by
      dsimp [logGammaWidth, U, V]; ring]
    rw [← log_div hU.ne' hV.ne', exp_log (div_pos hU hV)]
  rw [hG]
  apply (sq_eq_sq₀ (by positivity) hσ.le).mp
  rw [mul_pow, mul_pow, mul_pow, hpow, sq_sqrt (by norm_num), sq_sqrt (by positivity)]
  field_simp [hU.ne', hV.ne']

private theorem heldSd_logLik_eq {p σ : ℝ} (hp : 0 < p) (hσ : 0 < σ)
    (x : Fin n → ℝ) (μ : ℝ) :
    let ρ := sqrt 3 * σ
    Ged.logLik p x (μ,σ) =
      -(n : ℝ) * log (2 * (ρ * exp (logGammaWidth p⁻¹)) * Gamma (1+p⁻¹)) -
        ∑ i, exp (-(p * logGammaWidth p⁻¹)) * (|x i-μ|/ρ)^p := by
  intro ρ
  let b := ρ * exp (logGammaWidth p⁻¹)
  have hρ : 0 < ρ := by dsimp [ρ]; positivity
  have hb : 0 < b := mul_pos hρ (exp_pos _)
  rw [← held_kernel_width hp hσ, ← TwoPieceGED.kernelLogLik_eq hp hb]
  unfold TwoPieceGED.kernelLogLik
  simp only [one_div]
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  rw [show |x i-μ| / b = (|x i-μ|/ρ) / exp (logGammaWidth p⁻¹) by
    rw [div_div], div_rpow (by positivity) (exp_pos _).le, ← exp_mul, div_eq_mul_inv,
    ← exp_neg, mul_comm (logGammaWidth p⁻¹) p, mul_comm]

/-- At fixed mean and sd, the high-shape limit is the centered uniform
trace when every datum lies in the limiting interval. Each endpoint
observation contributes the penalty `exp (-eulerMascheroniConstant)`.
Any observation strictly outside the interval drives the log likelihood
to minus infinity. -/
theorem heldMeanSd_uniformTrace {σ : ℝ} (hσ : 0 < σ) (x : Fin n → ℝ) (μ : ℝ) :
    let ρ := sqrt 3 * σ
    let k := (Finset.univ.filter fun i ↦ |x i-μ| = ρ).card
    let f := fun p ↦ Ged.logLik p x (μ,σ)
    ((∀ i, |x i-μ| ≤ ρ) →
      Tendsto f atTop (𝓝 (-(n : ℝ)*log (2*ρ) - k*exp (-eulerMascheroniConstant)))) ∧
    ((∃ i, ρ < |x i-μ|) → Tendsto f atTop atBot) := by
  classical
  intro ρ k f
  have hρ : 0 < ρ := by dsimp [ρ]; positivity
  have hi : Tendsto (fun p : ℝ ↦ p⁻¹) atTop (𝓝 (0 : ℝ)) := tendsto_inv_atTop_zero
  have hB : Tendsto (fun p : ℝ ↦ logGammaWidth p⁻¹) atTop (𝓝 0) := by
    simpa only [Function.comp_def, logGammaWidth_zero] using
      (logGammaWidth_deriv.continuousAt.tendsto.comp hi)
  have hrate : Tendsto (fun p : ℝ ↦ p*logGammaWidth p⁻¹) atTop
      (𝓝 eulerMascheroniConstant) := by
    simpa only [Function.comp_def, zero_add, logGammaWidth_zero, sub_zero, smul_eq_mul, inv_inv]
      using logGammaWidth_deriv.tendsto_slope_zero_right.comp tendsto_inv_atTop_nhdsGT_zero
  have hG : Tendsto (fun p : ℝ ↦ Gamma (1+p⁻¹)) atTop (𝓝 1) := by
    have ht : Tendsto (fun p : ℝ ↦ 1+p⁻¹) atTop (𝓝 1) := by
      simpa only [add_zero] using tendsto_const_nhds.add hi
    simpa only [Function.comp_def, Gamma_one] using
      hasDerivAt_Gamma_one.continuousAt.tendsto.comp ht
  let G (p : ℝ) := -(n : ℝ) * log (2 * (ρ * exp (logGammaWidth p⁻¹)) * Gamma (1+p⁻¹))
  let A (p : ℝ) := exp (-(p*logGammaWidth p⁻¹))
  let P (i : Fin n) (p : ℝ) := A p * (|x i-μ|/ρ)^p
  have hA : Tendsto A atTop (𝓝 (exp (-eulerMascheroniConstant))) :=
    continuous_exp.continuousAt.tendsto.comp hrate.neg
  have hNorm : Tendsto G atTop (𝓝 (-(n : ℝ)*log (2*ρ))) := by
    have hArg : Tendsto (fun p : ℝ ↦ 2 * (ρ * exp (logGammaWidth p⁻¹)) * Gamma (1+p⁻¹))
        atTop (𝓝 (2*ρ)) := by
      have hE := continuous_exp.continuousAt.tendsto.comp hB
      simpa only [Function.comp_def, exp_zero, mul_one] using
        (((hE.const_mul ρ).const_mul 2).mul hG)
    exact (hArg.log (by positivity)).const_mul _
  have hEq : (fun p ↦ G p - ∑ i, P i p) =ᶠ[atTop] f := by
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
    exact (heldSd_logLik_eq hp hσ x μ).symm
  constructor
  · intro h
    let L (i : Fin n) := if |x i-μ| = ρ then exp (-eulerMascheroniConstant) else 0
    have hl (i : Fin n) : Tendsto (P i) atTop (𝓝 (L i)) := by
      by_cases he : |x i-μ| = ρ
      · simpa [P, L, he, hρ.ne'] using hA
      · have hb := (div_lt_one hρ).mpr (lt_of_le_of_ne (h i) he)
        have hnon : 0 ≤ |x i-μ|/ρ := by positivity
        have ht := tendsto_rpow_atTop_of_base_lt_one _ (by linarith) hb
        simpa only [P, L, ite_eq_right he, mul_zero] using hA.mul ht
    have hsum := tendsto_finsetSum Finset.univ (fun i _ ↦ hl i)
    have he : ∑ i, L i = k * exp (-eulerMascheroniConstant) := by
      simp [L, k, Finset.sum_ite, nsmul_eq_mul]
    rw [he] at hsum
    exact (hNorm.sub hsum).congr' hEq
  · rintro ⟨i, hi⟩
    have hbase := (one_lt_div hρ).mpr hi
    have hb : Tendsto (fun p : ℝ ↦ (|x i-μ|/ρ)^p) atTop atTop := by
      have hpos : 0 < |x i-μ|/ρ := zero_lt_one.trans hbase
      simp_rw [rpow_def_of_pos hpos]
      apply tendsto_exp_atTop.comp
      exact (tendsto_const_mul_atTop_of_pos (log_pos hbase)).mpr tendsto_id
    have hP : Tendsto (P i) atTop atTop := hA.pos_mul_atTop (exp_pos _) hb
    have hS : Tendsto (fun p ↦ ∑ j, P j p) atTop atTop := by
      apply tendsto_atTop_mono _ hP
      intro p
      exact Finset.single_le_sum (f := fun j ↦ P j p)
        (fun j _ ↦ mul_nonneg (exp_pos _).le (rpow_nonneg (by positivity) _))
        (Finset.mem_univ i)
    let C := -(n : ℝ)*log (2*ρ) + 1
    have hC : -(n : ℝ)*log (2*ρ) < C := lt_add_one _
    apply Tendsto.congr' hEq
    apply tendsto_atBot_mono' _ _
      (tendsto_atBot_add_const_right atTop C (tendsto_neg_atTop_atBot.comp hS))
    filter_upwards [hNorm.eventually (gt_mem_nhds hC)] with p hp
    change G p - (∑ j, P j p) ≤ -(∑ j, P j p) + C
    linarith

/-- GED with all coefficients free is unbounded on the unrestricted
shape domain. On the fitted domain constants collapse, and the retained
compact profile and uniform level decide attainment for nonconstant data. -/
theorem allFree_criterion_failure (hn : 0 < n) (x : Fin n → ℝ) :
    let V := TwoPieceGED.freeSdValues x univ
    (¬ BddAbove {L : ℝ | ∃ ν μ σ : ℝ, 0 < ν ∧ 0 < σ ∧ L = Ged.logLik ν x (μ, σ)}) ∧
    (¬ LocationScale.Nonconstant x → ¬ BddAbove V) ∧
    (LocationScale.Nonconstant x →
      ∃ l r : Fin n, (∀ i, x l ≤ x i ∧ x i ≤ x r) ∧
        let K := Icc 1 (TwoPieceGED.uniformCutoff n) ×ˢ Icc (x l) (x r)
        let F := -(n : ℝ) * log (x r - x l)
        ∃ y ∈ K, IsMaxOn (TwoPieceGED.freeSdProfile x) K y ∧
          ((∃ L, IsGreatest V L) ↔ F ≤ TwoPieceGED.freeSdProfile x y) ∧
          (TwoPieceGED.freeSdProfile x y < F → IsLUB V F)) := by
  intro V
  refine ⟨TwoPieceGED.unrestricted_unbounded hn x, ?_, ?_⟩
  · intro hx
    let i : Fin n := ⟨0, hn⟩
    have he (j : Fin n) : x j = x i := not_ne_iff.mp (fun h ↦ hx ⟨j, i, h⟩)
    exact TwoPieceGED.fitted_constant_collapse hn (mem_univ (x i)) he
  · intro hx
    simpa only [show ∀ z : ℝ, 2*(z/2) = z by intro z; ring] using
      TwoPieceGED.freeSd_allFree hn hx

private theorem heldMean_profile_differentiable {x : Fin n → ℝ} {μ p : ℝ}
    (hp : 0 < p) (hx : ∃ i, x i ≠ μ) :
    DifferentiableAt ℝ (fun q ↦ TwoPieceGED.freeSdProfile x (q,μ)) p := by
  let Q := fun q : ℝ ↦ Ged.residualSum q x μ
  let b := fun q : ℝ ↦ (q * Q q / n)^q⁻¹
  have hterm (i : Fin n) : DifferentiableAt ℝ (fun q : ℝ ↦ |x i-μ|^q) p := by
    by_cases h : x i = μ
    · apply (differentiableAt_const (c := (0 : ℝ))).congr_of_eventuallyEq
      filter_upwards [lt_mem_nhds hp] with q hq
      simp [h, zero_rpow hq.ne']
    · exact (differentiableAt_const _).rpow differentiableAt_id
        (abs_pos.mpr (sub_ne_zero.mpr h)).ne'
  have hQ : DifferentiableAt ℝ Q p := by
    unfold Q Ged.residualSum
    classical
    have h (s : Finset (Fin n)) : DifferentiableAt ℝ
        (fun q : ℝ ↦ ∑ i ∈ s, |x i-μ|^q) p := by
      induction s using Finset.induction_on with
      | empty => simp only [Finset.sum_empty]; exact differentiableAt_const _
      | insert i s hi hs =>
        simp only [Finset.sum_insert hi]
        exact (hterm i).add hs
    exact h Finset.univ
  obtain ⟨i, hi⟩ := hx
  have hn : (0 : ℝ) < n := by exact_mod_cast Nat.zero_lt_of_lt i.isLt
  have hqpos (q : ℝ) (hq : 0 < q) : 0 < q * Q q / n := by
    have hq' : 0 < Q q :=
      (rpow_pos_of_pos (abs_pos.mpr (sub_ne_zero.mpr hi)) q).trans_le
        (Finset.single_le_sum (f := fun i ↦ |x i-μ|^q)
          (fun _ _ ↦ rpow_nonneg (abs_nonneg _) _) (Finset.mem_univ i))
    exact div_pos (mul_pos hq hq') hn
  have hpos (q : ℝ) (hq : 0 < q) : 0 < b q :=
    rpow_pos_of_pos (hqpos q hq) _
  have hb : DifferentiableAt ℝ b p :=
    ((differentiableAt_id.mul hQ).div_const _).rpow
      (differentiableAt_id.inv hp.ne') (hqpos p hp).ne'
  have hG : DifferentiableAt ℝ Gamma (1+1/p) := differentiableAt_Gamma (by
    intro k
    linarith [div_pos one_pos hp, Nat.cast_nonneg (α := ℝ) k])
  have hG' : DifferentiableAt ℝ (fun q : ℝ ↦ Gamma (1+1/q)) p := by
    have harg : DifferentiableAt ℝ (fun q : ℝ ↦ 1+1/q) p :=
      (differentiableAt_const _).add ((differentiableAt_const _).div differentiableAt_id hp.ne')
    change DifferentiableAt ℝ (Real.Gamma ∘ (fun q : ℝ ↦ 1+1/q)) p
    exact hG.comp p harg
  have hGpos := Gamma_pos_of_pos (show 0 < 1+1/p by linarith [div_pos one_pos hp])
  have hN : DifferentiableAt ℝ (fun q : ℝ ↦
      -(n : ℝ)*log (2*b q*Gamma (1+1/q))) p :=
    ((((hb.const_mul 2).mul hG').log
      (mul_pos (mul_pos (by norm_num) (hpos p hp)) hGpos).ne').const_mul _)
  have hD : DifferentiableAt ℝ (fun q : ℝ ↦ b q ^ q) p :=
    hb.rpow differentiableAt_id (hpos p hp).ne'
  have hr : DifferentiableAt ℝ (fun q : ℝ ↦
      -(n : ℝ)*log (2*b q*Gamma (1+1/q)) - Q q/(b q)^q) p :=
    hN.sub (hQ.div hD (rpow_pos_of_pos (hpos p hp) _).ne')
  apply hr.congr_of_eventuallyEq
  filter_upwards [lt_mem_nhds hp] with q hq
  have hEq : TwoPieceGED.freeSdProfile x (q,μ) = TwoPieceGED.kernelLogLik q x μ (b q) := by
    rw [TwoPieceGED.kernelLogLik_eq hq (hpos q hq) x μ]
    unfold TwoPieceGED.freeSdProfile Ged.bestScale b Q
    congr 2
    ring
  rw [hEq]
  unfold TwoPieceGED.kernelLogLik Q Ged.residualSum
  simp only [abs_nonneg, div_rpow, (hpos q hq).le, Finset.sum_div]

/-- With mean held and shape fitted, zero residuals give unbounded
collapse. Otherwise attainment is decided by the compact shape
endpoints and stationary values against the centered uniform. -/
theorem heldMean_criterion_failure (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    let V := TwoPieceGED.freeSdValues x ({μ} : Set ℝ)
    let f := fun p ↦ TwoPieceGED.freeSdProfile x (p,μ)
    let P := TwoPieceGED.uniformCutoff n
    ((∀ i, x i = μ) → ¬ BddAbove V) ∧
    ((∃ i, x i ≠ μ) → BddAbove V ∧
      ∃ i : Fin n, (∀ j, |x j-μ| ≤ |x i-μ|) ∧
        let F := -(n : ℝ)*log (2*|x i-μ|)
        let S := {p ∈ Icc 1 P | p = 1 ∨ p = P ∨ deriv f p = 0}
        DifferentiableOn ℝ f (Ioi 0) ∧
        ((∃ L, IsGreatest V L) ↔ ∃ p ∈ S, F ≤ f p) ∧
        ((∀ p ∈ S, f p < F) → IsLUB V F)) := by
  intro V f P
  refine ⟨fun h ↦ TwoPieceGED.fitted_constant_collapse hn (mem_singleton μ) h, ?_⟩
  intro hx
  obtain ⟨i, hi⟩ := TwoPieceGED.freeSd_heldMean hn hx
  obtain ⟨hρ, hfar, ⟨⟨p,m⟩, hp, hm, hiff, hsup⟩⟩ := hi _ rfl
  have he : m = μ := hp.2
  subst m
  let F := -(n : ℝ)*log (2*|x i-μ|)
  let S := {p ∈ Icc 1 P | p = 1 ∨ p = P ∨ deriv f p = 0}
  have hmax : IsMaxOn f (Icc 1 P) p :=
    fun q hq ↦ hm (show (q,μ) ∈ Icc 1 P ×ˢ ({μ} : Set ℝ) from ⟨hq,rfl⟩)
  have hS : p ∈ S := by
    refine ⟨hp.1, ?_⟩
    rcases hp.1.1.eq_or_lt with h | hl
    · exact Or.inl h.symm
    · rcases hp.1.2.eq_or_lt with h | hr
      · exact Or.inr (Or.inl h)
      · exact Or.inr (Or.inr (hmax.isLocalMax (Icc_mem_nhds hl hr)).deriv_eq_zero)
  have hiff' : (∃ L, IsGreatest V L) ↔ F ≤ f p := hiff
  have hb : BddAbove V := by
    by_cases hw : F ≤ f p
    · obtain ⟨L,hL⟩ := hiff'.mpr hw
      exact ⟨L,hL.2⟩
    · exact ⟨F,(hsup (lt_of_not_ge hw)).1⟩
  refine ⟨hb,i,hfar,fun q hq ↦ (heldMean_profile_differentiable hq hx).differentiableWithinAt,
    hiff'.trans ⟨fun h ↦ ⟨p,hS,h⟩, fun ⟨q,hq,hw⟩ ↦ hw.trans (hmax hq.1)⟩,
    fun h ↦ hsup (h p hS)⟩

end UnivariateML.TwoPiece2GED

end
