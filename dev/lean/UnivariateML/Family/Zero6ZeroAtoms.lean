module

public import UnivariateML.Family.Zero6Families
public import UnivariateML.Family.Discrete5BNBinom

/-! All-zero inflation paths under beta-negative-binomial and discretised gamma parent holds. -/

@[expose] public section

open Set Filter Real MeasureTheory ProbabilityTheory
open scoped Topology

namespace UnivariateML.Zero6ZeroAtoms

open Zero6Families Zero6Inflated

private theorem mass_zero_eq (p : ℝ × ℝ × ℝ) :
    bnbinomMass p 0 = exp (Discrete3BNBinom.logLikelihood (fun _ : Fin 1 ↦ 0) p.1 p.2.1 p.2.2) := by
  simp [bnbinomMass, Discrete3BNBinom.logLikelihood]

private theorem lift_zero_path {P : Type*} {z : ℕ} {rho : ℝ} {F : Filter ℝ}
    {mass : P → ℕ → ℝ} {p : ℝ → P} (H : Tendsto (fun t ↦ mass (p t) 0) F (𝓝 1)) :
    Tendsto (fun t ↦ likelihood z (fun i : Fin 0 ↦ i.elim0) mass (p t,rho)) F (𝓝 1) := by
  have h := H.const_mul (1-rho) |>.const_add rho |>.pow z
  convert h using 1
  · ext t
    simp [likelihood,effective]
  · simp

/-- Bnbinom, every mask with a free parent coordinate, zero:inflated
criterion on all zeros at held inflation below one. Each free numerator
coordinate can shrink to zero, and free alpha can grow to infinity.
The compatible paths all reach the same excluded zero-atom height. -/
theorem bnbinom_held_inflation_zero {z : ℕ} (hz : 0 < z) {rho : ℝ} (hrho : rho ∈ Ico 0 1)
    (size alpha beta : Option ℝ) (hfree : size = none ∨ alpha = none ∨ beta = none)
    (hD : (bnbinomSlice size alpha beta).Nonempty) :
    let D := bnbinomSlice size alpha beta
    let L := fun p ↦ likelihood z (fun i : Fin 0 ↦ i.elim0) bnbinomMass (p,rho)
    (∀ p ∈ D, L p < 1) ∧ IsLUB (L '' D) 1 ∧
    ∀ p ∈ D,
      (size = none → Tendsto (fun t ↦ L (t,p.2.1,p.2.2)) (𝓝[>] 0) (𝓝 1)) ∧
      (alpha = none → Tendsto (fun t ↦ L (p.1,t,p.2.2)) atTop (𝓝 1)) ∧
      (beta = none → Tendsto (fun t ↦ L (p.1,p.2.1,t)) (𝓝[>] 0) (𝓝 1)) := by
  dsimp only
  let D := bnbinomSlice size alpha beta
  let L := fun p ↦ likelihood z (fun i : Fin 0 ↦ i.elim0) bnbinomMass (p,rho)
  have hstrict (p : ℝ × ℝ × ℝ) (hp : p ∈ D) : L p < 1 := by
    have h := (Discrete4BNBinom.zero_atom_likelihood_paths (by decide : 0 < 1)
      (fun _ : Fin 1 ↦ 0) (fun _ ↦ rfl) hp.1 hp.2.1 hp.2.2.1).1
    have hu : bnbinomMass p 0 < 1 := (mass_zero_eq p).trans_lt h
    have hpos : 0 < bnbinomMass p 0 := exp_pos _
    have hepos : 0 < effective (bnbinomMass p 0) rho :=
      add_pos_of_nonneg_of_pos hrho.1 (mul_pos (sub_pos.mpr hrho.2) hpos)
    have he : effective (bnbinomMass p 0) rho < 1 := by
      unfold effective
      nlinarith [mul_pos (sub_pos.mpr hrho.2) (sub_pos.mpr hu)]
    simpa [L,likelihood] using pow_lt_one₀ hepos.le he hz.ne'
  have hpath (p : ℝ × ℝ × ℝ) (hp : p ∈ D) :
      Tendsto (fun t ↦ L (t,p.2.1,p.2.2)) (𝓝[>] 0) (𝓝 1) ∧
      Tendsto (fun t ↦ L (p.1,t,p.2.2)) atTop (𝓝 1) ∧
      Tendsto (fun t ↦ L (p.1,p.2.1,t)) (𝓝[>] 0) (𝓝 1) := by
    have h := Discrete4BNBinom.zero_atom_likelihood_paths (by decide : 0 < 1)
      (fun _ : Fin 1 ↦ 0) (fun _ ↦ rfl) hp.1 hp.2.1 hp.2.2.1
    exact ⟨lift_zero_path (by simpa only [mass_zero_eq] using h.2.1),
      lift_zero_path (by simpa only [mass_zero_eq] using h.2.2.2),
      lift_zero_path (by simpa only [mass_zero_eq] using h.2.2.1)⟩
  refine ⟨hstrict,⟨?_,?_⟩,fun p hp ↦ ⟨fun _ ↦ (hpath p hp).1,
    fun _ ↦ (hpath p hp).2.1,fun _ ↦ (hpath p hp).2.2⟩⟩
  · rintro _ ⟨p,hp,rfl⟩
    exact (hstrict p hp).le
  · intro C hC
    obtain ⟨p,hp⟩ := hD
    rcases hfree with hs | ha | hb
    · apply le_of_tendsto (hpath p hp).1
      filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
      exact hC ⟨(t,p.2.1,p.2.2),⟨ht,hp.2.1,hp.2.2.1,
        by simp [hs,matchesHold],hp.2.2.2.2.1,hp.2.2.2.2.2⟩,rfl⟩
    · apply le_of_tendsto (hpath p hp).2.1
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
      exact hC ⟨(p.1,t,p.2.2),⟨hp.1,ht,hp.2.2.1,
        hp.2.2.2.1,by simp [ha,matchesHold],hp.2.2.2.2.2⟩,rfl⟩
    · apply le_of_tendsto (hpath p hp).2.2
      filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
      exact hC ⟨(p.1,p.2.1,t),⟨hp.1,hp.2.1,ht,
        hp.2.2.2.1,hp.2.2.2.2.1,by simp [hb,matchesHold]⟩,rfl⟩

private theorem gamma_cell_zero {a : ℝ} (ha : 0 < a) (r : ℝ) :
    dgammaMass (a,r) 0 = cdf (gammaMeasure a 1) r := by
  have hzero : cdf (gammaMeasure a 1) 0 = 0 := by
    rw [cdf_gammaMeasure_eq_integral ha one_pos, integral_Iic_eq_integral_Iio]
    apply setIntegral_eq_zero_of_forall_eq_zero
    intro y hy
    simp [gammaPDFReal, not_le.mpr (mem_Iio.mp hy)]
  simp [dgammaMass, Discrete3DGamma.cellProb, hzero]

/-- Dgamma, all free or one parent hold, zero:inflated all-zero criterion.
At held inflation below one, every finite likelihood is below one.
Either free parent coordinate reaches the excluded zero atom. -/
theorem dgamma_held_inflation_zero {z : ℕ} (hz : 0 < z) {rho : ℝ} (hrho : rho ∈ Ico 0 1)
    {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    let L := fun p ↦ likelihood z (fun i : Fin 0 ↦ i.elim0) dgammaMass (p,rho)
    L (a,r) < 1 ∧
    Tendsto (fun b ↦ L (b,r)) (𝓝[>] 0) (𝓝 1) ∧
    Tendsto (fun s ↦ L (a,s)) atTop (𝓝 1) := by
  dsimp only
  have hA := Discrete3DGamma.all_zero_held_rate hr (by decide : 0 < 1)
  have hR := Discrete3DGamma.all_zero_held_shape ha (by decide : 0 < 1)
  have hmass : dgammaMass (a,r) 0 < 1 := by
    rw [gamma_cell_zero ha]
    simpa using hA.2 a ha
  have h0 : 0 ≤ dgammaMass (a,r) 0 := by rw [gamma_cell_zero ha]; exact cdf_nonneg _ _
  have he0 : 0 ≤ effective (dgammaMass (a,r) 0) rho :=
    add_nonneg hrho.1 (mul_nonneg (sub_nonneg.mpr hrho.2.le) h0)
  have he1 : effective (dgammaMass (a,r) 0) rho < 1 := by
    unfold effective
    nlinarith [mul_pos (sub_pos.mpr hrho.2) (sub_pos.mpr hmass)]
  refine ⟨by simpa [likelihood] using pow_lt_one₀ he0 he1 hz.ne',?_,?_⟩
  · apply lift_zero_path
    refine (show Tendsto (fun b ↦ cdf (gammaMeasure b 1) r) (𝓝[>] 0) (𝓝 1)
      from by simpa using hA.1).congr' ?_
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with b hb
    exact (gamma_cell_zero hb r).symm
  · apply lift_zero_path
    simpa only [gamma_cell_zero ha, pow_one] using hR.1

end UnivariateML.Zero6ZeroAtoms

end
