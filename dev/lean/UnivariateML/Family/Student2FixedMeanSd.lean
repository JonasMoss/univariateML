module

public import UnivariateML.Family.StudentFaces

set_option linter.style.header false

/-!
# The two excluded ends with mean and standard deviation held
-/

@[expose] public section

open Filter Topology

namespace UnivariateML.Student2FixedMeanSd

/-- The df-two end is zero, finite positive, or infinite as the held-mean
atom count is below, at, or above two thirds. The other end is the matched
normal likelihood. -/
theorem std_fixed_mean_sd_ends {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ}
    (hσ : 0 < σ) :
    let L := fun ν : ℝ ↦ Real.exp (TDist.stdLogLik x μ ν σ)
    let k := TDist.multiplicity x μ
    (3 * k < 2 * n → Tendsto L (𝓝[>] 2) (𝓝 0)) ∧
    (3 * k = 2 * n → Tendsto L (𝓝[>] 2)
      (𝓝 (Real.exp (StudentFaces.fixedSdCollapseLog x μ)))) ∧
    (2 * n < 3 * k → Tendsto L (𝓝[>] 2) atTop) ∧
    Tendsto L atTop (𝓝 (∏ i, TDist.normalDensity μ σ (x i))) := by
  dsimp only
  obtain ⟨hp, he, hn⟩ := StudentFaces.std_fixed_sd_collapse x μ hσ
  refine ⟨fun h ↦ hp ?_, fun h ↦ he ?_, fun h ↦ hn ?_,
    StudentFaces.std_fixed_sd_normal x μ hσ⟩
  · have h' : 3 * (TDist.multiplicity x μ : ℝ) < 2 * (n : ℝ) := by exact_mod_cast h
    unfold TDist.exponent
    linarith
  · have h' : 3 * (TDist.multiplicity x μ : ℝ) = 2 * (n : ℝ) := by exact_mod_cast h
    unfold TDist.exponent
    linarith
  · have h' : 2 * (n : ℝ) < 3 * (TDist.multiplicity x μ : ℝ) := by exact_mod_cast h
    unfold TDist.exponent
    linarith

end UnivariateML.Student2FixedMeanSd
end
