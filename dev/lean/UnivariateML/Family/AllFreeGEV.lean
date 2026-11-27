module

public import UnivariateML.Family.GEV

set_option linter.style.header false

/-! The unrestricted GEV spike and the shape-one lower-endpoint collapse. -/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.AllFreeGEV
noncomputable section

variable {n : ℕ}

/-- Every nonempty sample has unbounded likelihood if shapes below minus one
are allowed. An attained sample maximum may have any multiplicity. -/
theorem unrestricted_unbounded (hn : 0 < n) (x : Fin n → ℝ) :
    ¬ BddAbove {v | ∃ ξ : ℝ, ∃ p ∈ GEV.domain x ξ, GEV.logLik x ξ p = v} := by
  obtain ⟨i, _, hi⟩ := Finset.exists_max_image Finset.univ x
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  intro h
  apply GEV.negative_unbounded (xi := -2) (by norm_num)
    (fun j ↦ hi j (Finset.mem_univ _)) ⟨i, rfl⟩
  apply h.mono
  rintro _ ⟨p, hp, rfl⟩
  exact ⟨-2, p, hp, rfl⟩

/-- On the fitted branch, a strict majority at the minimum suffices for
unboundedness along shape one. Constants are included. -/
theorem majority_unbounded (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m)
    (hc : n < 2 * Frechet.minimumCount (GEV.lowerGaps x m)) :
    ¬ BddAbove {v | ∃ ξ ∈ Icc (-1 : ℝ) 1, ∃ p ∈ GEV.domain x ξ, GEV.logLik x ξ p = v} := by
  have hc' : (n : ℝ) < 2 * Frechet.minimumCount (GEV.lowerGaps x m) := by
    exact_mod_cast hc
  intro h
  apply GEV.positive_unbounded hn (xi := 1) one_pos hm hz
  · unfold Frechet.collapseExponent
    norm_num
    linarith
  · apply h.mono
    rintro _ ⟨p, hp, rfl⟩
    exact ⟨1, by norm_num, p, hp, rfl⟩

/-- At equal minimum and nonminimum counts, the shape-one path tends to
the stated finite total log likelihood. The lower endpoint stays strictly
below the minimum at finite parameters. -/
theorem half_minimum_collapse_path (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m)
    (hc : n = 2 * Frechet.minimumCount (GEV.lowerGaps x m)) :
    (∀ e > 0, GEV.collapsePath x m 1 e ∈ GEV.domain x 1) ∧
    Tendsto (GEV.collapsePath x m 1) (𝓝[>] 0) (𝓝 (m, 0)) ∧
    Tendsto (fun e ↦ GEV.logLik x 1 (GEV.collapsePath x m 1 e)) (𝓝[>] 0)
      (𝓝 ((n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (x i - m))) := by
  have hc' : (n : ℝ) = 2 * Frechet.minimumCount (GEV.lowerGaps x m) := by
    exact_mod_cast hc
  have he : Frechet.collapseExponent (GEV.lowerGaps x m) ((1 : ℝ)⁻¹) = 0 := by
    unfold Frechet.collapseExponent
    norm_num
    linarith
  obtain ⟨hD, hpath, hvalue, _⟩ := GEV.positive_collapse_path hn one_pos hm hz
  refine ⟨hD, hpath, ?_⟩
  norm_num at hvalue ⊢
  exact hvalue (by simpa using he)

end
end UnivariateML.AllFreeGEV

end
