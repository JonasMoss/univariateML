module

public import UnivariateML.Family.Zero6
public import UnivariateML.Family.Zero6Inflated

/-! Comparison of conditional and original parent faces after transport.
Deviations: exhaustion of the named parent faces and finite critical
candidate coverage enter as hypotheses. -/

@[expose] public section

open Set Filter
open scoped Topology

namespace UnivariateML.Zero6Faces

open Truncation Zero6

noncomputable section

/-- A reachable height is a lower bound on the likelihood supremum. -/
theorem reachable_le {P : Type*} {D : Set P} {L : P → ℝ} {s h : ℝ}
    (hs : IsLUB (L '' D) s) {F : Filter P} [F.NeBot]
    (hD : ∀ᶠ p in F, p ∈ D) (hL : Tendsto L F (𝓝 h)) : h ≤ s :=
  le_of_tendsto hL (hD.mono fun p hp ↦ hs.1 ⟨p,hp,rfl⟩)

/-- Exact comparison with an exhaustive parent escape list, retaining
finite and escaped ties. The strict-gap hypothesis supplies the
compactness step for the chosen chart. -/
theorem compare {P A : Type*} {D C : Set P} {E : Set A} {L : P → ℝ} {H : A → ℝ}
    {s f : ℝ} (hs : IsLUB (L '' D) s) (hf : IsLUB (H '' E) f)
    (reachable : ∀ e ∈ E, H e ≤ s) (hc : C ⊆ D)
    (critical : ∀ p ∈ D, IsMaxOn L D p → p ∈ C)
    (strictGap : f < s → ∃ p ∈ D, IsMaxOn L D p) :
    ((∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ C, f ≤ L p) ∧
    {p ∈ D | IsMaxOn L D p} = {p ∈ C | L p = s} ∧
    {e ∈ E | s ≤ H e} = {e ∈ E | H e = s} := by
  have hfs : f ≤ s := hf.2 (by rintro _ ⟨e,he,rfl⟩; exact reachable e he)
  have hmax {p : P} (hp : p ∈ D) : IsMaxOn L D p ↔ L p = s := by
    constructor
    · intro H
      exact le_antisymm (hs.1 ⟨p,hp,rfl⟩) (hs.2 (by rintro _ ⟨q,hq,rfl⟩; exact H hq))
    · intro H q hq
      exact H ▸ hs.1 ⟨q,hq,rfl⟩
  refine ⟨?_, ?_, ?_⟩
  · constructor
    · rintro ⟨p,hp,hm⟩
      exact ⟨p,critical p hp hm, (hmax hp |>.mp hm) ▸ hfs⟩
    · rintro ⟨p,hp,hp'⟩
      rcases hfs.eq_or_lt with he | he
      · exact ⟨p,hc hp,(hmax (hc hp)).mpr (le_antisymm (hs.1 ⟨p,hc hp,rfl⟩) (he ▸ hp'))⟩
      · exact strictGap he
  · ext p
    constructor
    · rintro ⟨hp,hm⟩
      exact ⟨critical p hp hm,(hmax hp).mp hm⟩
    · rintro ⟨hp,he⟩
      exact ⟨hc hp,(hmax (hc hp)).mpr he⟩
  · ext e
    constructor
    · rintro ⟨he,hh⟩
      exact ⟨he,le_antisymm (reachable e he) hh⟩
    · rintro ⟨he,hh⟩
      exact ⟨he,hh.ge⟩

/-- Positive scaling preserves all tied conditional face heights. -/
theorem modified_tie {z k : ℕ} {w a b : ℝ} (h : 0 < bernoulli z k w) :
    bernoulli z k w * a ≤ bernoulli z k w * b ↔ a ≤ b :=
  mul_le_mul_iff_of_pos_left h

/-- The original-mass likelihood of a zero atom vanishes on any
positive sample, at every inflation weight. -/
theorem inflated_zero_atom {k : ℕ} (hk : 0 < k) {z : ℕ} {x : Fin k → ℕ}
    (hx : ∀ i, 0 < x i) (r : ℝ) :
    Zero6Inflated.likelihood z x (fun (_ : Unit) j ↦ if j = 0 then 1 else 0) ((),r) = 0 := by
  have h : (∏ i : Fin k, (1-r) * (if x i = 0 then 1 else 0)) = 0 :=
    Finset.prod_eq_zero (Finset.mem_univ ⟨0,hk⟩) (by simp [Nat.ne_of_gt (hx ⟨0,hk⟩)])
  unfold Zero6Inflated.likelihood
  rw [h,mul_zero]

end

end UnivariateML.Zero6Faces

end
