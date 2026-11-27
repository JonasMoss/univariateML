module

public import UnivariateML.Family.StudentGeodesic
public import UnivariateML.Family.StudentHeldShape

set_option linter.style.header false

/-!
# Joint maximizer cardinality for standard Student at held df
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Student2HeldDf

/-- The joint maximizer set at held df above two has exactly one element
under the strict mass test, and is empty otherwise. Testing every observed
atom is equivalent to testing a largest atom. -/
theorem std_held_df_cardinality {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ}
    (hν : 2 < ν) :
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let f := fun z : ℝ × ℝ ↦ TDist.stdLogLik x z.1 ν z.2
    let strictMass := ∀ i, (TDist.multiplicity x (x i) : ℝ) <
      ν * ((n : ℝ) - TDist.multiplicity x (x i))
    ((∃! p, p ∈ D ∧ IsMaxOn f D p) ↔ strictMass) ∧
      ((∀ p ∈ D, ¬ IsMaxOn f D p) ↔ ¬ strictMass) := by
  dsimp only
  have h := StudentHeldShape.std_held_df_attains_iff x hn hν
  simp only [TDist.exponent, sub_pos] at h
  constructor
  · constructor
    · rintro ⟨p, ⟨hp, hm⟩, _⟩
      exact h.mp ⟨p, hp, hm⟩
    · intro hs
      obtain ⟨p, hp, hm⟩ := h.mpr hs
      exact ⟨p, ⟨hp, hm⟩, fun q hq ↦
        StudentGeodesic.std_held_df_unique x hn hν hq.1.2 hp.2 hq.2 hm⟩
  · constructor
    · intro he hs
      obtain ⟨p, hp, hm⟩ := h.mpr hs
      exact he p hp hm
    · intro hs p hp hm
      exact hs (h.mp ⟨p, hp, hm⟩)

/-- At fixed df the only potentially nonvanishing excluded ends are
zero-scale paths at locations with nonpositive collapse exponent. -/
theorem std_held_df_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ}
    (hν : 2 < ν) :
    let L := fun m s ↦ exp (TDist.stdLogLik x m ν s)
    ((∀ b, TDist.exponent x b ν < 0 → Tendsto (L b) (𝓝[>] 0) atTop)) ∧
    (∀ b, TDist.exponent x b ν = 0 →
      let C := exp (TDist.collapseLogValue x b ν)
      Tendsto (L b) (𝓝[>] 0) (𝓝 C) ∧
      IsLUB ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' (univ ×ˢ Ioi 0)) C ∧
      ∀ m s : ℝ, 0 < s → L m s < C) ∧
    (∀ {ι : Type*} {l : Filter ι} {m s : ι → ℝ}, (∀ᶠ j in l, 0 < s j) →
      (Tendsto s l atTop ∨ Tendsto m l (cocompact ℝ)) →
      Tendsto (fun j ↦ L (m j) (s j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {m s : ι → ℝ} {b : ℝ},
      Tendsto m l (𝓝 b) → Tendsto s l (𝓝[>] 0) →
      (0 < TDist.exponent x b ν → Tendsto (fun j ↦ L (m j) (s j)) l (𝓝 0)) ∧
      (TDist.exponent x b ν = 0 → ∀ B > exp (TDist.collapseLogValue x b ν),
        ∀ᶠ j in l, L (m j) (s j) < B)) := by
  dsimp only
  have hv : 0 < ν := by linarith
  let c := sqrt ((ν - 2) / ν)
  have hc : 0 < c := sqrt_pos.mpr (div_pos (sub_pos.mpr hν) hv)
  have hEq (m s : ℝ) (hs : 0 < s) :
      exp (TDist.stdLogLik x m ν s) = TDist.likelihood x m ν (s * c) := by
    rw [TDist.stdLogLik_eq x m hν hs]
    rfl
  have hp : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
  have hPath : Tendsto (fun s : ℝ ↦ s * c) (𝓝[>] 0) (𝓝[>] 0) := by
    refine tendsto_nhdsWithin_iff.mpr ⟨?_, hp.mono fun s hs ↦ mul_pos hs hc⟩
    have h0 : Tendsto (id : ℝ → ℝ) (𝓝[>] 0) (𝓝 0) :=
      tendsto_id.mono_left nhdsWithin_le_nhds
    simpa using h0.mul_const c
  refine ⟨?_, ?_, ?_, ?_⟩
  · intro b hb
    exact ((TDist.collapse_unbounded x b hv hb).comp hPath).congr'
      (hp.mono fun s hs ↦ (hEq b s hs).symm)
  · intro b hb
    have hl := ((StudentFaces.lst_fixed_df_local_collapse x b hv
      (l := 𝓝[>] (0 : ℝ)) (m := fun _ ↦ b) (a := id) tendsto_const_nhds tendsto_id).2 hb).1
    exact ⟨(hl.comp hPath).congr' (hp.mono fun s hs ↦ (hEq b s hs).symm),
      StudentHeldShape.std_critical_joint x hn hν hb⟩
  · intro ι l m s hs ht
    have h := StudentFaces.lst_fixed_df_remote_ends x hn hv
      (m := m) (s := fun j ↦ s j * c) (hs.mono fun _ hj ↦ mul_pos hj hc)
      (ht.imp (fun h ↦ h.atTop_mul_const hc) id)
    exact h.congr' (hs.mono fun j hj ↦ (hEq (m j) (s j) hj).symm)
  · intro ι l m s b hm hs
    obtain ⟨hpos, hcrit⟩ := StudentFaces.lst_fixed_df_local_collapse x b hv hm (hPath.comp hs)
    have he := (tendsto_nhdsWithin_iff.mp hs).2.mono fun j hj ↦ hEq (m j) (s j) hj
    exact ⟨fun hb ↦ (hpos hb).congr' (he.mono fun _ hj ↦ hj.symm),
      fun hb B hB ↦ (he.and ((hcrit hb).2 B hB)).mono fun j ⟨hj, hlt⟩ ↦ hj.trans_lt hlt⟩

end UnivariateML.Student2HeldDf
end
