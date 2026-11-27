module

public import UnivariateML.Family.ShapeFacesInverseBurr
public import UnivariateML.Family.TiesLomax

set_option linter.style.header false

/-!
Burr results with scale and first shape held, and exact finite ties
at a held power.

## Deviations

The held-power counterexample uses the 21-point Lomax crossing and its power transform. The
four-point example and the all-free counterexample from the Burr reports are outside this
module.
-/

@[expose] public section

open Real Set Filter

namespace UnivariateML.Shape2Burr
open Transfer

noncomputable section

variable {n : ℕ}

/-- The free Burr power has a unique optimum exactly off the all-contact
sample. At all contacts the likelihood tends to infinity with power. -/
theorem held_scale_first_classification (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    ((∃ i, x i ≠ s) →
      ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun b ↦ Burr.logLik x b k s) (Ioi 0) b) ∧
    ((∀ i, x i = s) → Tendsto (fun b ↦ Burr.logLik x b k s) atTop atTop) := by
  let y := Transfer.reciprocal x
  have hy (i : Fin n) : 0 < y i := inv_pos.mpr (hx i)
  have hrel (b : ℝ) : ShapeFacesInverseBurr.logLik y k b s⁻¹ =
      Burr.logLik x b k s + Transfer.reciprocalJac y := by
    rw [ShapeFacesInverseBurr.logLik_reciprocal hy k b (inv_pos.mpr hs), inv_inv]
    congr 1
    congr 1
    funext i
    exact inv_inv (x i)
  constructor
  · rintro ⟨i, hi⟩
    have hie : y i ≠ s⁻¹ := fun h ↦ hi (inv_injective h)
    exact (Transfer.existsUnique_max_iff
      (g := fun b ↦ Burr.logLik x b k s) (bijOn_id (Ioi (0 : ℝ))) (fun b _ ↦ hrel b)).1
      (ShapeFacesInverseBurr.held_first_scale_unique hn hy hk (inv_pos.mpr hs) ⟨i, hie⟩)
  · intro he
    have hf (i : Fin n) : y i = s⁻¹ := congrArg Inv.inv (he i)
    have h := ShapeFacesInverseBurr.held_first_scale_unbounded hn hk (inv_pos.mpr hs) hf
    apply (tendsto_atTop_add_const_right _ (-Transfer.reciprocalJac y) h).congr
    intro b
    rw [hrel]
    ring

private theorem rate_bij {b : ℝ} (hb : 0 < b) :
    BijOn (fun p : ℝ × ℝ ↦ (p.1, Burr.rate b p.2)) Lomax.domain Lomax.domain := by
  have hpos (s : ℝ) (hs : 0 < s) : 0 < Burr.rate b s :=
    inv_pos.mpr (rpow_pos_of_pos hs b)
  refine ⟨fun p hp ↦ ⟨hp.1, hpos _ hp.2⟩, ?_, ?_⟩
  · intro u hu v hv he
    have hf := congrArg (fun p : ℝ × ℝ ↦ p.1) he
    refine Prod.ext hf ?_
    apply (rpow_left_inj hu.2.le hv.2.le hb.ne').1
    exact inv_injective (congrArg Prod.snd he)
  · intro p hp
    have hr : 0 < p.2 := hp.2
    refine ⟨(p.1, (p.2⁻¹) ^ b⁻¹),
      ⟨hp.1, rpow_pos_of_pos (inv_pos.mpr hr) _⟩, Prod.ext rfl ?_⟩
    change Burr.rate b ((p.2⁻¹) ^ b⁻¹) = p.2
    rw [Burr.rate, ← rpow_mul (inv_pos.mpr hr).le, inv_mul_cancel₀ hb.ne',
      rpow_one, inv_inv]

/-- For every held positive power, an exact positive 21-observation sample
has precisely two Burr maximizers. Both beat its Weibull face. -/
theorem held_power_exact_tie {b : ℝ} (hb : 0 < b) :
    ∃ x : Fin 21 → ℝ, (∀ i, 0 < x i) ∧
      ∃ p q : ℝ × ℝ, p ≠ q ∧
        {v ∈ Lomax.domain | IsMaxOn (fun r : ℝ × ℝ ↦ Burr.logLik x b r.1 r.2)
          Lomax.domain v} = {p, q} ∧
        Burr.bestWeibull x b < Burr.logLik x b p.1 p.2 := by
  obtain ⟨a, ⟨ha, ht⟩, _⟩ := TiesLomax.three_level_global_tie
  let u : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
  obtain ⟨v, hv, w, hw, hs₁, hs₂, hm₁, hm₂, hall, hface⟩ := ht
  have ha₀ : 0 < a := (by positivity : (0 : ℝ) < ((2 : ℝ) ^ (512 : ℕ))⁻¹).trans ha.1
  have hu (i : Fin 21) : 0 < u i := by
    dsimp [u]
    split
    · exact ha₀
    · split <;> norm_num
  let x : Fin 21 → ℝ := fun i ↦ (u i) ^ b⁻¹
  have hx (i : Fin 21) : 0 < x i := rpow_pos_of_pos (hu i) _
  have he : Burr.powered x b = u := by
    funext i
    change ((u i) ^ b⁻¹) ^ b = u i
    rw [← rpow_mul (hu i).le, inv_mul_cancel₀ hb.ne', rpow_one]
  let φ := fun p : ℝ × ℝ ↦ (p.1, Burr.rate b p.2)
  have hbφ : BijOn φ Lomax.domain Lomax.domain := rate_bij hb
  have hf (p : ℝ × ℝ) (hp : p ∈ Lomax.domain) :
      Burr.logLik x b p.1 p.2 = Lomax.logLik u (φ p).1 (φ p).2 + Burr.powerJac x b := by
    rw [Burr.logLik_transfer hx b p.1 hp.2, he]
  have hmap (z : ℝ × ℝ) (hz : z ∈ Lomax.domain) :
      IsMaxOn (fun r : ℝ × ℝ ↦ Burr.logLik x b r.1 r.2) Lomax.domain z ↔
        IsMaxOn (fun r : ℝ × ℝ ↦ Lomax.logLik u r.1 r.2) Lomax.domain (φ z) :=
    Transfer.isMaxOn_iff hbφ hf hz
  obtain ⟨p, hp, hpv⟩ := hbφ.surjOn hv
  obtain ⟨q, hq, hqw⟩ := hbφ.surjOn hw
  have hne : p ≠ q := by
    intro h
    have h' : v = w := hpv.symm.trans ((congrArg φ h).trans hqw)
    exact (hs₁.trans hs₂).ne (congrArg Prod.snd h')
  refine ⟨x, hx, p, q, hne, ?_, ?_⟩
  · ext z
    simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
    constructor
    · rintro ⟨hz, hmax⟩
      have hm := (hmap z hz).1 hmax
      rcases hall (φ z) (hbφ.mapsTo hz) hm with h | h
      · exact Or.inl (hbφ.injOn hz hp (h.trans hpv.symm))
      · exact Or.inr (hbφ.injOn hz hq (h.trans hqw.symm))
    · rintro (rfl | rfl)
      · exact ⟨hp, (hmap _ hp).2 (hpv.symm ▸ hm₁)⟩
      · exact ⟨hq, (hmap _ hq).2 (hqw.symm ▸ hm₂)⟩
  · rw [Burr.bestWeibull, he, hf p hp, hpv]
    exact add_lt_add_left hface _

end

end UnivariateML.Shape2Burr

end
