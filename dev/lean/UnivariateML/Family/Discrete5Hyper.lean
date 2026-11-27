module

public import UnivariateML.Family.Discrete4Hyper

import Mathlib.Topology.Sequences
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Filter Set
open scoped Topology BigOperators

namespace UnivariateML.Discrete5Hyper

open Hypergeometric Discrete4Hyper

private theorem proportion_subseq {A N : ℕ → ℕ} (hA : ∀ t, A t ≤ N t) :
    ∃ p ∈ Icc (0 : ℝ) 1, ∃ f : ℕ → ℕ, StrictMono f ∧
      Tendsto (fun t ↦ (A (f t) : ℝ)/N (f t)) atTop (𝓝 p) := by
  refine isCompact_Icc.tendsto_subseq (x := fun t ↦ (A t : ℝ) / N t) ?_
  intro t
  refine ⟨by positivity, ?_⟩
  by_cases hx : N t = 0
  · simp [hx]
  · exact (div_le_one (Nat.cast_pos.mpr (Nat.pos_of_ne_zero hx))).mpr (by exact_mod_cast hA t)

/-- With draws held, the likelihood limits of compatible diverging urns
are exactly the binomial heights, including the closed endpoints. -/
theorem held_draw_escape_iff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (k : ℕ)
    (hx : ∀ i, x i ≤ k) (L : ℝ) :
    (∃ A N : ℕ → ℕ, (∀ t, A t ≤ N t) ∧
      (∀ t i, Compatible (A t) (N t) k (x i)) ∧ Tendsto N atTop atTop ∧
      Tendsto (fun t ↦ likelihood (A t) (N t) k x) atTop (𝓝 L)) ↔
    ∃ p ∈ Icc (0 : ℝ) 1, L = ∏ i, binomialFace k (x i) p := by
  constructor
  · rintro ⟨A,N,hA,_,hN,hL⟩
    obtain ⟨p,hp,f,hf,hr⟩ := proportion_subseq hA
    have hfa := hf.tendsto_atTop
    have hl := held_draw_binomial_limit x k hp (hN.comp hfa)
      (Eventually.of_forall fun t ↦ hA (f t)) hr
    exact ⟨p,hp,tendsto_nhds_unique (hL.comp hfa) hl⟩
  · rintro ⟨p,hp,rfl⟩
    obtain ⟨A,N,hN,hc,_,hL⟩ := held_draw_reaches_binomial_face x k hx hp
    exact ⟨A,N,fun t ↦ (hc t ⟨0,hh⟩).1,hc,hN,hL⟩

/-- Exchanging draws and marks gives the same exhaustive fixed-mark result. -/
theorem held_mark_escape_iff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (m : ℕ)
    (hx : ∀ i, x i ≤ m) (L : ℝ) :
    (∃ A N : ℕ → ℕ, (∀ t, A t ≤ N t) ∧
      (∀ t i, Compatible m (N t) (A t) (x i)) ∧ Tendsto N atTop atTop ∧
      Tendsto (fun t ↦ likelihood m (N t) (A t) x) atTop (𝓝 L)) ↔
    ∃ p ∈ Icc (0 : ℝ) 1, L = ∏ i, binomialFace m (x i) p := by
  constructor
  · rintro ⟨A,N,hA,_,hN,hL⟩
    obtain ⟨p,hp,f,hf,hr⟩ := proportion_subseq hA
    have hfa := hf.tendsto_atTop
    have hl := held_mark_binomial_limit x m hp (hN.comp hfa)
      (Eventually.of_forall fun t ↦ hA (f t)) hr
    exact ⟨p,hp,tendsto_nhds_unique (hL.comp hfa) hl⟩
  · rintro ⟨p,hp,rfl⟩
    obtain ⟨A,N,hN,hc,_,hL⟩ := held_mark_reaches_binomial_face x m hx hp
    exact ⟨A,N,fun t ↦ (hc t ⟨0,hh⟩).2.1,hc,hN,hL⟩

end UnivariateML.Discrete5Hyper

end
