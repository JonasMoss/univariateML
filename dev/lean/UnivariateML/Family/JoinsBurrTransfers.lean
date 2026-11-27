module

public import UnivariateML.Family.BurrFree

import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set
open UnivariateML.BurrFree

namespace UnivariateML.JoinsBurrTransfers

private theorem constant_or_nonconstant {n : ℕ} (x : Fin n → ℝ) (j : Fin n) :
    (∀ i, x i = x j) ∨ ∃ i k, x i ≠ x k := by
  by_cases h : ∀ i, x i = x j
  · exact Or.inl h
  · obtain ⟨i, hi⟩ := not_forall.mp h
    exact Or.inr ⟨i,j,hi⟩

private theorem join_faces {n : ℕ} (x y : Fin n → ℝ) (j : Fin n)
    (L : (ℝ × ℝ × ℝ) → ℝ) (J : ℝ)
    (hc : (∀ i, x i = x j) → ¬ BddAbove (L '' physicalDomain))
    (hface : (∃ i k, x i ≠ x k) →
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ((∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
          ∃ p ∈ physicalDomain, n * max (paretoValue y) (weibullLik y c b) - J ≤ L p)) :
    ((∀ i, x i = x j) → ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
      (∃ i k, x i ≠ x k) ∧
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ∃ p ∈ physicalDomain, n * max (paretoValue y) (weibullLik y c b) - J ≤ L p) := by
  refine ⟨hc, ?_⟩
  constructor
  · rintro ⟨p,hp,hm⟩
    rcases constant_or_nonconstant x j with hz | hn
    · exact (hc hz hm.bddAbove).elim
    · obtain ⟨c,hc,b,hb,hfit⟩ := hface hn
      exact ⟨hn,c,hc,b,hb,hfit.mp ⟨p,hp,hm⟩⟩
  · rintro ⟨hn,c,hc,b,hb,p,hp,hm⟩
    obtain ⟨c',hc',b',hb',hfit⟩ := hface hn
    rw [le_antisymm (hb' c hc b) (hb c' hc' b')] at hm
    exact hfit.mpr ⟨p,hp,hm⟩

/-- Constant inverse Burr data diverge. On nonconstant data a finite
maximum exists exactly when it reaches the larger reciprocal face. -/
theorem inverse_burr_criterion {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x i ≤ x j) :
    let L := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let y := logExcess (Transfer.reciprocal x) j
    let J := ∑ i, Real.log (x i)
    ((∀ i, x i = x j) → ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
      (∃ i k, x i ≠ x k) ∧
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ∃ p ∈ physicalDomain, n * max (paretoValue y) (weibullLik y c b) - J ≤ L p) := by
  intro L y J
  refine join_faces x y j L J ?_ ?_
  · intro h
    dsimp only [L]
    rw [show x = fun _ ↦ x j from funext h]
    exact ShapeFacesInverseBurr.joint_constant_unbounded j.isLt.pos (hx j)
  · intro h
    obtain ⟨c,hc,b,hb,hfit,_⟩ := inverse_criterion_and_escape j.isLt.pos hx h hj
    exact ⟨c,hc,b,hb,hfit⟩

/-- At a held Pareto IV threshold with positive excesses, constants
diverge. On nonconstant excesses a finite maximum exists exactly when
it reaches the larger translated Burr face. -/
theorem paretoIV_held_threshold_criterion {n : ℕ} {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let L := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2
    let e := ShapeFacesParetoIV.excess x μ
    let y := logExcess e j
    let J := ∑ i, Real.log (e i)
    ((∀ i, x i = x j) → ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
      (∃ i k, x i ≠ x k) ∧
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ∃ p ∈ physicalDomain, n * max (paretoValue y) (weibullLik y c b) - J ≤ L p) := by
  intro L e y J
  refine join_faces x y j L J ?_ ?_
  · intro h
    dsimp only [L]
    rw [show x = fun _ ↦ x j from funext h]
    exact ShapeFacesParetoIV.held_threshold_constant_unbounded j.isLt.pos (hx j)
  · intro h
    obtain ⟨c,hc,b,hb,hfit,_⟩ := paretoIV_criterion_and_escape j.isLt.pos hx h hj
    exact ⟨c,hc,b,hb,hfit⟩

private theorem join_diagonal {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (L : (ℝ × ℝ) → ℝ)
    (hc : (∀ i, x i = x ⟨0, hn⟩) → ¬ BddAbove (L '' Weibull.domain))
    (hfit : (∃ i j, x i ≠ x j) → ∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) :
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔ ∃ i j, x i ≠ x j) ∧
    ((∀ i, x i = x ⟨0, hn⟩) → ¬ BddAbove (L '' Weibull.domain)) := by
  refine ⟨⟨?_, hfit⟩, hc⟩
  rintro ⟨p,hp,hm⟩
  exact (constant_or_nonconstant x ⟨0, hn⟩).resolve_left fun h ↦ hc h hm.bddAbove

/-- The paralogistic finite fit exists exactly on nonconstant positive
samples. On constant samples the log likelihood diverges. -/
theorem paralogistic_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    let L := fun p : ℝ × ℝ ↦ Paralogistic.logLik x p.1 p.2
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔ ∃ i j, x i ≠ x j) ∧
    ((∀ i, x i = x ⟨0, hn⟩) → ¬ BddAbove (L '' Weibull.domain)) := by
  intro L
  refine join_diagonal hn x L ?_ (fun h ↦ (paralogistic_MLE_and_escape hn hx h).1)
  intro h
  dsimp only [L]
  rw [show x = fun _ ↦ x ⟨0, hn⟩ from funext h]
  exact paralogistic_constant_unbounded hn (hx ⟨0, hn⟩)

/-- Inverse paralogistic has the same finite-attainment condition and
constant-sample divergence in physical coordinates. -/
theorem inverse_paralogistic_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    let L := fun p : ℝ × ℝ ↦ InvParalogistic.logLik x p.1 p.2
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔ ∃ i j, x i ≠ x j) ∧
    ((∀ i, x i = x ⟨0, hn⟩) → ¬ BddAbove (L '' Weibull.domain)) := by
  intro L
  refine join_diagonal hn x L ?_ (fun h ↦ (inverse_paralogistic_MLE_and_escape hn hx h).1)
  intro h
  dsimp only [L]
  rw [show x = fun _ ↦ x ⟨0, hn⟩ from funext h]
  exact inverse_paralogistic_constant_unbounded hn (hx ⟨0, hn⟩)

end UnivariateML.JoinsBurrTransfers

end
