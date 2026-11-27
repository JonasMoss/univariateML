module

public import UnivariateML.Generic.LocationScale
public import Mathlib.Topology.Order.OrderClosed
public import Mathlib.Order.Filter.Extr

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Transfers for location–scale likelihoods

A likelihood-preserving parameter bijection transfers the full maximizer
set, including ties. A reachable excluded face gives an exact supremum
whenever every feasible point loses strictly to it.
-/

@[expose] public section

open Filter Set
open scoped Topology

namespace UnivariateML.LocScaleCommon

/-- Reparameterization transports the full maximizing set. -/
theorem isMaxOn_iff_of_bijOn {P Q R : Type*} [Preorder R]
    {S : Set P} {T : Set Q} {f : P → R} {g : Q → R} {e : P → Q}
    (he : BijOn e S T) (hvalue : ∀ p ∈ S, f p = g (e p)) {p : P} (hp : p ∈ S) :
    IsMaxOn f S p ↔ IsMaxOn g T (e p) := by
  simp only [isMaxOn_iff]
  constructor
  · intro hm q hq
    obtain ⟨r, hr, rfl⟩ := he.surjOn hq
    simpa only [← hvalue r hr, ← hvalue p hp] using hm r hr
  · intro hm r hr
    rw [hvalue r hr, hvalue p hp]
    exact hm (e r) (he.mapsTo hr)

/-- A nonzero affine change of the observations transports all location–scale
maximizers. Negative slopes also reverse the standardized loss argument. -/
theorem locationScale_affine_maximizers {n : ℕ} (g : ℝ → ℝ) (x : Fin n → ℝ)
    (d : ℝ) {b : ℝ} (hb : b ≠ 0) {S : Set (ℝ × ℝ)}
    (hS : S ⊆ LocationScale.domain) {p : ℝ × ℝ} (hp : p ∈ S) :
    let e := fun q : ℝ × ℝ ↦ (d+b*q.1, |b| *q.2)
    IsMaxOn (LocationScale.logLik g x) S p ↔
      IsMaxOn (LocationScale.logLik (fun z ↦ g ((|b|/b)*z)) (fun i ↦ d+b*x i))
        (e '' S) (e p) := by
  let e := fun q : ℝ × ℝ ↦ (d+b*q.1, |b| *q.2)
  let L := LocationScale.logLik (fun z ↦ g ((|b|/b)*z)) (fun i ↦ d+b*x i)
  have hB : 0 < |b| := abs_pos.mpr hb
  have hbij : BijOn e S (e '' S) := by
    refine ⟨fun q hq ↦ ⟨q,hq,rfl⟩,?_,fun _ hq ↦ hq⟩
    intro q _ r _ h
    apply Prod.ext
    · exact mul_left_cancel₀ hb (add_left_cancel (congrArg Prod.fst h))
    · exact mul_left_cancel₀ hB.ne' (congrArg Prod.snd h)
  have he (q : ℝ × ℝ) (hq : q ∈ S) :
      L (e q) = LocationScale.logLik g x q - (n : ℝ)*Real.log |b| := by
    have hs : 0 < q.2 := (hS hq).2
    have hterm (i : Fin n) :
        (|b|/b)*((d+b*x i-(d+b*q.1))/(|b| *q.2)) = (x i-q.1)/q.2 := by
      field_simp [hb,hB.ne',hs.ne']
      ring
    change (∑ i, (-Real.log (|b| * q.2) -
      g ((|b|/b)*((d+b*x i-(d+b*q.1))/(|b| * q.2))))) = _
    simp only [LocationScale.logLik,Real.log_mul hB.ne' hs.ne',hterm]
    have ht (i : Fin n) : -(Real.log |b| + Real.log q.2) - g ((x i-q.1)/q.2) =
        (-Real.log q.2-g ((x i-q.1)/q.2))-Real.log |b| := by ring
    rw [Finset.sum_congr rfl (fun i _ ↦ ht i),Finset.sum_sub_distrib]
    simp [nsmul_eq_mul]
  have h := isMaxOn_iff_of_bijOn (f := LocationScale.logLik g x)
    (g := fun q ↦ L q + (n : ℝ)*Real.log |b|) hbij
    (fun q hq ↦ (sub_add_cancel _ _).symm.trans (congrArg
      (fun t ↦ t+(n : ℝ)*Real.log |b|) (he q hq).symm)) hp
  simpa only [isMaxOn_iff,add_le_add_iff_right] using h

/-- An attainment criterion at a reachable height also characterizes
nonattainment by the exact least upper bound and strict loss at every
feasible point. -/
theorem nonattainment_iff_of_face {P A R : Type*} [LinearOrder R] [TopologicalSpace R]
    [OrderClosedTopology R] {S : Set P} {L : P → R} {B : R}
    {l : Filter A} [l.NeBot] {path : A → P}
    (hpath : ∀ᶠ a in l, path a ∈ S)
    (hlim : Tendsto (L ∘ path) l (𝓝 B))
    (hcriterion : (∃ p ∈ S, IsMaxOn L S p) ↔ ∃ p ∈ S, B ≤ L p) :
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B := by
  rw [hcriterion]
  constructor
  · intro hn
    have hlt (p : P) (hp : p ∈ S) : L p < B :=
      lt_of_not_ge (fun h ↦ hn ⟨p, hp, h⟩)
    refine ⟨hlt, ?_, ?_⟩
    · rintro _ ⟨p, hp, rfl⟩
      exact (hlt p hp).le
    · intro y hy
      exact le_of_tendsto hlim (hpath.mono fun _ hp ↦ hy (mem_image_of_mem L hp))
  · rintro ⟨hlt, _⟩ ⟨p, hp, h⟩
    exact (hlt p hp).not_ge h

end UnivariateML.LocScaleCommon

end
