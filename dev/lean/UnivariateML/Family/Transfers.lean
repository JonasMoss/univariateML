module

public import UnivariateML.Family.Gamma
public import UnivariateML.Family.Lomax
public import UnivariateML.Generic.Limit

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Exact likelihood transfers

A bijection of feasible parameter sets and an additive data term transport
maximizers and finite least upper bounds.

## Deviations

Inverse gamma uses mean log likelihood, as in Gamma. The other families use
total log likelihood. Pareto II has a fixed threshold and strictly positive
excesses. Its dossier's blanket strict-support statement conflicts with
its separate endpoint rule. Burr has fixed positive power. The statements
concern exact real arithmetic.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Transfer

noncomputable section

variable {α β : Type*} {D : Set α} {E : Set β} {φ : α → β}
  {f : α → ℝ} {g : β → ℝ} {c : ℝ}

/-- Feasible maximizers correspond pointwise under the parameter bijection. -/
theorem isMaxOn_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c)
    {a : α} (ha : a ∈ D) : IsMaxOn f D a ↔ IsMaxOn g E (φ a) := by
  constructor
  · intro hm b hb
    obtain ⟨z, hz, rfl⟩ := hφ.surjOn hb
    have hle : f z ≤ f a := hm hz
    rw [h z hz, h a ha] at hle
    exact (add_le_add_iff_right c).mp hle
  · intro hm z hz
    change f z ≤ f a
    rw [h z hz, h a ha]
    exact (add_le_add_iff_right c).mpr (hm (hφ.mapsTo hz))

/-- The parameter map is a bijection of the full maximizer sets. -/
theorem bijOn_maximizers (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    BijOn φ {a ∈ D | IsMaxOn f D a} {b ∈ E | IsMaxOn g E b} := by
  refine ⟨fun a ha ↦ ⟨hφ.mapsTo ha.1, (isMaxOn_iff hφ h ha.1).mp ha.2⟩,
    fun a ha b hb hab ↦ hφ.injOn ha.1 hb.1 hab, ?_⟩
  intro b hb
  obtain ⟨a, ha, rfl⟩ := hφ.surjOn hb.1
  exact ⟨a, ⟨ha, (isMaxOn_iff hφ h ha).mpr hb.2⟩, rfl⟩

theorem exists_max_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    (∃ a ∈ D, IsMaxOn f D a) ↔ (∃ b ∈ E, IsMaxOn g E b) := by
  have H := bijOn_maximizers hφ h
  constructor
  · rintro ⟨a, ha, hm⟩
    exact ⟨φ a, H.mapsTo ⟨ha, hm⟩⟩
  · rintro ⟨b, hb, hm⟩
    obtain ⟨a, ha, _⟩ := H.surjOn ⟨hb, hm⟩
    exact ⟨a, ha⟩

theorem existsUnique_max_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    (∃! a, a ∈ D ∧ IsMaxOn f D a) ↔ (∃! b, b ∈ E ∧ IsMaxOn g E b) := by
  have H := bijOn_maximizers hφ h
  constructor
  · rintro ⟨a, ha, hu⟩
    refine ⟨φ a, H.mapsTo ha, ?_⟩
    intro b hb
    obtain ⟨z, hz, rfl⟩ := H.surjOn hb
    exact congrArg φ (hu z hz)
  · rintro ⟨b, hb, hu⟩
    obtain ⟨a, ha, hab⟩ := H.surjOn hb
    exact ⟨a, ha, fun z hz ↦ H.injOn hz ha ((hu (φ z) (H.mapsTo hz)).trans hab.symm)⟩

theorem image_eq (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    f '' D = (fun v ↦ v + c) '' (g '' E) := by
  ext v
  constructor
  · rintro ⟨a, ha, rfl⟩
    exact ⟨g (φ a), ⟨φ a, hφ.mapsTo ha, rfl⟩, (h a ha).symm⟩
  · rintro ⟨_, ⟨b, hb, rfl⟩, rfl⟩
    obtain ⟨a, ha, rfl⟩ := hφ.surjOn hb
    exact ⟨a, ha, h a ha⟩

theorem isLUB_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) (u : ℝ) :
    IsLUB (f '' D) (u + c) ↔ IsLUB (g '' E) u := by
  rw [image_eq hφ h]
  exact (OrderIso.addRight c).isLUB_image'

theorem sSup_eq (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c)
    (hE : E.Nonempty) (hB : BddAbove (g '' E)) :
    sSup (f '' D) = sSup (g '' E) + c := by
  have hL := (isLUB_iff hφ h _).mpr (isLUB_csSup (hE.image g) hB)
  exact hL.csSup_eq (image_eq hφ h ▸ ((hE.image g).image (fun v ↦ v + c)))

theorem attained_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) (u : ℝ) :
    u + c ∈ f '' D ↔ u ∈ g '' E := by
  rw [image_eq hφ h]
  constructor
  · rintro ⟨v, hv, he⟩
    rwa [(add_right_cancel he : v = u)] at hv
  · intro hu
    exact ⟨u, hu, rfl⟩

theorem bddAbove_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    BddAbove (f '' D) ↔ BddAbove (g '' E) := by
  rw [image_eq hφ h]
  constructor
  · rintro ⟨b, hb⟩
    exact ⟨b - c, fun v hv ↦ (le_sub_iff_add_le).mpr (hb ⟨v, hv, rfl⟩)⟩
  · rintro ⟨b, hb⟩
    refine ⟨b + c, ?_⟩
    rintro _ ⟨v, hv, rfl⟩
    exact (add_le_add_iff_right c).mpr (hb hv)

theorem unattained_sup_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c)
    (hE : E.Nonempty) (hB : BddAbove (g '' E)) :
    sSup (f '' D) ∉ f '' D ↔ sSup (g '' E) ∉ g '' E := by
  rw [sSup_eq hφ h hE hB]
  exact (attained_iff hφ h _).not

/-- A feasible source path supplies the corresponding bound in target likelihood units. -/
theorem limit_le_upperBound {ι : Type*} {F : Filter ι} [F.NeBot] {p : ι → β} {u b : ℝ}
    (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c)
    (hp : ∀ᶠ i in F, p i ∈ E) (hu : Tendsto (fun i ↦ g (p i)) F (𝓝 u))
    (hb : b ∈ upperBounds (f '' D)) : u + c ≤ b := by
  have hb' : b - c ∈ upperBounds (g '' E) := by
    intro v hv
    apply (le_sub_iff_add_le).mpr
    apply hb
    rw [image_eq hφ h]
    exact ⟨v, hv, rfl⟩
  exact (le_sub_iff_add_le).mp (Generic.limit_le_upperBound hp hu hb')

variable {n : ℕ}

private theorem bijOn_id (S : Set α) : BijOn id S S :=
  ⟨fun _ h ↦ h, fun _ _ _ _ h ↦ h, fun a ha ↦ ⟨a, ha, rfl⟩⟩

private theorem bijOn_pos {m inv : ℝ → ℝ}
    (hm : ∀ s > 0, 0 < m s) (hi : ∀ r > 0, 0 < inv r)
    (hl : ∀ s > 0, inv (m s) = s) (hr : ∀ r > 0, m (inv r) = r) :
    BijOn m (Ioi 0) (Ioi 0) :=
  ⟨hm, fun s hs t ht h ↦ (hl s hs).symm.trans ((congrArg inv h).trans (hl t ht)),
    fun r hr' ↦ ⟨inv r, hi r hr', hr r hr'⟩⟩

private theorem bijOn_inv : BijOn (fun s : ℝ ↦ s⁻¹) (Ioi 0) (Ioi 0) :=
  bijOn_pos (fun _ ↦ inv_pos.mpr) (fun _ ↦ inv_pos.mpr)
    (fun _ _ ↦ inv_inv _) (fun _ _ ↦ inv_inv _)

private theorem bijOn_pair {m : ℝ → ℝ} (hm : BijOn m (Ioi 0) (Ioi 0)) :
    BijOn (fun p : ℝ × ℝ ↦ (p.1, m p.2)) Lomax.domain Lomax.domain := by
  refine ⟨fun p hp ↦ ⟨hp.1, hm.mapsTo hp.2⟩, ?_, ?_⟩
  · intro p hp q hq he
    change (p.1, m p.2) = (q.1, m q.2) at he
    injection he with hshape hscale
    exact Prod.ext hshape (hm.injOn hp.2 hq.2 hscale)
  · rintro ⟨k, r⟩ hq
    obtain ⟨s, hs, he⟩ := hm.surjOn hq.2
    exact ⟨(k, s), ⟨hq.1, hs⟩, Prod.ext rfl he⟩

private theorem unique_of_strict {l : α → ℝ} {S : Set α} {a : α} (ha : a ∈ S)
    (h : ∀ b ∈ S, b ≠ a → l b < l a) :
    ∃! b, b ∈ S ∧ IsMaxOn l S b := by
  refine ⟨a, ⟨ha, ?_⟩, ?_⟩
  · intro b hb
    by_cases he : b = a
    · subst b
      exact le_rfl
    · exact (h b hb he).le
  · intro b hb
    by_contra he
    exact (h b hb.1 he).not_ge (hb.2 ha)

/-- Reciprocal observations. -/
def reciprocal (x : Fin n → ℝ) (i : Fin n) : ℝ := (x i)⁻¹

private theorem reciprocal_pos {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    ∀ i, 0 < reciprocal x i := fun i ↦ inv_pos.mpr (hx i)

/-- Total log-Jacobian of reciprocation. -/
def reciprocalJac (x : Fin n → ℝ) : ℝ := ∑ i, -2 * log (x i)

namespace InvGamma

def logDensity (a b t : ℝ) : ℝ :=
  a * log b - log (Real.Gamma a) - (a + 1) * log t - b / t

def logLik (x : Fin n → ℝ) (a b : ℝ) : ℝ := Gamma.avg (fun i ↦ logDensity a b (x i))

theorem logLik_transfer (hn : 0 < n) (x : Fin n → ℝ) (a b : ℝ) :
    logLik x a b = Gamma.logLik (reciprocal x) a b + reciprocalJac x / n := by
  rw [Gamma.logLik_eq_avg hn]
  unfold logLik Gamma.avg reciprocalJac
  rw [← add_div, ← Finset.sum_add_distrib]
  apply congrArg (fun t : ℝ ↦ t / n)
  apply Finset.sum_congr rfl
  intro i _
  simp only [logDensity, Gamma.logDensity, reciprocal, log_inv, div_eq_mul_inv]
  ring_nf

theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Gamma.domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) Gamma.domain p := by
  apply (existsUnique_max_iff (g := fun p ↦ Gamma.logLik (reciprocal x) p.1 p.2)
    (bijOn_id Gamma.domain)
    (fun p _ ↦ logLik_transfer hn x p.1 p.2)).mpr
  obtain ⟨i, j, hij⟩ := hne
  exact Gamma.jointMLE hn (reciprocal_pos hx) ⟨i, j, fun h ↦ hij (inv_injective h)⟩

theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun k ↦ logLik x k b) (Ioi 0) a :=
  (existsUnique_max_iff (bijOn_id (Ioi 0)) (fun a _ ↦ logLik_transfer hn x a b)).mpr
    (Gamma.heldRate (reciprocal x) hb)

theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x a) (Ioi 0) b := by
  have H := Gamma.heldShape hn (reciprocal_pos hx) ha
  exact (existsUnique_max_iff (bijOn_id (Ioi 0)) (fun b _ ↦ logLik_transfer hn x a b)).mpr
    (unique_of_strict H.1 H.2)

theorem fullyHeld_unique (x : Fin n → ℝ) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∃! p : ℝ × ℝ, p ∈ Gamma.domain ∩ {(a, b)} ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2) (Gamma.domain ∩ {(a, b)}) p := by
  apply unique_of_strict (show (a, b) ∈ Gamma.domain ∩ {(a, b)} from
    ⟨⟨ha, hb⟩, mem_singleton _⟩)
  intro q hq hne
  exact (hne (mem_singleton_iff.mp hq.2)).elim

theorem constant_noMLE (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    ¬ ∃ p ∈ Gamma.domain,
      IsMaxOn (fun q ↦ logLik (fun _ : Fin n ↦ t) q.1 q.2) Gamma.domain p :=
  (exists_max_iff (bijOn_id Gamma.domain)
    (fun p _ ↦ logLik_transfer hn (fun _ : Fin n ↦ t) p.1 p.2)).not.mpr
    (Gamma.constant_noMLE hn (inv_pos.mpr ht))

theorem constant_unbounded (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    Tendsto (fun a ↦ logLik (fun _ : Fin n ↦ t) a (a / t⁻¹)) atTop atTop := by
  simp only [logLik_transfer hn]
  unfold reciprocal
  exact tendsto_atTop_add_const_right atTop (reciprocalJac (fun _ : Fin n ↦ t) / n)
    (Gamma.constant_unbounded hn (inv_pos.mpr ht))

end InvGamma

private theorem lomax_exists {y : Fin n → ℝ} (hn : 0 < n) (hy : ∀ i, 0 < y i)
    {l : ℝ → ℝ → ℝ} {m : ℝ → ℝ} {j : ℝ}
    (hm : BijOn m (Ioi 0) (Ioi 0)) (h : ∀ k s, 0 < s → l k s = Lomax.logLik y k (m s) + j) :
    (∃ p ∈ Lomax.domain, IsMaxOn (fun q : ℝ × ℝ ↦ l q.1 q.2) Lomax.domain p) ↔
      ∃ u ∈ Ioo 0 (Lomax.uBar y), Lomax.psi y u ≤ 0 :=
  (exists_max_iff (bijOn_pair hm) (fun p hp ↦ h p.1 p.2 hp.2)).trans
    (Lomax.exists_max_iff hn hy)

private theorem lomax_face {y : Fin n → ℝ} (hn : 0 < n) (hy : ∀ i, 0 < y i)
    {l : ℝ → ℝ → ℝ} {m : ℝ → ℝ} {j : ℝ}
    (hm : BijOn m (Ioi 0) (Ioi 0)) (h : ∀ k s, 0 < s → l k s = Lomax.logLik y k (m s) + j)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar y), Lomax.psi y u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ l p.1 p.2) '' Lomax.domain) = Lomax.bestExp y + j ∧
      ∀ p ∈ Lomax.domain, l p.1 p.2 < Lomax.bestExp y + j := by
  obtain ⟨he, hlt⟩ := Lomax.exponential_sup_of_no_witness hn hy hw
  have hne : Lomax.domain.Nonempty := ⟨(1, 1), ⟨zero_lt_one, zero_lt_one⟩⟩
  have hb : BddAbove ((fun p : ℝ × ℝ ↦ Lomax.logLik y p.1 p.2) '' Lomax.domain) := by
    refine ⟨Lomax.bestExp y, ?_⟩
    rintro _ ⟨p, hp, rfl⟩
    exact (hlt p hp).le
  refine ⟨(sSup_eq (bijOn_pair hm) (fun p hp ↦ h p.1 p.2 hp.2) hne hb).trans
    (congrArg (· + j) he), ?_⟩
  intro p hp
  rw [h p.1 p.2 hp.2]
  exact (add_lt_add_iff_right j).mpr (hlt (p.1, m p.2) ((bijOn_pair hm).mapsTo hp))

private theorem lomax_heldScale {y : Fin n → ℝ} (hn : 0 < n) (hy : ∀ i, 0 < y i)
    {l : ℝ → ℝ} {r j : ℝ} (hr : 0 < r) (h : ∀ k, l k = Lomax.logLik y k r + j) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn l (Ioi 0) k :=
  (existsUnique_max_iff (bijOn_id (Ioi 0)) (fun k _ ↦ h k)).mpr
    (Lomax.heldRate_unique hn hy hr)

private theorem lomax_heldShape {y : Fin n → ℝ} (hn : 0 < n) (hy : ∀ i, 0 < y i)
    {l m : ℝ → ℝ} {k j : ℝ} (hk : 0 < k)
    (hm : BijOn m (Ioi 0) (Ioi 0)) (h : ∀ s > 0, l s = Lomax.logLik y k (m s) + j) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn l (Ioi 0) s :=
  (existsUnique_max_iff hm h).mpr (Lomax.heldShape_unique hn hy hk)

private theorem lomax_constant_face (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {l : ℝ → ℝ → ℝ} {m : ℝ → ℝ} {j : ℝ}
    (hm : BijOn m (Ioi 0) (Ioi 0))
    (h : ∀ k s, 0 < s → l k s = Lomax.logLik (fun _ : Fin n ↦ a) k (m s) + j) :
    sSup ((fun p : ℝ × ℝ ↦ l p.1 p.2) '' Lomax.domain) =
        Lomax.bestExp (fun _ : Fin n ↦ a) + j ∧
      ∀ p ∈ Lomax.domain, l p.1 p.2 < Lomax.bestExp (fun _ : Fin n ↦ a) + j :=
  lomax_face hn (fun _ ↦ ha) hm h
    ((Lomax.exists_max_iff hn (fun _ ↦ ha)).not.mp (Lomax.constant_no_max hn ha))

private theorem lomax_constant_profile_loss (hn : 0 < n) {a r j v : ℝ}
    (ha : 0 < a) (hr : 0 < r)
    (h : v = Lomax.logLik (fun _ : Fin n ↦ a) (Lomax.shape (fun _ : Fin n ↦ a) r) r + j) :
    let z := r * a
    (Lomax.bestExp (fun _ : Fin n ↦ a) + j - v) / n = log ((1 + z) * log (1 + z) / z) ∧
      0 < log ((1 + z) * log (1 + z) / z) := by
  rw [h, add_sub_add_right_eq_sub]
  exact Lomax.constant_profile_loss hn ha hr

private theorem lomax_limit {y : Fin n → ℝ} (hy : ∀ i, 0 ≤ y i)
    {l : ℝ → ℝ} {r j : ℝ} (hr : 0 < r)
    (h : ∀ᶠ k : ℝ in atTop, l k = Lomax.logLik y k (r / k) + j) :
    Tendsto l atTop (𝓝 (Lomax.expLogLik y r + j)) :=
  ((Lomax.logLik_tendsto y hy hr).add_const j).congr' (h.mono fun _ he ↦ he.symm)

namespace InvPareto

def logDensity (k s t : ℝ) : ℝ :=
  log k + log s - 2 * log t - (k + 1) * log (1 + s / t)

def logLik (x : Fin n → ℝ) (k s : ℝ) : ℝ := ∑ i, logDensity k s (x i)

def bestInvExp (x : Fin n → ℝ) : ℝ := Lomax.bestExp (reciprocal x) + reciprocalJac x

theorem logLik_transfer (x : Fin n → ℝ) (k s : ℝ) :
    logLik x k s = Lomax.logLik (reciprocal x) k s + reciprocalJac x := by
  unfold logLik Lomax.logLik reciprocalJac
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  simp only [logDensity, Lomax.logDensity, reciprocal, div_eq_mul_inv]
  ring_nf

theorem exists_max_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃ p ∈ Lomax.domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Lomax.domain p) ↔
      ∃ u ∈ Ioo 0 (Lomax.uBar (reciprocal x)), Lomax.psi (reciprocal x) u ≤ 0 :=
  lomax_exists hn (reciprocal_pos hx) (bijOn_id (Ioi 0)) (fun k s _ ↦ logLik_transfer x k s)

theorem exponential_sup_of_no_witness (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (reciprocal x)), Lomax.psi (reciprocal x) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' Lomax.domain) = bestInvExp x ∧
      ∀ p ∈ Lomax.domain, logLik x p.1 p.2 < bestInvExp x :=
  lomax_face hn (reciprocal_pos hx) (bijOn_id (Ioi 0)) (fun k s _ ↦ logLik_transfer x k s) hw

theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) k :=
  lomax_heldScale hn (reciprocal_pos hx) hs (fun k ↦ logLik_transfer x k s)

theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k) (Ioi 0) s :=
  lomax_heldShape hn (reciprocal_pos hx) hk (bijOn_id (Ioi 0)) (fun s _ ↦ logLik_transfer x k s)

theorem constant_face (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    let x : Fin n → ℝ := fun _ ↦ t
    sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' Lomax.domain) = bestInvExp x ∧
      ∀ p ∈ Lomax.domain, logLik x p.1 p.2 < bestInvExp x :=
  lomax_constant_face hn (inv_pos.mpr ht) (bijOn_id (Ioi 0))
    (fun k s _ ↦ logLik_transfer (fun _ : Fin n ↦ t) k s)

theorem constant_profile_loss (hn : 0 < n) {t s : ℝ} (ht : 0 < t) (hs : 0 < s) :
    let x : Fin n → ℝ := fun _ ↦ t
    let z := s * t⁻¹
    (bestInvExp x - logLik x (Lomax.shape (reciprocal x) s) s) / n =
        log ((1 + z) * log (1 + z) / z) ∧ 0 < log ((1 + z) * log (1 + z) / z) :=
  lomax_constant_profile_loss hn (inv_pos.mpr ht) hs (logLik_transfer _ _ _)

theorem face_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {r : ℝ} (hr : 0 < r) :
    Tendsto (fun k : ℝ ↦ logLik x k (r / k)) atTop
      (𝓝 (Lomax.expLogLik (reciprocal x) r + reciprocalJac x)) :=
  lomax_limit (fun i ↦ (reciprocal_pos hx i).le) hr
    (Eventually.of_forall fun k ↦ logLik_transfer x k (r / k))

end InvPareto

namespace ParetoII

def excess (x : Fin n → ℝ) (μ : ℝ) (i : Fin n) : ℝ := x i - μ

def logDensity (μ k s t : ℝ) : ℝ :=
  log k - log s - (k + 1) * log (1 + (t - μ) / s)

def logLik (x : Fin n → ℝ) (μ k s : ℝ) : ℝ := ∑ i, logDensity μ k s (x i)

def bestShiftedExp (x : Fin n → ℝ) (μ : ℝ) : ℝ := Lomax.bestExp (excess x μ)

theorem logLik_transfer (x : Fin n → ℝ) (μ k s : ℝ) :
    logLik x μ k s = Lomax.logLik (excess x μ) k s⁻¹ := by
  unfold logLik Lomax.logLik
  apply Finset.sum_congr rfl
  intro i _
  simp only [logDensity, Lomax.logDensity, excess, log_inv, div_eq_mul_inv]
  ring_nf

theorem exists_max_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i) :
    (∃ p ∈ Lomax.domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik x μ q.1 q.2) Lomax.domain p) ↔
      ∃ u ∈ Ioo 0 (Lomax.uBar (excess x μ)), Lomax.psi (excess x μ) u ≤ 0 :=
  lomax_exists hn (fun i ↦ sub_pos.mpr (hx i)) bijOn_inv
    (fun k s _ ↦ (logLik_transfer x μ k s).trans (add_zero _).symm)

theorem exponential_sup_of_no_witness (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (excess x μ)), Lomax.psi (excess x μ) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x μ p.1 p.2) '' Lomax.domain) = bestShiftedExp x μ ∧
      ∀ p ∈ Lomax.domain, logLik x μ p.1 p.2 < bestShiftedExp x μ := by
  simpa [bestShiftedExp] using lomax_face (y := excess x μ) hn
    (fun i ↦ sub_pos.mpr (hx i)) bijOn_inv
    (j := 0) (fun k s _ ↦ (logLik_transfer x μ k s).trans (add_zero _).symm) hw

theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {s : ℝ} (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik x μ a s) (Ioi 0) k :=
  lomax_heldScale hn (fun i ↦ sub_pos.mpr (hx i)) (inv_pos.mpr hs)
    (fun k ↦ (logLik_transfer x μ k s).trans (add_zero _).symm)

theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {k : ℝ} (hk : 0 < k) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x μ k) (Ioi 0) s :=
  lomax_heldShape hn (fun i ↦ sub_pos.mpr (hx i)) hk bijOn_inv
    (fun s _ ↦ (logLik_transfer x μ k s).trans (add_zero _).symm)

theorem constant_face (hn : 0 < n) {μ t : ℝ} (ht : μ < t) :
    let x : Fin n → ℝ := fun _ ↦ t
    sSup ((fun p : ℝ × ℝ ↦ logLik x μ p.1 p.2) '' Lomax.domain) = bestShiftedExp x μ ∧
      ∀ p ∈ Lomax.domain, logLik x μ p.1 p.2 < bestShiftedExp x μ := by
  dsimp only
  unfold bestShiftedExp excess
  simpa only [add_zero] using lomax_constant_face hn (sub_pos.mpr ht) bijOn_inv
    (j := 0) (fun k s _ ↦ (logLik_transfer (fun _ : Fin n ↦ t) μ k s).trans (add_zero _).symm)

theorem constant_profile_loss (hn : 0 < n) {μ t s : ℝ} (ht : μ < t) (hs : 0 < s) :
    let x : Fin n → ℝ := fun _ ↦ t
    let z := s⁻¹ * (t - μ)
    (bestShiftedExp x μ - logLik x μ (Lomax.shape (excess x μ) s⁻¹) s) / n =
        log ((1 + z) * log (1 + z) / z) ∧ 0 < log ((1 + z) * log (1 + z) / z) := by
  dsimp only
  unfold bestShiftedExp excess
  simpa only [add_zero] using lomax_constant_profile_loss hn (sub_pos.mpr ht)
    (inv_pos.mpr hs) (j := 0) ((logLik_transfer (fun _ : Fin n ↦ t) μ _ s).trans (add_zero _).symm)

theorem face_path {x : Fin n → ℝ} {μ r : ℝ} (hx : ∀ i, μ < x i) (hr : 0 < r) :
    Tendsto (fun k : ℝ ↦ logLik x μ k (k / r)) atTop
      (𝓝 (Lomax.expLogLik (excess x μ) r)) := by
  suffices Tendsto (fun k : ℝ ↦ logLik x μ k (k / r)) atTop
      (𝓝 (Lomax.expLogLik (excess x μ) r + 0)) by simpa using this
  apply lomax_limit (y := excess x μ) (fun i ↦ sub_nonneg.mpr (hx i).le) hr
  exact Eventually.of_forall fun k ↦ by rw [logLik_transfer, inv_div, add_zero]

end ParetoII

namespace Burr

def powered (x : Fin n → ℝ) (c : ℝ) (i : Fin n) : ℝ := x i ^ c

def powerJac (x : Fin n → ℝ) (c : ℝ) : ℝ := ∑ i, (log c + (c - 1) * log (x i))

def rate (c s : ℝ) : ℝ := (s ^ c)⁻¹

def logDensity (c k s t : ℝ) : ℝ :=
  log k + log c - log t - c * log (s / t) - (k + 1) * log (1 + (t / s) ^ c)

def logLik (x : Fin n → ℝ) (c k s : ℝ) : ℝ := ∑ i, logDensity c k s (x i)

def bestWeibull (x : Fin n → ℝ) (c : ℝ) : ℝ :=
  Lomax.bestExp (powered x c) + powerJac x c

private theorem powered_pos {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (c : ℝ) :
    ∀ i, 0 < powered x c i := fun i ↦ rpow_pos_of_pos (hx i) _

private theorem rate_pos {c s : ℝ} (hs : 0 < s) : 0 < rate c s :=
  inv_pos.mpr (rpow_pos_of_pos hs c)

private theorem rate_bijOn {c : ℝ} (hc : 0 < c) : BijOn (rate c) (Ioi 0) (Ioi 0) := by
  apply bijOn_pos (inv := fun r ↦ r⁻¹ ^ c⁻¹)
    (fun _ hs ↦ rate_pos hs) (fun _ hr ↦ rpow_pos_of_pos (inv_pos.mpr hr) _)
  · intro s hs
    rw [rate, inv_inv, ← rpow_mul hs.le, mul_inv_cancel₀ hc.ne', rpow_one]
  · intro r hr
    rw [rate, ← rpow_mul (inv_pos.mpr hr).le, inv_mul_cancel₀ hc.ne', rpow_one, inv_inv]

theorem logLik_transfer {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (c k : ℝ)
    {s : ℝ} (hs : 0 < s) :
    logLik x c k s = Lomax.logLik (powered x c) k (rate c s) + powerJac x c := by
  unfold logLik Lomax.logLik powerJac
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  simp only [logDensity, Lomax.logDensity, powered, rate]
  rw [log_div hs.ne' (hx i).ne', log_inv, log_rpow hs, div_rpow (hx i).le hs.le,
    div_eq_mul_inv, mul_comm (x i ^ c) ((s ^ c)⁻¹)]
  ring_nf

theorem exists_max_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c) :
    (∃ p ∈ Lomax.domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik x c q.1 q.2) Lomax.domain p) ↔
      ∃ u ∈ Ioo 0 (Lomax.uBar (powered x c)), Lomax.psi (powered x c) u ≤ 0 :=
  lomax_exists hn (powered_pos hx c) (rate_bijOn hc) (fun k _ hs ↦ logLik_transfer hx c k hs)

theorem exponential_sup_of_no_witness (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (powered x c)), Lomax.psi (powered x c) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x c p.1 p.2) '' Lomax.domain) = bestWeibull x c ∧
      ∀ p ∈ Lomax.domain, logLik x c p.1 p.2 < bestWeibull x c :=
  lomax_face hn (powered_pos hx c) (rate_bijOn hc) (fun k _ hs ↦ logLik_transfer hx c k hs) hw

theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik x c a s) (Ioi 0) k :=
  lomax_heldScale hn (powered_pos hx c) ((rate_bijOn hc).mapsTo hs)
    (fun k ↦ logLik_transfer hx c k hs)

theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c k : ℝ} (hc : 0 < c) (hk : 0 < k) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x c k) (Ioi 0) s :=
  lomax_heldShape hn (powered_pos hx c) hk (rate_bijOn hc) (fun _ hs ↦ logLik_transfer hx c k hs)

theorem constant_face (hn : 0 < n) {t c : ℝ} (ht : 0 < t) (hc : 0 < c) :
    let x : Fin n → ℝ := fun _ ↦ t
    sSup ((fun p : ℝ × ℝ ↦ logLik x c p.1 p.2) '' Lomax.domain) = bestWeibull x c ∧
      ∀ p ∈ Lomax.domain, logLik x c p.1 p.2 < bestWeibull x c :=
  lomax_constant_face hn (rpow_pos_of_pos ht c) (rate_bijOn hc)
    (fun k _ hs ↦ logLik_transfer (fun _ : Fin n ↦ ht) c k hs)

theorem constant_profile_loss (hn : 0 < n) {t c s : ℝ}
    (ht : 0 < t) (_hc : 0 < c) (hs : 0 < s) :
    let x : Fin n → ℝ := fun _ ↦ t
    let z := rate c s * t ^ c
    (bestWeibull x c - logLik x c (Lomax.shape (powered x c) (rate c s)) s) / n =
        log ((1 + z) * log (1 + z) / z) ∧ 0 < log ((1 + z) * log (1 + z) / z) :=
  lomax_constant_profile_loss hn (rpow_pos_of_pos ht c) (rate_pos hs)
    (logLik_transfer (fun _ : Fin n ↦ ht) c _ hs)

theorem face_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c r : ℝ} (hc : 0 < c) (hr : 0 < r) :
    Tendsto (fun k : ℝ ↦ logLik x c k ((k / r) ^ c⁻¹)) atTop
      (𝓝 (Lomax.expLogLik (powered x c) r + powerJac x c)) := by
  apply lomax_limit (fun i ↦ (powered_pos hx c i).le) hr
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
  have hkr := div_pos hk hr
  rw [logLik_transfer hx c k (rpow_pos_of_pos hkr _), rate,
    ← rpow_mul hkr.le, inv_mul_cancel₀ hc.ne', rpow_one, inv_div]

end Burr

end

end UnivariateML.Transfer
