module

public import UnivariateML.Family.TruncZero

/-! Zero-modifier transfers for beta-binomial, beta-negative binomial, CMP,
discrete gamma, discrete Weibull and Poisson inverse Gaussian.

Deviations: the parent chart is supplied by the caller. The set of compatible
coefficients can incorporate any parent holds. The conditional likelihood
and its maximizing status are inputs to the transfer. -/

@[expose] public section

open Set Filter
open scoped Topology

namespace UnivariateML.Trunc3Zero

open Truncation

/-- With a compatible held zero mass the positive-part maximizers carry
over exactly. With all zeros the empty product makes every parent tie. -/
theorem modified_held {P : Type*} {D : Set P} {z k : ℕ} {w : ℝ}
    (hw : 0 < bernoulli z k w) (x : Fin k → ℕ) (g : P → ℕ → ℝ) (p : P) :
    IsMaxOn (fun q ↦ modifiedLikelihood z x g (q,w)) D p ↔
      IsMaxOn (fun q ↦ ∏ i, g q (x i)) D p := by
  simp only [modified_factorization]
  exact held_modified_max_iff hw

/-- Free modification pairs every positive-part maximizer with the
empirical zero fraction. -/
theorem modified_free {P : Type*} {D : Set P} {z k : ℕ} (hn : 0 < z+k)
    {L : P → ℝ} (hL : ∀ q ∈ D, 0 ≤ L q) (hpos : ∃ q ∈ D, 0 < L q)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ Icc 0 1) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z k t.2 * L t.1) (D ×ˢ Icc 0 1) (p,w)
      ↔ IsMaxOn L D p ∧ w = zeroFraction z k :=
  modified_max_iff hn hL hpos hp hw

/-- On all-zero data every compatible parent coefficient fits at free
zero mass one. -/
theorem modified_zero {P : Type*} {D : Set P} {z : ℕ} (hz : 0 < z)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ Icc 0 1) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z 0 t.2) (D ×ˢ Icc 0 1) (p,w) ↔ w = 1 :=
  TruncZero.modified_all_zero hz hp hw

/-- Whenever a compatible path has a positive-part likelihood limit,
its zero-modified limit is the product with the limiting Bernoulli factor. -/
theorem modified_face_limit {P : Type*} {F : Filter P} {z k : ℕ}
    {w L : P → ℝ} {w₀ L₀ : ℝ}
    (hw : Tendsto w F (𝓝 w₀)) (hL : Tendsto L F (𝓝 L₀)) :
    Tendsto (fun t ↦ bernoulli z k (w t) * L t) F (𝓝 (bernoulli z k w₀ * L₀)) := by
  have h : Continuous (bernoulli z k) := by unfold bernoulli; fun_prop
  simpa only [Function.comp_def, Pi.mul_def] using (h.continuousAt.tendsto.comp hw).mul hL

/-- For a parent with a nonzero positive part, free inflation clips the
effective zero fraction at the parent zero mass. -/
theorem inflated_clip {z k : ℕ} (hn : 0 < z+k) {f rho L : ℝ}
    (hf : 0 ≤ f) (hf' : f < 1) (hrho : rho ∈ Icc 0 1) (hL : 0 < L) :
    IsMaxOn (fun r ↦ bernoulli z k (f + (1-f)*r) * L) (Icc 0 1) rho ↔
      f + (1-f)*rho = max f (zeroFraction z k) := by
  have hw := (inflation_weight_iff hf hf').mp ⟨rho, hrho, rfl⟩
  rw [← bernoulli_clip hn ⟨hf, hf'.le⟩ hw]
  constructor
  · intro H w hw
    obtain ⟨r, hr, rfl⟩ := (inflation_weight_iff hf hf').mpr hw
    exact (mul_le_mul_iff_left₀ hL).mp (H hr)
  · intro H r hr
    exact mul_le_mul_of_nonneg_right (H ((inflation_weight_iff hf hf').mp ⟨r,hr,rfl⟩)) hL.le

private theorem effective_zero_mem {f rho : ℝ} (hf : f ∈ Icc (0 : ℝ) 1)
    (hr : rho ∈ Icc (0 : ℝ) 1) : f + (1-f)*rho ∈ Icc (0 : ℝ) 1 :=
  ⟨add_nonneg hf.1 (mul_nonneg (sub_nonneg.mpr hf.2) hr.1), by
    nlinarith [mul_nonneg (sub_nonneg.mpr hf.2) (sub_nonneg.mpr hr.2)]⟩

/-- A conditional maximizer transfers whenever its empirical effective
zero mass can be realized. The inflation set may be a singleton or the
full unit interval. -/
theorem inflated_transfer {P : Type*} {D : Set P} {R : Set ℝ}
    {z k : ℕ} (hn : 0 < z+k) {f L : P → ℝ}
    (hf : ∀ q ∈ D, f q ∈ Icc 0 1) (hR : R ⊆ Icc 0 1)
    (hL : ∀ q ∈ D, 0 ≤ L q)
    {p : P} {rho : ℝ} (hp : p ∈ D) (hr : rho ∈ R)
    (hmax : IsMaxOn L D p)
    (he : f p + (1-f p)*rho = zeroFraction z k) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z k (f t.1 + (1-f t.1)*t.2) * L t.1)
      (D ×ˢ R) (p,rho) := by
  have H {q : P} {r : ℝ} (hq : q ∈ D) (hr : r ∈ R) :
      f q + (1-f q)*r ∈ Icc (0 : ℝ) 1 :=
    effective_zero_mem (hf q hq) (hR hr)
  have hb := (bernoulli_max_iff hn (H hp hr)).mpr he
  intro t ht
  exact mul_le_mul (hb (H ht.1 ht.2)) (hmax ht.1) (hL _ ht.1)
    (mul_nonneg (pow_nonneg (H hp hr).1 _) (pow_nonneg (sub_nonneg.mpr (H hp hr).2) _))

/-- Once a positive-part winner can realize the empirical effective zero
weight, all inflated winners are precisely the positive-part winners
that realize it. The allowed inflation set can also be a singleton. -/
theorem inflated_saturated_max_iff {P : Type*} {D : Set P} {R : Set ℝ}
    {z k : ℕ} (hn : 0 < z+k) {f L : P → ℝ}
    (hf : ∀ q ∈ D, f q ∈ Icc 0 1) (hR : R ⊆ Icc 0 1)
    (hL : ∀ q ∈ D, 0 ≤ L q) (hpos : ∃ q ∈ D, 0 < L q)
    {q : P} {r : ℝ} (hq : q ∈ D) (hr : r ∈ R)
    (hmax : IsMaxOn L D q) (he : f q + (1-f q)*r = zeroFraction z k)
    {p : P} {rho : ℝ} (hp : p ∈ D) (hrho : rho ∈ R) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z k (f t.1 + (1-f t.1)*t.2) * L t.1)
      (D ×ˢ R) (p,rho) ↔
    IsMaxOn L D p ∧ f p + (1-f p)*rho = zeroFraction z k := by
  let w := fun p rho ↦ f p + (1-f p)*rho
  let B := fun t : P × ℝ ↦ bernoulli z k t.2 * L t.1
  have hw {p : P} {rho : ℝ} (hp : p ∈ D) (hrho : rho ∈ R) :
      w p rho ∈ Icc (0 : ℝ) 1 := effective_zero_mem (hf p hp) (hR hrho)
  have hmodified : IsMaxOn B (D ×ˢ Icc 0 1) (q,w q r) :=
    (modified_free hn hL hpos hq (hw hq hr)).mpr ⟨hmax,he⟩
  constructor
  · intro H
    apply (modified_free hn hL hpos hp (hw hp hrho)).mp
    intro t ht
    have h := H (a := (q,r)) ⟨hq,hr⟩
    exact (hmodified ht).trans h
  · rintro ⟨H,he'⟩
    exact inflated_transfer hn hf hR hL hp hrho H he'

/-- If a positive-part winner has parent zero mass below the empirical
fraction, the displayed free inflation coefficient realizes it. -/
theorem inflated_free_feasible {P : Type*} {D : Set P} {z k : ℕ} (hk : 0 < k)
    {f L : P → ℝ} (hf : ∀ q ∈ D, f q ∈ Icc 0 1)
    (hL : ∀ q ∈ D, 0 ≤ L q) {p : P} (hp : p ∈ D)
    (hmax : IsMaxOn L D p) (hfit : f p ≤ zeroFraction z k) :
    let rho := (zeroFraction z k - f p)/(1-f p)
    rho ∈ Icc 0 1 ∧
      IsMaxOn (fun t : P × ℝ ↦ bernoulli z k (f t.1 + (1-f t.1)*t.2) * L t.1)
        (D ×ˢ Icc 0 1) (p,rho) := by
  have H : (0 : ℝ) < z + k := add_pos_of_nonneg_of_pos (Nat.cast_nonneg _) (by exact_mod_cast hk)
  have hz : zeroFraction z k < 1 :=
    (div_lt_one H).mpr (by linarith [show (0 : ℝ) < k by exact_mod_cast hk])
  have h : 0 < 1-f p := sub_pos.mpr (hfit.trans_lt hz)
  let rho := (zeroFraction z k - f p)/(1-f p)
  have hr : rho ∈ Icc 0 1 := ⟨div_nonneg (sub_nonneg.mpr hfit) h.le,
    (div_le_one h).mpr (by linarith)⟩
  have he : f p + (1-f p)*rho = zeroFraction z k := by
    dsimp [rho]
    field_simp [h.ne']
    ring
  exact ⟨hr, inflated_transfer (by omega) hf Subset.rfl hL hp hr hmax he⟩

/-- Free inflation on all zeros attains at inflation one or an included
parent zero atom. -/
theorem inflated_zero {z : ℕ} (hz : 0 < z) {f rho : ℝ}
    (hf : f ∈ Icc (0 : ℝ) 1) (hrho : rho ∈ Icc (0 : ℝ) 1) :
    (rho + (1-rho)*f)^z = 1 ↔ f = 1 ∨ rho = 1 :=
  TruncZero.inflated_all_zero hz hf hrho

/-- Holding inflation below one preserves the zero-only parent ranking. -/
theorem inflated_zero_held {z : ℕ} (hz : 0 < z) {f g rho : ℝ}
    (hf : 0 ≤ f) (hg : 0 ≤ g) (hrho : rho ∈ Ico 0 1) :
    (rho + (1-rho)*f)^z ≤ (rho + (1-rho)*g)^z ↔ f ≤ g := by
  have h := sub_pos.mpr hrho.2
  have ha := add_nonneg hrho.1 (mul_nonneg h.le hf)
  constructor
  · intro H
    by_contra! hfg
    have hb := add_nonneg hrho.1 (mul_nonneg h.le hg)
    exact (pow_lt_pow_left₀ (by linarith [mul_pos h (sub_pos.mpr hfg)]) hb hz.ne').not_ge H
  · intro H
    exact pow_le_pow_left₀ ha (by linarith [mul_nonneg h.le (sub_nonneg.mpr H)]) _

/-- Inflated parent masses factor through the positive conditional law
when the parent positive mass is nonzero. -/
theorem inflated_factorization {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    (f rho : ℝ) (hf : f < 1) (mass : ℕ → ℝ) :
    (f + (1-f)*rho)^z * (∏ i, (1-rho)*mass (x i)) =
      bernoulli z k (f + (1-f)*rho) * ∏ i, mass (x i)/(1-f) := by
  have h (i : Fin k) :
      (1-rho)*mass (x i) = (1-(f+(1-f)*rho))*(mass (x i)/(1-f)) := by
    field_simp [show 1-f ≠ 0 by linarith]
    ring
  simp only [h, Finset.prod_mul_distrib, Finset.prod_const, Finset.card_univ, Fintype.card_fin,
    bernoulli, mul_assoc]

/-- Without zeros, free inflation keeps the parent optimum at zero
inflation. Strictly positive parent likelihood excludes positive inflation. -/
theorem inflated_no_zeros {P : Type*} {D : Set P} {k : ℕ} (hk : 0 < k)
    {L : P → ℝ} (hL : ∀ q ∈ D, 0 ≤ L q) {p : P} {rho : ℝ}
    (hp : p ∈ D) (hr : rho ∈ Icc 0 1) (hpos : ∃ q ∈ D, 0 < L q) :
    IsMaxOn (fun t : P × ℝ ↦ (1-t.2)^k * L t.1) (D ×ˢ Icc 0 1) (p,rho)
      ↔ IsMaxOn L D p ∧ rho = 0 := by
  obtain ⟨q,hq,hqpos⟩ := hpos
  have h := factorized_max_iff D (Icc 0 1) L (bernoulli 0 k) hp hr hL
    (fun r hr ↦ mul_nonneg (pow_nonneg hr.1 _) (pow_nonneg (sub_nonneg.mpr hr.2) _))
    ⟨q,hq,0,⟨le_rfl,zero_le_one⟩,by simpa [bernoulli] using hqpos⟩
  rw [bernoulli_max_iff (by simpa using hk) hr] at h
  simpa [bernoulli] using h

/-- Without zeros, any held inflation below one preserves
the ordering of parent likelihoods. -/
theorem inflated_no_zeros_held {P : Type*} {D : Set P} {k : ℕ} {L : P → ℝ}
    {rho : ℝ} (hr : rho < 1) (p : P) :
    IsMaxOn (fun q ↦ (1-rho)^k * L q) D p ↔ IsMaxOn L D p := by
  simpa only [bernoulli, pow_zero, one_mul] using
    (held_modified_max_iff (z := 0) (k := k) (w := rho) (D := D) (L := L) (p := p)
      (by simpa [bernoulli] using pow_pos (sub_pos.mpr hr) k))

end UnivariateML.Trunc3Zero

end
