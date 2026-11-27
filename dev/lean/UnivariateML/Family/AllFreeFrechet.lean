module

public import UnivariateML.Family.GEV
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp

set_option linter.style.header false

/-! The Fréchet log-likelihood limit at fixed Gumbel location and scale. -/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.AllFreeFrechet
noncomputable section

variable {n : ℕ}

/-- Keep `σ / λ = a` and `μ + σ = b` as shape diverges. The Fréchet
log likelihood converges to Gumbel at the same location and scale. -/
theorem gumbel_path (x : Fin n → ℝ) (m b : ℝ) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun h : ℝ ↦ Frechet.logLik (GEV.lowerGaps x m) h⁻¹ (m - b + a / h) (a / h))
      (𝓝[>] 0) (𝓝 (GEV.logLik x 0 (b, a))) := by
  simp only [Frechet.logLik, GEV.logLik]
  apply tendsto_finsetSum
  intro i _
  let z := (x i - b) / a
  have hd : HasDerivAt (fun h : ℝ ↦ log (1 + h * z)) z 0 := by
    simpa using (((hasDerivAt_id (0 : ℝ)).mul_const z).const_add 1).log
      (by simp : 1 + id (0 : ℝ) * z ≠ 0)
  have hu : Tendsto (fun h : ℝ ↦ h⁻¹ * log (1 + h * z)) (𝓝[>] 0) (𝓝 z) := by
    simpa using hd.tendsto_slope_zero_right
  have h0 : Tendsto (fun h : ℝ ↦ h) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hl : Tendsto (fun h : ℝ ↦ log (1 + h * z)) (𝓝[>] 0) (𝓝 0) := by
    simpa [Function.comp_def] using hd.continuousAt.tendsto.comp h0
  have hE := Real.continuous_exp.continuousAt.tendsto.comp hu.neg
  have hlim := (((hl.add hu).const_sub (-log a)).sub hE)
  simp only [zero_add] at hlim
  apply hlim.congr'
  have hpos : ∀ᶠ h : ℝ in 𝓝[>] 0, 0 < 1 + h * z := by
    have h := (h0.mul_const z).const_add 1
    exact h.eventually (Ioi_mem_nhds (by simp : (0 : ℝ) < 1 + 0 * z))
  filter_upwards [self_mem_nhdsWithin, hpos] with h (hh : 0 < h) harg
  have hratio : (GEV.lowerGaps x m i + (m - b + a / h)) / (a / h) = 1 + h * z := by
    dsimp [GEV.lowerGaps, z]
    field_simp
    ring
  rw [Frechet.logDensity, hratio, log_inv, log_div ha.ne' hh.ne', rpow_def_of_pos harg]
  simp only [Function.comp_apply]
  congr 1
  · ring
  · congr 1
    ring

/-- Allowing all positive shapes makes the likelihood unbounded whenever
the nonnegative gaps include a zero. A small fixed shape already suffices. -/
theorem unrestricted_unbounded (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) :
    ¬ BddAbove {v | ∃ lam > 0, ∃ p ∈ Frechet.domain, Frechet.logLik g lam p.1 p.2 = v} := by
  let k := (Frechet.minimumCount g : ℝ)
  have hk : 0 < k := by
    obtain ⟨i, hi⟩ := hz
    have h : 0 < Frechet.minimumCount g :=
      Finset.card_pos.mpr ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩⟩
    dsimp [k]
    exact_mod_cast h
  let lam := k / ((n : ℝ) + 1)
  have hl : 0 < lam := div_pos hk (by positivity)
  have he : Frechet.collapseExponent g lam < 0 := by
    unfold Frechet.collapseExponent
    change lam * ((n : ℝ) - k) - k < 0
    have hprod : ((n : ℝ) + 1) * lam = k := by dsimp [lam]; field_simp
    nlinarith [mul_pos hk hl]
  rintro ⟨B, hB⟩
  obtain ⟨p, hp, hlt⟩ := (Frechet.heldShape_unbounded hn hg hz hl he).2 B
  exact hlt.not_ge (hB ⟨lam, hl, p, hp, rfl⟩)

private theorem sinh_le_scaled_cosh {a u : ℝ} (ha : 1 ≤ a) (hu : 0 ≤ u) :
    a * sinh u < cosh (a * u) := by
  have ha0 : 0 < a := zero_lt_one.trans_le ha
  have hc : ConvexOn ℝ (Ici 0) sinh := by
    apply convexOn_of_deriv2_nonneg' (convex_Ici 0) Real.differentiable_sinh.differentiableOn
      (by rw [Real.deriv_sinh]; exact Real.differentiable_cosh.differentiableOn)
    intro v hv
    change 0 ≤ deriv (deriv sinh) v
    rw [Real.deriv_sinh, Real.deriv_cosh]
    exact sinh_nonneg_iff.mpr hv
  have hi : 0 ≤ a⁻¹ := inv_nonneg.mpr ha0.le
  have h1 : 0 ≤ 1 - a⁻¹ := sub_nonneg.mpr (inv_le_one_of_one_le₀ ha)
  have hconc := hc.2 (show 0 ≤ a * u by positivity) (show (0 : ℝ) ≤ 0 by rfl)
    hi h1 (show a⁻¹ + (1 - a⁻¹) = 1 by ring)
  simp only [smul_eq_mul, sinh_zero, mul_zero, add_zero, ← mul_assoc,
    inv_mul_cancel₀ ha0.ne', one_mul] at hconc
  have hle : a * sinh u ≤ sinh (a * u) := by
    have hh := mul_le_mul_of_nonneg_left hconc ha0.le
    simpa only [← mul_assoc, mul_inv_cancel₀ ha0.ne', one_mul] using hh
  have hlt : sinh (a * u) < cosh (a * u) := by
    rw [sinh_eq, cosh_eq]
    linarith [exp_pos (-(a * u))]
  exact hle.trans_lt hlt

/-- The product of two Fréchet densities at distance `d` is strictly below
the half-minimum collapse bound on the fitted shape branch. -/
theorem pair_logLik_lt {a s e d : ℝ} (ha : 1 ≤ a) (hs : 0 < s)
    (he : 0 < e) (hd : 0 < d) :
    Frechet.logDensity a s e + Frechet.logDensity a s (e + d) <
      2 * (log 2 - 1 - log d) := by
  have ha0 := zero_lt_one.trans_le ha
  have hf : 0 < e + d := by positivity
  let m := (log (e + d) + log e) / 2
  let u := (log (e + d) - log e) / 2
  have hu : 0 < u := by dsimp [u]; linarith [log_lt_log he (show e < e + d by linarith)]
  let S := e ^ (-a) + (e + d) ^ (-a)
  have hS : 0 < S := add_pos (rpow_pos_of_pos he _) (rpow_pos_of_pos hf _)
  let U := s ^ a
  have hU : 0 < U := rpow_pos_of_pos hs _
  have hS_eq : S = 2 * exp (-a * m) * cosh (a * u) := by
    dsimp [S]
    rw [rpow_def_of_pos he, rpow_def_of_pos hf, cosh_eq]
    have h1 : log e * -a = -a * m + a * u := by dsimp [m, u]; ring
    have h2 : log (e + d) * -a = -a * m + -(a * u) := by dsimp [m, u]; ring
    rw [h1, h2, exp_add, exp_add]
    ring
  have hd_eq : d = 2 * exp m * sinh u := by
    rw [sinh_eq]
    have h1 : exp m * exp u = e + d := by
      rw [← exp_add, show m + u = log (e + d) by dsimp [m, u]; ring, exp_log hf]
    have h2 : exp m * exp (-u) = e := by
      rw [← exp_add, show m + -u = log e by dsimp [m, u]; ring, exp_log he]
    linarith only [h1, h2]
  have hSgt : a * d < exp ((a + 1) * m) * S := by
    rw [hd_eq, hS_eq]
    have hE : exp ((a + 1) * m) * exp (-a * m) = exp m := by
      rw [← exp_add, show (a + 1) * m + -a * m = m by ring]
    have h := mul_lt_mul_of_pos_left (sinh_le_scaled_cosh ha hu.le)
      (by positivity : 0 < 2 * exp m)
    calc
      _ = 2 * exp m * (a * sinh u) := by ring
      _ < 2 * exp m * cosh (a * u) := h
      _ = _ := by rw [← hE]; ring
  have hL := log_lt_log (mul_pos ha0 hd) hSgt
  rw [log_mul ha0.ne' hd.ne', log_mul (exp_pos _).ne' hS.ne', log_exp] at hL
  have hnorm := log_le_sub_one_of_pos (show 0 < U * S / 2 by positivity)
  rw [log_div (mul_pos hU hS).ne' two_ne_zero, log_mul hU.ne' hS.ne'] at hnorm
  have hform (z : ℝ) (hz : 0 < z) :
      Frechet.logDensity a s z =
        log a + log U - (1 + a) * log z - U * z ^ (-a) := by
    unfold Frechet.logDensity
    rw [log_div hz.ne' hs.ne', div_rpow hz.le hs.le, rpow_neg hs.le, div_inv_eq_mul]
    dsimp [U]
    rw [log_rpow hs]
    ring
  rw [hform e he, hform (e + d) hf]
  dsimp [m] at hL
  dsimp [S] at hnorm
  nlinarith only [hL, hnorm, show (a + 1) * ((log (e + d) + log e) / 2) * 2 =
    (1 + a) * log e + (1 + a) * log (e + d) by ring]

/-- At exactly half minimum multiplicity the fitted Fréchet likelihood
has a finite strict supremum, approached along shape one. -/
theorem half_minimum_supremum {g : Fin n → ℝ} (hn : 0 < n)
    (hg : ∀ i, 0 ≤ g i) (hc : n = 2 * Frechet.minimumCount g) :
    let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (g i)
    IsLUB {v | ∃ a ≥ 1, ∃ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 = v} A ∧
    (∀ a ≥ 1, ∀ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 < A) ∧
    Tendsto (Frechet.profile g 1) (𝓝[>] 0) (𝓝 A) := by
  dsimp only
  let S := Finset.univ.filter fun i ↦ g i = 0
  let T := Finset.univ.filter fun i ↦ g i ≠ 0
  let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (g i)
  have hS : S.card = Frechet.minimumCount g := rfl
  have hST : S.card + T.card = n := by
    simpa [S, T] using (Finset.card_filter_add_card_filter_not
      (s := (Finset.univ : Finset (Fin n))) (fun i ↦ g i = 0))
  have hSc : 0 < S.card := by omega
  have hT : T.card = S.card := by omega
  have hSc' : 0 < (S.card : ℝ) := by exact_mod_cast hSc
  have hzS (i : Fin n) (hi : i ∈ S) : g i = 0 := (Finset.mem_filter.mp hi).2
  have hpT (i : Fin n) (hi : i ∈ T) : 0 < g i :=
    lt_of_le_of_ne (hg i) (Ne.symm (Finset.mem_filter.mp hi).2)
  have hc' : (n : ℝ) = 2 * (S.card : ℝ) := by exact_mod_cast (show n = 2 * S.card by omega)
  have hsumT : ∑ i ∈ T, log (g i) = ∑ i, log (g i) := by
    have he : ∑ i ∈ S, log (g i) = 0 := Finset.sum_eq_zero fun i hi ↦ by simp [hzS i hi]
    have h := Finset.sum_filter_add_sum_filter_not Finset.univ (fun i ↦ g i = 0)
      (fun i ↦ log (g i))
    change (∑ i ∈ S, log (g i)) + (∑ i ∈ T, log (g i)) = _ at h
    rwa [he, zero_add] at h
  have hstrict (a : ℝ) (ha : 1 ≤ a) (p : ℝ × ℝ) (hp : p ∈ Frechet.domain) :
      Frechet.logLik g a p.1 p.2 < A := by
    let f (i : Fin n) := Frechet.logDensity a p.2 (g i + p.1)
    have he : 0 < p.1 := hp.1
    have hs : 0 < p.2 := hp.2
    have hpair (i j : Fin n) (hi : i ∈ S) (hj : j ∈ T) :
        f i + f j < 2 * (log 2 - 1 - log (g j)) := by
      simpa [f, hzS i hi, add_comm (g j) p.1] using pair_logLik_lt ha hs he (hpT j hj)
    have hsum : (∑ j ∈ T, ∑ i ∈ S, (f i + f j)) <
        ∑ j ∈ T, ∑ i ∈ S, 2 * (log 2 - 1 - log (g j)) :=
      Finset.sum_lt_sum_of_nonempty (Finset.card_pos.mp (hT ▸ hSc)) fun j hj ↦
        Finset.sum_lt_sum_of_nonempty (Finset.card_pos.mp hSc) fun i hi ↦ hpair i j hi hj
    have hleft : (∑ j ∈ T, ∑ i ∈ S, (f i + f j)) =
        (S.card : ℝ) * Frechet.logLik g a p.1 p.2 := by
      have heq := Finset.sum_filter_add_sum_filter_not Finset.univ (fun i ↦ g i = 0) f
      change (∑ i ∈ S, f i) + (∑ i ∈ T, f i) = ∑ i, f i at heq
      simp only [Finset.sum_add_distrib, Finset.sum_const, nsmul_eq_mul, hT,
        ← Finset.mul_sum, ← mul_add, heq]
      rfl
    have hright : (∑ j ∈ T, ∑ _i ∈ S, 2 * (log 2 - 1 - log (g j))) = (S.card : ℝ) * A := by
      simp only [Finset.sum_const, nsmul_eq_mul, ← Finset.mul_sum, Finset.sum_sub_distrib,
        hsumT, hT, A]
      rw [hc']
      ring
    rw [hleft, hright] at hsum
    exact lt_of_mul_lt_mul_left hsum hSc'.le
  have hz : ∃ i, g i = 0 := by
    obtain ⟨i, hi⟩ := Finset.card_pos.mp hSc
    exact ⟨i, hzS i hi⟩
  have hc0 : Frechet.collapseExponent g 1 = 0 := by
    unfold Frechet.collapseExponent
    rw [← hS]
    linarith
  have hA : Frechet.collapseValue g 1 = A := by
    unfold Frechet.collapseValue A
    rw [← hS, mul_one, show (n : ℝ) / S.card = 2 by
      apply (div_eq_iff hSc'.ne').2; exact hc']
    ring
  obtain ⟨_, hpath, _, _⟩ := Frechet.heldShape_collapse_eq hn hg hz one_pos hc0
  rw [hA] at hpath
  refine ⟨⟨?_, ?_⟩, hstrict, hpath⟩
  · rintro _ ⟨a, ha, p, hp, rfl⟩
    exact (hstrict a ha p hp).le
  · intro B hB
    apply le_of_tendsto hpath
    filter_upwards [self_mem_nhdsWithin] with e (he : 0 < e)
    have hs := (Frechet.heldShape hn hg one_pos he).1
    exact hB ⟨1, le_rfl, (e, Frechet.scale g 1 e), ⟨he, hs⟩, rfl⟩

/-- A strict majority of minimum observations causes divergence on the
fitted branch, already at Fréchet shape one. -/
theorem majority_unbounded {g : Fin n → ℝ} (hn : 0 < n)
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hc : n < 2 * Frechet.minimumCount g) :
    ¬ BddAbove {v | ∃ a ≥ 1, ∃ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 = v} := by
  have hc' : (n : ℝ) < 2 * Frechet.minimumCount g := by exact_mod_cast hc
  have he : Frechet.collapseExponent g 1 < 0 := by
    unfold Frechet.collapseExponent
    linarith
  rintro ⟨B, hB⟩
  obtain ⟨p, hp, hlt⟩ := (Frechet.heldShape_unbounded hn hg hz one_pos he).2 B
  exact hlt.not_ge (hB ⟨1, le_rfl, p, hp, rfl⟩)

end
end UnivariateML.AllFreeFrechet

end
