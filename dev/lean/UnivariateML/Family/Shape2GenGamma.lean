module

public import UnivariateML.Family.AllFreeGenGamma
public import UnivariateML.Family.ClosedForm
public import UnivariateML.Family.TransformedGamma

set_option linter.style.header false

/-!
Generalized-gamma likelihood with fixed shape and one further hold.

## Deviations

The held-location classification concerns a fixed-Q scale slice. The free-Q signed ends,
normal seam as a limit in Q, and global tie claims are outside this module.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.Shape2GenGamma

open GenGammaTwoPoint

noncomputable section

variable {n : ℕ}

theorem held_Q_mu_scale_classification (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (μ Q : ℝ) :
    ((∃ i, log (x i) ≠ μ) →
      ∃! σ, σ ∈ Ioi (0 : ℝ) ∧
        IsMaxOn (fun s ↦ sampleLogLik x (μ, s, Q)) (Ioi 0) σ) ∧
    ((∀ i, log (x i) = μ) →
      Tendsto (fun σ ↦ sampleLogLik x (μ, σ, Q)) (𝓝[>] 0) atTop) := by
  refine ⟨AllFreeGenGamma.held_location_fixed_shape_scale hn μ Q, ?_⟩
  intro he
  apply AllFreeGenGamma.constant_concentration hn μ Q
  intro i
  rw [← he i, exp_log (hx i)]

private theorem normal_seam (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    (n : ℝ) * sampleLogLik x (μ, σ, 0) = Lognormal.logLik x μ σ := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  unfold sampleLogLik Lognormal.logLik
  rw [mul_div_cancel₀ _ hN, Normal.logLik_formula _ _ hσ]
  simp only [logDensity, ite_true, Lognormal.logSample, Normal.residual, Finset.sum_sub_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.sum_div, div_pow]
  ring

/-- With scale and shape held, every nonempty positive sample has a
unique location maximizer, including the normal seam. -/
theorem held_Q_sigma_location_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {σ : ℝ} (hσ : 0 < σ) (Q : ℝ) :
    ∃! μ, IsMaxOn (fun m ↦ sampleLogLik x (m, σ, Q)) univ μ := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  by_cases hQ : Q = 0
  · subst Q
    let μ := Normal.mean (Lognormal.logSample x)
    have he (m : ℝ) : IsMaxOn (fun t ↦ sampleLogLik x (t, σ, 0)) univ m ↔
        IsMaxOn (fun t ↦ Lognormal.logLik x t σ) univ m := by
      simp only [isMaxOn_iff]
      constructor
      · intro h t ht
        rw [← normal_seam hn x t hσ, ← normal_seam hn x m hσ]
        exact mul_le_mul_of_nonneg_left (h t ht) hN.le
      · intro h t ht
        have h' := h t ht
        rw [← normal_seam hn x t hσ, ← normal_seam hn x m hσ] at h'
        exact le_of_mul_le_mul_left h' hN
    exact ⟨μ, (he μ).2 ((Lognormal.heldSd hn x hx hσ μ).2 rfl),
      fun t ht ↦ (Lognormal.heldSd hn x hx hσ t).1 ((he t).1 ht)⟩
  · let r := Q / σ
    let k := 1 / Q ^ 2
    let E (m : ℝ) := ∑ i, exp (r * (log (x i) - m))
    let S := (∑ i, exp (r * log (x i))) / n
    let m₀ := log S / r
    have hr : r ≠ 0 := div_ne_zero hQ hσ.ne'
    have hk : 0 < k := by dsimp [k]; positivity
    have hS : 0 < S := div_pos (Finset.sum_pos (fun _ _ ↦ exp_pos _)
      ⟨⟨0, hn⟩, Finset.mem_univ _⟩) hN
    have hEm : E m₀ = n := by
      have he (i : Fin n) : exp (r * (log (x i) - m₀)) =
          exp (r * log (x i)) / S := by
        rw [mul_sub, show r * m₀ = log S by dsimp [m₀]; field_simp, exp_sub, exp_log hS]
      simp only [E, he, ← Finset.sum_div, ← show (n : ℝ) * S =
        ∑ i, exp (r * log (x i)) by dsimp [S]; field_simp]
      field_simp
    have hE (m : ℝ) : (∑ i, exp (r * (log (x i) - m))) = exp (r * (m₀ - m)) * n := by
      calc
        _ = exp (r * (m₀ - m)) * E m₀ := by
          dsimp [E]
          rw [Finset.mul_sum]
          apply Finset.sum_congr rfl
          intro i _
          rw [← exp_add]
          congr 1
          ring
        _ = _ := by rw [hEm]
    let L := fun m ↦ sampleLogLik x (m, σ, Q)
    let C := log (|Q| / σ) + (k * log k - k - log (Real.Gamma k))
    have hform (m : ℝ) : L m =
        ((n : ℝ) * C - k * (E m - n - ∑ i, r * (log (x i) - m)) -
          ∑ i, log (x i)) / n := by
      simp only [L, sampleLogLik, logDensity, hQ, ite_false]
      change (∑ i, (C - k * (exp (r * (log (x i) - m)) - 1 -
        r * (log (x i) - m)) - log (x i))) / n = _
      simp only [E, Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const,
        Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, mul_one]
    have hdiff (m : ℝ) :
        L m - L m₀ = k * (r * (m₀ - m) + 1 - exp (r * (m₀ - m))) := by
      rw [hform m, hform m₀, hEm]
      have hEm' : E m = exp (r * (m₀ - m)) * n := hE m
      rw [hEm']
      have he (i : Fin n) :
          r * (log (x i) - m) - r * (log (x i) - m₀) = r * (m₀ - m) := by ring
      have hs : (∑ i, r * (log (x i) - m)) - (∑ i, r * (log (x i) - m₀)) =
          (n : ℝ) * (r * (m₀ - m)) := by
        rw [← Finset.sum_sub_distrib]
        simp [he, nsmul_eq_mul]
      rw [← sub_div, div_eq_iff hN.ne']
      linear_combination k * hs
    have hlt (m : ℝ) (hm : m ≠ m₀) : L m < L m₀ := by
      have hnz : r * (m₀ - m) ≠ 0 := mul_ne_zero hr (sub_ne_zero.mpr hm.symm)
      have h := mul_neg_of_pos_of_neg hk (sub_neg.mpr (add_one_lt_exp hnz))
      linarith only [hdiff m, h]
    refine ⟨m₀, fun m _ ↦ ?_, fun m hm ↦ ?_⟩
    · rcases eq_or_ne m m₀ with rfl | h
      · exact le_rfl
      · exact (hlt m h).le
    · by_contra h
      exact (hlt m h).not_ge (hm (mem_univ m₀))


private theorem fixed_nonzero_shape_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) {Q : ℝ} (hQ : Q ≠ 0) :
    ∃! p : ℝ × ℝ, p ∈ Normal.domain ∧
      IsMaxOn (fun s ↦ sampleLogLik x (s.1, s.2, Q)) Normal.domain p := by
  let ε := Q / |Q|
  let k := 1 / Q ^ 2
  let y : Fin n → ℝ := fun i ↦ exp (ε * log (x i))
  let φ : ℝ × ℝ → ℝ × ℝ :=
    fun p ↦ (|Q| / p.2, exp (ε * p.1 - log k / (|Q| / p.2)))
  have hA : 0 < |Q| := abs_pos.mpr hQ
  have hε : ε ≠ 0 := div_ne_zero hQ hA.ne'
  have hk : 0 < k := by dsimp [k]; positivity
  have hb : BijOn φ Normal.domain Gamma.domain := by
    refine ⟨fun p hp ↦ ⟨div_pos hA hp.2, exp_pos _⟩, ?_, ?_⟩
    · intro p hp q hq h
      have hp₂ : 0 < p.2 := hp.2
      have hq₂ : 0 < q.2 := hq.2
      have he₂ : p.2 = q.2 := by
        have h₁ : |Q| / p.2 = |Q| / q.2 := congrArg Prod.fst h
        have h' := (div_eq_div_iff hp₂.ne' hq₂.ne').mp h₁
        exact (mul_right_inj' hA.ne').1 h'.symm
      refine Prod.ext ?_ he₂
      have h₁ : exp (ε * p.1 - log k / (|Q| / p.2)) =
          exp (ε * q.1 - log k / (|Q| / q.2)) := congrArg Prod.snd h
      apply (mul_right_inj' hε).1
      rw [he₂, exp_eq_exp, sub_left_inj] at h₁
      exact h₁
    · rintro ⟨d, a⟩ hp
      have hd : 0 < d := hp.1
      have ha : 0 < a := hp.2
      refine ⟨((log a + log k / d) / ε, |Q| / d),
        ⟨mem_univ _, div_pos hA hd⟩, Prod.ext ?_ ?_⟩
      · change |Q| / (|Q| / d) = d
        field_simp
      · change exp (ε * ((log a + log k / d) / ε) - log k / (|Q| / (|Q| / d))) = a
        rw [show |Q| / (|Q| / d) = d by field_simp]
        rw [show ε * ((log a + log k / d) / ε) = log a + log k / d by field_simp,
          add_sub_cancel_right, exp_log ha]
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have he (p : ℝ × ℝ) (hp : p ∈ Normal.domain) :
      sampleLogLik x (p.1, p.2, Q) =
        TransformedGamma.logLik y k (φ p).1 (φ p).2 + (Gamma.avgLog y - Gamma.avgLog x) := by
    have hp₂ : 0 < p.2 := hp.2
    let r := |Q| / p.2
    have hr : 0 < r := div_pos hA hp₂
    have hrε : r * ε = Q / p.2 := by dsimp [r, ε]; field_simp
    have harg (i : Fin n) :
        (φ p).1 * (log (y i) - log (φ p).2) = Q / p.2 * (log (x i) - p.1) + log k := by
      change r * (log (exp (ε * log (x i))) - log (exp (ε * p.1 - log k / r))) = _
      rw [log_exp, log_exp, sub_sub_eq_add_sub, ← sub_add_eq_add_sub, mul_add, mul_sub]
      rw [show r * (log k / r) = log k by field_simp]
      linear_combination (log (x i) - p.1) * hrε
    have hpoint (i : Fin n) :
        logDensity p.1 p.2 Q (log (x i)) =
          log (φ p).1 - log (Real.Gamma k) -
          TransformedGamma.loss k ((φ p).1 * (log (y i) - log (φ p).2)) := by
      rw [harg, TransformedGamma.loss, exp_add, exp_log hk]
      simp only [logDensity, hQ, ite_false]
      change log r + (k * log k - k - log (Real.Gamma k)) -
        k * (exp (Q / p.2 * (log (x i) - p.1)) - 1 - Q / p.2 * (log (x i) - p.1)) =
        log r - log (Real.Gamma k) -
          (exp (Q / p.2 * (log (x i) - p.1)) * k -
            k * (Q / p.2 * (log (x i) - p.1) + log k))
      ring
    simp only [sampleLogLik, hpoint, TransformedGamma.logLik, Gamma.avg, Gamma.avgLog,
      Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    field_simp [hn']
    ring
  obtain ⟨i, j, hij⟩ := hne
  have hyne : y i ≠ y j := by
    intro h
    exact hij (log_injOn_pos (hx i) (hx j)
      ((mul_right_inj' hε).1 (exp_injective h)))
  exact (Transfer.existsUnique_max_iff hb he).2
    (TransformedGamma.heldShape hn (fun _ ↦ exp_pos _) ⟨i, j, hyne⟩ hk)

theorem held_Q_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (Q : ℝ) :
    ((∃ i j, log (x i) ≠ log (x j)) →
      ∃! p : ℝ × ℝ, p ∈ Normal.domain ∧
        IsMaxOn (fun s ↦ sampleLogLik x (s.1, s.2, Q)) Normal.domain p) ∧
    ((∀ i j, log (x i) = log (x j)) →
      ∃ μ, Tendsto (fun σ ↦ sampleLogLik x (μ, σ, Q)) (𝓝[>] 0) atTop) := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  constructor
  · intro hy
    by_cases hQ : Q = 0
    · subst Q
      have hne : ∃ i j, x i ≠ x j := by
        obtain ⟨i, j, hij⟩ := hy
        exact ⟨i, j, fun h ↦ hij (congrArg log h)⟩
      apply (existsUnique_congr (fun p ↦ and_congr_right fun hp ↦ ?_)).2
        ((Lognormal.existsUnique_mle_iff hn hx).2 hne)
      simp only [isMaxOn_iff]
      constructor
      · intro hm q hq
        rw [← normal_seam hn x q.1 hq.2, ← normal_seam hn x p.1 hp.2]
        exact mul_le_mul_of_nonneg_left (hm q hq) hN.le
      · intro hm q hq
        have h := hm q hq
        rw [← normal_seam hn x q.1 hq.2, ← normal_seam hn x p.1 hp.2] at h
        exact le_of_mul_le_mul_left h hN
    · obtain ⟨i, j, hij⟩ := hy
      exact fixed_nonzero_shape_mle hn hx ⟨i, j, fun h ↦ hij (congrArg log h)⟩ hQ
  · intro h
    let i : Fin n := ⟨0, hn⟩
    exact ⟨log (x i), (held_Q_mu_scale_classification hn hx (log (x i)) Q).2 (fun j ↦ h j i)⟩

end

end UnivariateML.Shape2GenGamma

end
