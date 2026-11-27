module

public import UnivariateML.Family.Frechet
public import UnivariateML.Family.Weibull

set_option linter.style.header false

/-!
Fréchet fits at held location, using the same physical scale and shape
as inverse Weibull on the positive excesses.

## Deviations

The scale-only result concerns the constant branch. The two held-threshold masks use the
unrestricted positive-shape domain, as in the dossier.
-/

@[expose] public section

open Real Set Filter

namespace UnivariateML.Shape2Frechet

variable {n : ℕ}

private theorem logLik_transfer {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {s : ℝ} (hs : 0 < s) (k : ℝ) :
    Frechet.logLik t k 0 s = Weibull.Inverse.logLik t k s := by
  unfold Frechet.logLik Weibull.Inverse.logLik
  apply Finset.sum_congr rfl
  intro i _
  rw [add_zero, Frechet.logDensity, Weibull.Inverse.logDensity, log_div (ht i).ne' hs.ne']
  ring

/-- Holding the threshold leaves a unique positive shape and scale on
nonconstant excesses and unbounded concentration on constant excesses. -/
theorem held_mu_classification (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i) :
    ((∃ i j, t i ≠ t j) →
      ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
        IsMaxOn (fun q ↦ Frechet.logLik t q.1 0 q.2) Weibull.domain p) ∧
    ((∀ i j, t i = t j) →
      ∃ s > 0, Tendsto (fun k ↦ Frechet.logLik t k 0 s) atTop atTop) := by
  constructor
  · intro h
    apply (existsUnique_congr (fun p ↦ and_congr_right fun hp ↦ ?_)).2
      (Weibull.Inverse.jointMLE hn ht h)
    simp only [isMaxOn_iff]
    constructor <;> intro hm q hq
    · rw [← logLik_transfer ht hq.2, ← logLik_transfer ht hp.2]
      exact hm q hq
    · rw [logLik_transfer ht hq.2, logLik_transfer ht hp.2]
      exact hm q hq
  · intro h
    let i : Fin n := ⟨0, hn⟩
    refine ⟨t i, ht i, ?_⟩
    have he : t = fun _ ↦ t i := funext fun j ↦ h j i
    simp only [logLik_transfer ht (ht i)]
    rw [he]
    exact Weibull.Inverse.constant_unbounded hn (ht i)

/-- With threshold and positive scale held, the free shape is unique if
an excess differs from scale and diverges otherwise. -/
theorem held_mu_sigma_classification (hn : 0 < n) {t : Fin n → ℝ}
    (ht : ∀ i, 0 < t i) {s : ℝ} (hs : 0 < s) :
    ((∃ i, t i ≠ s) →
      ∃! k, k ∈ Ioi (0 : ℝ) ∧
        IsMaxOn (fun q ↦ Frechet.logLik t q 0 s) (Ioi 0) k) ∧
    ((∀ i, t i = s) → Tendsto (fun k ↦ Frechet.logLik t k 0 s) atTop atTop) := by
  constructor
  · intro h
    simpa only [logLik_transfer ht hs] using Weibull.Inverse.heldScale hn ht hs h
  · intro h
    simpa only [logLik_transfer ht hs] using Weibull.Inverse.heldScale_unbounded hn hs h

/-- With scale held, a constant sample has an admissible threshold
whose free shape can diverge on the fitted shape interval. -/
theorem held_sigma_constant_concentration (hn : 0 < n) {s m : ℝ} (hs : 0 < s) :
    (m - s < m) ∧
      Tendsto (fun k ↦ Frechet.logLik (fun _ : Fin n ↦ 0) k (m - (m - s)) s) atTop atTop := by
  refine ⟨sub_lt_self _ hs, ?_⟩
  have he (k : ℝ) : Frechet.logLik (fun _ : Fin n ↦ 0) k (m - (m - s)) s =
      Frechet.logLik (fun _ : Fin n ↦ s) k 0 s := by
    simp [Frechet.logLik, show m - (m - s) = s by ring]
  simp only [he]
  exact (held_mu_sigma_classification hn (fun _ ↦ hs) hs).2 (fun _ ↦ rfl)

end UnivariateML.Shape2Frechet

end
