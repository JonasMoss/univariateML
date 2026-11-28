module

public import UnivariateML.Family.Endpoint3ParetoII

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! Pareto II with held positive scale and shape. -/

@[expose] public section

namespace UnivariateML.Base7ParetoII

open Filter Set Real Transfer
open scoped Topology

/-- In excess coordinates the sample minimum is at zero. The unit lower
power permits finite contact there, and a receding threshold has zero
likelihood in the limit. -/
theorem held_scale_shape_failure {n : ℕ} {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) {q s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    let L := fun t ↦ ParetoII.logLik d (-t) q s
    IsMaxOn L (Ici 0) 0 ∧ Tendsto (fun t ↦ exp (L t)) atTop (𝓝 0) := by
  dsimp only
  refine ⟨(Endpoint3ParetoII.threshold_contact_iff hd hz hq hs le_rfl).mpr rfl, ?_⟩
  apply tendsto_exp_atBot.comp
  have hn : 0 < (n : ℝ) := by
    obtain ⟨i, _⟩ := hz
    exact_mod_cast i.isLt.pos
  have harg : Tendsto (fun t : ℝ ↦ 1 + t / s) atTop atTop := by
    apply tendsto_atTop_add_const_left
    simpa [div_eq_mul_inv] using tendsto_id.atTop_mul_const (inv_pos.mpr hs)
  have hb := (tendsto_log_atTop.comp harg).const_mul_atTop (show 0 < n * (q + 1) by positivity)
  have hlim := tendsto_atBot_add_const_left _ ((n : ℝ) * (log q - log s))
    (tendsto_neg_atTop_atBot.comp hb)
  apply tendsto_atBot_mono' _ _ hlim
  filter_upwards [eventually_ge_atTop (0 : ℝ)] with t ht
  unfold ParetoII.logLik ParetoII.logDensity
  calc
    _ ≤ ∑ _ : Fin n, (log q - log s - (q + 1) * log (1 + t / s)) := by
      apply Finset.sum_le_sum
      intro i _
      have hdi := hd i
      have hlog : log (1 + t / s) ≤ log (1 + (d i - -t) / s) :=
        strictMonoOn_log.monotoneOn ((show 0 < 1 + t / s by positivity) : 1 + t / s ∈ Ioi 0)
          (show 1 + (d i - -t) / s ∈ Ioi 0 from
            show 0 < 1 + (d i - -t) / s by rw [sub_neg_eq_add]; positivity)
          (by gcongr; linarith [hd i])
      nlinarith only [mul_le_mul_of_nonneg_left hlog (show 0 ≤ q + 1 by linarith)]
    _ = _ := by simp [nsmul_eq_mul]; ring

end UnivariateML.Base7ParetoII

end
