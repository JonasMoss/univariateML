module

public import UnivariateML.Family.Endpoint3ParetoIII

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! Pareto III collapse with scale held. -/

@[expose] public section

namespace UnivariateML.Base7ParetoIII

open Filter Transfer
open scoped Topology

/-- At every positive held scale, every subunit positive shape gives
divergence as the threshold approaches the sample minimum from below. -/
theorem held_scale_threshold_collapse {n : ℕ} {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) {c s : ℝ}
    (hc : 0 < c) (hc' : c < 1) (hs : 0 < s) :
    Tendsto (fun t ↦ ParetoIII.logLik d (-t) (c, s)) (𝓝[>] 0) atTop :=
  Endpoint2ParetoIII.threshold_spike hd hz hc hc' hs

end UnivariateML.Base7ParetoIII

end
