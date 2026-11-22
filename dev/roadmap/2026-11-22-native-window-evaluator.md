# Native conditional window evaluator (later)

**Idea.** The finite truncation-window d/p/q for geom and nbinom is computed twice, in `bindings/r/R/modifiers.R`
and `bindings/js/modifiers.mjs` (short-window, TASK-228, merge 8e34d86c), while Poisson, binomial and finite zeta
already have native mode-relative evaluators (`pois/tpois_dpqr_core.hpp`, `zeta/truncation.hpp`). One C++ evaluator
taking a parent log pmf and an optional log adjacent-ratio callback, building one relative-weight state per parameter
vector and window, and exposed through the Rcpp and WASM modifier interfaces, would remove the duplication and put the
evaluation in the core (AGENTS.md: the core evaluates without R).

**What it must carry** (from the short-window report): both tail conventions, the public and sampling inverse
comparisons, the zero-mass and unresolved-window outcomes, the 256-count bound, compensated sums.

**Trigger.** A third binding needs window evaluation, or a bug fixed in one binding's copy and not the other.
