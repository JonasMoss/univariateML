# TASK-151.1 reproducible evidence

All drivers use the unchanged frozen sample or explicit seeds. Compile C++
with `-O2 -std=c++17 -IunivariateML-cpp/include` through the CPU slot wrapper.
`compare.cpp N SEEDS` uses seeds 777+N, the eleven prototype generators,
and routes 0 all free, 1 held location .123, 2 held scale 1.07, 3 held shape -.5.
The before executable was compiled before changing the source. Seed counts
are 8, 6 and 2 at N=30,101,1000. `timing.cpp` follows the deterministic
quantile driver, including every mask at N=101,500 and the all-free large N.
Mask bits are held location=1, scale=2, shape=4. Its controls remain 1e-7/1000.

`inverse.cpp` emits binary64 inverse inputs and outputs. `check_inverse.py`
checks them by independent high-precision monotone bisection and writes
`inverse-checks.json`. The supplied log deficit is authoritative in the
endpoint regime, including deficits that cannot survive in rounded z.
`density_timing.cpp` compares the unchanged public density with the fitting
inverse on 10001 deterministic quantiles per shape. Timings exclude data
generation; the fitting measurement includes first-use table initialization.

`golden.cpp` reads N followed by the frozen observations and checks all-free,
held-location and held-scale fits. Held values equal the supplied independent
50-digit reference, so the same reference applies to their remaining free
coordinates. No frozen sample or golden estimate is rewritten.

`baseline-mle.hpp` preserves the original proposal fitter from revision
6977702df860d9fbc20cc681507b927cd8565f87. Its quoted includes are relative
to the original tlambda directory; to rebuild the baseline in scratch,
resolve those includes against that directory before compilation (as done
for `extended.cpp` with `-DBASELINE -Ibuild/scratch`). The certificate and
public evaluator headers remain unchanged. `extended.cpp` additionally
probes held location and held shape at generating shapes -2,-8,-20.
At generating shape -20, the held-location fit now has fitted shape below
-16 and improves the independently recomputed likelihood by 148.2837. The
joint nuisance fit at held shape -20 stops numerically; no capped estimate
is accepted.

`check_golden.py` independently recomputes the inverse and likelihood score
at 80 digits and converts the free scores to coordinate corrections with a
high-precision observed-information matrix. `summarize.py` checks every
comparison key, status and likelihood difference and records aggregate
route timings. [methods.md](methods.md) states the exact-arithmetic bounds,
the computed-sign acceptance rules and the scope of the Newton criteria.

`dev-suites.sh` runs the dev C++, full no-long-double and R dev commands
serially with four workers on the selected machine. The source manifest
covers every changed numerical file, the unchanged public density,
certificate and golden, and the corrected frozen-model regression test.
An initial R dev failure in that unrelated test is retained in
`initial-r-dev-failure.log`: it expected rejection of an infinite geometric
median despite the existing implementation explicitly allowing quantile
overflow. The test now compares with stats::qgeom directly. No frozen-model
implementation, quantile evaluator or tolerance was changed.

`affine.cpp` and `check_affine.py` test an actual affine support residual of
2^-1074: x=2, mu=2^-1073, sigma=1, lambda=1/2. The rounded standardized
observation is exactly the endpoint, while the correct positive deficit is
retained. A 420-digit closed-form density check verifies the finite answer.
`derivative_guard.cpp` checks that a finite likelihood with overflowing
analytic derivatives exits before damping.

`initial-js-golden-failure.log` retains the expected strict JS coefficient
failure against the legacy golden estimates. The JS golden test uses the
supplied independent tlambda reference with 1e-10 tolerance; other families
and the frozen JSON are unchanged. This tightens the existing lambda check
and preserves the existing strict mu/scale checks.

`check_held.py` polishes the extreme held-location fit at 90 digits, confirms
positive observed information and verifies the C++ free coefficients. It
also independently recomputes the legacy likelihood on the identical data.
`check_wasm_held.mjs` checks that reference against the WebAssembly fitter.
`extended.cpp data` emits the exact binary64 observations for these checks.

The extended affine checker also records the declared public rounded-endpoint
alias at lambda=nextafter(1,2), x=1, mu=2^-1074 and sigma=lambda. The fitting
inverse follows the positive affine interior deficit and matches mpmath;
public dtlambda intentionally returns the endpoint density. This expected
convention difference is in `endpoint-alias-checks.json`. The later public
inverse job must retain or explicitly reconsider that existing alias guard.

Held-location negative profiles retain a verified logistic fit (or the
feasible uniform scale) as their initial warm start. Together with the
shape-specific scale start, this supplies multistart even when the first,
very negative grid points fail. The frozen source manifest covers this final
initialization as well as the scaled affine Newton frame.
