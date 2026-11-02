# Limit-face comparisons at huge log-likelihood magnitudes

Completed 2026-11-02 in `codex/face-compare`, based on
`1bec30eadbd874bfed5d2dd97fea7a1981d14ab8`. No commit was made.

The 13 requested families now stop with their existing numerical-failure
status when rounding leaves a requested face decision unresolved. Huber also
uses a direct Gaussian gain when location and scale are held, and rejects a
nonstationary finite shape candidate in that regime. All six required final
recipes passed on the final numerical source. Golden and truth files are
unchanged.

## Comparison rule and its mathematical scope

The new header [core/face_compare.hpp](../../../univariateML-cpp/include/cppunivariate/core/face_compare.hpp)
provides `FaceComparison::{FirstWins, SecondWins, NumericalTie}` and
`compare_face(a,b,m,C=0)`. The family margin `m >= 0` favors the first
candidate. It distinguishes the intentional family tolerance from a numerical
tie: equal ordinary objectives can still resolve in favor of the face when
the family margin exceeds the rounding allowance.

With double machine epsilon `eps`, the implementation evaluates

```text
S = max(1, |a|, |b|)
d = a/S - b/S + m/S
R = 8*eps*(|a| + |b| + C)/S + 4*eps*(|a/S| + |b/S|).
```

The three products in the first part of the allowance are evaluated before
addition, avoiding overflow of `|a|+|b|` for opposite finite extremes. Scaling
also avoids overflow in `a-b`. The result is first wins for `d > R`, second
wins for `d < -R`, and numerical tie otherwise. NaN objectives, invalid margins
or invalid cancellation scales give numerical tie. Unequal infinities are
ordered; equal infinities give numerical tie. `C` is zero for stored full
objectives and supplies an absolute-term scale for a directly computed gain.

The exact-arithmetic implication is elementary: if the error in the normalized
penalized gain is bounded by `R`, either strict inequality certifies its sign.
The factors 8 and 4 are a stated practical floating-point allowance, **not a
proved forward-error bound for every likelihood evaluator, special function,
reduction or optimizer**. No outward-rounded interval arithmetic is claimed.
The certificate is conditional on the computed comparisons with this margin;
it does not establish a global maximum for an existing numerical profile search.

The magnitude-dependent allowance is uncertainty, not an enlarged tolerance
that silently awards a face. For example, two objectives both rounded to
`-1e100` with margin `2e-8` give numerical tie. Two ordinary objectives both
`-10` with that margin resolve in favor of the first candidate. Permanent
regressions also cover strict separation in both directions, NaN, absent
negative-infinite candidates, the decision threshold and opposite finite
`double` extremes.

Except for Huber's direct-gain branch, existing selectors remain after a
numerical-tie guard. Once the comparison is resolved beyond the allowance,
their historical arithmetic and family precedence are retained. This avoids
changing ordinary numerical plateaus. Strict normal comparisons in ST and LST
have guards at both signed margin thresholds. Secondary face precedence in STD,
Huber, EMG and Bhatt is guarded as well. Existing coordinate normalizations and
Jacobians are preserved; genbeta already compares before restoring its common
Jacobian. No aggregator edit was needed because each affected family includes
the new header directly.

## Direct Huber gain and finite-candidate validation

Write `c=epsilon`, `z_i=(x_i-mu)/sigma`, and

```text
Z(c) = sqrt(2*pi)*erf(c/sqrt(2)) + 2*exp(-c*c/2)/c.
```

When both location and scale are held, the Gaussian and finite Huber objectives
share their entire Gaussian quadratic. The exact identities

```text
rho_c(z) - z*z/2 = -max(|z|-c,0)^2/2
ell_normal - ell_c
  = n*(log Z(c) - log(2*pi)/2) - sum(max(|z_i|-c,0)^2)/2
```

allow cancellation before rounding. This branch compares that direct gain to
zero, with the unchanged family margin. Its cancellation scale is `n` plus the
sum of the absolute normalizer and tail contributions. The `n` term also
protects the normalizer subtraction when its computed difference is zero.
Other held subsets use guarded full objectives. The Huber dossier records the
identity and score gate.

The focused regression takes `c=3e10`, `sigma=1e-10` and observations `{1,2}`.
All residuals lie in the quadratic region. The two stored objectives at order
`-1e20` cannot resolve the comparison, but the direct normalizer gain resolves
it within the family tolerance. Another regression ensures that a supplied
large absolute-term scale cannot be hidden by a small resulting gain.

The extreme free-shape fit revealed a separate defect in the existing sampled
shape search: with `mu=0`, `sigma=1e-10`, `{1,2}`, it could keep `c=1.345`
with likelihood approximately `-40349999954.096466`. A feasible
`c=sigma/1.5` gives approximately `-4.197224577336222`. This is not a permissible
finite maximum. For example, the bound `Z(c) <= sqrt(2*pi)+2/c` and the tail-loss
formula already establish a vastly better feasible likelihood at this small
`c`; finding the exact optimum is unnecessary to reject the sampled point.

Since

```text
Z'(c) = -2*exp(-c*c/2)/(c*c),
d ell / d log(c)
  = 2*n*exp(-c*c/2)/(c*Z(c)) - c*sum_{|z_i|>c}(|z_i|-c),
```

a returned finite candidate in this held-coordinate regime now must have a
finite **total log-shape score** of absolute value at most `1e-3`. Otherwise it
returns `IterationFailure`. The original point has score approximately
`-40349999995.92955`; the independent tail-formula check at `sigma/1.5` gives
zero at double precision. These numerical values are reproduced by the native
probe; the derivative and cancellation identities above are exact-arithmetic
results. Stationarity alone is not a global-optimality proof. The search grid,
initialization, bounds and iteration limits were not replaced, and an unchecked
sampled point is no longer returned for this fixture.

SN already has an analytic route worth preserving: with a held location and
one-sided observations, each nonconstant `log Phi(alpha*z_i)` is monotone toward
the corresponding half-normal face. Its `{1,2}`, held-location-zero fixture
therefore selects the right half-normal face without subtracting two enormous
finite candidate likelihoods. Other face comparisons conservatively stop when
no direct or analytic resolution is available.

## Family coverage and extreme fixtures

Paths and line numbers below refer to the final `distributions/<id>/mle.hpp`
under `univariateML-cpp/include/cppunivariate`. Every family has appended
`test.cpp` cases for rounded equal objectives at its audited magnitude and for
an actual held-parameter fit. The source audit motivating the magnitudes is
[the Gompertz extreme report](2026-11-02-gompertz-extreme-report.md).

Unless stated otherwise, observations are `{1,2}`, location or mean is held at
zero, and scale or standard deviation is held at `1e-10`; the compared shape
is free. These are extreme-input regressions and numerical comparisons, not
proofs of every fitted family's global optimality.

| Family | Guarded comparison lines | Tie/failure status | Extreme fixture and final native outcome |
|---|---|---|---|
| betapr | 191, 313 | `BfgsFailure` | Hold alpha `1e100`; other shape and scale free. The gamma-face contrast has order `1e99`; the conditional optimizer stops before a usable finite candidate (`logLik=-inf`). No face is silently selected. |
| gb2 | 366 | `SearchUnresolved` | Hold power `1` and p `1e100`; q and scale free. Finite fit, likelihood `-1.4115473713080307`; not the concentrated gamma face. |
| genbeta | 149 | `SearchUnresolved` | Scale `1`, observations `{exp(-1),exp(-2)}`, beta shape b `1e100`, a and t free. Finite fit, likelihood `2.3103709118998559`; not the concentrated unit-gamma face. The competing power face can itself prevent the final best-face value from being huge. |
| sn | 301 | `IterationFailure` | Held xi and omega, alpha free: right half-normal boundary, likelihood about `-2.5e20`, justified by the one-sided monotonicity route. |
| snorm | 145 | `BfgsFailure` | Held mean and sd, xi free: honest failure, likelihood about `-9.0845e19`; no unchecked half-normal selection. |
| st | 107, 137 | `NumericalFailure` | Held xi, omega and alpha `0`, nu free: finite fit, likelihood `-49.727455992699603`; not the Gaussian face. |
| std | 292, 353 | `BfgsFailure` / `IterationFailure` | Held mean and sd, nu free: finite fit, likelihood `-103.98005450974381`; not the normal face. Both unconditional and conditional face selectors are guarded. |
| lst | 209, 325, 328 | `BfgsFailure` / `IterationFailure` | Held mu and sigma, df free: finite fit, likelihood `-10.569881778637729`; not the normal face. Attained endpoint and collapse comparisons are guarded too. |
| huber | 482–499; score gate following | `IterationFailure` | Free epsilon with both coordinates held: the missed-peak fixture now stops with failure. Its retained diagnostic likelihood is `-40349999954.096466`, not an accepted estimate. |
| emg | 166, 176 | `IterationFailure` | Held mu and sigma, lambda free: finite fit, likelihood `-2.8109302162163283`; not the normal face. Exponential-face precedence is also guarded. |
| bhatt | 220, 232 | `IterationFailure` | Held mu and sigma, width a free: finite nonnormal fit, likelihood `-2.7725887378739378`. Uniform-face precedence is also guarded. |
| ged | 196, 276, 290 | `BfgsFailure` | Hold mean `0`, sd `1e-20`, nu free: attained Laplace member, likelihood about `-4.24264e20`. Conditional and unconditional uniform-face decisions are guarded. |
| pig | 99 | `SearchFailure` | Hold mean `1e100`, shape free, integer observations `{1,2}`: finite fit, likelihood `-4.0146926978298794`; not the Poisson face at order `-1e100`. |

The extreme beta-prime fixture is explicitly an optimizer-failure regression,
not a claim that its final face guard was reached. Likewise, the actual large
inputs do not force a rounded tie in every family: several clearly favor a
finite candidate. The separate helper cases force an unresolved rounded
comparison at each family's nominated magnitude and require `NumericalTie`,
whereas the real fits reject false limit decisions or require an appropriate
failure status. Input-validation failures do not count as passes. There are
28 appended test cases in total. Original golden assertions and tolerances
were retained.

## Ordinary fits and reproducibility

On the final numerical source, a native harness compared all 13 families
against the original headers from the base revision, with C++17, `-O1`, and
the installed BH include directory. Each continuous golden sample was perturbed
multiplicatively by independent `Uniform(-0.001,0)` values using Python
`random.Random(20261102 + family_index)`, with the family order below. This
keeps genbeta inside its fixed support. PIG uses a same-size seeded bootstrap
of its integer golden sample instead. Every result had status `Ok`.

Statuses, log likelihoods and every fitted parameter were printed with 17
significant digits. `diff -u` between original and patched outputs was empty,
so these finite double values are identical in the tested environment. The
following is the final output, with status then likelihood then the result
fields specified by the generator in the appendix:

```text
betapr 0 -439.38847155666235 2.0141740141934803 3.2599755046077274 1.0589599198847612
gb2 0 -1322.3322440116619 0.57108033654218981 1.1231509527030152 0.71171760842556842 1.0074268572907181
genbeta 0 374.6879070297565 0.41927788510693753 0.31850072296692877 2.2012606637034957 1
sn 0 -456.56746969188345 0.071188993229471698 0.90037971324474808 2.241644470762659
snorm 0 -639.37809001299206 -0.020729997774951558 0.99867164909163875 3.3925866846027377
st 0 -774.05811251783109 -0.036660326239165653 0.91990810931169043 0.10890388140174059 4.8314016788323473
std 0 -579.22163582192456 -0.0062576735841295018 0.83905987244932878 3.9778896906442203
lst 0 -898.38197188569654 0.074516153467405408 1.0266178845711142 3.0244181900474074
huber 0 -838.69426777032186 0.02405774489442122 1.0788259979453938 1.3201400885521946
emg 0 -846.51370027265477 0.17556773611630982 1.0512324827295461 1.2096276958319261
bhatt 0 -896.05273853459494 -0.0037961158484466767 1.0574220155279876 1.7325793236680593
ged 0 -711.43608641885703 -0.017695122879485457 1.0173431987051458 3.0172246824840179
pig 0 -906.60324899836633 1.9059999999999999 4.0759482370854716
```

Golden regression and ordinary held-subset conformance also passed in both R
tiers. The read-only `git diff --exit-code` checks for all golden and truth JSON
files returned zero with no output. Settled d/p/q/r, special functions, truth
generators, R code generation and R adapters were not edited.

## Final runs

All numerical runs used the local workstation, the supplied slot locks and
`nice -n 10` from those wrappers. Simbox was not used. Each required final
recipe had its own blocking `run4` invocation; no locks were nested.
`OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS` and `MKL_NUM_THREADS` were one.
The build parallelism was at most four. The JS build additionally uses its
existing four-core affinity and `EMCC_CORES=4` controls.

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just cpp-test-no-long-double
/tmp/umvl-cpu/run4 just test
/tmp/umvl-cpu/run4 just test-full
/tmp/umvl-cpu/run4 just js-build
/tmp/umvl-cpu/run4 just js-test
/tmp/umvl-cpu/run4 bash build/scratch/run-probes.sh
```

| Final recipe | Result on final numerical source |
|---|---|
| `just cpp-test` | 106/106 dev shards passed; ctest elapsed 88.78 s |
| `just cpp-test-no-long-double` | 127/127 shards passed; ctest elapsed 166.22 s |
| `just test` | FAIL 0, WARN 0, SKIP 153, PASS 20,500; R harness elapsed 159.973 s |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43,317; R harness elapsed 336.106 s |
| `just js-build` | Passed, exit 0 |
| `just js-test` | 413/413 passed; zero failures, cancellations and skips; 127.640 s |
| Final native probes and seeded original-versus-patched comparison | Passed, exit 0; exact seeded output agreement for all 13 families |

All 153 R dev skips explicitly require the full tier; that tier has no skips.
Timings above exclude slot waits and, for harness timings, preceding build work;
they are observations on a shared workstation, not performance benchmarks.
Earlier green runs before the final Huber score gate were superseded by the
runs in this table. One manually prepared seeded harness initially failed to
compile because it used R-facing mean/sd names instead of C++ mu/sigma fields;
that harness was corrected before the successful final comparison. A lost
queued tool session also required recovering the R run through its log and
using fresh final logs with explicit zero exit markers. No failed or ambiguous
run is counted as a final pass.

Environment: Linux 7.0.0-38-generic; GCC 15.2.0; R 4.6.1; BH 1.90.0-1;
Rcpp 1.1.2; testthat 3.3.2; Node v22.22.1. The final native probes used the same
BH installation under the R 4.6 library as the official builds. Compilation
emitted warnings in untouched code; login setup also mentioned an absent
`~/.cargo/env`. These did not fail the commands. R test warning counts are zero.
The assembled R comparison and Huber headers were checked byte-for-byte against
the canonical final headers.

## Remaining limits and separate work

- **Open, Huber recovery:** the extreme held-coordinate shape search can miss a
  finite peak. This job makes that case stop by its total shape score; it does
  not recover the small positive optimum. Recovering it requires a rescaled
  search and a bracket/profile certificate. Returning numerical failure is the
  implemented outcome, rather than an unchecked estimate.
- **Open, beta-prime extreme candidate construction:** the held-alpha `1e100`
  fixture fails before a usable finite profile candidate. A stable huge-held-
  shape profile evaluation/search is needed to obtain an end-to-end rounded-
  tie fixture through that selector. The helper's rounded-objective regression
  and the actual honest-failure regression are distinct coverage.
- Existing family search guarantees remain those in their dossiers. These
  comparison fixes and the Huber stationarity check do not prove exhaustive
  enumeration of stationary points or global uniqueness.
- **HT is intentionally unchanged:** its `mle.hpp:124` compares
  `normal_ll >= best - 1e-8*n`, and `mle.hpp:178` compares
  `normal_ll >= challenger - tol`. These belong to the parallel owner and need
  the same comparison review there. No changes to the settled d/p/q/r or
  special-function components were required by this job.

## Reproducing the seeded comparison

Save the following generator as `build/scratch/prepare_seed.py`. It reconstructs
original headers with read-only Git calls and writes a deterministic C++
harness. The base revision is fixed above; `HEAD` was that revision throughout
this job. All writes stay inside the worktree.

```python
import json,random,shutil,subprocess
from pathlib import Path
r=Path('univariateML-cpp/include')
b=Path('build/scratch/baseline-include')
shutil.copytree(r,b,dirs_exist_ok=True)
fs='betapr gb2 genbeta sn snorm st std lst huber emg bhatt ged pig'.split()
for f in fs:
 p=Path('cppunivariate/distributions')/f/'mle.hpp'
 (b/p).write_bytes(subprocess.check_output(['git','show','HEAD:univariateML-cpp/include/'+str(p)]))
fields={'betapr':'alpha beta scale','gb2':'shape1 shape2 shape3 scale','genbeta':'shape1 shape2 shape3 scale','sn':'xi omega alpha','snorm':'mu sigma xi','st':'xi omega alpha nu','std':'mu sigma nu','lst':'mu sigma df','huber':'mu sigma epsilon','emg':'mu sigma lambda','bhatt':'mu sigma a','ged':'mu sigma nu','pig':'mean shape'}
calls={'betapr':'betapr_mle(s)','gb2':'gb2_mle(s)','genbeta':'genbeta_mle(s,1)','sn':'sn_mle(s)','snorm':'snorm_mle(s,1e-6,1000)','st':'st_mle(s)','std':'student_t_mle(s,1e-6,1000)','lst':'lst_mle(s,1e-6,1000)','huber':'huber_mle(s,1e-10,200,nan)','emg':'emg_mle(s)','bhatt':'bhatt_mle(s)','ged':'ged_mle(s,1e-6,1000)','pig':'pig_mle(s)'}
s='#include "cppunivariate/cppunivariate.hpp"\n#include <iostream>\n#include <iomanip>\nint main(){const double nan=std::numeric_limits<double>::quiet_NaN();std::cout<<std::setprecision(17);\n'
for i,f in enumerate(fs):
 g=json.loads((r/'cppunivariate/distributions'/f/'golden.json').read_text()); rng=random.Random(20261102+i)
 x=([g['data'][rng.randrange(len(g['data']))] for _ in g['data']] if f=='pig' else [v*(1+rng.uniform(-.001,0)) for v in g['data']])
 typ='int' if f=='pig' else 'double';sp='ispan' if f=='pig' else 'span'
 s+='{std::vector<'+typ+'> x{'+','.join(map(str,x))+'};cppunivariate::'+sp+' s{x.data(),x.size()};auto r=cppunivariate::'+calls[f]+';std::cout<<"'+f+' "<<int(r.status)<<" "<<r.logLik'
 for k in fields[f].split():s+='<<" "<<r.'+k
 s+='<<"\\n";}\n'
s+='}\n';Path('build/scratch/seed.cpp').write_text(s)
```

After generating the harness, compile and run both variants under a CPU slot
allocation. The final job used `run4` around this script (including the extreme
probe and independent Huber arithmetic):

```sh
#!/bin/bash
set -eu
python3 build/scratch/prepare_seed.py
boost=/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include
c++ -std=c++17 -O1 -IunivariateML-cpp/include -I"$boost" build/scratch/probe.cpp -o build/scratch/probe
build/scratch/probe > build/scratch/probe.out
c++ -std=c++17 -O1 -Ibuild/scratch/baseline-include -I"$boost" build/scratch/seed.cpp -o build/scratch/seed-old
build/scratch/seed-old > build/scratch/seed-old.out
c++ -std=c++17 -O1 -IunivariateML-cpp/include -I"$boost" build/scratch/seed.cpp -o build/scratch/seed-new
build/scratch/seed-new > build/scratch/seed-new.out
diff -u build/scratch/seed-old.out build/scratch/seed-new.out
python3 - <<'PY'
import math
n=2; sigma=1e-10
for c in (1.345,sigma/1.5):
    z=math.sqrt(2*math.pi)*math.erf(c/math.sqrt(2))+2*math.exp(-c*c/2)/c
    ll=-n*(math.log(sigma)+math.log(z))-sum(c*(v/sigma-c/2) for v in (1,2))
    score=2*n*math.exp(-c*c/2)/(c*z)-sum(c*(v/sigma-c) for v in (1,2))
    print('Huber independent tail formula',c,ll,score)
PY
```

The independent Huber arithmetic printed:

```text
Huber independent tail formula 1.345 -40349999954.096466 -40349999995.92955
Huber independent tail formula 6.666666666666667e-11 -4.197224577336222 0.0
```

The extreme probe's family calls are also preserved here; the permanent
per-family test cases contain the corresponding assertions.

```cpp
#include "cppunivariate/cppunivariate.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main(){
const double nan=std::numeric_limits<double>::quiet_NaN();
std::vector<double>x{1,2};cppunivariate::span s{x.data(),x.size()};std::vector<int> z{1,2};cppunivariate::ispan iz{z.data(),z.size()};
#define P(f,call) {auto r=call;std::cout<<f<<" "<<int(r.status)<<" "<<std::setprecision(17)<<r.logLik<<"\n";}
P("betapr",cppunivariate::betapr_mle_holds(s,1e100,nan,nan))
P("gb2",cppunivariate::gb2_mle(s,1e-7,300,nan,1,1e100,nan))
std::vector<double> v{std::exp(-1.),std::exp(-2.)};cppunivariate::span sv{v.data(),v.size()};
P("genbeta",cppunivariate::genbeta_mle(sv,1,1e-7,300,nan,1e100,nan))
P("sn",cppunivariate::sn_mle(s,1e-9,100,0,1e-10,nan))
P("snorm",cppunivariate::snorm_mle(s,1e-6,100,0,1e-10,nan))
P("st",cppunivariate::st_mle(s,1e-6,200,0,1e-10,0,nan))
P("std",cppunivariate::student_t_mle(s,1e-6,200,0,1e-10,nan))
P("lst",cppunivariate::lst_mle(s,1e-6,200,0,1e-10,nan))
P("huber",cppunivariate::huber_mle(s,1e-10,200,nan,0,1e-10))
P("emg",cppunivariate::emg_mle(s,1e-8,500,0,1e-10,nan))
P("bhatt",cppunivariate::bhatt_mle(s,1e-8,500,1e-10,nan,0))
P("ged",cppunivariate::ged_mle(s,1e-6,500,0,1e-20,nan))
P("pig",cppunivariate::pig_mle(iz,1e-9,200,1e100,nan))
}
```

## Identities of tested authored sources

The base revision plus these SHA-256 identities identifies the tested source
patch. No commit, push, board edit, or files in another worktree were used.

```text
6ef2019b4574c70b87e15d0d0209e4ab2d693dd53e47902a7a82e06b671b959c  univariateML-cpp/include/cppunivariate/core/face_compare.hpp
7fd7eefd2667443cb6f973e4db7884cc45758d57befbb6f0af4748cf5d650ef9  univariateML-cpp/include/cppunivariate/distributions/betapr/README.md
d3a3515824ed13ce92d1c4705271cbc130329c5f1fac50621fb5d44355356daf  univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp
8725ecc62a6052c8d9deee05084b73c388d01c5c02a80b733beefc04d095dfff  univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp
9f7cef19cf1081be424ebf4040c704342ad02773582190136c6a8c1bca8fd9bc  univariateML-cpp/include/cppunivariate/distributions/bhatt/README.md
22965734bc51f8405aa1719b0414b83df6bff6dcb2d932680d961ea03a5018d0  univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp
24705c488865c4730078df402aa1d77402ba76e586b8a50fc0c69cdaedadea9e  univariateML-cpp/include/cppunivariate/distributions/bhatt/test.cpp
fb2844f286ffed0bd40b8786110892345f4ef148f54ca48a6d87673408d678e9  univariateML-cpp/include/cppunivariate/distributions/emg/README.md
6d8f13bd05f81d1c4f4761c0f15d43362a6b558b180198ed15f1e1f733170878  univariateML-cpp/include/cppunivariate/distributions/emg/mle.hpp
05fd70d9bac94dad16db4e1ee431c4bb3400ff28ad974717f9cedfd26b16116b  univariateML-cpp/include/cppunivariate/distributions/emg/test.cpp
b6a69d1830ce7172763c9a1353d917793ed5b84cceabbd7c9e6dd9f9831992d1  univariateML-cpp/include/cppunivariate/distributions/gb2/README.md
687b66c21a7ef8221d5b1201861201cff7fe1d91cb138b46daefb56ed7f1730b  univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp
0da8258aba4e7f384c77d738fbbbea804031a8ced5fdeed7c76c95258490461c  univariateML-cpp/include/cppunivariate/distributions/gb2/test.cpp
7a74dc1a690fa36291580aa968827b5fbe071a51495a86e77588f320aa8bcba9  univariateML-cpp/include/cppunivariate/distributions/ged/README.md
20f89858bd0c18fb1a1b1a33de9586b7cba6c568add4f8141c73f2bf6ec7cdbe  univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp
acd4e80a2adb0e880e4bfe3364bf94f347376ef0c157b90e46441d74bd82741a  univariateML-cpp/include/cppunivariate/distributions/ged/test.cpp
229ffcb7f5a92726aa8a286034f39fa37eb9c33876f59fa327a67cff622033dc  univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md
c5e4786dfc903703a679ed16ff6f105e6d470a40c7dfce1c450560d600cb0c69  univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp
0807c06db720bb02f70609f2acb684ee4172d8b8f2066effae4f365dc52328b1  univariateML-cpp/include/cppunivariate/distributions/genbeta/test.cpp
41da7688ffd15077d58be0c854faa352545c267c260e4cdb36f6d1d02b22ccfb  univariateML-cpp/include/cppunivariate/distributions/huber/README.md
faeed200d34f03b75783f96a8bfaca7127fc96b77787d0e0cd9af41eb9eaa07f  univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp
1b77a0f8e50dc7df808c6ab8c607f914f7e7d1292937b23c4474922beb690314  univariateML-cpp/include/cppunivariate/distributions/huber/test.cpp
c6fdf56811674598ed7ad2e0f74614927e8b597a2a06b569f2458182012f0d1e  univariateML-cpp/include/cppunivariate/distributions/lst/README.md
c573ef030c64a8c6e5fbeb0a033b36f1ebdef756cad31ea7e184fbd72f56447a  univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp
cb38963aea9e927747b96a2ae524acd87e9900071f3c545582ba9f472d93ee6c  univariateML-cpp/include/cppunivariate/distributions/lst/test.cpp
7bd590e4f5f981cb6b601ded0f814759459d1311cf8c916a6562312b8fd80324  univariateML-cpp/include/cppunivariate/distributions/pig/README.md
984835cdd8542b4992144f69c969cb82b70ed0a388cbdce171e538dd2c807b31  univariateML-cpp/include/cppunivariate/distributions/pig/mle.hpp
1632294f81faebcd5f35eabe4936907320c61d592d3dc42bc98a4552ee01d6e5  univariateML-cpp/include/cppunivariate/distributions/pig/test.cpp
4567c14341693dd7bbb102e6545c937b5ac31b0a1459d2d0ac0cdf14375b375a  univariateML-cpp/include/cppunivariate/distributions/sn/README.md
84deed9277800077fa671f6b2a8fca619cbdbc3a7beb918e3a1e839e5bf130d2  univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp
97828fb54940be2c3d7521692a2be01477ab1bf238bef34f7029b7978da24ae3  univariateML-cpp/include/cppunivariate/distributions/sn/test.cpp
e0f2b4be462e7810b5d5092e586976f28008c86a3367164e2e89180d46308904  univariateML-cpp/include/cppunivariate/distributions/snorm/README.md
ef7a292d83968aa6fe0b516522c94da8b6b2fe29954d61b7cbffb2a3f2ae5f65  univariateML-cpp/include/cppunivariate/distributions/snorm/mle.hpp
a13c844c3ed50261440de714cf30f1505f35976f246c495ff6eff1552d3d7bd0  univariateML-cpp/include/cppunivariate/distributions/snorm/test.cpp
989a11eec731968935c35217236ac1a45a4b50fb933472e2c0ccb00cd14c0a80  univariateML-cpp/include/cppunivariate/distributions/st/README.md
f497151928389e6076b449709b570255d717a061f439a36fb44c3cf58024aa50  univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp
364fe0048ec383f098feb764f254183538f767ff718c08a6f21412bfbcf98195  univariateML-cpp/include/cppunivariate/distributions/st/test.cpp
835229d638bc5f196b1f2f7e06b2691aeb119a3ca4b55a51817f60d048d6bc49  univariateML-cpp/include/cppunivariate/distributions/std/README.md
75279715fdc15e72769d5d0abb9ca0c936249931fad6549eac35697ad5f85aa9  univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp
bf1154d2427460d741230e690c6da321d93fb679780800e798645ce50e5a760a  univariateML-cpp/include/cppunivariate/distributions/std/test.cpp
```

The prescribed artifact-pane `show` command was attempted after this report
was written. It returned HTTP 403 because this worktree is outside the viewer's
configured readable roots. The report remains at the requested worktree path;
viewer configuration and files outside this worktree were left untouched.
