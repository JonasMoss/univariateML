#!/usr/bin/env python3
"""Write the final TASK-102.5 report only after every foreground stage passed.
Run from repository root under /tmp/umvl-cpu/run. Uses retained cell results;
never reruns Monte Carlo. Keeps source identities and final suite summaries.
"""
import hashlib, json, math, os, platform, re, subprocess
from pathlib import Path
from scipy import stats
ROOT=Path(__file__).resolve().parents[2]
OUT=ROOT/'dev/research/evaluations/inference-5'
LOG=ROOT/'build/scratch'
stages=(LOG/'inference-5-final-stages.log').read_text()
for name in ['cpp-dev','cpp-no-long-double','calibration','r-dev','cpp-full','r-full']:
    assert f'PASS {name}\n' in stages,f'Unfinished or failed {name}'
summary=json.loads((OUT/'summary.json').read_text());rows=summary['rows']
assert len(rows)==120 and summary['reps']==5000 and summary['reference_reps']==10000
assert all(r['reps']==5000 and r['failures']==0 for r in rows)
for path,digest in summary['hashes'].items():
    assert hashlib.sha256((ROOT/path).read_bytes()).hexdigest()==digest,f'Calibration source changed: {path}'
results={}
for name in ['cpp-dev','cpp-no-long-double','cpp-full']:
    log=(LOG/f'inference-5-{name}.log').read_text()
    m=re.search(r'100% tests passed, 0 tests failed out of (\d+)',log);assert m,name
    results[name]={'passed_shards':int(m[1]),'failed_shards':0}
for name in ['r-dev','r-full']:
    log=(LOG/f'inference-5-{name}.log').read_text()
    matches=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',log)
    assert matches,name
    fail,warn,skip,passed=map(int,matches[-1]);assert fail==warn==0
    results[name]={'fail':fail,'warn':warn,'skip':skip,'pass':passed}
# Deterministic topology check, independent closed form 10*log(25/9).
command='triang 0.5 10 .1 .1 .1 .1 .1 .9 .9 .9 .9 .9\n'
reply=subprocess.check_output([str(ROOT/'build/scratch/inference-5-probe')],input=command,text=True)
v=list(map(float,reply.split()));expected=10*math.log(25/9)
assert v[0]==v[2]==0 and v[1]==0 and abs(v[3]-expected)<2e-12
assert v[3]>stats.chi2.isf(.05,1)
topology={'input':command,'output':reply,'expected_lr':expected,'critical_chi_square_1_95':float(stats.chi2.isf(.05,1)),'tolerance_absolute':2e-12,'result':'passed'}
(OUT/'topology.json').write_text(json.dumps(topology,indent=2)+'\n')
# Retain the reason for the deliberately pointwise LR API in the canonical note.
note=ROOT/'univariateML-cpp/include/cppunivariate/core/inference.md'
text=note.read_text()
marker='A connected triangular LR interval is false in general.'
if marker not in text:
    text=text.replace('### Validation and retained choices', '''A connected triangular LR interval is false in general. With known endpoints
0 and 1 and five observations each at .1 and .9, the two global mode maxima
are .1 and .9. The LR at .5 is 10*log(25/9)=10.2165124753, exceeding the
95% chi-square(1) cutoff, whereas LR at both maxima is zero. Thus the constant
cutoff confidence set has separated components. Small distinct perturbations
of the observations preserve the strict rejection at .5, so ties are not the
reason. This is an algebraic counterexample, independently replayed through
the production bridge in the retained `inference-5/topology.json` check.
Enumerating all components and controlling null-specific simulation error
remain necessary for a general simulated-LR confidence-set interface.

### Validation and retained choices''')
    note.write_text(text)
version=subprocess.check_output(['Rscript','-e','cat(R.version.string, as.character(packageVersion("BH")), system.file("include",package="BH"),sep="\\n")'],text=True).splitlines()
environment={'platform':platform.platform(),'compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],'R':version[0],'BH':version[1],'BH_include':version[2],'python':summary['environment'],'thread_limits':{k:os.environ.get(k) for k in ['OMP_NUM_THREADS','OPENBLAS_NUM_THREADS','MKL_NUM_THREADS']}}
revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
branch=subprocess.check_output(['git','branch','--show-current'],cwd=ROOT,text=True).strip()
assert revision==summary['source_revision']
paths=list(summary['hashes'])+['univariateML-cpp/include/cppunivariate/core/inference.md','dev/validation/inference-5-report.py','dev/validation/inference-5-check.sh']
identities={p:hashlib.sha256((ROOT/p).read_bytes()).hexdigest() for p in paths}
record={'source_revision':revision,'branch':branch,'environment':environment,'source_hashes':identities,'final_runs':results,'stages':stages,'calibration_identity':summary['identity'],'topology':topology}
(OUT/'source-identities.json').write_text(json.dumps(record,indent=2)+'\n')
# Compact tables: one row per setting/procedure and four sample-size columns.
def table(family):
    selected=[r for r in rows if r['family']==family]
    keys=list(dict.fromkeys((r['regime'],r['parameter'],r['procedure']) for r in selected))
    out=['| Setting | Procedure | n=10 | n=30 | n=100 | n=1000 |','|---|---|---:|---:|---:|---:|']
    for regime,param,method in keys:
        setting={'laplace':'Laplace(0,1)' if regime=='model' else 'Normal(0,1) alternative','pareto':f'a={param}, b=2','exp':'rate=2, origin=0','norm':'mean=2, sd=1.5','unif':'Uniform(0,1)','triang':f'a=0, b=1, c={param}'}[family]
        values=[]
        for n in [10,30,100,1000]:
            r=next(r for r in selected if (r['regime'],r['parameter'],r['procedure'],r['n'])==(regime,param,method,n))
            se=r['mcse']
            if method=='mode LR simulated':
                m=summary['reference_reps'];k=math.ceil(.95*(m+1));p=k/(m+1)
                se=math.sqrt(p*(1-p)*(1/r['reps']+(1-1/r['reps'])/(m+2)))
            values.append(f"{r['coverage']:.4f} ± {se:.4f}")
        out.append('| '+setting+' | '+method+' | '+' | '.join(values)+' |')
    return '\n'.join(out)
refs=[]
for path in OUT.glob('triang-model-*.json'):
    cell=json.loads(path.read_text());assert cell['signature']['identity']==summary['identity']
    r=cell['rows'][0];refs.append((r['parameter'],r['n'],cell['reference_critical']))
ref_table='| Relative mode c | n | Simulated 95% LR critical |\n|---:|---:|---:|\n'+'\n'.join(f'| {c} | {n} | {q:.6f} |' for c,n,q in sorted(refs))
sign_rows=[]
for n in [10,30,100,1000]:
    k=max([0]+[k for k in range(1,n//2+1) if stats.binom.cdf(k-1,n,.5)<=.025])
    coverage=1-2*stats.binom.cdf(k-1,n,.5) if k else 1
    sign_rows.append(f'| {n} | {k} | {coverage:.8f} |')
sign_table='| n | k | Exact nonrandomized sign coverage |\n|---:|---:|---:|\n'+'\n'.join(sign_rows)
limiting=2*stats.norm.cdf(stats.norm.isf(.025)*2/math.pi)-1
report=f'''# TASK-102.5: exact and nonsmooth inference, 2026-10-31

Implemented and calibrated the requested C++ procedures in branch `{branch}`,
from source revision `{revision}`. All final suites passed. The six opt-in
family `inference.hpp` headers extend the existing interval/status contract;
13 appended doctest cases exercise analytic pivots, equivariance, invalid input,
confidence levels and underflow. No fitter, authored R/JS source, default fit,
golden, special-function backend or d/p/q/r implementation changed.

The canonical mathematical and algorithm documentation is
[core/inference.md](../../../univariateML-cpp/include/cppunivariate/core/inference.md).
[Source identities and final run records](inference-5/source-identities.json)
identify the uncommitted implementation. No commit or board edit was made.

## Procedures and theory

All exact model claims below are finite-sample real-arithmetic results under
iid sampling from the stated model. Quantiles are numerical calls through the
existing no-throw special-function seam. Independent checks support tested
points, not universal floating-point error bounds. Alpha defaults to .05.

| Procedure / C++ function | Hypothesis, nuisance and theory |
|---|---|
| `laplace_location_wald_interval` | Laplace location with scale fitted by mean absolute deviation about the existing upper sample median. Per-observation Fisher is diag(1,1)/sigma² in (mu,sigma), full rank with zero cross term. SE=sigma_hat/sqrt(n). The median quantile expansion and consistent scale give root-n model-normal inference. The Laplace family is DQM despite its kink; its ordinary sample location Hessian is unsuitable. This is asymptotic, not exact or robust under another density. |
| `median_sign_interval` | Unique median of independent continuous observations sharing that median; iid in this calibration. Binomial(n,1/2) sign inversion returns [X_(k),X_(n-k+1)] with the largest k whose lower tail at k-1 is at most alpha/2. Coverage is at least 1-alpha, with no randomization; k=0 returns the real line. Scale is irrelevant. With positive density at the median the endpoint error is order n^(-1/2). Atoms/ties do not retain the stated binomial equality. |
| `median_sandwich`, `median_sandwich_interval` | Under iid continuous sampling with unique median m and g(m)>0, median variance is 1/(4 n g(m)²). Gaussian KDE at the upper sample median uses h=1.06*(sample MAD/log(2))*n^(-1/5); MAD is median absolute deviation, also using the upper median. No bandwidth floor or fallback is hidden. Zero/nonfinite MAD or unresolved variance fails. The median estimating equation has sensitivity g(m) and meat 1/4; this is a nonsmooth sandwich, not an ordinary Hessian inverse. Under local KDE consistency and a positive consistent MAD scale, studentized median inference is root-n valid without the Laplace model. With two smooth local derivatives the density error is O_p(n^(-2/5)); at a Lipschitz Laplace cusp it is O_p(n^(-1/5)). These are density estimation rates, not coverage-error rates. |
| `pareto_shape_exact_interval` | Pareto a>0, b>0. If b is unknown, T=sum(log(X_i/min X)); exponential memorylessness gives 2aT ~ chi-square(2(n-1)), n>=2. If b is supplied as a known constant, use sum(log(X_i/b)) and 2n df. Divide chi-square quantiles by 2T. Unknown endpoint b has rate n and joint DQM fails; this does not invalidate the shape pivot or its root-n asymptotic behavior. Known-b shape alone is DQM with finite information 1/a². Both versions are model-only. |
| `exp_rate_exact_interval` | Rate lambda, origin known to be zero: 2 lambda sum X ~ chi-square(2n), n>=1 and positive total. Divide the quantiles by 2 sum X. Exact under the exponential model; ordinary rate inference is also regular root-n with information 1/lambda². No unknown origin is fitted. |
| `unif_exact_interval` | Both a,b unknown, n>=2, positive sample range R. From the joint extreme density, R/(b-a) ~ Beta(n-1,2). Endpoint gaps (min X-a)/R and (b-max X)/R have survival (1+t)^(-(n-1)). Inverting these pivots gives exact marginal intervals for a, b and width. Their errors/lengths have order 1/n. Free endpoints are non-DQM; no joint Fisher Wald assertion is made. The three marginal intervals are not a simultaneous 95% box. |
| `unif_endpoint_known_opposite_interval` | The other endpoint is a known constant, n>=1. The extreme gap divided by width has Beta(n,1) law; reflect for the lower endpoint. Translating/reflection of this interval gives the width interval with identical coverage events, so those width results are also covered by the known-opposite rows below. This endpoint submodel remains non-DQM with rate n. |
| `norm_variance_exact_interval` | Positive normal variance, mean fitted: Q=sum((X-Xbar)²), Q/variance ~ chi-square(n-1), n>=2. If the mean is supplied as a known constant, use deviations from it and n df, allowing n=1. Interval [Q/q_upper,Q/q_lower] targets variance, not sd. A centered online sum avoids subtractive cancellation. Regular root-n theory also applies; the pivot is exact only under normal sampling. It complements the existing exact t interval for the mean. |
| `triang_mode_wald_interval`, `triang_mode_tests` | Known a<b and interior a<c<b. The score is -1/(c-a) left of c and 1/(b-c) right of c; per-observation information is 1/((c-a)(b-c)), DQM and rank one. Ordinary negative pointwise Hessian has expectation -I and omits the kink contribution. Consistency, bounded threshold-score empirical processes and the local likelihood expansion yield the efficient root-n model limit. Wald uses sqrt((c_hat-a)*(b-c_hat)/n). Pointwise score and LR statistics have asymptotic chi-square(1) references; independently simulated null LR critical values supply the finite-model alternative. No robust triangular procedure, free-endpoint joint covariance, boundary-mode procedure or general two-endpoint LR interval is exposed. |

Known-parameter arguments mean known constants, not nuisance estimates plugged
back in as though known. Fits used by triangular inference must succeed under
the existing certified finite-candidate fitter. All numerical fields are
unusable on failure. Empty/invalid samples, invalid probabilities/constants,
zero denominators/spread and unresolved arithmetic return statuses; no capped
search estimate, pseudoinverse or Hessian rescue was introduced. The sign
interval's deliberate infinite limits are the sole infinite-endpoint success.

## Calibration design and interpretation

The launcher and streaming production bridge reuse the inference-4 design:
[launcher](../../validation/inference-5.sh),
[calibration](../../validation/inference-5.py),
[probe](../../validation/inference-5-probe.cpp).
There are 40 independently seeded cells, 5,000 validation samples each,
and 120 procedure/setting/sample-size rows. Triangular LR uses a separate
10,000-sample reference bank for each of 12 mode/n cells: 200,000 validation
and 120,000 reference datasets in total. Sampling is single-threaded NumPy;
actual production C++ intervals/tests determine acceptance. Failures count as
noncoverage rather than being discarded: **zero procedure failures** occurred.

Seed is `{summary['seed']}` plus the integer from the first 12 hex digits of
SHA256(cell key); reference keys have a separate suffix. Source-hashed,
workload-keyed cells are atomically saved and resumable. [Summary](inference-5/summary.json)
and the per-cell JSON files retain counts, conditional MCSEs, reference critical
values and signatures. Initial 100-replicate smoke cells were replaced because
the final workload signature differs. No tuning was selected from the final
Monte Carlo outcomes.

Tables report coverage probabilities, with **± one Monte Carlo standard error**.
For fixed procedures MCSE=sqrt(p_hat*(1-p_hat)/5000). Simulated-LR entries include
both validation and reference-bank uncertainty: with M=10000 and
k=ceil(.95*(M+1)), the unconditional null acceptance probability is
p0=k/(M+1)=9501/10001 for a continuous statistic. Reference probability MCSE is
sqrt(p0*(1-p0)/(M+2))=0.00218; total coverage SE is
sqrt(p0*(1-p0)*(1/N+(1-1/N)/(M+2)))=0.00377. The raw validation-only SE remains
in JSON. The bank is shared within its validation cell, so ignoring its
uncertainty would understate error. Two-SE bands are about 95% Monte Carlo
uncertainty bands, not statistical confidence intervals for the parameters.

Independent endpoint checks passed at rtol 2e-10 and atol 2e-12: SciPy gamma,
beta/Lomax, normal and binomial calculations, Gaussian KDE, and exhaustive
triangular sample-knot/design-endpoint likelihood comparison on the retained
check sample. Analytic df=2 exponential quantiles and affine/reflection
identities are separately exercised in doctest. These are finite numerical
checks; the pivot derivations supply the exact-law claims.

### Laplace and the continuous normal alternative

{table('laplace')}

The model Wald is materially anticonservative at small n, and still below
nominal at n=1000 under Laplace. Under the normal alternative its limiting
coverage is 2 Phi(z_.975*2/pi)-1 = {limiting:.6f}, since the fitted Laplace
scale tends to sqrt(2/pi) while the median variance is pi/(2n). The observed
0.7896 at n=1000 agrees with that separate calculation. Computing a model
Fisher variance therefore does not establish misspecification robustness.

The declared KDE rule oversmooths the Laplace cusp and yields conservative
sandwich coverage on this grid. Under the smooth normal alternative it is
close to nominal, with some finite-n error. The sign interval has the following
model-independent nonrandomized coverage, explaining its conservative rows:

{sign_table}

### Pareto shape, known and unknown endpoint

{table('pareto')}

The a=.7 case has infinite original-data mean; the exact log-spacing pivot
needs no original-data moment assumption. It is distinct from fitting a
regular joint endpoint/shape model.

### Exponential rate and normal variance

{table('exp')}

{table('norm')}

### Uniform endpoints and width

{table('unif')}

Each row is a marginal interval under the declared known/fitted nuisance
setting; no simultaneous coverage claim follows from these rows.

### Triangular mode with known endpoints

{table('triang')}

{ref_table}

Triangular Wald is not a reliable finite-n default on this grid, even at
n=1000. Unadjusted chi-square LR undercovers at small n; null-specific simulated
LR is much closer to nominal, with the reference error included above.
The score statistic also has discreteness effects: its count below null c is
Binomial(n,(c-a)/(b-a)); the current production API returns its model-information
statistic, not an exact binomial inversion.

A connected LR interval cannot simply be assumed. With known a=0,b=1 and five
observations each at .1 and .9, the two global mode maxima are .1 and .9, but
LR(.5)=10 log(25/9)={expected:.10f} exceeds the 95% chi-square(1) cutoff.
The accepted set has separated components; small distinct perturbations preserve
the strict rejection, so this is not an artifact confined to ties. The
[production replay](inference-5/topology.json) matches the algebra within 2e-12.
After building the launcher, reproduce with:

```sh
printf 'triang 0.5 10 .1 .1 .1 .1 .1 .9 .9 .9 .9 .9\\n' | build/scratch/inference-5-probe
```

## Recommendations and TASK-102.6 availability handoff

Use the exact pivots under their declared models, and prefer the sign interval
for finite-sample median coverage. Keep Laplace model Wald and the fixed-rule
median sandwich explicit asymptotic choices with the observed finite-sample
caveats. For fully specified triangular nulls with known endpoints, prefer
null-specific simulated LR testing over the calibrated grid's poor Wald
performance; do not interpolate these 12 critical values into an asserted
continuous confidence interval.

All new availability below is **opt-in C++ only**. R inference integration is
outside this job; passing R regressions does not expose these procedures in R.

| Target / regime and nuisance | Available after this job | Precise limitation / open work |
|---|---|---|
| Laplace location; scale fitted, sigma>0 | Model Fisher Wald, root-n; calibrated | Finite-n undercoverage. Ordinary location Hessian unsuitable. No exact Fisher-Wald coverage claim. |
| Continuous unique median; scale irrelevant | Conservative exact sign/order-statistic interval | Atoms/ties require a separate convention/theory; no randomization. |
| Median under a continuous alternative, g(m)>0 | MAD-bandwidth density sandwich and root-n interval; Laplace and normal calibration | No uniform finite-sample guarantee or validated adaptive/bias-corrected bandwidth; zero MAD fails. |
| Pareto shape, a,b>0; b known | Exact chi-square pivot, 2n df | Model-only; not a robust shape procedure. |
| Pareto shape; b fitted as min X | Exact chi-square pivot, 2(n-1) df, n>=2 | Joint endpoint/shape remains non-DQM; no joint Wald covariance inferred. |
| Exponential rate; origin zero known | Exact chi-square interval, 2n df | Unknown origin is outside the contract. |
| Continuous uniform endpoints; both fitted | Exact marginal endpoint and width intervals, n>=2 | Rate n, non-DQM. No simultaneous-box guarantee. |
| Continuous uniform single endpoint; opposite known | Exact endpoint/translated width interval, n>=1 | Rate n, non-DQM. Known means genuinely fixed. |
| Normal variance; mean fitted or known | Exact chi-square interval, n-1 or n df | Normal-model claim; nonnormal variance inference needs a separate method and moments. |
| Triangular interior mode; endpoints known | Model Wald interval; pointwise model score/LR; independent simulated LR calibration | Wald undercoverage. General simulated-LR confidence-set API needs all components, null-specific calibration and controlled simulation error. A single connected LR interval is disproved above. |
| Triangular mode at a or b, or endpoints fitted | No new procedure | Interior information/limit argument does not cover these regimes. Boundary-mode code needs a separate one-sided analysis and calibration; free endpoints have divergent squared scores. |
| Triangular mode under misspecification | No robust procedure | Sensitivity must include the kink and true g(c), may vanish or change sign; density/sensitivity estimation, identification and calibration remain unvalidated. |

The robust triangular sensitivity candidate is stated in the canonical note;
its algebra is not a validated robust procedure. The open entries concern
regimes or interfaces beyond the implemented/calibrated hypotheses, not
unfinished requested interior calibration.

## Final validation and reproduction

Every build/test ran as a foreground command through its own four-slot lock,
with nice level 10 and BLAS/OpenMP threads pinned to one. No simbox, agents,
package installs, Git writes or board edits were used. TMPDIR, JUST_TEMPDIR
and XDG_RUNTIME_DIR were set to `$PWD/build/tmp`; scratch/logs are in
`build/scratch/`. The calibration stage wall time was 10m24s including the
shared-slot wait; one streaming worker computed the cells, within the stated
one-hour/four-core ceiling. Monte Carlo used the default compiler arithmetic;
no-long-double coverage here is deterministic test coverage, not a second
Monte Carlo experiment.

| Final command (each prefixed `/tmp/umvl-cpu/run4`) | Result |
|---|---|
| `just cpp-test` | {results['cpp-dev']['passed_shards']}/{results['cpp-dev']['passed_shards']} dev shards passed |
| `just cpp-test-no-long-double` | {results['cpp-no-long-double']['passed_shards']}/{results['cpp-no-long-double']['passed_shards']} shards passed |
| `bash dev/validation/inference-5.sh --reps 5000 --reference-reps 10000` | 40 cells, 120 rows, zero procedure failures; independent comparisons passed |
| `just test` | FAIL 0, WARN 0, SKIP {results['r-dev']['skip']}, PASS {results['r-dev']['pass']} |
| `env UMVL_TEST_TIER=full just cpp-test` | {results['cpp-full']['passed_shards']}/{results['cpp-full']['passed_shards']} full shards passed |
| `just test-full` | FAIL 0, WARN 0, SKIP {results['r-full']['skip']}, PASS {results['r-full']['pass']} |

R dev skips are the declared full-tier exclusions plus the unavailable optional
`sn` comparison; R full has only that optional-package skip. New doctests add
no skip or permitted-failure annotations. CTest reports shard status and does
not expose successful-case assertion totals. Existing compiler/roxygen
messages are not a warning-free build claim; the table's WARN counts are
specifically testthat results. During implementation two compile-only errors
(an unavailable binomial seam name and a mixed-type test initializer) were
corrected before these final green runs. No failed numerical calibration cell
was excluded. Tests/fixtures were not regenerated to change expected estimates.

Environment: {environment['platform']}; {environment['compiler']};
{environment['R']}; BH {environment['BH']}; Python
{summary['environment']['python']}, NumPy {summary['environment']['numpy']},
SciPy {summary['environment']['scipy']}. Counts belong to the tested bfdf257
source tree plus the recorded changes, rather than the older 14598b7 baseline.
The retained record has full hashes and stage timestamps; no commit was made.

To reproduce all six foreground runs, their logs and the report, run
`bash dev/validation/inference-5-check.sh` from the repository root. The
[controller](../../validation/inference-5-check.sh) acquires each reservation
itself; do not wrap the controller in another slot lock. To regenerate only
the report from existing green logs, use
`/tmp/umvl-cpu/run python3 dev/validation/inference-5-report.py`.
The launcher reuses cells only when source hashes, seed and workloads match;
`--reset` forces deterministic regeneration. The postprocessor refuses to
write this report unless all six stages passed and retained calculation-source
hashes still match. The final source identity file additionally records the
current algorithm note and report generator.
'''
report_path=ROOT/'dev/research/evaluations/2026-10-31-inference-5-report.md'
report_path.write_text(report)
print(report_path)
print(json.dumps(results,indent=2))
print('Report complete; topology check passed; calculation-source hashes matched.')
