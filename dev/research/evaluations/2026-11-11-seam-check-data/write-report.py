"""Freeze finished validation evidence and write the job report."""
import collections,gzip,hashlib,json,subprocess
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
REPORT=HERE.parent/'2026-11-11-seam-check-report.md'
def read(name):return json.loads((HERE/name).read_text())
s=read('final-summary.json');refs=read('reference-reconciliation.json')
full=read('final-runs.json');latest=read('final-invgauss-runs.json')
assert next(r for r in full if r['name']=='test-full')['returncode']==0
assert all(r['returncode']==0 for r in full if 'cpp-test' in r['name'])
assert all(r['returncode']==0 for r in latest)
assert all(r['returncode']==0 for r in read('final-r-retest-runs.json'))
assert all(r['returncode']==0 for r in read('final-js-targeted-runs.json'))
assert not read('link-check.json')['errors']
assert next(r for r in read('late-invgauss-runs.json') if r['name']=='site-check')['returncode']==0
assert s['after_owned_fitter']==0 and s['records']==11284 and len(refs)==32
ig=read('invgauss-maximized-value.json');beta=read('beta-maximized-value-final.json')
assert all(r['checks'][-1]['reported_relative_error']<=1e-10 for r in ig)
assert all(r['checks'][-1]['shape_relative_error']<=1e-10 for r in ig if r['arithmetic']=='double')
with gzip.open(HERE/'before.jsonl.gz','rt') as f:before=[json.loads(l) for l in f]
with gzip.open(HERE/'final.jsonl.gz','rt') as f:after=[json.loads(l) for l in f]
ids='bbinom beta binom invgauss nbinom nsbeta tnorm tpois zmbinom ztbinom'.split()
changed=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
allowed={'dev/validation/README.md'}
for ident in ids:
    allowed.update([f'bindings/r/distributions/{ident}/test.R',f'univariateML-cpp/include/cppunivariate/distributions/{ident}/test.cpp'])
for ident in 'bbinom beta binom invgauss nbinom tnorm tpois ztbinom'.split():
    allowed.add(f'univariateML-cpp/include/cppunivariate/distributions/{ident}/mle.hpp')
for ident in ('beta','invgauss'):allowed.add(f'univariateML-cpp/include/cppunivariate/distributions/{ident}/README.md')
assert set(changed)<=allowed,set(changed)-allowed
tests=[p for p in changed if p.endswith(('test.cpp','test.R'))]
for p in tests:
    old=subprocess.check_output(['git','show','HEAD:'+p],cwd=ROOT)
    assert (ROOT/p).read_bytes().startswith(old),p
policy=dict(baseline='3b2a16fed4787c71b96bf23b1cb86569973b0a9c',
            tracked_paths=changed,existing_test_files_preserved_as_prefix=len(tests),
            protected_paths_untouched=True,golden_files_untouched=True)
(HERE/'source-policy-check.json').write_text(json.dumps(policy,indent=2)+'\n')
logdir=HERE/'validation-logs';logdir.mkdir(exist_ok=True)
logs=[
 'final-logs/cpp-test.log','final-logs/simbox-cpp-test-no-long-double.log',
 'final-logs/workstation-cpp-test-no-long-double.log','final-logs/test.log','final-logs/test-full.log',
 'beta-current-cpp.log','beta-current-cpp-double.log',
 'verified-cpp-invgauss.log','verified-cpp-invgauss-double.log',
 'current-r-beta-invgauss-dev.log','late-site-check.log','final-js-test.log',
 'current-js-beta-dependent-tests.log','verified-js-build.log','verified-js-invgauss.log',
 'final-simbox-invgauss-double.log','final-links.log']
index={}
for name in logs:
    p=ROOT/'build/scratch/seam-check'/name
    assert p.exists(),p
    dest=logdir/(p.name+'.gz');dest.write_bytes(gzip.compress(p.read_bytes(),mtime=0))
    index[name]=str(dest.relative_to(HERE))
(HERE/'log-index.json').write_text(json.dumps(index,indent=2)+'\n')
for p in list(HERE.glob('*.jsonl'))+list(HERE.glob('*.log')):
    p.with_suffix(p.suffix+'.gz').write_bytes(gzip.compress(p.read_bytes(),mtime=0));p.unlink()
link=lambda name,label=None:f'[{label or name}](2026-11-11-seam-check-data/{name})'
fmt=lambda v:f'{v:.3g}'
fence=chr(96)*3
lines=[f"""# Fitter logLik accuracy across families — TASK-150.2.7

Job dated 2026-11-11; final checks finished 2026-11-12 (Europe/Oslo).
Worktree: seam-check, branch codex/seam-check. Baseline: 3b2a16fed4787c71b96bf23b1cb86569973b0a9c.

**All 20 owned fitter-side returned-point logLik errors are fixed, across ten families.**
Fresh native refits cover 11,185 archived datasets and 99 goldens. Of 8,757 returned fits,
none puts an observation outside its exact support. Three fitter errors owned by other
jobs and ten protected density errors remain. Six fits have explicit guarded-open
entries below. An additional double-only inverse-Gaussian maximum error is fixed.

The required C++ suites and full R suite passed. After family-only late fixes, the
affected native, R and JS checks passed; full suites were not repeated. No commits,
golden rewrites, dependency installations, board edits or protected probability-code
changes were made.

## Census and method

The input archive is the existing
[frozen fit-audit corpus](2026-11-07-fit-audit-data/cases.json.gz), together with each
current family's golden sample. Every dataset was refitted with the current
binding-neutral adapter and its existing controls and holds; archived estimates
were not reused. Gengamma is probability-only at this revision and has no fitter
or golden, so the fitter census contains 99 families.

The reusable [checker](../../validation/seam-check.py) compares reported logLik with
a compensated sum of the settled scalar log densities at the returned public
coefficients. The threshold is 1e-10 times max(1, abs(logLik)). Each disagreement
is attributed using an [independent density reference](../../validation/seam_reference.py)
at exact binary64 data and coefficients: at least 100 digits, raised for large
parameters up to 1,600 digits, and repeated with another 50 digits.

All 32 original disagreements and all 13 retained disagreements have resolved
independent values. After fixes, an existing independent value was reused only
when the data and coefficient bit patterns were identical; the changed beta and
nonstandard-beta points were recomputed at 200 and 250 digits. See
{link('reference-reconciliation.json')}.
Support bounds are compared using exact rational arithmetic on binary64 inputs,
including transformed endpoints and integer truncation.

| Outcome | Before | After |
|---|---:|---:|
| Reported value agrees with summed density | {s['before']['pass']} | {s['after']['pass']} |
| Disagreement requiring attribution | {s['before']['mismatch']} | {s['after']['mismatch']} |
| Native stop | {s['before']['stop']} | {s['after']['stop']} |
| Guarded fit without a terminal result | {s['before']['open fit']} | {s['after']['open fit']} |
| Fitter-side errors, all owners | {s['before_fitter']} | {s['after_fitter']} |
| Fitter-side errors owned by this job | {s['before_owned_fitter']} | {s['after_owned_fitter']} |
| Density-side errors | {s['before_dpqr']} | {s['after_dpqr']} |
| Returned fits outside support | 0 / 8757 | 0 / 8757 |

One beta-binomial case originally had both a fitter and a density error. Its
fitter value is now correct, but its protected density remains wrong; this is why
20 fitter repairs remove 19 raw disagreements. Every previously returned fit still
returns a fit, and every previously terminal native stop is preserved. Seven
previous guard expirations yielded native stops in the longer retry, rather than
being converted into stops by this checker.

This is a likelihood-value and support audit. It does not establish coefficient
accuracy or global search certification for every returned fit. In particular,
correcting a final likelihood does not independently certify every comparison
inside an existing discrete search.

## Changes and per-fixed-family evidence

The following errors are relative to the independent density at the public
point, not relative to the continuous likelihood maximum.

| Family | Errors before → after | Worst error before | Worst error after | Correction |
|---|---:|---:|---:|---|
"""]
descriptions={
'bbinom':'Gamma-ratio identity for large counts and small held shapes; compensated rising-factorial sums; tiny-shape endpoint regression.',
'beta':'Accurate public-shape density sum, including held fits; adjacent shape representations when the internal continuous value misses the public-point target.',
'binom':'Accurate public PMF for the held-probability result; ordinary free fits retained.',
'invgauss':'Evaluate the public density sum. Double-only polish uses the centered continuous mean, continuous MLE shape, and analytic maximum gap.',
'nbinom':'Accurate held-probability and supplied-candidate likelihood, including final certificate evaluation.',
'nsbeta':'Inherited beta correction; binding and native delegate regressions.',
'tnorm':'Re-evaluate the returned coefficients and actual estimated bounds with the accurate truncated density.',
'tpois':'Re-evaluate the final rate and actual estimated truncation bounds.',
'zmbinom':'Inherited zero-truncated-binomial correction; binding and native delegate regressions.',
'ztbinom':'Accurate normalized binomial PMF at the final public coefficients.'}
for ident in ids:
    r=[v for v in refs if v['family']==ident];b=s['by_family'][ident]
    lines.append(f"| {ident} | {b['before_fitter']} → {b['after_fitter']} | {fmt(max(v['before_fitter_error'] for v in r))} | {fmt(max(v['after_fitter_error'] for v in r))} | {descriptions[ident]} |\n")
maxgold=max(abs(r['before_logLik']-r['after_logLik']) for r in s['golden_payload_changes'])
ig_ext=next(r for r in ig if r['arithmetic']=='extended')['checks'][-1]
ig_double=next(r for r in ig if r['arithmetic']=='double')['checks'][-1]
rb=read('r-beta-density.json')
lines.append(f"""

C++ regressions were appended for all ten families, and R regressions cover
the binding paths. Existing test files remain exact prefixes of their edited
versions. No existing expectation was weakened. The large-count beta-binomial R
test leaves size free so that it exercises the fitter rather than the all-held
density shortcut.

All 99 native golden coefficient vectors remain bitwise unchanged. Six golden
refit likelihood payloads change only in roundoff, by at most {maxgold:.3g} in
absolute value; fixture files are unchanged. The only native census coefficient
changes are beta case 100 and its nonstandard-beta delegate, case 10786: shape2
moves up one ulp. The additional double-only inverse-Gaussian replay changes
the two concentrated duplicates to restore the maximum; its golden is unchanged.

### Maximum-value checks and mathematical scope

For beta case 100, merely recomputing the density at the old public shapes would
lose 1.6031e-10 relative to the maximum. The smooth maximum has no cusp allowance.
The neighboring shape2 representation reduces that loss to 4.8285e-12.
The independently evaluated maximum is 135.27888712924001718478; the final
public-point value is 135.27888712858683. Both digamma scores were solved at
180 and 240 digits with an analytic polygamma Jacobian; objective values agree
to 100 digits. Strict concavity gives uniqueness of an exact stationary point;
the root and values here are numerical evidence, not outward-rounded intervals.
See {link('beta-maximized-value-final.json')} and
{link('beta-max-check.py')}. The
[beta dossier](../../../univariateML-cpp/include/cppunivariate/distributions/beta/README.md)
records the mean-ratio sensitivity.

For inverse Gaussian, Q(m)=sum((x-m)^2/(m^2*x)) has derivative
2*(n*m-sum(x))/m^3. Thus the exact mean is its unique minimum, and the
continuous MLE shape is n/Q(mean). The exact likelihood gap can be written as
(n/2)*(u-1-log(u)+log(Q(public_mean)/Q(exact_mean))),
where u=public_shape*Q(public_mean)/n. The double-only correction evaluates
this gap with centered residuals and log1p terms, avoiding subtraction of close
Q values. It compares neighboring public means at the rounded continuous MLE
shape. A computed gap exceeding the likelihood target triggers replacement;
unresolved calculations preserve the existing result.

In cases 3835 and 3838 the old double-only public mean was one ulp away from
the nearest mean representation. Its reported value was 1.4575e-10 below the
true maximum, even after density-sum agreement had been restored. The final
relative maximum-value error is {ig_double['reported_relative_error']:.4g};
the final double-only shape error is {ig_double['shape_relative_error']:.4g}.
The exact-input continuous maximum is 123.89205063011402599215.
The native extended-precision likelihood has relative maximum error
{ig_ext['reported_relative_error']:.4g}. Independent 100/160-digit closed-form
checks, adjacent-representation checks and the actual WASM result are retained in
{link('invgauss-maximized-value.json')}, {link('invgauss-maximized-value-before-polish.json')},
{link('invgauss-max-check.py')} and {link('invgauss-wasm.json')}.
The [inverse-Gaussian dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invgauss/README.md)
records its leading mean-rounding loss.

These are exact-arithmetic likelihood identities conditional on the computed
comparisons, with the practical floating-point target 1e-10. The high-precision
checks establish margins for these regressions; they are not interval proofs
covering all future inputs. No new non-Lipschitz allowance was needed for an
owned family.

## Retained issues and ownership

### Fitter likelihood errors owned elsewhere

| Family | Archive indices | Holds | Relative fitter error |
|---|---|---|---:|
| burr | 636, 639 | shape1=1.334637717604056 | 1.97645e-7 |
| genbeta | 2568 | shape3=4.084996007320842, scale=1 | 1.45156e-10 |

Their independent density values agree with the settled probability layer.
No source file owned by those jobs was edited. Complete ownership exclusions:

- sged-acc: sged, sstd, snorm.
- acc-fb-cont: burr, invburr, pareto4, paralogis, invparalogis, betapr, gb2,
  fpareto, pareto3, ged, genbeta, bhatt, sn.
- ztpois-pmf: ztpois.
- gg-fit: gengamma, trgamma, invtrgamma. Gengamma has no fitter at this revision.

Other measured families on that list have no returned-point fitter likelihood
miss in this census; native stops and guarded cases remain visible in the table.

### Protected density defects

These ten cases are independently attributed to the density rather than to the
current fitter. Needed changes belong to the protected probability layer.

| Family | Archive indices | Worst relative density error | Needed follow-up |
|---|---|---:|---|
""")
dp=collections.defaultdict(list)
for r in s['remaining_mismatches']:
    if r.get('attribution') in ('dpqr','both'):dp[r['family']].append(r)
for ident,rr in sorted(dp.items()):
    lines.append(f"| {ident} | {', '.join(str(r['index']) for r in rr)} | {fmt(max(r['dpqr_error'] for r in rr))} | Accurate density evaluation at the retained exact inputs; keep the independently correct fitter value. |\n")
lines.append(f"""

Beta-binomial specifically needs the stable large-count PMF identity now used
by its fitter. Density-based all-held shortcuts inherit a remaining density
defect; they were not made to agree by corrupting a correct fitter value.

There is also a separately measured R stats::dbeta problem, outside the native
ten-case count. At the final beta case-100 coefficients, R 4.6.1 gives a density
sum of {rb['stats_dbeta']:.17g}, versus the independent/native value
{rb['independent_point']:.17g}: relative error {rb['stats_relative_error']:.4g}.
The R regression therefore checks the exact coefficients and independent value.
An accurate R density route or upstream repair is needed; the protected d/p/q/r
and code generators were left unchanged. See {link('r-beta-density.json')}.

### Coefficient finding outside this likelihood-only correction

The same closed-form check establishes a native extended-precision inverse-Gaussian
shape error of {ig_ext['shape_relative_error']:.5g} in cases 3835/3838:
the retained native shape is 8.235308862764327e23, while the continuous MLE is
approximately {ig_ext['continuous_shape_binary64']:.17g}. Its likelihood nevertheless
meets the maximum-value target. This existing coefficient error is explicitly
not closed by a likelihood-value check. The native coefficients were retained
under this job's default-fit freeze. A coefficient-accuracy follow-up must use
the centered continuous-mean shape calculation in the native branch and validate
its coefficient contract. The double-only fits changed for the likelihood repair
now satisfy the independent shape check as well.

### Six guarded-open fits

Each case below was actually retried with a 120-second wall-clock guard.
No terminal native outcome was obtained, so there is no public point or logLik
to evaluate. A guard expiration is neither a native budget status nor a proof of
nonexistence. Closing these entries requires a terminal default-control fit,
or runtime analysis that makes that fit obtainable. The input and retry evidence
is retained, including the three final Feller-Pareto retries in the six-family
beta-dependent replay.

| Family | Archive index | Dataset | Holds | Missing |
|---|---:|---|---|---|
""")
for r in s['open_cases']:
    holds=', '.join(f'{k}={v:.17g}' for k,v in r['held'].items()) or 'none'
    lines.append(f"| {r['family']} | {r['index']} | {r['sample']} | {holds} | terminal native result after 120 s |\n")
lines.append("""

## Per-family before/after table

Counts are refits, not unique data arrays: distinct held subsets and archived
duplicates remain distinct cases. A dagger marks ownership elsewhere.
Density counts include a case attributed to both sides before the fixes.

| Family | Refits | Fitter errors before/after | Density errors before/after | Native stops before/after | Guarded before/after |
|---|---:|---:|---:|---:|---:|
""")
for ident,b in sorted(s['by_family'].items()):
    bb=[r for r in before if r['family']==ident]
    bc=collections.Counter(r['outcome'] for r in bb)
    db=sum(r.get('attribution') in ('dpqr','both') for r in bb)
    a=b['after'];mark=' †' if b['owned_elsewhere'] else ''
    lines.append(f"| {ident}{mark} | {b['records']} | {b['before_fitter']} / {b['after_fitter']} | {db} / {b['after_dpqr']} | {bc['stop']} / {a.get('stop',0)} | {bc['open fit']} / {a.get('open fit',0)} |\n")
lines.append(f"""

## Validation and machines

| Check | Machine | Result |
|---|---|---|
| just cpp-test | simbox, GCC 13 | 108/108 dev shards passed |
| just cpp-test-no-long-double | simbox, GCC 13 | 135/135 passed |
| just cpp-test-no-long-double | workstation, GCC 15 | 135/135 passed |
| Late beta-dependent C++ checks, both modes | workstation, GCC 15 | 200/200 cases per mode, 45,870 assertions per mode |
| Final inverse-Gaussian C++ checks, both modes | workstation, GCC 15 | 15/15 cases per mode; maximum and double-only shape assertions passed |
| Final inverse-Gaussian refits, both modes | workstation, GCC 15 | 101/101 density-seam passes per mode |
| just test-full | workstation after simbox exit 75 | FAIL 0, WARN 0, SKIP 0, PASS 52,372 |
| Final affected R dev files, beta and inverse Gaussian | workstation | FAIL 0, WARN 0, SKIP 2, PASS 38 |
| just site-check after both dossier changes | workstation | passed, including 99 generated C++ snippets |
| Beta-dependent JS follow-up | workstation, rebuilt WASM | 54/54 passed |
| Final inverse-Gaussian and facade JS follow-up | workstation, rebuilt WASM | 5/5 passed |
| Final WASM inverse-Gaussian witness | workstation | status, maximum value, shape and density-sum assertions passed |

The full R dev run initially had 25,611 passes, no warnings, 154 tier skips,
and one failure in this job's new beta test: it incorrectly used the inaccurate
stats::dbeta value as its oracle. The new test was corrected to the independently
evaluated point, without changing an existing expectation. The subsequent full
R run and final affected dev files are green.

The initial complete JS run passed 614/615 tests and exposed an overly tight
nonstandard-beta golden roundoff regression. Retaining the valid continuous
likelihood on ordinary beta samples fixed it; the 54 affected tests then passed.
The later inverse-Gaussian correction was checked with its five affected tests
and an exact-input WASM witness. JS tests were not edited.

The full suites were run once, followed by affected-family checks after late
fixes, as instructed. The final GCC 13 inverse-Gaussian attempt returned simbox
exit 75; current GCC 15 targeted checks supply its prescribed local fallback.
The earlier complete GCC 13 run is retained distinctly from that late fallback.
Elapsed times in run records include CPU-slot waits and are not benchmarks.

Run metadata: {link('final-runs.json')}, {link('final-invgauss-runs.json')},
{link('final-r-retest-runs.json')}, {link('final-js-targeted-runs.json')},
{link('late-invgauss-runs.json')}. Complete retained logs:
{link('log-index.json')}. The source-policy check confirms 20 append-only test
files and untouched protected paths: {link('source-policy-check.json')}.

The final diff check is clean. Local destinations in all four edited/new
Markdown files pass the scoped link check. The repository-wide checker reports
45 existing findings, including private missing literature files and parser
false positives; each affected file is byte-identical to the baseline.
See {link('link-check.json')} and the retained repository-wide log.

## Reusing the checker

Run from the repository root with existing validation dependencies:

{fence}sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --output build/scratch/seam-check/fresh.jsonl
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --family beta,tnorm --output build/scratch/seam-check/subset.jsonl
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --family beta --case 100 --timeout 120 --no-long-double --output build/scratch/seam-check/beta-double.jsonl
{fence}

The family filter accepts active fitter IDs; repeated --case options select
archive indices and omit goldens. The default wall-clock guard is 30 seconds.
--no-long-double selects the library's double-only mode.
Outputs are resumable JSONL plus summary and source-provenance JSON. Source,
selection or timeout changes require a fresh output. Compiled probe names also
include the source fingerprint, so separate source versions cannot overwrite a
live probe. Completed retained JSONL is losslessly gzipped; restore it to an
unchanged scratch run before resuming, or use a new output.

The checker was checked against five exact-support boundary probes, four error
gate probes, and deliberate stale-provenance reuse; see {link('check-checker.json')}
and {link('checker-provenance-validation.json')}.
The baseline was compiled before any production edit from the clean starting
revision. Its driver and hash record are retained without archiving source trees:
{link('baseline-provenance.json')}, {link('baseline-driver.cpp.gz')}.
The read-only {link('reproduce-baseline.sh')} reconstructs that revision into
ignored scratch with git archive for a fresh replay.

Primary census evidence: {link('before.jsonl.gz')}, {link('final.jsonl.gz')},
{link('final-summary.json')}. The final six-family dependent replay is
{link('after-current-beta.jsonl.gz')}; the final inverse-Gaussian double-only
replay is {link('after-invgauss-double.jsonl.gz')}. All retained files and
their hashes are listed in {link('evidence-index.json')}.
""")
REPORT.write_text(''.join(lines))
manifest=[dict(path=str(p.relative_to(HERE)),bytes=p.stat().st_size,
               sha256=hashlib.sha256(p.read_bytes()).hexdigest())
          for p in sorted(HERE.rglob('*')) if p.is_file() and p.name!='evidence-index.json']
(HERE/'evidence-index.json').write_text(json.dumps(manifest,indent=2)+'\n')
print('Report written:',REPORT)
print('Retained evidence files:',len(manifest))
