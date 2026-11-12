"""Write the requested report only after all suites and audits have passed."""
import csv
import gzip
import json
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
PREFIX = HERE.name
REPORT = HERE.parent / '2026-11-11-quantile-rule-report.md'
MODES = ('native', 'double', 'js', 'r')
counts = {m: json.loads((HERE / (m+'-counts.json')).read_text()) for m in MODES}
probes = {m: json.loads((HERE / (m+'-probe-verified.json')).read_text())[0] for m in MODES}
runs = []
for name in ('quantile-rule-runs.tsv', 'quantile-rule-native-runs.tsv'):
    runs.extend(list(csv.reader((ROOT/'build/scratch'/name).read_text().splitlines(), delimiter='\t')))
assert {r[0] for r in runs} == {'js-build','js-test','cpp-test','cpp-test-no-long-double','test','test-full'}
assert len(runs) == 6 and all(r[2] == '0' for r in runs)
assert all(c['forward_failures'] == c['backward_accepted'] == 0 for c in counts.values())
assert all(all(n == 0 for n in c['families'].values()) for c in counts.values())
for m in MODES:
    assert json.loads((HERE/(m+'-decisions.json')).read_text()) == []
    p = probes[m]
    assert not p['forward_accepted'] and p['backward_accepted']
    assert p['oracle_probability_error'] < 1e-12 and p['cdf_error'] < 1e-13
    assert p['precision_delta'] < 1e-30 and p['stored_root_error_ulps'] <= .5
    assert p['absolute_quantile_error'] <= p['center_error_bound']
assert len({(p['actual'],p['cdf']) for p in probes.values()}) == 1

ansi = re.compile(r'\x1b\[[0-?]*[ -/]*[@-~]')
summary = []
for suite,machine,rc,start,finish,path in runs:
    raw = (ROOT/path).read_bytes()
    clean = ansi.sub('', raw.decode(errors='replace')).replace('\r','\n')
    (HERE/(suite+'.log.gz')).write_bytes(gzip.compress(raw, mtime=0))
    summary.extend([f'{suite} | {machine} | exit {rc} | {start} -> {finish}',
                    *clean.splitlines()[-30:], ''])
(HERE/'validation-summary.log').write_text('\n'.join(summary)+'\n')
(HERE/'runs.tsv').write_text('\n'.join('\t'.join(r) for r in runs)+'\n')
(HERE/'simbox-attempt.log').write_text((ROOT/'build/scratch/quantile-rule-native-launch.log').read_text().split('cpp-test on')[0])
families = sorted(counts['native']['families'])
with (HERE/'family-counts.csv').open('w') as out:
    writer = csv.writer(out)
    writer.writerow(['family', *MODES])
    for f in families:
        writer.writerow([f, *(counts[m]['families'].get(f, '') for m in MODES)])

def link(name): return f'{PREFIX}/{name}'
p = probes['native']
lines = ['# TASK-197: backward probability acceptance for continuous quantiles', '',
 'Implemented the backward probability test in the C++, R and JS truth consumers, appended the requested Frechet regression, and documented the rule in the validation guide. All required suites and the final independent audits passed. No active stored quantile row changes its acceptance at this baseline.', '',
 'Baseline: `7825e912b3e5177254853cc020d52b662faeeaa8`, branch `codex/quantile-rule`. Runs completed on 2026-11-12. The counts below use this worktree’s unchanged active truth tables; they do not include another job’s subsequent truth regeneration.', '',
 '## Consumer change', '',
 '- [C++ consumer](../../../univariateML-cpp/tests/test_support.hpp): after the relative/floor and bracket tests fail, evaluate the family CDF at the computed quantile with the row’s original tail and scale.',
 '- [R consumer](../../../bindings/r/tests/testthat/test-generated-dpqr.R): apply the same fallback to the failing continuous rows using the family’s generated `p` function and the current parameters.',
 '- [JS consumer](../../../bindings/js/test.mjs): apply the same fallback through the family’s own WASM `p` binding.', '',
 'The extra acceptance condition is `abs(CDF(q_hat) - input) <= 1e-12 * abs(input)` for natural inputs, or `1e-12 * max(1, abs(input))` for log inputs. The implementation requires finite computed and reference quantiles, an interior probability input, and a finite CDF value. Endpoint and nonfinite acceptance retain their previous rules. Relative quantile tolerances, zero floors, brackets, ambiguous lattice endpoints and the large-discrete minimality gate are unchanged; discrete families never use the new fallback.', '',
 'The [validation guide](../../validation/README.md) adds one sentence describing the new rule. No family d/p/q/r code, special functions, truth generator/table, code generator, default fitter or golden was changed.', '',
 '## Per-family deciding-row counts', '',
 'A deciding row fails every existing forward rule and passes the new probability rule. The standalone audit extracts the actual d/p/q adapters from the family tests and checks every active continuous quantile row. It repeats the same forward predicate, including floors and brackets, before evaluating any backward candidate. The R and JS audits call the actual generated bindings; the R audit covers every available row, a superset of the consumer’s tier sampling.', '',
 '| Observer | Continuous families | Quantile rows | Forward failures | Newly accepted |',
 '|---|---:|---:|---:|---:|']
for m,label in [('native','C++ native'),('double','C++ no-long-double macro'),('js','JS/WASM'),('r','R generated functions')]:
    c=counts[m]
    lines.append(f"| {label} | {len(c['families'])} | {c['total_rows']:,} | {c['forward_failures']} | {c['backward_accepted']} |")
lines += ['', '**Deciding stored rows: none in any family or observer.** Thus there are no newly forgiven stored-reference errors and no deciding stored rows with insufficient CDF accuracy. The complete counts are listed below and in [family-counts.csv]('+link('family-counts.csv')+'). A dash means the family is outside that generated binding’s consumer, rather than an unmeasured zero.', '',
 '| Family | C++ | C++ noLD | R | JS |', '|---|---:|---:|---:|---:|']
for f in families:
    lines.append('| '+f+' | '+' | '.join(str(counts[m]['families'].get(f, '—')) for m in ('native','double','r','js'))+' |')
lines += ['', 'JS coverage omits: `'+ '`, `'.join(sorted(set(families)-set(counts['js']['families'])))+'`. R coverage omits: `'+ '`, `'.join(sorted(set(families)-set(counts['r']['families'])))+'`. The existing R consumer excludes stats-dispatched families and absent generated functions; neither binding’s family registry was changed.', '',
 'The empty deciding-row lists are retained as [native]('+link('native-decisions.json')+'), [noLD]('+link('double-decisions.json')+'), [R]('+link('r-decisions.json')+') and [JS]('+link('js-decisions.json')+'). Excluded/archived truth rows are not active consumer inputs and are not counted.', '',
 '## Frechet probe: genuine ill-conditioning', '',
 'The appended [C++ regression](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/test.cpp) uses exactly `qfrechet(-7.3890561137087625, 1e9, -999999998, 1e9, true, true)`. It checks the backward log-probability property and the absolute error bound `4 * epsilon * max(1, abs(mu + sigma))`; here the characteristic center `mu + sigma` is 2. This is a regression case, not a truth-table row.', '',
 'The native C++, noLD, R and JS observations agree bit for bit for this probe. Independent evaluations use exact binary64 inputs in the existing mathematical Frechet route at 80 and 160 decimal digits. The direct inverse `mu + sigma * exp(-log(-input) / lambda)` also verifies the supplied reference root, whose nearest double is within 0.452 ulp of the high-precision root.', '',
 '| Quantity | Result |', '|---|---:|',
 '| Independent quantile | `-1.9991476033510408592152e-18` |',
 f"| Computed quantile | `{p['actual']:.17g}` |",
 f"| Relative forward error | `{p['forward_relative_error']:.6g}` |",
 f"| Absolute quantile error | `{p['absolute_quantile_error']:.6g}` |",
 f"| Center-based absolute error bound | `{p['center_error_bound']:.6g}` |",
 f"| Own log CDF at the computed quantile | `{p['cdf']:.17g}` |",
 f"| Independent backward probability error | `{p['oracle_probability_error']:.6g}` |",
 f"| Own CDF error against the independent CDF at q_hat | `{p['cdf_error']:.6g}` |",
 f"| 80/160-digit CDF agreement | `{p['precision_delta']:.6g}` |",
 f"| Relative quantile condition number | `{p['relative_condition_number']:.6g}` |", '',
 'Both probability-error figures and the precision-agreement figure are normalized on the log input’s scale, `max(1, abs(input))`. The condition number uses the independent density and the derivative of the selected log tail at the reference root. Its magnitude explains the large relative coordinate error: the root is nearly zero while the probability residual is near binary64 rounding error. The independently evaluated probability residual is more than 8,000 times smaller than the allowed `1e-12`, so the own-CDF check is not concealing a wrong probability at this probe.', '',
 'This is high-precision numerical evidence, not an outward-rounded error enclosure or a proof of CDF accuracy throughout every family’s domain. There are no other deciding stored rows requiring a CDF accuracy claim. Raw probe observations, reference comparisons and conditioning metrics are retained for [native]('+link('native-probe-verified.json')+'), [noLD]('+link('double-probe-verified.json')+'), [R]('+link('r-probe-verified.json')+') and [JS]('+link('js-probe-verified.json')+'). The R evidence writer converts string-encoded parameters exactly as the consumer does and explicitly verifies 17-digit numeric round trips.', '',
 '## Finished validation', '',
 '| Command | Machine | Final result |', '|---|---|---|',
 '| `just js-build` | Workstation | Exit 0 |',
 '| `just js-test` | Workstation | 615/615; zero failures |',
 '| `just cpp-test` | Workstation fallback, GCC 15 | 108/108 dev shards |',
 '| `just cpp-test-no-long-double` | Workstation fallback, GCC 15 | 135/135 shards, including full labels |',
 '| `just test` | Workstation fallback | FAIL 0, WARN 0, SKIP 154, PASS 25,600 |',
 '| `just test-full` | Workstation fallback | FAIL 0, WARN 0, SKIP 0, PASS 52,358 |', '',
 'Simbox was unreachable and `simbox-run` returned 75 before its native suite batch could start. The same batch ran locally through `run4`, as required by the fallback instruction. The no-long-double target therefore includes the required workstation run; no GCC 13/simbox result is claimed. See the [availability log]('+link('simbox-attempt.log')+').', '',
 'Local builds and parallel suites reserved four slots and used at most four workers; the independent evidence work used one slot. BLAS/OpenMP were pinned to one thread and the wrappers applied nice priority 10. Commands ran in the foreground and were waited to completion, including CPU reservation waits. The native batch held its reservation only across sequential builds/suites. Temporary files and scratch logs stayed under this worktree’s `build/`.', '',
 'No production fix followed these final suites. The R audit checker was corrected for numeric decoding and full-precision evidence serialization, then its complete row audit, round-trip assertions and high-precision reference checks passed. No suite or implementation tolerance was changed. Site checks were not triggered: no family dossier or public fitter signature changed.', '',
 'The [validation summary]('+link('validation-summary.log')+') and [run records]('+link('runs.tsv')+') retain results, execution machines and timestamps; wrapper timestamps include reservation waits. Full compressed logs: '+', '.join('['+r[0]+']('+link(r[0]+'.log.gz')+')' for r in runs)+'.', '',
 '## Reproduction and source integrity', '',
 'From the worktree root, after building the R and JS bindings:', '',
 '```sh',
 'export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp',
 '/tmp/umvl-cpu/run bash '+str(HERE.relative_to(ROOT))+'/evidence.sh',
 '```', '',
 'The [observer and reference checker]('+link('audit.py')+'), [R observer]('+link('audit-r.R')+') and [JS observer]('+link('audit-js.mjs')+') rebuild or call the real family kernels and retain all decision lists. The evidence run checks every active continuous row, then independently checks every new decision and the separate regression probe. The [provenance record]('+link('provenance.json')+') records the baseline, compiler/interpreter versions, source and observed-binary hashes, and hashes for the unchanged family d/p/q/r, truth and golden files. No source trees were archived.', '',
 'Open implementation or CDF-accuracy findings for TASK-197: none. Truth tables, default fits, goldens, settled family kernels and the forbidden project-status files are unchanged. No commit, push, publication or board edit was made.', '']
REPORT.write_text('\n'.join(lines))
print(REPORT)
