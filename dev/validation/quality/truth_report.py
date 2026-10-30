"""Assemble the fix queue from oracle inventory and standalone C++ audit CSVs.

Usage: python -m quality.truth_report /tmp/umvl-codex/dpqrtruth/checks
The audit is independent of the expected-failure decorations in test.cpp.
"""
import csv
import json
import re
import sys
from quality.truth import DISTS, REPORT, ROOT


def main():
    checks = __import__('pathlib').Path(sys.argv[1])
    probe = {}
    with (ROOT/'dev/research/evaluations/2026-10-29-dpqr-probe/results.csv').open() as f:
        for row in csv.DictReader(f):
            probe[(row['id'],row['check'])] = row
    aliases = {'log_d':'d_log','log_lower':'p_lower_log','log_upper':'p_upper_log',
               'lower':'p_lower','upper':'p_upper','q_lower_log':'q_lower_log','q_upper_log':'q_upper_log',
               'q_lower':'q_lower','q_upper':'q_upper'}
    rows=[]; gaps=[]; pending=[]; decorated=[]; missing=[]; generated=[]; timings=[]; sampling=[]
    for folder in sorted(DISTS.glob('*/README.md')):
        id=folder.parent.name; statusfile=REPORT/(id+'.json')
        status=json.loads(statusfile.read_text()) if statusfile.exists() else dict(status='pending')
        if status['status']=='generated':generated.append(status)
        elif status['status']=='gap':gaps.append((id,status['reason']))
        else:pending.append(id)
        test=folder.with_name('test.cpp').read_text()
        if re.search(r'TEST_CASE\("'+id+r': d/p/q/r against the truth table" \* doctest::may_fail',test): decorated.append(id)
        if not re.search(r'\bdouble\s+q\w+\s*\(',folder.with_name('dpqr.hpp').read_text()):missing.append(id)
        path=checks/(id+'.csv')
        if path.exists():
            with path.open() as f:
                for row in csv.reader(f):
                    previous=probe.get((id,aliases.get(row[1],'')),{})
                    rows.append(row+[previous.get('ours_wrong',''),previous.get('ref_wrong','')])
        else:
            kind='oracle_gap' if status['status']=='gap' else ('density_only_api' if id in missing else 'not_audited')
            rows.append([id,kind,'',status.get('reason',''),'','','',''])
        timing=checks/(id+'.seconds')
        if timing.exists():
            seconds=float(timing.read_text());timings.append((id,seconds))
            marker=test.find('"'+id+': d/p/q/r against the truth table"')
            count=re.search(r', 1e-10, 1e-9, (\d+)\);',test[marker:])
            draws=int(count[1]) if count else 20000
            table=json.loads(folder.with_name('truth.json').read_text())
            sampled=len(table.get('sampler_points',table['parameters']))
            sampling.append([id,sampled,draws,sampled*draws,seconds])
    with (REPORT/'summary.csv').open('w') as f:
        writer=csv.writer(f); writer.writerow(['family','check_kind','number_failing','worst_point','ours','truth','probe_ours_wrong','probe_reference_wrong']);writer.writerows(rows)
    with (REPORT/'sampling.csv').open('w') as f:
        writer=csv.writer(f);writer.writerow(['family','parameter_points','draws_per_point','total_draws','seconds']);writer.writerows(sampling)
    failures=sorted({r[0] for r in rows if r[2] and int(r[2])})
    old_failures={id for (id,kind),r in probe.items() if r.get('ours_wrong','').isdigit() and int(r['ours_wrong'])}
    common=sorted(set(failures)&old_failures)
    additional=sorted(set(failures)-old_failures)
    cleared=sorted((old_failures&{r[0] for r in rows if r[2]})-set(failures))
    slow_sampling='; '.join(f'{r[0]}: {r[2]} draws/point, {r[4]:.3f} seconds' for r in sampling if r[2]<20000)
    sampler_failures=sum(int(r[2]) for r in rows if r[2] and r[1].startswith('r_'))
    corrections=json.loads((REPORT/'support-corrections.json').read_text()) if (REPORT/'support-corrections.json').exists() else []
    lines=['# Independent d/p/q/r truth tables', '',
      f"Generated: {len(generated)}/98 families, {sum(r['points'] for r in generated)} parameter points, {sum(r['values'] for r in generated)} x rows and {sum(r['quantiles'] for r in generated)} quantile rows.", '',
      '`python -m quality truth [ids]` and `just dpqr-truth [ids]` generate tables. Run either command through `/tmp/umvl-cpu/run`, as for other repository computations. It returns nonzero when any requested family is a gap. No C++, R, stats, extraDistr, actuar or scipy probability functions are used as truth.', '',
      'Parameter rule: sampler arguments and fitted estimates, then one coordinate at a time. Distances to a single finite domain bound are multiplied by 0.1 and 10; bounded intervals move 90% towards either edge; unrestricted coordinates are multiplied by 0.1 and 10 (zero moves to -1 and 1). Integer, design, fixed and endpoint parameters stay at fitted golden values. Historical points are deduplicated and capped at eight; the README default is appended if absent. Sampler checks use the explicitly indexed README default and golden fit. Family-local special points take priority: GEV and GPD xi, and Tukey lambda lambda, use -5e-10, 0, +5e-10. Triangular endpoints and the Frechet lower endpoint are held by local routes. Local routes also mark the two negative-binomial size parameters mislabeled trials as continuous. Later coordinates can receive only sampler/fit coverage when the eight-point cap is reached.', '',
      'The shared parser preserves the quality harness\'s existing spec.py preference. Truth normally uses the exact README expression because several existing specs are approximations valid only at the fitted golden. Every exception and support correction lives beside its family in truth.py; README front matter is untouched.', '',
      'Numerics: at least 80 decimal digits (up to 600 for small shapes). Continuous normalization integrates the declarative density in a logarithmic support chart, split at independent quantiles and local kinks, with tail mass 1e-32 omitted at each end; the integral must differ from 1 by at most 1e-25 and the quadrature error estimate must be at most 1e-28. Antiderivative derivatives are checked against the density. Family-local beta and latent-normal changes of variable avoid endpoint cancellation; their omitted masses are bounded explicitly. Tukey lambda integrates its implicit density in quantile coordinates. Positive discrete sums use finite sums, telescoping identities, exact special functions or a geometric remainder below 1e-45. At enormous integer x, explicit lower/upper series bounds instead certify relative error below 1e-35 on the stored log value. Tables are written atomically only after every point passes verification.', '',
      'Both tails are evaluated through the smaller tail, using expm1 for complements. Roots are bracketed in a logarithmic support coordinate; discrete roots use integer bisection. Natural and logarithmic inputs are rounded to binary64 BEFORE computing their answers. Quantile rows store the rounded true answer and independent adjacent-double brackets. Overflow requires signed infinity; underflow rounds to zero or the finite support endpoint. Endpoint densities are one-sided limits of the declared formula. JSON numbers round-trip binary64 (at most 17 significant digits); nonfinite values are strings.', '',
      'Grid: both-tail quantiles at 1e-300, 1e-100, 1e-30, 1e-12, 1e-6, .01, .25, .5; support edges and adjacent doubles; 1e100 and 1e300 in support. Discrete tables add up to 200 body integers and a noninteger. Quantile inputs additionally include log p=-800 and -10000 and p=0,1.', '',
      'C++ checks use 1e-10 relative error on log values (absolute when magnitude <=1), 1e-10 relative on natural probabilities >1e-300, and 1e-9 relative on quantiles or the independently stored double bracket. Each parameter point normally uses 20000 StdRng draws with seed 20261029+point; body CDF checks use five binomial standard errors plus 1/n. The standalone audit records failures even when doctest permits the case to fail.', '',
      '`summary.csv` contains all observed failures, worst parameter-point index and input, and ours/truth values. Probe columns retain its counts for comparison only: its grid and 1e-6 tolerance differ, and its README documents false positives. Oracle gaps and unaudited families are explicit rows, never zero failures.', '',
      'Observed failing families: '+(', '.join(failures) or 'none audited yet')+'.', '',
      'Cases decorated may_fail: '+(', '.join(decorated) or 'none')+'.', '',
      'Density-only APIs (tests untouched as requested): '+', '.join(missing)+'.', '',
      f"Sampler checks: {sum(r[1] for r in sampling)} parameter points, {sum(r[3] for r in sampling):,} draws; {sampler_failures} failing empirical CDF or validity checks. Total sampler time {sum(t for id,t in timings):.3f} seconds. Per-family counts and timings are in sampling.csv. Reduced sampling: {slow_sampling or 'none'}. The reason is documented in the corresponding family test.cpp.", '',
      f'Probe comparison: {len(common)} failing families agree with the earlier probe; {len(additional)} additional failing families appear on the larger grid and tighter tolerances. Audited probe failures now passing: '+(', '.join(cleared) or 'none')+'. The half-Cauchy oracle uses its direct lower atan tail, avoiding the extreme-lower false positives from recovering it by subtraction.', '',
      f'Support metadata differs from the actual density support in {len(corrections)} families at the first point: '+', '.join(r['family'] for r in corrections)+'. Full first-point bounds are in support-corrections.json; no front matter was edited.', '',
      '## Gaps', '']
    lines += [f'- {id}: {reason}' for id,reason in gaps]
    validation=REPORT/'validation.json'
    if validation.exists():
        v=json.loads(validation.read_text());final=v['final'];audit=v['independent_cpp_audit']
        lines += ['', '## Validation', '',
                  f"Recorded {v['recorded_at']}, base {v['base_commit']}: {final['ctest_targets_passed']}/{final['ctest_targets_total']} ctest targets passed ({final['ctest_seconds']:.2f}s). Distribution tests: {final['distribution_cases_passed']} passed, {final['distribution_cases_skipped']} skipped oracle gaps; {final['may_fail_cases']} cases permit known failures.",
                  f"R: FAIL {final['R']['FAIL']} | WARN {final['R']['WARN']} | SKIP {final['R']['SKIP']} | PASS {final['R']['PASS']}; the skip is the optional sn package. Python: {final['python_tests_passed']} tests passed, including five closed-form validations to 1e-15 relative error and extreme-count/half-tail regression checks. Original/current quality results for norm and zip are identical.",
                  f"Standalone C++ audit: {audit['cases_including_loader']} cases, {audit['assertions']} assertions; {audit['numeric_mismatches']} numerical mismatches and {audit['unexpected_exception_assertions']} unexpected Boost exception assertions. These feed the fix queue; ctest permits only the recorded failing family cases. Worst normalization residual: {v['normalization']['worst_absolute_residual']}; no NaN cells. Details: validation.json."]
    if pending:lines+=['', 'Pending: '+', '.join(pending)+'.']
    (REPORT/'README.md').write_text('\n'.join(lines)+'\n')

if __name__=='__main__':main()
