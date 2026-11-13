"""Write the report after final validation, including any documented targeted correction."""
import csv,json,pathlib,re
D=pathlib.Path(__file__).resolve().parent
checks=json.loads((D/'final-results.json').read_text())
required=['cpp-test','cpp-test-no-long-double','test','test-full','js-build','js-test']
assert list(checks)==required
assert all(checks[k]['exit_code']==0 for k in required if k not in ('test','js-test'))
assert checks['js-test']['exit_code']==0 or checks['js-test'].get('corrective_exit_code')==0
assert checks['test']['exit_code']==0 or (checks['test'].get('corrective_exit_code')==0 and checks['test-full']['exit_code']==0)
assert all((D/checks[k]['log']).exists() for k in required)
summary=json.loads((D/'summary.json').read_text())
for interface in ('cpp','r'):
 before=summary[interface+'-before'];after=summary[interface+'-after']
 assert before['n']==after['n'] and after['up']==after['invalid']==after['crossing_failure']==0
assert 'identical True' in (D/'sampler-cpp-comparison.txt').read_text()
assert re.search(r'identical\s+TRUE',(D/'sampler-r-comparison.txt').read_text())
rows=list(csv.DictReader((D/'counts.csv').open()))
counts={(r['interface'],r['stage'],r['configuration'],r['mode']):r for r in rows}
modes=['lower-linear','lower-log','upper-linear','upper-log']
labels={'zip':'pois inflated','ztpois':'pois truncated','zmpois':'pois modified',
 'zib':'binom inflated','ztbinom':'binom truncated','zmbinom':'binom modified',
 'zinb':'nbinom inflated','ztnbinom':'nbinom truncated','zmnbinom':'nbinom modified',
 'ztgeom':'geom truncated','zmgeom':'geom modified','geom-inflated':'geom inflated',
 'zmlogarithmic':'lgser modified','tpois':'pois finite-window fixtures',
 'tbinom':'binom finite-window fixtures','zipf':'zeta finite-window fixtures',
 'zeta-window':'zeta signed/zero windows'}
def label(key):
 if key in labels:return labels[key]
 if '-window-' in key:
  base,bounds=key.split('-window-',1);lo,hi=bounds.split('-',1)
  return f'{base} [{lo}, {hi.replace("Inf","∞")}]'
 return key.replace('-none',' none')
def cell(key,interface):
 if (interface,'before',key,modes[0]) not in counts:return '—'
 return ' / '.join(str(counts[(interface,'before',key,m)]['raw_misses'])+'→'+str(counts[(interface,'after',key,m)]['raw_misses']) for m in modes)
order={'pois':0,'binom':1,'nbinom':2,'geom':3,'lgser':4,'zeta':5}
keys=sorted({r['configuration'] for r in rows},key=lambda k:(order[label(k).split()[0]],label(k)))
table='\n'.join('| '+label(k)+' | '+cell(k,'cpp')+' | '+cell(k,'r')+' |' for k in keys)
link=lambda name:f'[{name}]({D.name}/{name if (D/name).exists() else name+chr(46)+chr(103)+chr(122)})'
suites='\n'.join(f'| `just {k}` | {checks[k]["summary"]} | {link(checks[k]["log"])} |' for k in required)
text=f'''# TASK-215: modifier-route quantiles

Starting tree: `c167a97580db656bba2e8153d970cd4de69c8f64`. All **58,072 C++** and **59,234 R** eligible cases now satisfy the first adjusted crossing: **zero upward, invalid or crossing failures**. Before there were 914 C++ and 999 R crossing failures. The 628 earlier answers after the change, in each interface, are permitted crossings of atoms smaller than the requested band.

**Rule.** The existing `special::discrete_reached` rule uses `e = 64 * DBL_EPSILON` relative to the supplied number, including a supplied logarithm. Targets are lower linear `p*(1-e)`, upper linear `p*(1+e)`, lower log `p*(1+e)` and upper log `p*(1-e)`, with inclusive lower `>=` and upper `<=` crossings and the existing endpoint/overflow/near-one guards. For a window `W=[L,U]`, compare the conditional quantities `F_W(k)=Pr(L≤X≤k)/Pr(W)` or `S_W(k)=Pr(k<X≤U)/Pr(W)` on that supplied scale. The band is relative to the conditional probability or log probability, rather than a parent probability after adding a discarded prefix. Integer inversion brackets and verifies the first crossing. At a first atom whose interval subtraction is unresolved, the R inverse uses its mass `d(k_min)/Pr(W)`; the interior resolution guard remains.

The modifier kernels, signed finite-zeta evaluator, R composition and JS integer composition follow that rule. Half-atom shortcuts no longer bypass the public comparison. Sampling inverses use unadjusted comparisons. Finite-window inversion expands its bracket from the retained lower bound, avoiding remote finite-size likelihood-ratio walks. First-crossing correctness follows from monotonicity, conditional on the computed comparisons; the fixture counts and sampler replays are numerical regression evidence.

**Round trips.** Each cell gives raw equality misses `q(p(k)) != k`, **before→after**, in the order **lower linear / lower log / upper linear / upper log**. Earlier answers are accepted only at the first adjusted crossing; the tests require equality when the preceding atom is outside the band. Eligible probabilities are interior and non-flat. Original fixture windows contain 201 counts; supplemental finite/one-sided/whole windows contain five counts at every parameter point. The C++ column is native precision; the full double-mode suite repeats the assertions. Complete eligible counts and error categories are in {link('counts.csv')}, with points/windows in {link('cases.json')} and raw {link('cpp-before.csv')}, {link('cpp-after.csv')}, {link('r-before.csv')} and {link('r-after.csv')}.

| Configuration | C++ raw misses | R raw misses |
|---|---|---|
{table}

Appended tests cover all retained configurations in both interfaces, dyadic-probability truncated/inflated binomial rational ties, positive geometric ties, signed finite-zeta rational masses, and the conditional geometric masses `(8,4,2,1)/15`. The independent rational generator records fractions and 100-digit log conversions: {link('exact.py')}, {link('exact.json')}. Existing public log-half/subnormal-odds expectations were updated for the requested band; raw sampling comparisons remain checked. The original R first-atom/refusal regression was retained and passes. JS tests additionally check conditional rational jumps and separation of public fuzz from sampling. A diffuse-geometric regression checks the final finite binary64 bracket.

**Samplers and fits.** C++ replay: **92,160 draws, byte-identical**; R replay: **398 models, 135,824 entries, identical**, including the 512 inherited NaNs noted below. Original-kernel replays use 512 draws per point; supplemental R windows use 16. See {link('sampler-cpp-comparison.txt')}, {link('sampler-r-comparison.txt')} and the compressed replay files. Actual WASM/JS replay additionally checked **59,218 eligible round trips** (990 crossing failures before, zero after; 628 allowed earlier answers) and **135,809 identical sampler draws**. See {link('js-composition-counts.json')} and {link('js-sampler-layer-comparison.json')}. The extra JS replay uses one count/draw for the expensive huge-binomial one-sided point; C++/R retain their full supplemental grids. Reproduce with {link('build-js-baseline.sh')} and {link('probe-js.mjs')}. The C++ stream SHA-256 is `f150f268d16305a8dd5e35b4728025c85c24d3f52f88cde1b0f26e7c14d5ab75`.

Both precision modes have **0/1,934 fit differences** against the unchanged starting tree, ignoring no fields: {link('starting-final-native-diff.txt')}, {link('starting-final-double-diff.txt')}. Both also have **0/1,934 differences** against the supplied retirement snapshots, ignoring only the three requested outcome fields: {link('retirement-final-native-diff.txt')}, {link('retirement-final-double-diff.txt')}. The unchanged starting tree also matched those retirement snapshots; no inherited Poisson/hypergeometric repair difference occurred on this grid. Snapshots and compiler/source provenance are retained beside these comparisons.

**Final suites.** The runs below finished on the **workstation**, sequentially through `/tmp/umvl-cpu/run4`, with pinned worker threads and nice priority. C++ uses GCC 15. No simbox run was attempted.

| Command | Result | Evidence |
|---|---|---|
{suites}

The dev R run found one stale ZIB log-half expectation (10 instead of the new public answer 0). Only that expectation was corrected; its targeted recheck passed 280 assertions, and the subsequent full R suite passed all 65,256. The JS suite similarly passed 579 checks and found the same stale ZIB log-half expectation in its existing test. Its targeted correction and both new JS modifier tests passed (5/5 selected checks). These late changes were confined to tests, so full suites were not repeated, following the requested targeted-recheck rule; see {link('final-js-test-recheck.log')}. See {link('final-test-zib-recheck.log')}.

The final source changes are modifier quantile/sampling headers under `pois`, `binom`, `nbinom`, `geom`, `lgser` and `zeta`, `bindings/r/R/modifiers.R`, the JS modifier composition/sampling adapter, and modifier tests; {link('source-files.txt')} lists every source file. Fit algorithms, frozen fixtures and fit expectations were retained. Reproduce measurements with {link('run-probes.sh')} (`before`, then `after`); it reconstructs the baseline commit into ignored scratch rather than retaining a source archive. Fit snapshots use the original brief's `fit-snapshot.py --modifier-routes` commands.

**Open inherited evaluation gaps.** These were measured before the change and remain explicit, not successful round trips. The 13 empty conditioning events are expected refusals; the three other constructor refusals are listed in {link('r-before-exclusions.json')} and {link('r-after-exclusions.json')}.

1. NB `size=1e8, prob=.5`, windows `[2,12]` and `[0,5]`: R refuses the conditional normalizer at its existing floating-point resolution guard. Missing: a stable conditional normalizer for those extremely small events. The probability evaluator was not loosened.
2. Base `lgser`, `zero="none", theta=0`: the R frozen-model domain rejects this boundary, while the C++ evaluator includes the point-mass limit. Missing: a consistent R domain/closure decision; changing held fits was outside this job.
3. Base `nbinom`, `zero="none", size=0, prob=.5`: R's unchanged `stats::rnbinom` delegate returns NaNs (512/512 in the replay); the C++ sampler returns zero. Missing: R boundary-sampler handling. This pre-existing stream was preserved as required. Reproducer and case: {link('inspect-samplers.R')}, {link('sampler-baseline-limits.jsonl')}.
'''
(D.parent/'2026-11-13-qtie-2-report.md').write_text(text)
