"""Rebuild the installed-package coverage inventory and standalone HTML report.

Run from any directory with Python 3 and Rscript. No fitting or package changes.
Classifications are a reviewed snapshot, not inferred from matching names.
"""
import collections
import csv
import html
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
groups = {}


def add(package, status, entries):
    for name, detail in entries.items():
        key = (package, name)
        assert key not in groups, key
        groups[key] = (status, detail)


add('extraDistr', 'Covered', {
    'dbbinom': 'mlbbinom; size is known, not estimated.',
    'dbetapr': 'mlbetapr; scale estimated or fixed.',
    'dbhatt': 'mlbhatt; stops at its normal and uniform limits.',
    'dbnbinom': 'mlbnbinom; see its identification and boundary policy.',
    'ddgamma': 'mldgamma; data on two adjacent integers escape to a shifted Bernoulli.',
    'ddlaplace': 'mldlaplace; integer location.',
    'ddnorm': 'mldnorm; data on two adjacent integers escape to a shifted Bernoulli.',
    'ddunif': 'mldunif', 'ddweibull': 'mldweibull',
    'dfrechet': 'mlfrechet; free mu restricts lambda >= 1 (GEV xi <= 1), held mu is mlinvweibull on x - mu.',
    'dgompertz': 'mlgompertz', 'dgumbel': 'mlgumbel',
    'dhcauchy': 'mlhcauchy', 'dht': 'mlht',
    'dhnorm': 'mlhnorm(x, mu = 0).',
    'dhuber': 'mlhuber, including estimated epsilon.',
    'dinvgamma': 'mlinvgamma', 'dkumar': 'mlkumar',
    'dlaplace': 'mllaplace', 'dlgser': 'mllgser', 'dlomax': 'mllomax',
    'dnhyper': 'mlnhyper; known population N and stopping count r, estimates m.',
    'dnsbeta': 'mlnsbeta with min and max held (free bounds: unbounded likelihood, explained in the stop message).',
    'dpareto': 'mlpareto (type I)', 'dpower': 'mlpower',
    'drayleigh': 'mlrayleigh', 'dsgomp': 'mlsgomp',
    'dskellam': 'mlskellam with own d/p/q/r (extraDistr has no p/q).',
    'dslash': 'mlslash',
    'dtbinom': 'mltbinom with known bounds and size.',
    'dtnorm': 'mltnorm with known truncation bounds.',
    'dtpois': 'mltpois with known bounds.',
    'dtriang': 'mltriang; endpoints and mode can be fixed.',
    'dzib': 'mlzib with known size.', 'dzinb': 'mlzinb', 'dzip': 'mlzip',
})
add('extraDistr', 'Equivalent', {
    'dbern': 'mlbinom(x, size=1); Bernoulli is binomial.',
    'dgpois': 'mlnbinom; gamma mixing of a Poisson is negative binomial, not generalized Poisson.',
    'dinvchisq': 'mlinvgamma; alpha=nu/2, beta=nu*tau/2.',
    'dprop': 'mlbeta for default prior=0: size=alpha+beta, mean=alpha/size. Fixed positive prior constrains shapes; free prior is not identifiable.',
    'dwald': 'mlinvgauss; same inverse Gaussian family.',
})
add('extraDistr', 'Partial / restricted', {
    'dfatigue': 'mlfatigue fixes mu=0; free location gives an unbounded likelihood (derivation in its README).',
    'dgev': 'mlgev restricts xi to [-0.5,1]; unrestricted xi<-1 admits endpoint blow-up.',
    'dgpd': 'mlgpd uses a fixed threshold and xi>=-0.5.',
    'dlst': 'mllst restricts df>=1; unrestricted df>0 has scale-collapse paths.',
})
add('extraDistr', 'Vector-parameter tier (planned)', {
    'dcat': 'Empirical-frequency MLE; needs parameter vectors of user-chosen length.',
    'dmixnorm': 'Free component scales: unbounded likelihood; held or equal scales are bounded. Label switching, local maxima.',
    'dmixpois': 'Bounded likelihood; component count, identification and merging boundaries.',
})
add('extraDistr', 'Excluded multivariate', dict.fromkeys(
    ['dbvnorm', 'dbvpois', 'ddirichlet', 'ddirmnom', 'dmnom', 'dmvhyper'],
    'Joint law; outside the univariate inventory.'))
add('actuar', 'Covered', {
    'dburr': 'mlburr', 'dgumbel': 'mlgumbel; check location/scale parameter mapping.',
    'dinvburr': 'mlinvburr', 'dinvexp': 'mlinvexp', 'dinvgamma': 'mlinvgamma',
    'dinvgauss': 'mlinvgauss', 'dinvparalogis': 'mlinvparalogis',
    'dinvpareto': 'mlinvpareto', 'dinvtrgamma': 'mlinvtrgamma',
    'dinvweibull': 'mlinvweibull', 'dlgamma': 'mllgamma',
    'dllogis': 'mlllogis', 'dlogarithmic': 'mllgser',
    'dparalogis': 'mlparalogis', 'dpareto': 'mllomax; actuar pareto is type II with zero endpoint.',
    'dpareto1': 'mlpareto; type I, estimates its lower endpoint.',
    'dpig': 'mlpig', 'dtrbeta': 'mlgb2', 'dtrgamma': 'mltrgamma',
    'dzmbinom': 'mlzmbinom with known size.', 'dzmgeom': 'mlzmgeom',
    'dzmlogarithmic': 'mlzmlogarithmic', 'dzmnbinom': 'mlzmnbinom',
    'dzmpois': 'mlzmpois', 'dztbinom': 'mlztbinom with known size.',
    'dztgeom': 'mlztgeom', 'dztnbinom': 'mlztnbinom', 'dztpois': 'mlztpois',
})
add('actuar', 'Covered (endpoint held)', {
    'dpareto2': 'mlpareto2 with min held (mlpareto2(x, min = 0) is mllomax). Free min: unbounded along min=m-epsilon, scale=epsilon, shape<k/(n-k).',
    'dpareto3': 'mlpareto3 with min held (min = 0 is mlllogis). Free min: singular endpoint for shape<1.',
    'dpareto4': 'mlpareto4 with min held (min = 0 is mlburr). Free min: singular endpoint for shape2<1.',
    'dfpareto': 'mlfpareto with min held (min = 0 is mlgb2). Free min: singular endpoint for shape2*shape3<1.',
    'dgenbeta': 'mlgenbeta with scale held (GB1). Free scale: singular upper endpoint for shape2<1.',
})
add('actuar', 'Equivalent', {
    'dlgompertz': 'Alias of dinvweibull; NOT the ordinary Gompertz family.',
    'dpearson6': 'Alias of dtrbeta in this package (all three shapes); mlgb2.',
    'dpoisinvgauss': 'Alias of dpig; mlpig.',
    'dgenpareto': 'mlbetapr: actuar shape1 is beta-prime shape2, actuar shape2 is beta-prime shape1. NOT extraDistr dgpd.',
})
add('actuar', 'Excluded (out of scope)', {
    'dphtype': 'Phase-type: many non-identified representations of one law; general phase-type fitting is a field of its own.',
})
add('actuar', 'Excluded utility', dict.fromkeys(
    ['discretise', 'discretize'], 'Discretization utility, not a density.'))

r = '''for (pkg in c("extraDistr", "actuar")) {
  for (n in sort(grep("^d", getNamespaceExports(pkg), value=TRUE))) {
    cat(pkg, as.character(packageVersion(pkg)), n,
        paste(names(formals(getExportedValue(pkg,n))),collapse=","),sep="\\t")
    cat("\\n")
  }
}'''
lines = subprocess.check_output(['Rscript', '-e', r], text=True).splitlines()
rows = []
for line in lines:
    package, version, name, args = line.split('\t')
    status, detail = groups.pop((package, name))  # Fail on an unreviewed new export.
    rows.append(dict(package=package, version=version, density=name,
                     status=status, detail=detail, arguments=args))
assert not groups, f'Classified exports disappeared: {groups}'
with HERE.joinpath('2026-10-29-density-coverage.csv').open('w') as f:
    writer = csv.DictWriter(f, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)

parts = ['''<!doctype html><html lang="en"><meta charset="utf-8">
<title>extraDistr and actuar: MLE coverage</title>
<style>body{font:16px/1.5 system-ui;max-width:1250px;margin:40px auto;padding:0 24px;color:#202a35}
table{border-collapse:collapse;width:100%;margin:20px 0}td,th{padding:9px 12px;border-bottom:1px solid #dce1e6;text-align:left;vertical-align:top}th{background:#eef3f7}code{white-space:nowrap}h2{margin-top:38px}a{color:#1466a2}</style>
<h1>extraDistr and actuar: MLE coverage</h1>
<p>Audit date: 29 October 2026. Inventory comes from installed namespace exports;
coverage comes from distribution folders in this checkout, including recently added
families whose board tasks still say queued. This is an API and model-domain audit,
not a new numerical validation of the fitters or reference d/p/q/r functions.</p>
<p><b>Covered</b> means a fitter exists within its documented sample, fixed-design and
boundary policies. <b>Covered (endpoint held)</b>: the unrestricted likelihood is unbounded
for every sample, so the fitter requires the endpoint and its stop message says why.
<b>Equivalent</b> means a reparameterization or alias, not an exported wrapper for that
name. <b>Partial / restricted</b> is deliberately not counted as full coverage.</p>
<p>Inclusion decisions are recorded in the
<a href="../../roadmap/2026-10-29-density-scope.md">density scope decisions</a>
(full coverage plan, 29 October 2026). extraDistr's Tukey lambda exports only
<code>qtlambda</code>/<code>rtlambda</code>, so it is not a row below; it is covered by
<code>mltlambda</code> with the package's own d/p/q/r, lambda in [-1, 1].</p>''']
parts.append('<table><tr><th>Classification</th><th>extraDistr</th><th>actuar</th></tr>')
statuses = list(dict.fromkeys(row['status'] for row in rows))
for status in statuses:
    counts = [sum(row['package'] == p and row['status'] == status for row in rows)
              for p in ['extraDistr', 'actuar']]
    parts.append(f'<tr><td>{status}</td><td>{counts[0]}</td><td>{counts[1]}</td></tr>')
parts.append('''</table><h2>Remaining</h2>
<p>The vector-parameter tier (categorical, Poisson and normal mixtures) needs parameter
vectors whose length the user chooses. The partial rows keep documented restrictions:
fatigue's location is fixed at zero, the GEV and GPD shapes are bounded, and the
location-scale t has df at least one.</p>
<h2>Counting traps</h2>
<p>actuar's log-Gompertz is inverse Weibull; Pearson6 is its transformed-beta alias;
Poisson-inverse-Gaussian has two names. Its generalized Pareto is beta-prime, whereas
extraDistr's generalized Pareto is the threshold/EVT family. extraDistr's gamma-Poisson
is negative binomial. A phase-type observation is scalar, so matrix-valued parameters
do not make that law multivariate.</p>''')
for package in ['extraDistr', 'actuar']:
    selected = [row for row in rows if row['package'] == package]
    univariate = sum(not row['status'].startswith('Excluded') for row in selected)
    covered = sum(row['status'] in ['Covered', 'Covered (endpoint held)', 'Equivalent'] for row in selected)
    parts.append(f'<h2>{package} {selected[0]["version"]}</h2><p>{len(selected)} d-prefixed exports; '
                 f'{univariate} univariate density names; {covered} covered or equivalent. '
                 'Aliases remain separate rows.</p>')
    parts.append('<table><tr><th>Density</th><th>Status</th><th>Coverage / action</th></tr>')
    for row in selected:
        parts.append('<tr>' + ''.join(f'<td>{html.escape(row[k])}</td>'
                                    for k in ['density', 'status', 'detail']) + '</tr>')
    parts.append('</table>')
parts.append('''<h2>Evidence and reproduction</h2>
<p>Primary evidence: installed extraDistr and actuar help pages and function signatures;
this checkout's per-distribution README metadata and implementation files. Public
references: <a href="https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf">extraDistr manual</a>,
<a href="https://cran.r-project.org/web/packages/actuar/actuar.pdf">actuar manual</a>,
<a href="https://stat.ethz.ch/CRAN/web/packages/actuar/vignettes/distributions.pdf">actuar family inventory</a>.
The nonexistence arguments above are deductions from these densities.</p>
<p>Run <code>python3 notes/evaluations/2026-10-29-density-coverage.py</code> from the
repository. It regenerates this HTML and the adjacent CSV, and fails if a d-prefixed
export is unclassified or disappears. Model classifications require human review
when the implementation changes.</p></html>''')
HERE.joinpath('2026-10-29-density-coverage.html').write_text('\n'.join(parts))
for package in ['extraDistr', 'actuar']:
    print(package, dict(collections.Counter(row['status'] for row in rows
                                          if row['package'] == package)))
