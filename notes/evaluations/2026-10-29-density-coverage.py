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
    'dbnbinom': 'mlbnbinom; see its identification and boundary policy.',
    'ddunif': 'mldunif', 'ddweibull': 'mldweibull',
    'dgompertz': 'mlgompertz', 'dgumbel': 'mlgumbel',
    'dhuber': 'mlhuber, including estimated epsilon.',
    'dinvgamma': 'mlinvgamma', 'dkumar': 'mlkumar',
    'dlaplace': 'mllaplace', 'dlgser': 'mllgser', 'dlomax': 'mllomax',
    'dpareto': 'mlpareto (type I)', 'dpower': 'mlpower',
    'drayleigh': 'mlrayleigh', 'dsgomp': 'mlsgomp; folder exists despite queued board wording.',
    'dtnorm': 'mltnorm with known truncation bounds.',
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
    'dfrechet': 'mlinvweibull for known mu after subtracting mu. mlgev covers xi=1/lambda only up to 1; not the unrestricted free-location family.',
    'dgev': 'mlgev restricts xi to [-0.5,1]; unrestricted xi<-1 admits endpoint blow-up.',
    'dgpd': 'mlgpd uses a fixed threshold and xi>=-0.5.',
    'dhnorm': 'mlhnorm estimates a translation; extraDistr fixes the endpoint at zero. Fixing it changes the scale estimate.',
    'dlst': 'mllst restricts df>=1; unrestricted df>0 has scale-collapse paths.',
    'dnsbeta': 'Known min/max: transform data and use mlbeta, with Jacobian. Free bounds: unbounded likelihood when a shape is below 1.',
    'dtbinom': 'Only zero truncation via mlztbinom; arbitrary known a/b not implemented.',
    'dtpois': 'Only zero truncation via mlztpois; arbitrary known a/b not implemented.',
})
add('extraDistr', 'Missing', {
    'dbhatt': 'Normal-uniform convolution; compare normal and uniform limits before adding.',
    'dcat': 'Categorical probabilities have empirical-frequency MLEs; no dedicated fitter.',
    'ddgamma': 'Discretized gamma; continuous mlgamma is not a substitute.',
    'ddlaplace': 'Discrete Laplace on integers; continuous mllaplace is not a substitute.',
    'ddnorm': 'Discretized normal; continuous mlnorm is not a substitute.',
    'dhcauchy': 'Half-Cauchy with zero endpoint; scale-only fit is missing.',
    'dht': 'Half-t with zero endpoint; scale/df fit is missing.',
    'dnhyper': 'Negative hypergeometric; needs a known-design policy.',
    'dskellam': 'Difference of two Poisson variables; useful new integer family.',
    'dslash': 'Location-scale slash; missing, needs likelihood and closure analysis.',
})
add('extraDistr', 'Mixture project', {
    'dmixnorm': 'Unrestricted normal mixtures with at least two components have unbounded likelihood via component scale collapse. Fixed scales are a different model.',
    'dmixpois': 'Poisson mixtures do not share the normal scale-collapse argument. Need component-count, identification and boundary policy.',
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
add('actuar', 'Equivalent', {
    'dlgompertz': 'Alias of dinvweibull; NOT the ordinary Gompertz family.',
    'dpearson6': 'Alias of dtrbeta in this package (all three shapes); mlgb2.',
    'dpoisinvgauss': 'Alias of dpig; mlpig.',
    'dgenpareto': 'mlbetapr: actuar shape1 is beta-prime shape2, actuar shape2 is beta-prime shape1. NOT extraDistr dgpd.',
})
add('actuar', 'Reject unrestricted MLE', {
    'dpareto2': 'Known min reduces to mllomax. Free min, shape and scale: likelihood diverges along min=m-epsilon, scale=epsilon, shape<k/(n-k), where k minima occur among n observations.',
    'dfpareto': 'Known min reduces to mlgb2; free min is unbounded by choosing shape2*shape3<1 and approaching the sample minimum.',
    'dgenbeta': 'Free scale (upper endpoint) is unbounded when shape2<1. Fixed scale is a useful separate model, currently missing.',
    'dpareto3': 'Known min reduces to mlllogis. Free min is unbounded by choosing shape<1.',
    'dpareto4': 'Known min reduces to mlburr. Free min is unbounded by choosing shape2<1.',
})
add('actuar', 'Mixture project', {
    'dphtype': 'Univariate phase-type law despite vector/matrix parameters. Dimension and representation must be specified; currently missing.',
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
boundary policies. <b>Equivalent</b> means a reparameterization or alias, not an exported
wrapper for that name. <b>Partial / restricted</b> is deliberately not counted as full
coverage. Rejecting a model is a proposed policy here: no error-only fitters were added.</p>
<p>Inclusion decisions and accepted study candidates are recorded in the
<a href="../../roadmap/2026-10-29-density-scope.md">density scope decisions</a>.
Bhattacharjee and categorical are deliberately excluded, along with multivariate laws.</p>''']
parts.append('<table><tr><th>Classification</th><th>extraDistr</th><th>actuar</th></tr>')
statuses = list(dict.fromkeys(row['status'] for row in rows))
for status in statuses:
    counts = [sum(row['package'] == p and row['status'] == status for row in rows)
              for p in ['extraDistr', 'actuar']]
    parts.append(f'<tr><td>{status}</td><td>{counts[0]}</td><td>{counts[1]}</td></tr>')
parts.append('''</table><h2>What is worth adding?</h2>
<p>The most natural next projects are Skellam, discrete Laplace, discrete normal,
discrete gamma, zero-endpoint half-normal and the half-Cauchy/half-t pair. These are
missing fitter candidates, not claims that every sample has an attained MLE.
Arbitrary fixed truncation of Poisson/binomial is also a useful extension.
Categorical is excluded because its unrestricted probability vector has a dimension
determined by the category set; Bhattacharjee is excluded as too specialized.
Negative hypergeometric needs design decisions. Slash needs its own boundary analysis.
Poisson mixtures and phase-type fitting are larger projects.</p>
<h2>Where an explicit error makes sense</h2>
<p>For a freely translated Weibull, fix any shape k between 0 and 1 and any positive
scale. Let mu approach the sample minimum from below. The density at each minimum
is proportional to (min(x)-mu)<sup>k-1</sup> and diverges, while other densities
approach finite positive values. Thus the likelihood is unbounded. A known threshold
still gives the ordinary two-parameter Weibull after subtraction. Shifted Weibull
is not itself a density export in either audited package.</p>
<p>The same endpoint argument rejects freely bounded nonstandard beta (shape1&lt;1
at a lower endpoint or shape2&lt;1 at an upper endpoint), generalized beta with
free scale and shape2&lt;1, and freely translated Pareto III/IV and Feller–Pareto.
Keep all other parameters fixed along each path. These conclusions follow directly
from their documented density powers. They do not rule out fixed-endpoint or
shape-constrained models. The existing fatigue README gives its separate
free-location collapse argument. Normal mixtures have a component-collapse path.</p>
<p>Free-location Pareto II also has unbounded likelihood: with k minima among n
observations, set min=m-epsilon and scale=epsilon. The likelihood is proportional
to epsilon<sup>alpha(n-k)-k</sup>, which diverges for sufficiently small positive
alpha. This corrects the earlier classification as partial coverage.</p>
<p>Suggested diagnostic: “Unrestricted maximum likelihood is unbounded when the
endpoint is estimated. Supply a fixed endpoint or use a documented constrained model.”
Only use this after identifying the actual free-parameter model. Do not give this
error merely because an otherwise valid family's MLE fails for a particular sample.</p>
<h2>Counting traps</h2>
<p>actuar's log-Gompertz is inverse Weibull; Pearson6 is its transformed-beta alias;
Poisson-inverse-Gaussian has two names. Its generalized Pareto is beta-prime, whereas
extraDistr's generalized Pareto is the threshold/EVT family. extraDistr's gamma-Poisson
is negative binomial. A phase-type observation is scalar, so matrix-valued parameters
do not make that law multivariate. Fixed half-normal is not covered exactly by fitting
an additional translation. Full free-location Fréchet exceeds the present GEV domain.</p>''')
for package in ['extraDistr', 'actuar']:
    selected = [row for row in rows if row['package'] == package]
    univariate = sum(not row['status'].startswith('Excluded') for row in selected)
    covered = sum(row['status'] in ['Covered', 'Equivalent'] for row in selected)
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
