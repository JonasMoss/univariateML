"""Join every requested gap to its route, attempt history and final table state."""
import json,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
HERE=Path(__file__).resolve().parent
DISTS=ROOT/'univariateML-cpp/include/cppunivariate/distributions'
METHODS={}

def group(names,cause,remedy):
    for name in names.split():
        assert name not in METHODS
        METHODS[name]=dict(mechanism=cause,remedy=remedy)

group('beta betapr fpareto gb2','Slow hypergeometric evaluation at simultaneous large beta shapes; concentrated normalization.',
      'Euler beta continued fraction, centered logit normalization, contracting bracketed inversion.')
group('binom zib zmbinom ztbinom cmb','Finite-support enumeration grows with the very large design size.',
      'Incomplete-beta binomial CDF and exact binomial normalization; cmb uses its nu=1 submodel.')
group('bnbinom','The near-Poisson beta-mixture remainder did not close.',
      'Positive PMF ratio, factorial-moment Markov bound, and bounded far-count gamma ratios.')
group('burr invburr paralogis invparalogis lomax pareto2 pareto4 invpareto',
      'Physical-coordinate quadrature does not resolve the transformed mass or enormous support range.',
      'Exponential cumulative-hazard coordinate, exact inverse, and explicit omitted exponential mass.')
group('gamma invgamma trgamma invtrgamma naka',
      'Concentrated or endpoint gamma mass, slow small-argument upper-gamma evaluation, and stalled inverse brackets.',
      'Scaled latent gamma coordinate, centered gamma tails, bounded small/large argument expansions, contracting inversion.')
group('ged sged','Extreme powers and gamma arguments defeat direct evaluation; concentration and inverse bracket stagnation.',
      'Explicit mp log density, exact gamma normalization, scaled physical chart, bounded gamma tails and safeguarded inversion.')
group('ht lst std sstd','Normalization concentrates on a small location-scale range; large-degree beta evaluation is slow.',
      'Complete Student beta normalization, stable incomplete-beta tails and density-width verification.')
group('geom zmgeom ztgeom nbinom zinb zmnbinom ztnbinom pois zip zmpois ztpois',
      'Finite normalization truncation or a slow special-function tail at a very large or diffuse count scale.',
      'Exact generating-series normalization; stable geometric, beta or gamma tails and conditional/mixture identities.')
group('dnorm dlaplace','Finite normalization truncation cannot cover the extreme spread.',
      'Exact telescoping normal bins or geometric-series normalization, with independent PMF differences.')
group('lgser zmlogarithmic','Slow normalization and repeated survival quadrature near p=1.',
      'Exact logarithmic-series normalization; positive finite prefix and enveloping Stieltjes tail expansion.')
group('pareto power','Endpoint concentration defeats physical-coordinate normalization.',
      'Exact power-law inverse and exponential-coordinate normalization.')
group('frechet invweibull','The listed gap is already referenced at the baseline commit.',
      'Retain the baseline TASK-187 reference and all numerical rows; no new point counted.')
group('cmp','A slow diffuse normalizing series or a large Poisson submodel exhausts the reference budget.',
      'Exact nu=1 Poisson submodel; unit-rate Euler--Maclaurin tail with integrated Cauchy remainder bound.')
group('dgamma','Concentration defeats finite normalization and interval subtraction.',
      'Telescoping normalization and centered positive gamma-density integration over each bin.')
group('dweibull','Exponentiating astronomically negative log masses exhausts time/memory.',
      'Log-domain PMF-difference verification and negligible natural-probability guard in jump checks.')
group('lgamma','Endpoint or large-shape gamma evaluation and inverse support concentration.',
      'Explicit log density and complete-gamma normalization; bounded gamma tails and contracting inverse brackets.')
group('nhyper','Position-support enumeration is enormous although the marked population is small.',
      'Finite hypergeometric count over the m marked items, using m+1 positive terms.')
group('pig','Large-order normalization and a thin count-tail integration layer exceed the reference budget.',
      'Exact Poisson-mixture normalization, scaled positive tail integral, Taylor/tail bounds, checked Gauss quadrature.')
group('skellam','Large-rate positive convolution repeats concentrated gamma evaluations; far-count log differences lose increments.',
      'Poisson adjacent-cut recurrence, positive convolution bounds and a sharper descending Bessel-mass majorant.')
group('sn','Gaussian submodel inversion and normalization checks lose the required extreme precision.',
      'Exact shape-zero/plus-or-minus-one normal identities; stable squared-normal tails and high-precision inverse.')
group('st','A thin skew-t angular layer and large-degree beta evaluation exhaust the reference budget.',
      'Endpoint-scaled angular wedge, concavity remainder, Student reflection normalization and stable beta tails.')
group('tlambda','The finite-difference verification step is too large for sigma=1e-8.',
      'Scale the existing implicit-density derivative check by the local density width.')
group('tnorm','Far truncation creates a boundary layer and unstable subtraction of normalizing log probabilities.',
      'Stable log-normal interval density and boundary-scaled normalizing integral.')
group('yulesimon','Polynomial survival with tiny rho makes truncated normalization impractical.',
      'Exact telescoping normalization, retaining independent mass-difference checks.')
group('gengamma','Concurrent family ownership excludes changes in this job.',
      'Explicitly untouched; retain all four requested gaps and their original diagnostics.')

def main():
    manifest=json.loads((ROOT/'dev/research/evaluations/2026-11-11-dpqr-extreme-data/oracle-gaps.json').read_text())
    assert set(METHODS)=={r['family'] for r in manifest}
    out=[]
    for entry in manifest:
        family=entry['family'];params=entry['gap']['parameters'];path=DISTS/family/'truth.json'
        table=json.loads(path.read_text())
        old=json.loads(subprocess.check_output(['git','show','7825e912:'+str(path.relative_to(ROOT))],cwd=ROOT,text=True))
        before=old['parameters']+[p['parameters'] for p in old.get('excluded_parameter_points',[])]
        active=params in table['parameters']
        excluded=params in [p['parameters'] for p in table.get('excluded_parameter_points',[])]
        attempts=[]
        for stage in ('generation-pass1','generation-pass2','generation-pass3','generation-pass4','generation'):
            receipt=HERE/stage/(family+'.json')
            if not receipt.exists():continue
            data=json.loads(receipt.read_text())
            for r in data.get('records',[]):
                if r['parameters']==params:
                    attempt=dict(r)
                    attempt.update(stage=stage,digits=data.get('digits'),timeout=data.get('timeout'),
                                   memory_bytes=data.get('memory_bytes'))
                    attempts.append(attempt)
        out.append(dict(family=family,parameters=params,original_diagnostic=entry['gap']['reason'],
                        **METHODS[family],already_present_at_baseline=params in before,
                        final_reference_status='active' if active else 'archived' if excluded else 'open',
                        attempts=attempts))
    (HERE/'diagnoses.json').write_text(json.dumps(out,indent=2,allow_nan=False)+'\n')
    print('Per-point diagnoses and attempt histories:',len(out))

if __name__=='__main__':main()
