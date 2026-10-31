"""Inventory derivative source and initial DQM classifications at a Git revision.

Run from any directory:
  python3 dev/research/evaluations/2026-10-30-inference-inventory.py

The CSV is a source inventory, not an inference implementation or a completed
98-family regularity proof. Classifications and conditions are explained in
dev/roadmap/2026-10-30-inference-and-dqm.md. Unreviewed is distinct from non-DQM.
Uses committed files so concurrent working-tree edits cannot mix revisions.
"""

import argparse
import copy
import csv
import io
import json
from pathlib import Path
import re
import subprocess

import yaml


ROOT = Path(__file__).resolve().parents[3]
PREFIX = 'cppunivariate/include/cppunivariate/distributions/'
HISTORICAL = 'bf5a3ac^'
AUDITED = 'c61a4979c6169388dea97c6b821b3bd89e530dfd'
OUTPUT = Path(__file__).with_suffix('.csv')

# Each entry specifies the actual model regime; no global rank assertion is
# inferred from smoothness. Arguments and excluded regimes are in the report.
REMAINING = {
 'bbinom': ('argued', 'size N known integer >=2; alpha,beta>0', 'open: prove score independence for N>=2; N=1 rank one; N=0 rank zero', 'finite'),
 'betapr': ('argued', 'shape1,shape2,scale>0', 'open: establish joint shape/scale score independence', 'positive'),
 'bhatt': ('argued', 'sigma>0, a>0, mu real', 'open: joint rank; a=0 is normal boundary and a score vanishes', 'individual'),
 'bnbinom': ('argued', 'size,alpha,beta>0', 'open: joint rank and parameter coincidences', 'count'),
 'burr': ('argued', 'shape1,shape2,scale>0', 'open: joint shape/scale score independence', 'positive'),
 'dgamma': ('argued', 'shape,rate>0; fixed unit bins', 'open: joint rank of bin-probability scores', 'count'),
 'dnorm': ('argued', 'mean real, sd>0; fixed unit bins on all integers', 'open: joint rank of bin-probability scores', 'count'),
 'dweibull': ('argued', '0<shape1<1, shape2>0', 'open: global rank of two score sequences', 'count'),
 'emg': ('argued', 'mu real, sigma,lambda>0', 'open: rank of joint location/scale/rate scores', 'individual'),
 'fatigue': ('argued', 'alpha,beta>0; mu real; all free', 'open: joint score independence; mu fixed submodel included', 'flat_endpoint'),
 'fpareto': ('conditional', 'shape1,shape2,shape3,scale>0; min known or free', 'open: rank; free min DQM if shape2*shape3>2, not DQM if <=2', 'endpoint'),
 'frechet': ('argued', 'lambda,sigma>0; mu real, any subset', 'open: prove joint rank', 'flat_endpoint'),
 'gb2': ('argued', 'shape1,shape2,shape3,scale>0', 'open: joint rank and exceptional parameter surfaces', 'positive'),
 'genbeta': ('conditional', 'positive shapes and scale; scale held, or scale free with shape2>2', 'open: rank; free scale non-DQM if shape2<=2', 'endpoint'),
 'gompertz': ('argued', 'a,b>0', 'open: rank; compare a and b scores', 'positive'),
 'gumbel': ('proved', 'mu real, sigma>0; either or both free', 'nonsingular: independent location and scale scores', 'locscale'),
 'huber': ('argued', 'mu real, sigma,epsilon>0; any subset', 'open: joint rank and pointwise Hessian treatment at knots', 'individual'),
 'invburr': ('argued', 'shape1,shape2,scale>0', 'open: joint rank, inherited from Burr by reciprocal', 'positive'),
 'invgamma': ('proved', 'alpha,beta>0; either or both free', 'nonsingular: reciprocal gamma', 'transform'),
 'invgauss': ('proved', 'mean,shape>0; either or both free', 'nonsingular: sufficient statistics x and 1/x affinely independent', 'positive'),
 'invparalogis': ('argued', 'shape,scale>0', 'open: prove joint rank', 'positive'),
 'invpareto': ('argued', 'shape,scale>0', 'open: prove joint rank', 'positive'),
 'invtrgamma': ('argued', 'shape1,shape2,scale>0', 'open: joint rank; inherited from transformed gamma by reciprocal', 'positive'),
 'invweibull': ('proved', 'shape,scale>0', 'nonsingular: log reciprocal is Gumbel with invertible parameter map', 'transform'),
 'kumar': ('argued', 'a,b>0; fixed endpoints 0,1', 'open: prove joint score independence', 'positive'),
 'lgser': ('proved', '0<theta<1', 'positive: variance of X / theta^2', 'count_exp'),
 'llogis': ('proved', 'shape,scale>0', 'nonsingular: log X logistic; invertible transformation', 'transform'),
 'logis': ('proved', 'location real, scale>0', 'nonsingular: odd location score and nonzero even scale score', 'locscale'),
 'lomax': ('argued', 'lambda,kappa>0; support starts at fixed zero', 'open: prove score independence; no threshold coordinate in this parametrization', 'positive'),
 'lst': ('argued', 'mu real, sigma,df>0; df known or fitted', 'location/scale rank proved by odd/even scores; joint df rank open', 'student'),
 'naka': ('proved', 'shape,scale>0', 'nonsingular: X squared gamma and parameter map invertible', 'transform'),
 'nbinom': ('argued', 'size>0, 0<prob<1; size is continuous', 'open: proof of joint rank; size=0 or prob=1 degenerate boundaries', 'count'),
 'nsbeta': ('conditional', 'positive shapes; min<max; endpoints known or free', 'open: rank; free min requires shape1>2, free max requires shape2>2; <=2 non-DQM', 'endpoint'),
 'paralogis': ('argued', 'shape,scale>0', 'open: prove joint rank', 'positive'),
 'pareto3': ('conditional', 'shape,scale>0; min known or free', 'open: rank; free min DQM if shape>2, non-DQM if shape<2 except shape=1; shape=2 non-DQM; shape=1 positive boundary non-DQM', 'endpoint'),
 'pareto4': ('conditional', 'shape1,shape2,scale>0; min known or free', 'open: rank; free min DQM if shape2>2, non-DQM if shape2<=2', 'endpoint'),
 'pig': ('open', 'mean>0, shape>0; lambda mixture representation; shape=0 boundary', 'open: prove score tail domination and joint rank', 'individual'),
 'sged': ('conditional', 'sd,xi,nu>0; cusp moves with free coordinates', 'open: rank; cusp-moving directions require nu>1/2; other held subsets need tangent check', 'cusp'),
 'sgomp': ('argued', 'b>0, eta>0', 'open: joint rank; eta=0 one-sided exponential boundary', 'positive'),
 'skellam': ('argued', 'mu1,mu2>0', 'open: joint rank; zero-rate boundaries change support', 'count'),
 'slash': ('argued', 'mu real, sigma>0', 'nonsingular: odd location and nonzero even scale scores', 'locscale'),
 'snorm': ('argued', 'mean real, sd,xi>0; two-piece normal', 'open: joint rank including xi=1; do not conflate with Azzalini skew-normal', 'cusp'),
 'sstd': ('argued', 'mean real, sd,xi>0, nu>2', 'open: full rank and symmetric xi=1 case', 'cusp'),
 'std': ('argued', 'mean real, sd>0, nu>2', 'location/scale rank by odd/even scores; joint nu rank open', 'student'),
 'tbinom': ('proved', 'size N known, a,b fixed, 0<prob<1; retained integer set has >=2 points', 'positive for prob; singleton rank zero; unknown size discrete; a,b floor changes', 'trunc_count'),
 'tlambda': ('open', 'sigma>0, mu and lambda real; endpoints depend on lambda for lambda>0', 'open: support-dependent DQM and rank, including lambda=0 and held subsets', 'individual'),
 'tnorm': ('proved', 'a<b fixed known, mean real, sd>0; positive truncation probability', 'nonsingular: x and x² independent on an interval; free finite bounds non-DQM', 'trunc_cont'),
 'tpois': ('proved', 'lambda>0; fixed a,b with >=2 retained nonnegative integers', 'positive for lambda; singleton rank zero; a,b floor changes', 'trunc_count'),
 'trgamma': ('argued', 'shape1,shape2,scale>0', 'open: joint rank and parameter coincidences', 'positive'),
 'weibull': ('proved', 'shape,scale>0', 'nonsingular: log X Gumbel with invertible parameter map', 'transform'),
 'yulesimon': ('argued', 'rho>0', 'positive: score strictly varies with count; log-count moments finite', 'count'),
 'zeta': ('proved', 's>1', 'positive: variance of log X', 'count_exp'),
 'zib': ('conditional', 'N known integer >=2; 0<prob,pi<1', 'open: rank for N>=2; N=1 rank one, N=0 rank zero', 'mixture'),
 'zinb': ('argued', 'size>0, 0<prob,pi<1', 'open: joint rank and exceptional surfaces; boundary pi=0 or 1 is one-sided', 'mixture'),
 'zip': ('argued', 'lambda>0, 0<pi<1', 'nonsingular: positive-count ratios identify lambda, zero identifies pi', 'mixture'),
 'zmbinom': ('conditional', 'N known >=2; 0<prob,p0<1', 'nonsingular: conditional positive count varies with prob; N=1 only p0 identifiable', 'modified'),
 'zmgeom': ('proved', '0<prob,p0<1', 'nonsingular: zero indicator and positive geometric score', 'modified'),
 'zmlogarithmic': ('proved', '0<prob,p0<1', 'nonsingular: zero indicator and positive logarithmic score', 'modified'),
 'zmnbinom': ('argued', 'size>0, 0<prob,p0<1', 'open: positive truncated negative-binomial joint rank', 'modified'),
 'zmpois': ('proved', 'lambda>0, 0<p0<1', 'nonsingular: zero indicator and positive truncated Poisson score', 'modified'),
 'ztbinom': ('proved', 'N known >=2, 0<prob<1', 'positive: conditional X varies; N=1 point mass', 'trunc_count'),
 'ztgeom': ('proved', '0<prob<1', 'positive: shifted geometric', 'trunc_count'),
 'ztnbinom': ('argued', 'size>0, 0<prob<1', 'open: joint size/prob rank', 'trunc_count'),
 'ztpois': ('proved', 'lambda>0', 'positive: variance of positive count / lambda^2', 'trunc_count'),
}

# Extra rows make boundary and fixed-design exceptions machine-visible.
EXCEPTIONS = {
 'cmb': [('size=1', 'proved', 'size=1 known; 0<prob<1; nu finite', 'rank one for (prob,nu): nu score zero'), ('size=0', 'proved', 'size=0 known; any admissible prob,nu', 'rank zero: point mass')],
 'cmp': [('lambda=0', 'open', 'lambda=0; nu>=0; boundary', 'nu unidentified; one-sided DQM in lambda remains open'), ('nu=0', 'open', 'nu=0, 0<lambda<1; boundary', 'one-sided DQM and rank require boundary analysis')],
 'foldnorm': [('mu=0', 'proved', 'mu=0, sigma>0; one-sided mu domain', 'mu score zero; sigma information positive')],
 'bbinom': [('size<=1', 'proved', 'size=1: alpha,beta>0; size=0 also allowed', 'size=1 rank one; size=0 rank zero')],
 'zib': [('size<=1', 'proved', 'size=1; 0<prob,pi<1; size=0 also allowed', 'size=1 rank one; size=0 rank zero')],
 'zmbinom': [('size=1', 'proved', 'size=1; 0<prob,p0<1', 'prob unidentified; p0 information positive')],
 'ztbinom': [('size=1', 'proved', 'size=1; 0<prob<1', 'rank zero: retained count is always one')],
 'tbinom': [('singleton', 'proved', 'known size and bounds retain exactly one integer', 'rank zero for prob')],
 'tpois': [('singleton', 'proved', 'known bounds retain exactly one nonnegative integer', 'rank zero for lambda')],
 'nsbeta': [('free_min', 'conditional', 'min free, max held, positive shapes', 'DQM if shape1>2; non-DQM if shape1<=2; rank open'), ('free_max', 'conditional', 'max free, min held, positive shapes', 'DQM if shape2>2; non-DQM if shape2<=2; rank open')],
 'genbeta': [('free_scale', 'conditional', 'scale free; positive shapes', 'DQM if shape2>2; non-DQM if shape2<=2; rank open')],
 'pareto3': [('free_min', 'conditional', 'min free, positive shape and scale', 'DQM if shape>2; non-DQM if shape<=2; rank open')],
 'pareto4': [('free_min', 'conditional', 'min free, positive shapes and scale', 'DQM if shape2>2; non-DQM if shape2<=2; rank open')],
 'fpareto': [('free_min', 'conditional', 'min free, positive shapes and scale', 'DQM if shape2*shape3>2; non-DQM otherwise; rank open')],
 'bhatt': [('a=0', 'proved', 'a=0, sigma>0; one-sided a domain', 'a score zero; mu,sigma block is normal information')],
 'zip': [('pi_boundary', 'open', 'pi=0 or pi=1; lambda>0', 'one-sided null at pi=0; lambda unidentified at pi=1')],
 'zinb': [('pi_boundary', 'open', 'pi=0 or pi=1; size>0, 0<prob<1', 'one-sided null at pi=0; base coordinates unidentified at pi=1')],
}


def historical_costs(contents, family):
    """Describe named methods by actual enclosing struct and declared dimension."""
    found = []
    for path, body in sorted(contents.items()):
        if not path.startswith(PREFIX + family + '/') or not path.endswith('.hpp'):
            continue
        for match in re.finditer(r'\bstruct\s+(\w+)\s*\{', body):
            start, depth, end = match.end(), 1, match.end()
            while end < len(body) and depth:
                depth += (body[end] == '{') - (body[end] == '}')
                end += 1
            block = body[start:end]
            methods = [m for m in ('gradient', 'hessian') if re.search(r'\bvoid\s+' + m + r'\s*\(', block)]
            if not methods:
                continue
            dim = re.search(r'\bint\s+dim\s*\(\s*\)\s*const\s*\{\s*return\s+(\d+)\s*;', block)
            found.append(match.group(1) + '(dim=' + (dim.group(1) if dim else 'unknown') + ';' + '+'.join(methods) + ')')
    return '; '.join(found) if found else 'no_named_method_found'


def git(*args):
    return subprocess.check_output(['git', '-C', str(ROOT), *args], text=True)


def snapshot(revision):
    commit = git('rev-parse', revision).strip()
    files = git('ls-tree', '-r', '--name-only', commit, PREFIX).splitlines()
    # One process supplies all contents, rather than a process per header.
    objects = [f'{commit}:{name}' for name in files
               if name.endswith('.hpp') or name.endswith('/README.md')]
    proc = subprocess.run(
        ['git', '-C', str(ROOT), 'cat-file', '--batch'],
        input=('\n'.join(objects) + '\n').encode(), capture_output=True, check=True)
    stream = io.BytesIO(proc.stdout)
    contents = {}
    for obj in objects:
        _, kind, size = stream.readline().split()
        assert kind == b'blob', obj
        contents[obj.split(':', 1)[1]] = stream.read(int(size)).decode()
        assert stream.read(1) == b'\n'
    return commit, contents


def named_methods(contents, family, method):
    pattern = re.compile(r'\bvoid\s+' + method + r'\s*\(')
    return ';'.join(path for path, body in sorted(contents.items())
                    if path.startswith(PREFIX + family + '/')
                    and path.endswith('.hpp') and pattern.search(body))


def classification(family):
    if family in REMAINING:
        grade, conditions, rank, group = REMAINING[family]
        return (grade, conditions, rank, 'inference_1:' + group)
    if family == 'cmp':
        return ('proved_interior', 'lambda>0, nu>0; both free or any held subset; boundary lambda=0 or nu=0 excluded', 'nonsingular; covariance of k and log(k!)', 'inference_1_report')
    if family == 'cmb':
        return ('proved_conditional', 'size known integer >=2, 0<prob<1, finite nu; both free or any held subset; size=1 and size=0 exceptional', 'nonsingular for size>=2; dispersion unidentified for size<=1', 'inference_1_report')
    if family == 'foldnorm':
        return ('proved_conditional', 'sigma>0; mu>0: both free; mu=0: one-sided boundary with zero mu score', 'nonsingular for mu>0; mu score zero at mu=0; scale information positive', 'inference_1_report')
    if family == 'invexp':
        return ('proved_interior', 'scale>0; reciprocal transform of exponential', 'nonsingular for scale', 'inference_1_report')
    if family == 'cauchy':
        return ('proved_interior', 'location real; scale>0; either or both free', 'nonsingular; diagonal information 1/(2*scale^2)', 'inference_1_report')
    standard = {'norm', 'exp', 'gamma', 'beta', 'pois', 'geom', 'rayleigh',
                'lnorm', 'logitnorm', 'lgamma'}
    if family in standard:
        return ('yes_direct_argument', 'ordinary interior continuous parameters',
                'nonsingular_in_stated_regime', 'standard_models')
    if family == 'binom':
        return ('conditional_direct_argument', 'size known integer >=1; 0<prob<1',
                'nonsingular_for_prob', 'standard_models')
    if family == 'laplace':
        return ('yes_direct_argument', 'sigma>0; location unrestricted',
                'nonsingular; pointwise location Hessian is unsuitable', 'laplace')
    if family == 'gev':
        return ('conditional_literature', 'full model DQM iff xi>-1/2',
                'nonsingular_in_regular_regime', 'moving_support')
    if family == 'gpd':
        return ('conditional_literature', 'threshold mu known; sigma>0; xi>-1/2',
                'review_information_implementation', 'moving_support')
    if family in {'hnorm', 'hcauchy', 'ht', 'pareto', 'pareto2', 'power', 'unif'}:
        cases = {
            'hnorm': 'free mu fails DQM; known mu gives regular scale submodel',
            'hcauchy': 'free mu fails DQM; known mu gives regular scale submodel',
            'ht': 'free mu fails DQM; known mu: review scale/df submodel',
            'pareto': 'free b fails DQM; known b gives regular shape submodel',
            'pareto2': 'free min fails DQM; known min: review shape/scale submodel',
            'power': 'free alpha fails DQM; known alpha gives regular beta submodel',
            'unif': 'any free endpoint fails DQM; exact spacing procedures possible',
        }
        return ('not_dqm_with_free_endpoint', cases[family],
                'joint_regular_covariance_not_applicable', 'moving_support')
    if family == 'triang':
        return ('conditional_direct_argument',
                'free endpoint fails DQM; known a,b and a<c<b gives DQM for c',
                'I_cc=1/((c-a)*(b-c)); pointwise Hessian unsuitable', 'triangular')
    if family == 'ged':
        return ('conditional_direct_argument',
                'free location requires nu>1/2; held location permits nu>0; inspect fitter bounds',
                'not_fully_audited', 'cusps_and_singularities')
    if family == 'sn':
        return ('yes_direct_argument', 'finite alpha; omega>0; xi unrestricted',
                'singular_at_alpha=0_with_location_free', 'cusps_and_singularities')
    if family == 'dlaplace':
        return ('conditional_direct_argument',
                'integer location has no Euclidean DQM coordinate; scale regular with location known',
                'not_fully_audited', 'discrete_coordinates')
    if family in {'dunif', 'hyper', 'nhyper'}:
        return ('not_euclidean_full_model', 'unknown count/endpoint coordinates are integer',
                'continuous_joint_information_not_applicable', 'discrete_coordinates')
    if family == 'zipf':
        return ('conditional_direct_argument', 'N known integer >=2; s interior',
                'nonsingular_for_s; unknown N is discrete', 'discrete_coordinates')
    return ('not_reviewed', 'classify free subset and parameter regime before inference',
            'not_reviewed', 'remaining_families')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--revision', default=AUDITED)
    parser.add_argument('--output', type=Path, default=OUTPUT)
    args = parser.parse_args()
    if git('rev-parse', args.revision).strip() != AUDITED:
        parser.error('Manual capability and mathematical findings are tied to '
                     + AUDITED + '; review and update them before auditing another revision.')
    revision, current = snapshot(args.revision)
    historical_revision, historical = snapshot(HISTORICAL)
    old_paths = set(git('ls-tree', '-r', '--name-only', historical_revision).splitlines())
    rows = []
    for path, body in sorted(current.items()):
        if not path.endswith('/README.md'):
            continue
        meta = yaml.safe_load(body.split('---', 2)[1])
        family = meta['id']
        status, conditions, rank, section = classification(family)
        historical_gradient = named_methods(historical, family, 'gradient') or 'none_found'
        report_path = f'quality/derived/{family}/hessian_report.json'
        if report_path in old_paths:
            report = json.loads(git('show', f'{historical_revision}:{report_path}'))
            validation = ('historical_symbolic_grid_passed' if report.get('passed') else
                          'historical_symbolic_grid_not_passed') + '; n=' + str(report.get('summary', {}).get('n', 'unknown'))
        else:
            validation = 'no_historical_hessian_report_found'
        rows.append(dict(
            id=family, regime='primary', name=meta['name'], source_commit=revision,
            parameters=';'.join(str(p['name']) for p in meta['params']),
            parameter_kinds=';'.join(str(p['name']) + ':' + p.get('kind', 'regular')
                                     for p in meta['params']),
            support_type=meta['support']['type'], dqm_status=status,
            dqm_conditions=conditions, information_rank=rank,
            finite_information=('finite in stated DQM regime; exceptional boundaries excluded' if status in ('proved', 'proved_interior', 'proved_conditional', 'argued') else 'open or regime-dependent; see DQM conditions and rank'),
            likelihood_differentiability=('open: verify pointwise derivatives and boundary terms in each free subset' if section.startswith('inference_1:') else 'see linked classification section'),
            estimator_asymptotics='open: consistency, existence, optimizer accuracy and limit law not established for this fitter and regime',
            current_named_hessian_sources=named_methods(current, family, 'hessian') or 'none_found',
            current_named_gradient_sources=named_methods(current, family, 'gradient') or 'none_found',
            historical_hessian_commit=historical_revision,
            historical_named_hessian_sources=named_methods(historical, family, 'hessian') or 'none_found',
            historical_named_gradient_sources=historical_gradient,
            historical_hessian_validation=validation,
            derivative_semantics=historical_costs(historical, family) + '; optimizer objective, coordinates and normalization require individual audit; not validated for inference',
            uniform_observation_score_api='missing', uniform_opg_api='missing',
            public_vcov_sandwich_api='missing',
            r_intervals='generic_parametric_percentile_bootstrap; validity_not_audited',
            classification_section=section))
    assert len({row['id'] for row in rows}) == len(rows)
    assert all(row['dqm_status'] != 'not_reviewed' for row in rows)
    extras = []
    for row in rows:
        for regime, grade, conditions, rank in EXCEPTIONS.get(row['id'], []):
            extra = copy.copy(row)
            extra.update(regime=regime, dqm_status=grade, dqm_conditions=conditions,
                         information_rank=rank, classification_section='inference_1:exception',
                         finite_information='regime-dependent; see rank and DQM conditions',
                         likelihood_differentiability='open: verify boundary or discrete-design derivative meaning')
            extras.append(extra)
    rows.extend(extras)
    rows.sort(key=lambda row: (row['id'], row['regime'] != 'primary', row['regime']))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator='\n')
        writer.writeheader()
        writer.writerows(rows)
    for key in ('current_named_hessian_sources', 'current_named_gradient_sources',
                'historical_named_hessian_sources'):
        print(key, sum(row[key] != 'none_found' for row in rows if row['regime'] == 'primary'))
    print('rows', len(rows), 'families', len({row['id'] for row in rows}), 'not_reviewed',
          sum(row['dqm_status'] == 'not_reviewed' for row in rows))
    print('source_commit', revision)
    print('output', args.output)


if __name__ == '__main__':
    main()
