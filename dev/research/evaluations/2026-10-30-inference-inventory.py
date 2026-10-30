"""Inventory derivative source and initial DQM classifications at a Git revision.

Run from any directory:
  python3 dev/research/evaluations/2026-10-30-inference-inventory.py

The CSV is a source inventory, not an inference implementation or a completed
98-family regularity proof. Classifications and conditions are explained in
dev/roadmap/2026-10-30-inference-and-dqm.md. Unreviewed is distinct from non-DQM.
Uses committed files so concurrent working-tree edits cannot mix revisions.
"""

import argparse
import csv
import io
from pathlib import Path
import re
import subprocess

import yaml


ROOT = Path(__file__).resolve().parents[3]
PREFIX = 'cppunivariate/include/cppunivariate/distributions/'
HISTORICAL = 'bf5a3ac^'
AUDITED = 'c61a4979c6169388dea97c6b821b3bd89e530dfd'
OUTPUT = Path(__file__).with_suffix('.csv')


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
    rows = []
    for path, body in sorted(current.items()):
        if not path.endswith('/README.md'):
            continue
        meta = yaml.safe_load(body.split('---', 2)[1])
        family = meta['id']
        status, conditions, rank, section = classification(family)
        rows.append(dict(
            id=family, name=meta['name'], source_commit=revision,
            parameters=';'.join(str(p['name']) for p in meta['params']),
            parameter_kinds=';'.join(str(p['name']) + ':' + p.get('kind', 'regular')
                                     for p in meta['params']),
            support_type=meta['support']['type'], dqm_status=status,
            dqm_conditions=conditions, information_rank=rank,
            current_named_hessian_sources=named_methods(current, family, 'hessian') or 'none_found',
            current_named_gradient_sources=named_methods(current, family, 'gradient') or 'none_found',
            historical_hessian_commit=historical_revision,
            historical_named_hessian_sources=named_methods(historical, family, 'hessian') or 'none_found',
            derivative_semantics='not_validated_for_inference',
            uniform_observation_score_api='missing', uniform_opg_api='missing',
            public_vcov_sandwich_api='missing',
            r_intervals='generic_parametric_percentile_bootstrap; validity_not_audited',
            classification_section=section))
    assert len({row['id'] for row in rows}) == len(rows)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator='\n')
        writer.writeheader()
        writer.writerows(rows)
    for key in ('current_named_hessian_sources', 'current_named_gradient_sources',
                'historical_named_hessian_sources'):
        print(key, sum(row[key] != 'none_found' for row in rows))
    print('families', len(rows), 'not_reviewed',
          sum(row['dqm_status'] == 'not_reviewed' for row in rows))
    print('source_commit', revision)
    print('output', args.output)


if __name__ == '__main__':
    main()
