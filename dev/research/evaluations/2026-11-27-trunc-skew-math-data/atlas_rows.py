"""Write the TASK-458 proposals from the family dispatch below."""
import argparse
import csv
import re
from pathlib import Path


DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
REPORT = DATA.with_name(DATA.name.removesuffix('-data') + '-report.md')
ATLAS = ROOT / 'dev/research/existence-atlas'
FAMILIES = {
    'ged': ('mean', 'sd', 'nu'),
    'sged': ('mean', 'sd', 'nu', 'xi'),
    'sn': ('xi', 'omega', 'alpha'),
    'snorm': ('mean', 'sd', 'xi'),
    'sstd': ('mean', 'sd', 'nu', 'xi'),
    'st': ('xi', 'omega', 'nu', 'alpha'),
    'lst': ('mu', 'sigma', 'df'),
    'slash': ('mu', 'sigma'),
    'huber': ('mu', 'sigma', 'epsilon'),
    'ht': ('mu', 'sigma', 'nu'),
    'tlambda': ('mu', 'sigma', 'lambda'),
    'bhatt': ('mu', 'sigma', 'a'),
    'emg': ('mu', 'sigma', 'lambda'),
    'logitnorm': ('mu', 'sigma'),
    'dblpareto': ('scale', 'left_rate', 'right_rate'),
    'lgamma': ('shapelog', 'ratelog'),
}


def link(section):
    return f'[{section}](#{section.lower().replace(" ", "-")})'


def rule(family, held):
    center, scale, *shapes = FAMILIES[family]
    free = set(FAMILIES[family]) - held
    if not free:
        return 'fixed', '', 'A compatible held model with positive likelihood is the sole feasible model.'
    if family == 'logitnorm':
        detail = {
            center: 'Use the projected minimum and the uniform mean of the squared logit residual.',
            scale: 'The average logit must lie strictly between the transformed cuts.',
        }.get(next(iter(held), ''), 'For nonconstant logits compare their variance with that of the mean-matched linear tilt.')
        return 'direct', 'Log-scale reductions', detail
    if family == 'lgamma':
        detail = {
            center: 'Match the mean of log x between the lower cut and the zero-rate mean.',
            scale: 'Match the mean of log log x between the zero-shape and upper thresholds.',
        }.get(next(iter(held), ''), 'Exclude constants and test the transformed gamma shape edge, rate edge and corner by the inward moments.')
        return 'direct', 'Log-scale reductions', detail
    if family == 'dblpareto':
        return 'components', 'Double Pareto on the log scale', (
            'Use the held-kink rate box.' if center in held else
            'Compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus.')
    if family == 'huber':
        return 'threshold', 'Huber envelopes in physical coordinates', (
            'Use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt.')
    if family == 'sn' and shapes[0] in held:
        return 'threshold', 'Azzalini skew-normal', 'Use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds.'
    if family == 'sn' and free == {'alpha'}:
        return 'threshold', 'Azzalini skew-normal', 'Compare the two favored-half or wrong-side collapse ends of alpha.'
    if family == 'ged' and free <= {'sd'}:
        return 'direct', 'Fixed stretched-exponential shape', 'Strictly bracket the sample mean of the absolute residual to the held power.'
    if family in {'ged', 'sged', 'snorm'} and not free.intersection(shapes):
        return 'threshold', 'Fixed stretched-exponential shape', (
            'Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible.')
    if family in {'sstd', 'st', 'lst', 'ht', 'slash'} and not free.intersection(shapes):
        extra = ' Include effective uniforms, upper-cut contact and the sample-minimum branch.' if family == 'ht' else ''
        return 'threshold', 'Fixed power shape', (
            'Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first.' + extra)
    if family in {'sged', 'snorm', 'sstd'} and free == {'xi'}:
        return 'threshold', 'Skew-only two-piece envelopes', (
            'Compare both skew ends including a moving join, exterior power or collapse as applicable to the window.')
    if family in {'ged', 'sged', 'sstd'} and free == {'nu'}:
        return 'threshold', 'Shape-only GED and standardized Student', (
            'Compare the explicit scalar shape endpoints, retaining the lower endpoint when included.')
    if family in {'lst', 'ht'}:
        return 'threshold', 'Free degrees of freedom in LST and HT', (
            'Apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights.')
    if family == 'tlambda' and 'lambda' in held:
        return 'threshold', 'Tukey lambda at fixed shape', (
            'Negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers.')
    if family in {'bhatt', 'emg'} and len(free) == 1:
        return 'threshold', 'Convolution kernels', {
            center: 'Use the distant-location comparison at held Gaussian scale and held shape.',
            scale: 'Use the broad-scale uniform or diffusion and the appropriate sharp-scale face or cut collapse.',
            shapes[0]: 'Compare the width endpoints (zero is included).' if family == 'bhatt' else
            'Compare the normal at infinite rate and the zero-rate Gaussian CDF kernel where integrable.',
        }[next(iter(free))]
    frontier = {
        'ged': 'Uniform-edge layers with shape tending to infinity while location or scale drifts.',
        'sged': 'Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero.',
        'sn': 'Wrong-side conditioning with alpha unbounded while squared-scale tilt or the moving join changes.',
        'snorm': 'Moderate-side broad or remote tails coupled to the collapsing side and a moving kink.',
        'sstd': 'Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute.',
        'st': 'Contact at the moving skew join and uniform bounds when shape, scale or location co-escape.',
        'lst': 'Uniform-in-df remote and sharp limits, including small-df lost mass on rays when df 0 is accessible.',
        'ht': 'Uniform-in-df estimates at support contact and a drifting endpoint, including small-df normalization when accessible.',
        'tlambda': 'Coupled support-edge and shape motion near shape 0 and 1, and the large negative shape end where allowed.',
        'emg': 'A moving join at a cut or sample, and joint changes of the physical Gaussian and exponential tail parameters.',
        'bhatt': 'Joint Gaussian smearing and width or center motion, with normalized overlap tending to zero.',
    }[family]
    return 'open', 'Accessible free-shape faces', frontier


def cases(family, held, free):
    if family == 'slash' and held == {'sigma'}:
        yield 'nonunique', 'At fixed sigma 1, (-4,4) on [-100,100] has distinct reflected maximizers.'
    if family == 'lst' and held == {'df', 'sigma'}:
        yield 'nonunique', 'At df 3 and sigma 1/sqrt(3), (-4,4) on [-100,100] has distinct reflected maximizers.'
    if family == 'dblpareto' and held == {'left_rate', 'right_rate'}:
        yield 'nonunique', 'At rates 1 and 1, (1,1) on [1,e] has every kink scale in (0,1] maximizing.'
    if family == 'ht' and held == {'nu', 'sigma'}:
        yield 'left-ray-unique', 'On a left ray with sample minimum below the upper cut, the unique endpoint is the sample minimum.'
    if family in {'ged', 'sged', 'snorm', 'sn', 'huber', 'bhatt', 'emg'}:
        scale = FAMILIES[family][1]
        if scale in free or (family == 'huber' and 'epsilon' in free):
            scope = ' For GED and SGED this requires held shape at least 1 if held.' if family in {'ged', 'sged'} else ''
            yield 'endpoint-pair', 'On a bounded window [a,b], (a,b) has unattained uniform supremum.' + scope
    if family == 'sstd' and 'nu' in free:
        where = 'at the held mean' if 'mean' in held else 'at a sample value chosen as the mean'
        yield 'divergent-tie', f'More than two thirds of the data tied {where} give divergence along df down to 2 at fixed sd and skew.'
    if family == 'tlambda' and 'lambda' in free and 'mu' in held and 'sigma' in free:
        yield 'divergent-tie', 'A tie at the held center diverges at sufficiently negative fixed shape as scale shrinks.'
    if family == 'tlambda' and 'lambda' in free and 'mu' in free and 'sigma' in held:
        yield 'cut-collapse', 'A constant at a finite cut diverges by sliding the included shape-1 support against that cut.'
    if family == 'lgamma' and not held:
        yield 'corner', 'The sample (e,e^4) on [e,e^4] has an unattained log-gamma corner supremum.'
    if family == 'logitnorm' and not held:
        yield 'uniform-logit', 'The sample (1/4,3/4) on [1/4,3/4] escapes to a law uniform on the logit scale.'


DIRECT_FAILURE = {
    'logitnorm': 'The applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.',
    'lgamma': 'The applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.',
    'ged': 'Lower equality gives projection collapse. At or above the bounded upper threshold only the uniform wins.',
}
THRESHOLD_FAILURE = {
    'Fixed stretched-exponential shape': 'Broad uniform, allowed exponential tilt, or divergent projection or cut collapse.',
    'Free degrees of freedom in LST and HT': 'Divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.',
    'Fixed power shape': 'The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.',
    'Huber envelopes in physical coordinates': 'The hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.',
    'Azzalini skew-normal': 'The positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.',
    'Tukey lambda at fixed shape': 'The sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.',
    'Skew-only two-piece envelopes': 'Both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.',
    'Shape-only GED and standardized Student': 'Only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.',
    'Convolution kernels': 'Only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included.',
}


def generate():
    sections, rows = [], []
    with (ATLAS / 'modifier-atlas.csv').open(newline='') as stream:
        masks = [r for r in csv.DictReader(stream) if r['modifier'] == 'truncate:known']
    for family, coefficients in FAMILIES.items():
        for mask in (r['held'] for r in masks if r['family'] == family):
            held = set() if mask == 'all free' else set(mask.split('+'))
            free = set(coefficients) - held
            mode, section, detail = rule(family, held)
            title = f'{family} {mask} truncate:known'
            anchor = re.sub(r'[^\w -]', '', title).replace(' ', '-')
            evidence = f'{REPORT.relative_to(ROOT)}#{anchor}'
            sections.append(f'### {title}\n')
            if mode == 'open':
                sections.append(f'**Open.** {detail} The accessible faces give exact path heights. '
                                f'Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. {link(section)}\n')
            else:
                if mode == 'fixed':
                    criterion, failure, unique = detail, 'A compatible fixed model has no parameter escape.', 'The fit is unique.'
                elif mode == 'direct':
                    criterion, failure, unique = detail, DIRECT_FAILURE[family], 'Every attained fit is unique.'
                elif mode == 'components':
                    criterion = detail + ' Attainment holds iff a finite component winner reaches every component supremum.'
                    failure = 'If no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.'
                    unique = 'The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored.'
                else:
                    criterion = detail + ' A finite fit exists iff a finite feasible point reaches the finite excluded threshold.'
                    failure, unique = THRESHOLD_FAILURE[section], ''
                if mode == 'threshold':
                    sections.append('**Proved.** ' + criterion + ' ' + failure + ' '
                                    + 'General finite cardinality is open. ' + link(section) + '\n')
                else:
                    sections.append('**Proved.** ' + ' '.join((criterion, failure, unique)) +
                                    (' ' + link(section) if section else '') + '\n')
                frontier = 'General finite cardinality and implementable root exclusion remain open.' if mode == 'threshold' else ''
                for question, claim, direction in [('criterion', criterion, 'iff'), ('failure', failure, 'exact'),
                                                   ('uniqueness', unique, 'exact')]:
                    if claim:
                        rows.append((family, mask, 'truncate:known', question, claim, direction,
                                     'written', evidence, frontier, 'trunc-skew-math'))
            for question, claim in cases(family, held, free):
                if question == 'endpoint-pair':
                    reference = 'Concentration and endpoint pairs'
                elif question in {'nonunique', 'left-ray-unique'}:
                    reference = 'Double Pareto on the log scale' if family == 'dblpareto' else 'Fixed power shape'
                elif question in {'corner', 'uniform-logit'}:
                    reference = 'Log-scale reductions'
                else:
                    reference = 'Accessible free-shape faces'
                sections.append(f'**Proved case.** {claim} {link(reference)}.\n')
                rows.append((family, mask, 'truncate:known', 'case:' + question, claim, 'one-sided',
                             'written', evidence, '' if mode != 'open' else detail, 'trunc-skew-math'))
    return rows, sections


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--sections', type=Path, help='optionally write the generated mask sections')
    args = parser.parse_args()
    rows, sections = generate()
    with (DATA / 'proofs.csv').open('w', newline='') as stream:
        writer = csv.writer(stream)
        writer.writerow(('family', 'held', 'modifier', 'question', 'claim', 'direction', 'status',
                         'evidence', 'frontier', 'origin'))
        writer.writerows(rows)
    if args.sections:
        args.sections.write_text('## Family and mask results\n\n' + '\n'.join(sections), encoding='utf-8')
    print(f'{len(rows)} proof rows across {len({(r[0], r[1]) for r in rows})} masks, '
          f'{len(FAMILIES)} families, {sum(s.startswith("### ") for s in sections)} mask sections')


if __name__ == '__main__':
    main()
