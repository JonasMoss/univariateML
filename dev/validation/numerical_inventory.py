#!/usr/bin/env python3
"""Reproduce the step-7 lexical call-path index (no compiler/dispatch claims).

Outputs are deterministic; only repository source headers are read. Calls in
comments are removed. Family entrypoints and named shared-function bodies are
followed conservatively, while include closure is recorded independently.
"""
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
INC = ROOT / 'univariateML-cpp/include/cppunivariate'
OUT = ROOT / 'dev/research/evaluations'

def code(path):
    s = path.read_text()
    return re.sub(r'//[^\n]*|/\*.*?\*/', '', s, flags=re.S)

def closure(path, seen=None):
    seen = set() if seen is None else seen
    path = path.resolve()
    if path in seen or not path.is_file() or not path.is_relative_to(INC):
        return seen
    seen.add(path)
    for name in re.findall(r'#include\s+"([^"]+)"', path.read_text()):
        closure(path.parent / name, seen)
    return seen

def calls(s, prefix):
    return sorted(set(re.findall(prefix + r'([A-Za-z_]\w*)\s*(?:<[^;{}]*?>)?\s*\(', s)))

def run():
    rows = []
    shared_symbols = {}
    wrappers = {}
    for p in sorted(list((INC/'core').glob('*.hpp'))+list((INC/'special').glob('*.hpp'))):
        for name in re.findall(r'inline\s+(?:constexpr\s+)?[\w:<>]+\s+(\w+)\s*\(', code(p)):
            shared_symbols.setdefault(name, []).append(str(p.relative_to(INC)))
        text = code(p)
        for match in re.finditer(r'inline\s+[\w:<>]+\s+(\w+)\([^{};]*\)\s*\{', text):
            start = match.end(); end = start; depth = 1
            while depth and end < len(text):
                depth += (text[end] == '{') - (text[end] == '}'); end += 1
            # Same-name helpers are unioned conservatively, not overload-resolved.
            wrappers[match[1]] = wrappers.get(match[1], '')+'\n'+text[start:end-1]
    for family in sorted((INC / 'distributions').iterdir()):
        if not family.is_dir():
            continue
        own = sorted(family.glob('*.hpp'))
        reachable = set().union(*(closure(p) for p in own))
        direct = '\n'.join(code(p) for p in own)
        special = calls(direct, r'(?:cppunivariate::)?special::')
        expanded = set(special) | {name for name in wrappers if re.search(r'\b'+re.escape(name)+r'\s*\(', direct)}
        pending = list(expanded)
        while pending:
            name = pending.pop()
            for called in calls(wrappers.get(name, ''), r'\b'):
                if called in wrappers and called not in expanded:
                    expanded.add(called); pending.append(called)
        boost = sorted(set(c for name in expanded for c in calls(wrappers.get(name, ''), r'boost::math::')))
        # distribution constructors must also be listed (template syntax).
        for name in expanded:
            boost += re.findall(r'boost::math::(\w+_distribution)\s*<', wrappers.get(name, ''))
        markers = []
        for p in own:
            for n, line in enumerate(p.read_text().splitlines(), 1):
                if re.search(r'newton|bisect|brent|safeguard|\b(?:mid|step)\s*=', line, re.I):
                    markers.append(f'{p.name}:{n}: {line.strip()}')
        rows.append(dict(family=family.name, direct_special=special, shared_function_closure=sorted(expanded), boost_via_shared_functions=sorted(set(boost)),
            direct_std=calls(direct, r'std::'), std_via_shared_functions=sorted(set(c for name in expanded for c in calls(wrappers.get(name, ''), r'std::'))), shared_headers=sorted(str(p.relative_to(INC)) for p in reachable if p.parent.name in ('core','special')),
            local_headers=[p.name for p in own], reachable_header_calls={str(p.relative_to(INC)): {'std': calls(code(p), r'std::'), 'boost': calls(code(p), r'boost::math::')} for p in sorted(reachable)}, direct_own_symbols={name: paths for name,paths in shared_symbols.items() if re.search(r'(?<![\w:])'+re.escape(name)+r'\s*\(', direct) or re.search(r'cppunivariate::'+re.escape(name)+r'\s*\(', direct)}, solver_markers=markers,
            loops=sum(len(re.findall(r'\b(?:for|while)\s*\(',code(p))) for p in own),
            tail_helpers=[f'{p.name}:{n}: {line.strip()}' for p in own for n,line in enumerate(p.read_text().splitlines(),1)
                if re.search(r'^\s*(?:inline|static)\s+\w+\s+(?:log1mexp\w*|logadd\w*|log_sum_exp|logupper|logmills)\s*\(',line)]))
    # Explicit calls to public entrypoints and uniquely owned helpers follow transformations and
    # delegates without treating every included special wrapper as executed.
    public = {}
    for r in rows:
        for symbol in [r['family']+'_mle']+[prefix+r['family'] for prefix in 'dpq']:
            public[symbol] = r['family']
    unique_helpers = {}
    for r in rows:
        for p in sorted((INC/'distributions'/r['family']).glob('*.hpp')):
            for name in re.findall(r'inline\s+[\w:<>]+\s+(\w+)\s*\(', code(p)):
                unique_helpers.setdefault(name, set()).add(r['family'])
    for name, owners in unique_helpers.items():
        if len(owners) == 1 and '_' in name and name not in shared_symbols:
            public.setdefault(name, next(iter(owners)))
    by_id = {r['family']: r for r in rows}
    for r in rows:
        direct = '\n'.join(code(p) for p in sorted((INC/'distributions'/r['family']).glob('*.hpp')))
        r['family_calls'] = {name: family for name,family in public.items()
            if family != r['family'] and re.search(r'\b'+name+r'\s*\(', direct)}
    for r in rows:
        reachable = {r['family']}; pending = [r['family']]
        while pending:
            name = pending.pop()
            for target in by_id[name]['family_calls'].values():
                if target not in reachable:
                    reachable.add(target); pending.append(target)
        r['family_call_closure'] = sorted(reachable-{r['family']})
        r['boost_via_family_calls'] = sorted(set(c for family in reachable
            for c in by_id[family]['boost_via_shared_functions']))
        r['std_via_family_calls'] = sorted(set(c for family in reachable
            for c in by_id[family]['direct_std']+by_id[family]['std_via_shared_functions']))
    output = OUT / '2026-10-31-numerical-call-paths.json'
    output.write_text(json.dumps(rows, indent=2)+'\n')
    md = ['<!-- Generated by dev/validation/numerical_inventory.py; do not hand edit. -->',
          '# Family numerical call paths', '',
          'Lexical index: direct calls in all family headers; Boost names are expanded through named shared-function bodies and explicit family delegates (entrypoints and uniquely owned helper names containing an underscore (excluding shared-name collisions)). Shared same-name overload bodies are unioned conservatively; namespace/overload dispatch is not compiler-resolved. Explicit family entrypoint and uniquely owned distinctive helper calls are followed transitively in the JSON (with their Boost targets); this is conservative over all held/default branches. Include closure lists *available*, not necessarily executed, shared routines. Delegation, templates, unqualified calls, RNG internals and conditional reachability require source review; empty cells do not prove absence. Solver markers nominate comments/calls, including multidimensional Newton and shared-helper calls, rather than count certified scalar solvers. JSON preserves source locations and local headers for review.', '',
          '| Family | Direct special calls → Boost names (shared helpers and delegates) | Standard calls (including named shared helpers/delegates) | Named own helpers (lexical) | Shared core/special header closure | Solver markers / loops |',
          '|---|---|---|---|---|---|']
    for r in rows:
        md.append('| '+r['family']+' | '+', '.join(r['direct_special'])+' → '+', '.join(r['boost_via_family_calls'])+' | '+', '.join(r['std_via_family_calls'])+' | '+', '.join(r['direct_own_symbols'])+' | '+', '.join(r['shared_headers'])+f" | {len(r['solver_markers'])} / {r['loops']} |")
    (OUT/'2026-10-31-numerical-call-paths.md').write_text('\n'.join(md)+'\n')
    print(json.dumps(dict(families=len(rows), solver_marker_families=[r['family'] for r in rows if r['solver_markers']],tail_helpers={r['family']:r['tail_helpers'] for r in rows if r['tail_helpers']}), indent=2))

if __name__ == '__main__':
    run()
