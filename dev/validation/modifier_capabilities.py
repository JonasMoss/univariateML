#!/usr/bin/env python3
"""Generate configuration coverage and family reductions from dossier front matter.

Golden coverage names the retained legacy regression fixture, not proof of
correctness. Certification is copied from the declared scope, never inferred.
"""
from pathlib import Path
import csv
import itertools
import json
import yaml
ROOT = Path(__file__).resolve().parents[2]
DOSSIERS = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'

def inventory():
    return {p.parent.name: yaml.safe_load(p.read_text().split('---', 2)[1])
            for p in sorted(DOSSIERS.glob('*/README.md')) if (p.parent/'mle.hpp').exists()}

def reduction(id, m):
    relations = m.get('relations', {})
    edges = relations.get('modification_of', [])
    if edges:
        e = edges[0]
        return e['family'], 'zero=' + e['type'].removeprefix('zero-')
    edges = relations.get('truncation_of', []) if 'truncation' in m.get('construction', []) or m.get('modifier_replacement',False) else []
    if edges:
        return edges[0]['family'], 'truncate=closed-window'
    return id, 'none'

def generate():
    models = inventory()
    reductions = {id: reduction(id, m) for id, m in models.items()}
    inverse = {(base, value): id for id, (base, value) in reductions.items() if base != id}
    rows = []
    for id, m in models.items():
        s = m['support']; declared = m.get('modifiers', {})
        eligible = (s['type'] == 'integer' and s['lower'] == 0 and
                    not set(m.get('construction', [])) & {'truncation', 'modification'}) or declared.get('zero_exception', False)
        names = [p['name'] for p in m['params']]
        configs = [('none', True, m.get('fit', {}))]
        if eligible:
            configs += [('zero='+v, True, declared.get('zero', {}).get(v, {})) for v in ['truncated','modified','inflated']]
        configs += [('truncate=closed-window', True, declared.get('truncate', {}))]
        configs += [('censor='+v, True, declared.get('censor', {}).get(v, {})) for v in ['right','left','interval']]
        for config, eligible, capability in configs:
            coefficients = names + (['p0'] if config=='zero=modified' else ['pi'] if config=='zero=inflated' else [])
            for n in range(len(coefficients)+1):
                for held in itertools.combinations(coefficients, n):
                    status = 'implemented' if config=='none' else capability.get('status', 'not-implemented')
                    cert = capability.get('certified', 'unknown')
                    if status=='certified': status, cert = 'implemented', 'yes'
                    exceptions = m.get('holds', []) if config=='none' or config.startswith('zero=') else capability.get('holds', [])
                    for h in exceptions:
                        if status=='implemented' and set(h['held']) == set(held).intersection(names): status=h['status']
                    if not set(capability.get('requires_held',[])).issubset(held) or set(capability.get('requires_free',[])).intersection(held):
                        status='not-implemented'
                    if status=='not-implemented': cert='unknown'
                    fixture = id if config=='none' else inverse.get((id,config),'')
                    golden = ''
                    if fixture and (DOSSIERS/fixture/'golden.json').exists():
                        golden = fixture + '/golden.json'
                    rows.append(dict(family=id, base_family=reductions[id][0], family_modifier=reductions[id][1],
                                     retirement='pending' if reductions[id][0]!=id else '', held=';'.join(held), modifier=config,
                                     eligible=str(eligible).lower(), status=status, certified=cert,
                                     escape_count=len(capability.get('limits',m.get('limits',[]) if config=='none' else [])),
                                     golden_coverage=golden))
    target = ROOT/'dev/validation/capabilities.csv'
    with target.open('w', newline='') as f:
        writer=csv.DictWriter(f, fieldnames=rows[0].keys(), lineterminator='\n');writer.writeheader();writer.writerows(rows)
    target=ROOT/'dev/validation/family-reductions.csv'
    with target.open('w',newline='') as f:
        writer=csv.writer(f, lineterminator='\n');writer.writerow(['family','base_family','modifier','modifier_arguments','base_coefficients','domain_condition','retirement'])
        for id,(base,value) in reductions.items():
            m=models[id];arguments='';mapping={};condition=''
            if id!=base:
                kind='modification_of' if value.startswith('zero=') else 'truncation_of'
                edge=m['relations'][kind][0];mapping=edge['at'];condition=edge.get('when','')
                if kind=='modification_of':
                    arguments='zero = "'+edge['type'].removeprefix('zero-')+'"'
                    if edge.get('mass'): arguments+=', '+('pi' if edge['type']=='zero-inflated' else 'p0')+' = '+str(edge['mass'])
                else:
                    def bound(v): return str(v).replace('inf','Inf')
                    arguments='truncate = c('+bound(m['support']['lower'])+', '+bound(m['support']['upper'])+')'
            writer.writerow([id,base,value,arguments,json.dumps(mapping,sort_keys=True) if mapping else '',condition,'pending' if id!=base else ''])
    print(json.dumps(dict(families=len(models),retirements=sum(id!=b for id,(b,v) in reductions.items()),
                          remaining=sum(id==b for id,(b,v) in reductions.items()),configurations=len(rows))))

if __name__=='__main__': generate()
