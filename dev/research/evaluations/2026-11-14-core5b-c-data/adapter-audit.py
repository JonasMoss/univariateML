#!/usr/bin/env python3
"""Inventory the removed binding decisions, independently of snapshot coverage."""
from pathlib import Path
import json,re,yaml
here=Path(__file__).resolve().parent;root=here.parents[3]
F='betapr burr cmp dnorm emg foldnorm gb2 gengamma gpd hnorm hyper invgamma invpareto kumar lnorm lomax nhyper pareto pareto4 rayleigh skellam snorm std triang yulesimon'.split()
rows=[]
for f in F:
 s=(root/'bindings/common/distributions'/f/'fit.hpp').read_text()
 dossier=(root/'univariateML-cpp/include/cppunivariate/distributions'/f/'README.md').read_text()
 metadata=yaml.safe_load(dossier.split('---',2)[1])
 done=s.split('const auto done =',1)[1].split('\n  };',1)[0]
 record=dict(family=f,direct_core_outcome=bool(re.search(r'const auto o = cppunivariate::outcome\(',s)),
   attaches_outcome_without_reclassification='with_outcome(' in done and ', o,' in done and 'if (' not in done and 'if(' not in done,
   data_reclassification=bool(re.search(r'x\.empty\(|x\[|\b(?:all_of|any_of)\(',s)),
   classifier_lambda='const auto classify' in s,override='overridden(' in s or 'FitOutcome::' in s,
   post_core_point_veto=bool(re.search(r'isfinite\((?:res|fit)\.(?!stop_detail)',s)),
   frontend_hold_guard_entries=len(metadata.get('holds') or []))
 assert record['direct_core_outcome'] and record['attaches_outcome_without_reclassification'],record
 assert not any(record[k] for k in ['data_reclassification','classifier_lambda','override','post_core_point_veto']),record
 assert record['frontend_hold_guard_entries']==0,record
 rows.append(record)
(here/'adapter-audit.json').write_text(json.dumps(rows,indent=2)+'\n')
print('25 adapters: direct core outcomes; no data classifiers, outcome overrides, post-core point vetoes, or dossier-generated hold guards.')
