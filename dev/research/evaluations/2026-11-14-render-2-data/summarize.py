#!/usr/bin/env python3
"""Per-family counts of the four classifications (render-2), for the report.

Usage: summarize.py  (reads classification-*.json.gz beside this script)
Prints category totals per mode and, for the former tie categories and the
invalid-input category, the per-family counts and case lists.
"""
import collections,gzip,json
from pathlib import Path
HERE=Path(__file__).resolve().parent
MODES=('native','double','routes-native','routes-double')
FOCUS=('tie stop to estimate (point log likelihood)','tie stop to limit stop','tie stop to unresolved stop','invalid-input status (binom, lgamma)')
for mode in MODES:
 d=json.loads(gzip.open(HERE/('classification-%s.json.gz'%mode),'rt').read())
 print('==',mode,d['cases'],'cases; defects',len(d['defects']))
 for k,v in d['categories'].items():print('  %-52s %5d'%(k,v))
 for cat in FOCUS:
  per=collections.defaultdict(list)
  for c in d['classified']:
   if c['category']==cat:per[c['family']].append(c['case'])
  if per:print('  --',cat+':',', '.join('%s %d (%s)'%(f,len(v),' '.join(v)) for f,v in sorted(per.items())))
