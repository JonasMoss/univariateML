#!/usr/bin/env python3
"""Negative control for dev/validation/equivariance.py: a wrong map must fail.

For every declared family, the first mapped coefficient's expression is
perturbed (a factor 1.001, or 1.001*A + 0.01 in place of A), and the check
must report a failure on both the dossier density (40 digits) and the C++
density. Run from anywhere:
  python3 equivariance_negative_control.py > equivariance-negative-control.txt
"""
import copy, sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parents[3]/'validation'))
import equivariance as eqv
cpp = eqv.Cpp()
try:
    for ident, meta, eq in eqv.declared():
        bad = copy.deepcopy(eq)
        k = sorted(bad['map'])[0]
        bad['map'][k] = '(%s)*1.001' % bad['map'][k] if 'A +' not in bad['map'][k] else bad['map'][k].replace('A +', '1.001*A + 0.01 +')
        r = eqv.check(ident, meta, bad, cpp=cpp, show_symbolic=False)
        print(ident, k, bad['map'][k], 'detected' if not r['ok'] else 'NOT DETECTED', '|', r.get('dossier'), '|', r.get('cpp'))
finally: cpp.close()
