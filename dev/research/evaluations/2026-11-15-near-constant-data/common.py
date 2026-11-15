"""Shared helpers: exact near-constant samples from the stored snapshots, representability tests."""
import sys
from pathlib import Path
import mpmath as mp
ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
import strict_common as sc
SNAP = ROOT/'dev/research/evaluations/2026-11-15-fit-snapshot-data'
DBL_MAX = mp.mpf(2)**1024*(1-mp.mpf(2)**-53)
DBL_TRUE_MIN = mp.mpf(2)**-1074

def record(family, mode='native', case='near-constant'):
    return sc.load(str(SNAP/(mode+'.jsonl.gz')))[(family, 'class', case)]

def sample(family, case='near-constant'):
    r = record(family, case=case)
    return [mp.mpf(float(v)) for v in r['data']], r['held']

def representable(v):
    return DBL_TRUE_MIN <= abs(v) <= DBL_MAX

def hexval(s):
    return mp.mpf(float.fromhex(s))

def delta(x):
    return x[2]/x[0]-1
