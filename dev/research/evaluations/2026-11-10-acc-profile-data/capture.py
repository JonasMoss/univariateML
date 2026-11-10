#!/usr/bin/env python3
"""Emit default binding calls for the eleven accuracy-contract families."""
from pathlib import Path
source=Path(__file__).resolve().parents[0].parent/'2026-11-09-golden-ref-port-data/capture.py'
s=source.read_text().replace("ids=sorted(p.parent.name for p in dists.glob('*/golden.json'))", "ids='sn skellam sgomp dgamma pareto3 zinb zmnbinom ztnbinom ht emg genbeta'.split()")
exec(compile(s,str(source),'exec'),{'__file__':str(source)})
