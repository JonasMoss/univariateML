"""Replay the one final-census golden timeout with the 120-second guard."""
import importlib.util,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('seam',ROOT/'dev/validation/seam-check.py')
s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)
all_cases=s.cases
s.cases=lambda families:(c for c in all_cases(families) if c['source']=='golden')
sys.argv=[str(ROOT/'dev/validation/seam-check.py'),'--family','bhatt','--timeout','120',
          '--output',str(Path(__file__).with_name('retry-golden.jsonl'))]
s.main()
