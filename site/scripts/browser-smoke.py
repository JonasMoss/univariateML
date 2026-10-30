"""Optional browser checks, pinned to the single acquired CPU slot."""
from pathlib import Path
import os
ROOT=Path(__file__).resolve().parents[2]
if hasattr(os,'sched_setaffinity'): os.sched_setaffinity(0,{min(os.sched_getaffinity(0))})
node=str(ROOT/'site/node_modules/.bin/node')
os.execv(node,[node,str(ROOT/'site/scripts/browser-smoke.mjs')])
