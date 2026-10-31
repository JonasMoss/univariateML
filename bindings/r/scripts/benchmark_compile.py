#!/usr/bin/env python3
"""Compiler timing launcher for clean unity-build measurements.

Set CXX17 = <python3> <absolute-path>/benchmark_compile.py in a private
R_MAKEVARS_USER, CXX17FLAGS = -g -O2, and UMVL_COMPILE_LOG to a JSONL path.
Run R CMD INSTALL --preclean from a fresh package copy under one CPU slot,
with MAKEFLAGS=-j1 and CCACHE_DISABLE=1. This launcher bypasses ccache.
Each record covers one compiler invocation and includes child peak RSS.
"""
import json
import os
import resource
import subprocess
import sys
import time

start = time.monotonic()
result = subprocess.run(["/usr/bin/g++", *sys.argv[1:]], check=False)
usage = resource.getrusage(resource.RUSAGE_CHILDREN)
with open(os.environ["UMVL_COMPILE_LOG"], "a", encoding="utf-8") as output:
    output.write(json.dumps({
        "args": sys.argv[1:], "seconds": time.monotonic() - start,
        "rss_kib": usage.ru_maxrss, "status": result.returncode,
    }) + "\n")
sys.exit(result.returncode)
