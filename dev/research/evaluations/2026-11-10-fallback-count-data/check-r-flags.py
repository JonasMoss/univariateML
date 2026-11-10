#!/usr/bin/env python3
"""Run the current three affected R files without racing a queued package build."""
import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
families = ("zib", "ztbinom", "zmbinom")
scratch = ROOT / "build/scratch"
scratch.mkdir(parents=True, exist_ok=True)
clone = Path(tempfile.mkdtemp(prefix="count-r-flags-", dir=scratch))
package = clone / "univariateML"
shutil.copytree(
    ROOT / "univariateML-r", package,
    ignore=shutil.ignore_patterns("*.o", "*.so", "*.dll", "*.a", "*.tar.gz",
                                 ".Rproj.user", "__pycache__", "node_modules"),
)
hashes = {}
for family in families:
    source = ROOT / f"univariateML-cpp/include/cppunivariate/distributions/{family}/mle.hpp"
    destination = package / f"inst/include/cppunivariate/distributions/{family}/mle.hpp"
    shutil.copyfile(source, destination)
    hashes[str(source.relative_to(ROOT))] = hashlib.sha256(source.read_bytes()).hexdigest()
    source = ROOT / f"bindings/r/distributions/{family}/test.R"
    shutil.copyfile(source, package / f"tests/testthat/test-dist-{family}.R")
    hashes[str(source.relative_to(ROOT))] = hashlib.sha256(source.read_bytes()).hexdigest()

csv_path = HERE / "r-flags-dev-results.csv"
runner = clone / "check.R"
runner.write_text(
    'options(pkg.build_extra_flags=FALSE)\n'
    'Sys.setenv(PKG_BUILD_EXTRA_FLAGS="false")\n'
    'results <- testthat::test_local(".", filter="dist-(zib|ztbinom|zmbinom)$", '
    'stop_on_failure=TRUE, stop_on_warning=TRUE)\n'
    'rows <- as.data.frame(results)\n'
    f'write.csv(rows[,setdiff(names(rows),"result"),drop=FALSE], '
    f'{json.dumps(str(csv_path))}, row.names=FALSE)\n'
)
environment = dict(os.environ, UMVL_TEST_TIER="dev", TESTTHAT_CPUS="1",
                   PKG_BUILD_EXTRA_FLAGS="false", MAKEFLAGS="-j1",
                   CCACHE_DIR=os.environ.get("UMVL_CCACHE_DIR", str(
                       Path.home() / ".cache/ccache/univariateML")),
                   CCACHE_BASEDIR=str(ROOT), CCACHE_NOHASHDIR="1",
                   UMVL_DISTRIBUTIONS_DIR=str(
                       ROOT / "univariateML-cpp/include/cppunivariate/distributions"))
print("Isolated R validation:", package, flush=True)
subprocess.run(["Rscript", str(runner)], cwd=package, env=environment, check=True)
with csv_path.open() as stream:
    rows = list(csv.DictReader(stream))
expected = (
    "ztbinom does not certify a sub-machine probability target",
    "a closed binomial mean respects the requested coefficient precision",
    "zmbinom carries the zero-ratio precision flag",
)
assert rows
assert all(any(title in row.get("test", "") for row in rows) for title in expected)
(HERE / "r-flags-dev-validation.json").write_text(json.dumps(
    dict(tier="dev", workers=1, families=families, source_sha256=hashes,
         required_new_tests=list(expected), new_tests_present=True,
         test_rows=len(rows), csv=csv_path.name, isolated=True), indent=2) + "\n")
shutil.rmtree(clone)
print("Current isolated R checks passed; clone removed.", flush=True)
