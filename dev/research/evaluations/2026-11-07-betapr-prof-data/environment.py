"""Small machine/environment record; invoke under the CPU wrapper."""
import json,platform,subprocess
import numpy,scipy,mpmath
print(json.dumps(dict(platform=platform.platform(),python=platform.python_version(),
    numpy=numpy.__version__,scipy=scipy.__version__,mpmath=mpmath.__version__,
    compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
    r_bh=subprocess.check_output(['Rscript','-e','cat(R.version.string,"; BH ",as.character(packageVersion("BH")))'],text=True)),indent=2))
