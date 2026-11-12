"""Read freshly regenerated JSON or its compact retained gzip form."""
import gzip,json
from pathlib import Path
def read_json(path):
 path=Path(path)
 with (path.open() if path.exists() else gzip.open(str(path)+'.gz','rt')) as f:return json.load(f)
