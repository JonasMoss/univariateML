"""Check local note keys and DOI inventory; not a bibliographic support audit."""
import hashlib
import json
from pathlib import Path
import re

root = Path(__file__).resolve().parents[4]
source = root / 'dev/research/boundary-points.tex'
tex = source.read_text()
entries = dict(re.findall(r'\\bibitem\{([^}]+)\}(.*?)(?=\\bibitem|\\end\{thebibliography\})', tex, re.S))
cites = set()
for group in re.findall(r'\\cite(?:\[[^]]*\])?\{([^}]+)\}', tex):
    cites.update(group.split(','))
assert len(entries) == 21
assert set(entries) == cites, (set(entries)-cites, cites-set(entries))
assert '\u2014' not in tex and '---' not in tex
metadata = json.loads(Path(__file__).with_name('metadata.json').read_text())['records']
assert set(metadata) == set(entries)-{'bkrw'}
for key, record in metadata.items():
    assert 'error' not in record, record
    dois = re.findall(r'\\doi\{([^}]+)\}', entries[key])
    assert len(dois) == 1
    assert dois[0].casefold() == record['metadata']['DOI'].casefold()
    # DOI agreement is an identity check only; field review is in the report.
out = {'source_sha256': hashlib.sha256(source.read_bytes()).hexdigest(),
       'bibliography_entries': len(entries), 'distinct_cited_keys': len(cites),
       'missing_keys': [], 'uncited_entries': [], 'duplicate_keys': [],
       'DOIs_matched_to_retrieved_records': 20, 'no_DOI_entry': 'bkrw (1993 Johns Hopkins edition)',
       'scope': 'local key and DOI agreement only; bibliographic fields and claim support reviewed separately'}
Path(__file__).with_name('validation.json').write_text(json.dumps(out, indent=2)+'\n')
print(json.dumps(out, indent=2))
