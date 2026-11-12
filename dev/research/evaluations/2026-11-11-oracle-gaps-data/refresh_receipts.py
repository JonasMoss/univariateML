"""Verify final membership and fingerprints after metadata-only refresh."""
import collections,gzip,hashlib,json
import audit
from classify_rows import canonical
from quality.truth import input_sha256
HERE=audit.HERE
active=collections.Counter();hashes={};fingerprints=[]
for family in audit.adapters():
 path=audit.DISTS/family/'truth.json';table=json.loads(path.read_text())
 hashes[family]=hashlib.sha256(path.read_bytes()).hexdigest()
 if family!='gengamma':
  assert table['input_sha256']==input_sha256(path.parent),family
  assert 'density_sha256' not in table
  assert all('seconds' not in row for row in table['normalization'])
 fingerprints.append(dict(family=family,checked=family!='gengamma',reason='Explicit concurrent ownership; unchanged baseline table' if family=='gengamma' else None))
 for category in ('values','quantiles'):
  for row in table[category]:active[canonical(family,table['parameters'][row[0]],category,row)]+=1
for mode,binary in [('native','probe-fixed'),('double','probe-double-fixed')]:
 path=HERE/f'final-{mode}-summary.json';receipt=json.loads(path.read_text())
 assert receipt['binary_sha256']==hashlib.sha256((audit.WORK/binary).read_bytes()).hexdigest()
 with gzip.open(HERE/(receipt['source_measurement']+'-rows.json.gz'),'rt') as stream:measured=collections.Counter(json.load(stream))
 assert not active-measured
 for f in json.loads((HERE/f'final-{mode}-failures.json').read_text()):
  assert canonical(f['family'],f['parameters'],f['category'],f['row']) not in active
 receipt['table_sha256']=hashes;receipt['final_active_rows']=sum(active.values())
 path.write_text(json.dumps(receipt,indent=2)+'\n')
(HERE/'fingerprints.json').write_text(json.dumps(fingerprints,indent=2)+'\n')
print('Final table membership, binary hashes and in-scope fingerprints verified:',len(hashes))
