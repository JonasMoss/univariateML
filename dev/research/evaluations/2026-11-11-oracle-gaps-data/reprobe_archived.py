"""Repeat the explicit unsupported-member probes against the final binaries."""
import hashlib,json,math
import audit

def main():
    old=json.loads((audit.HERE/'open-point-probes.json').read_text())
    entries=[r for r in old if r['mode']=='native']
    for entry in entries:
        table=json.loads((audit.DISTS/entry['family']/'truth.json').read_text())
        point=next(p for p in table['excluded_parameter_points'] if p['parameters']==entry['parameters'])
        row=next(r for r in point['values'] if r[1:]==entry['row'][1:])
        entry['row']=row
    out=[]
    for mode,binary in [('native','probe-fixed'),('double','probe-double-fixed')]:
        audit.EXE=audit.WORK/binary
        digest=hashlib.sha256(audit.EXE.read_bytes()).hexdigest()
        actual=audit.evaluate([r['job'] for r in entries])
        assert digest==hashlib.sha256(audit.EXE.read_bytes()).hexdigest()
        for entry,value in zip(entries,actual):
            assert not math.isfinite(value),(mode,entry['family'],value)
            out.append(dict(entry,mode=mode,binary_sha256=digest,actual=str(value),failed=True))
    (audit.HERE/'open-point-final-probes.json').write_text(json.dumps(out,indent=2,allow_nan=False)+'\n')
    print('Final-binary unsupported-member probes:',len(out),flush=True)

if __name__=='__main__':main()
