// Run from the repository root after just js-build.
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {pathToFileURL} from 'node:url';
const {default: create}=await import(pathToFileURL(`${process.cwd()}/build/js/univariateML.mjs`));
const m=await create();
const evidence=JSON.parse(readFileSync('dev/research/evaluations/2026-11-08-ged-density-data/comparison-native.json','utf8'));
let checks=0;
for(const record of evidence.records) {
  const [kind,...fields]=record.row.split(' ');
  if(kind==='quantile') {
    const [xi,nu]=fields.map(Number);
    const got=m.qsged(new Float64Array([.1]),[0,1e300,nu,xi],true,false)[0];
    assert.ok(Math.abs(got/Number(record.reference)-1)<2e-10,record.row);
    ++checks;
  } else {
    const [x,mean,sd,nu,xi]=fields.map(Number);
    const pars=kind==='ged' ? [mean,sd,nu] : [mean,sd,nu,xi];
    const values=[m[`d${kind}`](new Float64Array([x]),pars,true,true)[0],
      m[`p${kind}`](new Float64Array([x]),pars,true,true)[0],
      m[`p${kind}`](new Float64Array([x]),pars,false,true)[0]];
    for(let i=0;i<3;++i) {
      const ref=Number(record.reference[i]);
      const tolerance=Math.abs(ref)>.1 ? 3e-11*Math.max(1,Math.abs(ref)) : 3e-10*Math.abs(ref);
      assert.ok(Math.abs(values[i]-ref)<tolerance,`${record.row}: ${values[i]} versus ${ref}`);
      ++checks;
    }
  }
}
console.log(`${checks} WASM scalar comparisons against independent 100-digit references passed`);
