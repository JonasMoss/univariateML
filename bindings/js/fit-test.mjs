import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync,writeFileSync} from 'node:fs';
import create from '../../build/js/univariateML.mjs';
const m=await create();
const cases=JSON.parse(readFileSync(new URL('../../build/js/generated/fit-cases.json',import.meta.url)),(_,v)=>v==='Inf'||v==='Infinity'?Infinity:v==='-Inf'||v==='-Infinity'?-Infinity:v==='NaN'?NaN:v);
if(process.env.FIT_CAPTURE) {
 const selected=new Set(process.env.FIT_CAPTURE.split(','));
 const capture=cases.filter(c=>selected.has(c.id)).map(c=>({id:c.id,g:c.g,result:m['fit'+c.id](new Float64Array(c.g.data),c.held,c.control)}));
 writeFileSync('build/scratch/wasm-fit-capture.json',JSON.stringify(capture,null,2));
}
for(const c of cases) {
 if(process.env.FIT_FAMILY && !process.env.FIT_FAMILY.split(/[,|]/).includes(c.id)) continue;
 const fit=(held,control=c.control)=>m['fit'+c.id](new Float64Array(c.g.data),held,control);
 test(`${c.id} shared golden`,()=>{
  const r=fit(c.held);assert.equal(r.error,undefined,r.error);assert.equal(r.status,'Ok');
  for(const [k,v] of Object.entries(c.g.estimates)) assert.ok(r.estimates[k]===v || Math.abs(r.estimates[k]-v)<=(c.tolerances[k]??c.tol)*((c.tolerance_scales[k]??0)+Math.max(Math.abs(v),Math.abs(r.estimates[k]))),`${k}: ${r.estimates[k]} vs ${v} tolerance ${c.tolerances[k]??c.tol}`);
  assert.ok(Math.abs(r.logLik-c.g.logLik)<=c.ll_tol*(c.ll_scale+Math.max(Math.abs(r.logLik),Math.abs(c.g.logLik))),`${r.logLik} vs ${c.g.logLik}`);
 });
 test(`${c.id} all held subsets`,()=>{
  const names=Object.keys(c.g.estimates);
  for(let mask=0;mask<1<<names.length;mask++) {
   const held=Object.fromEntries(names.filter((_,i)=>mask>>i&1).map(k=>[k,c.g.estimates[k]]));
   const r=fit(held);
   if(r.error) {
    if(c.id==='cmb' && !('size' in held) && !('nu' in held) && r.error.includes('not implemented for this sample')) continue;
    const exceptions=c.holds.filter(h=>h.held.every(k=>k in held)&&(h.held.length===Object.keys(held).length||h.held.length&&!h.reason.includes('Estimating')));
    const reasons=[...exceptions.map(h=>h.reason),...c.limits.map(l=>l.message||l.model&&`ml${l.model}`||l.name&&l.name.replace(/^shifted /,'').split(';')[0]||'')].filter(Boolean);
    assert.ok(exceptions.length || reasons.some(s=>r.error.toLowerCase().includes(s.toLowerCase())),`${JSON.stringify(held)}: ${r.error}`);
   } else {
    for(const [k,v] of Object.entries(held)) assert.equal(r.estimates[k],v);
    assert.ok(names.filter(k=>!(k in held)).every(k=>Number.isFinite(r.estimates[k])));
    assert.ok(r.logLik>=c.g.logLik-1e-6*c.g.n,`${JSON.stringify(held)}: logLik ${r.logLik}`);
   }
  }
 });
 test(`${c.id} held-value diagnostic`,()=>{
  for(const name of Object.keys(c.g.estimates)) {
   const held={...c.held,[name]:NaN};
   const r=fit(held);assert.ok(r.error,`${name}: ${JSON.stringify(r)}`);
  }
  for(const bad of [NaN,Infinity]) {
   const data=[...c.g.data];data[0]=bad;
   const invalid=m['fit'+c.id](new Float64Array(data),c.held,c.control);
   assert.ok(invalid.error,`${c.id} accepted nonfinite observations`);
  }
  if(c.integer) {
   const data=[...c.g.data];data[0]+=.5;
   const invalid=m['fit'+c.id](new Float64Array(data),c.held,c.control);
   assert.match(invalid.error,/representable finite integers/);
  }
 });
}
