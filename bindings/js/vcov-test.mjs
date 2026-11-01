import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {execFileSync} from 'node:child_process';
import create from './index.mjs';
const api=await create();
const golden=id=>JSON.parse(readFileSync(new URL(`../../univariateML-cpp/include/cppunivariate/distributions/${id}/golden.json`,import.meta.url)));
for(const id of ['norm','gamma','beta','pois','exp','lnorm','betapr','binom']) {
 test(`covariance native agreement: ${id}`,()=>{
  const g=golden(id), x=new Float64Array(g.data);
  const holds=[{},...Object.keys(g.estimates).map(name=>({[name]:g.estimates[name]})),g.estimates];
  if(id==='binom') holds.splice(0,holds.length,{size:Math.max(...x)+2});
  for(const held of holds) {
   const fit=api[id].fit(x,held), names=Object.keys(fit.estimates);
   const mask=names.reduce((m,k,i)=>m|(fit.fixed.includes(k)?0:1<<i),0);
   const input=[id,names.length,mask,x.length,...Object.values(fit.estimates),...x].join(' ');
   const rows=execFileSync(fileURLToPath(new URL('../../build/scratch/vcov-native',import.meta.url)),{input,encoding:'utf8'}).trim().split('\n');
   for(const [i,type] of ['sandwich','observed'].entries()) {
    const [status,dim,...expected]=rows[i].split(' ').map(Number);
    assert.equal(status,0);
    const v=api[id].vcov(x,fit,type);
    assert.deepEqual(v.names,names.filter(k=>!fit.fixed.includes(k)));
    assert.deepEqual(v.held,fit.fixed); assert.equal(v.matrix.length,dim);
    assert.equal(v.type,type); assert.ok(v.validity.length>0);
    v.matrix.flatMap(row=>[...row]).forEach((z,j)=>assert.ok(Math.abs(z-expected[j])<=2e-4*Math.max(1e-6,Math.abs(expected[j])),`${id} ${type} ${j}: ${z} vs ${expected[j]}`));
   }
   assert.deepEqual(api[id].vcov(x,fit),api[id].vcov(x,fit,'sandwich'));
  }
 });
}
test('covariance diagnostics and input ownership',()=>{
 const x=new Float64Array(golden('norm').data), fit=api.norm.fit(x);
 assert.throws(()=>api.norm.vcov(x,fit,'bad'),/type/);
 assert.throws(()=>api.norm.vcov(null,fit),/observations/);
 assert.throws(()=>api.norm.vcov(x,{...fit,fixed:undefined}),/free mask/);
 assert.throws(()=>api.norm.vcov(x,{...fit,restricted:{sd:'lower'}}),/fitting restriction/);
 assert.throws(()=>api.norm.vcov(x,{...fit,estimates:{...fit.estimates,sd:0}}),/boundary or limit/);
 const lx=new Float64Array([-2,0,.5,1,3]);
 assert.throws(()=>api.laplace.vcov(lx,api.laplace.fit(lx)),/regime unavailable.*nonsmooth/);
 const ux=new Float64Array([0,1,2]);
 assert.throws(()=>api.unif.vcov(ux,api.unif.fit(ux)),/unavailable/);
 for(const [scale,reason] of [[3000,null],[1e5,/ill-conditioned information/]]) {
  const gx=new Float64Array(golden('gamma').data.map(z=>z*scale)),gf=api.gamma.fit(gx);
  if(reason) assert.throws(()=>api.gamma.vcov(gx,gf),reason);
  else {const v=api.gamma.vcov(gx,gf);assert.ok(v.condition>1e6);assert.match(v.warnings[0],/poorly conditioned/);}
 }
 const bx=new Float64Array(golden('binom').data), bf=api.binom.fit(bx,{size:Math.max(...bx)+2});
 assert.throws(()=>api.binom.vcov(bx,{...bf,fixed:[]}),/coefficient subset.*size/);
 assert.throws(()=>api.norm.vcov(new Float64Array(),fit),/empty sample/);
 assert.throws(()=>api.norm.vcov(new Float64Array([Infinity]),fit),/invalid observation|nonfinite/);
 assert.throws(()=>api.norm.vcov(x,{...fit,estimates:{...fit.estimates,sd:fit.estimates.sd*100}}),/indefinite information/);
 const first=api.norm.vcov(x,fit);first.matrix[0][0]=123;
 assert.notEqual(api.norm.vcov(x,fit).matrix[0][0],123);
});
