import assert from 'node:assert/strict';
import {readFileSync,mkdirSync,writeFileSync} from 'node:fs';
import create from '../../../../bindings/js/index.mjs';
import {specs,preset,parseSample,profile,profileSvg,faces,facesSvg,faceSample,conditional,conditionalSvg,niceTicks} from './model.mjs';
const api=await create(),html=readFileSync(new URL('../../../dist/theory/index.html',import.meta.url),'utf8'),results=[];
assert.deepEqual(parseSample('1, 2; 3\n4'),[1,2,3,4]);
for(const bad of ['', '1 2', '1 junk 3','1 Infinity 3'])assert.throws(()=>parseSample(bad));
for(const id of Object.keys(specs)){
 const r=profile(api,id,preset(id));assert.ok(html.includes(profileSvg(r)),`${id}: static SVG matches live computation`);
 assert.equal(r.points.filter(p=>p.error).length,0,`${id}: held fit errors`);
 if(id==='gamma'){
  assert.ok(r.free.fit);assert.ok(r.points.every(p=>p.y<=r.free.fit.logLik+1e-7));
 }else{
  assert.match(r.free.error,new RegExp('ml'+specs[id].limit));assert.ok(r.limit.fit);
  const ref=r.limit.fit.logLik,last=r.points.at(-1).y;
  assert.ok(Math.abs(last-ref)<.02,`${id}: final gap ${last-ref}`);
  assert.ok(Math.abs(last-ref)<Math.abs(r.points[0].y-ref),`${id}: approach improves`);
 }
 results.push({id,message:r.free.error||'finite MLE',limit:r.limit?.fit?.logLik,last:r.points.at(-1).y});
}
const r=faces(api,faceSample),n=faceSample.length,mean=faceSample.reduce((a,b)=>a+b,0)/n;
const sd=Math.sqrt(faceSample.reduce((a,b)=>a+(b-mean)**2,0)/n);
assert.ok(Math.abs(r.free.estimates.mean-mean)<1e-12);assert.ok(Math.abs(r.free.estimates.sd-sd)<1e-12);
assert.ok(Math.abs(r.meanFit.estimates.sd-Math.sqrt(faceSample.reduce((a,b)=>a+b*b,0)/n))<1e-12);
assert.ok(Math.abs(r.sdFit.estimates.mean-mean)<1e-12);
assert.ok(r.meanFit.logLik<=r.free.logLik&&r.sdFit.logLik<=r.free.logLik);
assert.ok(html.includes(facesSvg(r)),'static contour matches live computation');
const far=faces(api,faceSample,20,.2);assert.ok(far.ymax>far.meanFit.estimates.sd&&far.xmax>20,'held fits stay inside plot');
// Failure paths must produce gaps and preserve messages, without a fallback estimate.
const fail=profile({gamma:{fit(){throw Error('deliberate numerical failure');}}},'gamma',[1,2,3]);
assert.equal(fail.points.filter(p=>p.y===null).length,65);assert.match(profileSvg(fail),/No successful held fits/);
const broken=profile(api,'gamma',[-1,-2,-3]);assert.ok(broken.free.error);assert.ok(broken.points.every(p=>p.y===null));
mkdirSync('build/scratch/theory-viz',{recursive:true});writeFileSync('build/scratch/theory-viz/numerical.json',JSON.stringify(results,null,2)+'\n');
console.log('PASS theory numerical: 5 static/live profiles, 4 escape messages and limit approaches, analytic normal held fits, static/live contours, parser and failed-fit gaps.');

assert.deepEqual(niceTicks(-70,-30),[-70,-60,-50,-40,-30]);
assert.deepEqual(niceTicks(.1,100,true),[.1,.2,.5,1,2,5,10,20,50,100].filter((v,i)=>i%2===0));
for(const [lo,hi] of [[-67.9,-36.09],[.003,.019],[-3,8],[0,2]]) {
 const ticks=niceTicks(lo,hi);assert.ok(ticks.length>=2&&ticks.length<=7);
 const step=ticks[1]-ticks[0],mantissa=step/10**Math.floor(Math.log10(step));assert.ok([1,2,5,10].some(m=>Math.abs(m-mantissa)<1e-10));
 assert.ok(ticks.every(t=>t>=lo-1e-12&&t<=hi+1e-12));
}
const c=conditional(api);assert.ok(c.free.fit);assert.ok(c.free.fit.estimates.b>0&&c.free.fit.estimates.b<2);assert.ok(c.held[0].fit&&c.held[1].fit);assert.match(c.held[2].error,/mlexp/);
assert.ok(html.includes(conditionalSvg(c)),'conditional static/live SVG equality');
const sum=c.sample.reduce((s,x)=>s+x,0),sum2=c.sample.reduce((s,x)=>s+x*x,0);
assert.ok(sum<2*sum2/2,'held-a dossier criterion');
for(const h of c.held){
 assert.equal(h.points[0].y,c.sample.length*Math.log(h.a)-h.a*sum);
 if(h.fit){assert.ok(h.fit.estimates.b>0&&h.fit.estimates.b<c.xmax);assert.ok(h.fit.logLik<=c.free.fit.logLik+1e-8);assert.ok(h.points.every(p=>p.y<=h.fit.logLik+1e-8));}
 else {assert.ok(h.points.slice(1).every(p=>p.y<h.points[0].y));}
}
assert.ok(html.includes('<math'));assert.ok(!/<script[^>]*(mathjax|katex)/i.test(html));
console.log('PASS nice linear/log ticks; Gompertz free/held fits, conditional escape, fixed-rate boundary, static/live equality and build-time MathML.');
