// Compare the starting JS/C++ module with the final module on the same seeds.
import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath,pathToFileURL} from 'node:url';
import {execFileSync} from 'node:child_process';
const d=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(d,'../../../..');
const oldFile=path.join(root,'build/scratch/qtie-2-js-before.mjs');
fs.writeFileSync(oldFile,execFileSync('git',['show','c167a97580db656bba2e8153d970cd4de69c8f64:bindings/js/modifiers.mjs'],{cwd:root}));
const {modifierAPI:before}=await import(pathToFileURL(oldFile));
const {modifierAPI:after}=await import(pathToFileURL(path.join(root,'bindings/js/modifiers.mjs')));
const {modifiers}=await import(pathToFileURL(path.join(root,'bindings/js/modifier-metadata.mjs')));
const {coefficients}=await import(pathToFileURL(path.join(root,'bindings/js/families.mjs')));
const {default:create}=await import(pathToFileURL(path.join(root,'build/js/univariateML.mjs')));
const module=await create();
const {default:createBefore}=await import(pathToFileURL(path.join(root,'build/js/baseline/univariateML.mjs')));
const baseline=await createBefore();
const cases=JSON.parse(fs.readFileSync(path.join(d,'cases.json'),'utf8'));
const reached=(v,p,l,g)=>{const e=64*Number.EPSILON,t=g?(l?(p>-Number.MAX_VALUE/(1+e)?p*(1+e):p):p*(1-e)):l?p*(1-e):1-p>4*e?p*(1+e):p;return l?v>=t:v<=t;};
const rows=[],exclusions=[],samplers=[];
for(const [i,c] of cases.entries()) {
 console.error(c.id,'point',c.point);
 const flags={...c.config};
 if(flags.truncate)flags.truncate=flags.truncate.map(v=>v==='Inf'?Infinity:Number(v));
 const p=coefficients[c.base].map(n=>c.args[n]);
 if(flags.zero==='modified')p.push(c.args.p0);if(flags.zero==='inflated')p.push(c.args.pi);
 // The original C++/R supplemental grids cover this costly old inverse.
 const remoteBinomial=c.base==='binom'&&p[0]>=1e7&&flags.truncate?.[0]===2&&flags.truncate?.[1]===Infinity;
 const meta=modifiers[c.base],old=before(baseline,c.base,meta.eligibility,meta.capabilities),now=after(module,c.base,meta.eligibility,meta.capabilities);
 const minimum=flags.truncate?Math.max(c.base==='zeta'?1:0,Math.ceil(flags.truncate[0])):flags.zero==='truncated'?1:c.base==='lgser'&&flags.zero!=='modified'?1:0;
 try {if(Number.isNaN(old.evaluate('d',[minimum],p,flags)[0]))throw new Error('NaN density: invalid conditioning event');}
 catch(error){exclusions.push({id:c.id,point:c.point,error:error.message});continue;}
 for(const lower of [true,false])for(const log of [false,true]) {
  const width=remoteBinomial?0:c.id.includes('-window-')?2:100,k=Array.from({length:2*width+1},(_,j)=>(remoteBinomial?2:c.centre)+j-width),f={...flags,lower,log};
  const pv=old.evaluate('p',k,p,f),prev=old.evaluate('p',k.map(v=>v-1),p,f),eligible=k.map((_,j)=>j).filter(j=>(log?Number.isFinite(pv[j])&&pv[j]<0:pv[j]>0&&pv[j]<1)&&pv[j]!==prev[j]);
  const targets=eligible.map(j=>pv[j]);
  for(const [stage,api] of [['before',old],['after',now]]) {
   const q=api.evaluate('q',targets,p,f),at=api.evaluate('p',q,p,f),left=api.evaluate('p',Array.from(q,v=>v-1),p,f);
   let up=0,earlier=0,invalid=0,crossing=0;
   for(const [j,index] of eligible.entries()) {
    if(!Number.isFinite(q[j]))invalid++;else if(q[j]>k[index])up++;else if(q[j]<k[index])earlier++;
    if(!reached(at[j],targets[j],lower,log)||reached(left[j],targets[j],lower,log))crossing++;
   }
   rows.push({id:c.id,point:c.point,stage,lower,log,n:eligible.length,up,earlier,invalid,crossing_failure:crossing});
  }
 }
 if(flags.truncate||flags.zero&&flags.zero!=='none') {
  const n=remoteBinomial?1:c.id.includes('-window-')?16:512,u=baseline.runif(n,[0,1],215+i),current=module.runif(n,[0,1],215+i);
  if(Array.from(u,(_,j)=>j).some(j=>!Object.is(u[j],current[j])))throw new Error('Uniform source changed');
  const a=old.evaluate('q',u,p,flags),b=now.sampleQuantile(current,p,flags);
  const differences=Array.from(a,(_,j)=>j).filter(j=>!Object.is(a[j],b[j]));
  samplers.push({id:c.id,point:c.point,seed:215+i,n,invalidBefore:Array.from(a).filter(v=>!Number.isFinite(v)).length,invalidAfter:Array.from(b).filter(v=>!Number.isFinite(v)).length,differences: differences.map(j=>({index:j,u:u[j],before:a[j],after:b[j]}))});
 } else {
  const n=512,a=baseline[`r${c.base}`](n,p,215+i),b=module[`r${c.base}`](n,p,215+i);
  const differences=Array.from(a,(_,j)=>j).filter(j=>!Object.is(a[j],b[j]));
  samplers.push({id:c.id,point:c.point,seed:215+i,n,invalidBefore:Array.from(a).filter(v=>!Number.isFinite(v)).length,invalidAfter:Array.from(b).filter(v=>!Number.isFinite(v)).length,differences:differences.map(j=>({index:j,before:a[j],after:b[j]}))});
 }
}
fs.writeFileSync(path.join(d,'js-composition-counts.json'),JSON.stringify(rows,null,2)+'\n');
fs.writeFileSync(path.join(d,'js-composition-exclusions.json'),JSON.stringify(exclusions,null,2)+'\n');
fs.writeFileSync(path.join(d,'js-sampler-layer-comparison.json'),JSON.stringify(samplers,null,2)+'\n');
for(const stage of ['before','after']) {
 const r=rows.filter(v=>v.stage===stage),sum=key=>r.reduce((n,v)=>n+v[key],0);
 console.log(stage,Object.fromEntries(['n','up','earlier','invalid','crossing_failure'].map(k=>[k,sum(k)])));
}
console.log('samplers',samplers.reduce((n,v)=>n+v.n,0),'different',samplers.reduce((n,v)=>n+v.differences.length,0));
if(rows.some(v=>v.stage==='after'&&(v.up||v.invalid||v.crossing_failure)) || samplers.some(v=>v.differences.length))process.exitCode=1;
