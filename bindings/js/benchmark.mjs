import {readFileSync,writeFileSync,statSync} from 'node:fs';
import {performance} from 'node:perf_hooks';
import assert from 'node:assert/strict';
const times=[];const path=new URL('../../build/js/O2.mjs',import.meta.url);
let m;
for(let i=0;i<11;i++){const start=performance.now();const create=(await import(`${path}?trial=${i}`)).default;m=await create();times.push(performance.now()-start);}
const fits={};
for(const f of ['exp','gamma','gb2']){const g=JSON.parse(readFileSync(new URL(`../../univariateML-cpp/include/cppunivariate/distributions/${f}/golden.json`,import.meta.url))),x=new Float64Array(g.data),n=f==='gb2'?3:10000;
 assert.equal(m[`fit${f}`](x,{},{}).status,'Ok');const start=performance.now();let r;
 for(let i=0;i<n;i++)r=m[`fit${f}`](x,{},{});assert.equal(r.status,'Ok');fits[f]={repetitions:n,ms:(performance.now()-start)/n,logLik:r.logLik};
}
const result={node:process.version,load_ms:times, fits};console.log(JSON.stringify(result,null,2));writeFileSync(new URL('../../build/scratch/wasm-benchmark.json',import.meta.url),JSON.stringify(result,null,2));
