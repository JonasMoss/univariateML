import create from '../../../../bindings/js/index.mjs';
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
const api=await create();
const x=new Float64Array([Number.MAX_VALUE]),p=[1,1e-8];
const actual=[api.cmp.d(x,p,{log:true})[0],api.cmp.p(x,p,{lower:false,log:true})[0]];
for(const value of actual){assert.ok(Number.isFinite(value));assert.ok(value>-1.3e303 && value<-1.2e303);}
writeFileSync(new URL('./cmp-far-js.json',import.meta.url),JSON.stringify({actual},null,2)+'\n');
console.log('CMP far log weights, JS:',actual);
const bnbinom=Array.from(api.bnbinom.d(new Float64Array([10000,1e6]),[1e8,1e16,1e8],{log:true}));
for(const [v,r] of bnbinom.map((v,i)=>[v,[-82108.92817013602,-12805552.582318537][i]])) assert.ok(Math.abs(v-r)<1e-10*Math.abs(r));
const pig=api.pig.p(new Float64Array([2.0004e19]),[10,.1],{lower:false,log:true})[0];
assert.ok(Number.isFinite(pig) && Math.abs(pig+9999500333187624)<1e-10*9999500333187624);
writeFileSync(new URL('./cmp-far-js.json',import.meta.url),JSON.stringify({actual,bnbinom,pig},null,2)+'\n');
console.log('Late BNB and PIG coordinates, JS:',bnbinom,pig);
