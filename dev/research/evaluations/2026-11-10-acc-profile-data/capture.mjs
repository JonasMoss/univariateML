import {readFileSync} from 'node:fs';
const {default:create}=await import(process.env.UMVL_CAPTURE_MODULE || new URL('../../../../build/js/univariateML.mjs',import.meta.url));
const m=await create();
const ids=new Set('sn skellam sgomp dgamma pareto3 zinb zmnbinom ztnbinom ht emg genbeta'.split(' '));
const cases=JSON.parse(readFileSync(process.env.UMVL_CAPTURE_CASES || new URL('../../../../build/js/generated/fit-cases.json',import.meta.url)),(_,v)=>v==='Inf'?Infinity:v==='-Inf'?-Infinity:v);
for(const c of cases.filter(c=>ids.has(c.id))) {
 try {
  const r=m['fit'+c.id](new Float64Array(c.g.data),c.held,c.control);
  console.log(JSON.stringify({id:c.id,estimates:r.estimates,logLik:r.logLik,error:r.error||''},(_,v)=>v===Infinity?'Inf':v===-Infinity?'-Inf':v));
 } catch(e) {console.log(JSON.stringify({id:c.id,estimates:{},error:String(e)}));}
}
