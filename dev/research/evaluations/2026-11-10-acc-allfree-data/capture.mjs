import {readFileSync} from 'node:fs';
import create from '../../../../build/js/univariateML.mjs';
const m=await create();
for(const id of ['slash','sstd','sged','st','bnbinom','gb2']) {
 const g=JSON.parse(readFileSync(new URL(`../../../../univariateML-cpp/include/cppunivariate/distributions/${id}/golden.json`,import.meta.url)));
 const r=m['fit'+id](new Float64Array(g.data),{},{});
 console.log(JSON.stringify({id,estimates:r.estimates,logLik:r.logLik,error:r.error||''}));
}
