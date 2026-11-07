import {readFileSync} from 'node:fs';
import createApi from '../../../../bindings/js/index.mjs';
const api=await createApi();
const g=JSON.parse(readFileSync('univariateML-cpp/include/cppunivariate/distributions/bbinom/golden.json','utf8'));
for(const iterlim of [100,200,500,1000]){
 const start=performance.now();
 try{const fit=api.bbinom.fit(new Float64Array(g.data),{alpha:g.estimates.alpha},{iterlim});console.log(iterlim,'ok',fit.logLik,(performance.now()-start)/1000);}
 catch(error){console.log(iterlim,error.kind,error.message,(performance.now()-start)/1000);}
}
