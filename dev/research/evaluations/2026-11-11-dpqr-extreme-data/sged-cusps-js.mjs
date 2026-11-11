import create from '../../../../bindings/js/index.mjs';
import {writeFileSync} from 'node:fs';
const api=await create();const data=[-3,-1,-.2,0,.3,.4,2],rows=[];
for(const nu of [.3,.5,.9])for(const xi of [.3,1.3,3]){
 const fit=api.sged.fit(new Float64Array(data),{mean:.1,nu,xi});
 const dpqr=Array.from(api.sged.d(new Float64Array(data),[.1,fit.estimates.sd,nu,xi],{log:true})).reduce((a,b)=>a+b,0);
 rows.push({mean:.1,sd:fit.estimates.sd,nu,xi,reported:fit.logLik,dpqr,status:fit.status});
}
writeFileSync(new URL('./sged-cusp-js-raw.json',import.meta.url),JSON.stringify(rows,null,2)+'\n');
