import create from '../../../../bindings/js/index.mjs';
const api=await create();
for(const [family,x,held] of [
 ['ztbinom',[1,1,1,2,3,7,12],{}],
 ['zmbinom',[0,0,1,1,1,2,3,7,12],{p0:.2}],
 ['zib',[0,0,0,0,0,0,0,0,1,1,1,2,3,7,12],{pi:.2}],
 ['tbinom',[1,2,3],{}]
]) {
 try { console.log(JSON.stringify({family,fit:api[family].fit(new Float64Array(x),held)})); }
 catch(e){ console.log(JSON.stringify({family,kind:e.kind,model:e.model,upper:e.loglik_upper,limit_loglik:e.limit_loglik,candidates:e.candidates?.length,message:e.message})); }
}
