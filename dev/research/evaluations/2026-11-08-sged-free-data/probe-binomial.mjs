import createApi from '../../../../bindings/js/index.mjs';
const api=await createApi();
for(const [family,x,held] of [
 ['ztbinom',[1,1,1,2,3,7,12],{}],
 ['zmbinom',[0,0,1,1,1,2,3,7,12],{p0:.2}],
 ['zib',[0,0,0,0,0,0,0,0,1,1,1,2,3,7,12],{pi:.2}],
 ['tbinom',[1,2,3],{}]
]) {
 try {const result=api[family].fit(new Float64Array(x),held);console.log(JSON.stringify({family,result}));}
 catch(e){console.log(JSON.stringify({family,kind:e.kind,message:e.message,model:e.model,candidates:e.candidates,estimates:e.estimates,loglik_lower:e.loglik_lower,loglik_upper:e.loglik_upper}));}
}
