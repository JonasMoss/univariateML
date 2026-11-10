import create from '../../../../bindings/js/index.mjs';
const api=await create(),u=Number.MIN_VALUE;
for(const [id,x,held] of [['gumbel',[0,u],{}],['weibull',[u,2*u],{}],['foldnorm',[0,u],{}],['pig',[0,2],{mean:1e-158}]]) {
 try{console.log(JSON.stringify({id,result:api[id].fit(x,held)}));}
 catch(e){console.log(JSON.stringify({id,kind:e.kind,message:e.message}));}
}
const e=.0028867513459481287;
console.log(JSON.stringify({id:'foldnorm-seam',result:api.foldnorm.fit([e,e,1])}));
