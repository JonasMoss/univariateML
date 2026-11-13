// First computed crossing, on the integer/binary64 lattice. Sampling omits
// the public band; the caller supplies the already conditioned probability.
function discreteInverse(values, minimum, maximum, upperEndpoint, lower, log, fuzz, probability, firstDensity) {
  if(!values.length)return new Float64Array();
  const e=64*Number.EPSILON;
  const adjusted=p=>log?(lower?(p>-Number.MAX_VALUE/(1+e)?p*(1+e):p):p*(1-e)):
    lower?p*(1-e):(1-p>4*e?p*(1+e):p);
  let first;
  try {first=probability(minimum);} catch(error) {
    if(!/Conditional interval probability cannot be resolved/.test(error.message))throw error;
    const mass=Math.min(0,firstDensity());
    const tail=lower?mass:(mass<-Math.LN2?Math.log1p(-Math.exp(mass)):Math.log(-Math.expm1(mass)));
    first=log?tail:Math.exp(tail);
  }
  return Float64Array.from(values,p=>{
    if(Number.isNaN(p)||(log?p>0:p<0||p>1))return NaN;
    if(p===(log?-Infinity:0))return lower?minimum:upperEndpoint;
    if(p===(log?0:1))return lower?upperEndpoint:minimum;
    const target=fuzz?adjusted(p):p;
    const reached=k=>{
      let value;
      try {value=k===minimum?first:probability(k);} catch(error) {
        throw new Error(`Conditional quantile cannot be resolved at the required floating-point accuracy: ${error.message}`);
      }
      if(Number.isNaN(value))throw new Error('Conditional quantile probability evaluation failed');
      return lower?value>=target:value<=target;
    };
    if(reached(minimum))return minimum;
    let lo=minimum,hi=Math.min(maximum,Math.max(1,minimum));
    while(!reached(hi)) {
      lo=hi;
      const next=Math.min(maximum,Number.MAX_VALUE,Math.max(hi+1,2*hi+1));
      if(!Number.isFinite(next)||next<=hi)throw new Error('Conditional quantile cannot be resolved on the floating-point lattice');
      hi=next;
    }
    while(hi-lo>1) {
      const mid=lo+Math.floor((hi-lo)/2);
      if(!(mid>lo&&mid<hi))break;
      if(reached(mid))hi=mid;else lo=mid;
    }
    return hi;
  });
}

// Modifier operations use the same native seam as R.
export function modifierAPI(module, id, eligibility, capabilities) {
  const refusal = value => { throw new Error(`${value} is not implemented for \`ml${id}\`; see TASK-191 or its successor`); };
  const validate = (zero, truncate) => {
    if (!['none','truncated','modified','inflated'].includes(zero)) throw new Error('`zero` must be one of none, truncated, modified, inflated');
    if (zero !== 'none' && !eligibility.zero) throw new Error(`ml${id} has no argument zero`);
    if (zero !== 'none' && truncate) refusal('zero + truncate');
    if (zero !== 'none' && capabilities.zero?.[zero]?.status !== 'implemented') refusal(`\`zero = "${zero}"\``);
    if (truncate && capabilities.truncate?.status !== 'implemented') refusal('`truncate`');
    if (truncate && (!Array.isArray(truncate) || truncate.length !== 2 || truncate.some(v => typeof v !== 'number') || truncate[0] > truncate[1])) throw new Error('truncate requires a closed [lower, upper] window');
  };
  const variant = zero => zero === 'truncated' ? `zt${id}` : zero === 'modified' ? (id==='lgser'?'zmlogarithmic':`zm${id}`) : ({pois:'zip',binom:'zib',nbinom:'zinb',geom:'zinb'})[id];
  const parameters = (zero,p) => id==='geom' && zero==='inflated' ? [1,...p] : p;
  const complement=v=>v<-Math.LN2?Math.log1p(-Math.exp(v)):Math.log(-Math.expm1(v));
  const logdiff = (a,b) => b===-Infinity?a:a+complement(Math.min(0,b-a));
  const logadd = (a,b) => {const m=Math.max(a,b);return m===-Infinity?m:m+Math.log1p(Math.exp(Math.min(a,b)-m));};
  const evaluate = (kind,x,p,flags={}) => {
    const {zero='none',truncate=null,lower=true,log=false}=flags;
    validate(zero,truncate);
    if (zero!=='none') return module[`modifier_${kind}${variant(zero)}`](x,parameters(zero,p),lower,log);
    if(id==='zeta' && truncate) {
      if(Math.max(1,Math.ceil(truncate[0]))!==1 || !Number.isFinite(truncate[1]) || truncate[1]<1 || truncate[1]>2147483647) throw new Error('finite zeta window requires [1,N]');
      return module.zetaWindow(kind,x,Math.floor(truncate[1]),p[0],lower,log);
    }
    if (!truncate) return module[`${kind}${id}`](x,p,lower,log);
    if(truncate[0]===1 && truncate[1]===Infinity && ['pois','binom','nbinom','geom'].includes(id)) return module[`modifier_${kind}zt${id}`](x,p,lower,log);
    if(['pois','binom','norm'].includes(id)) {
      const a=id==='norm'?truncate[0]:Math.ceil(truncate[0])-1,b=id==='norm'?truncate[1]:Math.floor(truncate[1]);
      const params=id==='binom'?[p[1],a,b,p[0]]:[...p,a,b];
      return module[`modifier_${kind}t${id}`](x,params,lower,log);
    }
    if (truncate.some(Number.isNaN)) throw new Error('Evaluation requires known truncation bounds');
    if(eligibility.integer && Math.max(0,Math.ceil(truncate[0]))>=2**53) throw new Error('Conditional integer window cannot be resolved at the required floating-point accuracy');
    const integer=eligibility.integer, a=integer?Math.max(0,Math.ceil(truncate[0]))-1:truncate[0],b=integer?Math.floor(truncate[1]):truncate[1];
    const tail=(x,lower=true)=>module[`p${id}`]([x],p,lower,true)[0];
    const mass=(a,b)=> {const f=tail(b),lo=f<Math.log(.5),big=lo?f:tail(a,false),small=lo?tail(a):tail(b,false);if(b>a && Number.isFinite(big) && Number.isFinite(small) && small-big>-1e-4) throw new Error('Conditional interval probability cannot be resolved at the required floating-point accuracy');return logdiff(big,small);};
    const z=mass(a,b);
    if(!Number.isFinite(z)) throw new Error('Truncation event has zero or unresolved probability');
    if(64*Number.EPSILON*Math.max(1,Math.abs(z))>1e-10) throw new Error('Conditional normalization cannot be resolved at the required floating-point accuracy');
    if(kind==='q' && integer) {
      const probability=k=>evaluate('p',[k],p,{...flags,lower,log})[0];
      const density=()=>module[`d${id}`]([a+1],p,true,true)[0]-z;
      const end=Number.isFinite(b)?b:module[`q${id}`]([1],p,true,false)[0];
      return discreteInverse(x,a+1,b,end,lower,log,true,probability,density);
    }
    return Float64Array.from(x,v=>{
      if(kind==='d') {const d=v<truncate[0]||v>truncate[1]?-Infinity:module[`d${id}`]([v],p,true,true)[0]-z;return log?d:Math.exp(d);}
      if(kind==='p') {
        const k=Math.max(a,Math.min(b,integer?Math.floor(v):v));let result;
        if(k<=a)result=lower?-Infinity:0;
        else if(k>=b)result=lower?0:-Infinity;
        else {
          const lc=mass(a,k)-z,ls=mass(k,b)-z,fromLower=lc<=ls;
          result=lower?(fromLower?lc:complement(ls)):(fromLower?complement(lc):ls);
        }
        return log?result:Math.exp(result);
      }
      let lp=log?v:Math.log(v),ls=log?complement(v):Math.log1p(-v);
      if(!lower)[lp,ls]=[ls,lp];
      if(lp===-Infinity)return integer?a+1:a;if(ls===-Infinity)return Number.isFinite(b)?b:module[`q${id}`]([1],p,true,false)[0];
      if(lp<=module[`d${id}`]([a+1],p,true,true)[0]-z)return a+1;
      if(Number.isFinite(b) && ls<module[`d${id}`]([b],p,true,true)[0]-z)return b;
      const fa=tail(a),fb=tail(b),sa=tail(a,false),sb=tail(b,false);
      const lf=logadd(fa,lp+z),sf=logadd(sb,ls+z),useLower=lf<-Math.LN2;
      const prefix=useLower?(lp<=ls?fa:fb):(ls<=lp?sb:sa);
      if(Number.isFinite(prefix) && Math.min(lp,ls)+z-prefix<Math.log(1e-4)) throw new Error('Conditional quantile cannot be resolved at the required floating-point accuracy');
      return module[`q${id}`]([lf<Math.log(.5)?lf:sf],p,lf<Math.log(.5),true)[0];
    });
  };
  const sampleQuantile=(u,p,flags)=>{
    if(!eligibility.integer)return evaluate('q',u,p,flags);
    const {zero='none',truncate=null}=flags;
    validate(zero,truncate);
    if(id==='zeta'&&truncate) {
      if(Math.max(1,Math.ceil(truncate[0]))!==1 || !Number.isFinite(truncate[1]) || truncate[1]<1 || truncate[1]>2147483647) throw new Error('finite zeta window requires [1,N]');
      // Keep the native sampling inverse, including its historical Zipf CDF.
      return module.zetaWindow('r',u,Math.floor(truncate[1]),p[0],true,false);
    }
    const physical=id==='lgser'||id==='zeta'?1:0;
    const minimum=truncate?Math.max(physical,Math.ceil(truncate[0])):zero==='truncated'?1:zero==='none'?physical:0;
    const maximum=Math.min(id==='binom'?p[0]:Infinity,truncate?Math.floor(truncate[1]):Infinity);
    const probability=k=>evaluate('p',[k],p,{...flags,lower:true,log:false})[0];
    const density=()=>evaluate('d',[minimum],p,{...flags,log:true})[0];
    return discreteInverse(u,minimum,maximum,maximum,true,false,false,probability,density);
  };
  return {evaluate,sampleQuantile,fit(x,held,control) {
    const {zero='none',truncate=null}=control;validate(zero,truncate);
    if(control.reltol!==undefined && (!Number.isFinite(control.reltol) || control.reltol<=0) || control.iterlim!==undefined && (!Number.isInteger(control.iterlim) || control.iterlim<1 || control.iterlim>2147483647)) throw new Error('invalid modifier controls');
    if(control.abstol!==undefined && (!Number.isFinite(control.abstol) || control.abstol<=0)) throw new Error('invalid modifier controls');
    const censor=x && !ArrayBuffer.isView(x) && !Array.isArray(x) && 'lower' in x;
    if(censor && zero!=='none')refusal('censor + zero');
    if(censor && truncate)refusal('censor + truncate');
    const positiveWindow=truncate?.[0]===1 && truncate?.[1]===Infinity && ['pois','binom','nbinom','geom'].includes(id);
    const result=module.fitModifier(id,censor?x.lower:x,censor?x.upper:[],held,zero,!!truncate,truncate?.[0]??NaN,truncate?.[1]??NaN,!!censor,control.reltol??control.abstol??1e-10,control.iterlim??(truncate&&!positiveWindow&&id==='binom'&&held.size==null?512:truncate&&!positiveWindow||censor?400:200));
    if (!result.error) {
      result.zero=zero;result.truncate=truncate;result.truncate_spec=truncate;result.truncate_estimated=truncate?.map(Number.isNaN)??[];result.censored=!!censor;
      if (truncate?.some(Number.isNaN)) {result.truncate=[...truncate];const data=Array.from(x);if(Number.isNaN(result.truncate[0]))result.truncate[0]=Math.min(...data);if(Number.isNaN(result.truncate[1]))result.truncate[1]=Math.max(...data);}
    }
    return result;
  }};
}
