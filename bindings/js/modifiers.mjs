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
  return {evaluate,fit(x,held,control) {
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
