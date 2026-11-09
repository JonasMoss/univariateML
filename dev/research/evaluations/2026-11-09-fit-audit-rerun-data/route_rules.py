"""Route-level certificate attribution from current dossiers and executed holds.

Runtime flags are evidence when supplied; -1 denotes an absent result member.
For partial families without flags, the tests below encode the held-subset
scope in the current dossier and public fitter dispatch, not family-wide credit.
"""
def classify(case,meta,flags):
 f=case['family'];h=case['held'];ps=meta['params'];declared=str(meta.get('fit',{}).get('certified','no')).lower()
 runtime=flags.get(case['index'],[-1,-1])
 if len(h)==len(ps):return 'fixed evaluation','No free coordinate; only parameter payload and likelihood evaluation are checked.'
 if any(v==1 for v in runtime):return 'runtime certified','Observed certified/global_certified flag is true.'
 if declared=='yes':return 'declared certified','Current fit.certified=yes; analytic or certified route, with runtime flags '+repr(runtime)+'.'
 if declared!='partial':return 'uncertified numerical','Current dossier does not claim a global certificate for this route.'
 cert=False;basis='Current partial dossier leaves this held subset numerical.'
 if f in ['gb2','fpareto']:
  shapes=['shape1','shape2','shape3','scale'];cert=sum(n in h for n in shapes)>=2 or h.get('shape1')==1 or h.get('shape3')==1
  basis='GB2 early conditional/unit-shape delegates or two-coordinate concavity/profile certificates; the five zero/one-held larger mixed subsets retain numerical outer searches.'
 elif f=='genbeta':
  cert=any(n in h for n in ['shape1','shape2','shape3']);basis='Held power or a held beta shape is certified; the all-free three-shape outer profile is numerical.'
 elif f=='frechet':
  cert=not h or 'mu' in h or 'lambda' in h;basis='All-free joint cover, inverse-Weibull with held mu, or held-lambda monotone profile; sigma-only remains numerical outside special two-point members.'
 elif f=='gev':
  cert=not h or {'mu','sigma'}.issubset(h) or h.get('xi')==-1 or ('sigma' in h and 'xi' in h and -1<h['xi']<=0)
  basis='All-free joint cover; shape-only interval cover; held xi=-1 closed form; held sigma with -1<xi<=0 concave/analytic location. Other conditional searches are numerical outside special two-point certificates.'
 elif f in ['lst','std']:
  cert=not h or ('df' if f=='lst' else 'nu') in h
  basis='All-free exhaustive profile or held-shape scalar/joint certificate; free-shape held profiles generally remain numerical.'
 elif f=='sstd':
  cert=({'nu','xi'}.issubset(h) and ('mean' in h or 'sd' in h)) or ({'nu','mean','sd'}.issubset(h)) or (h.get('xi')==1 and len(h)==1)
  basis='Held nu/xi scalar certificates, held nu plus both moments skew cover, or exact symmetric all-free std delegate; other subsets remain numerical.'
 elif f in ['st','sged','tlambda']:
  # These fitters carry the route flag through certificate dispatch.
  cert=False;basis='This partial fitter exposes certified; the observed finite route did not set it (flags '+repr(runtime)+'). The dossier lists the certified subroutes separately.'
 elif f=='bhatt':
  cert=bool(h);basis='Held positive a, held sigma, or held mu certificates; all-free numerical search.'
 elif f=='bnbinom':
  cert=bool(h);basis='All one-/two-free-shape subsets have scalar or rectangle/face certificates; all-free finite fits remain multistart.'
 elif f in ['zib','zmbinom','ztbinom','tbinom']:
  cert=True;basis='Finite Ok uses exact conditional profiles or a complete integer prefix with tail exclusion. Partial refers to unresolved faces that must stop, not permission for an unchecked finite winner.'
 else:
  basis='Certification attribution open: this partial family is not in the reviewed dispatch map.'
 return ('conditional certified' if cert else 'uncertified numerical'),basis
