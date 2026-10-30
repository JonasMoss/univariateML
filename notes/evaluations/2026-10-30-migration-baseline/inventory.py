"""Source inventory: no truth/golden regeneration. Run from repository root."""
import collections,json,pathlib,re
import yaml
root=pathlib.Path("cppunivariate/include/cppunivariate/distributions")
out=pathlib.Path("notes/evaluations/2026-10-30-migration-baseline")
counts=collections.Counter()
rows=["family\ttruth_values\ttruth_quantiles\ttruth_parameter_sets\tgolden_fixtures\tgolden_observations"]
holds=[]; limits=[]; permitted=[]; boundaries=[]
for d in sorted(root.iterdir()):
    if not (d/"README.md").exists(): continue
    meta=yaml.safe_load((d/"README.md").read_text().split("---",2)[1])
    t=json.loads((d/"truth.json").read_text()) if (d/"truth.json").exists() else {}
    g=json.loads((d/"golden.json").read_text()) if (d/"golden.json").exists() else {}
    rows.append("\t".join(map(str,[d.name,len(t.get("values",[])),len(t.get("quantiles",[])),len(t.get("parameters",[])),int(bool(g)),len(g.get("data",[]))])))
    counts["families"]+=1; counts["truth_files"]+=bool(t);counts["truth_values"]+=len(t.get("values",[]));counts["truth_quantiles"]+=len(t.get("quantiles",[]));counts["goldens"]+=bool(g)
    for h in meta.get("holds",[]) or []:
        counts["holds_"+h["status"]]+=1
        holds.append(d.name+" "+json.dumps(h,sort_keys=True))
    for lim in meta.get("limits",[]) or []:
        counts["limits"]+=1
        limits.append(d.name+" "+json.dumps(lim,sort_keys=True))
    text=(d/"test.cpp").read_text()
    for match in re.finditer(r'TEST_CASE\s*\((.*?)\)\s*\{',text,re.S):
        name=match.group(1)
        if "may_fail" in name:
            permitted.append(d.name+":"+str(text[:match.start()].count("\n")+1)+" "+name)
    # Deliberately broad nominations; not a claim that every nominated test is a boundary proof.
    for n,line in enumerate(text.splitlines(),1):
        if "TEST_CASE" in line and re.search(r'boundary|boundaries|endpoint|closed|attained|point law|point mass|size zero|zero shape|zero-size',line,re.I):
            boundaries.append(f"{d.name}/test.cpp:{n} {line.strip()}")
(out/"coverage.tsv").write_text("\n".join(rows)+"\n")
(out/"holds.log").write_text("\n".join(holds)+"\n")
(out/"limits.log").write_text("\n".join(limits)+"\n")
(out/"may-fail.log").write_text("\n".join(permitted) if permitted else "No may_fail decorators in per-family test.cpp files.\n")
(out/"boundary-candidates.log").write_text("\n".join(boundaries)+"\n")
(out/"coverage-summary.log").write_text(json.dumps(counts,indent=2)+"\n")
# Reviewed literal evidence at this revision. These are presence counts, not
# coverage of every boundary/intersection/tail. Each marker must still exist.
attained={
"bhatt":"dbhatt(0,0,1,0)", "binom":"rbinom(rng,5,1)",
"cmb":"rcmb(rng,5,1,2)", "cmp":"dcmp(2,lambda,0,log)",
"dunif":"ddunif(3,3,3,log)", "geom":"dgeom(1.0, 1.0, true)",
"gev":"dgev(5,2,3,-1,logarithm)", "gpd":"dgpd(5,2,3,-1,logarithm)",
"hyper":"const long double b = brute_ll({x}, N, m, k);",
"nhyper":"dnhyper(2,20,20,2,false)",
"nbinom":"dnbinom(x,0,.4,log)", "pig":"dpig(x,2,0,false)",
"skellam":"dskellam(0,0,0)", "tbinom":"rtbinom(rng,1,-1,6,5)",
"tlambda":"dtlambda(1,0,1,1)", "tpois":"qtpois(1,0,-INFINITY,INFINITY)",
"triang":"dtriang(0,0,1,0)", "zib":"dzib(0,0,.5,.3)",
"zinb":"dzinb(x,0,.4,.3,false)", "zipf":"qzipf(.5,2147483647,0)",
"zmbinom":"rzmbinom(rng,5,1,0)", "ztbinom":"qztbinom(0.3,5,1,true,false)"}
endpoint={
"gev":"dgev(5,2,3,-1,logarithm)", "gpd":"dgpd(5,2,3,-1,logarithm)",
"tnorm":"dtnorm(x,0,1,-1,1,log)", "triang":"dtriang(0,0,1,0)",
"power":"dpower(3,3,2,false)", "nsbeta":"dnsbeta(2,1,3,2,5,true)",
"genbeta":"dgenbeta(3,2,1,2,3,false)", "hcauchy":"dhcauchy(0,0,2,false)"}
qonly={
"fatigue":"qfatigue(-INFINITY,1,1,0,true,true)",
"fpareto":"qfpareto(0,20,2,20,1,0,true,false)",
"frechet":"qfrechet(-INFINITY,2,3,1,true,true)",
"hnorm":"qhnorm(cppunivariate::special::kNegInf",
"ht":"qht(0.0, mu, 3.0, 2.0, true, false)",
"pareto":"qpareto(-INFINITY,4,3,true,true)",
"pareto2":"qpareto2(-INFINITY, 2, 3, 4, true, true)",
"pareto3":"qpareto3(-INFINITY, 4, 3, 4, true, true)",
"pareto4":"qpareto4(-INFINITY, 2, 4, 3, 4, true, true)",
"unif":"qunif(-INFINITY,0,1,true,true)"}
fitter_only={
"foldnorm":"CHECK(r.sigma==doctest::Approx(std::sqrt(.2)))",
"pois":"CHECK(r.logLik == 0.0)", "sgomp":"CHECK(f.b==doctest::Approx(5./11))",
"zip":"CHECK(zero.logLik == 0.0)", "ztpois":"CHECK(one.logLik==0)",
"ztgeom":"CHECK(one.logLik==0)", "zmpois":"CHECK(one.logLik==0)",
"zmgeom":"CHECK(one.logLik==0)", "zmlogarithmic":"CHECK(one.logLik==0)",
"ztnbinom":"CHECK(one.logLik==0)", "zmnbinom":"CHECK(one.logLik==0)"}
evidence=["category\tfamily\tsource\tassertion (may continue on following lines)"]
for category,mapping in [("attained parameter",attained),("R7a density",endpoint),("R7a quantile only",qonly),("fitter closed form only",fitter_only)]:
 for id,marker in sorted(mapping.items()):
  lines=(root/id/"test.cpp").read_text().splitlines()
  matches=[(i+1,l.strip()) for i,l in enumerate(lines) if marker in l]
  assert matches,(id,marker)
  n,line=matches[0]
  evidence.append(f"{category}\t{id}\t{id}/test.cpp:{n}\t{line}")
(out/"boundary-evidence.tsv").write_text("\n".join(evidence)+"\n")
counts.update(attained_parameter_families=len(attained),R7a_density_families=len(endpoint),R7a_quantile_only_families=len(qonly),boundary_evaluation_union=len(set(attained)|set(endpoint)|set(qonly)),fitter_only_families=len(fitter_only),boundary_union=len(set(attained)|set(endpoint)|set(qonly)|set(fitter_only)))
(out/"coverage-summary.log").write_text(json.dumps(counts,indent=2)+"\n")
# SHA256 provenance for the fixtures and tests, without trusting file mtimes.
import hashlib
manifest=[]
for d in sorted(root.iterdir()):
 for name in ("README.md","test.cpp","truth.json","golden.json"):
  p=d/name
  if p.is_file(): manifest.append(hashlib.sha256(p.read_bytes()).hexdigest()+"  "+str(p))
(out/"source-sha256.log").write_text("\n".join(manifest)+"\n")
# Runtime quality coverage, when the requested all-family run has produced it.
qroot=pathlib.Path("build/quality")
if (qroot/"report.md").is_file():
    results=[json.loads((qroot/(d.name+".json")).read_text())
             for d in sorted(root.iterdir()) if (d/"README.md").is_file()
             and (qroot/(d.name+".json")).is_file()]
    lines=[]; stats=collections.Counter()
    for r in results:
        stats["families"]+=1
        stats["passed"]+=bool(r.get("passed"))
        for key in ("logLik","score_hessian","oracle"):
            stats[key+"_"+r.get(key,{}).get("status","unchecked")]+=1
        details={k:r.get(k) for k in ("skipped_parameters","derivative_held_parameters")}
        details["score_hessian_reason"]=r.get("score_hessian",{}).get("reason")
        details["one_sided"]=r.get("score_hessian",{}).get("one_sided")
        details["oracle_reason"]=r.get("oracle",{}).get("reason")
        lines.append(r["id"]+" "+json.dumps(details,sort_keys=True))
    (out/"quality-coverage.log").write_text("\n".join(lines)+"\n")
    (out/"quality-summary.log").write_text(json.dumps(stats,indent=2)+"\n")

print(json.dumps(counts))
