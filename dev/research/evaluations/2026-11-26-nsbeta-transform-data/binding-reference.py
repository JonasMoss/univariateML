"""Check diffuse fits and missing-endpoint dispatch with the snapshot probe."""
import json
import subprocess
import sys

import mpmath as mp


def fit(probe, sample, mask):
    values = (1., 1., -2.**50, 1.) if mask != 12 else (0., 0., 0., 1.)
    tokens = [str(mask), str(len(sample)), *(v.hex() for v in (*values, *sample))]
    return json.loads(subprocess.check_output([probe], input=" ".join(tokens)+"\n", text=True))


def root(sample):
    x = list(map(mp.mpf, sample))
    lx = mp.fsum(map(mp.log, x))/len(x)
    lc = mp.fsum(mp.log1p(-v) for v in x)/len(x)
    return mp.findroot(lambda a,b: (mp.digamma(a+b)-mp.digamma(a)+lx,
                                   mp.digamma(a+b)-mp.digamma(b)+lc),
                       (.0027,.085))


if __name__ == "__main__":
    print("mode,case,order,status,shape1_ref,shape2_ref,max_relative_error")
    for arg in sys.argv[1:]:
        mode, probe = arg.split("=", 1)
        for tiny in (1e-320, 1e-300):
            for sample in ((tiny,.5),(.5,tiny)):
                result = fit(probe,sample,12)
                assert result["kind"] == "ok", result
                with mp.workdps(100):
                    low = [mp.nstr(v,45) for v in root(sample)]
                with mp.workdps(150):
                    ref = root(sample)
                    assert low == [mp.nstr(v,45) for v in ref]
                    a,b = (float.fromhex(result["estimates"][k]) for k in ("shape1","shape2"))
                    err = max(abs(a/ref[0]-1),abs(b/ref[1]-1))
                    assert err < mp.mpf("1e-10")
                    print(mode, tiny, "forward" if sample[0]==tiny else "reversed",
                          result["kind"], *low, mp.nstr(err,8), sep=",")
        for mask in range(12):
            result = fit(probe,(0.,.125),mask)
            assert result["reason"] == "unsupported", result
            print(mode,"missing-endpoints",mask,result["reason"],"","","",sep=",")
