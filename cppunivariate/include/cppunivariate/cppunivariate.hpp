// Aggregator header for the cppunivariate library. Pulls in every
// per-distribution estimator plus the shared core headers.
//
// Per-distribution dossiers live under distributions/<id>/; this file
// includes their mle.hpp. Distributions whose MLE is delegated to
// another estimator at the R-side wrapper level (e.g. lgamma reuses
// gamma_mle on log-transformed data) have no mle.hpp and are not
// included here.
//
// dpqr.hpp and metadata.hpp are not included by the aggregator; they
// are pulled in on demand by their R-side wrapper or any C++ consumer
// that needs them.

#pragma once

#include "core/inverse_digamma.hpp"
#include "core/lambert_w.hpp"
#include "core/newton.hpp"
#include "core/reductions.hpp"
#include "core/span.hpp"
#include "special/special.hpp"

#include "distributions/bbinom/mle.hpp"
#include "distributions/beta/mle.hpp"
#include "distributions/betapr/mle.hpp"
#include "distributions/binom/mle.hpp"
#include "distributions/burr/mle.hpp"
#include "distributions/cauchy/mle.hpp"
#include "distributions/dunif/mle.hpp"
#include "distributions/exp/mle.hpp"
#include "distributions/fatigue/mle.hpp"
#include "distributions/gamma/mle.hpp"
#include "distributions/ged/mle.hpp"
#include "distributions/geom/mle.hpp"
#include "distributions/gompertz/mle.hpp"
#include "distributions/gumbel/mle.hpp"
#include "distributions/hnorm/mle.hpp"
#include "distributions/invburr/mle.hpp"
#include "distributions/invexp/mle.hpp"
#include "distributions/invgamma/mle.hpp"
#include "distributions/invpareto/mle.hpp"
#include "distributions/invparalogis/mle.hpp"
#include "distributions/invgauss/mle.hpp"
#include "distributions/invweibull/mle.hpp"
#include "distributions/kumar/mle.hpp"
#include "distributions/laplace/mle.hpp"
// lgamma: no mle.hpp; uses gamma_mle via R-side log-transform.
#include "distributions/lgser/mle.hpp"
#include "distributions/llogis/mle.hpp"
#include "distributions/lnorm/mle.hpp"
#include "distributions/logis/mle.hpp"
#include "distributions/logitnorm/mle.hpp"
#include "distributions/lomax/mle.hpp"
#include "distributions/naka/mle.hpp"
#include "distributions/nbinom/mle.hpp"
#include "distributions/norm/mle.hpp"
#include "distributions/paralogis/mle.hpp"
#include "distributions/pareto/mle.hpp"
#include "distributions/pois/mle.hpp"
#include "distributions/power/mle.hpp"
#include "distributions/rayleigh/mle.hpp"
#include "distributions/sged/mle.hpp"
#include "distributions/sn/mle.hpp"
#include "distributions/snorm/mle.hpp"
#include "distributions/sstd/mle.hpp"
#include "distributions/std/mle.hpp"
#include "distributions/unif/mle.hpp"
#include "distributions/weibull/mle.hpp"
#include "distributions/zip/mle.hpp"
#include "distributions/zipf/mle.hpp"

#include "distributions/lst/mle.hpp"

#include "distributions/zinb/mle.hpp"
#include "distributions/gpd/mle.hpp"
#include "distributions/triang/mle.hpp"
#include "distributions/huber/mle.hpp"
#include "distributions/ztpois/mle.hpp"
#include "distributions/ztgeom/mle.hpp"
#include "distributions/ztnbinom/mle.hpp"
#include "distributions/ztbinom/mle.hpp"
#include "distributions/zmpois/mle.hpp"
#include "distributions/zmgeom/mle.hpp"
#include "distributions/zmnbinom/mle.hpp"
#include "distributions/zmbinom/mle.hpp"
#include "distributions/zmlogarithmic/mle.hpp"
#include "distributions/zib/mle.hpp"
#include "distributions/hyper/mle.hpp"
#include "distributions/nhyper/mle.hpp"
#include "distributions/gev/mle.hpp"
#include "distributions/trgamma/mle.hpp"
#include "distributions/invtrgamma/mle.hpp"
#include "distributions/tnorm/mle.hpp"
#include "distributions/pig/mle.hpp"
#include "distributions/dweibull/mle.hpp"

#include "distributions/bnbinom/mle.hpp"
#include "distributions/cmb/mle.hpp"
#include "distributions/gb2/mle.hpp"
#include "distributions/sgomp/mle.hpp"

#include "distributions/cmp/mle.hpp"
#include "distributions/zeta/mle.hpp"
#include "distributions/yulesimon/mle.hpp"
#include "distributions/foldnorm/mle.hpp"
#include "distributions/emg/mle.hpp"

#include "distributions/hcauchy/mle.hpp"
#include "distributions/ht/mle.hpp"
#include "distributions/tpois/mle.hpp"
#include "distributions/tbinom/mle.hpp"
#include "distributions/dlaplace/mle.hpp"
#include "distributions/dnorm/mle.hpp"
#include "distributions/dgamma/mle.hpp"
