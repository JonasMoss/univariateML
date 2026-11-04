inline InvBurrResult invburr_before(span x, double tol, int maxit,
    double shape10 = std::numeric_limits<double>::quiet_NaN(),
    double shape20 = std::numeric_limits<double>::quiet_NaN(),
    double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {
  std::vector<double> reciprocal;
  double jacobian = 0.0;
  for (double v : x) {
    if (!(v > 0.0) || !std::isfinite(v) || !std::isfinite(1.0/v)) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,nan,0,InvBurrStatus::InvalidInit};
    }
    reciprocal.push_back(1.0/v); jacobian -= 2.0*std::log(v);
  }
  if (x.size() > 1 && std::isnan(shape20)) {
    // The old inverse two-shape fit is an optional start, never a constraint.
    double center = 0.0;
    for (double v : x) center += std::log(v)/static_cast<double>(x.size());
    std::vector<double> normalized;
    bool representable = true;
    for (double v : x) {
      normalized.push_back(std::exp(std::log(v)-center));
      if (!(normalized.back() > 0.0) || !std::isfinite(normalized.back())) representable = false;
    }
    if (representable) {
      InvBurrCost fixed{span{normalized.data(),normalized.size()}};
      BfgsControl ctl; ctl.maxit=maxit; ctl.grad_tol=tol; ctl.reltol=reltol;
      const double start[2] = {1.0,2.0};
      auto seed = bfgs_minimize(fixed,start,ctl);
      if (std::isfinite(seed.f) && seed.theta[1]>0.0) shape20=seed.theta[1];
    }
  }
  auto r = burr_mle_legacy(span{reciprocal.data(),reciprocal.size()},tol,maxit,
    shape10,shape20,std::isnan(scale0) ? scale0 : 1.0/scale0,reltol);
  InvBurrStatus status = InvBurrStatus::Ok;
  if (r.status == BurrStatus::WeibullBoundary) status = InvBurrStatus::InvWeibullBoundary;
  if (r.status == BurrStatus::Unbounded) status = InvBurrStatus::Unbounded;
  if (r.status == BurrStatus::ParetoBoundary) status = InvBurrStatus::PowerBoundary;
  if (r.status == BurrStatus::InvalidInit) status = InvBurrStatus::InvalidInit;
  if (r.status == BurrStatus::IterationFailure || r.status == BurrStatus::CertificateTie ||
      r.status == BurrStatus::CertificateBudget || r.status == BurrStatus::NumericalFailure) status = InvBurrStatus::IterationFailure;
  if (status == InvBurrStatus::InvWeibullBoundary) {
    const auto face = invweibull_mle(x,1e-10,std::max(maxit,100));
    return {INFINITY,face.shape,face.scale,face.logLik,r.iterations,status};
  }
  if (status == InvBurrStatus::PowerBoundary) {
    const auto face = power_mle(x,0.0);
    return {0.0,INFINITY,face.alpha,face.logLik,r.iterations,status};
  }
  return {r.shape1,r.shape2,1.0/r.scale,r.logLik+jacobian,r.iterations,status};
}

