#pragma once
#include "../beta/mle.hpp"

namespace cppunivariate { namespace nsbeta_detail {

inline double increment(double free, double held) {
  return held==1 ? 1/free : beta_detail::psi_increment(free,held);
}

inline bool score_sign(double free, double held, double target, double sign) {
  const double eps = std::numeric_limits<double>::epsilon();
  const double direct = held!=1 && free<20 && held>=.01*free ?
      std::abs(special::digamma(free+held))+std::abs(special::digamma(free)) : 0;
  const double margin = eps*(128*target+16*direct);
  return sign*(increment(free,held)-target)>margin;
}

// For a diffuse fit, the ordinary shape scores can enclose a candidate
// directly. Each score increases with the other shape, so the two diagonal
// corners suffice to prove that the gradient points inward on all four faces.
inline bool shape_box(span x, double lo, double hi, double a, double b, double tol) {
  if (!(a>0 && b>0 && std::isfinite(a+b))) return false;
  transformed_detail::Sum left, right;
  for (double v : x) {
    const auto logs = special::interval_log_coordinates(v,lo,hi);
    left.push(-logs.lower);
    right.push(-logs.upper);
  }
  const double first = left.get()/x.size(), second = right.get()/x.size();
  if (!(first>0 && second>0)) return false;
  const double low = 1-tol/2, high = 1+tol/2;
  return score_sign(a*low,b*low,first,1) && score_sign(b*low,a*low,second,1) &&
         score_sign(a*high,b*high,first,-1) && score_sign(b*high,a*high,second,-1);
}

// Keep the original separation x-anchor in the two relative deviations.
// Subtracting two rounded affine coordinates would lose that separation.
inline BetaResult joint_fit(span x, double lo, double hi, double tol, int maxit,
                            double initial_precision) {
  const double anchor = x[0];
  const double scale = std::isfinite(hi-lo) ? 1 : .5;
  const double left = scale*anchor-scale*lo, right = scale*hi-scale*anchor;
  const double width = scale*hi-scale*lo, p = left/width, c = right/width;
  if (!(p>0 && c>0)) return {NAN,NAN,NAN,0,BetaStatus::IterationFailure};
  const auto anchor_logs = special::interval_log_coordinates(anchor,lo,hi);
  const double count = static_cast<double>(x.size());
  const double lower = std::log(std::numeric_limits<double>::min());
  const double upper = std::log(std::numeric_limits<double>::max());
  const auto budget = solver_budget(maxit);
  double t = initial_precision;
  std::int64_t work = 0;

  // eta is logit(mu) minus the anchor's logit. Keeping the offset small
  // permits mean changes much smaller than an ulp of the absolute logit.
  const auto evaluate = [&](double eta, double& mu, double& mc, double& kl, double& gap, double& gap_scale) {
    const double shift_left = c*std::expm1(-eta), shift_right = p*std::expm1(eta);
    const double ratio_left = 1+shift_left, ratio_right = 1+shift_right;
    mu = p/ratio_left;
    mc = c/ratio_right;
    if (!(mu>0 && mc>0 && mu<=1 && mc<=1)) return false;
    transformed_detail::Sum divergence, angular, magnitude;
    for (double v : x) {
      const double offset = scale*v-scale*anchor;
      const double u = std::fma(offset/left,ratio_left,shift_left);
      const double w = std::fma(-offset/right,ratio_right,shift_right);
      double lu, lw;
      if (u < -.5 || w < -.5) {
        const auto logs = special::interval_log_coordinates(v,lo,hi);
        lu = u < -.5 ? logs.lower-anchor_logs.lower+std::log(ratio_left) : std::log1p(u);
        lw = w < -.5 ? logs.upper-anchor_logs.upper+std::log(ratio_right) : std::log1p(w);
      } else {
        lu = std::log1p(u);
        lw = std::log1p(w);
      }
      divergence.push(mu*(std::abs(u)<.1 ? beta_detail::u_minus_log1p(u) : u-lu)
                    + mc*(std::abs(w)<.1 ? beta_detail::u_minus_log1p(w) : w-lw));
      angular.push(lu-lw);
      magnitude.push(std::abs(lu-lw));
    }
    kl = divergence.get()/count;
    gap = angular.get()/count;
    gap_scale = magnitude.get()/count;
    return std::isfinite(kl) && std::isfinite(gap);
  };

  const auto solve_ray = [&](double mu, double mc, double kl, double initial) {
    const auto ray = [&](double tau, double& f, double& df) {
      const double precision = std::exp(tau), first = mu*precision, second = mc*precision;
      f = -kl + mu*beta_detail::psi_increment_rest(first,second)
              + mc*beta_detail::psi_increment_rest(second,first);
      df = precision*beta_detail::trigamma_minus_inv(precision)
           - mu*(first*beta_detail::trigamma_minus_inv(first))
           - mc*(second*beta_detail::trigamma_minus_inv(second));
      if (!(df<0)) df = -.5/precision;
      return std::isfinite(f);
    };
    const auto root = beta_detail::decreasing_root(ray,std::log(initial),0,1e-15,lower,upper,budget);
    work += root.evaluations;
    return root;
  };

  double a = NAN, b = NAN, ray_slope = NAN;
  const auto profile = [&](double eta, double& score, double& derivative) {
    double mu, mc, kl, gap, gap_scale;
    if (!evaluate(eta,mu,mc,kl,gap,gap_scale)) return false;
    if (!std::isfinite(t)) t = std::clamp(.5/kl,1e-3,1e300);
    const auto root = solve_ray(mu,mc,kl,t);
    if (!root.ok) return false;
    t = std::exp(root.t);
    a = mu*t;
    b = mc*t;
    const double ca = beta_detail::trigamma_minus_inv(a), cb = beta_detail::trigamma_minus_inv(b);
    const double d1 = mc*cb-mu*ca;
    const double h = t*beta_detail::trigamma_minus_inv(t)-mu*(a*ca)-mc*(b*cb);
    score = gap + beta_detail::psi_increment_rest(a,b)-beta_detail::psi_increment_rest(b,a);
    ray_slope = -mu*mc*t*d1/h;
    const double angular_slope = -1-mc*(a*ca)-mu*(b*cb)+ray_slope*(score+t*d1);
    derivative = angular_slope < 0 && std::isfinite(angular_slope) ? angular_slope : -1;
    return std::isfinite(score);
  };

  double mu, mc, kl, gap, gap_scale;
  if (!evaluate(0,mu,mc,kl,gap,gap_scale)) return {NAN,NAN,NAN,0,BetaStatus::IterationFailure};
  const double start = gap; // The logit at the mean of the observed log odds.
  double score, derivative;
  if (!profile(start,score,derivative)) return {NAN,NAN,NAN,0,BetaStatus::IterationFailure};
  // Constrain both log shapes: along the exact profile d(log t)/d eta is
  // ray_slope at the root. The quadratic term matters at high precision.
  const auto resolution = [&]() {
    return std::min((tol/32)/(1+std::abs(ray_slope)),
                    std::sqrt((tol/256)/(1+a*(b/t))));
  };
  // Enclose the ray at both ends of a logit interval to decide the
  // profile signs. At fixed mean, the angular score is monotone in
  // precision because z*trigamma(z) decreases with z.
  const double eps = std::numeric_limits<double>::epsilon();
  const auto check = [&](double eta, double precision, double radius, double tau_radius) {
    struct Scores { double ray, angle, angle_error, ray_error, tangent; };
    const auto scores = [&](double mu, double mc, double kl, double gap, double gap_scale, double t) {
      const double a = mu*t, b = mc*t;
      const double ra = beta_detail::psi_increment_rest(a,b);
      const double rb = beta_detail::psi_increment_rest(b,a);
      // The first omitted asymptotic term is at most about
      // 0.152*z^-9 of the leading difference for z >= 20.
      const auto rest_error = [&](double z, double h, double value) {
        if (z>=20) return std::abs(value)*(64*eps+.18*std::pow(1/z,9));
        if (h<.01*z) return std::abs(value)*8e-13;
        const auto kernel_scale = [](double x) {
          return x>=20 ? 1/x : 1+std::abs(std::log(x))+std::abs(beta_detail::psi_minus_log(x));
        };
        return 8*eps*(kernel_scale(z)+kernel_scale(z+h));
      };
      const double ea = rest_error(a,b,ra), eb = rest_error(b,a,rb);
      const double angle = gap+ra-rb;
      const double slope = angle+mc*t*beta_detail::trigamma_minus_inv(mc*t)
                                -mu*t*beta_detail::trigamma_minus_inv(mu*t);
      return Scores{-kl+mu*ra+mc*rb,angle,
          128*eps*gap_scale+ea+eb,128*eps*kl+mu*ea+mc*eb,slope};
    };
    const double t_low = precision*std::exp(-tau_radius);
    const double t_high = precision*std::exp(tau_radius);
    double center_mu, center_mc, center_kl, center_gap, center_scale;
    if (!evaluate(eta,center_mu,center_mc,center_kl,center_gap,center_scale)) return false;
    const auto center = scores(center_mu,center_mc,center_kl,center_gap,center_scale,t_high);
    if (!(center.ray < -center.ray_error)) return false;
    for (double side : {-1.,1.}) {
      const double edge = eta+side*radius;
      if (edge==eta) return false;
      double mu, mc, kl, gap, gap_scale;
      if (!evaluate(edge,mu,mc,kl,gap,gap_scale)) return false;
      const auto low = scores(mu,mc,kl,gap,gap_scale,t_low);
      // At fixed precision the ray score is concave in mu. Its minimum
      // is at an endpoint and its tangent at the center bounds it above.
      const double movement = center_mu*center_mc*std::abs(std::expm1(edge-eta))/
                              (1+center_mu*std::expm1(edge-eta));
      if (!(low.ray>low.ray_error &&
            center.ray+center.ray_error+movement*(std::abs(center.tangent)+center.angle_error)<0))
        return false;
      const auto ray = solve_ray(mu,mc,kl,precision);
      if (!ray.ok) return false;
      const double local_width = tol/32;
      const auto ray_low = scores(mu,mc,kl,gap,gap_scale,std::exp(ray.t-local_width));
      const auto ray_high = scores(mu,mc,kl,gap,gap_scale,std::exp(ray.t+local_width));
      if (!(ray_low.ray>ray_low.ray_error && ray_high.ray<-ray_high.ray_error &&
            -side*ray_low.angle>ray_low.angle_error && -side*ray_high.angle>ray_high.angle_error))
        return false;
    }
    // |d log(mu)/d eta| and |d log(1-mu)/d eta| are at most one.
    return radius+tau_radius <= tol*.375;
  };
  double step = std::min(1.0,std::max(4*resolution(),2*std::abs(score/derivative)));
  for (std::int64_t pass=0; pass<budget; ++pass) {
    const double width_eta = resolution();
    // The shared root finder has an absolute ulp floor in eta. Recenter
    // its coordinate and scale it by the requested profile resolution.
    const double origin = pass == 0 ? start : gap;
    if (!(width_eta>0 && std::isfinite(width_eta))) break;
    const auto centered = [&](double z, double& f, double& df) {
      const bool ok = profile(std::fma(width_eta,z,origin),f,df);
      df *= width_eta;
      return ok;
    };
    const auto root = beta_detail::decreasing_root(centered,0,1,1,
        -700/width_eta,700/width_eta,budget,step/width_eta);
    if (!root.ok) break;
    gap = std::fma(width_eta,root.t,origin);
    if (!profile(gap,score,derivative)) break;
    if (resolution() >= width_eta*.99) {
      if (check(gap,t,4*width_eta,tol/4)) {
        const int iterations = static_cast<int>(std::min<std::int64_t>(work,std::numeric_limits<int>::max()));
        return {a,b,NAN,iterations,BetaStatus::Ok};
      }
      break;
    }
    step = 4*resolution();
  }
  return {NAN,NAN,NAN,0,BetaStatus::IterationFailure};
}

} } // namespace cppunivariate::nsbeta_detail
