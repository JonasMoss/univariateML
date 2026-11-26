#pragma once

namespace cppunivariate { namespace dnorm_detail {

// Bounds the error against every coefficient in a real enclosure, including
// rounding of the published location after adding the data centre.
inline bool enclosed(double location, double scale, double location_error,
                     double scale_error, double tol) {
  return std::isfinite(location) && std::isfinite(scale) && scale > scale_error &&
      scale_error <= tol * (scale - scale_error) &&
      location_error <= tol * std::max(std::abs(location) - location_error,
                                      scale - scale_error);
}

inline double location_rounding(double centre, double offset, double location) {
  // TwoSum residual, including the case where the offset rounds away.
  const double added = location - centre;
  return std::abs((centre - (location - added)) + (offset - added));
}

inline bool score_enclosure(const Cells& p, double A, double B, bool hm, bool hs,
                            double mean, double sd, double tol, double padding=1) {
  const double offset = A/B;
  const double location = hm ? mean : p.centre + offset;
  const double scale = hs ? sd : 1/B;
  const double roundoff = hm ? 0 : location_rounding(p.centre, offset, location)
                                  + 2*std::numeric_limits<double>::epsilon()*std::abs(offset);
  const double radius_limit = std::min(tol/4, 1e-5);
  const double rad_b = hs ? 0 : radius_limit*B;
  const double rad_a = hm ? 0 : radius_limit;
  if (!(B > rad_b) || !std::isfinite(B)) return false;
  const double scale_error = rad_b/(B-rad_b)/B +
      (hs ? 0 : 2*std::numeric_limits<double>::epsilon()*scale);
  const double location_error = hm ? 0 : (rad_a+std::abs(offset)*rad_b)/(B-rad_b)+roundoff;
  if (!enclosed(location, scale, location_error, scale_error, tol)) return false;

  std::vector<double> g, H;
  EvaluationError error;
  if (hm || hs) {
    const auto sign = [&](double a, double b) {
      if (!std::isfinite(evaluate(p,a,b,hm,hs,mean-p.centre,g,H,error,true,mean)))
        return 0.0;
      return std::abs(g[0]) > padding*error.g[0] ? g[0] : 0.0;
    };
    const double lo = sign(A-rad_a,B-rad_b);
    const double hi = sign(A+rad_a,B+rad_b);
    return lo < 0 && hi > 0;
  }

  if (!std::isfinite(evaluate(p,A,B,false,false,0,g,H,error,true))) return false;
  for (double& element : error.H) element*=padding;
  const double det = H[0]*H[3]-H[1]*H[2];
  if (!(det > 0) || !std::isfinite(det)) return false;
  const double inverse[2][2] = {{H[3]/det,-H[1]/det},{-H[2]/det,H[0]/det}};

  // Write each mass as B times the integral of phi(z+B*u), |u|<=1/2.
  // Bound the third derivatives of -log mass by ranges under that integral.
  // Each covariance is bounded by the product of ranges / 4. Each third
  // central moment is bounded by the product of the three ranges.
  for (std::size_t cell=0; cell<p.x.size(); ++cell) {
    const double m = p.x[cell]+0.5-p.centre;
    const double z = m*B-A;
    const double upper_b = B+rad_b;
    const double abs_z = std::abs(z)+rad_a+std::abs(m)*rad_b;
    const double score_range[2] = {upper_b, abs_z+std::abs(m)*upper_b+upper_b/4};
    const double curvature_range[2][2] = {{0,1},{1,2*std::abs(m)+0.25}};
    const double radius[2] = {rad_a,rad_b};
    for (int i=0; i<2; ++i) for (int j=0; j<2; ++j) for (int k=0; k<2; ++k) {
      const double variation = score_range[i]*score_range[j]*score_range[k] +
          (score_range[i]*curvature_range[j][k]+score_range[j]*curvature_range[i][k]+
           score_range[k]*curvature_range[i][j])/4;
      error.H[2*i+j] += p.w[cell]*radius[k]*variation;
    }
  }
  const double lower_b = B-rad_b;
  error.H[3] += 2*p.n*rad_b/lower_b/lower_b/lower_b;
  const double radius[2] = {rad_a,rad_b};
  for (int i=0; i<2; ++i) {
    double bound = std::abs(inverse[i][0]*g[0]+inverse[i][1]*g[1]);
    for (int j=0; j<2; ++j) {
      bound += std::abs(inverse[i][j])*padding*error.g[j];
      const double residual = (i==j ? 1.0 : 0.0) -
          (inverse[i][0]*H[j]+inverse[i][1]*H[2+j]);
      bound += (std::abs(residual)+std::abs(inverse[i][0])*error.H[j] +
                std::abs(inverse[i][1])*error.H[2+j])*radius[j];
    }
    if (!(bound < radius[i])) return false;
  }
  return true;
}

}}  // namespace cppunivariate::dnorm_detail
