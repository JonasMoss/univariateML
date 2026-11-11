# Small-shape diagnostic

For nonzero Q, write w = (log x - mu)/sigma, a = Q^-2, and
R(a) = log Gamma(a) - (a - 1/2) log a + a - log(2 pi)/2.
The exact density of w is

    exp(-log(2 pi)/2 - R(a) - (exp(Q w) - 1 - Q w)/Q^2).

This expression has a normal limit. It also gives a direct, independent
high-precision quadrature target for the CDF, provided the cancellation in
the exponent and R is controlled. The companion `reference.py` computes this
integral with mpmath. `gamma-seam.cpp` tests the existing gamma-tail helper
with its log argument log(a) + Q at w=1. Its output is not an accuracy
certificate: it demonstrates that direct use of the helper is unsuitable.

These are the initial diagnostic findings. The subsequent implementation,
validation and fitter-free staging changes are described in the job report.

The 70-digit numerical quadrature in `reference.json` gives, at w=1,
CDF 0.8413448670538749618 for Q=10^-6 and 0.8413447472783965682
for Q=10^-8. On this workstation, the expanded `gamma-seam.cpp`
reports 0.86409497202949082 and 0.99759239046464721, respectively.
The Boost gamma_p and gamma_q values themselves have this discrepancy;
the helper does not introduce it. This comparison is numerical evidence,
not a bound on the backend's whole parameter range. The quadrature truncates
at |w|=80 for |Q|<=0.01, and its output is not interval-certified.

The large-shape RNG seam has a related issue: `StdRng::rgamma` returns a
rounded gamma variate, so the subsequent subtraction of the shape and division
by Q can discard the normal-scale fluctuation. The new sampler retains that fluctuation in its log-domain rejection
proposal; see the job report for its seeded validation.
