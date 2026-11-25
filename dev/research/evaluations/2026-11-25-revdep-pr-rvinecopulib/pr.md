# Test non-finite margin log-likelihoods without univariateML

Hi Thomas, this is a small test change for an upcoming univariateML bugfix
release. In 1.5.0, fitting a Rayleigh margin to data containing zero returned
a fit with log-likelihood `-Inf`. The upcoming release raises
`univariateML_invalid` because every scale gives likelihood zero. This
condition inherits from `univariateML_stop` and carries `kind` and `reason`.
rvinecopulib already catches it and reports the candidate failure.

The test now supplies a custom fitted margin with log-likelihood `-Inf`.
It still checks the error when that is the only candidate and the warning
and fallback when another candidate fits.

I ran `test_margins.R` and the full testthat suite against CRAN 1.5.0 and the
upcoming build. The patched suite passed with 1.5.0, and the two changed
assertions passed with both builds. The upcoming build still had two
discrete-support failures. We plan to restore the support type attribute in
univariateML, which cleared both failures in a diagnostic run. That run
then reached an ordered-binomial case whose call now needs an explicit
`size`. That compatibility question is separate from this test change.

Happy to adjust the fixture if you prefer a different test style.
