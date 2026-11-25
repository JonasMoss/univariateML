# Make rate-fit fallback test independent of constant-data fits

Hi Jacob, the upcoming univariateML bugfix release raises `univariateML_nomle`
for Normal and Cauchy fits to constant data because both likelihoods are
unbounded. The finite Cauchy result in 1.5.0 happened to give this test a
fallback.

This patch supplies non-finite Normal statistics with a scoped mock and
fits Cauchy on an ordinary rate sample. It keeps the assertions about
selection, recorded failures, and the error when no candidate produces
finite statistics.

The changed file and the installed-package testthat suite passed with both
1.5.0 and the upcoming build. With repository fixtures enabled, the upcoming
build also exposed a separate Gumbel bootstrap assertion that fails with
and without the patch. The seeded upper bound rounds to 0.144 where the
fixture expects 0.142.

Happy to adjust the fixture or its placement.
