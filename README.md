# univariateML

Maximum likelihood estimation for univariate distributions, with a header-only
C++17 core and an R package. Python bindings are planned.

- [R package: usage and distribution catalog](bindings/r/README.md)
- [C++ library](univariateML-cpp/): public headers remain under `cppunivariate/`
- [Planned Python bindings](bindings/python/README.md)
- [Engineering and build guide](AGENTS.md)
- [Development records](dev/STATUS.md), [plans](dev/roadmap/README.md) and
  [research](dev/research/)

Install the published R package from CRAN:

```r
install.packages("univariateML")
```

Build the development R package from this checkout (R, Rcpp, BH and the
repository's development dependencies are required):

```sh
just regen
R CMD INSTALL univariateML-r
```

For C++ consumers, add `univariateML-cpp/include` to the include path and
provide Boost.Math. See [CMake configuration](univariateML-cpp/CMakeLists.txt)
and [tests](univariateML-cpp/tests/) for usage. Run `just cpp-test` to build
and test the core.

See [LICENSE.md](LICENSE.md) for licensing.
