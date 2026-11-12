# univariateML C++

Header-only C++17 library. Requires a C++17 compiler and Boost headers;
CMake tests include doctest and their fixtures in this tree. R is not required.

```sh
cmake -S . -B build -DBoost_INCLUDE_DIR=/path/to/boost-headers
cmake --build build
ctest --test-dir build --output-on-failure
```

Omit `Boost_INCLUDE_DIR` for installed Boost (CONFIG, then MODULE discovery),
or set `BOOST_ROOT` to a Boost installation. The header directory must contain
`boost/version.hpp`.

Test executables default to `-g -O2 -DNDEBUG` and GNU C++17, matching R's shipped
optimization, assertion and language modes on GCC. No fast-math flags are added.
The test flags do not propagate to consumers of the header-only library. For an
unoptimized debugging build, configure a separate directory with
`-DCPPUNIV_TEST_OPTIMIZE=OFF`; this selects `-g -O0` with assertions enabled
for every test target.
Keep that directory separate from the optimized validation builds.

Mathematical definitions, fitting algorithms and validation evidence live in
[the family dossiers](include/cppunivariate/distributions/).
