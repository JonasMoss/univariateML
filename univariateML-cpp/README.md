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

Mathematical definitions, fitting algorithms and validation evidence live in
[the family dossiers](include/cppunivariate/distributions/).
