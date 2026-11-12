#include <cmath>
// g++ -g -O2 -DNDEBUG -std=gnu++17 -c contraction_probe.cpp -o contraction_probe.o
// objdump -dr --no-show-raw-insn contraction_probe.o
// Distinguish implicit contraction from an explicitly requested fused operation.
double multiply_add(double a, double b, double c) { return a*b+c; }
double explicit_fma(double a, double b, double c) { return std::fma(a,b,c); }
