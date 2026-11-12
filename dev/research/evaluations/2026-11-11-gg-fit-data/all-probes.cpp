// A single translation unit keeps compilation of the independent probes cheap.
#define main held_probe_main
#include "held-probe.cpp"
#undef main
#define main local_probe_main
#include "local-probe.cpp"
#undef main
#define main profile_probe_main
#include "profile-probe.cpp"
#undef main
#define main score_probe_main
#include "shape-score-probe.cpp"
#undef main
#define main face_probe_main
#include "face-probe.cpp"
#undef main
#define main zero_probe_main
#include "zero-probe.cpp"
#undef main
#include <cstdlib>
#include <string>
int main() {
  const std::string mode=std::getenv("GG_PROBE_MODE") ? std::getenv("GG_PROBE_MODE") : "";
  if(mode=="zero") return zero_probe_main();
  if(mode=="face") return face_probe_main();
  if(mode=="held") return held_probe_main();
  if(mode=="local") return local_probe_main();
  if(mode=="profile") return profile_probe_main();
  if(mode=="score") return score_probe_main();
  return 2;
}
