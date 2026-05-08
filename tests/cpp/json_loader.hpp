// Minimal JSON loader for tests/golden/<id>.json files.
//
// Hand-rolled because the format is fixed: a flat object with a few
// scalar fields, a nested "estimates" object, and a "data" array.
// Vendoring nlohmann/json (~25k LoC) just to read that would be silly.
//
// Not a general parser. Assumes the goldens are produced by
// scripts/gen_goldens.R (jsonlite with auto_unbox + digits = 17).

#pragma once

#include "../../src/ml/core/span.hpp"

#include <cstddef>
#include <fstream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace mltest {

struct Golden {
  std::string id;
  std::map<std::string, double> estimates;
  double logLik = 0.0;
  std::vector<double> data;
};

namespace detail {

inline std::string slurp(const std::string &path) {
  std::ifstream in(path);
  if (!in) throw std::runtime_error("could not open " + path);
  std::stringstream ss;
  ss << in.rdbuf();
  return ss.str();
}

// Find `"key"` and return the position right after the colon that follows it.
// Returns std::string::npos if not found. start_at lets the caller scope the
// search inside a substring (e.g. inside an "estimates": { ... } block).
inline std::size_t find_key(const std::string &s, const std::string &key,
                            std::size_t start_at = 0) {
  const std::string needle = "\"" + key + "\"";
  std::size_t pos = s.find(needle, start_at);
  if (pos == std::string::npos) return pos;
  pos = s.find(':', pos + needle.size());
  if (pos == std::string::npos) return pos;
  return pos + 1;
}

inline double parse_number_at(const std::string &s, std::size_t pos) {
  while (pos < s.size() &&
         (s[pos] == ' ' || s[pos] == '\t' || s[pos] == '\n' || s[pos] == '\r'))
    ++pos;
  return std::stod(s.substr(pos));
}

inline std::string parse_string_at(const std::string &s, std::size_t pos) {
  while (pos < s.size() && s[pos] != '"') ++pos;
  if (pos >= s.size()) return {};
  std::size_t end = s.find('"', pos + 1);
  return s.substr(pos + 1, end - pos - 1);
}

inline std::vector<double> parse_array_at(const std::string &s,
                                          std::size_t pos) {
  std::vector<double> out;
  while (pos < s.size() && s[pos] != '[') ++pos;
  if (pos >= s.size()) return out;
  ++pos;  // skip '['
  while (pos < s.size() && s[pos] != ']') {
    while (pos < s.size() && (s[pos] == ' ' || s[pos] == ',' || s[pos] == '\n' ||
                              s[pos] == '\r' || s[pos] == '\t'))
      ++pos;
    if (pos >= s.size() || s[pos] == ']') break;
    std::size_t consumed = 0;
    out.push_back(std::stod(s.substr(pos), &consumed));
    pos += consumed;
  }
  return out;
}

}  // namespace detail

inline Golden load_golden(const std::string &path) {
  const std::string text = detail::slurp(path);
  Golden g;

  // id (string)
  std::size_t p = detail::find_key(text, "id");
  if (p != std::string::npos) g.id = detail::parse_string_at(text, p);

  // logLik (number)
  p = detail::find_key(text, "logLik");
  if (p == std::string::npos)
    throw std::runtime_error("missing logLik in " + path);
  g.logLik = detail::parse_number_at(text, p);

  // data (array of numbers)
  p = detail::find_key(text, "data");
  if (p == std::string::npos)
    throw std::runtime_error("missing data in " + path);
  g.data = detail::parse_array_at(text, p);

  // estimates: scoped key search inside the object
  p = detail::find_key(text, "estimates");
  if (p != std::string::npos) {
    while (p < text.size() && text[p] != '{') ++p;
    if (p < text.size()) {
      const std::size_t obj_start = p + 1;
      // matching brace (no nested objects expected, but handle them anyway)
      int depth = 1;
      std::size_t obj_end = obj_start;
      for (; obj_end < text.size() && depth > 0; ++obj_end) {
        if (text[obj_end] == '{') ++depth;
        else if (text[obj_end] == '}') --depth;
      }
      const std::string inner = text.substr(obj_start, obj_end - obj_start - 1);
      // walk: each entry is "name": value
      std::size_t cursor = 0;
      while (cursor < inner.size()) {
        const std::size_t k0 = inner.find('"', cursor);
        if (k0 == std::string::npos) break;
        const std::size_t k1 = inner.find('"', k0 + 1);
        if (k1 == std::string::npos) break;
        const std::string name = inner.substr(k0 + 1, k1 - k0 - 1);
        const std::size_t colon = inner.find(':', k1);
        if (colon == std::string::npos) break;
        std::size_t v = colon + 1;
        while (v < inner.size() &&
               (inner[v] == ' ' || inner[v] == '\t' || inner[v] == '\n' ||
                inner[v] == '\r'))
          ++v;
        std::size_t consumed = 0;
        try {
          g.estimates[name] = std::stod(inner.substr(v), &consumed);
        } catch (...) {
          // skip non-numeric
          consumed = 1;
        }
        cursor = v + consumed;
      }
    }
  }

  return g;
}

inline cppunivariate::span as_span(const std::vector<double> &v) {
  return {v.data(), v.size()};
}

}  // namespace mltest
