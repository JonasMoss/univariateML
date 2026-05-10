// JSON I/O for the validation-harness round-trip.
//
// Reads quality/derived/<id>/eval_request.json (data + theta sweep)
// and writes quality/derived/<id>/cpp_eval.json (nll/grad/Hessian at
// each theta). Handles nested numeric arrays (thetas as 2D, hess as
// 3D), which the existing json_loader.hpp doesn't support.
//
// Same hand-rolled style as json_loader.hpp: the format is fixed and
// produced by quality/harness/, so a full JSON library is overkill.

#pragma once

#include "json_loader.hpp"

#include <fstream>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace mltest {

struct EvalRequest {
  std::string id;
  std::string spec_hash;
  std::size_t n_params = 0;
  std::vector<std::string> param_names;
  std::vector<double> data;
  std::vector<double> mle;
  std::vector<std::vector<double>> thetas;
};

namespace detail {

inline std::vector<std::vector<double>>
parse_2d_array_at(const std::string &s, std::size_t pos) {
  std::vector<std::vector<double>> out;
  while (pos < s.size() && s[pos] != '[') ++pos;
  if (pos >= s.size()) return out;
  ++pos;
  while (pos < s.size()) {
    while (pos < s.size() &&
           (s[pos] == ' ' || s[pos] == ',' || s[pos] == '\n' ||
            s[pos] == '\r' || s[pos] == '\t'))
      ++pos;
    if (pos >= s.size() || s[pos] == ']') break;
    if (s[pos] != '[') break;
    ++pos;  // skip inner '['
    std::vector<double> inner;
    while (pos < s.size() && s[pos] != ']') {
      while (pos < s.size() &&
             (s[pos] == ' ' || s[pos] == ',' || s[pos] == '\n' ||
              s[pos] == '\r' || s[pos] == '\t'))
        ++pos;
      if (pos >= s.size() || s[pos] == ']') break;
      std::size_t consumed = 0;
      inner.push_back(std::stod(s.substr(pos), &consumed));
      pos += consumed;
    }
    if (pos < s.size() && s[pos] == ']') ++pos;
    out.push_back(std::move(inner));
  }
  return out;
}

inline std::vector<std::string>
parse_string_array_at(const std::string &s, std::size_t pos) {
  std::vector<std::string> out;
  while (pos < s.size() && s[pos] != '[') ++pos;
  if (pos >= s.size()) return out;
  ++pos;
  while (pos < s.size() && s[pos] != ']') {
    while (pos < s.size() &&
           (s[pos] == ' ' || s[pos] == ',' || s[pos] == '\n' ||
            s[pos] == '\r' || s[pos] == '\t'))
      ++pos;
    if (pos >= s.size() || s[pos] == ']') break;
    if (s[pos] != '"') {
      ++pos;
      continue;
    }
    const std::size_t start = pos + 1;
    const std::size_t end = s.find('"', start);
    if (end == std::string::npos) break;
    out.emplace_back(s.substr(start, end - start));
    pos = end + 1;
  }
  return out;
}

inline std::string fmt_double(double v) {
  std::ostringstream os;
  os.precision(17);
  os << v;
  return os.str();
}

inline std::string fmt_1d(const std::vector<double> &v) {
  std::ostringstream os;
  os << "[";
  for (std::size_t i = 0; i < v.size(); ++i) {
    if (i) os << ", ";
    os << fmt_double(v[i]);
  }
  os << "]";
  return os.str();
}

inline std::string fmt_2d(const std::vector<std::vector<double>> &v) {
  std::ostringstream os;
  os << "[\n";
  for (std::size_t i = 0; i < v.size(); ++i) {
    os << "    " << fmt_1d(v[i]);
    if (i + 1 < v.size()) os << ",";
    os << "\n";
  }
  os << "  ]";
  return os.str();
}

inline std::string
fmt_3d(const std::vector<std::vector<std::vector<double>>> &v) {
  std::ostringstream os;
  os << "[\n";
  for (std::size_t i = 0; i < v.size(); ++i) {
    os << "    " << fmt_2d(v[i]);
    if (i + 1 < v.size()) os << ",";
    os << "\n";
  }
  os << "  ]";
  return os.str();
}

}  // namespace detail

inline EvalRequest load_eval_request(const std::string &path) {
  const std::string text = detail::slurp(path);
  EvalRequest req;
  std::size_t p;

  p = detail::find_key(text, "id");
  if (p != std::string::npos) req.id = detail::parse_string_at(text, p);

  p = detail::find_key(text, "spec_hash");
  if (p != std::string::npos) req.spec_hash = detail::parse_string_at(text, p);

  p = detail::find_key(text, "n_params");
  if (p != std::string::npos)
    req.n_params = static_cast<std::size_t>(detail::parse_number_at(text, p));

  p = detail::find_key(text, "param_names");
  if (p != std::string::npos)
    req.param_names = detail::parse_string_array_at(text, p);

  p = detail::find_key(text, "data");
  if (p == std::string::npos)
    throw std::runtime_error("missing 'data' in " + path);
  req.data = detail::parse_array_at(text, p);

  p = detail::find_key(text, "mle");
  if (p != std::string::npos) req.mle = detail::parse_array_at(text, p);

  p = detail::find_key(text, "thetas");
  if (p == std::string::npos)
    throw std::runtime_error("missing 'thetas' in " + path);
  req.thetas = detail::parse_2d_array_at(text, p);

  return req;
}

inline void write_cpp_eval(
    const std::string &path,
    const EvalRequest &req,
    const std::vector<double> &nll_at_theta,
    const std::vector<std::vector<double>> &grad_at_theta,
    const std::vector<std::vector<std::vector<double>>> &hess_at_theta) {
  std::ofstream out(path);
  if (!out)
    throw std::runtime_error("could not open " + path + " for writing");
  out << "{\n";
  out << "  \"id\": \"" << req.id << "\",\n";
  out << "  \"spec_hash\": \"" << req.spec_hash << "\",\n";
  out << "  \"n_thetas\": " << req.thetas.size() << ",\n";
  out << "  \"n_params\": " << req.n_params << ",\n";
  out << "  \"nll_at_theta\": " << detail::fmt_1d(nll_at_theta) << ",\n";
  out << "  \"grad_at_theta\": " << detail::fmt_2d(grad_at_theta) << ",\n";
  out << "  \"hess_at_theta\": " << detail::fmt_3d(hess_at_theta) << "\n";
  out << "}\n";
}

}  // namespace mltest
