#pragma once

#include "cppunivariate/core/maximizer_set.hpp"
#include <iomanip>
#include <locale>
#include <sstream>
#include <string>

namespace umvl_binding {
inline std::string maxset_number(double x, bool integer = false) {
  if (std::isinf(x)) return x < 0 ? "-Inf" : "Inf";
  std::ostringstream value;
  value.imbue(std::locale::classic());
  if (integer) value << std::fixed << std::setprecision(0);
  else value << std::setprecision(std::numeric_limits<double>::max_digits10);
  value << cppunivariate::maxset_zero(x);
  return value.str();
}

inline std::string maxset_form(cppunivariate::RelationForm form) {
  switch (form) {
    case cppunivariate::RelationForm::Product: return "product";
    case cppunivariate::RelationForm::PartOverSum: return "part_over_sum";
    case cppunivariate::RelationForm::CountRatio: return "count_ratio";
    case cppunivariate::RelationForm::Semicircle: return "semicircle";
  }
  return "";
}

inline std::string maxset_level(const cppunivariate::Level& level) {
  return level.denominator ? std::to_string(level.numerator) + "/" + std::to_string(level.denominator)
                           : maxset_number(level.value);
}

inline std::string maxset_axis(const cppunivariate::Axis& axis, const std::string& name) {
  const auto& r = axis.range;
  const auto bound = [&](double value, bool rounded) {
    return (rounded ? "approximately " : "") + maxset_number(value, axis.integer);
  };
  std::string description = name;
  if (r.lower == r.upper) description += " = " + bound(r.lower, r.lower_rounded || r.upper_rounded);
  else {
    description += r.lower_closed ? " in [" : " in (";
    description += bound(r.lower, r.lower_rounded) + ", " + bound(r.upper, r.upper_rounded);
    description += r.upper_closed ? "]" : ")";
    if (axis.integer) description += " (integer)";
  }
  return description;
}

template <class Names>
inline std::string maxset_message(const cppunivariate::MaximizerSet& set, const Names& names) {
  std::string message = "Maximum is not unique. ";
  const auto append = [](std::string& target, const std::string& value, const char* separator) {
    if (!target.empty()) target += separator;
    target += value;
  };
  std::string clauses;
  for (std::size_t k = 0; k < set.size; ++k) {
    const auto& piece = set.pieces[k].value;
    if (const auto* points = std::get_if<cppunivariate::Points>(&piece)) {
      for (std::size_t i = 0; i < points->size; ++i) {
        const auto& point = points->values[i];
        std::string clause;
        for (std::size_t j = 0; j < set.dimension; ++j) {
          const double x = point.estimate.values[j];
          std::string coordinate = names[j].first + " = ";
          if (point.enclosure && point.enclosure->lower.values[j] != point.enclosure->upper.values[j]) {
            coordinate += "approximately " + maxset_number(x) + " in [" +
                maxset_number(point.enclosure->lower.values[j]) + ", " +
                maxset_number(point.enclosure->upper.values[j]) + "]";
          } else coordinate += maxset_number(x);
          append(clause, coordinate, " and ");
        }
        append(clauses, clause, " or ");
      }
      continue;
    }
    std::string clause;
    const auto* box = std::get_if<cppunivariate::Box>(&piece);
    const auto* relation = std::get_if<cppunivariate::Relation>(&piece);
    const auto& axes = box ? box->axes : relation->axes;
    for (std::size_t j = 0; j < set.dimension; ++j)
      append(clause, maxset_axis(axes[j], names[j].first), " and ");
    if (box) {
      for (std::size_t j = 0; j < box->order_size; ++j) {
        const auto order = box->order[j];
        append(clause, names[order.lhs].first + " <= " + names[order.rhs].first, " and ");
      }
    } else {
      const auto& x = names[relation->driver].first;
      const auto& y = names[relation->dependent].first;
      std::string equation;
      if (relation->form == cppunivariate::RelationForm::Semicircle) {
        equation = y + "^2 = (" + x + " - " + maxset_number(relation->anchors[0]) +
            ") * (" + maxset_number(relation->anchors[1]) + " - " + x + ")";
      } else {
        if (relation->form == cppunivariate::RelationForm::Product)
          equation = (relation->complement_driver ? "(1 - " + x + ")" : x) + " * " +
              (relation->complement_dependent ? "(1 - " + y + ")" : y);
        else if (relation->form == cppunivariate::RelationForm::PartOverSum)
          equation = x + "/(" + x + " + " + y + ")";
        else equation = x + "/" + y;
        equation += relation->level.rounded ? " = approximately " : " = ";
        equation += maxset_level(relation->level);
      }
      append(clause, equation, " and ");
    }
    append(clauses, clause, " or ");
  }
  return message + clauses + ".";
}

inline std::string maxset_json_string(const std::string& value) {
  std::string result = "\"";
  for (unsigned char ch : value) {
    if (ch == '\\' || ch == '"') result += '\\';
    if (ch < 0x20) {
      constexpr char digits[] = "0123456789abcdef";
      result += "\\u00";
      result += digits[ch >> 4];
      result += digits[ch & 15];
    } else result += static_cast<char>(ch);
  }
  return result + "\"";
}

inline std::string maxset_json_number(double x) {
  if (std::isinf(x)) return x < 0 ? "\"-inf\"" : "\"inf\"";
  return maxset_number(x);
}

template <class Names>
inline std::string maxset_json(const cppunivariate::MaximizerSet& set, const Names& names) {
  if (!set.size) return "null";
  const auto boolean = [](bool value) { return value ? "true" : "false"; };
  const auto vector = [&](const cppunivariate::Coordinates& coordinates) {
    std::string value = "[";
    for (std::size_t i = 0; i < set.dimension; ++i)
      value += std::string(i ? "," : "") + maxset_json_number(coordinates.values[i]);
    return value + "]";
  };
  std::string json = "{\"version\":1,\"coordinates\":[";
  for (std::size_t i = 0; i < set.dimension; ++i)
    json += std::string(i ? "," : "") + maxset_json_string(names[i].first);
  json += "],\"pieces\":[";
  for (std::size_t k = 0; k < set.size; ++k) {
    if (k) json += ',';
    const auto& piece = set.pieces[k].value;
    if (const auto* points = std::get_if<cppunivariate::Points>(&piece)) {
      json += "{\"kind\":\"points\",\"points\":[";
      for (std::size_t i = 0; i < points->size; ++i) {
        if (i) json += ',';
        const auto& point = points->values[i];
        json += "{\"estimate\":" + vector(point.estimate);
        if (point.enclosure)
          json += ",\"enclosure\":{\"lower\":" + vector(point.enclosure->lower) +
                  ",\"upper\":" + vector(point.enclosure->upper) + "}";
        json += '}';
      }
      json += "]}";
      continue;
    }
    const auto* box = std::get_if<cppunivariate::Box>(&piece);
    const auto* relation = std::get_if<cppunivariate::Relation>(&piece);
    json += box ? "{\"kind\":\"box\",\"axes\":[" : "{\"kind\":\"relation\",\"axes\":[";
    const auto& axes = box ? box->axes : relation->axes;
    for (std::size_t i = 0; i < set.dimension; ++i) {
      if (i) json += ',';
      const auto& axis = axes[i];
      const auto& r = axis.range;
      json += "{\"lower\":" + maxset_json_number(r.lower) + ",\"upper\":" + maxset_json_number(r.upper) +
              ",\"closed\":[" + boolean(r.lower_closed) + "," + boolean(r.upper_closed) +
              "],\"integer\":" + boolean(axis.integer);
      if (r.lower_rounded || r.upper_rounded)
        json += std::string(",\"rounded\":[") + boolean(r.lower_rounded) + "," + boolean(r.upper_rounded) + "]";
      json += '}';
    }
    json += ']';
    if (box) {
      json += ",\"order\":[";
      for (std::size_t j = 0; j < box->order_size; ++j) {
        if (j) json += ',';
        json += "[" + std::to_string(box->order[j].lhs) + "," + std::to_string(box->order[j].rhs) + "]";
      }
      json += ']';
    } else {
      json += ",\"form\":" + maxset_json_string(maxset_form(relation->form)) +
              ",\"driver\":" + std::to_string(relation->driver) +
              ",\"dependent\":" + std::to_string(relation->dependent);
      if (relation->form == cppunivariate::RelationForm::Semicircle)
        json += ",\"anchors\":[" + maxset_json_number(relation->anchors[0]) + "," +
                maxset_json_number(relation->anchors[1]) + "]";
      else {
        const auto& level = relation->level;
        json += ",\"level\":";
        if (level.denominator)
          json += "{\"numerator\":\"" + std::to_string(level.numerator) +
                  "\",\"denominator\":\"" + std::to_string(level.denominator) + "\"}";
        else json += maxset_json_number(level.value);
        if (level.rounded) json += ",\"level_rounded\":true";
        if (relation->form == cppunivariate::RelationForm::Product)
          json += std::string(",\"complement\":[") + boolean(relation->complement_driver) +
                  "," + boolean(relation->complement_dependent) + "]";
      }
    }
    json += '}';
  }
  return json + "]}";
}
}  // namespace umvl_binding
