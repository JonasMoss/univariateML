// Conversion only; shared fit headers must precede Rcpp.h.
#pragma once
#include <Rcpp.h>
#include <algorithm>
inline umvl_binding::Held common_held(const Rcpp::Nullable<double>& value) {
  return value.isNull() ? umvl_binding::Held{} : umvl_binding::Held{true, Rcpp::as<double>(value)};
}
inline umvl_binding::Held common_held(const Rcpp::Nullable<Rcpp::NumericVector>& value) {
  if (value.isNull()) return {};
  const auto vector = Rcpp::as<Rcpp::NumericVector>(value);
  return {true, vector.size() == 1 ? vector[0] : NAN, static_cast<std::size_t>(vector.size())};
}
inline umvl_binding::Held common_held(double value) {
  return {true, value};
}
inline Rcpp::List common_fit_rows(const std::vector<umvl_binding::NamedCandidate>& rows) {
  Rcpp::List candidates;
  // Preserve model-specific coordinates such as the offset of a shifted face.
  std::vector<std::string> columns;
  for(const auto& candidate:rows)for(const auto& item:candidate.estimates)
    if(std::find(columns.begin(),columns.end(),item.first)==columns.end())columns.push_back(item.first);
  const int n=rows.size();
  Rcpp::CharacterVector models(n);Rcpp::NumericVector lower(n),upper(n);
  for(int i=0;i<n;++i){models[i]=rows[i].model;lower[i]=rows[i].loglik_lower;upper[i]=rows[i].loglik_upper;}
  candidates["model"]=models;
  for(const auto& name:columns) {
    Rcpp::NumericVector values(n,NA_REAL);
    for(int i=0;i<n;++i)for(const auto& item:rows[i].estimates)if(item.first==name)values[i]=item.second;
    candidates[name]=values;
  }
  candidates["loglik_lower"]=lower;candidates["loglik_upper"]=upper;
  candidates.attr("class")="data.frame";
  candidates.attr("row.names")=Rcpp::IntegerVector::create(NA_INTEGER,-n);
  return candidates;
}
inline Rcpp::List common_maximizer_set(const cppunivariate::MaximizerSet& set,
                                       const Rcpp::CharacterVector& names) {
  const auto coordinates = [&](const cppunivariate::Coordinates& values) {
    return Rcpp::NumericVector(values.values.begin(), values.values.begin() + set.dimension);
  };
  Rcpp::List pieces(set.size);
  for (std::size_t i = 0; i < set.size; ++i) {
    const auto& piece = set.pieces[i].value;
    if (const auto* points = std::get_if<cppunivariate::Points>(&piece)) {
      Rcpp::List entries(points->size);
      for (std::size_t j = 0; j < points->size; ++j) {
        const auto& point = points->values[j];
        auto entry = Rcpp::List::create(Rcpp::_["estimate"] = coordinates(point.estimate));
        if (point.enclosure)
          entry["enclosure"] = Rcpp::List::create(
            Rcpp::_["lower"] = coordinates(point.enclosure->lower),
            Rcpp::_["upper"] = coordinates(point.enclosure->upper));
        entries[j] = entry;
      }
      pieces[i] = Rcpp::List::create(Rcpp::_["kind"] = "points", Rcpp::_["points"] = entries);
      continue;
    }
    const auto* box = std::get_if<cppunivariate::Box>(&piece);
    const auto* relation = std::get_if<cppunivariate::Relation>(&piece);
    const auto& source = box ? box->axes : relation->axes;
    Rcpp::List axes(set.dimension);
    for (std::size_t j = 0; j < set.dimension; ++j) {
      const auto& axis = source[j];
      const auto& r = axis.range;
      auto out = Rcpp::List::create(Rcpp::_["lower"] = r.lower, Rcpp::_["upper"] = r.upper,
          Rcpp::_["closed"] = Rcpp::LogicalVector::create(r.lower_closed, r.upper_closed),
          Rcpp::_["integer"] = axis.integer);
      if (r.lower_rounded || r.upper_rounded)
        out["rounded"] = Rcpp::LogicalVector::create(r.lower_rounded, r.upper_rounded);
      axes[j] = out;
    }
    auto out = Rcpp::List::create(Rcpp::_["kind"] = box ? "box" : "relation", Rcpp::_["axes"] = axes);
    if (box) {
      Rcpp::List order(box->order_size);
      for (std::size_t j = 0; j < box->order_size; ++j)
        order[j] = Rcpp::IntegerVector::create(static_cast<int>(box->order[j].lhs),
                                               static_cast<int>(box->order[j].rhs));
      out["order"] = order;
    } else {
      out["form"] = umvl_binding::maxset_form(relation->form);
      out["driver"] = static_cast<int>(relation->driver);
      out["dependent"] = static_cast<int>(relation->dependent);
      if (relation->form == cppunivariate::RelationForm::Semicircle)
        out["anchors"] = Rcpp::NumericVector::create(relation->anchors[0], relation->anchors[1]);
      else {
        const auto& level = relation->level;
        if (level.denominator)
          out["level"] = Rcpp::List::create(Rcpp::_["numerator"] = std::to_string(level.numerator),
                                            Rcpp::_["denominator"] = std::to_string(level.denominator));
        else out["level"] = level.value;
        if (level.rounded) out["level_rounded"] = true;
        if (relation->form == cppunivariate::RelationForm::Product)
          out["complement"] = Rcpp::LogicalVector::create(relation->complement_driver,
                                                          relation->complement_dependent);
      }
    }
    pieces[i] = out;
  }
  return Rcpp::List::create(Rcpp::_["version"] = 1, Rcpp::_["coordinates"] = names,
                            Rcpp::_["pieces"] = pieces);
}
inline Rcpp::List common_result(const umvl_binding::FitResult& input, bool iterations) {
  const auto fit=umvl_binding::render_fit(input);
  for (const auto& message : fit.warnings) Rcpp::warning("%s", message);
  const auto candidates = common_fit_rows(fit.candidates);
  if (!fit.error_kind.empty()) {
    Rcpp::NumericVector estimates;
    for(const auto& item:fit.limit_estimates)estimates.push_back(item.second,item.first);
    auto condition=Rcpp::List::create(Rcpp::_["message"]=fit.error,Rcpp::_["call"]=R_NilValue,
        Rcpp::_["attainable_tol"]=fit.attainable_tol,Rcpp::_["kind"]=fit.error_kind,Rcpp::_["reason"]=cppunivariate::outcome_reason_name(fit.outcome.reason),Rcpp::_["candidates"]=candidates,
        Rcpp::_["model"]=fit.model,Rcpp::_["estimates"]=estimates,
        Rcpp::_["limit_loglik"]=fit.limit_loglik,Rcpp::_["loglik_lower"]=fit.loglik_lower,
        Rcpp::_["loglik_upper"]=fit.loglik_upper,Rcpp::_["segment_loglik_upper"]=fit.segment_loglik_upper);
    if(fit.outcome.kind==cppunivariate::OutcomeKind::Limit)condition["certified"]=fit.outcome.certified;
    if(fit.error_kind=="tolerance" || fit.error_kind=="conditioning")
      condition.attr("class")=Rcpp::CharacterVector::create("univariateML_"+fit.error_kind,"univariateML_numerical","univariateML_stop","error","condition");
    else condition.attr("class")=Rcpp::CharacterVector::create("univariateML_"+fit.error_kind,"univariateML_stop","error","condition");
    return condition;
  }
  Rcpp::NumericVector estimates(fit.estimates.size());
  Rcpp::CharacterVector names(fit.estimates.size());
  for (std::size_t i=0; i<fit.estimates.size(); ++i) {
    names[i]=fit.estimates[i].first; estimates[i]=fit.estimates[i].second;
  }
  estimates.attr("names")=names;
  estimates.attr("certified")=fit.outcome.certified;
  estimates.attr("unique")=Rcpp::LogicalVector::create(
      fit.outcome.uniqueness_known ? static_cast<int>(fit.outcome.unique) : NA_LOGICAL);
  for (const auto& item : fit.extra_vectors) estimates.attr(item.first)=Rcpp::wrap(item.second);
  if (fit.maximizer_set) {
    estimates.attr("maximizer_set") = common_maximizer_set(*fit.maximizer_set, names);
    estimates.attr("maximizers") = common_fit_rows(fit.maximizers);
    estimates.attr("rendered_names") = names;
    estimates.attr("outcome_message") = fit.outcome_message;
  }
  if(!fit.candidates.empty()) {
    estimates.attr("candidates")=candidates;
    estimates.attr("rendered_names")=names;
    estimates.attr("outcome_message")=fit.outcome_message;
    estimates.attr("comparison_model")=fit.model;
    estimates.attr("loglik_lower")=fit.loglik_lower;
    estimates.attr("loglik_upper")=fit.loglik_upper;
    estimates.attr("segment_loglik_upper")=fit.segment_loglik_upper;
    estimates.attr("limit_loglik")=fit.limit_loglik;
  }
  auto out=Rcpp::List::create(Rcpp::_["estimates"]=estimates,Rcpp::_["logLik"]=fit.logLik);
  if (fit.has_restricted) {
    Rcpp::CharacterVector values;
    for (const auto& item : fit.restricted) values.push_back(item.second, item.first);
    out["restricted"] = values;
  }
  for (const auto& item : fit.extra) out[item.first]=item.second;
  for (const auto& item : fit.extra_vectors) out[item.first] = Rcpp::wrap(item.second);
  if(iterations) out["iterations"]=fit.iterations;
  out["fixed"]=Rcpp::wrap(fit.fixed);
  return out;
}

inline Rcpp::List common_result(const umvl_binding::FitResult& fit) {
  auto out = common_result(fit, fit.has_iterations);
  if (out.inherits("univariateML_stop")) return out;
  if (!fit.has_fixed) out.erase(out.findName("fixed"));
  return out;
}
