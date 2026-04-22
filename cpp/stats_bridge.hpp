#ifndef STATS_BRIDGE_HPP
#define STATS_BRIDGE_HPP

#include <initializer_list>
#include <stdexcept>
#include <string>
#include <vector>

namespace stats_bridge {

using NumberList = std::vector<double>;
using StringList = std::vector<std::string>;

void initialize_bridge(const char* argv0);
void run_operation(const std::string& operation, const StringList& args);

inline NumberList make_number_list(std::initializer_list<double> values) {
  return NumberList(values);
}

inline StringList number_args(const NumberList& values) {
  StringList args;
  args.reserve(values.size());

  for (double value : values) {
    args.push_back(std::to_string(value));
  }

  return args;
}

inline void append_number_args(StringList& args, const NumberList& values) {
  for (double value : values) {
    args.push_back(std::to_string(value));
  }
}

inline void ensure_same_size(const NumberList& left, const NumberList& right, const std::string& label) {
  if (left.size() != right.size()) {
    throw std::runtime_error(label + " vectors must have the same size.");
  }
}

inline StringList interleave_pairs(const NumberList& first, const NumberList& second) {
  ensure_same_size(first, second, "Pair");

  StringList args;
  args.reserve(first.size() * 2);

  for (std::size_t index = 0; index < first.size(); ++index) {
    args.push_back(std::to_string(first[index]));
    args.push_back(std::to_string(second[index]));
  }

  return args;
}

inline StringList interleave_triplets(const NumberList& first, const NumberList& second, const NumberList& third) {
  ensure_same_size(first, second, "Triplet");
  ensure_same_size(first, third, "Triplet");

  StringList args;
  args.reserve(first.size() * 3);

  for (std::size_t index = 0; index < first.size(); ++index) {
    args.push_back(std::to_string(first[index]));
    args.push_back(std::to_string(second[index]));
    args.push_back(std::to_string(third[index]));
  }

  return args;
}

inline StringList interleave_distribution_pairs(const NumberList& values, const NumberList& probabilities) {
  return interleave_pairs(values, probabilities);
}

inline StringList interleave_label_count_pairs(const StringList& labels, const NumberList& counts) {
  if (labels.size() != counts.size()) {
    throw std::runtime_error("labels and counts must have the same size.");
  }

  StringList args;
  args.reserve(labels.size() * 2);

  for (std::size_t index = 0; index < labels.size(); ++index) {
    args.push_back(labels[index]);
    args.push_back(std::to_string(counts[index]));
  }

  return args;
}

inline void run_vector_operation(const std::string& operation, const NumberList& values) {
  run_operation(operation, number_args(values));
}

inline void run_two_vector_operation(
  const std::string& operation,
  const NumberList& first,
  const NumberList& second
) {
  run_operation(operation, interleave_pairs(first, second));
}

inline void run_three_vector_operation(
  const std::string& operation,
  const NumberList& first,
  const NumberList& second,
  const NumberList& third
) {
  run_operation(operation, interleave_triplets(first, second, third));
}

inline void findMean(const NumberList& data_set) { run_vector_operation("sample_mean", data_set); }
inline void findMean(std::initializer_list<double> data_set) { findMean(make_number_list(data_set)); }

inline void findPopulationMean(const NumberList& data_set) { run_vector_operation("population_mean", data_set); }
inline void findPopulationMean(std::initializer_list<double> data_set) { findPopulationMean(make_number_list(data_set)); }

inline void findMedian(const NumberList& data_set) { run_vector_operation("median", data_set); }
inline void findMedian(std::initializer_list<double> data_set) { findMedian(make_number_list(data_set)); }

inline void findMode(const NumberList& data_set) { run_vector_operation("mode", data_set); }
inline void findMode(std::initializer_list<double> data_set) { findMode(make_number_list(data_set)); }

inline void findRange(const NumberList& data_set) { run_vector_operation("range", data_set); }
inline void findRange(std::initializer_list<double> data_set) { findRange(make_number_list(data_set)); }

inline void findSampleVariance(const NumberList& data_set) { run_vector_operation("sample_variance", data_set); }
inline void findSampleVariance(std::initializer_list<double> data_set) { findSampleVariance(make_number_list(data_set)); }

inline void findPopulationVariance(const NumberList& data_set) { run_vector_operation("population_variance", data_set); }
inline void findPopulationVariance(std::initializer_list<double> data_set) { findPopulationVariance(make_number_list(data_set)); }

inline void findSampleStandardDeviation(const NumberList& data_set) { run_vector_operation("sample_sd", data_set); }
inline void findSampleStandardDeviation(std::initializer_list<double> data_set) { findSampleStandardDeviation(make_number_list(data_set)); }

inline void findPopulationStandardDeviation(const NumberList& data_set) { run_vector_operation("population_sd", data_set); }
inline void findPopulationStandardDeviation(std::initializer_list<double> data_set) { findPopulationStandardDeviation(make_number_list(data_set)); }

inline void findTrimmedMean(double trim, const NumberList& data_set) {
  StringList args = {std::to_string(trim)};
  append_number_args(args, data_set);
  run_operation("trimmed_mean", args);
}
inline void findTrimmedMean(double trim, std::initializer_list<double> data_set) { findTrimmedMean(trim, make_number_list(data_set)); }

inline void findMeanAbsoluteDeviation(const NumberList& data_set) { run_vector_operation("mean_absolute_deviation", data_set); }
inline void findMeanAbsoluteDeviation(std::initializer_list<double> data_set) { findMeanAbsoluteDeviation(make_number_list(data_set)); }

inline void findCoefficientOfVariation(const NumberList& data_set) { run_vector_operation("coefficient_variation", data_set); }
inline void findCoefficientOfVariation(std::initializer_list<double> data_set) { findCoefficientOfVariation(make_number_list(data_set)); }

inline void findType2Quantile(double probability, const NumberList& data_set) {
  StringList args = {std::to_string(probability)};
  append_number_args(args, data_set);
  run_operation("quantile_type2", args);
}
inline void findType2Quantile(double probability, std::initializer_list<double> data_set) { findType2Quantile(probability, make_number_list(data_set)); }

inline void findPercentileRank(double value, const NumberList& data_set) {
  StringList args = {std::to_string(value)};
  append_number_args(args, data_set);
  run_operation("percentile_rank", args);
}
inline void findPercentileRank(double value, std::initializer_list<double> data_set) { findPercentileRank(value, make_number_list(data_set)); }

inline void findIQRType2(const NumberList& data_set) { run_vector_operation("iqr_type2", data_set); }
inline void findIQRType2(std::initializer_list<double> data_set) { findIQRType2(make_number_list(data_set)); }

inline void findOutlierFences(const NumberList& data_set) { run_vector_operation("outlier_fences", data_set); }
inline void findOutlierFences(std::initializer_list<double> data_set) { findOutlierFences(make_number_list(data_set)); }

inline void findFrequencyTable(const NumberList& data_set) { run_vector_operation("frequency_table", data_set); }
inline void findFrequencyTable(std::initializer_list<double> data_set) { findFrequencyTable(make_number_list(data_set)); }

inline void findMostFrequentCategory(const StringList& labels, const NumberList& counts) {
  run_operation("most_frequent_category", interleave_label_count_pairs(labels, counts));
}

inline void findCategoryProportionByLabel(
  const std::string& target_label,
  const StringList& labels,
  const NumberList& counts
) {
  StringList args = {target_label};
  StringList label_count_pairs = interleave_label_count_pairs(labels, counts);
  args.insert(args.end(), label_count_pairs.begin(), label_count_pairs.end());
  run_operation("category_proportion", args);
}

inline void findWeightedMean(const NumberList& observations, const NumberList& weights) {
  run_operation("weighted_mean", interleave_pairs(observations, weights));
}
inline void findWeightedMean(std::initializer_list<double> observations, std::initializer_list<double> weights) {
  findWeightedMean(make_number_list(observations), make_number_list(weights));
}

inline void findMissingValueFromMean(double known_mean, int n_total, const NumberList& known_values) {
  StringList args = {std::to_string(known_mean), std::to_string(n_total)};
  append_number_args(args, known_values);
  run_operation("missing_value_from_mean", args);
}
inline void findMissingValueFromMean(double known_mean, int n_total, std::initializer_list<double> known_values) {
  findMissingValueFromMean(known_mean, n_total, make_number_list(known_values));
}

inline void findSampleProportion(double successes, int total_count) {
  run_operation("sample_proportion", {std::to_string(successes), std::to_string(total_count)});
}

inline void findCountFromProportion(int total_count, double proportion) {
  run_operation("count_from_proportion", {std::to_string(total_count), std::to_string(proportion)});
}

inline void findPercentChange(double old_value, double new_value) {
  run_operation("percent_change", {std::to_string(old_value), std::to_string(new_value)});
}

inline void findPopulationZScore(double x, double mu, double sigma) {
  run_operation("z_score_population", {std::to_string(x), std::to_string(mu), std::to_string(sigma)});
}

inline void findSampleZScore(double x, double x_bar, double s) {
  run_operation("z_score_sample", {std::to_string(x), std::to_string(x_bar), std::to_string(s)});
}

inline void findValueFromPopulationZScore(double z, double mu, double sigma) {
  run_operation("value_from_z_population", {std::to_string(z), std::to_string(mu), std::to_string(sigma)});
}

inline void findValueFromSampleZScore(double z, double x_bar, double s) {
  run_operation("value_from_z_sample", {std::to_string(z), std::to_string(x_bar), std::to_string(s)});
}

inline void findChebyshevByK(double k) { run_operation("chebyshev_k", {std::to_string(k)}); }

inline void findChebyshevByInterval(double mean_value, double sd_value, double lower_bound, double upper_bound) {
  run_operation(
    "chebyshev_interval",
    {std::to_string(mean_value), std::to_string(sd_value), std::to_string(lower_bound), std::to_string(upper_bound)}
  );
}

inline void findEmpiricalRule(const NumberList& data_set) { run_vector_operation("empirical_rule", data_set); }
inline void findEmpiricalRule(std::initializer_list<double> data_set) { findEmpiricalRule(make_number_list(data_set)); }

inline void findEmpiricalRuleFromSummary(
  double mean_value,
  double sd_value,
  const std::string& event,
  double value1
) {
  run_operation(
    "empirical_rule_summary",
    {event, std::to_string(mean_value), std::to_string(sd_value), std::to_string(value1)}
  );
}

inline void findEmpiricalRuleFromSummary(
  double mean_value,
  double sd_value,
  const std::string& event,
  double value1,
  double value2
) {
  run_operation(
    "empirical_rule_summary",
    {event, std::to_string(mean_value), std::to_string(sd_value), std::to_string(value1), std::to_string(value2)}
  );
}

inline void findGroupedMidpoints(const NumberList& lower, const NumberList& upper) {
  run_operation("grouped_midpoints", interleave_pairs(lower, upper));
}

inline void findGroupedMidpointAt(const NumberList& lower, const NumberList& upper, int class_index) {
  StringList args = {std::to_string(class_index)};
  StringList pair_args = interleave_pairs(lower, upper);
  args.insert(args.end(), pair_args.begin(), pair_args.end());
  run_operation("grouped_midpoint_at", args);
}

inline void findGroupedSampleStats(const NumberList& lower, const NumberList& upper, const NumberList& freq) {
  run_operation("grouped_sample_stats", interleave_triplets(lower, upper, freq));
}
inline void findGroupedPopulationStats(const NumberList& lower, const NumberList& upper, const NumberList& freq) {
  run_operation("grouped_population_stats", interleave_triplets(lower, upper, freq));
}
inline void findGroupedReplicationStats(const NumberList& lower, const NumberList& upper, const NumberList& freq) {
  run_operation("grouped_replication_stats", interleave_triplets(lower, upper, freq));
}

inline void findComplementRule(double p_a) { run_operation("complement_rule", {std::to_string(p_a)}); }

inline void findAdditionRule(double p_a, double p_b, double p_a_and_b) {
  run_operation("addition_rule", {std::to_string(p_a), std::to_string(p_b), std::to_string(p_a_and_b)});
}

inline void findIndependentMultiplication(const NumberList& probabilities) {
  run_vector_operation("multiplication_independent", probabilities);
}
inline void findIndependentMultiplication(std::initializer_list<double> probabilities) {
  findIndependentMultiplication(make_number_list(probabilities));
}

inline void findDependentMultiplication(const NumberList& probabilities) {
  run_vector_operation("multiplication_dependent", probabilities);
}
inline void findDependentMultiplication(std::initializer_list<double> probabilities) {
  findDependentMultiplication(make_number_list(probabilities));
}

inline void findIndependenceCheck(double p_a, double p_b, double p_a_and_b) {
  run_operation("independence_check", {std::to_string(p_a), std::to_string(p_b), std::to_string(p_a_and_b)});
}

inline void findBayesTheorem(double p_a, double p_b_given_a, double p_b_given_not_a) {
  run_operation("bayes_theorem", {std::to_string(p_a), std::to_string(p_b_given_a), std::to_string(p_b_given_not_a)});
}

inline void findFactorial(int n) { run_operation("factorial", {std::to_string(n)}); }
inline void findPermutation(int n, int r) { run_operation("permutation", {std::to_string(n), std::to_string(r)}); }
inline void findCombination(int n, int r) { run_operation("combination", {std::to_string(n), std::to_string(r)}); }
inline void findStringsWithRepetition(int symbol_count, int length_value) {
  run_operation("strings_with_repetition", {std::to_string(symbol_count), std::to_string(length_value)});
}
inline void findAtLeastOneCount(int total_choices, int length_value, int no_target_choices) {
  run_operation(
    "at_least_one_count",
    {std::to_string(total_choices), std::to_string(length_value), std::to_string(no_target_choices)}
  );
}

inline void findDiscreteDistributionStats(const NumberList& values, const NumberList& probabilities) {
  run_operation("discrete_distribution_stats", interleave_distribution_pairs(values, probabilities));
}
inline void findDiscreteTableProbability(
  const std::string& event,
  double value1,
  const NumberList& values,
  const NumberList& probabilities
) {
  StringList args = {event, std::to_string(value1)};
  StringList pairs = interleave_distribution_pairs(values, probabilities);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("discrete_table_probability", args);
}
inline void findDiscreteTableProbability(
  const std::string& event,
  double value1,
  double value2,
  const NumberList& values,
  const NumberList& probabilities
) {
  StringList args = {event, std::to_string(value1), std::to_string(value2)};
  StringList pairs = interleave_distribution_pairs(values, probabilities);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("discrete_table_probability", args);
}

inline void findEmpiricalDistribution(const NumberList& data_set) { run_vector_operation("empirical_distribution", data_set); }
inline void findEmpiricalDistribution(std::initializer_list<double> data_set) { findEmpiricalDistribution(make_number_list(data_set)); }

inline void findGeometricStats(double p_success) { run_operation("geometric_stats", {std::to_string(p_success)}); }
inline void findGeometricProbability(const std::string& event, double p_success, int x_value) {
  run_operation("geometric_probability", {event, std::to_string(p_success), std::to_string(x_value)});
}
inline void findGeometricProbability(const std::string& event, double p_success, int x_value, int value2) {
  run_operation("geometric_probability", {event, std::to_string(p_success), std::to_string(x_value), std::to_string(value2)});
}

inline void findBinomialStats(int n, double p) {
  run_operation("binomial_stats", {std::to_string(n), std::to_string(p)});
}
inline void findBinomialProbability(const std::string& event, int n, double p, int x_value) {
  run_operation("binomial_probability", {event, std::to_string(n), std::to_string(p), std::to_string(x_value)});
}
inline void findBinomialProbability(const std::string& event, int n, double p, int x_value, int value2) {
  run_operation(
    "binomial_probability",
    {event, std::to_string(n), std::to_string(p), std::to_string(x_value), std::to_string(value2)}
  );
}
inline void findBinomialQuantile(double probability, int n, double p) {
  run_operation("binomial_quantile", {std::to_string(probability), std::to_string(n), std::to_string(p)});
}

inline void findPoissonStats(double lambda) { run_operation("poisson_stats", {std::to_string(lambda)}); }
inline void findPoissonProbability(const std::string& event, double lambda, int x_value) {
  run_operation("poisson_probability", {event, std::to_string(lambda), std::to_string(x_value)});
}
inline void findPoissonProbability(const std::string& event, double lambda, int x_value, int value2) {
  run_operation("poisson_probability", {event, std::to_string(lambda), std::to_string(x_value), std::to_string(value2)});
}
inline void findPoissonQuantile(double probability, double lambda) {
  run_operation("poisson_quantile", {std::to_string(probability), std::to_string(lambda)});
}

inline void findHypergeometricStats(int successes, int failures, int draws) {
  run_operation("hypergeometric_stats", {std::to_string(successes), std::to_string(failures), std::to_string(draws)});
}
inline void findHypergeometricProbability(const std::string& event, int successes, int failures, int draws, int x_value) {
  run_operation(
    "hypergeometric_probability",
    {event, std::to_string(successes), std::to_string(failures), std::to_string(draws), std::to_string(x_value)}
  );
}
inline void findHypergeometricProbability(
  const std::string& event,
  int successes,
  int failures,
  int draws,
  int x_value,
  int value2
) {
  run_operation(
    "hypergeometric_probability",
    {
      event,
      std::to_string(successes),
      std::to_string(failures),
      std::to_string(draws),
      std::to_string(x_value),
      std::to_string(value2)
    }
  );
}

inline void findUniformDiscreteStats(const NumberList& values) { run_vector_operation("uniform_discrete_stats", values); }
inline void findUniformDiscreteStats(std::initializer_list<double> values) { findUniformDiscreteStats(make_number_list(values)); }

inline void findNormalProbability(const std::string& event, double mu, double sigma, double x_value) {
  run_operation("normal_probability", {event, std::to_string(mu), std::to_string(sigma), std::to_string(x_value)});
}
inline void findNormalProbability(const std::string& event, double mu, double sigma, double lower, double upper) {
  run_operation(
    "normal_probability",
    {event, std::to_string(mu), std::to_string(sigma), std::to_string(lower), std::to_string(upper)}
  );
}
inline void findNormalDensity(double mu, double sigma, double x_value) {
  run_operation("normal_density", {std::to_string(mu), std::to_string(sigma), std::to_string(x_value)});
}
inline void findNormalQuantile(double probability) { run_operation("normal_quantile", {std::to_string(probability)}); }
inline void findNormalQuantile(double probability, double mu, double sigma) {
  run_operation("normal_quantile", {std::to_string(probability), std::to_string(mu), std::to_string(sigma)});
}

inline void findStandardNormalProbability(const std::string& event, double x_value) {
  run_operation("standard_normal_probability", {event, std::to_string(x_value)});
}
inline void findStandardNormalProbability(const std::string& event, double lower, double upper) {
  run_operation("standard_normal_probability", {event, std::to_string(lower), std::to_string(upper)});
}

inline void findUniformContinuousDensity(double min_value, double max_value, double x_value) {
  run_operation(
    "uniform_continuous_density",
    {std::to_string(min_value), std::to_string(max_value), std::to_string(x_value)}
  );
}
inline void findUniformContinuousProbability(const std::string& event, double min_value, double max_value, double x_value) {
  run_operation(
    "uniform_continuous_probability",
    {event, std::to_string(min_value), std::to_string(max_value), std::to_string(x_value)}
  );
}
inline void findUniformContinuousProbability(
  const std::string& event,
  double min_value,
  double max_value,
  double lower,
  double upper
) {
  run_operation(
    "uniform_continuous_probability",
    {event, std::to_string(min_value), std::to_string(max_value), std::to_string(lower), std::to_string(upper)}
  );
}
inline void findUniformContinuousQuantile(double probability, double min_value, double max_value) {
  run_operation(
    "uniform_continuous_quantile",
    {std::to_string(probability), std::to_string(min_value), std::to_string(max_value)}
  );
}

inline void findExponentialDensity(double rate, double x_value) {
  run_operation("exponential_density", {std::to_string(rate), std::to_string(x_value)});
}
inline void findExponentialProbability(const std::string& event, double rate, double x_value) {
  run_operation("exponential_probability", {event, std::to_string(rate), std::to_string(x_value)});
}
inline void findExponentialProbability(const std::string& event, double rate, double lower, double upper) {
  run_operation("exponential_probability", {event, std::to_string(rate), std::to_string(lower), std::to_string(upper)});
}
inline void findExponentialQuantile(double probability, double rate) {
  run_operation("exponential_quantile", {std::to_string(probability), std::to_string(rate)});
}

inline void findTProbability(const std::string& event, int df, double x_value) {
  run_operation("t_probability", {event, std::to_string(df), std::to_string(x_value)});
}
inline void findTProbability(const std::string& event, int df, double lower, double upper) {
  run_operation("t_probability", {event, std::to_string(df), std::to_string(lower), std::to_string(upper)});
}
inline void findTQuantile(double probability, int df) {
  run_operation("t_quantile", {std::to_string(probability), std::to_string(df)});
}

inline void findChiSquareProbability(const std::string& event, int df, double x_value) {
  run_operation("chi_square_probability", {event, std::to_string(df), std::to_string(x_value)});
}
inline void findChiSquareProbability(const std::string& event, int df, double lower, double upper) {
  run_operation(
    "chi_square_probability",
    {event, std::to_string(df), std::to_string(lower), std::to_string(upper)}
  );
}
inline void findChiSquareQuantile(double probability, int df) {
  run_operation("chi_square_quantile", {std::to_string(probability), std::to_string(df)});
}

inline void findFProbability(const std::string& event, int df1, int df2, double x_value) {
  run_operation(
    "f_probability",
    {event, std::to_string(df1), std::to_string(df2), std::to_string(x_value)}
  );
}
inline void findFProbability(const std::string& event, int df1, int df2, double lower, double upper) {
  run_operation(
    "f_probability",
    {event, std::to_string(df1), std::to_string(df2), std::to_string(lower), std::to_string(upper)}
  );
}
inline void findFQuantile(double probability, int df1, int df2) {
  run_operation("f_quantile", {std::to_string(probability), std::to_string(df1), std::to_string(df2)});
}

inline void findSamplingMeanProbability(const std::string& event, double mu, double sigma, int n, double x_value) {
  run_operation(
    "sampling_mean_probability",
    {event, std::to_string(mu), std::to_string(sigma), std::to_string(n), std::to_string(x_value)}
  );
}
inline void findSamplingMeanProbability(
  const std::string& event,
  double mu,
  double sigma,
  int n,
  double lower,
  double upper
) {
  run_operation(
    "sampling_mean_probability",
    {event, std::to_string(mu), std::to_string(sigma), std::to_string(n), std::to_string(lower), std::to_string(upper)}
  );
}

inline void findSamplingProportionProbability(const std::string& event, double p, int n, double phat_value) {
  run_operation(
    "sampling_proportion_probability",
    {event, std::to_string(p), std::to_string(n), std::to_string(phat_value)}
  );
}
inline void findSamplingProportionProbability(
  const std::string& event,
  double p,
  int n,
  double lower,
  double upper
) {
  run_operation(
    "sampling_proportion_probability",
    {event, std::to_string(p), std::to_string(n), std::to_string(lower), std::to_string(upper)}
  );
}

inline void findMeanCIKnownSigma(double xbar, double sigma, int n, double conf_level) {
  run_operation(
    "mean_ci_known_sigma",
    {std::to_string(xbar), std::to_string(sigma), std::to_string(n), std::to_string(conf_level)}
  );
}
inline void findMeanCIUnknownSigma(double xbar, double s, int n, double conf_level) {
  run_operation(
    "mean_ci_unknown_sigma",
    {std::to_string(xbar), std::to_string(s), std::to_string(n), std::to_string(conf_level)}
  );
}
inline void findMarginOfErrorFromInterval(double lower, double upper) {
  run_operation("margin_of_error_from_interval", {std::to_string(lower), std::to_string(upper)});
}
inline void findMeanSampleSizeKnownSigma(double sigma, double margin_error, double conf_level) {
  run_operation(
    "sample_size_mean_known_sigma",
    {std::to_string(sigma), std::to_string(margin_error), std::to_string(conf_level)}
  );
}
inline void findProportionCI(double x, int n, double conf_level) {
  run_operation("proportion_ci", {std::to_string(x), std::to_string(n), std::to_string(conf_level)});
}
inline void findProportionSampleSize(double p_hat, double margin_error, double conf_level) {
  run_operation(
    "sample_size_proportion",
    {std::to_string(p_hat), std::to_string(margin_error), std::to_string(conf_level)}
  );
}

inline void runOneSampleZTest(double xbar, double mu0, double sigma, int n, double alpha, const std::string& tail) {
  run_operation(
    "one_sample_z_test",
    {std::to_string(xbar), std::to_string(mu0), std::to_string(sigma), std::to_string(n), std::to_string(alpha), tail}
  );
}
inline void runOneSampleTTest(double xbar, double mu0, double s, int n, double alpha, const std::string& tail) {
  run_operation(
    "one_sample_t_test",
    {std::to_string(xbar), std::to_string(mu0), std::to_string(s), std::to_string(n), std::to_string(alpha), tail}
  );
}
inline void runOneProportionZTest(double p_hat, double p0, int n, double alpha, const std::string& tail) {
  run_operation(
    "one_proportion_z_test",
    {std::to_string(p_hat), std::to_string(p0), std::to_string(n), std::to_string(alpha), tail}
  );
}
inline void checkNormalApproximation(int n, double p0) {
  run_operation("normal_approximation_check", {std::to_string(n), std::to_string(p0)});
}
inline void findVarianceCISigma(int n, double s, double conf_level) {
  run_operation("variance_ci_sigma", {std::to_string(n), std::to_string(s), std::to_string(conf_level)});
}
inline void findVarianceCIVariance(int n, double s2, double conf_level) {
  run_operation("variance_ci_variance", {std::to_string(n), std::to_string(s2), std::to_string(conf_level)});
}
inline void runVarianceTest(int n, double s, double sigma0, double alpha, const std::string& tail) {
  run_operation(
    "variance_test",
    {std::to_string(n), std::to_string(s), std::to_string(sigma0), std::to_string(alpha), tail}
  );
}

inline void runTwoSampleZTest(
  double xbar1,
  double xbar2,
  double sigma1,
  double sigma2,
  int n1,
  int n2,
  double alpha,
  const std::string& tail
) {
  run_operation(
    "two_sample_z_test",
    {
      std::to_string(xbar1),
      std::to_string(xbar2),
      std::to_string(sigma1),
      std::to_string(sigma2),
      std::to_string(n1),
      std::to_string(n2),
      std::to_string(alpha),
      tail
    }
  );
}

inline void findTwoSampleZCI(
  double xbar1,
  double xbar2,
  double sigma1,
  double sigma2,
  int n1,
  int n2,
  double conf_level
) {
  run_operation(
    "two_sample_z_ci",
    {
      std::to_string(xbar1),
      std::to_string(xbar2),
      std::to_string(sigma1),
      std::to_string(sigma2),
      std::to_string(n1),
      std::to_string(n2),
      std::to_string(conf_level)
    }
  );
}

inline void runWelchTTest(
  double xbar1,
  double xbar2,
  double s1,
  double s2,
  int n1,
  int n2,
  double alpha,
  const std::string& tail
) {
  run_operation(
    "welch_t_test",
    {
      std::to_string(xbar1),
      std::to_string(xbar2),
      std::to_string(s1),
      std::to_string(s2),
      std::to_string(n1),
      std::to_string(n2),
      std::to_string(alpha),
      tail
    }
  );
}

inline void findWelchTCI(
  double xbar1,
  double xbar2,
  double s1,
  double s2,
  int n1,
  int n2,
  double conf_level
) {
  run_operation(
    "welch_t_ci",
    {
      std::to_string(xbar1),
      std::to_string(xbar2),
      std::to_string(s1),
      std::to_string(s2),
      std::to_string(n1),
      std::to_string(n2),
      std::to_string(conf_level)
    }
  );
}

inline void runPooledTTest(
  double xbar1,
  double xbar2,
  double s1,
  double s2,
  int n1,
  int n2,
  double alpha,
  const std::string& tail
) {
  run_operation(
    "pooled_t_test",
    {
      std::to_string(xbar1),
      std::to_string(xbar2),
      std::to_string(s1),
      std::to_string(s2),
      std::to_string(n1),
      std::to_string(n2),
      std::to_string(alpha),
      tail
    }
  );
}

inline void findPooledTCI(
  double xbar1,
  double xbar2,
  double s1,
  double s2,
  int n1,
  int n2,
  double conf_level
) {
  run_operation(
    "pooled_t_ci",
    {
      std::to_string(xbar1),
      std::to_string(xbar2),
      std::to_string(s1),
      std::to_string(s2),
      std::to_string(n1),
      std::to_string(n2),
      std::to_string(conf_level)
    }
  );
}

inline void findClaimTail(
  const std::string& claim_text,
  const std::string& difference_definition = "second_minus_first"
) {
  run_operation("infer_claim_tail", {claim_text, difference_definition});
}

inline void findProblemHypotheses(const std::string& problem_text) {
  run_operation("problem_hypotheses", {problem_text});
}

inline void findProblemStepAnswers(const std::string& problem_text) {
  run_operation("problem_step_answers", {problem_text});
}

inline void findHypothesisConclusion(
  double alpha,
  const std::string& decision,
  const std::string& claim_text,
  const std::string& claim_location = ""
) {
  if (claim_location.empty()) {
    run_operation("hypothesis_conclusion", {std::to_string(alpha), decision, claim_text});
  } else {
    run_operation("hypothesis_conclusion", {std::to_string(alpha), decision, claim_text, claim_location});
  }
}

inline void findPairedDifferenceMean(const NumberList& before, const NumberList& after) {
  run_operation("paired_difference_mean", interleave_pairs(before, after));
}
inline void findPairedDifferenceMean(std::initializer_list<double> before, std::initializer_list<double> after) {
  findPairedDifferenceMean(make_number_list(before), make_number_list(after));
}

inline void runPairedTTest(const NumberList& before, const NumberList& after, double alpha, const std::string& tail) {
  StringList args = {std::to_string(alpha), tail};
  StringList pairs = interleave_pairs(before, after);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("paired_t_test", args);
}
inline void runPairedTTest(
  std::initializer_list<double> before,
  std::initializer_list<double> after,
  double alpha,
  const std::string& tail
) {
  runPairedTTest(make_number_list(before), make_number_list(after), alpha, tail);
}

inline void solvePairedProblemFromText(
  const NumberList& before,
  const NumberList& after,
  const std::string& problem_text
) {
  StringList args = {problem_text};
  StringList pairs = interleave_pairs(before, after);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("paired_problem_step_answers", args);
}
inline void solvePairedProblemFromText(
  std::initializer_list<double> before,
  std::initializer_list<double> after,
  const std::string& problem_text
) {
  solvePairedProblemFromText(make_number_list(before), make_number_list(after), problem_text);
}

inline void runPairedTTestFromProblem(
  const NumberList& before,
  const NumberList& after,
  const std::string& problem_text
) {
  StringList args = {problem_text};
  StringList pairs = interleave_pairs(before, after);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("paired_t_test_problem", args);
}
inline void runPairedTTestFromProblem(
  std::initializer_list<double> before,
  std::initializer_list<double> after,
  const std::string& problem_text
) {
  runPairedTTestFromProblem(make_number_list(before), make_number_list(after), problem_text);
}

inline void runPairedTTestFromClaim(
  const NumberList& before,
  const NumberList& after,
  double alpha,
  const std::string& claim_text,
  const std::string& difference_definition = "second_minus_first"
) {
  StringList args = {std::to_string(alpha), claim_text, difference_definition};
  StringList pairs = interleave_pairs(before, after);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("paired_t_test_claim", args);
}
inline void runPairedTTestFromClaim(
  std::initializer_list<double> before,
  std::initializer_list<double> after,
  double alpha,
  const std::string& claim_text,
  const std::string& difference_definition = "second_minus_first"
) {
  runPairedTTestFromClaim(make_number_list(before), make_number_list(after), alpha, claim_text, difference_definition);
}

inline void findPairedTCI(const NumberList& before, const NumberList& after, double conf_level) {
  StringList args = {std::to_string(conf_level)};
  StringList pairs = interleave_pairs(before, after);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("paired_t_ci", args);
}
inline void findPairedTCI(
  std::initializer_list<double> before,
  std::initializer_list<double> after,
  double conf_level
) {
  findPairedTCI(make_number_list(before), make_number_list(after), conf_level);
}

inline void findTwoProportionCI(double x1, int n1, double x2, int n2, double conf_level) {
  run_operation(
    "two_proportion_ci",
    {std::to_string(x1), std::to_string(n1), std::to_string(x2), std::to_string(n2), std::to_string(conf_level)}
  );
}
inline void runTwoProportionZTest(double x1, int n1, double x2, int n2, double alpha, const std::string& tail) {
  run_operation(
    "two_proportion_z_test",
    {std::to_string(x1), std::to_string(n1), std::to_string(x2), std::to_string(n2), std::to_string(alpha), tail}
  );
}

inline void findCorrelation(const NumberList& x, const NumberList& y) { run_two_vector_operation("correlation", x, y); }
inline void findCorrelation(std::initializer_list<double> x, std::initializer_list<double> y) {
  findCorrelation(make_number_list(x), make_number_list(y));
}

inline void findRegressionLine(const NumberList& x, const NumberList& y) { run_two_vector_operation("regression_line", x, y); }
inline void findRegressionLine(std::initializer_list<double> x, std::initializer_list<double> y) {
  findRegressionLine(make_number_list(x), make_number_list(y));
}

inline void findRegressionPredictionFromCoefficients(double b0, double b1, double x_value) {
  run_operation("regression_predict_coeffs", {std::to_string(b0), std::to_string(b1), std::to_string(x_value)});
}

inline void findRegressionPredictionRow(double observed, double x_value, double b0, double b1) {
  run_operation(
    "regression_prediction_row",
    {std::to_string(observed), std::to_string(x_value), std::to_string(b0), std::to_string(b1)}
  );
}

inline void findRegressionPredictionTable(const NumberList& observed, const NumberList& x, double b0, double b1) {
  ensure_same_size(observed, x, "Regression table");

  StringList args = {std::to_string(b0), std::to_string(b1)};
  StringList pairs = interleave_pairs(observed, x);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("regression_prediction_table", args);
}
inline void findRegressionPredictionTable(
  std::initializer_list<double> observed,
  std::initializer_list<double> x,
  double b0,
  double b1
) {
  findRegressionPredictionTable(make_number_list(observed), make_number_list(x), b0, b1);
}

inline void findRegressionPredictionFromData(const NumberList& x, const NumberList& y, double x_new) {
  StringList args = {std::to_string(x_new)};
  StringList pairs = interleave_pairs(x, y);
  args.insert(args.end(), pairs.begin(), pairs.end());
  run_operation("regression_predict_data", args);
}
inline void findRegressionPredictionFromData(
  std::initializer_list<double> x,
  std::initializer_list<double> y,
  double x_new
) {
  findRegressionPredictionFromData(make_number_list(x), make_number_list(y), x_new);
}

inline void findRegressionRSquared(const NumberList& x, const NumberList& y) { run_two_vector_operation("regression_r_squared", x, y); }
inline void findRegressionRSquared(std::initializer_list<double> x, std::initializer_list<double> y) {
  findRegressionRSquared(make_number_list(x), make_number_list(y));
}

inline void findRegressionError(double observed, double predicted) {
  run_operation("regression_error", {std::to_string(observed), std::to_string(predicted)});
}
inline void findRegressionError(double observed, double b0, double b1, double x_value) {
  run_operation(
    "regression_error",
    {std::to_string(observed), std::to_string(b0), std::to_string(b1), std::to_string(x_value)}
  );
}

inline void findRegressionSSE(const NumberList& x, const NumberList& y) { run_two_vector_operation("regression_sse", x, y); }
inline void findRegressionSSE(std::initializer_list<double> x, std::initializer_list<double> y) {
  findRegressionSSE(make_number_list(x), make_number_list(y));
}

}  // namespace stats_bridge

#endif
