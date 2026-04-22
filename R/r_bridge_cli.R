get_script_directory <- function() {
  command_line_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_line_args, value = TRUE)

  if (length(file_arg) > 0) {
    file_path <- sub("^--file=", "", file_arg[1])
    file_path <- gsub("~\\+~", " ", file_path)
    return(dirname(normalizePath(file_path)))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }

  normalizePath(getwd())
}

script_dir <- get_script_directory()
project_root <- dirname(script_dir)
CODEX_PROJECT_ROOT <- project_root
source(file.path(project_root, "main.R"))

parse_numeric_args <- function(args) {
  numeric_args <- suppressWarnings(as.numeric(args))

  if (length(numeric_args) > 0 && any(is.na(numeric_args))) {
    stop("All calculation inputs must be numeric unless the operation expects an event or tail string.", call. = FALSE)
  }

  numeric_args
}

parse_pair_matrix <- function(args) {
  split_interleaved_pairs(parse_numeric_args(args))
}

parse_triplet_matrix <- function(args) {
  split_interleaved_triplets(parse_numeric_args(args))
}

parse_distribution_pair_matrix <- function(args) {
  split_interleaved_distribution_pairs(parse_numeric_args(args))
}

parse_label_count_pairs <- function(args) {
  if (length(args) < 2 || length(args) %% 2 != 0) {
    stop("Label/count operations require alternating label and count arguments.", call. = FALSE)
  }

  list(
    labels = args[seq(1, length(args), by = 2)],
    counts = parse_numeric_args(args[seq(2, length(args), by = 2)])
  )
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript R/r_bridge_cli.R <operation> [operation arguments...]", call. = FALSE)
}

operation <- args[1]
op_args <- args[-1]

calculation <- switch(
  operation,
  sample_mean = sample_mean(parse_numeric_args(op_args)),
  population_mean = population_mean(parse_numeric_args(op_args)),
  median = median_worked(parse_numeric_args(op_args)),
  mode = mode_worked(parse_numeric_args(op_args)),
  range = range_worked(parse_numeric_args(op_args)),
  sample_variance = sample_variance(parse_numeric_args(op_args)),
  population_variance = population_variance(parse_numeric_args(op_args)),
  sample_sd = sample_sd(parse_numeric_args(op_args)),
  population_sd = population_sd(parse_numeric_args(op_args)),
  trimmed_mean = {
    numeric_args <- parse_numeric_args(op_args)
    trimmed_mean_worked(numeric_args[-1], trim = numeric_args[1])
  },
  mean_absolute_deviation = mean_absolute_deviation_worked(parse_numeric_args(op_args)),
  coefficient_variation = coefficient_of_variation_worked(parse_numeric_args(op_args)),
  quantile_type2 = {
    numeric_args <- parse_numeric_args(op_args)
    quantile_type2_worked(numeric_args[-1], probability = numeric_args[1])
  },
  percentile_rank = {
    numeric_args <- parse_numeric_args(op_args)
    percentile_rank_worked(numeric_args[-1], value = numeric_args[1])
  },
  iqr_type2 = iqr_type2_worked(parse_numeric_args(op_args)),
  outlier_fences = outlier_fences_worked(parse_numeric_args(op_args)),
  frequency_table = frequency_table_worked(parse_numeric_args(op_args)),
  most_frequent_category = {
    labeled_counts <- parse_label_count_pairs(op_args)
    most_frequent_category_worked(labeled_counts$labels, labeled_counts$counts)
  },
  category_proportion = {
    target_label <- op_args[1]
    labeled_counts <- parse_label_count_pairs(op_args[-1])
    category_proportion_worked(target_label, labeled_counts$labels, labeled_counts$counts)
  },
  weighted_mean = {
    pair_matrix <- parse_pair_matrix(op_args)
    weighted_mean_worked(pair_matrix[, 1], pair_matrix[, 2])
  },
  missing_value_from_mean = {
    numeric_args <- parse_numeric_args(op_args)
    missing_value_from_mean_worked(
      known_mean = numeric_args[1],
      known_values = numeric_args[-c(1, 2)],
      n_total = numeric_args[2]
    )
  },
  sample_proportion = {
    numeric_args <- parse_numeric_args(op_args)
    sample_proportion_worked(numeric_args[1], numeric_args[2])
  },
  count_from_proportion = {
    numeric_args <- parse_numeric_args(op_args)
    count_from_proportion_worked(numeric_args[1], numeric_args[2])
  },
  percent_change = {
    numeric_args <- parse_numeric_args(op_args)
    percent_change_worked(numeric_args[1], numeric_args[2])
  },
  z_score_population = {
    numeric_args <- parse_numeric_args(op_args)
    z_score_worked(numeric_args[1], numeric_args[2], numeric_args[3], mean_symbol = "mu", sd_symbol = "sigma")
  },
  z_score_sample = {
    numeric_args <- parse_numeric_args(op_args)
    z_score_worked(numeric_args[1], numeric_args[2], numeric_args[3], mean_symbol = "x_bar", sd_symbol = "s")
  },
  value_from_z_population = {
    numeric_args <- parse_numeric_args(op_args)
    value_from_z_worked(numeric_args[1], numeric_args[2], numeric_args[3], mean_symbol = "mu", sd_symbol = "sigma")
  },
  value_from_z_sample = {
    numeric_args <- parse_numeric_args(op_args)
    value_from_z_worked(numeric_args[1], numeric_args[2], numeric_args[3], mean_symbol = "x_bar", sd_symbol = "s")
  },
  chebyshev_k = chebyshev_k_worked(parse_numeric_args(op_args)[1]),
  chebyshev_interval = {
    numeric_args <- parse_numeric_args(op_args)
    chebyshev_interval_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4])
  },
  empirical_rule = empirical_rule_worked(parse_numeric_args(op_args)),
  empirical_rule_summary = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (event == "between") {
      empirical_rule_summary_worked(numeric_args[1], numeric_args[2], event, numeric_args[3], numeric_args[4])
    } else {
      empirical_rule_summary_worked(numeric_args[1], numeric_args[2], event, numeric_args[3])
    }
  },
  grouped_midpoints = {
    pair_matrix <- parse_pair_matrix(op_args)
    grouped_midpoints_worked(pair_matrix[, 1], pair_matrix[, 2])
  },
  grouped_midpoint_at = {
    numeric_args <- parse_numeric_args(op_args)
    pair_matrix <- split_interleaved_pairs(numeric_args[-1])
    grouped_midpoint_at_worked(pair_matrix[, 1], pair_matrix[, 2], numeric_args[1])
  },
  grouped_sample_stats = {
    triplets <- parse_triplet_matrix(op_args)
    grouped_sample_stats_worked(triplets[, 1], triplets[, 2], triplets[, 3])
  },
  grouped_population_stats = {
    triplets <- parse_triplet_matrix(op_args)
    grouped_population_stats_worked(triplets[, 1], triplets[, 2], triplets[, 3])
  },
  grouped_replication_stats = {
    triplets <- parse_triplet_matrix(op_args)
    grouped_replication_stats_worked(triplets[, 1], triplets[, 2], triplets[, 3])
  },
  complement_rule = complement_rule_worked(parse_numeric_args(op_args)[1]),
  addition_rule = {
    numeric_args <- parse_numeric_args(op_args)
    addition_rule_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  multiplication_independent = multiplication_independent_worked(parse_numeric_args(op_args)),
  multiplication_dependent = multiplication_dependent_worked(parse_numeric_args(op_args)),
  independence_check = {
    numeric_args <- parse_numeric_args(op_args)
    independence_check_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  bayes_theorem = {
    numeric_args <- parse_numeric_args(op_args)
    bayes_theorem_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  factorial = factorial_worked(parse_numeric_args(op_args)[1]),
  permutation = {
    numeric_args <- parse_numeric_args(op_args)
    permutation_worked(numeric_args[1], numeric_args[2])
  },
  combination = {
    numeric_args <- parse_numeric_args(op_args)
    combination_worked(numeric_args[1], numeric_args[2])
  },
  strings_with_repetition = {
    numeric_args <- parse_numeric_args(op_args)
    strings_with_repetition_worked(numeric_args[1], numeric_args[2])
  },
  at_least_one_count = {
    numeric_args <- parse_numeric_args(op_args)
    at_least_one_count_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  discrete_distribution_stats = {
    pair_matrix <- parse_distribution_pair_matrix(op_args)
    discrete_distribution_stats_worked(pair_matrix[, 1], pair_matrix[, 2])
  },
  discrete_table_probability = {
    event <- op_args[1]
    if (event == "between") {
      numeric_args <- parse_numeric_args(op_args[-1])
      pair_matrix <- split_interleaved_distribution_pairs(numeric_args[-c(1, 2)])
      discrete_table_probability_worked(pair_matrix[, 1], pair_matrix[, 2], event, numeric_args[1], numeric_args[2])
    } else {
      numeric_args <- parse_numeric_args(op_args[-1])
      pair_matrix <- split_interleaved_distribution_pairs(numeric_args[-1])
      discrete_table_probability_worked(pair_matrix[, 1], pair_matrix[, 2], event, numeric_args[1])
    }
  },
  empirical_distribution = empirical_distribution_worked(parse_numeric_args(op_args)),
  geometric_stats = geometric_stats_worked(parse_numeric_args(op_args)[1]),
  geometric_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 2) {
      geometric_probability_worked(numeric_args[1], event, numeric_args[2])
    } else {
      geometric_probability_worked(numeric_args[1], event, numeric_args[2], numeric_args[3])
    }
  },
  binomial_stats = {
    numeric_args <- parse_numeric_args(op_args)
    binomial_stats_worked(numeric_args[1], numeric_args[2])
  },
  binomial_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 3) {
      binomial_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3])
    } else {
      binomial_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3], numeric_args[4])
    }
  },
  binomial_quantile = {
    numeric_args <- parse_numeric_args(op_args)
    binomial_quantile_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  poisson_stats = poisson_stats_worked(parse_numeric_args(op_args)[1]),
  poisson_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 2) {
      poisson_probability_worked(numeric_args[1], event, numeric_args[2])
    } else {
      poisson_probability_worked(numeric_args[1], event, numeric_args[2], numeric_args[3])
    }
  },
  poisson_quantile = {
    numeric_args <- parse_numeric_args(op_args)
    poisson_quantile_worked(numeric_args[1], numeric_args[2])
  },
  hypergeometric_stats = {
    numeric_args <- parse_numeric_args(op_args)
    hypergeometric_stats_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  hypergeometric_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 4) {
      hypergeometric_probability_worked(numeric_args[1], numeric_args[2], numeric_args[3], event, numeric_args[4])
    } else {
      hypergeometric_probability_worked(numeric_args[1], numeric_args[2], numeric_args[3], event, numeric_args[4], numeric_args[5])
    }
  },
  uniform_discrete_stats = uniform_discrete_stats_worked(parse_numeric_args(op_args)),
  normal_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 3) {
      normal_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3])
    } else {
      normal_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3], numeric_args[4])
    }
  },
  normal_density = {
    numeric_args <- parse_numeric_args(op_args)
    normal_density_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  normal_quantile = {
    numeric_args <- parse_numeric_args(op_args)
    if (length(numeric_args) == 1) {
      normal_quantile_worked(numeric_args[1])
    } else {
      normal_quantile_worked(numeric_args[1], numeric_args[2], numeric_args[3])
    }
  },
  standard_normal_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 1) {
      standard_normal_probability_worked(event, numeric_args[1])
    } else {
      standard_normal_probability_worked(event, numeric_args[1], numeric_args[2])
    }
  },
  uniform_continuous_density = {
    numeric_args <- parse_numeric_args(op_args)
    uniform_continuous_density_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  uniform_continuous_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 3) {
      uniform_continuous_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3])
    } else {
      uniform_continuous_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3], numeric_args[4])
    }
  },
  uniform_continuous_quantile = {
    numeric_args <- parse_numeric_args(op_args)
    uniform_continuous_quantile_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  exponential_density = {
    numeric_args <- parse_numeric_args(op_args)
    exponential_density_worked(numeric_args[1], numeric_args[2])
  },
  exponential_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 2) {
      exponential_probability_worked(numeric_args[1], event, numeric_args[2])
    } else {
      exponential_probability_worked(numeric_args[1], event, numeric_args[2], numeric_args[3])
    }
  },
  exponential_quantile = {
    numeric_args <- parse_numeric_args(op_args)
    exponential_quantile_worked(numeric_args[1], numeric_args[2])
  },
  t_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 2) {
      t_distribution_probability_worked(numeric_args[1], event, numeric_args[2])
    } else {
      t_distribution_probability_worked(numeric_args[1], event, numeric_args[2], numeric_args[3])
    }
  },
  t_quantile = {
    numeric_args <- parse_numeric_args(op_args)
    t_distribution_quantile_worked(numeric_args[1], numeric_args[2])
  },
  chi_square_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 2) {
      chi_square_probability_worked(numeric_args[1], event, numeric_args[2])
    } else {
      chi_square_probability_worked(numeric_args[1], event, numeric_args[2], numeric_args[3])
    }
  },
  chi_square_quantile = {
    numeric_args <- parse_numeric_args(op_args)
    chi_square_quantile_worked(numeric_args[1], numeric_args[2])
  },
  f_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 3) {
      f_distribution_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3])
    } else {
      f_distribution_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3], numeric_args[4])
    }
  },
  f_quantile = {
    numeric_args <- parse_numeric_args(op_args)
    f_distribution_quantile_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  sampling_mean_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 4) {
      sampling_mean_probability_worked(numeric_args[1], numeric_args[2], numeric_args[3], event, numeric_args[4])
    } else {
      sampling_mean_probability_worked(numeric_args[1], numeric_args[2], numeric_args[3], event, numeric_args[4], numeric_args[5])
    }
  },
  sampling_proportion_probability = {
    event <- op_args[1]
    numeric_args <- parse_numeric_args(op_args[-1])
    if (length(numeric_args) == 3) {
      sampling_proportion_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3])
    } else {
      sampling_proportion_probability_worked(numeric_args[1], numeric_args[2], event, numeric_args[3], numeric_args[4])
    }
  },
  mean_ci_known_sigma = {
    numeric_args <- parse_numeric_args(op_args)
    mean_ci_known_sigma_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4])
  },
  mean_ci_unknown_sigma = {
    numeric_args <- parse_numeric_args(op_args)
    mean_ci_unknown_sigma_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4])
  },
  margin_of_error_from_interval = {
    numeric_args <- parse_numeric_args(op_args)
    margin_of_error_from_interval_worked(numeric_args[1], numeric_args[2])
  },
  sample_size_mean_known_sigma = {
    numeric_args <- parse_numeric_args(op_args)
    sample_size_mean_known_sigma_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  proportion_ci = {
    numeric_args <- parse_numeric_args(op_args)
    proportion_ci_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  sample_size_proportion = {
    numeric_args <- parse_numeric_args(op_args)
    sample_size_proportion_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  one_sample_z_test = {
    numeric_args <- parse_numeric_args(op_args[-length(op_args)])
    one_sample_z_test_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], op_args[length(op_args)])
  },
  one_sample_t_test = {
    numeric_args <- parse_numeric_args(op_args[-length(op_args)])
    one_sample_t_test_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], op_args[length(op_args)])
  },
  one_proportion_z_test = {
    numeric_args <- parse_numeric_args(op_args[-length(op_args)])
    one_proportion_z_test_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], op_args[length(op_args)])
  },
  normal_approximation_check = {
    numeric_args <- parse_numeric_args(op_args)
    normal_approximation_check_worked(numeric_args[1], numeric_args[2])
  },
  variance_ci_sigma = {
    numeric_args <- parse_numeric_args(op_args)
    variance_ci_sigma_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  variance_ci_variance = {
    numeric_args <- parse_numeric_args(op_args)
    variance_ci_variance_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  variance_test = {
    numeric_args <- parse_numeric_args(op_args[-length(op_args)])
    variance_test_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], op_args[length(op_args)])
  },
  two_sample_z_test = {
    numeric_args <- parse_numeric_args(op_args[-length(op_args)])
    two_sample_z_test_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], numeric_args[6], numeric_args[7], op_args[length(op_args)])
  },
  two_sample_z_ci = {
    numeric_args <- parse_numeric_args(op_args)
    two_sample_z_ci_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], numeric_args[6], numeric_args[7])
  },
  welch_t_test = {
    numeric_args <- parse_numeric_args(op_args[-length(op_args)])
    welch_t_test_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], numeric_args[6], numeric_args[7], op_args[length(op_args)])
  },
  welch_t_ci = {
    numeric_args <- parse_numeric_args(op_args)
    welch_t_ci_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], numeric_args[6], numeric_args[7])
  },
  pooled_t_test = {
    numeric_args <- parse_numeric_args(op_args[-length(op_args)])
    pooled_t_test_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], numeric_args[6], numeric_args[7], op_args[length(op_args)])
  },
  pooled_t_ci = {
    numeric_args <- parse_numeric_args(op_args)
    pooled_t_ci_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], numeric_args[6], numeric_args[7])
  },
  infer_claim_tail = {
    if (length(op_args) == 1) {
      infer_claim_tail_worked(op_args[1])
    } else {
      infer_claim_tail_worked(op_args[1], op_args[2])
    }
  },
  hypothesis_conclusion = {
    alpha <- as.numeric(op_args[1])
    decision <- op_args[2]
    claim_text <- op_args[3]

    if (length(op_args) >= 4) {
      hypothesis_conclusion_worked(alpha, decision, claim_text, op_args[4])
    } else {
      hypothesis_conclusion_worked(alpha, decision, claim_text)
    }
  },
  problem_hypotheses = problem_hypotheses_worked(op_args[1]),
  problem_step_answers = problem_step_answers_worked(op_args[1]),
  paired_difference_mean = {
    pair_matrix <- parse_pair_matrix(op_args)
    paired_difference_mean_worked(pair_matrix[, 1], pair_matrix[, 2])
  },
  paired_t_test = {
    alpha <- as.numeric(op_args[1])
    tail <- op_args[2]
    pair_matrix <- parse_pair_matrix(op_args[-c(1, 2)])
    paired_t_test_worked(pair_matrix[, 1], pair_matrix[, 2], alpha, tail)
  },
  paired_t_ci = {
    conf_level <- as.numeric(op_args[1])
    pair_matrix <- parse_pair_matrix(op_args[-1])
    paired_t_ci_worked(pair_matrix[, 1], pair_matrix[, 2], conf_level)
  },
  paired_t_test_claim = {
    alpha <- as.numeric(op_args[1])
    claim_text <- op_args[2]
    difference_definition <- op_args[3]
    pair_matrix <- parse_pair_matrix(op_args[-c(1, 2, 3)])
    paired_t_test_from_claim_worked(pair_matrix[, 1], pair_matrix[, 2], alpha, claim_text, difference_definition)
  },
  paired_t_test_problem = {
    problem_text <- op_args[1]
    pair_matrix <- parse_pair_matrix(op_args[-1])
    paired_t_test_from_problem_worked(pair_matrix[, 1], pair_matrix[, 2], problem_text)
  },
  paired_problem_step_answers = {
    problem_text <- op_args[1]
    pair_matrix <- parse_pair_matrix(op_args[-1])
    paired_problem_step_answers_worked(pair_matrix[, 1], pair_matrix[, 2], problem_text)
  },
  two_proportion_ci = {
    numeric_args <- parse_numeric_args(op_args)
    two_proportion_ci_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5])
  },
  two_proportion_z_test = {
    numeric_args <- parse_numeric_args(op_args[-length(op_args)])
    two_proportion_z_test_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4], numeric_args[5], op_args[length(op_args)])
  },
  correlation = {
    pair_matrix <- parse_pair_matrix(op_args)
    correlation_worked(pair_matrix[, 1], pair_matrix[, 2])
  },
  regression_line = {
    pair_matrix <- parse_pair_matrix(op_args)
    regression_line_worked(pair_matrix[, 1], pair_matrix[, 2])
  },
  regression_predict_coeffs = {
    numeric_args <- parse_numeric_args(op_args)
    regression_predict_from_coefficients_worked(numeric_args[1], numeric_args[2], numeric_args[3])
  },
  regression_prediction_row = {
    numeric_args <- parse_numeric_args(op_args)
    regression_prediction_row_worked(numeric_args[1], numeric_args[2], numeric_args[3], numeric_args[4])
  },
  regression_prediction_table = {
    numeric_args <- parse_numeric_args(op_args)
    pair_matrix <- split_interleaved_pairs(numeric_args[-c(1, 2)])
    regression_prediction_table_worked(pair_matrix[, 1], pair_matrix[, 2], numeric_args[1], numeric_args[2])
  },
  regression_predict_data = {
    x_new <- as.numeric(op_args[1])
    pair_matrix <- parse_pair_matrix(op_args[-1])
    regression_predict_from_data_worked(pair_matrix[, 1], pair_matrix[, 2], x_new)
  },
  regression_r_squared = {
    pair_matrix <- parse_pair_matrix(op_args)
    regression_r_squared_worked(pair_matrix[, 1], pair_matrix[, 2])
  },
  regression_error = {
    numeric_args <- parse_numeric_args(op_args)
    if (length(numeric_args) == 2) {
      regression_error_worked(observed = numeric_args[1], predicted = numeric_args[2])
    } else {
      regression_error_worked(observed = numeric_args[1], b0 = numeric_args[2], b1 = numeric_args[3], x_value = numeric_args[4])
    }
  },
  regression_sse = {
    numeric_args <- parse_numeric_args(op_args)
    if (length(numeric_args) %% 2 == 0 && length(numeric_args) >= 4) {
      pair_matrix <- split_interleaved_pairs(numeric_args)
      regression_sse_worked(pair_matrix[, 1], pair_matrix[, 2])
    } else {
      stop("regression_sse expects interleaved x,y pairs.", call. = FALSE)
    }
  },
  stop(paste("Unsupported operation:", operation), call. = FALSE)
)

print_calculation(calculation)
