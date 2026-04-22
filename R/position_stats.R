trimmed_mean_worked <- function(x, trim = 0.2, digits = 4) {
  x <- ensure_numeric_vector(x)
  trim <- ensure_probability(trim, "trim")

  result <- mean(x, trim = trim)

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0("trim = ", format_percent(trim, 1), " from each tail"),
    paste0("Trimmed mean = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Trimmed Mean",
    notation = "trimmed mean",
    formula = "mean(x, trim = trim)",
    steps = steps,
    answer = paste0("Trimmed mean = ", format_number(result, digits)),
    result = result
  )
}

mean_absolute_deviation_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x)

  center <- mean(x)
  absolute_deviations <- abs(x - center)
  result <- mean(absolute_deviations)

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0("x_bar = ", format_number(center, digits)),
    paste0(
      "Absolute deviations = |x_i - x_bar| = ",
      paste0("|", format_number(x, digits), " - ", format_number(center, digits), "|", collapse = " + ")
    ),
    paste0(
      "Absolute deviation values = ",
      join_values(absolute_deviations, digits, separator = " + "),
      " over ",
      length(x),
      " values"
    ),
    paste0("MAD from mean = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Mean Absolute Deviation",
    notation = "MAD",
    formula = "MAD = mean(|x_i - x_bar|)",
    steps = steps,
    answer = paste0("Mean absolute deviation = ", format_number(result, digits)),
    result = result
  )
}

coefficient_of_variation_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x, min_length = 2)

  mean_value <- mean(x)
  sd_value <- sd(x)
  result <- sd_value / mean_value * 100

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0("x_bar = ", format_number(mean_value, digits)),
    paste0("s = ", format_number(sd_value, digits)),
    paste0(
      "CV = (",
      format_number(sd_value, digits),
      " / ",
      format_number(mean_value, digits),
      ") * 100 = ",
      format_number(result, digits),
      "%"
    )
  )

  new_worked_calculation(
    title = "Coefficient of Variation",
    notation = "CV",
    formula = "CV = (s / x_bar) * 100%",
    steps = steps,
    answer = paste0("Coefficient of variation = ", format_number(result, digits), "%"),
    result = result
  )
}

quantile_type2_worked <- function(x, probability, digits = 4) {
  x <- ensure_numeric_vector(x)
  probability <- ensure_probability(probability, "probability")

  sorted_x <- sort(x)
  result <- as.numeric(quantile(sorted_x, probs = probability, type = 2))

  steps <- c(
    paste0("Sorted data = {", join_values(sorted_x, digits), "}"),
    paste0("Requested probability = ", format_number(probability, digits)),
    paste0("Using type = 2 quantile convention from class notes."),
    paste0("Quantile = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Type 2 Quantile",
    notation = "Q(p)",
    formula = "quantile(x, probs = p, type = 2)",
    steps = steps,
    answer = paste0("Type 2 quantile = ", format_number(result, digits)),
    result = result
  )
}

percentile_rank_worked <- function(x, value, digits = 4) {
  x <- ensure_numeric_vector(x)
  value <- ensure_numeric_scalar(value, "value")

  ecdf_value <- ecdf(x)(value)
  result <- ecdf_value * 100

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0("Value = ", format_number(value, digits)),
    paste0("Percentile rank = F(", format_number(value, digits), ") * 100"),
    paste0(
      "Percentile rank = ",
      format_number(ecdf_value, digits),
      " * 100 = ",
      format_number(result, digits),
      "%"
    )
  )

  new_worked_calculation(
    title = "Percentile Rank",
    notation = "F(x)",
    formula = "Percentile rank = ecdf(x)(value) * 100",
    steps = steps,
    answer = paste0("Percentile rank = ", format_number(result, digits), "%"),
    result = result
  )
}

iqr_type2_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x)

  q1 <- as.numeric(quantile(x, 0.25, type = 2))
  q3 <- as.numeric(quantile(x, 0.75, type = 2))
  result <- q3 - q1

  steps <- c(
    paste0("Q1 = ", format_number(q1, digits)),
    paste0("Q3 = ", format_number(q3, digits)),
    paste0(
      "IQR = Q3 - Q1 = ",
      format_number(q3, digits),
      " - ",
      format_number(q1, digits),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Interquartile Range",
    notation = "IQR",
    formula = "IQR = Q3 - Q1 using type = 2 quartiles",
    steps = steps,
    answer = paste0("IQR = ", format_number(result, digits)),
    result = result
  )
}

outlier_fences_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x)

  q1 <- as.numeric(quantile(x, 0.25, type = 2))
  q3 <- as.numeric(quantile(x, 0.75, type = 2))
  iqr_value <- q3 - q1
  lower_fence <- q1 - 1.5 * iqr_value
  upper_fence <- q3 + 1.5 * iqr_value
  outliers <- x[x < lower_fence | x > upper_fence]

  steps <- c(
    paste0("Q1 = ", format_number(q1, digits)),
    paste0("Q3 = ", format_number(q3, digits)),
    paste0("IQR = ", format_number(iqr_value, digits)),
    paste0("Lower fence = Q1 - 1.5 * IQR = ", format_number(lower_fence, digits)),
    paste0("Upper fence = Q3 + 1.5 * IQR = ", format_number(upper_fence, digits)),
    if (length(outliers) == 0) {
      "No outliers fall outside the fences."
    } else {
      paste0("Outliers = {", join_values(outliers, digits), "}")
    }
  )

  new_worked_calculation(
    title = "Outlier Fences",
    notation = "1.5 * IQR rule",
    formula = "Lower = Q1 - 1.5IQR, Upper = Q3 + 1.5IQR",
    steps = steps,
    answer = paste0(
      "Fences = ",
      format_interval(lower_fence, upper_fence, digits),
      if (length(outliers) == 0) "" else paste0("; Outliers = {", join_values(outliers, digits), "}")
    ),
    result = list(lower_fence = lower_fence, upper_fence = upper_fence, outliers = outliers)
  )
}

frequency_table_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x)

  freq <- table(x)
  rel <- prop.table(freq)
  cum_freq <- cumsum(freq)

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0("Frequencies = ", paste0(names(freq), ":", as.integer(freq), collapse = ", ")),
    paste0("Relative frequencies = ", paste0(names(rel), ":", format_number(as.numeric(rel), digits), collapse = ", ")),
    paste0("Cumulative frequencies = ", paste0(names(cum_freq), ":", as.integer(cum_freq), collapse = ", "))
  )

  new_worked_calculation(
    title = "Frequency Table",
    notation = "freq(x)",
    formula = "table(x), prop.table(table(x)), cumsum(table(x))",
    steps = steps,
    answer = "Frequency, relative frequency, and cumulative frequency table computed.",
    result = list(frequency = freq, relative_frequency = rel, cumulative_frequency = cum_freq)
  )
}

weighted_mean_worked <- function(observations, weights, digits = 4) {
  observations <- ensure_numeric_vector(observations)
  weights <- ensure_numeric_vector(weights)
  ensure_same_length(observations, weights, names = c("observations", "weights"))

  result <- weighted.mean(observations, weights)

  steps <- c(
    paste0("Observations = {", join_values(observations, digits), "}"),
    paste0("Weights = {", join_values(weights, digits), "}"),
    paste0(
      "Weighted sum = ",
      join_values(observations * weights, digits, separator = " + "),
      " = ",
      format_number(sum(observations * weights), digits)
    ),
    paste0("Sum of weights = ", format_number(sum(weights), digits)),
    paste0("Weighted mean = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Weighted Mean",
    notation = "x_bar_w",
    formula = "Weighted mean = sum(w_i x_i) / sum(w_i)",
    steps = steps,
    answer = paste0("Weighted mean = ", format_number(result, digits)),
    result = result
  )
}

missing_value_from_mean_worked <- function(known_mean, known_values, n_total, digits = 4) {
  known_mean <- ensure_numeric_scalar(known_mean, "known_mean")
  known_values <- ensure_numeric_vector(known_values)
  n_total <- ensure_count_scalar(n_total, "n_total", min_value = length(known_values) + 1)

  missing_value <- (known_mean * n_total) - sum(known_values)

  steps <- c(
    paste0("Known mean = ", format_number(known_mean, digits)),
    paste0("Known values = {", join_values(known_values, digits), "}"),
    paste0("n = ", n_total),
    paste0(
      "Missing value = (",
      format_number(known_mean, digits),
      " * ",
      n_total,
      ") - ",
      format_number(sum(known_values), digits),
      " = ",
      format_number(missing_value, digits)
    )
  )

  new_worked_calculation(
    title = "Missing Value From Mean",
    notation = "x_missing",
    formula = "x_missing = mean * n - sum(known values)",
    steps = steps,
    answer = paste0("Missing value = ", format_number(missing_value, digits)),
    result = missing_value
  )
}

sample_proportion_worked <- function(x_success, n_total, digits = 4) {
  x_success <- ensure_count_scalar(x_success, "x_success")
  n_total <- ensure_count_scalar(n_total, "n_total", min_value = 1)

  if (x_success > n_total) {
    stop("x_success cannot be larger than n_total.", call. = FALSE)
  }

  result <- x_success / n_total

  steps <- c(
    paste0("x = ", x_success),
    paste0("n = ", n_total),
    paste0("p_hat = x / n = ", x_success, " / ", n_total, " = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Sample Proportion",
    notation = "p_hat",
    formula = "p_hat = x / n",
    steps = steps,
    answer = paste0("Sample proportion = ", format_number(result, digits), " = ", format_percent(result, digits)),
    result = result
  )
}

count_from_proportion_worked <- function(n_total, proportion, digits = 4) {
  n_total <- ensure_count_scalar(n_total, "n_total", min_value = 1)
  proportion <- ensure_probability(proportion, "proportion")

  result <- n_total * proportion

  steps <- c(
    paste0("n = ", n_total),
    paste0("Proportion = ", format_number(proportion, digits), " = ", format_percent(proportion, digits)),
    paste0(
      "Count = n * proportion = ",
      n_total,
      " * ",
      format_number(proportion, digits),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Count From Proportion",
    notation = "count",
    formula = "Count = n * proportion",
    steps = steps,
    answer = paste0("Count = ", format_number(result, digits)),
    result = result
  )
}

percent_change_worked <- function(old_value, new_value, digits = 4) {
  old_value <- ensure_numeric_scalar(old_value, "old_value")
  new_value <- ensure_numeric_scalar(new_value, "new_value")

  result <- (new_value - old_value) / old_value * 100

  steps <- c(
    paste0("Old value = ", format_number(old_value, digits)),
    paste0("New value = ", format_number(new_value, digits)),
    paste0(
      "Percent change = (",
      format_number(new_value, digits),
      " - ",
      format_number(old_value, digits),
      ") / ",
      format_number(old_value, digits),
      " * 100 = ",
      format_number(result, digits),
      "%"
    )
  )

  new_worked_calculation(
    title = "Percent Change",
    notation = "% change",
    formula = "Percent change = ((new - old) / old) * 100",
    steps = steps,
    answer = paste0("Percent change = ", format_number(result, digits), "%"),
    result = result
  )
}

chebyshev_k_worked <- function(k, digits = 4) {
  k <- ensure_positive_scalar(k, "k")

  result <- 1 - 1 / (k^2)

  steps <- c(
    paste0("k = ", format_number(k, digits)),
    paste0(
      "At least proportion = 1 - 1 / k^2 = 1 - 1 / ",
      format_number(k^2, digits),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Chebyshev's Theorem",
    notation = "1 - 1/k^2",
    formula = "At least 1 - 1/k^2 of the data lies within k standard deviations.",
    steps = steps,
    answer = paste0("At least proportion within k SDs = ", format_number(result, digits)),
    result = result
  )
}

chebyshev_interval_worked <- function(mean_value, sd_value, lower_bound, upper_bound, digits = 4) {
  mean_value <- ensure_numeric_scalar(mean_value, "mean_value")
  sd_value <- ensure_positive_scalar(sd_value, "sd_value")
  lower_bound <- ensure_numeric_scalar(lower_bound, "lower_bound")
  upper_bound <- ensure_numeric_scalar(upper_bound, "upper_bound")

  k_value <- min(abs(lower_bound - mean_value), abs(upper_bound - mean_value)) / sd_value
  result <- 1 - 1 / (k_value^2)

  steps <- c(
    paste0("Mean = ", format_number(mean_value, digits)),
    paste0("SD = ", format_number(sd_value, digits)),
    paste0("Interval = ", format_interval(lower_bound, upper_bound, digits)),
    paste0("k = min(|lower - mean|, |upper - mean|) / SD = ", format_number(k_value, digits)),
    paste0("At least proportion = 1 - 1 / k^2 = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Chebyshev From Interval",
    notation = "k",
    formula = "k = min(|lower - mean|, |upper - mean|) / SD, then 1 - 1/k^2",
    steps = steps,
    answer = paste0("At least proportion in the interval = ", format_number(result, digits)),
    result = result
  )
}

empirical_rule_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x, min_length = 2)

  mu <- mean(x)
  sigma <- sd(x)

  interval_1 <- c(mu - sigma, mu + sigma)
  interval_2 <- c(mu - 2 * sigma, mu + 2 * sigma)
  interval_3 <- c(mu - 3 * sigma, mu + 3 * sigma)

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0("x_bar = ", format_number(mu, digits)),
    paste0("s = ", format_number(sigma, digits)),
    paste0("About 68% interval = ", format_interval(interval_1[1], interval_1[2], digits)),
    paste0("About 95% interval = ", format_interval(interval_2[1], interval_2[2], digits)),
    paste0("About 99.7% interval = ", format_interval(interval_3[1], interval_3[2], digits))
  )

  new_worked_calculation(
    title = "Empirical Rule",
    notation = "68-95-99.7",
    formula = "mu ± sigma, mu ± 2sigma, mu ± 3sigma",
    steps = steps,
    answer = paste0(
      "68%: ", format_interval(interval_1[1], interval_1[2], digits),
      "; 95%: ", format_interval(interval_2[1], interval_2[2], digits),
      "; 99.7%: ", format_interval(interval_3[1], interval_3[2], digits)
    ),
    result = list(interval_68 = interval_1, interval_95 = interval_2, interval_997 = interval_3)
  )
}

empirical_rule_summary_landmarks <- function() {
  c(
    "-3" = 0.0015,
    "-2" = 0.025,
    "-1" = 0.16,
    "0" = 0.5,
    "1" = 0.84,
    "2" = 0.975,
    "3" = 0.9985
  )
}

empirical_rule_match_k <- function(z_value) {
  candidate_values <- c(-3, -2, -1, 0, 1, 2, 3)
  differences <- abs(candidate_values - z_value)
  best_index <- which.min(differences)

  if (differences[best_index] > 1e-9) {
    stop(
      "empirical_rule_summary_worked expects values located exactly at the mean or at 1, 2, or 3 standard deviations from the mean.",
      call. = FALSE
    )
  }

  candidate_values[best_index]
}

empirical_rule_summary_worked <- function(mean_value, sd_value, event, value1, value2 = NULL, digits = 4) {
  mean_value <- ensure_numeric_scalar(mean_value, "mean_value")
  sd_value <- ensure_positive_scalar(sd_value, "sd_value")
  event <- ensure_event(event)
  value1 <- ensure_numeric_scalar(value1, "value1")

  landmarks <- empirical_rule_summary_landmarks()
  z_value1 <- (value1 - mean_value) / sd_value
  k_value1 <- empirical_rule_match_k(z_value1)
  cumulative1 <- unname(landmarks[as.character(k_value1)])

  steps <- c(
    paste0("Mean = ", format_number(mean_value, digits)),
    paste0("SD = ", format_number(sd_value, digits))
  )

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
    z_value2 <- (value2 - mean_value) / sd_value
    k_value2 <- empirical_rule_match_k(z_value2)
    cumulative2 <- unname(landmarks[as.character(k_value2)])
    lower_value <- min(value1, value2)
    upper_value <- max(value1, value2)
    lower_k <- min(k_value1, k_value2)
    upper_k <- max(k_value1, k_value2)
    lower_cumulative <- unname(landmarks[as.character(lower_k)])
    upper_cumulative <- unname(landmarks[as.character(upper_k)])
    result <- upper_cumulative - lower_cumulative

    steps <- c(
      steps,
      paste0("Lower bound = ", format_number(lower_value, digits), ", z = ", format_number((lower_value - mean_value) / sd_value, digits)),
      paste0("Upper bound = ", format_number(upper_value, digits), ", z = ", format_number((upper_value - mean_value) / sd_value, digits)),
      paste0(
        "Empirical-rule area = cumulative(",
        upper_k,
        " SD) - cumulative(",
        lower_k,
        " SD) = ",
        format_number(upper_cumulative, digits),
        " - ",
        format_number(lower_cumulative, digits),
        " = ",
        format_number(result, digits)
      )
    )

    return(
      new_worked_calculation(
        title = "Empirical Rule From Mean And SD",
        notation = "68-95-99.7",
        formula = "Convert bounds to standard-deviation landmarks and subtract empirical-rule cumulative areas.",
        steps = steps,
        answer = paste0("Empirical-rule proportion = ", format_number(result, digits), " = ", format_percent(result, digits)),
        result = result
      )
    )
  }

  if (event %in% c("less_than", "at_most")) {
    result <- cumulative1
    steps <- c(
      steps,
      paste0("Bound = ", format_number(value1, digits), ", z = ", format_number(z_value1, digits)),
      paste0("Empirical-rule cumulative area = ", format_number(result, digits))
    )
  } else if (event %in% c("greater_than", "at_least")) {
    result <- 1 - cumulative1
    steps <- c(
      steps,
      paste0("Bound = ", format_number(value1, digits), ", z = ", format_number(z_value1, digits)),
      paste0(
        "Empirical-rule upper-tail area = 1 - ",
        format_number(cumulative1, digits),
        " = ",
        format_number(result, digits)
      )
    )
  } else {
    stop("empirical_rule_summary_worked supports less_than, at_most, greater_than, at_least, and between.", call. = FALSE)
  }

  new_worked_calculation(
    title = "Empirical Rule From Mean And SD",
    notation = "68-95-99.7",
    formula = "Convert the bound to a standard-deviation landmark and use the empirical-rule cumulative area.",
    steps = steps,
    answer = paste0("Empirical-rule proportion = ", format_number(result, digits), " = ", format_percent(result, digits)),
    result = result
  )
}
