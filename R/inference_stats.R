mean_ci_known_sigma_worked <- function(xbar, sigma, n, conf_level, digits = 4) {
  xbar <- ensure_numeric_scalar(xbar, "xbar")
  sigma <- ensure_positive_scalar(sigma, "sigma")
  n <- ensure_count_scalar(n, "n", min_value = 1)
  conf_level <- ensure_confidence_level(conf_level)

  alpha <- 1 - conf_level
  z_star <- qnorm(1 - alpha / 2)
  se <- sigma / sqrt(n)
  margin_error <- z_star * se
  lower <- xbar - margin_error
  upper <- xbar + margin_error

  steps <- c(
    paste0("x_bar = ", format_number(xbar, digits)),
    paste0("sigma = ", format_number(sigma, digits)),
    paste0("n = ", n),
    paste0("Confidence level = ", format_percent(conf_level, 1)),
    paste0("z* = ", format_number(z_star, digits)),
    paste0("SE = sigma / sqrt(n) = ", format_number(se, digits)),
    paste0("Margin of error = z* * SE = ", format_number(margin_error, digits)),
    paste0("CI = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Confidence Interval for Mean (Known Sigma)",
    notation = "mu",
    formula = "x_bar ± z* * sigma / sqrt(n)",
    steps = steps,
    answer = paste0("Confidence interval = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

mean_ci_unknown_sigma_worked <- function(xbar, s, n, conf_level, digits = 4) {
  xbar <- ensure_numeric_scalar(xbar, "xbar")
  s <- ensure_positive_scalar(s, "s")
  n <- ensure_count_scalar(n, "n", min_value = 2)
  conf_level <- ensure_confidence_level(conf_level)

  alpha <- 1 - conf_level
  df <- n - 1
  t_star <- qt(1 - alpha / 2, df)
  se <- s / sqrt(n)
  margin_error <- t_star * se
  lower <- xbar - margin_error
  upper <- xbar + margin_error

  steps <- c(
    paste0("x_bar = ", format_number(xbar, digits)),
    paste0("s = ", format_number(s, digits)),
    paste0("n = ", n, ", df = ", df),
    paste0("Confidence level = ", format_percent(conf_level, 1)),
    paste0("t* = ", format_number(t_star, digits)),
    paste0("SE = s / sqrt(n) = ", format_number(se, digits)),
    paste0("Margin of error = t* * SE = ", format_number(margin_error, digits)),
    paste0("CI = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Confidence Interval for Mean (Unknown Sigma)",
    notation = "mu",
    formula = "x_bar ± t* * s / sqrt(n)",
    steps = steps,
    answer = paste0("Confidence interval = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

margin_of_error_from_interval_worked <- function(lower, upper, digits = 4) {
  lower <- ensure_numeric_scalar(lower, "lower")
  upper <- ensure_numeric_scalar(upper, "upper")
  result <- (upper - lower) / 2

  steps <- c(
    paste0("Interval = ", format_interval(lower, upper, digits)),
    paste0(
      "Margin of error = (upper - lower) / 2 = (",
      format_number(upper, digits),
      " - ",
      format_number(lower, digits),
      ") / 2 = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Margin of Error From Interval",
    notation = "E",
    formula = "E = (upper - lower) / 2",
    steps = steps,
    answer = paste0("Margin of error = ", format_number(result, digits)),
    result = result
  )
}

sample_size_mean_known_sigma_worked <- function(sigma, margin_error, conf_level, digits = 4) {
  sigma <- ensure_positive_scalar(sigma, "sigma")
  margin_error <- ensure_positive_scalar(margin_error, "margin_error")
  conf_level <- ensure_confidence_level(conf_level)

  alpha <- 1 - conf_level
  z_star <- qnorm(1 - alpha / 2)
  raw_n <- (z_star * sigma / margin_error)^2
  result <- ceiling(raw_n)

  steps <- c(
    paste0("sigma = ", format_number(sigma, digits)),
    paste0("Margin of error = ", format_number(margin_error, digits)),
    paste0("Confidence level = ", format_percent(conf_level, 1)),
    paste0("z* = ", format_number(z_star, digits)),
    paste0("n = (z* * sigma / E)^2 = ", format_number(raw_n, digits)),
    paste0("Round up to the next whole number: ", result)
  )

  new_worked_calculation(
    title = "Sample Size for Mean",
    notation = "n",
    formula = "n = (z* * sigma / E)^2, then round up",
    steps = steps,
    answer = paste0("Required sample size = ", result),
    result = result
  )
}

proportion_ci_worked <- function(x, n, conf_level, digits = 4) {
  x <- ensure_numeric_scalar(x, "x")
  n <- ensure_count_scalar(n, "n", min_value = 1)
  conf_level <- ensure_confidence_level(conf_level)

  p_hat <- x / n
  alpha <- 1 - conf_level
  z_star <- qnorm(1 - alpha / 2)
  se <- sqrt(p_hat * (1 - p_hat) / n)
  margin_error <- z_star * se
  lower <- p_hat - margin_error
  upper <- p_hat + margin_error

  steps <- c(
    paste0("x = ", format_number(x, digits), ", n = ", n),
    paste0("p_hat = x / n = ", format_number(p_hat, digits)),
    paste0("z* = ", format_number(z_star, digits)),
    paste0("SE = sqrt(p_hat(1-p_hat)/n) = ", format_number(se, digits)),
    paste0("Margin of error = ", format_number(margin_error, digits)),
    paste0("CI = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Confidence Interval for Proportion",
    notation = "p",
    formula = "p_hat ± z* * sqrt(p_hat(1-p_hat)/n)",
    steps = steps,
    answer = paste0("Confidence interval = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

sample_size_proportion_worked <- function(p_hat, margin_error, conf_level, digits = 4) {
  p_hat <- ensure_probability(p_hat, "p_hat")
  margin_error <- ensure_positive_scalar(margin_error, "margin_error")
  conf_level <- ensure_confidence_level(conf_level)

  z_star <- qnorm(1 - (1 - conf_level) / 2)
  q_hat <- 1 - p_hat
  raw_n <- (z_star^2 * p_hat * q_hat) / (margin_error^2)
  result <- ceiling(raw_n)

  steps <- c(
    paste0("p_hat = ", format_number(p_hat, digits)),
    paste0("q_hat = 1 - p_hat = ", format_number(q_hat, digits)),
    paste0("Margin of error = ", format_number(margin_error, digits)),
    paste0("z* = ", format_number(z_star, digits)),
    paste0("n = (z^2 * p_hat * q_hat) / E^2 = ", format_number(raw_n, digits)),
    paste0("Round up to the next whole number: ", result)
  )

  new_worked_calculation(
    title = "Sample Size for Proportion",
    notation = "n",
    formula = "n = (z^2 * p_hat * q_hat) / E^2, then round up",
    steps = steps,
    answer = paste0("Required sample size = ", result),
    result = result
  )
}

one_sample_z_test_worked <- function(xbar, mu0, sigma, n, alpha, tail, digits = 4) {
  xbar <- ensure_numeric_scalar(xbar, "xbar")
  mu0 <- ensure_numeric_scalar(mu0, "mu0")
  sigma <- ensure_positive_scalar(sigma, "sigma")
  n <- ensure_count_scalar(n, "n", min_value = 1)
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  z_value <- (xbar - mu0) / (sigma / sqrt(n))
  p_value <- tail_p_value(z_value, tail, distribution = "z")
  critical_value <- tail_critical_value(alpha, tail, distribution = "z")
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("H0: mu = ", format_number(mu0, digits)),
    paste0("Ha: mu ", tail_symbol(tail), " ", format_number(mu0, digits), " (", tail_description(tail), ")"),
    paste0("z = (x_bar - mu0) / (sigma / sqrt(n)) = ", format_number(z_value, digits)),
    paste0("Critical value = ", format_number(critical_value, digits)),
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "One-Sample Z Test",
    notation = "z",
    formula = "z = (x_bar - mu0) / (sigma / sqrt(n))",
    steps = steps,
    answer = paste0("z = ", format_number(z_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = z_value, p_value = p_value, critical_value = critical_value, decision = decision)
  )
}

one_sample_t_test_worked <- function(xbar, mu0, s, n, alpha, tail, digits = 4) {
  xbar <- ensure_numeric_scalar(xbar, "xbar")
  mu0 <- ensure_numeric_scalar(mu0, "mu0")
  s <- ensure_positive_scalar(s, "s")
  n <- ensure_count_scalar(n, "n", min_value = 2)
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  t_value <- (xbar - mu0) / (s / sqrt(n))
  df <- n - 1
  p_value <- tail_p_value(t_value, tail, distribution = "t", df = df)
  critical_value <- tail_critical_value(alpha, tail, distribution = "t", df = df)
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("H0: mu = ", format_number(mu0, digits)),
    paste0("Ha: mu ", tail_symbol(tail), " ", format_number(mu0, digits), " (", tail_description(tail), ")"),
    paste0("df = n - 1 = ", df),
    paste0("t = (x_bar - mu0) / (s / sqrt(n)) = ", format_number(t_value, digits)),
    paste0("Critical value = ", format_number(critical_value, digits)),
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "One-Sample t Test",
    notation = "t",
    formula = "t = (x_bar - mu0) / (s / sqrt(n))",
    steps = steps,
    answer = paste0("t = ", format_number(t_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = t_value, p_value = p_value, critical_value = critical_value, decision = decision, df = df)
  )
}

one_proportion_z_test_worked <- function(p_hat, p0, n, alpha, tail, digits = 4) {
  p_hat <- ensure_probability(p_hat, "p_hat")
  p0 <- ensure_probability(p0, "p0")
  n <- ensure_count_scalar(n, "n", min_value = 1)
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  z_value <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
  p_value <- tail_p_value(z_value, tail, distribution = "z")
  critical_value <- tail_critical_value(alpha, tail, distribution = "z")
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("H0: p = ", format_number(p0, digits)),
    paste0("Ha: p ", tail_symbol(tail), " ", format_number(p0, digits)),
    paste0("p_hat = ", format_number(p_hat, digits)),
    paste0("z = (p_hat - p0) / sqrt(p0(1-p0)/n) = ", format_number(z_value, digits)),
    paste0("Critical value = ", format_number(critical_value, digits)),
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "One-Proportion Z Test",
    notation = "z",
    formula = "z = (p_hat - p0) / sqrt(p0(1-p0)/n)",
    steps = steps,
    answer = paste0("z = ", format_number(z_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = z_value, p_value = p_value, critical_value = critical_value, decision = decision)
  )
}

normal_approximation_check_worked <- function(n, p0, digits = 4) {
  n <- ensure_count_scalar(n, "n", min_value = 1)
  p0 <- ensure_probability(p0, "p0")

  np <- n * p0
  nq <- n * (1 - p0)
  valid <- np >= 5 && nq >= 5

  steps <- c(
    paste0("n = ", n),
    paste0("p0 = ", format_number(p0, digits)),
    paste0("np = ", format_number(np, digits)),
    paste0("n(1-p0) = ", format_number(nq, digits)),
    if (valid) "Normal approximation conditions are met." else "Normal approximation conditions are not met."
  )

  new_worked_calculation(
    title = "Normal Approximation Check",
    notation = "np, n(1-p)",
    formula = "Need np >= 5 and n(1-p) >= 5",
    steps = steps,
    answer = if (valid) "Normal approximation is appropriate." else "Normal approximation is not appropriate.",
    result = valid
  )
}

variance_ci_sigma_worked <- function(n, s, conf_level, digits = 4) {
  n <- ensure_count_scalar(n, "n", min_value = 2)
  s <- ensure_positive_scalar(s, "s")
  conf_level <- ensure_confidence_level(conf_level)

  alpha <- 1 - conf_level
  df <- n - 1
  chi_left <- qchisq(1 - alpha / 2, df)
  chi_right <- qchisq(alpha / 2, df)
  lower <- sqrt((df * s^2) / chi_left)
  upper <- sqrt((df * s^2) / chi_right)

  steps <- c(
    paste0("n = ", n, ", df = ", df),
    paste0("s = ", format_number(s, digits)),
    paste0("Confidence level = ", format_percent(conf_level, 1)),
    paste0("Chi-square left = ", format_number(chi_left, digits)),
    paste0("Chi-square right = ", format_number(chi_right, digits)),
    paste0("CI for sigma = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Confidence Interval for Standard Deviation",
    notation = "sigma",
    formula = "sqrt((df * s^2) / chi_left) to sqrt((df * s^2) / chi_right)",
    steps = steps,
    answer = paste0("Confidence interval for sigma = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

variance_ci_variance_worked <- function(n, s2, conf_level, digits = 4) {
  n <- ensure_count_scalar(n, "n", min_value = 2)
  s2 <- ensure_positive_scalar(s2, "s2")
  conf_level <- ensure_confidence_level(conf_level)

  alpha <- 1 - conf_level
  df <- n - 1
  chi_left <- qchisq(1 - alpha / 2, df)
  chi_right <- qchisq(alpha / 2, df)
  lower <- (df * s2) / chi_left
  upper <- (df * s2) / chi_right

  steps <- c(
    paste0("n = ", n, ", df = ", df),
    paste0("s^2 = ", format_number(s2, digits)),
    paste0("Chi-square left = ", format_number(chi_left, digits)),
    paste0("Chi-square right = ", format_number(chi_right, digits)),
    paste0("CI for sigma^2 = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Confidence Interval for Variance",
    notation = "sigma^2",
    formula = "((df * s^2) / chi_left, (df * s^2) / chi_right)",
    steps = steps,
    answer = paste0("Confidence interval for variance = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

variance_test_worked <- function(n, s, sigma0, alpha, tail, digits = 4) {
  n <- ensure_count_scalar(n, "n", min_value = 2)
  s <- ensure_positive_scalar(s, "s")
  sigma0 <- ensure_positive_scalar(sigma0, "sigma0")
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  df <- n - 1
  chi_value <- (df * s^2) / sigma0^2
  p_value <- tail_p_value(chi_value, tail, distribution = "chisq", df = df)
  critical_value <- tail_critical_value(alpha, tail, distribution = "chisq", df = df)
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("H0: sigma = ", format_number(sigma0, digits)),
    paste0("Ha: sigma ", tail_symbol(tail), " ", format_number(sigma0, digits)),
    paste0("Chi-square = (n - 1)s^2 / sigma0^2 = ", format_number(chi_value, digits)),
    if (tail == "two") {
      paste0(
        "Critical values = ",
        format_interval(critical_value["lower"], critical_value["upper"], digits)
      )
    } else {
      paste0("Critical value = ", format_number(critical_value, digits))
    },
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "Chi-Square Test for Variance / SD",
    notation = "chi-square",
    formula = "chi-square = (n - 1)s^2 / sigma0^2",
    steps = steps,
    answer = paste0("Chi-square = ", format_number(chi_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = chi_value, p_value = p_value, critical_value = critical_value, decision = decision, df = df)
  )
}

two_sample_z_test_worked <- function(xbar1, xbar2, sigma1, sigma2, n1, n2, alpha, tail, digits = 4) {
  xbar1 <- ensure_numeric_scalar(xbar1, "xbar1")
  xbar2 <- ensure_numeric_scalar(xbar2, "xbar2")
  sigma1 <- ensure_positive_scalar(sigma1, "sigma1")
  sigma2 <- ensure_positive_scalar(sigma2, "sigma2")
  n1 <- ensure_count_scalar(n1, "n1", min_value = 1)
  n2 <- ensure_count_scalar(n2, "n2", min_value = 1)
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  se <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))
  z_value <- (xbar1 - xbar2) / se
  p_value <- tail_p_value(z_value, tail, distribution = "z")
  critical_value <- tail_critical_value(alpha, tail, distribution = "z")
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("Difference in sample means = ", format_number(xbar1 - xbar2, digits)),
    paste0("SE = sqrt(sigma1^2/n1 + sigma2^2/n2) = ", format_number(se, digits)),
    paste0("z = (x_bar1 - x_bar2) / SE = ", format_number(z_value, digits)),
    paste0("Critical value = ", format_number(critical_value, digits)),
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "Two-Sample Z Test",
    notation = "z",
    formula = "z = (x_bar1 - x_bar2) / sqrt(sigma1^2/n1 + sigma2^2/n2)",
    steps = steps,
    answer = paste0("z = ", format_number(z_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = z_value, p_value = p_value, critical_value = critical_value, decision = decision)
  )
}

two_sample_z_ci_worked <- function(xbar1, xbar2, sigma1, sigma2, n1, n2, conf_level, digits = 4) {
  xbar1 <- ensure_numeric_scalar(xbar1, "xbar1")
  xbar2 <- ensure_numeric_scalar(xbar2, "xbar2")
  sigma1 <- ensure_positive_scalar(sigma1, "sigma1")
  sigma2 <- ensure_positive_scalar(sigma2, "sigma2")
  n1 <- ensure_count_scalar(n1, "n1", min_value = 1)
  n2 <- ensure_count_scalar(n2, "n2", min_value = 1)
  conf_level <- ensure_confidence_level(conf_level)

  se <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))
  z_star <- qnorm(1 - (1 - conf_level) / 2)
  diff_means <- xbar1 - xbar2
  margin_error <- z_star * se
  lower <- diff_means - margin_error
  upper <- diff_means + margin_error

  steps <- c(
    paste0("x_bar1 - x_bar2 = ", format_number(diff_means, digits)),
    paste0("SE = ", format_number(se, digits)),
    paste0("z* = ", format_number(z_star, digits)),
    paste0("Margin of error = ", format_number(margin_error, digits)),
    paste0("CI = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Two-Sample Z Interval",
    notation = "mu1 - mu2",
    formula = "(x_bar1 - x_bar2) ± z* * sqrt(sigma1^2/n1 + sigma2^2/n2)",
    steps = steps,
    answer = paste0("Confidence interval = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

welch_df <- function(s1, s2, n1, n2) {
  ((s1^2 / n1 + s2^2 / n2)^2) / (((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1)))
}

welch_t_test_worked <- function(xbar1, xbar2, s1, s2, n1, n2, alpha, tail, digits = 4) {
  xbar1 <- ensure_numeric_scalar(xbar1, "xbar1")
  xbar2 <- ensure_numeric_scalar(xbar2, "xbar2")
  s1 <- ensure_positive_scalar(s1, "s1")
  s2 <- ensure_positive_scalar(s2, "s2")
  n1 <- ensure_count_scalar(n1, "n1", min_value = 2)
  n2 <- ensure_count_scalar(n2, "n2", min_value = 2)
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  se <- sqrt((s1^2 / n1) + (s2^2 / n2))
  t_value <- (xbar1 - xbar2) / se
  df <- welch_df(s1, s2, n1, n2)
  p_value <- tail_p_value(t_value, tail, distribution = "t", df = df)
  critical_value <- tail_critical_value(alpha, tail, distribution = "t", df = df)
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("Difference in sample means = ", format_number(xbar1 - xbar2, digits)),
    paste0("SE = ", format_number(se, digits)),
    paste0("Welch df = ", format_number(df, digits)),
    paste0("t = ", format_number(t_value, digits)),
    paste0("Critical value = ", format_number(critical_value, digits)),
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "Welch Two-Sample t Test",
    notation = "t",
    formula = "t = (x_bar1 - x_bar2) / sqrt(s1^2/n1 + s2^2/n2)",
    steps = steps,
    answer = paste0("t = ", format_number(t_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = t_value, p_value = p_value, critical_value = critical_value, decision = decision, df = df)
  )
}

welch_t_ci_worked <- function(xbar1, xbar2, s1, s2, n1, n2, conf_level, digits = 4) {
  xbar1 <- ensure_numeric_scalar(xbar1, "xbar1")
  xbar2 <- ensure_numeric_scalar(xbar2, "xbar2")
  s1 <- ensure_positive_scalar(s1, "s1")
  s2 <- ensure_positive_scalar(s2, "s2")
  n1 <- ensure_count_scalar(n1, "n1", min_value = 2)
  n2 <- ensure_count_scalar(n2, "n2", min_value = 2)
  conf_level <- ensure_confidence_level(conf_level)

  se <- sqrt((s1^2 / n1) + (s2^2 / n2))
  df <- welch_df(s1, s2, n1, n2)
  t_star <- qt(1 - (1 - conf_level) / 2, df)
  diff_means <- xbar1 - xbar2
  margin_error <- t_star * se
  lower <- diff_means - margin_error
  upper <- diff_means + margin_error

  steps <- c(
    paste0("x_bar1 - x_bar2 = ", format_number(diff_means, digits)),
    paste0("SE = ", format_number(se, digits)),
    paste0("Welch df = ", format_number(df, digits)),
    paste0("t* = ", format_number(t_star, digits)),
    paste0("Margin of error = ", format_number(margin_error, digits)),
    paste0("CI = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Welch Two-Sample t Interval",
    notation = "mu1 - mu2",
    formula = "(x_bar1 - x_bar2) ± t* * sqrt(s1^2/n1 + s2^2/n2)",
    steps = steps,
    answer = paste0("Confidence interval = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

pooled_t_test_worked <- function(xbar1, xbar2, s1, s2, n1, n2, alpha, tail, digits = 4) {
  xbar1 <- ensure_numeric_scalar(xbar1, "xbar1")
  xbar2 <- ensure_numeric_scalar(xbar2, "xbar2")
  s1 <- ensure_positive_scalar(s1, "s1")
  s2 <- ensure_positive_scalar(s2, "s2")
  n1 <- ensure_count_scalar(n1, "n1", min_value = 2)
  n2 <- ensure_count_scalar(n2, "n2", min_value = 2)
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  df <- n1 + n2 - 2
  sp2 <- (((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / df
  sp <- sqrt(sp2)
  se <- sp * sqrt(1 / n1 + 1 / n2)
  t_value <- (xbar1 - xbar2) / se
  p_value <- tail_p_value(t_value, tail, distribution = "t", df = df)
  critical_value <- tail_critical_value(alpha, tail, distribution = "t", df = df)
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("Pooled variance s_p^2 = ", format_number(sp2, digits)),
    paste0("Pooled SD s_p = ", format_number(sp, digits)),
    paste0("SE = s_p * sqrt(1/n1 + 1/n2) = ", format_number(se, digits)),
    paste0("df = ", df),
    paste0("t = ", format_number(t_value, digits)),
    paste0("Critical value = ", format_number(critical_value, digits)),
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "Pooled Two-Sample t Test",
    notation = "t",
    formula = "Use pooled variance when population variances are assumed equal.",
    steps = steps,
    answer = paste0("t = ", format_number(t_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = t_value, p_value = p_value, critical_value = critical_value, decision = decision, df = df, pooled_sd = sp)
  )
}

pooled_t_ci_worked <- function(xbar1, xbar2, s1, s2, n1, n2, conf_level, digits = 4) {
  xbar1 <- ensure_numeric_scalar(xbar1, "xbar1")
  xbar2 <- ensure_numeric_scalar(xbar2, "xbar2")
  s1 <- ensure_positive_scalar(s1, "s1")
  s2 <- ensure_positive_scalar(s2, "s2")
  n1 <- ensure_count_scalar(n1, "n1", min_value = 2)
  n2 <- ensure_count_scalar(n2, "n2", min_value = 2)
  conf_level <- ensure_confidence_level(conf_level)

  df <- n1 + n2 - 2
  sp2 <- (((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / df
  sp <- sqrt(sp2)
  se <- sp * sqrt(1 / n1 + 1 / n2)
  t_star <- qt(1 - (1 - conf_level) / 2, df)
  diff_means <- xbar1 - xbar2
  margin_error <- t_star * se
  lower <- diff_means - margin_error
  upper <- diff_means + margin_error

  steps <- c(
    paste0("Pooled variance s_p^2 = ", format_number(sp2, digits)),
    paste0("Pooled SD s_p = ", format_number(sp, digits)),
    paste0("SE = ", format_number(se, digits)),
    paste0("t* = ", format_number(t_star, digits)),
    paste0("CI = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Pooled Two-Sample t Interval",
    notation = "mu1 - mu2",
    formula = "(x_bar1 - x_bar2) ± t* * s_p * sqrt(1/n1 + 1/n2)",
    steps = steps,
    answer = paste0("Confidence interval = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

paired_differences <- function(before, after) {
  before <- ensure_numeric_vector(before, min_length = 2)
  after <- ensure_numeric_vector(after, min_length = 2)
  ensure_same_length(before, after, names = c("before", "after"))
  after - before
}

paired_difference_mean_worked <- function(before, after, digits = 4) {
  d <- paired_differences(before, after)
  result <- mean(d)

  steps <- c(
    paste0("Differences (after - before) = {", join_values(d, digits), "}"),
    paste0("Mean difference d_bar = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Mean of Paired Differences",
    notation = "d_bar",
    formula = "d = after - before, then d_bar = mean(d)",
    steps = steps,
    answer = paste0("Mean paired difference = ", format_number(result, digits)),
    result = result
  )
}

paired_t_test_worked <- function(before, after, alpha, tail, mu0 = 0, digits = 4) {
  d <- paired_differences(before, after)
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)
  mu0 <- ensure_numeric_scalar(mu0, "mu0")

  n <- length(d)
  d_bar <- mean(d)
  s_d <- sd(d)
  t_value <- (d_bar - mu0) / (s_d / sqrt(n))
  df <- n - 1
  p_value <- tail_p_value(t_value, tail, distribution = "t", df = df)
  critical_value <- tail_critical_value(alpha, tail, distribution = "t", df = df)
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("Differences (after - before) = {", join_values(d, digits), "}"),
    paste0("d_bar = ", format_number(d_bar, digits)),
    paste0("s_d = ", format_number(s_d, digits)),
    paste0("df = ", df),
    paste0("t = (d_bar - mu0) / (s_d / sqrt(n)) = ", format_number(t_value, digits)),
    paste0("Critical value = ", format_number(critical_value, digits)),
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "Paired t Test",
    notation = "t",
    formula = "d = after - before, t = (d_bar - mu0) / (s_d / sqrt(n))",
    steps = steps,
    answer = paste0("t = ", format_number(t_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = t_value, p_value = p_value, critical_value = critical_value, decision = decision, df = df)
  )
}

paired_t_ci_worked <- function(before, after, conf_level, digits = 4) {
  d <- paired_differences(before, after)
  conf_level <- ensure_confidence_level(conf_level)

  n <- length(d)
  d_bar <- mean(d)
  s_d <- sd(d)
  df <- n - 1
  t_star <- qt(1 - (1 - conf_level) / 2, df)
  margin_error <- t_star * (s_d / sqrt(n))
  lower <- d_bar - margin_error
  upper <- d_bar + margin_error

  steps <- c(
    paste0("Differences (after - before) = {", join_values(d, digits), "}"),
    paste0("d_bar = ", format_number(d_bar, digits)),
    paste0("s_d = ", format_number(s_d, digits)),
    paste0("df = ", df),
    paste0("t* = ", format_number(t_star, digits)),
    paste0("Margin of error = ", format_number(margin_error, digits)),
    paste0("CI = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Paired t Interval",
    notation = "mu_d",
    formula = "d_bar ± t* * s_d / sqrt(n)",
    steps = steps,
    answer = paste0("Confidence interval = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

two_proportion_ci_worked <- function(x1, n1, x2, n2, conf_level, digits = 4) {
  x1 <- ensure_numeric_scalar(x1, "x1")
  n1 <- ensure_count_scalar(n1, "n1", min_value = 1)
  x2 <- ensure_numeric_scalar(x2, "x2")
  n2 <- ensure_count_scalar(n2, "n2", min_value = 1)
  conf_level <- ensure_confidence_level(conf_level)

  p1_hat <- x1 / n1
  p2_hat <- x2 / n2
  z_star <- qnorm(1 - (1 - conf_level) / 2)
  se <- sqrt((p1_hat * (1 - p1_hat)) / n1 + (p2_hat * (1 - p2_hat)) / n2)
  diff_prop <- p1_hat - p2_hat
  margin_error <- z_star * se
  lower <- diff_prop - margin_error
  upper <- diff_prop + margin_error

  steps <- c(
    paste0("p1_hat = ", format_number(p1_hat, digits)),
    paste0("p2_hat = ", format_number(p2_hat, digits)),
    paste0("Difference = p1_hat - p2_hat = ", format_number(diff_prop, digits)),
    paste0("SE = ", format_number(se, digits)),
    paste0("z* = ", format_number(z_star, digits)),
    paste0("CI = ", format_interval(lower, upper, digits))
  )

  new_worked_calculation(
    title = "Two-Proportion Confidence Interval",
    notation = "p1 - p2",
    formula = "(p1_hat - p2_hat) ± z* * sqrt(p1_hat(1-p1_hat)/n1 + p2_hat(1-p2_hat)/n2)",
    steps = steps,
    answer = paste0("Confidence interval = ", format_interval(lower, upper, digits)),
    result = c(lower = lower, upper = upper)
  )
}

two_proportion_z_test_worked <- function(x1, n1, x2, n2, alpha, tail, digits = 4) {
  x1 <- ensure_numeric_scalar(x1, "x1")
  n1 <- ensure_count_scalar(n1, "n1", min_value = 1)
  x2 <- ensure_numeric_scalar(x2, "x2")
  n2 <- ensure_count_scalar(n2, "n2", min_value = 1)
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  p1_hat <- x1 / n1
  p2_hat <- x2 / n2
  p_pool <- (x1 + x2) / (n1 + n2)
  se <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
  z_value <- (p1_hat - p2_hat) / se
  p_value <- tail_p_value(z_value, tail, distribution = "z")
  critical_value <- tail_critical_value(alpha, tail, distribution = "z")
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"

  steps <- c(
    paste0("p1_hat = ", format_number(p1_hat, digits)),
    paste0("p2_hat = ", format_number(p2_hat, digits)),
    paste0("Pooled proportion = ", format_number(p_pool, digits)),
    paste0("SE = ", format_number(se, digits)),
    paste0("z = ", format_number(z_value, digits)),
    paste0("Critical value = ", format_number(critical_value, digits)),
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision)
  )

  new_worked_calculation(
    title = "Two-Proportion Z Test",
    notation = "z",
    formula = "z = (p1_hat - p2_hat) / sqrt(p_pool(1-p_pool)(1/n1 + 1/n2))",
    steps = steps,
    answer = paste0("z = ", format_number(z_value, digits), "; P-value = ", format_number(p_value, digits), "; ", decision),
    result = list(statistic = z_value, p_value = p_value, critical_value = critical_value, decision = decision)
  )
}
