geometric_stats_worked <- function(p_success, digits = 4) {
  p_success <- ensure_probability(p_success, "p_success")
  if (p_success == 0) {
    stop("p_success must be greater than 0 for geometric statistics.", call. = FALSE)
  }

  mean_value <- 1 / p_success
  variance_value <- (1 - p_success) / (p_success^2)
  sd_value <- sqrt(variance_value)

  steps <- c(
    paste0("p = ", format_number(p_success, digits)),
    paste0("Mean = 1 / p = ", format_number(mean_value, digits)),
    paste0("Variance = (1 - p) / p^2 = ", format_number(variance_value, digits)),
    paste0("SD = sqrt(Variance) = ", format_number(sd_value, digits))
  )

  new_worked_calculation(
    title = "Geometric Distribution Statistics",
    notation = "X = trial of first success",
    formula = "Mean = 1/p, Variance = (1-p)/p^2",
    steps = steps,
    answer = paste0(
      "Mean = ", format_number(mean_value, digits),
      "; variance = ", format_number(variance_value, digits),
      "; SD = ", format_number(sd_value, digits)
    ),
    result = list(mean = mean_value, variance = variance_value, sd = sd_value)
  )
}

geometric_probability_worked <- function(p_success, event, x_value, value2 = NA_real_, digits = 4) {
  p_success <- ensure_probability(p_success, "p_success")
  event <- ensure_event(event)
  x_value <- ensure_count_scalar(x_value, "x_value", min_value = 1)

  if (event == "between") {
    value2 <- ensure_count_scalar(value2, "value2", min_value = x_value)
  }

  result <- switch(
    event,
    exact = dgeom(x_value - 1, prob = p_success),
    less_than = pgeom(x_value - 2, prob = p_success),
    at_most = pgeom(x_value - 1, prob = p_success),
    greater_than = 1 - pgeom(x_value - 1, prob = p_success),
    at_least = 1 - pgeom(x_value - 2, prob = p_success),
    between = pgeom(value2 - 1, prob = p_success) - pgeom(x_value - 2, prob = p_success)
  )

  steps <- c(
    paste0("p = ", format_number(p_success, digits)),
    switch(
      event,
      exact = paste0("P(X = ", x_value, ") = dgeom(", x_value - 1, ", p)"),
      less_than = paste0("P(X < ", x_value, ") = pgeom(", x_value - 2, ", p)"),
      at_most = paste0("P(X <= ", x_value, ") = pgeom(", x_value - 1, ", p)"),
      greater_than = paste0("P(X > ", x_value, ") = 1 - pgeom(", x_value - 1, ", p)"),
      at_least = paste0("P(X >= ", x_value, ") = 1 - pgeom(", x_value - 2, ", p)"),
      between = paste0("P(", x_value, " <= X <= ", value2, ") = pgeom(", value2 - 1, ", p) - pgeom(", x_value - 2, ", p)")
    ),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Geometric Distribution Probability",
    notation = "X = trial of first success",
    formula = "R uses failures before first success, so convert X to X - 1.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

binomial_stats_worked <- function(n, p, digits = 4) {
  n <- ensure_count_scalar(n, "n", min_value = 1)
  p <- ensure_probability(p, "p")

  mean_value <- n * p
  variance_value <- n * p * (1 - p)
  sd_value <- sqrt(variance_value)

  steps <- c(
    paste0("n = ", n),
    paste0("p = ", format_number(p, digits)),
    paste0("Mean = np = ", format_number(mean_value, digits)),
    paste0("Variance = np(1-p) = ", format_number(variance_value, digits)),
    paste0("SD = sqrt(np(1-p)) = ", format_number(sd_value, digits))
  )

  new_worked_calculation(
    title = "Binomial Distribution Statistics",
    notation = "X ~ Bin(n, p)",
    formula = "Mean = np, Variance = np(1-p), SD = sqrt(np(1-p))",
    steps = steps,
    answer = paste0(
      "Mean = ", format_number(mean_value, digits),
      "; variance = ", format_number(variance_value, digits),
      "; SD = ", format_number(sd_value, digits)
    ),
    result = list(mean = mean_value, variance = variance_value, sd = sd_value)
  )
}

binomial_probability_worked <- function(n, p, event, x_value, value2 = NA_real_, digits = 4) {
  n <- ensure_count_scalar(n, "n", min_value = 1)
  p <- ensure_probability(p, "p")
  event <- ensure_event(event)
  x_value <- ensure_count_scalar(x_value, "x_value", min_value = 0)

  if (event == "between") {
    value2 <- ensure_count_scalar(value2, "value2", min_value = x_value)
  }

  result <- switch(
    event,
    exact = dbinom(x_value, size = n, prob = p),
    less_than = pbinom(x_value - 1, size = n, prob = p),
    at_most = pbinom(x_value, size = n, prob = p),
    greater_than = 1 - pbinom(x_value, size = n, prob = p),
    at_least = 1 - pbinom(x_value - 1, size = n, prob = p),
    between = pbinom(value2, size = n, prob = p) - pbinom(x_value - 1, size = n, prob = p)
  )

  steps <- c(
    paste0("n = ", n, ", p = ", format_number(p, digits)),
    switch(
      event,
      exact = paste0("P(X = ", x_value, ") = dbinom(", x_value, ", n, p)"),
      less_than = paste0("P(X < ", x_value, ") = pbinom(", x_value - 1, ", n, p)"),
      at_most = paste0("P(X <= ", x_value, ") = pbinom(", x_value, ", n, p)"),
      greater_than = paste0("P(X > ", x_value, ") = 1 - pbinom(", x_value, ", n, p)"),
      at_least = paste0("P(X >= ", x_value, ") = 1 - pbinom(", x_value - 1, ", n, p)"),
      between = paste0("P(", x_value, " <= X <= ", value2, ") = pbinom(", value2, ", n, p) - pbinom(", x_value - 1, ", n, p)")
    ),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Binomial Distribution Probability",
    notation = "X ~ Bin(n, p)",
    formula = "Use dbinom for exact values and pbinom for cumulative probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

binomial_quantile_worked <- function(probability, n, p, digits = 4) {
  probability <- ensure_probability(probability, "probability")
  n <- ensure_count_scalar(n, "n", min_value = 1)
  p <- ensure_probability(p, "p")

  result <- qbinom(probability, size = n, prob = p)

  steps <- c(
    paste0("Requested probability = ", format_number(probability, digits)),
    paste0("n = ", n, ", p = ", format_number(p, digits)),
    paste0("Quantile = qbinom(", format_number(probability, digits), ", n, p) = ", result)
  )

  new_worked_calculation(
    title = "Binomial Quantile",
    notation = "qbinom",
    formula = "The quantile is the smallest x with P(X <= x) >= p.",
    steps = steps,
    answer = paste0("Binomial quantile = ", result),
    result = result
  )
}

poisson_stats_worked <- function(lambda, digits = 4) {
  lambda <- ensure_positive_scalar(lambda, "lambda")

  steps <- c(
    paste0("lambda = ", format_number(lambda, digits)),
    paste0("Mean = lambda = ", format_number(lambda, digits)),
    paste0("Variance = lambda = ", format_number(lambda, digits)),
    paste0("SD = sqrt(lambda) = ", format_number(sqrt(lambda), digits))
  )

  new_worked_calculation(
    title = "Poisson Distribution Statistics",
    notation = "X ~ Pois(lambda)",
    formula = "Mean = lambda, Variance = lambda, SD = sqrt(lambda)",
    steps = steps,
    answer = paste0(
      "Mean = ", format_number(lambda, digits),
      "; variance = ", format_number(lambda, digits),
      "; SD = ", format_number(sqrt(lambda), digits)
    ),
    result = list(mean = lambda, variance = lambda, sd = sqrt(lambda))
  )
}

poisson_probability_worked <- function(lambda, event, x_value, value2 = NA_real_, digits = 4) {
  lambda <- ensure_positive_scalar(lambda, "lambda")
  event <- ensure_event(event)
  x_value <- ensure_count_scalar(x_value, "x_value", min_value = 0)

  if (event == "between") {
    value2 <- ensure_count_scalar(value2, "value2", min_value = x_value)
  }

  result <- switch(
    event,
    exact = dpois(x_value, lambda = lambda),
    less_than = ppois(x_value - 1, lambda = lambda),
    at_most = ppois(x_value, lambda = lambda),
    greater_than = 1 - ppois(x_value, lambda = lambda),
    at_least = 1 - ppois(x_value - 1, lambda = lambda),
    between = ppois(value2, lambda = lambda) - ppois(x_value - 1, lambda = lambda)
  )

  steps <- c(
    paste0("lambda = ", format_number(lambda, digits)),
    switch(
      event,
      exact = paste0("P(X = ", x_value, ") = dpois(", x_value, ", lambda)"),
      less_than = paste0("P(X < ", x_value, ") = ppois(", x_value - 1, ", lambda)"),
      at_most = paste0("P(X <= ", x_value, ") = ppois(", x_value, ", lambda)"),
      greater_than = paste0("P(X > ", x_value, ") = 1 - ppois(", x_value, ", lambda)"),
      at_least = paste0("P(X >= ", x_value, ") = 1 - ppois(", x_value - 1, ", lambda)"),
      between = paste0("P(", x_value, " <= X <= ", value2, ") = ppois(", value2, ", lambda) - ppois(", x_value - 1, ", lambda)")
    ),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Poisson Distribution Probability",
    notation = "X ~ Pois(lambda)",
    formula = "Use dpois for exact values and ppois for cumulative probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

poisson_quantile_worked <- function(probability, lambda, digits = 4) {
  probability <- ensure_probability(probability, "probability")
  lambda <- ensure_positive_scalar(lambda, "lambda")

  result <- qpois(probability, lambda = lambda)

  steps <- c(
    paste0("Requested probability = ", format_number(probability, digits)),
    paste0("lambda = ", format_number(lambda, digits)),
    paste0("Quantile = qpois(", format_number(probability, digits), ", lambda) = ", result)
  )

  new_worked_calculation(
    title = "Poisson Quantile",
    notation = "qpois",
    formula = "The quantile is the smallest x with P(X <= x) >= p.",
    steps = steps,
    answer = paste0("Poisson quantile = ", result),
    result = result
  )
}

hypergeometric_stats_worked <- function(successes, failures, draws, digits = 4) {
  successes <- ensure_count_scalar(successes, "successes", min_value = 0)
  failures <- ensure_count_scalar(failures, "failures", min_value = 0)
  draws <- ensure_count_scalar(draws, "draws", min_value = 0)

  population_size <- successes + failures
  mean_value <- draws * (successes / population_size)
  variance_value <- draws * (successes / population_size) * (1 - successes / population_size) * ((population_size - draws) / (population_size - 1))
  sd_value <- sqrt(variance_value)

  steps <- c(
    paste0("Population successes = ", successes),
    paste0("Population failures = ", failures),
    paste0("Draws = ", draws),
    paste0("Mean = n * (K / N) = ", format_number(mean_value, digits)),
    paste0("Variance = n(K/N)(1-K/N)((N-n)/(N-1)) = ", format_number(variance_value, digits)),
    paste0("SD = sqrt(Variance) = ", format_number(sd_value, digits))
  )

  new_worked_calculation(
    title = "Hypergeometric Distribution Statistics",
    notation = "X ~ Hypergeo",
    formula = "Mean = n(K/N), Variance = n(K/N)(1-K/N)((N-n)/(N-1))",
    steps = steps,
    answer = paste0(
      "Mean = ", format_number(mean_value, digits),
      "; variance = ", format_number(variance_value, digits),
      "; SD = ", format_number(sd_value, digits)
    ),
    result = list(mean = mean_value, variance = variance_value, sd = sd_value)
  )
}

hypergeometric_probability_worked <- function(successes, failures, draws, event, x_value, value2 = NA_real_, digits = 4) {
  successes <- ensure_count_scalar(successes, "successes", min_value = 0)
  failures <- ensure_count_scalar(failures, "failures", min_value = 0)
  draws <- ensure_count_scalar(draws, "draws", min_value = 0)
  event <- ensure_event(event)
  x_value <- ensure_count_scalar(x_value, "x_value", min_value = 0)

  if (event == "between") {
    value2 <- ensure_count_scalar(value2, "value2", min_value = x_value)
  }

  result <- switch(
    event,
    exact = dhyper(x_value, m = successes, n = failures, k = draws),
    less_than = phyper(x_value - 1, m = successes, n = failures, k = draws),
    at_most = phyper(x_value, m = successes, n = failures, k = draws),
    greater_than = 1 - phyper(x_value, m = successes, n = failures, k = draws),
    at_least = 1 - phyper(x_value - 1, m = successes, n = failures, k = draws),
    between = phyper(value2, m = successes, n = failures, k = draws) - phyper(x_value - 1, m = successes, n = failures, k = draws)
  )

  steps <- c(
    paste0("Successes in population = ", successes),
    paste0("Failures in population = ", failures),
    paste0("Draws = ", draws),
    switch(
      event,
      exact = paste0("P(X = ", x_value, ") = dhyper(", x_value, ", m, n, k)"),
      less_than = paste0("P(X < ", x_value, ") = phyper(", x_value - 1, ", m, n, k)"),
      at_most = paste0("P(X <= ", x_value, ") = phyper(", x_value, ", m, n, k)"),
      greater_than = paste0("P(X > ", x_value, ") = 1 - phyper(", x_value, ", m, n, k)"),
      at_least = paste0("P(X >= ", x_value, ") = 1 - phyper(", x_value - 1, ", m, n, k)"),
      between = paste0("P(", x_value, " <= X <= ", value2, ") = phyper(", value2, ", m, n, k) - phyper(", x_value - 1, ", m, n, k)")
    ),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Hypergeometric Probability",
    notation = "dhyper / phyper",
    formula = "Use dhyper for exact values and phyper for cumulative probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

uniform_discrete_stats_worked <- function(values, digits = 4) {
  values <- ensure_numeric_vector(values, min_length = 1)
  probabilities <- rep(1 / length(values), length(values))
  mean_value <- sum(values * probabilities)
  variance_value <- sum((values - mean_value)^2 * probabilities)
  sd_value <- sqrt(variance_value)

  steps <- c(
    paste0("Equally likely values = {", join_values(values, digits), "}"),
    paste0("Probability for each value = 1 / ", length(values), " = ", format_number(probabilities[1], digits)),
    paste0("Mean = ", format_number(mean_value, digits)),
    paste0("Variance = ", format_number(variance_value, digits)),
    paste0("SD = ", format_number(sd_value, digits))
  )

  new_worked_calculation(
    title = "Uniform Discrete Statistics",
    notation = "equal p(x)",
    formula = "Assign each value probability 1 / number of values.",
    steps = steps,
    answer = paste0(
      "Mean = ", format_number(mean_value, digits),
      "; variance = ", format_number(variance_value, digits),
      "; SD = ", format_number(sd_value, digits)
    ),
    result = list(mean = mean_value, variance = variance_value, sd = sd_value)
  )
}

normal_probability_worked <- function(mu, sigma, event, x_value, value2 = NA_real_, digits = 4) {
  mu <- ensure_numeric_scalar(mu, "mu")
  sigma <- ensure_positive_scalar(sigma, "sigma")
  event <- ensure_event(event)
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  result <- switch(
    event,
    less_than = pnorm(x_value, mean = mu, sd = sigma),
    at_most = pnorm(x_value, mean = mu, sd = sigma),
    greater_than = 1 - pnorm(x_value, mean = mu, sd = sigma),
    at_least = 1 - pnorm(x_value, mean = mu, sd = sigma),
    between = pnorm(value2, mean = mu, sd = sigma) - pnorm(x_value, mean = mu, sd = sigma),
    stop("Normal probabilities do not support an exact point probability. Use normal_density_worked instead.", call. = FALSE)
  )

  steps <- c(
    paste0("mu = ", format_number(mu, digits)),
    paste0("sigma = ", format_number(sigma, digits)),
    switch(
      event,
      less_than = paste0("P(X < ", format_number(x_value, digits), ") = pnorm(x, mu, sigma)"),
      at_most = paste0("P(X <= ", format_number(x_value, digits), ") = pnorm(x, mu, sigma)"),
      greater_than = paste0("P(X > ", format_number(x_value, digits), ") = 1 - pnorm(x, mu, sigma)"),
      at_least = paste0("P(X >= ", format_number(x_value, digits), ") = 1 - pnorm(x, mu, sigma)"),
      between = paste0("P(", format_number(x_value, digits), " < X < ", format_number(value2, digits), ") = pnorm(upper) - pnorm(lower)")
    ),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Normal Distribution Probability",
    notation = "X ~ N(mu, sigma)",
    formula = "Use pnorm for cumulative normal probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

normal_density_worked <- function(mu, sigma, x_value, digits = 4) {
  mu <- ensure_numeric_scalar(mu, "mu")
  sigma <- ensure_positive_scalar(sigma, "sigma")
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  result <- dnorm(x_value, mean = mu, sd = sigma)

  steps <- c(
    paste0("mu = ", format_number(mu, digits)),
    paste0("sigma = ", format_number(sigma, digits)),
    paste0("Density = dnorm(", format_number(x_value, digits), ", mu, sigma) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Normal Density",
    notation = "f(x)",
    formula = "Use dnorm(x, mean = mu, sd = sigma).",
    steps = steps,
    answer = paste0("Density = ", format_number(result, digits)),
    result = result
  )
}

normal_quantile_worked <- function(probability, mu = 0, sigma = 1, digits = 4) {
  probability <- ensure_probability(probability, "probability")
  mu <- ensure_numeric_scalar(mu, "mu")
  sigma <- ensure_positive_scalar(sigma, "sigma")

  result <- qnorm(probability, mean = mu, sd = sigma)

  steps <- c(
    paste0("Probability = ", format_number(probability, digits)),
    paste0("mu = ", format_number(mu, digits), ", sigma = ", format_number(sigma, digits)),
    paste0("Quantile = qnorm(probability, mu, sigma) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Normal Quantile",
    notation = "qnorm",
    formula = "Use qnorm(p, mean = mu, sd = sigma).",
    steps = steps,
    answer = paste0("Normal quantile = ", format_number(result, digits)),
    result = result
  )
}

standard_normal_probability_worked <- function(event, x_value, value2 = NA_real_, digits = 4) {
  normal_probability_worked(0, 1, event, x_value, value2, digits)
}

uniform_continuous_density_worked <- function(min_value, max_value, x_value, digits = 4) {
  min_value <- ensure_numeric_scalar(min_value, "min_value")
  max_value <- ensure_numeric_scalar(max_value, "max_value")
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  result <- dunif(x_value, min = min_value, max = max_value)

  steps <- c(
    paste0("a = ", format_number(min_value, digits), ", b = ", format_number(max_value, digits)),
    paste0("Density = dunif(", format_number(x_value, digits), ", a, b) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Uniform Density",
    notation = "X ~ U(a, b)",
    formula = "Use dunif(x, min = a, max = b).",
    steps = steps,
    answer = paste0("Density = ", format_number(result, digits)),
    result = result
  )
}

uniform_continuous_probability_worked <- function(min_value, max_value, event, x_value, value2 = NA_real_, digits = 4) {
  min_value <- ensure_numeric_scalar(min_value, "min_value")
  max_value <- ensure_numeric_scalar(max_value, "max_value")
  event <- ensure_event(event)
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  result <- switch(
    event,
    less_than = punif(x_value, min = min_value, max = max_value),
    at_most = punif(x_value, min = min_value, max = max_value),
    greater_than = 1 - punif(x_value, min = min_value, max = max_value),
    at_least = 1 - punif(x_value, min = min_value, max = max_value),
    between = punif(value2, min = min_value, max = max_value) - punif(x_value, min = min_value, max = max_value),
    stop("Uniform continuous exact probability is 0. Use the density helper instead.", call. = FALSE)
  )

  steps <- c(
    paste0("a = ", format_number(min_value, digits), ", b = ", format_number(max_value, digits)),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Uniform Continuous Probability",
    notation = "X ~ U(a, b)",
    formula = "Use punif for cumulative uniform probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

uniform_continuous_quantile_worked <- function(probability, min_value, max_value, digits = 4) {
  probability <- ensure_probability(probability, "probability")
  min_value <- ensure_numeric_scalar(min_value, "min_value")
  max_value <- ensure_numeric_scalar(max_value, "max_value")

  result <- qunif(probability, min = min_value, max = max_value)

  steps <- c(
    paste0("Probability = ", format_number(probability, digits)),
    paste0("a = ", format_number(min_value, digits), ", b = ", format_number(max_value, digits)),
    paste0("Quantile = qunif(probability, a, b) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Uniform Continuous Quantile",
    notation = "qunif",
    formula = "Use qunif(p, min = a, max = b).",
    steps = steps,
    answer = paste0("Quantile = ", format_number(result, digits)),
    result = result
  )
}

exponential_density_worked <- function(rate, x_value, digits = 4) {
  rate <- ensure_positive_scalar(rate, "rate")
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  result <- dexp(x_value, rate = rate)

  steps <- c(
    paste0("lambda = ", format_number(rate, digits)),
    paste0("Density = dexp(", format_number(x_value, digits), ", rate = lambda) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Exponential Density",
    notation = "X ~ Exp(lambda)",
    formula = "Use dexp(x, rate = lambda).",
    steps = steps,
    answer = paste0("Density = ", format_number(result, digits)),
    result = result
  )
}

exponential_probability_worked <- function(rate, event, x_value, value2 = NA_real_, digits = 4) {
  rate <- ensure_positive_scalar(rate, "rate")
  event <- ensure_event(event)
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  result <- switch(
    event,
    less_than = pexp(x_value, rate = rate),
    at_most = pexp(x_value, rate = rate),
    greater_than = 1 - pexp(x_value, rate = rate),
    at_least = 1 - pexp(x_value, rate = rate),
    between = pexp(value2, rate = rate) - pexp(x_value, rate = rate),
    stop("Exponential exact probability is 0. Use the density helper instead.", call. = FALSE)
  )

  steps <- c(
    paste0("lambda = ", format_number(rate, digits)),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Exponential Probability",
    notation = "X ~ Exp(lambda)",
    formula = "Use pexp for cumulative exponential probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

exponential_quantile_worked <- function(probability, rate, digits = 4) {
  probability <- ensure_probability(probability, "probability")
  rate <- ensure_positive_scalar(rate, "rate")

  result <- qexp(probability, rate = rate)

  steps <- c(
    paste0("Probability = ", format_number(probability, digits)),
    paste0("lambda = ", format_number(rate, digits)),
    paste0("Quantile = qexp(probability, rate = lambda) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Exponential Quantile",
    notation = "qexp",
    formula = "Use qexp(p, rate = lambda).",
    steps = steps,
    answer = paste0("Quantile = ", format_number(result, digits)),
    result = result
  )
}

t_distribution_probability_worked <- function(df, event, x_value, value2 = NA_real_, digits = 4) {
  df <- ensure_count_scalar(df, "df", min_value = 1)
  event <- ensure_event(event)
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  result <- switch(
    event,
    less_than = pt(x_value, df),
    at_most = pt(x_value, df),
    greater_than = 1 - pt(x_value, df),
    at_least = 1 - pt(x_value, df),
    between = pt(value2, df) - pt(x_value, df),
    stop("Use density functions separately for exact t density requests.", call. = FALSE)
  )

  steps <- c(
    paste0("df = ", df),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "t Distribution Probability",
    notation = "T ~ t(df)",
    formula = "Use pt for cumulative t probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

t_distribution_quantile_worked <- function(probability, df, digits = 4) {
  probability <- ensure_probability(probability, "probability")
  df <- ensure_count_scalar(df, "df", min_value = 1)
  result <- qt(probability, df)

  steps <- c(
    paste0("Probability = ", format_number(probability, digits)),
    paste0("df = ", df),
    paste0("Quantile = qt(probability, df) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "t Distribution Quantile",
    notation = "qt",
    formula = "Use qt(p, df).",
    steps = steps,
    answer = paste0("t quantile = ", format_number(result, digits)),
    result = result
  )
}

chi_square_probability_worked <- function(df, event, x_value, value2 = NA_real_, digits = 4) {
  df <- ensure_count_scalar(df, "df", min_value = 1)
  event <- ensure_event(event)
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  result <- switch(
    event,
    less_than = pchisq(x_value, df),
    at_most = pchisq(x_value, df),
    greater_than = 1 - pchisq(x_value, df),
    at_least = 1 - pchisq(x_value, df),
    between = pchisq(value2, df) - pchisq(x_value, df),
    stop("Use density functions separately for exact chi-square density requests.", call. = FALSE)
  )

  steps <- c(
    paste0("df = ", df),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Chi-Square Distribution Probability",
    notation = "X^2 ~ ChiSq(df)",
    formula = "Use pchisq for cumulative chi-square probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

chi_square_quantile_worked <- function(probability, df, digits = 4) {
  probability <- ensure_probability(probability, "probability")
  df <- ensure_count_scalar(df, "df", min_value = 1)
  result <- qchisq(probability, df)

  steps <- c(
    paste0("Probability = ", format_number(probability, digits)),
    paste0("df = ", df),
    paste0("Quantile = qchisq(probability, df) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Chi-Square Quantile",
    notation = "qchisq",
    formula = "Use qchisq(p, df).",
    steps = steps,
    answer = paste0("Chi-square quantile = ", format_number(result, digits)),
    result = result
  )
}

f_distribution_probability_worked <- function(df1, df2, event, x_value, value2 = NA_real_, digits = 4) {
  df1 <- ensure_count_scalar(df1, "df1", min_value = 1)
  df2 <- ensure_count_scalar(df2, "df2", min_value = 1)
  event <- ensure_event(event)
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  result <- switch(
    event,
    less_than = pf(x_value, df1, df2),
    at_most = pf(x_value, df1, df2),
    greater_than = 1 - pf(x_value, df1, df2),
    at_least = 1 - pf(x_value, df1, df2),
    between = pf(value2, df1, df2) - pf(x_value, df1, df2),
    stop("Use density functions separately for exact F density requests.", call. = FALSE)
  )

  steps <- c(
    paste0("df1 = ", df1, ", df2 = ", df2),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "F Distribution Probability",
    notation = "F ~ F(df1, df2)",
    formula = "Use pf for cumulative F probabilities.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

f_distribution_quantile_worked <- function(probability, df1, df2, digits = 4) {
  probability <- ensure_probability(probability, "probability")
  df1 <- ensure_count_scalar(df1, "df1", min_value = 1)
  df2 <- ensure_count_scalar(df2, "df2", min_value = 1)
  result <- qf(probability, df1, df2)

  steps <- c(
    paste0("Probability = ", format_number(probability, digits)),
    paste0("df1 = ", df1, ", df2 = ", df2),
    paste0("Quantile = qf(probability, df1, df2) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "F Distribution Quantile",
    notation = "qf",
    formula = "Use qf(p, df1, df2).",
    steps = steps,
    answer = paste0("F quantile = ", format_number(result, digits)),
    result = result
  )
}

sampling_mean_probability_worked <- function(mu, sigma, n, event, x_value, value2 = NA_real_, digits = 4) {
  mu <- ensure_numeric_scalar(mu, "mu")
  sigma <- ensure_positive_scalar(sigma, "sigma")
  n <- ensure_count_scalar(n, "n", min_value = 1)
  event <- ensure_event(event)
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  se <- sigma / sqrt(n)
  result <- switch(
    event,
    less_than = pnorm(x_value, mean = mu, sd = se),
    at_most = pnorm(x_value, mean = mu, sd = se),
    greater_than = 1 - pnorm(x_value, mean = mu, sd = se),
    at_least = 1 - pnorm(x_value, mean = mu, sd = se),
    between = pnorm(value2, mean = mu, sd = se) - pnorm(x_value, mean = mu, sd = se),
    stop("Sampling mean exact probability is 0. Use interval or tail probabilities.", call. = FALSE)
  )

  steps <- c(
    paste0("mu = ", format_number(mu, digits)),
    paste0("sigma = ", format_number(sigma, digits)),
    paste0("n = ", n),
    paste0("SE = sigma / sqrt(n) = ", format_number(se, digits)),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Sampling Distribution of the Mean",
    notation = "X_bar",
    formula = "X_bar ~ N(mu, sigma / sqrt(n))",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

sampling_proportion_probability_worked <- function(p, n, event, phat_value, value2 = NA_real_, digits = 4) {
  p <- ensure_probability(p, "p")
  n <- ensure_count_scalar(n, "n", min_value = 1)
  event <- ensure_event(event)
  phat_value <- ensure_numeric_scalar(phat_value, "phat_value")

  if (event == "between") {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  se <- sqrt(p * (1 - p) / n)
  result <- switch(
    event,
    less_than = pnorm(phat_value, mean = p, sd = se),
    at_most = pnorm(phat_value, mean = p, sd = se),
    greater_than = 1 - pnorm(phat_value, mean = p, sd = se),
    at_least = 1 - pnorm(phat_value, mean = p, sd = se),
    between = pnorm(value2, mean = p, sd = se) - pnorm(phat_value, mean = p, sd = se),
    stop("Sampling proportion exact probability is 0. Use interval or tail probabilities.", call. = FALSE)
  )

  steps <- c(
    paste0("Population proportion p = ", format_number(p, digits)),
    paste0("n = ", n),
    paste0("SE = sqrt(p(1-p)/n) = ", format_number(se, digits)),
    paste0("Probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Sampling Distribution of the Proportion",
    notation = "p_hat",
    formula = "p_hat ~ N(p, sqrt(p(1-p)/n))",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}
