complement_rule_worked <- function(p_a, digits = 4) {
  p_a <- ensure_probability(p_a, "p_a")
  result <- 1 - p_a

  steps <- c(
    paste0("P(A) = ", format_number(p_a, digits)),
    paste0("P(A^c) = 1 - P(A) = 1 - ", format_number(p_a, digits), " = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Complement Rule",
    notation = "P(A^c)",
    formula = "P(A^c) = 1 - P(A)",
    steps = steps,
    answer = paste0("P(A^c) = ", format_number(result, digits)),
    result = result
  )
}

addition_rule_worked <- function(p_a, p_b, p_a_and_b, digits = 4) {
  p_a <- ensure_probability(p_a, "p_a")
  p_b <- ensure_probability(p_b, "p_b")
  p_a_and_b <- ensure_probability(p_a_and_b, "p_a_and_b")

  result <- p_a + p_b - p_a_and_b

  steps <- c(
    paste0("P(A) = ", format_number(p_a, digits)),
    paste0("P(B) = ", format_number(p_b, digits)),
    paste0("P(A and B) = ", format_number(p_a_and_b, digits)),
    paste0(
      "P(A or B) = P(A) + P(B) - P(A and B) = ",
      format_number(p_a, digits),
      " + ",
      format_number(p_b, digits),
      " - ",
      format_number(p_a_and_b, digits),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Addition Rule",
    notation = "P(A or B)",
    formula = "P(A or B) = P(A) + P(B) - P(A and B)",
    steps = steps,
    answer = paste0("P(A or B) = ", format_number(result, digits)),
    result = result
  )
}

multiplication_independent_worked <- function(probabilities, digits = 4) {
  probabilities <- ensure_probabilities(probabilities, "probabilities")
  result <- prod(probabilities)

  steps <- c(
    paste0("Independent probabilities = {", join_values(probabilities, digits), "}"),
    paste0(
      "Multiply them: ",
      join_values(probabilities, digits, separator = " * "),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Multiplication Rule (Independent)",
    notation = "P(A and B ...)",
    formula = "Multiply independent event probabilities.",
    steps = steps,
    answer = paste0("Joint probability = ", format_number(result, digits)),
    result = result
  )
}

multiplication_dependent_worked <- function(probabilities, digits = 4) {
  probabilities <- ensure_probabilities(probabilities, "probabilities")
  result <- prod(probabilities)

  steps <- c(
    paste0("Dependent step probabilities = {", join_values(probabilities, digits), "}"),
    paste0(
      "Joint probability = ",
      join_values(probabilities, digits, separator = " * "),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Multiplication Rule (Dependent)",
    notation = "P(A then B ...)",
    formula = "Multiply the conditional step probabilities in order.",
    steps = steps,
    answer = paste0("Joint probability = ", format_number(result, digits)),
    result = result
  )
}

independence_check_worked <- function(p_a, p_b, p_a_and_b, digits = 4) {
  p_a <- ensure_probability(p_a, "p_a")
  p_b <- ensure_probability(p_b, "p_b")
  p_a_and_b <- ensure_probability(p_a_and_b, "p_a_and_b")

  expected_joint <- p_a * p_b
  result <- abs(p_a_and_b - expected_joint) < 1e-12

  steps <- c(
    paste0("P(A) = ", format_number(p_a, digits)),
    paste0("P(B) = ", format_number(p_b, digits)),
    paste0("Observed P(A and B) = ", format_number(p_a_and_b, digits)),
    paste0("If independent, P(A)P(B) = ", format_number(expected_joint, digits)),
    if (result) {
      "Since P(A and B) = P(A)P(B), the events are independent."
    } else {
      "Since P(A and B) != P(A)P(B), the events are not independent."
    }
  )

  new_worked_calculation(
    title = "Independence Check",
    notation = "P(A and B) ?= P(A)P(B)",
    formula = "Events are independent when P(A and B) = P(A)P(B).",
    steps = steps,
    answer = if (result) "The events are independent." else "The events are not independent.",
    result = result
  )
}

bayes_theorem_worked <- function(p_a, p_b_given_a, p_b_given_not_a, digits = 4) {
  p_a <- ensure_probability(p_a, "p_a")
  p_b_given_a <- ensure_probability(p_b_given_a, "p_b_given_a")
  p_b_given_not_a <- ensure_probability(p_b_given_not_a, "p_b_given_not_a")

  p_b <- p_b_given_a * p_a + p_b_given_not_a * (1 - p_a)
  result <- (p_b_given_a * p_a) / p_b

  steps <- c(
    paste0("P(A) = ", format_number(p_a, digits)),
    paste0("P(B|A) = ", format_number(p_b_given_a, digits)),
    paste0("P(B|A^c) = ", format_number(p_b_given_not_a, digits)),
    paste0(
      "P(B) = P(B|A)P(A) + P(B|A^c)P(A^c) = ",
      format_number(p_b, digits)
    ),
    paste0(
      "P(A|B) = P(B|A)P(A) / P(B) = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Bayes' Theorem",
    notation = "P(A|B)",
    formula = "P(A|B) = P(B|A)P(A) / P(B)",
    steps = steps,
    answer = paste0("P(A|B) = ", format_number(result, digits)),
    result = result
  )
}

factorial_worked <- function(n, digits = 4) {
  n <- ensure_count_scalar(n, "n")
  result <- factorial(n)

  steps <- c(
    paste0("n = ", n),
    paste0(n, "! = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Factorial",
    notation = "n!",
    formula = "n! = n(n-1)(n-2)...1",
    steps = steps,
    answer = paste0(n, "! = ", format_number(result, digits)),
    result = result
  )
}

permutation_worked <- function(n, r, digits = 4) {
  n <- ensure_count_scalar(n, "n", min_value = 1)
  r <- ensure_count_scalar(r, "r", min_value = 0)

  if (r > n) {
    stop("r cannot be larger than n.", call. = FALSE)
  }

  result <- factorial(n) / factorial(n - r)

  steps <- c(
    paste0("n = ", n, ", r = ", r),
    paste0("nPr = n! / (n-r)! = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Permutations",
    notation = "nPr",
    formula = "nPr = n! / (n-r)!",
    steps = steps,
    answer = paste0("Permutations = ", format_number(result, digits)),
    result = result
  )
}

combination_worked <- function(n, r, digits = 4) {
  n <- ensure_count_scalar(n, "n", min_value = 1)
  r <- ensure_count_scalar(r, "r", min_value = 0)

  if (r > n) {
    stop("r cannot be larger than n.", call. = FALSE)
  }

  result <- choose(n, r)

  steps <- c(
    paste0("n = ", n, ", r = ", r),
    paste0("nCr = n! / (r!(n-r)!) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Combinations",
    notation = "nCr",
    formula = "nCr = n! / (r!(n-r)!)",
    steps = steps,
    answer = paste0("Combinations = ", format_number(result, digits)),
    result = result
  )
}

strings_with_repetition_worked <- function(symbol_count, length_value, digits = 4) {
  symbol_count <- ensure_count_scalar(symbol_count, "symbol_count", min_value = 1)
  length_value <- ensure_count_scalar(length_value, "length_value", min_value = 1)

  result <- symbol_count^length_value

  steps <- c(
    paste0("Number of symbols = ", symbol_count),
    paste0("String length = ", length_value),
    paste0(
      "Count = m^k = ",
      symbol_count,
      "^",
      length_value,
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Strings With Repetition",
    notation = "m^k",
    formula = "If each of k positions has m choices, count = m^k.",
    steps = steps,
    answer = paste0("Number of strings = ", format_number(result, digits)),
    result = result
  )
}

at_least_one_count_worked <- function(total_choices, length_value, no_target_choices, digits = 4) {
  total_choices <- ensure_count_scalar(total_choices, "total_choices", min_value = 1)
  length_value <- ensure_count_scalar(length_value, "length_value", min_value = 1)
  no_target_choices <- ensure_count_scalar(no_target_choices, "no_target_choices", min_value = 0)

  total_outcomes <- total_choices^length_value
  no_target_outcomes <- no_target_choices^length_value
  result <- total_outcomes - no_target_outcomes

  steps <- c(
    paste0("Total choices per position = ", total_choices),
    paste0("Choices avoiding the target per position = ", no_target_choices),
    paste0("Sequence length = ", length_value),
    paste0("Total outcomes = ", total_choices, "^", length_value, " = ", format_number(total_outcomes, digits)),
    paste0("No-target outcomes = ", no_target_choices, "^", length_value, " = ", format_number(no_target_outcomes, digits)),
    paste0("At least one target = total - no-target = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "At Least One Via Complement",
    notation = "total - none",
    formula = "Count(at least one) = total outcomes - outcomes with none of the target.",
    steps = steps,
    answer = paste0("Count with at least one target = ", format_number(result, digits)),
    result = result
  )
}

discrete_distribution_stats_worked <- function(x, p, digits = 4) {
  x <- ensure_numeric_vector(x)
  p <- ensure_probabilities(p, "p")
  ensure_same_length(x, p, names = c("x", "p"))

  probability_sum <- sum(p)
  mu <- sum(x * p)
  variance_value <- sum((x - mu)^2 * p)
  sd_value <- sqrt(variance_value)
  valid <- abs(probability_sum - 1) < 1e-9

  steps <- c(
    paste0("x values = {", join_values(x, digits), "}"),
    paste0("p(x) values = {", join_values(p, digits), "}"),
    paste0("Sum of probabilities = ", format_number(probability_sum, digits)),
    if (valid) {
      "This is a valid PMF because the probabilities sum to 1."
    } else {
      "This is not a valid PMF because the probabilities do not sum to 1."
    },
    paste0("mu = sum(x * p(x)) = ", format_number(mu, digits)),
    paste0("Var(X) = sum((x - mu)^2 p(x)) = ", format_number(variance_value, digits)),
    paste0("SD(X) = sqrt(Var(X)) = ", format_number(sd_value, digits))
  )

  new_worked_calculation(
    title = "Discrete Distribution Statistics",
    notation = "mu, Var(X), SD(X)",
    formula = "mu = sum(xp), Var(X) = sum((x-mu)^2 p), SD = sqrt(Var)",
    steps = steps,
    answer = paste0(
      "mu = ", format_number(mu, digits),
      "; variance = ", format_number(variance_value, digits),
      "; SD = ", format_number(sd_value, digits)
    ),
    result = list(valid = valid, probability_sum = probability_sum, mean = mu, variance = variance_value, sd = sd_value)
  )
}

discrete_table_probability_worked <- function(x, p, event, value1, value2 = NA_real_, digits = 4) {
  x <- ensure_numeric_vector(x)
  p <- ensure_probabilities(p, "p")
  ensure_same_length(x, p, names = c("x", "p"))
  event <- ensure_event(event)
  value1 <- ensure_numeric_scalar(value1, "value1")
  if (!is.na(value2)) {
    value2 <- ensure_numeric_scalar(value2, "value2")
  }

  mask <- switch(
    event,
    exact = x == value1,
    less_than = x < value1,
    at_most = x <= value1,
    greater_than = x > value1,
    at_least = x >= value1,
    between = x >= value1 & x <= value2
  )

  result <- sum(p[mask])
  selected_x <- x[mask]
  selected_p <- p[mask]

  steps <- c(
    paste0("x values = {", join_values(x, digits), "}"),
    paste0("p(x) values = {", join_values(p, digits), "}"),
    switch(
      event,
      exact = paste0("Selected x = ", format_number(value1, digits)),
      less_than = paste0("Selected x < ", format_number(value1, digits)),
      at_most = paste0("Selected x <= ", format_number(value1, digits)),
      greater_than = paste0("Selected x > ", format_number(value1, digits)),
      at_least = paste0("Selected x >= ", format_number(value1, digits)),
      between = paste0("Selected ", format_number(value1, digits), " <= x <= ", format_number(value2, digits))
    ),
    paste0(
      "Included probabilities = {",
      if (length(selected_p) == 0) "" else join_values(selected_p, digits),
      "}"
    ),
    paste0("Requested probability = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Discrete Table Probability",
    notation = "sum p(x)",
    formula = "Add the probabilities for the x values in the requested event.",
    steps = steps,
    answer = paste0("Probability = ", format_number(result, digits)),
    result = result
  )
}

empirical_distribution_worked <- function(data_values, digits = 4) {
  data_values <- ensure_numeric_vector(data_values)

  freq <- table(data_values)
  p_emp <- prop.table(freq)
  x_values <- as.numeric(names(p_emp))
  p_values <- as.numeric(p_emp)
  mean_value <- sum(x_values * p_values)
  variance_value <- sum((x_values - mean_value)^2 * p_values)
  sd_value <- sqrt(variance_value)

  steps <- c(
    paste0("Raw data = {", join_values(data_values, digits), "}"),
    paste0("Frequency table = ", paste0(names(freq), ":", as.integer(freq), collapse = ", ")),
    paste0("Empirical probabilities = ", paste0(names(freq), ":", format_number(p_values, digits), collapse = ", ")),
    paste0("Empirical mean = ", format_number(mean_value, digits)),
    paste0("Empirical variance = ", format_number(variance_value, digits)),
    paste0("Empirical SD = ", format_number(sd_value, digits))
  )

  new_worked_calculation(
    title = "Empirical Distribution",
    notation = "p_emp(x)",
    formula = "Use relative frequencies as probabilities.",
    steps = steps,
    answer = paste0(
      "Empirical mean = ", format_number(mean_value, digits),
      "; variance = ", format_number(variance_value, digits),
      "; SD = ", format_number(sd_value, digits)
    ),
    result = list(frequency = freq, probabilities = p_emp, mean = mean_value, variance = variance_value, sd = sd_value)
  )
}
