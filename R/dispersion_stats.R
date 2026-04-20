build_variance_details <- function(x, sample = TRUE) {
  minimum_length <- if (sample) 2 else 1
  x <- ensure_numeric_vector(x, min_length = minimum_length)

  count <- length(x)
  center <- mean(x)
  squared_deviations <- (x - center)^2
  total_squared_deviation <- sum(squared_deviations)
  denominator <- if (sample) count - 1 else count

  list(
    x = x,
    count = count,
    center = center,
    squared_deviations = squared_deviations,
    total_squared_deviation = total_squared_deviation,
    denominator = denominator,
    sample = sample
  )
}

variance_steps <- function(details, digits = 4) {
  size_step <- if (details$sample) {
    paste0("n = ", details$count, ", so n - 1 = ", details$denominator)
  } else {
    paste0("N = ", details$count)
  }

  center_symbol <- if (details$sample) "x_bar" else "mu"
  variance_symbol <- if (details$sample) "s^2" else "sigma^2"

  c(
    paste0("Data = {", join_values(details$x, digits), "}"),
    size_step,
    paste0(center_symbol, " = ", format_number(details$center, digits)),
    paste0(
      "Squared deviation setup = ",
      join_squared_deviation_setup(details$x, details$center, digits)
    ),
    paste0(
      "Squared deviations = ",
      join_squared_deviation_values(details$x, details$center, digits),
      " = ",
      format_number(details$total_squared_deviation, digits)
    ),
    paste0(
      variance_symbol,
      " = ",
      format_number(details$total_squared_deviation, digits),
      " / ",
      details$denominator,
      " = ",
      format_number(details$total_squared_deviation / details$denominator, digits)
    )
  )
}

sample_variance <- function(x, digits = 4) {
  details <- build_variance_details(x, sample = TRUE)
  result <- details$total_squared_deviation / details$denominator

  new_worked_calculation(
    title = "Sample Variance",
    notation = "s^2",
    formula = "s^2 = sum((x_i - x_bar)^2) / (n - 1)",
    steps = variance_steps(details, digits),
    answer = paste0("Sample variance (s^2) = ", format_number(result, digits))
  )
}

population_variance <- function(x, digits = 4) {
  details <- build_variance_details(x, sample = FALSE)
  result <- details$total_squared_deviation / details$denominator

  new_worked_calculation(
    title = "Population Variance",
    notation = "sigma^2",
    formula = "sigma^2 = sum((x_i - mu)^2) / N",
    steps = variance_steps(details, digits),
    answer = paste0("Population variance (sigma^2) = ", format_number(result, digits))
  )
}

sample_sd <- function(x, digits = 4) {
  details <- build_variance_details(x, sample = TRUE)
  variance_value <- details$total_squared_deviation / details$denominator
  result <- sqrt(variance_value)
  steps <- c(
    variance_steps(details, digits),
    paste0(
      "s = sqrt(",
      format_number(variance_value, digits),
      ") = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Sample Standard Deviation",
    notation = "s",
    formula = "s = sqrt(sum((x_i - x_bar)^2) / (n - 1))",
    steps = steps,
    answer = paste0("Sample standard deviation (s) = ", format_number(result, digits))
  )
}

population_sd <- function(x, digits = 4) {
  details <- build_variance_details(x, sample = FALSE)
  variance_value <- details$total_squared_deviation / details$denominator
  result <- sqrt(variance_value)
  steps <- c(
    variance_steps(details, digits),
    paste0(
      "sigma = sqrt(",
      format_number(variance_value, digits),
      ") = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Population Standard Deviation",
    notation = "sigma",
    formula = "sigma = sqrt(sum((x_i - mu)^2) / N)",
    steps = steps,
    answer = paste0("Population standard deviation (sigma) = ", format_number(result, digits))
  )
}
