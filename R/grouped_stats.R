validate_grouped_bounds <- function(lower, upper) {
  lower <- ensure_numeric_vector(lower)
  upper <- ensure_numeric_vector(upper)
  ensure_same_length(lower, upper, names = c("lower", "upper"))

  if (any(upper <= lower)) {
    stop("Each upper class boundary must be larger than its lower boundary.", call. = FALSE)
  }

  list(lower = lower, upper = upper)
}

validate_grouped_inputs <- function(lower, upper, freq) {
  bounds <- validate_grouped_bounds(lower, upper)
  freq <- ensure_numeric_vector(freq)
  ensure_same_length(bounds$lower, bounds$upper, freq, names = c("lower", "upper", "freq"))

  if (any(freq < 0) || any(abs(freq - round(freq)) > 1e-9)) {
    stop("Grouped frequencies must be nonnegative integers.", call. = FALSE)
  }

  list(lower = bounds$lower, upper = bounds$upper, freq = round(freq))
}

grouped_details <- function(lower, upper, freq, sample = TRUE) {
  grouped <- validate_grouped_inputs(lower, upper, freq)

  lower <- grouped$lower
  upper <- grouped$upper
  freq <- grouped$freq

  midpoints <- (lower + upper) / 2
  n_total <- sum(freq)
  mean_value <- sum(freq * midpoints) / n_total
  ss <- sum(freq * (midpoints - mean_value)^2)
  denominator <- if (sample) n_total - 1 else n_total
  variance_value <- ss / denominator

  list(
    lower = lower,
    upper = upper,
    freq = freq,
    midpoints = midpoints,
    n_total = n_total,
    mean_value = mean_value,
    ss = ss,
    denominator = denominator,
    variance_value = variance_value,
    sample = sample
  )
}

build_grouped_steps <- function(details, digits = 4) {
  label_mean <- if (details$sample) "x_bar_g" else "mu_g"
  label_var <- if (details$sample) "s_g^2" else "sigma_g^2"
  label_sd <- if (details$sample) "s_g" else "sigma_g"
  count_label <- if (details$sample) "n" else "N"

  c(
    paste0(
      "Classes = ",
      paste0(
        "[",
        format_number(details$lower, digits),
        ", ",
        format_number(details$upper, digits),
        "] f=",
        details$freq,
        collapse = "; "
      )
    ),
    paste0("Midpoints = {", join_values(details$midpoints, digits), "}"),
    paste0(count_label, " = sum(f) = ", details$n_total),
    paste0(
      label_mean,
      " = sum(f * midpoint) / ",
      count_label,
      " = ",
      format_number(sum(details$freq * details$midpoints), digits),
      " / ",
      details$n_total,
      " = ",
      format_number(details$mean_value, digits)
    ),
    paste0(
      "SS = sum(f * (midpoint - center)^2) = ",
      format_number(details$ss, digits)
    ),
    paste0(
      label_var,
      " = SS / ",
      details$denominator,
      " = ",
      format_number(details$variance_value, digits)
    ),
    paste0(
      label_sd,
      " = sqrt(",
      format_number(details$variance_value, digits),
      ") = ",
      format_number(sqrt(details$variance_value), digits)
    )
  )
}

grouped_sample_stats_worked <- function(lower, upper, freq, digits = 4) {
  details <- grouped_details(lower, upper, freq, sample = TRUE)

  new_worked_calculation(
    title = "Grouped Sample Statistics",
    notation = "x_bar_g, s_g^2, s_g",
    formula = "Use class midpoints with frequencies.",
    steps = build_grouped_steps(details, digits),
    answer = paste0(
      "Grouped sample mean = ", format_number(details$mean_value, digits),
      "; variance = ", format_number(details$variance_value, digits),
      "; SD = ", format_number(sqrt(details$variance_value), digits)
    ),
    result = list(
      mean = details$mean_value,
      variance = details$variance_value,
      sd = sqrt(details$variance_value)
    )
  )
}

grouped_population_stats_worked <- function(lower, upper, freq, digits = 4) {
  details <- grouped_details(lower, upper, freq, sample = FALSE)

  new_worked_calculation(
    title = "Grouped Population Statistics",
    notation = "mu_g, sigma_g^2, sigma_g",
    formula = "Use class midpoints with frequencies and divide by N.",
    steps = build_grouped_steps(details, digits),
    answer = paste0(
      "Grouped population mean = ", format_number(details$mean_value, digits),
      "; variance = ", format_number(details$variance_value, digits),
      "; SD = ", format_number(sqrt(details$variance_value), digits)
    ),
    result = list(
      mean = details$mean_value,
      variance = details$variance_value,
      sd = sqrt(details$variance_value)
    )
  )
}

grouped_replication_stats_worked <- function(lower, upper, freq, digits = 4) {
  grouped <- validate_grouped_inputs(lower, upper, freq)
  midpoints <- (grouped$lower + grouped$upper) / 2
  replicated <- rep(midpoints, grouped$freq)

  mean_value <- mean(replicated)
  variance_value <- var(replicated)
  sd_value <- sd(replicated)

  steps <- c(
    paste0("Midpoints = {", join_values(midpoints, digits), "}"),
    paste0("Frequencies = {", join_values(grouped$freq, digits), "}"),
    paste0("Replicated midpoint vector length = ", length(replicated)),
    paste0("Mean of replicated data = ", format_number(mean_value, digits)),
    paste0("Sample variance of replicated data = ", format_number(variance_value, digits)),
    paste0("Sample SD of replicated data = ", format_number(sd_value, digits))
  )

  new_worked_calculation(
    title = "Grouped Data Replication Method",
    notation = "rep(midpoint, freq)",
    formula = "Replicate each midpoint by its class frequency and compute raw-data statistics.",
    steps = steps,
    answer = paste0(
      "Replicated mean = ", format_number(mean_value, digits),
      "; variance = ", format_number(variance_value, digits),
      "; SD = ", format_number(sd_value, digits)
    ),
    result = list(mean = mean_value, variance = variance_value, sd = sd_value)
  )
}

grouped_midpoints_worked <- function(lower, upper, digits = 4) {
  bounds <- validate_grouped_bounds(lower, upper)
  midpoints <- (bounds$lower + bounds$upper) / 2

  steps <- c(
    paste0(
      "Classes = ",
      paste0(
        "[",
        format_number(bounds$lower, digits),
        ", ",
        format_number(bounds$upper, digits),
        "]",
        collapse = "; "
      )
    ),
    paste0(
      "Midpoints = (lower + upper) / 2 = {",
      join_values(midpoints, digits),
      "}"
    )
  )

  new_worked_calculation(
    title = "Grouped Midpoints",
    notation = "class midpoint",
    formula = "Midpoint = (lower class limit + upper class limit) / 2",
    steps = steps,
    answer = paste0("Grouped midpoints = {", join_values(midpoints, digits), "}"),
    result = midpoints
  )
}

grouped_midpoint_at_worked <- function(lower, upper, class_index, digits = 4) {
  bounds <- validate_grouped_bounds(lower, upper)
  class_index <- ensure_count_scalar(class_index, "class_index", min_value = 1)

  if (class_index > length(bounds$lower)) {
    stop("class_index cannot be larger than the number of classes.", call. = FALSE)
  }

  lower_value <- bounds$lower[class_index]
  upper_value <- bounds$upper[class_index]
  midpoint <- (lower_value + upper_value) / 2

  steps <- c(
    paste0("Class index = ", class_index),
    paste0("Lower class limit = ", format_number(lower_value, digits)),
    paste0("Upper class limit = ", format_number(upper_value, digits)),
    paste0(
      "Midpoint = (",
      format_number(lower_value, digits),
      " + ",
      format_number(upper_value, digits),
      ") / 2 = ",
      format_number(midpoint, digits)
    )
  )

  new_worked_calculation(
    title = "Grouped Midpoint For One Class",
    notation = "class midpoint",
    formula = "Midpoint = (lower class limit + upper class limit) / 2",
    steps = steps,
    answer = paste0("Class midpoint = ", format_number(midpoint, digits)),
    result = midpoint
  )
}
