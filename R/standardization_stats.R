z_score_worked <- function(x_value, center_value, spread_value, mean_symbol = "mu", sd_symbol = "sigma", digits = 4) {
  if (!is.numeric(c(x_value, center_value, spread_value))) {
    stop("x_value, center_value, and spread_value must be numeric.", call. = FALSE)
  }

  if (length(x_value) != 1 || length(center_value) != 1 || length(spread_value) != 1) {
    stop("z_score_worked requires single numeric values.", call. = FALSE)
  }

  if (is.na(x_value) || is.na(center_value) || is.na(spread_value)) {
    stop("z_score_worked inputs cannot contain NA values.", call. = FALSE)
  }

  if (spread_value == 0) {
    stop("spread_value cannot be 0 for a z-score.", call. = FALSE)
  }

  result <- (x_value - center_value) / spread_value

  steps <- c(
    paste0("x = ", format_number(x_value, digits)),
    paste0(mean_symbol, " = ", format_number(center_value, digits)),
    paste0(sd_symbol, " = ", format_number(spread_value, digits)),
    paste0(
      "z = (",
      format_number(x_value, digits),
      " - ",
      format_number(center_value, digits),
      ") / ",
      format_number(spread_value, digits),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Z-Score",
    notation = "z",
    formula = paste0("z = (x - ", mean_symbol, ") / ", sd_symbol),
    steps = steps,
    answer = paste0("z = ", format_number(result, digits))
  )
}

value_from_z_worked <- function(
  z_value,
  center_value,
  spread_value,
  mean_symbol = "mu",
  sd_symbol = "sigma",
  digits = 4
) {
  if (!is.numeric(c(z_value, center_value, spread_value))) {
    stop("z_value, center_value, and spread_value must be numeric.", call. = FALSE)
  }

  if (length(z_value) != 1 || length(center_value) != 1 || length(spread_value) != 1) {
    stop("value_from_z_worked requires single numeric values.", call. = FALSE)
  }

  if (is.na(z_value) || is.na(center_value) || is.na(spread_value)) {
    stop("value_from_z_worked inputs cannot contain NA values.", call. = FALSE)
  }

  if (spread_value == 0) {
    stop("spread_value cannot be 0 when solving for x from a z-score.", call. = FALSE)
  }

  result <- center_value + z_value * spread_value

  steps <- c(
    paste0("z = ", format_number(z_value, digits)),
    paste0(mean_symbol, " = ", format_number(center_value, digits)),
    paste0(sd_symbol, " = ", format_number(spread_value, digits)),
    paste0("Rearrange z = (x - ", mean_symbol, ") / ", sd_symbol, " into x = ", mean_symbol, " + z * ", sd_symbol, "."),
    paste0(
      "x = ",
      format_number(center_value, digits),
      " + ",
      format_number(z_value, digits),
      " * ",
      format_number(spread_value, digits),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Value From Z-Score",
    notation = "x",
    formula = paste0("x = ", mean_symbol, " + z * ", sd_symbol),
    steps = steps,
    answer = paste0("x = ", format_number(result, digits)),
    result = result
  )
}
