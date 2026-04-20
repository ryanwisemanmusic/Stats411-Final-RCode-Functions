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
