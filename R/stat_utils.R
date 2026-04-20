ensure_numeric_vector <- function(x, min_length = 1) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.", call. = FALSE)
  }

  if (any(is.na(x))) {
    stop("x cannot contain NA values.", call. = FALSE)
  }

  if (length(x) < min_length) {
    stop(
      paste0("x must contain at least ", min_length, " value(s)."),
      call. = FALSE
    )
  }

  x
}

format_number <- function(x, digits = 4) {
  formatted <- vapply(
    x,
    FUN = function(value) {
      rounded <- round(value, digits)

      if (abs(rounded - round(rounded)) < 10^(-digits)) {
        as.character(as.integer(round(rounded)))
      } else {
        format(rounded, trim = TRUE, scientific = FALSE, nsmall = 0)
      }
    },
    FUN.VALUE = character(1)
  )

  unname(formatted)
}

join_values <- function(x, digits = 4, separator = ", ") {
  paste(format_number(x, digits), collapse = separator)
}

join_sum <- function(x, digits = 4) {
  join_values(x, digits, separator = " + ")
}

frequency_counts <- function(x) {
  values <- sort(unique(x))
  counts <- vapply(values, function(value) sum(x == value), numeric(1))

  list(values = values, counts = counts)
}

join_squared_deviation_setup <- function(x, center, digits = 4) {
  paste0(
    "(",
    format_number(x, digits),
    " - ",
    format_number(center, digits),
    ")^2",
    collapse = " + "
  )
}

join_squared_deviation_values <- function(x, center, digits = 4) {
  deviations <- (x - center)^2
  paste(format_number(deviations, digits), collapse = " + ")
}
