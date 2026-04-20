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

ensure_numeric_scalar <- function(x, name = "value") {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop(paste0(name, " must be a single numeric value."), call. = FALSE)
  }

  x
}

ensure_positive_scalar <- function(x, name = "value", allow_zero = FALSE) {
  x <- ensure_numeric_scalar(x, name)

  if (allow_zero) {
    if (x < 0) {
      stop(paste0(name, " must be at least 0."), call. = FALSE)
    }
  } else if (x <= 0) {
    stop(paste0(name, " must be greater than 0."), call. = FALSE)
  }

  x
}

ensure_probability <- function(p, name = "p") {
  p <- ensure_numeric_scalar(p, name)

  if (p < 0 || p > 1) {
    stop(paste0(name, " must be between 0 and 1."), call. = FALSE)
  }

  p
}

ensure_probabilities <- function(p, name = "p") {
  p <- ensure_numeric_vector(p)

  if (any(p < 0 | p > 1)) {
    stop(paste0(name, " values must all be between 0 and 1."), call. = FALSE)
  }

  p
}

ensure_count_scalar <- function(x, name = "value", min_value = 0) {
  x <- ensure_numeric_scalar(x, name)

  if (abs(x - round(x)) > 1e-9 || x < min_value) {
    stop(
      paste0(name, " must be an integer greater than or equal to ", min_value, "."),
      call. = FALSE
    )
  }

  as.integer(round(x))
}

ensure_same_length <- function(..., names = NULL) {
  vectors <- list(...)
  lengths <- vapply(vectors, length, integer(1))

  if (length(unique(lengths)) != 1) {
    if (is.null(names)) {
      stop("All vectors must have the same length.", call. = FALSE)
    }

    stop(
      paste0("The following vectors must have the same length: ", paste(names, collapse = ", "), "."),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

ensure_tail <- function(tail) {
  if (!tail %in% c("left", "right", "two")) {
    stop("tail must be one of: left, right, two.", call. = FALSE)
  }

  tail
}

ensure_event <- function(event) {
  valid_events <- c(
    "exact",
    "less_than",
    "at_most",
    "greater_than",
    "at_least",
    "between"
  )

  if (!event %in% valid_events) {
    stop(
      paste0("event must be one of: ", paste(valid_events, collapse = ", "), "."),
      call. = FALSE
    )
  }

  event
}

ensure_confidence_level <- function(conf_level) {
  conf_level <- ensure_probability(conf_level, "conf_level")

  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be strictly between 0 and 1.", call. = FALSE)
  }

  conf_level
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

format_interval <- function(lower, upper, digits = 4) {
  paste0("(", format_number(lower, digits), ", ", format_number(upper, digits), ")")
}

format_percent <- function(x, digits = 2) {
  paste0(format_number(x * 100, digits), "%")
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

tail_symbol <- function(tail) {
  tail <- ensure_tail(tail)

  switch(
    tail,
    left = "<",
    right = ">",
    two = "!="
  )
}

tail_description <- function(tail) {
  tail <- ensure_tail(tail)

  switch(
    tail,
    left = "left-tailed",
    right = "right-tailed",
    two = "two-tailed"
  )
}

tail_p_value <- function(statistic, tail, distribution = "z", df = NULL) {
  tail <- ensure_tail(tail)

  if (distribution == "z") {
    return(
      switch(
        tail,
        left = pnorm(statistic),
        right = pnorm(statistic, lower.tail = FALSE),
        two = 2 * pnorm(abs(statistic), lower.tail = FALSE)
      )
    )
  }

  if (distribution == "t") {
    if (is.null(df)) {
      stop("df is required for a t distribution p-value.", call. = FALSE)
    }

    return(
      switch(
        tail,
        left = pt(statistic, df),
        right = pt(statistic, df, lower.tail = FALSE),
        two = 2 * pt(-abs(statistic), df)
      )
    )
  }

  if (distribution == "chisq") {
    if (is.null(df)) {
      stop("df is required for a chi-square distribution p-value.", call. = FALSE)
    }

    return(
      switch(
        tail,
        left = pchisq(statistic, df),
        right = pchisq(statistic, df, lower.tail = FALSE),
        two = {
          lower_tail <- pchisq(statistic, df)
          upper_tail <- pchisq(statistic, df, lower.tail = FALSE)
          2 * min(lower_tail, upper_tail)
        }
      )
    )
  }

  stop("Unsupported distribution for tail_p_value.", call. = FALSE)
}

tail_critical_value <- function(alpha, tail, distribution = "z", df = NULL) {
  alpha <- ensure_probability(alpha, "alpha")
  tail <- ensure_tail(tail)

  if (distribution == "z") {
    return(
      switch(
        tail,
        left = qnorm(alpha),
        right = qnorm(1 - alpha),
        two = qnorm(1 - alpha / 2)
      )
    )
  }

  if (distribution == "t") {
    if (is.null(df)) {
      stop("df is required for a t critical value.", call. = FALSE)
    }

    return(
      switch(
        tail,
        left = qt(alpha, df),
        right = qt(1 - alpha, df),
        two = qt(1 - alpha / 2, df)
      )
    )
  }

  if (distribution == "chisq") {
    if (is.null(df)) {
      stop("df is required for a chi-square critical value.", call. = FALSE)
    }

    return(
      switch(
        tail,
        left = qchisq(alpha, df),
        right = qchisq(alpha, df, lower.tail = FALSE),
        two = c(
          lower = qchisq(alpha / 2, df),
          upper = qchisq(alpha / 2, df, lower.tail = FALSE)
        )
      )
    )
  }

  stop("Unsupported distribution for tail_critical_value.", call. = FALSE)
}

split_interleaved_pairs <- function(values) {
  values <- ensure_numeric_vector(values, min_length = 2)

  if (length(values) %% 2 != 0) {
    stop("Expected an even number of values for paired data.", call. = FALSE)
  }

  matrix(values, ncol = 2, byrow = TRUE)
}

split_interleaved_triplets <- function(values) {
  values <- ensure_numeric_vector(values, min_length = 3)

  if (length(values) %% 3 != 0) {
    stop("Expected a multiple of 3 values for grouped data.", call. = FALSE)
  }

  matrix(values, ncol = 3, byrow = TRUE)
}

split_interleaved_distribution_pairs <- function(values) {
  values <- ensure_numeric_vector(values, min_length = 2)

  if (length(values) %% 2 != 0) {
    stop("Expected x and p values to be passed as interleaved pairs.", call. = FALSE)
  }

  matrix(values, ncol = 2, byrow = TRUE)
}
