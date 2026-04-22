build_mean_calculation <- function(x, sample = TRUE, digits = 4) {
  x <- ensure_numeric_vector(x)

  count <- length(x)
  total <- sum(x)
  result <- total / count

  title <- if (sample) "Sample Mean" else "Population Mean"
  symbol <- if (sample) "x_bar" else "mu"
  count_label <- if (sample) "n" else "N"

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0(count_label, " = ", count),
    paste0("Sum of values = ", join_sum(x, digits), " = ", format_number(total, digits)),
    paste0(
      symbol,
      " = ",
      format_number(total, digits),
      " / ",
      count,
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = title,
    notation = symbol,
    formula = paste0(symbol, " = sum(x_i) / ", count_label),
    steps = steps,
    answer = paste0(title, " (", symbol, ") = ", format_number(result, digits))
  )
}

sample_mean <- function(x, digits = 4) {
  build_mean_calculation(x, sample = TRUE, digits = digits)
}

population_mean <- function(x, digits = 4) {
  build_mean_calculation(x, sample = FALSE, digits = digits)
}

median_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x)

  sorted_x <- sort(x)
  count <- length(sorted_x)

  steps <- c(
    paste0("Original data = {", join_values(x, digits), "}"),
    paste0("Sorted data = {", join_values(sorted_x, digits), "}")
  )

  if (count %% 2 == 1) {
    middle_position <- (count + 1) / 2
    result <- sorted_x[middle_position]

    steps <- c(
      steps,
      paste0("n = ", count, " is odd, so the median is the value in position ", middle_position, "."),
      paste0("Median = ", format_number(result, digits))
    )
  } else {
    left_position <- count / 2
    right_position <- left_position + 1
    left_value <- sorted_x[left_position]
    right_value <- sorted_x[right_position]
    result <- (left_value + right_value) / 2

    steps <- c(
      steps,
      paste0(
        "n = ",
        count,
        " is even, so the median is the average of positions ",
        left_position,
        " and ",
        right_position,
        "."
      ),
      paste0(
        "Median = (",
        format_number(left_value, digits),
        " + ",
        format_number(right_value, digits),
        ") / 2 = ",
        format_number(result, digits)
      )
    )
  }

  new_worked_calculation(
    title = "Median",
    notation = "Median",
    formula = "Median is the middle value after sorting the data.",
    steps = steps,
    answer = paste0("Median = ", format_number(result, digits))
  )
}

mode_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x)

  frequency <- frequency_counts(x)
  values <- frequency$values
  counts <- frequency$counts
  highest_frequency <- max(counts)

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0(
      "Frequencies = ",
      paste0(format_number(values, digits), ":", counts, collapse = ", ")
    )
  )

  if (highest_frequency == 1) {
    steps <- c(steps, "Every value appears once, so there is no mode.")

    return(
      new_worked_calculation(
        title = "Mode",
        notation = "Mode",
        formula = "Mode is the value or values with the greatest frequency.",
        steps = steps,
        answer = "No mode"
      )
    )
  }

  modes <- values[counts == highest_frequency]
  mode_text <- join_values(modes, digits)

  steps <- c(
    steps,
    paste0("The greatest frequency is ", highest_frequency, "."),
    paste0("Mode value(s) = ", mode_text)
  )

  new_worked_calculation(
    title = "Mode",
    notation = "Mode",
    formula = "Mode is the value or values with the greatest frequency.",
    steps = steps,
    answer = paste0("Mode = ", mode_text)
  )
}

validate_labeled_counts <- function(labels, counts) {
  if (!is.character(labels) || length(labels) < 1 || any(is.na(labels)) || any(nchar(labels) == 0)) {
    stop("labels must be a non-empty character vector with no blank values.", call. = FALSE)
  }

  counts <- ensure_numeric_vector(counts)
  ensure_same_length(labels, counts, names = c("labels", "counts"))

  if (any(counts < 0) || any(abs(counts - round(counts)) > 1e-9)) {
    stop("counts must be nonnegative integers.", call. = FALSE)
  }

  list(labels = labels, counts = round(counts))
}

most_frequent_category_worked <- function(labels, counts, digits = 4) {
  labeled_counts <- validate_labeled_counts(labels, counts)

  labels <- labeled_counts$labels
  counts <- labeled_counts$counts
  largest_count <- max(counts)
  winners <- labels[counts == largest_count]

  steps <- c(
    paste0(
      "Category counts = ",
      paste0(labels, ":", counts, collapse = "; ")
    ),
    paste0("Largest count = ", largest_count)
  )

  if (length(winners) == 1) {
    steps <- c(steps, paste0("Most frequent category = ", winners[1]))
  } else {
    steps <- c(steps, paste0("Tie for most frequent category = ", paste(winners, collapse = ", ")))
  }

  new_worked_calculation(
    title = "Most Frequent Category",
    notation = "arg max count",
    formula = "Choose the category or categories with the largest frequency.",
    steps = steps,
    answer = paste0("Most frequent category = ", paste(winners, collapse = ", ")),
    result = list(categories = winners, count = largest_count)
  )
}

category_proportion_worked <- function(target_label, labels, counts, digits = 4) {
  labeled_counts <- validate_labeled_counts(labels, counts)

  labels <- labeled_counts$labels
  counts <- labeled_counts$counts

  if (!is.character(target_label) || length(target_label) != 1 || is.na(target_label) || nchar(target_label) == 0) {
    stop("target_label must be a single non-empty character string.", call. = FALSE)
  }

  if (!target_label %in% labels) {
    stop("target_label was not found in labels.", call. = FALSE)
  }

  total_count <- sum(counts)
  target_count <- counts[match(target_label, labels)]
  proportion <- target_count / total_count

  steps <- c(
    paste0(
      "Category counts = ",
      paste0(labels, ":", counts, collapse = "; ")
    ),
    paste0("Target category = ", target_label),
    paste0("Target count = ", target_count),
    paste0("Total count = ", total_count),
    paste0(
      "Proportion = ",
      target_count,
      " / ",
      total_count,
      " = ",
      format_number(proportion, digits)
    )
  )

  new_worked_calculation(
    title = "Category Proportion",
    notation = "p_hat",
    formula = "Proportion = target count / total count",
    steps = steps,
    answer = paste0("Category proportion = ", format_number(proportion, digits), " = ", format_percent(proportion, digits)),
    result = proportion
  )
}

range_worked <- function(x, digits = 4) {
  x <- ensure_numeric_vector(x)

  minimum <- min(x)
  maximum <- max(x)
  result <- maximum - minimum

  steps <- c(
    paste0("Data = {", join_values(x, digits), "}"),
    paste0("Minimum = ", format_number(minimum, digits)),
    paste0("Maximum = ", format_number(maximum, digits)),
    paste0(
      "Range = ",
      format_number(maximum, digits),
      " - ",
      format_number(minimum, digits),
      " = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Range",
    notation = "Range",
    formula = "Range = max(x) - min(x)",
    steps = steps,
    answer = paste0("Range = ", format_number(result, digits))
  )
}
