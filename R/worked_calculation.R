new_worked_calculation <- function(title, notation = NULL, formula = NULL, steps, answer) {
  structure(
    list(
      title = title,
      notation = notation,
      formula = formula,
      steps = steps,
      answer = answer
    ),
    class = "worked_calculation"
  )
}

print.worked_calculation <- function(x, ...) {
  cat("\n", x$title, "\n", sep = "")

  if (!is.null(x$notation)) {
    cat("Notation: ", x$notation, "\n", sep = "")
  }

  if (!is.null(x$formula)) {
    cat("Formula: ", x$formula, "\n", sep = "")
  }

  cat("Work:\n")
  for (step in x$steps) {
    cat(" - ", step, "\n", sep = "")
  }

  cat("Answer: ", x$answer, "\n", sep = "")

  invisible(x)
}

print_calculation <- function(calculation) {
  print(calculation)
  invisible(calculation)
}
