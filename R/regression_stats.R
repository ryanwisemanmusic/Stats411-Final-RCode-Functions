correlation_worked <- function(x, y, digits = 4) {
  x <- ensure_numeric_vector(x, min_length = 2)
  y <- ensure_numeric_vector(y, min_length = 2)
  ensure_same_length(x, y, names = c("x", "y"))

  result <- cor(x, y)

  steps <- c(
    paste0("x = {", join_values(x, digits), "}"),
    paste0("y = {", join_values(y, digits), "}"),
    paste0("r = cor(x, y) = ", format_number(result, digits))
  )

  new_worked_calculation(
    title = "Correlation Coefficient",
    notation = "r",
    formula = "r = cor(x, y)",
    steps = steps,
    answer = paste0("Correlation coefficient = ", format_number(result, digits)),
    result = result
  )
}

regression_line_worked <- function(x, y, digits = 4) {
  x <- ensure_numeric_vector(x, min_length = 2)
  y <- ensure_numeric_vector(y, min_length = 2)
  ensure_same_length(x, y, names = c("x", "y"))

  model <- lm(y ~ x)
  coefficients <- coef(model)
  intercept <- unname(coefficients[1])
  slope <- unname(coefficients[2])

  steps <- c(
    paste0("x = {", join_values(x, digits), "}"),
    paste0("y = {", join_values(y, digits), "}"),
    paste0("Intercept b0 = ", format_number(intercept, digits)),
    paste0("Slope b1 = ", format_number(slope, digits)),
    paste0("Estimated line: y_hat = ", format_number(intercept, digits), " + ", format_number(slope, digits), "x")
  )

  new_worked_calculation(
    title = "Simple Linear Regression Line",
    notation = "y_hat = b0 + b1x",
    formula = "Fit lm(y ~ x) and read the intercept and slope.",
    steps = steps,
    answer = paste0("Estimated regression line: y_hat = ", format_number(intercept, digits), " + ", format_number(slope, digits), "x"),
    result = list(intercept = intercept, slope = slope)
  )
}

regression_prediction_row_worked <- function(observed, x_value, b0, b1, digits = 4) {
  observed <- ensure_numeric_scalar(observed, "observed")
  x_value <- ensure_numeric_scalar(x_value, "x_value")
  b0 <- ensure_numeric_scalar(b0, "b0")
  b1 <- ensure_numeric_scalar(b1, "b1")

  predicted <- b0 + b1 * x_value
  error <- observed - predicted
  squared_error <- error^2

  steps <- c(
    paste0("Observed y = ", format_number(observed, digits)),
    paste0("Predictor x = ", format_number(x_value, digits)),
    paste0("Intercept b0 = ", format_number(b0, digits)),
    paste0("Slope b1 = ", format_number(b1, digits)),
    paste0(
      "Predicted y_hat = b0 + b1x = ",
      format_number(b0, digits),
      " + ",
      format_number(b1, digits),
      "(",
      format_number(x_value, digits),
      ") = ",
      format_number(predicted, digits)
    ),
    paste0(
      "Error = y - y_hat = ",
      format_number(observed, digits),
      " - ",
      format_number(predicted, digits),
      " = ",
      format_number(error, digits)
    ),
    paste0(
      "Squared error = (",
      format_number(error, digits),
      ")^2 = ",
      format_number(squared_error, digits)
    )
  )

  new_worked_calculation(
    title = "Regression Table Row",
    notation = "y_hat, error, error^2",
    formula = "y_hat = b0 + b1x, error = y - y_hat, squared error = error^2",
    steps = steps,
    answer = paste0(
      "Predicted = ",
      format_number(predicted, digits),
      "; error = ",
      format_number(error, digits),
      "; squared error = ",
      format_number(squared_error, digits)
    ),
    result = list(predicted = predicted, error = error, squared_error = squared_error)
  )
}

regression_prediction_table_worked <- function(observed, x, b0, b1, digits = 4) {
  observed <- ensure_numeric_vector(observed, min_length = 1)
  x <- ensure_numeric_vector(x, min_length = 1)
  ensure_same_length(observed, x, names = c("observed", "x"))
  b0 <- ensure_numeric_scalar(b0, "b0")
  b1 <- ensure_numeric_scalar(b1, "b1")

  predicted <- b0 + b1 * x
  error <- observed - predicted
  squared_error <- error^2
  sse <- sum(squared_error)

  row_steps <- vapply(
    seq_along(x),
    FUN = function(index) {
      paste0(
        "Row ",
        index,
        ": y_hat = ",
        format_number(b0, digits),
        " + ",
        format_number(b1, digits),
        "(",
        format_number(x[index], digits),
        ") = ",
        format_number(predicted[index], digits),
        "; error = ",
        format_number(observed[index], digits),
        " - ",
        format_number(predicted[index], digits),
        " = ",
        format_number(error[index], digits),
        "; squared error = (",
        format_number(error[index], digits),
        ")^2 = ",
        format_number(squared_error[index], digits)
      )
    },
    FUN.VALUE = character(1)
  )

  steps <- c(
    paste0("Observed y column = {", join_values(observed, digits), "}"),
    paste0("Predictor x column = {", join_values(x, digits), "}"),
    paste0("Use y_hat = ", format_number(b0, digits), " + ", format_number(b1, digits), "x for each row."),
    row_steps,
    paste0("Predicted y_hat column = {", join_values(predicted, digits), "}"),
    paste0("Error column = {", join_values(error, digits), "}"),
    paste0("Squared error column = {", join_values(squared_error, digits), "}"),
    paste0("SSE = ", join_sum(squared_error, digits), " = ", format_number(sse, digits))
  )

  new_worked_calculation(
    title = "Regression Prediction Table",
    notation = "y_hat, error, error^2, SSE",
    formula = "y_hat = b0 + b1x, error = y - y_hat, squared error = error^2, SSE = sum(error^2)",
    steps = steps,
    answer = paste0(
      "Completed table. Predicted column = {",
      join_values(predicted, digits),
      "}; SSE = ",
      format_number(sse, digits)
    ),
    result = list(predicted = predicted, error = error, squared_error = squared_error, sse = sse)
  )
}

regression_predict_from_coefficients_worked <- function(b0, b1, x_value, digits = 4) {
  b0 <- ensure_numeric_scalar(b0, "b0")
  b1 <- ensure_numeric_scalar(b1, "b1")
  x_value <- ensure_numeric_scalar(x_value, "x_value")

  result <- b0 + b1 * x_value

  steps <- c(
    paste0("b0 = ", format_number(b0, digits)),
    paste0("b1 = ", format_number(b1, digits)),
    paste0("x = ", format_number(x_value, digits)),
    paste0(
      "y_hat = b0 + b1x = ",
      format_number(b0, digits),
      " + ",
      format_number(b1, digits),
      "(",
      format_number(x_value, digits),
      ") = ",
      format_number(result, digits)
    )
  )

  new_worked_calculation(
    title = "Regression Prediction From Coefficients",
    notation = "y_hat",
    formula = "y_hat = b0 + b1x",
    steps = steps,
    answer = paste0("Predicted value = ", format_number(result, digits)),
    result = result
  )
}

regression_predict_from_data_worked <- function(x, y, x_new, digits = 4) {
  x <- ensure_numeric_vector(x, min_length = 2)
  y <- ensure_numeric_vector(y, min_length = 2)
  ensure_same_length(x, y, names = c("x", "y"))
  x_new <- ensure_numeric_scalar(x_new, "x_new")

  model <- lm(y ~ x)
  coefficients <- coef(model)
  intercept <- unname(coefficients[1])
  slope <- unname(coefficients[2])
  prediction <- predict(model, newdata = data.frame(x = x_new))

  steps <- c(
    paste0("Intercept b0 = ", format_number(intercept, digits)),
    paste0("Slope b1 = ", format_number(slope, digits)),
    paste0("New x = ", format_number(x_new, digits)),
    paste0("Predicted y_hat = ", format_number(prediction, digits))
  )

  new_worked_calculation(
    title = "Regression Prediction From Data",
    notation = "y_hat",
    formula = "Fit lm(y ~ x), then predict at x_new.",
    steps = steps,
    answer = paste0("Predicted value = ", format_number(prediction, digits)),
    result = as.numeric(prediction)
  )
}

regression_r_squared_worked <- function(x, y, digits = 4) {
  x <- ensure_numeric_vector(x, min_length = 2)
  y <- ensure_numeric_vector(y, min_length = 2)
  ensure_same_length(x, y, names = c("x", "y"))

  model <- lm(y ~ x)
  r_squared <- summary(model)$r.squared

  steps <- c(
    paste0("x = {", join_values(x, digits), "}"),
    paste0("y = {", join_values(y, digits), "}"),
    paste0("R^2 = ", format_number(r_squared, digits)),
    paste0("Percent explained = ", format_number(r_squared * 100, digits), "%")
  )

  new_worked_calculation(
    title = "Coefficient of Determination",
    notation = "R^2",
    formula = "R^2 = summary(lm(y ~ x))$r.squared",
    steps = steps,
    answer = paste0("R^2 = ", format_number(r_squared, digits), " = ", format_number(r_squared * 100, digits), "% explained"),
    result = r_squared
  )
}

regression_error_worked <- function(observed, predicted = NULL, b0 = NULL, b1 = NULL, x_value = NULL, digits = 4) {
  observed <- ensure_numeric_scalar(observed, "observed")

  if (is.null(predicted)) {
    b0 <- ensure_numeric_scalar(b0, "b0")
    b1 <- ensure_numeric_scalar(b1, "b1")
    x_value <- ensure_numeric_scalar(x_value, "x_value")
    predicted <- b0 + b1 * x_value
  } else {
    predicted <- ensure_numeric_scalar(predicted, "predicted")
  }

  error <- observed - predicted
  squared_error <- error^2

  steps <- c(
    paste0("Observed y = ", format_number(observed, digits)),
    paste0("Predicted y_hat = ", format_number(predicted, digits)),
    paste0("Error = observed - predicted = ", format_number(error, digits)),
    paste0("Squared error = ", format_number(squared_error, digits))
  )

  new_worked_calculation(
    title = "Regression Error",
    notation = "error, error^2",
    formula = "error = y - y_hat, squared error = error^2",
    steps = steps,
    answer = paste0("Predicted = ", format_number(predicted, digits), "; error = ", format_number(error, digits), "; squared error = ", format_number(squared_error, digits)),
    result = list(predicted = predicted, error = error, squared_error = squared_error)
  )
}

regression_sse_worked <- function(x, y, b0 = NULL, b1 = NULL, digits = 4) {
  x <- ensure_numeric_vector(x, min_length = 2)
  y <- ensure_numeric_vector(y, min_length = 2)
  ensure_same_length(x, y, names = c("x", "y"))

  if (is.null(b0) || is.null(b1)) {
    model <- lm(y ~ x)
    coefficients <- coef(model)
    b0 <- unname(coefficients[1])
    b1 <- unname(coefficients[2])
  } else {
    b0 <- ensure_numeric_scalar(b0, "b0")
    b1 <- ensure_numeric_scalar(b1, "b1")
  }

  fitted_values <- b0 + b1 * x
  residuals <- y - fitted_values
  sse <- sum(residuals^2)

  steps <- c(
    paste0("b0 = ", format_number(b0, digits), ", b1 = ", format_number(b1, digits)),
    paste0("Predicted values = {", join_values(fitted_values, digits), "}"),
    paste0("Residuals = {", join_values(residuals, digits), "}"),
    paste0("SSE = sum(residual^2) = ", format_number(sse, digits))
  )

  new_worked_calculation(
    title = "Sum of Squared Errors",
    notation = "SSE",
    formula = "SSE = sum((y - y_hat)^2)",
    steps = steps,
    answer = paste0("SSE = ", format_number(sse, digits)),
    result = sse
  )
}
