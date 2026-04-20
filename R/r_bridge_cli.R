get_project_root <- function() {
  command_line_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_line_args, value = TRUE)

  if (length(file_arg) > 0) {
    file_path <- sub("^--file=", "", file_arg[1])
    file_path <- gsub("~\\+~", " ", file_path)
    return(dirname(normalizePath(file_path)))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }

  normalizePath(getwd())
}

script_dir <- get_project_root()
project_root <- dirname(script_dir)

source(file.path(project_root, "R", "stat_utils.R"))
source(file.path(project_root, "R", "worked_calculation.R"))
source(file.path(project_root, "R", "descriptive_stats.R"))
source(file.path(project_root, "R", "dispersion_stats.R"))
source(file.path(project_root, "R", "standardization_stats.R"))

parse_numeric_args <- function(args) {
  numeric_args <- suppressWarnings(as.numeric(args))

  if (any(is.na(numeric_args))) {
    stop("All calculation inputs must be numeric.", call. = FALSE)
  }

  numeric_args
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript R/r_bridge_cli.R <operation> <number> [more numbers...]", call. = FALSE)
}

operation <- args[1]
numeric_args <- parse_numeric_args(args[-1])

calculation <- switch(
  operation,
  sample_mean = sample_mean(numeric_args),
  population_mean = population_mean(numeric_args),
  median = median_worked(numeric_args),
  mode = mode_worked(numeric_args),
  range = range_worked(numeric_args),
  sample_variance = sample_variance(numeric_args),
  population_variance = population_variance(numeric_args),
  sample_sd = sample_sd(numeric_args),
  population_sd = population_sd(numeric_args),
  z_score_population = {
    if (length(numeric_args) != 3) {
      stop("z_score_population requires x, mu, and sigma.", call. = FALSE)
    }

    z_score_worked(
      x_value = numeric_args[1],
      center_value = numeric_args[2],
      spread_value = numeric_args[3],
      mean_symbol = "mu",
      sd_symbol = "sigma"
    )
  },
  z_score_sample = {
    if (length(numeric_args) != 3) {
      stop("z_score_sample requires x, x_bar, and s.", call. = FALSE)
    }

    z_score_worked(
      x_value = numeric_args[1],
      center_value = numeric_args[2],
      spread_value = numeric_args[3],
      mean_symbol = "x_bar",
      sd_symbol = "s"
    )
  },
  stop(paste("Unsupported operation:", operation), call. = FALSE)
)

print_calculation(calculation)
