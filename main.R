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

project_root <- if (exists("CODEX_PROJECT_ROOT", inherits = TRUE)) {
  get("CODEX_PROJECT_ROOT", inherits = TRUE)
} else {
  get_project_root()
}

load_stat_modules <- function(project_root_path) {
  source(file.path(project_root_path, "R", "stat_utils.R"))
  source(file.path(project_root_path, "R", "worked_calculation.R"))
  source(file.path(project_root_path, "R", "claim_analysis.R"))
  source(file.path(project_root_path, "R", "descriptive_stats.R"))
  source(file.path(project_root_path, "R", "dispersion_stats.R"))
  source(file.path(project_root_path, "R", "standardization_stats.R"))
  source(file.path(project_root_path, "R", "position_stats.R"))
  source(file.path(project_root_path, "R", "grouped_stats.R"))
  source(file.path(project_root_path, "R", "probability_basics.R"))
  source(file.path(project_root_path, "R", "distribution_stats.R"))
  source(file.path(project_root_path, "R", "inference_stats.R"))
  source(file.path(project_root_path, "R", "regression_stats.R"))
}

load_stat_modules(project_root)

main <- function() {
  data <- c(12, 15, 18, 20, 20, 25)
  example_x <- 23
  example_mu <- 18
  example_sigma <- 4

  # Uncomment the calculations you want to run during the exam.
  print_calculation(sample_mean(data))
  # print_calculation(population_mean(data))
  # print_calculation(median_worked(data))
  # print_calculation(mode_worked(data))
  # print_calculation(range_worked(data))
  # print_calculation(sample_variance(data))
  # print_calculation(population_variance(data))
  # print_calculation(sample_sd(data))
  # print_calculation(population_sd(data))
  # print_calculation(z_score_worked(example_x, example_mu, example_sigma))
  # print_calculation(z_score_worked(example_x, mean(data), sd(data), mean_symbol = "x_bar", sd_symbol = "s"))
  # print_calculation(binomial_probability_worked(10, 0.3, "exact", 4))
  # print_calculation(mean_ci_unknown_sigma_worked(650, 60, 25, 0.99))
  # print_calculation(one_sample_t_test_worked(433, 437, 22, 17, 0.05, "left"))
  # print_calculation(correlation_worked(c(2, 3, 5, 3, 4, 6), c(125, 138, 116, 121, 136, 115)))

  invisible(0)
}

if (sys.nframe() == 0) {
  main()
}
