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

project_root <- get_project_root()

source(file.path(project_root, "R", "stat_utils.R"))
source(file.path(project_root, "R", "worked_calculation.R"))
source(file.path(project_root, "R", "descriptive_stats.R"))
source(file.path(project_root, "R", "dispersion_stats.R"))
source(file.path(project_root, "R", "standardization_stats.R"))

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

  invisible(0)
}

if (sys.nframe() == 0) {
  main()
}
