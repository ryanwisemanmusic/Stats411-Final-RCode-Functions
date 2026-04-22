# STAT 411 Final Helper Scaffold

This repo is set up so you can work from `main.cpp` while letting R handle the actual statistics calculations and worked-out steps.
Outputs are intentionally printed at full display precision so you can decide the final rounding yourself based on the problem statement.
The large `STAT_Scratchpaper.R` notebook is preserved in the repo, and its reusable calculation patterns have been extracted into modular files under `R/`.

## Files

- `main.cpp`: the exam-facing file where you call helpers like `findMean({1, 5, 9, 3, 4});`, with labeled 1-column, 2-column, 3-column, and 4-column data containers for exam problems.
- `cpp/stats_bridge.hpp` and `cpp/stats_bridge.cpp`: the C++ to R bridge layer used by `main.cpp`.
- `main.R`: optional pure-R entrypoint if you want to run the helpers directly in R.
- `R/r_bridge_cli.R`: command-line bridge that receives values from C++ and calls the R helpers.
- `R/descriptive_stats.R`, `R/dispersion_stats.R`, `R/standardization_stats.R`: core descriptive helpers.
- `R/position_stats.R`: quartiles, percentile rank, IQR, outliers, weighted mean, sample proportion, percent change, Chebyshev, and empirical rule helpers.
- `R/grouped_stats.R`: grouped-data mean, variance, SD, and midpoint replication helpers.
- `R/probability_basics.R`: complement, addition, Bayes, counting rules, discrete table helpers, and empirical distributions.
- `R/distribution_stats.R`: geometric, binomial, Poisson, hypergeometric, normal, uniform, exponential, t, chi-square, F, and sampling-distribution helpers.
- `R/inference_stats.R`: confidence intervals, hypothesis tests, sample-size formulas, paired tests, two-sample tests, two-proportion tests, and variance tests.
- `R/regression_stats.R`: correlation, regression line, prediction, row-by-row regression table filling, residual/error, `R^2`, and SSE helpers.
- `R/worked_calculation.R`: the shared object/print format for showing formulas and work.
- `R/stat_utils.R`: formatting and validation helpers.
- `STAT_Scratchpaper.R`: the original semester scratch notebook kept for reference.

## How to Build and Run

Build from the repo root:

```sh
make
```

Run it:

```sh
make run
```

Clean the compiled binary:

```sh
make clean
```

The program shells out to `Rscript`, so R still does the heavy lifting and prints the formulas, substitutions, and final answer.

## Pure R Option

If you want to bypass C++ and use the R helpers directly:

```sh
Rscript main.R
```

Or inside an R console:

```r
source("main.R")
main()
```

## C++ Exam Workflow

1. Keep one active table per shape in `main.cpp`: `oneColumnData`, `twoColumnData`, `threeColumnData`, and `fourColumnData`.
2. Replace the values inside the matching table instead of creating extra tables.
3. For paired data, use the first two-column vector as "before/without" and the second as "after/with" unless the problem explicitly defines a different order.
4. Keep any two-column settings, like confidence level, alpha, tail, or regression coefficients, directly under `twoColumnData`.
5. Uncomment the function calls you need in `main.cpp`.
6. Build and run `./stats_helper`.
7. Read the printed formula, substitutions, and final answer, then round only as the problem instructs.

Example:

```cpp
stats_bridge::findMean(oneColumnData.first_values);
stats_bridge::findPairedDifferenceMean(twoColumnData.first_values, twoColumnData.second_values);
stats_bridge::findPairedTCI(twoColumnData.first_values, twoColumnData.second_values, twoColumnConfidenceLevel);
stats_bridge::runPairedTTest(twoColumnData.first_values, twoColumnData.second_values, twoColumnAlpha, twoColumnTail);
stats_bridge::findGroupedSampleStats(
  threeColumnData.first_values,
  threeColumnData.second_values,
  threeColumnData.third_values
);
stats_bridge::findRegressionPredictionTable(
  twoColumnData.first_values,
  twoColumnData.second_values,
  twoColumnRegressionIntercept,
  twoColumnRegressionSlope
);
```

## Current C++ Helper Families

- descriptive statistics and relative position
- grouped-data statistics
- probability rules and counting
- discrete and continuous distributions
- confidence intervals and sample-size formulas
- one-sample, two-sample, paired, and proportion tests
- variance / chi-square procedures
- correlation and simple linear regression

Open `main.cpp` to see representative call names and argument order.

## Pure R Helpers

If you want to call R directly instead, these still exist:

- `sample_mean(x)`
- `population_mean(x)`
- `median_worked(x)`
- `mode_worked(x)`
- `range_worked(x)`
- `sample_variance(x)`
- `population_variance(x)`
- `sample_sd(x)`
- `population_sd(x)`
- `z_score_worked(x_value, center_value, spread_value, mean_symbol = "mu", sd_symbol = "sigma")`
- plus the additional helpers in `position_stats.R`, `grouped_stats.R`, `probability_basics.R`, `distribution_stats.R`, `inference_stats.R`, and `regression_stats.R`

## Likely Next Helpers

Public course descriptions suggest `STAT 411` is `Statistical Methods I` and covers distributions, estimation, and hypothesis testing. Hawkes' public statistics materials also emphasize central tendency, variability, normal/binomial work, confidence intervals, and hypothesis tests.

Good next additions would be:

- binomial probability helpers
- normal distribution probability helpers
- confidence interval helpers
- one-sample hypothesis test helpers

Public references:

- [UNLV summer schedule: STAT 411 = Statistical Methods I](https://summerterm.unlv.edu/eschedule.php?sub=STAT&submit=qsub)
- [UNLV quantitative psychology recommendations mentioning STAT 411 topics](https://www.unlv.edu/sites/default/files/media/document/2022-04/Psychology-QuantitativeRecommendations.pdf)
- [Hawkes Essential Statistics table of contents](https://www.hawkeslearning.com/products/essential-statistics)
- [Hawkes R instructions](https://www.hawkeslearning.com/Statistics/r.html)
