# STAT 411 Final Helper Scaffold

This repo is set up so you can work from `main.cpp` while letting R handle the actual statistics calculations and worked-out steps.

## Files

- `main.cpp`: the exam-facing file where you call helpers like `findMean({1, 5, 9, 3, 4});`.
- `main.R`: optional pure-R entrypoint if you want to run the helpers directly in R.
- `R/r_bridge_cli.R`: command-line bridge that receives values from C++ and calls the R helpers.
- `R/descriptive_stats.R`: mean, median, mode, and range helpers.
- `R/dispersion_stats.R`: sample/population variance and standard deviation helpers.
- `R/standardization_stats.R`: starter helper for z-scores.
- `R/worked_calculation.R`: the shared object/print format for showing formulas and work.
- `R/stat_utils.R`: formatting and validation helpers.

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

1. Put your numbers into the `dataSet` vector in `main.cpp`.
2. Uncomment the function calls you need in `main.cpp`.
3. Build and run `./stats_helper`.
4. Read the printed formula, substitutions, and final answer.

Example:

```cpp
const std::vector<double> dataSet = {1, 5, 9, 3, 4};

findMean(dataSet);
findMedian(dataSet);
findSampleStandardDeviation(dataSet);
findPopulationZScore(23, 18, 4);
```

## Current C++ Helpers

- `findMean(dataSet)`
- `findPopulationMean(dataSet)`
- `findMedian(dataSet)`
- `findMode(dataSet)`
- `findRange(dataSet)`
- `findSampleVariance(dataSet)`
- `findPopulationVariance(dataSet)`
- `findSampleStandardDeviation(dataSet)`
- `findPopulationStandardDeviation(dataSet)`
- `findPopulationZScore(x, mu, sigma)`
- `findSampleZScore(x, xBar, s)`

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
