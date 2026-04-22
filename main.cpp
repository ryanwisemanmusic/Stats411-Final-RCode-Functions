#include "cpp/stats_bridge.hpp"

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

struct OneColumnDataSet {
  std::string first_label;
  stats_bridge::NumberList first_values;
};

struct TwoColumnDataSet {
  std::string first_label;
  stats_bridge::NumberList first_values;
  std::string second_label;
  stats_bridge::NumberList second_values;
};

struct ThreeColumnDataSet {
  std::string first_label;
  stats_bridge::NumberList first_values;
  std::string second_label;
  stats_bridge::NumberList second_values;
  std::string third_label;
  stats_bridge::NumberList third_values;
};

struct FourColumnDataSet {
  std::string first_label;
  stats_bridge::NumberList first_values;
  std::string second_label;
  stats_bridge::NumberList second_values;
  std::string third_label;
  stats_bridge::NumberList third_values;
  std::string fourth_label;
  stats_bridge::NumberList fourth_values;
};

int main(int argc, char* argv[]) {
  try {
    stats_bridge::initialize_bridge(argc > 0 ? argv[0] : nullptr);

    // Keep exactly one active table for each shape.
    // During the exam, replace the values inside these tables instead of adding more tables below.
    [[maybe_unused]] const OneColumnDataSet oneColumnData = {
      "Single data set",
      {1, 5, 9, 3, 4}
    };

    // Two-column table:
    // paired data convention = first column is "before/without", second column is "after/with"
    // regression convention = first column is observed/response y, second column is predictor x
    [[maybe_unused]] const TwoColumnDataSet twoColumnData = {
      "Prices in 2018",
      {11.44, 18.44, 16.61, 13.77, 17.59, 19.74, 16.86, 17.56, 19.04},
      "Prices in 2019",
      {10.65, 16.86, 15.47, 12.43, 16.25, 19.06, 18.63, 16.07, 17.22}
    };
    // Keep two-column settings directly under the two-column table they belong to.
    [[maybe_unused]] const std::string twoColumnProblemText = R"(An economist studying inflation in electricity prices in 2018 and 2019 believes that the average price of electricity, even after adjusting for inflation, changed between these two years. To test his claim, he samples 9 different counties and records the average price of electricity in each county from each year. He then adjusts the prices for inflation. His results are given in the following table. Test the economist’s claim at the 0.05 level of significance assuming that the population distribution of the paired differences is approximately normal. Let d= (prices in 2019)-(prices in 2018).)";
    [[maybe_unused]] const std::string twoColumnDifferenceDefinition = "second_minus_first";
    [[maybe_unused]] const std::string twoColumnClaimText = "the average price of electricity, even after adjusting for inflation, changed between these two years";
    [[maybe_unused]] const double twoColumnConfidenceLevel = 0.95;
    [[maybe_unused]] const double twoColumnAlpha = 0.05;
    [[maybe_unused]] const std::string twoColumnTail = "two";
    [[maybe_unused]] const double twoColumnRegressionIntercept = 0;
    [[maybe_unused]] const double twoColumnRegressionSlope = 0;
    [[maybe_unused]] const double twoColumnObservedRowValue = 0;
    [[maybe_unused]] const double twoColumnPredictorRowValue = 0;

    [[maybe_unused]] const ThreeColumnDataSet threeColumnData = {
      "Lower class boundary",
      {8, 13, 18, 23, 28},
      "Upper class boundary",
      {12, 17, 22, 27, 32},
      "Frequency",
      {5, 9, 14, 8, 4}
    };

    [[maybe_unused]] const FourColumnDataSet fourColumnData = {
      "Column 1",
      {},
      "Column 2",
      {},
      "Column 3",
      {},
      "Column 4",
      {}
    };

    // Uncomment only the calculations you want during the exam.
    // stats_bridge::findMean(oneColumnData.first_values);
    // stats_bridge::findMedian(oneColumnData.first_values);
    // stats_bridge::findSampleStandardDeviation(oneColumnData.first_values);
    // stats_bridge::findType2Quantile(0.75, {16, 18, 20, 21, 23, 23, 24, 32, 36, 42});
    // stats_bridge::findProblemHypotheses(twoColumnProblemText);
    // stats_bridge::findClaimTail(twoColumnClaimText, twoColumnDifferenceDefinition);
    // stats_bridge::findPairedDifferenceMean(twoColumnData.first_values, twoColumnData.second_values);
    // stats_bridge::findPairedTCI(twoColumnData.first_values, twoColumnData.second_values, twoColumnConfidenceLevel);
    // stats_bridge::runPairedTTest(twoColumnData.first_values, twoColumnData.second_values, twoColumnAlpha, twoColumnTail);
    stats_bridge::solvePairedProblemFromText(
      twoColumnData.first_values,
      twoColumnData.second_values,
      twoColumnProblemText
    );
    // stats_bridge::runPairedTTestFromClaim(
    //   twoColumnData.first_values,
    //   twoColumnData.second_values,
    //   twoColumnAlpha,
    //   twoColumnClaimText,
    //   twoColumnDifferenceDefinition
    // );
    // stats_bridge::findGroupedSampleStats(
    //   threeColumnData.first_values,
    //   threeColumnData.second_values,
    //   threeColumnData.third_values
    // );
    // stats_bridge::findBinomialProbability("exact", 10, 0.3, 4);
    // stats_bridge::findPoissonProbability("at_most", 5.0, 4);
    // stats_bridge::findNormalProbability("between", 17.64, 12.1, 12, 15);
    // stats_bridge::findMeanCIUnknownSigma(650, 60, 25, 0.99);
    // stats_bridge::runOneSampleTTest(433, 437, 22, 17, 0.05, "left");
    // stats_bridge::runWelchTTest(21.5, 20.9, 2.5, 4.1, 48, 40, 0.10, "right");
    // stats_bridge::runTwoProportionZTest(100, 500, 77, 500, 0.01, "right");
    // stats_bridge::findCorrelation(twoColumnData.first_values, twoColumnData.second_values);
    // stats_bridge::findRegressionPredictionFromCoefficients(
    //   twoColumnRegressionIntercept,
    //   twoColumnRegressionSlope,
    //   twoColumnPredictorRowValue
    // );
    // stats_bridge::findRegressionPredictionRow(
    //   twoColumnObservedRowValue,
    //   twoColumnPredictorRowValue,
    //   twoColumnRegressionIntercept,
    //   twoColumnRegressionSlope
    // );
    // stats_bridge::findRegressionPredictionTable(
    //   twoColumnData.first_values,
    //   twoColumnData.second_values,
    //   twoColumnRegressionIntercept,
    //   twoColumnRegressionSlope
    // );
    // stats_bridge::findRegressionLine(twoColumnData.second_values, twoColumnData.first_values);
    // stats_bridge::findRegressionSSE(twoColumnData.second_values, twoColumnData.first_values);

    return 0;
  } catch (const std::exception& error) {
    std::cerr << "Error: " << error.what() << '\n';
    return 1;
  }
}
