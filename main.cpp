#include "cpp/stats_bridge.hpp"

#include <iostream>
#include <stdexcept>
#include <vector>

int main(int argc, char* argv[]) {
  try {
    stats_bridge::initialize_bridge(argc > 0 ? argv[0] : nullptr);

    // Single-list problems: plug one data set in here and uncomment what you need.
    [[maybe_unused]] const stats_bridge::NumberList dataSet = {1, 5, 9, 3, 4};

    // Table-style regression problems: paste the full columns here.
    // observedY is the response / observed value column.
    // predictorX is the explanatory / x column.
    [[maybe_unused]] const stats_bridge::NumberList observedY = {
      227.6, 257.2, 269.4, 203.5, 225.4, 217.4, 182.2, 184.8
    };
    [[maybe_unused]] const stats_bridge::NumberList predictorX = {
      1428, 1494, 1694, 1214, 1399, 1291, 1193, 1006
    };
    [[maybe_unused]] const double regressionIntercept = 32.98;
    [[maybe_unused]] const double regressionSlope = 0.14;
    [[maybe_unused]] const double rowObservedY = 184.8;
    [[maybe_unused]] const double rowPredictorX = 1006;

    // Uncomment only the calculations you want during the exam.
    stats_bridge::findMean(dataSet);
    // stats_bridge::findMedian(dataSet);
    // stats_bridge::findSampleStandardDeviation(dataSet);
    // stats_bridge::findType2Quantile(0.75, {16, 18, 20, 21, 23, 23, 24, 32, 36, 42});
    // stats_bridge::findGroupedSampleStats({8, 13, 18, 23, 28}, {12, 17, 22, 27, 32}, {5, 9, 14, 8, 4});
    // stats_bridge::findBinomialProbability("exact", 10, 0.3, 4);
    // stats_bridge::findPoissonProbability("at_most", 5.0, 4);
    // stats_bridge::findNormalProbability("between", 17.64, 12.1, 12, 15);
    // stats_bridge::findMeanCIUnknownSigma(650, 60, 25, 0.99);
    // stats_bridge::runOneSampleTTest(433, 437, 22, 17, 0.05, "left");
    // stats_bridge::runWelchTTest(21.5, 20.9, 2.5, 4.1, 48, 40, 0.10, "right");
    // stats_bridge::runTwoProportionZTest(100, 500, 77, 500, 0.01, "right");
    // stats_bridge::findCorrelation({2, 3, 5, 3, 4, 6}, {125, 138, 116, 121, 136, 115});
    // stats_bridge::findRegressionPredictionFromCoefficients(12.25, 0.14, 1956);
    // stats_bridge::findRegressionPredictionRow(rowObservedY, rowPredictorX, regressionIntercept, regressionSlope);
    // stats_bridge::findRegressionPredictionTable(observedY, predictorX, regressionIntercept, regressionSlope);
    // stats_bridge::findRegressionLine(predictorX, observedY);
    // stats_bridge::findRegressionSSE(predictorX, observedY);

    return 0;
  } catch (const std::exception& error) {
    std::cerr << "Error: " << error.what() << '\n';
    return 1;
  }
}
