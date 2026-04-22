#include "cpp/stats_bridge.hpp"

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#if __has_include("R/cli/cpp/problem_bridge.hpp")
#include "R/cli/cpp/problem_bridge.hpp"
#define HAS_CLI_PROBLEM_BRIDGE 1
#else
#define HAS_CLI_PROBLEM_BRIDGE 0
namespace cli_problem_bridge {
using NumberList = std::vector<double>;

struct LabeledNumberList {
  std::string label;
  NumberList values;
};

inline bool findProblemAnswer(
  const char*,
  const std::string&,
  const std::vector<LabeledNumberList>&,
  const std::string& = "",
  const std::string& = "",
  const std::string& = "heuristic"
) {
  std::cerr << "Optional prompt solver is unavailable because R/cli was not found.\n";
  return false;
}
}  // namespace cli_problem_bridge
#endif

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
    // During the exam, replace the values inside these tables instead of creating extra copies below.
    [[maybe_unused]] const OneColumnDataSet oneColumnData = {
      "One-column raw data",
      {11, 12, 9, 3, 6, 13}
    };

    [[maybe_unused]] const stats_bridge::StringList categoryLabels = {
      "Pay a Kick-Back",
      "Lucrative Golf Outings",
      "Lavish Dinners",
      "Free Private Jet Use",
      "Prime Seats at Sports Events",
      "Other",
      "No Perk Received"
    };
    [[maybe_unused]] const stats_bridge::NumberList categoryCounts = {17, 24, 20, 19, 30, 28, 12};
    [[maybe_unused]] const std::string categoryTargetLabel = "Prime Seats at Sports Events";

    // Two-column table:
    // paired-data convention = first column is before/without/population 1, second column is after/with/population 2
    // regression convention = first column is observed/response y, second column is predictor x
    [[maybe_unused]] const TwoColumnDataSet twoColumnData = {
      "Model A",
      {151, 154, 150, 149, 147, 153},
      "Model B",
      {155, 158, 152, 153, 147, 157}
    };

    [[maybe_unused]] const double empiricalRuleMean = 91;
    [[maybe_unused]] const double empiricalRuleSd = 19;
    [[maybe_unused]] const double empiricalRuleLowerBound = 72;
    [[maybe_unused]] const double empiricalRuleUpperBound = 110;
    [[maybe_unused]] const double empiricalRuleLowerTailValue = 53;

    [[maybe_unused]] const int motelSampleSize = 85;
    [[maybe_unused]] const double motelMean = 92;
    [[maybe_unused]] const double motelSd = 14;
    [[maybe_unused]] const double motelObservedPrice = 105;
    [[maybe_unused]] const double motelKnownPercentile = 0.75;
    [[maybe_unused]] const double motelUpperTailProportion = 0.25;
    [[maybe_unused]] const double motelLowerTailProportion = 0.15;
    [[maybe_unused]] const double motelKnownZScore = 0.7;

    [[maybe_unused]] const std::string twoColumnStepHint = "2";
    [[maybe_unused]] const std::string twoColumnDifferenceDefinition = "second_minus_first";
    [[maybe_unused]] const std::string twoColumnClaimText = "there is a difference in the braking distances of the two models";
    [[maybe_unused]] const double twoColumnConfidenceLevel = 0.95;
    [[maybe_unused]] const double twoColumnAlpha = 0.05;
    [[maybe_unused]] const std::string twoColumnTail = "two";
    [[maybe_unused]] const double twoColumnRegressionIntercept = 0;
    [[maybe_unused]] const double twoColumnRegressionSlope = 0;
    [[maybe_unused]] const double twoColumnObservedRowValue = 0;
    [[maybe_unused]] const double twoColumnPredictorRowValue = 0;

    [[maybe_unused]] const ThreeColumnDataSet threeColumnData = {
      "Lower class limit",
      {5.8, 6.5, 7.2, 7.9, 8.6},
      "Upper class limit",
      {6.4, 7.1, 7.8, 8.5, 9.2},
      "Frequency",
      {3, 4, 13, 12, 10}
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

    // Prompt-driven optional solver section:
    // Put the full question here. If the problem asks for one step, put that in problemRequestText.
    // If the problem includes a small numeric table, add the labeled rows to problemData.
    [[maybe_unused]] const std::string problemText =
      "A technology company changed its marketing budget split between LinkedIn and Facebook and found the bounce "
      "rate decreased after the change. Is this an observational study or a controlled experiment?";
    [[maybe_unused]] const std::string problemRequestText = "Step 1 of 3 only.";
    [[maybe_unused]] const std::string problemStepHint = "1";
    [[maybe_unused]] const std::vector<cli_problem_bridge::LabeledNumberList> problemData = {};

    // Uncomment only the calculation block you want during the exam.

    /**
     * Quiz example: the bounce-rate marketing question from Quiz_Problems.md.
     * Use this block for wording-heavy questions such as:
     * controlled experiment vs observational study, response vs explanatory variable,
     * control vs treatment group, true/false interpretation, or other prompt-only questions.
     */
    // cli_problem_bridge::findProblemAnswer(
    //   argc > 0 ? argv[0] : nullptr,
    //   problemText,
    //   problemData,
    //   problemRequestText,
    //   problemStepHint
    // );

    /**
     * Quiz example: 11, 12, 9, 3, 6, 13
     * Use this block for one-column center questions.
     * Quiz answers: mean = 9, median = 10, mode = no mode.
     */
    // stats_bridge::findMean(oneColumnData.first_values);
    // stats_bridge::findMedian(oneColumnData.first_values);
    // stats_bridge::findMode(oneColumnData.first_values);

    /**
     * Quiz example: 15, -10, 5, 12, -10, -6
     * Use this block for one-column spread questions after replacing oneColumnData.first_values.
     * Quiz answers: sample variance = 124.8, sample SD = 11.2, range = 25.
     */
    // stats_bridge::findSampleVariance(oneColumnData.first_values);
    // stats_bridge::findSampleStandardDeviation(oneColumnData.first_values);
    // stats_bridge::findRange(oneColumnData.first_values);

    /**
     * Quiz example: Wall Street perk table.
     * Use this block for named-category frequency tables.
     * Quiz answers: most frequent category = Prime Seats at Sports Events,
     * proportion for that category = 0.2, proportion for No Perk Received = 0.08.
     */
    // stats_bridge::findMostFrequentCategory(categoryLabels, categoryCounts);
    // stats_bridge::findCategoryProportionByLabel(categoryTargetLabel, categoryLabels, categoryCounts);
    // stats_bridge::findCategoryProportionByLabel("No Perk Received", categoryLabels, categoryCounts);

    /**
     * Quiz example: glucose levels with mean 91 and SD 19.
     * Use this block when the problem gives the mean and standard deviation instead of raw data.
     * Quiz answers: between 72 and 110 = 0.68, less than 53 = 0.025, greater than 110 = 0.16.
     */
    // stats_bridge::findEmpiricalRuleFromSummary(
    //   empiricalRuleMean,
    //   empiricalRuleSd,
    //   "between",
    //   empiricalRuleLowerBound,
    //   empiricalRuleUpperBound
    // );
    // stats_bridge::findEmpiricalRuleFromSummary(
    //   empiricalRuleMean,
    //   empiricalRuleSd,
    //   "less_than",
    //   empiricalRuleLowerTailValue
    // );
    // stats_bridge::findEmpiricalRuleFromSummary(
    //   empiricalRuleMean,
    //   empiricalRuleSd,
    //   "greater_than",
    //   empiricalRuleUpperBound
    // );

    /**
     * Quiz example: motel rates with n = 85, mean = 92, SD = 14.
     * Use this block for z-scores, inverse z, complements, and converting proportions into counts.
     * Quiz answers before manual rounding: z(105) = 0.928571428571429,
     * count at least $124 = 21.25, count below $60 = 12.75, x for z = 0.7 is 101.8.
     */
    // stats_bridge::findPopulationZScore(motelObservedPrice, motelMean, motelSd);
    // stats_bridge::findComplementRule(motelKnownPercentile);
    // stats_bridge::findCountFromProportion(motelSampleSize, motelUpperTailProportion);
    // stats_bridge::findCountFromProportion(motelSampleSize, motelLowerTailProportion);
    // stats_bridge::findValueFromPopulationZScore(motelKnownZScore, motelMean, motelSd);

    /**
     * Quiz examples: lunch-cost classes and rose shelf-life grouped data.
     * Use this block for grouped-class midpoints and grouped mean / variance / SD.
     * Lunch quiz answers: second midpoint = 6.8, fourth midpoint = 8.2.
     * Rose quiz answers after replacing threeColumnData with the shelf-life table:
     * grouped population mean = 12.1923076923077, grouped population SD = 4.72936189198135.
     */
    // stats_bridge::findGroupedMidpointAt(threeColumnData.first_values, threeColumnData.second_values, 2);
    // stats_bridge::findGroupedMidpointAt(threeColumnData.first_values, threeColumnData.second_values, 4);
    // stats_bridge::findGroupedMidpoints(threeColumnData.first_values, threeColumnData.second_values);
    // stats_bridge::findGroupedPopulationStats(
    //   threeColumnData.first_values,
    //   threeColumnData.second_values,
    //   threeColumnData.third_values
    // );

    /**
     * General two-column block for paired-data, confidence interval, hypothesis test,
     * correlation, and regression problems elsewhere in the repo.
     * Keep the table and settings directly above in twoColumnData and the two-column settings block.
     */
    // stats_bridge::findProblemHypotheses(problemText);
    // stats_bridge::findClaimTail(twoColumnClaimText, twoColumnDifferenceDefinition);
    // stats_bridge::findPairedDifferenceMean(twoColumnData.first_values, twoColumnData.second_values);
    // stats_bridge::findPairedTCI(twoColumnData.first_values, twoColumnData.second_values, twoColumnConfidenceLevel);
    // stats_bridge::runPairedTTest(twoColumnData.first_values, twoColumnData.second_values, twoColumnAlpha, twoColumnTail);
    // stats_bridge::runPairedTTestFromClaim(
    //   twoColumnData.first_values,
    //   twoColumnData.second_values,
    //   twoColumnAlpha,
    //   twoColumnClaimText,
    //   twoColumnDifferenceDefinition
    // );
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

    /**
     * General probability / inference block kept here so the repo still exposes the broader toolkit.
     * These are not the quiz examples above, but they stay nearby for the final.
     */
    // stats_bridge::findType2Quantile(0.75, {16, 18, 20, 21, 23, 23, 24, 32, 36, 42});
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

    return 0;
  } catch (const std::exception& error) {
    std::cerr << "Error: " << error.what() << '\n';
    return 1;
  }
}
