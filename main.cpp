#include <cstdlib>
#include <filesystem>
#include <initializer_list>
#include <iomanip>
#include <iostream>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace stats_bridge {

std::filesystem::path g_project_root;

std::string shell_escape(const std::string& value) {
  std::string escaped = "'";

  for (char character : value) {
    if (character == '\'') {
      escaped += "'\\''";
    } else {
      escaped += character;
    }
  }

  escaped += "'";
  return escaped;
}

std::string number_to_string(double value) {
  std::ostringstream stream;
  stream << std::setprecision(std::numeric_limits<double>::digits10 + 1) << value;
  return stream.str();
}

std::filesystem::path detect_project_root(const char* argv0) {
  std::vector<std::filesystem::path> candidates;
  candidates.push_back(std::filesystem::current_path());

  if (argv0 != nullptr && std::string(argv0).size() > 0) {
    candidates.push_back(std::filesystem::absolute(argv0).parent_path());
  }

  for (const auto& candidate : candidates) {
    if (std::filesystem::exists(candidate / "R" / "r_bridge_cli.R")) {
      return candidate;
    }
  }

  throw std::runtime_error(
    "Could not find R/r_bridge_cli.R. Run the program from the repo root or keep the binary there."
  );
}

void initialize_bridge(const char* argv0) {
  g_project_root = detect_project_root(argv0);
}

void run_r_calculation(const std::string& operation, const std::vector<double>& values) {
  if (g_project_root.empty()) {
    throw std::runtime_error("Bridge not initialized. Call initialize_bridge() first.");
  }

  const auto bridge_script = g_project_root / "R" / "r_bridge_cli.R";

  std::ostringstream command;
  command << "Rscript " << shell_escape(bridge_script.string()) << " " << shell_escape(operation);

  for (double value : values) {
    command << " " << shell_escape(number_to_string(value));
  }

  const int exit_code = std::system(command.str().c_str());
  if (exit_code != 0) {
    throw std::runtime_error("R calculation failed for operation: " + operation);
  }
}

void findMean(const std::vector<double>& dataSet) {
  run_r_calculation("sample_mean", dataSet);
}

void findPopulationMean(const std::vector<double>& dataSet) {
  run_r_calculation("population_mean", dataSet);
}

void findMedian(const std::vector<double>& dataSet) {
  run_r_calculation("median", dataSet);
}

void findMode(const std::vector<double>& dataSet) {
  run_r_calculation("mode", dataSet);
}

void findRange(const std::vector<double>& dataSet) {
  run_r_calculation("range", dataSet);
}

void findSampleVariance(const std::vector<double>& dataSet) {
  run_r_calculation("sample_variance", dataSet);
}

void findPopulationVariance(const std::vector<double>& dataSet) {
  run_r_calculation("population_variance", dataSet);
}

void findSampleStandardDeviation(const std::vector<double>& dataSet) {
  run_r_calculation("sample_sd", dataSet);
}

void findPopulationStandardDeviation(const std::vector<double>& dataSet) {
  run_r_calculation("population_sd", dataSet);
}

void findPopulationZScore(double x, double mu, double sigma) {
  run_r_calculation("z_score_population", {x, mu, sigma});
}

void findSampleZScore(double x, double xBar, double s) {
  run_r_calculation("z_score_sample", {x, xBar, s});
}

}  // namespace stats_bridge

int main(int argc, char* argv[]) {
  try {
    stats_bridge::initialize_bridge(argc > 0 ? argv[0] : nullptr);

    const std::vector<double> dataSet = {1, 5, 9, 3, 4};

    // Uncomment only the calculations you want during the exam.
    stats_bridge::findMean(dataSet);
    // stats_bridge::findPopulationMean(dataSet);
    // stats_bridge::findMedian(dataSet);
    // stats_bridge::findMode(dataSet);
    // stats_bridge::findRange(dataSet);
    // stats_bridge::findSampleVariance(dataSet);
    // stats_bridge::findPopulationVariance(dataSet);
    // stats_bridge::findSampleStandardDeviation(dataSet);
    // stats_bridge::findPopulationStandardDeviation(dataSet);
    // stats_bridge::findPopulationZScore(23, 18, 4);
    // stats_bridge::findSampleZScore(23, 18.3333, 4.5898);

    return 0;
  } catch (const std::exception& error) {
    std::cerr << "Error: " << error.what() << '\n';
    return 1;
  }
}
