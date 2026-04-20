#include "stats_bridge.hpp"

#include <cstdlib>
#include <filesystem>
#include <sstream>

namespace stats_bridge {

namespace {

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

}  // namespace

void initialize_bridge(const char* argv0) {
  g_project_root = detect_project_root(argv0);
}

void run_operation(const std::string& operation, const StringList& args) {
  if (g_project_root.empty()) {
    throw std::runtime_error("Bridge not initialized. Call initialize_bridge() first.");
  }

  const auto bridge_script = g_project_root / "R" / "r_bridge_cli.R";

  std::ostringstream command;
  command << "Rscript " << shell_escape(bridge_script.string()) << " " << shell_escape(operation);

  for (const auto& arg : args) {
    command << " " << shell_escape(arg);
  }

  const int exit_code = std::system(command.str().c_str());
  if (exit_code != 0) {
    throw std::runtime_error("R calculation failed for operation: " + operation);
  }
}

}  // namespace stats_bridge
