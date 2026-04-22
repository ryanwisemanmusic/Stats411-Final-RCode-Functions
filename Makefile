CXX ?= clang++
CXXFLAGS ?= -std=c++17 -Wall -Wextra -pedantic
TARGET := stats_helper
SOURCES := main.cpp cpp/stats_bridge.cpp
CPP_HEADERS := cpp/stats_bridge.hpp
R_SCRIPTS := \
	R/claim_analysis.R \
	R/descriptive_stats.R \
	R/distribution_stats.R \
	R/dispersion_stats.R \
	R/grouped_stats.R \
	R/inference_stats.R \
	R/position_stats.R \
	R/probability_basics.R \
	R/r_bridge_cli.R \
	R/regression_stats.R \
	R/standardization_stats.R \
	R/stat_utils.R \
	R/worked_calculation.R \
	main.R

.PHONY: all run clean rebuild

all: $(TARGET)

$(TARGET): $(SOURCES) $(CPP_HEADERS) $(R_SCRIPTS)
	$(CXX) $(CXXFLAGS) $(SOURCES) -o $(TARGET)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)

rebuild: clean all
