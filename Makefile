CXX ?= clang++
CXXFLAGS ?= -std=c++17 -Wall -Wextra -pedantic
TARGET := stats_helper
SOURCES := main.cpp
R_SCRIPTS := \
	R/descriptive_stats.R \
	R/dispersion_stats.R \
	R/r_bridge_cli.R \
	R/standardization_stats.R \
	R/stat_utils.R \
	R/worked_calculation.R \
	main.R

.PHONY: all run clean rebuild

all: $(TARGET)

$(TARGET): $(SOURCES) $(R_SCRIPTS)
	$(CXX) $(CXXFLAGS) $(SOURCES) -o $(TARGET)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)

rebuild: clean all
