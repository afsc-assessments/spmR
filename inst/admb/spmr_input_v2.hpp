#ifndef SPMR_INPUT_V2_HPP
#define SPMR_INPUT_V2_HPP
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

struct SpmrStockMetadata {
  std::string filename;
  int nsexes, nages, basis, abundance_unit, weight_unit, biomass_unit, male_substitute;
  std::vector<int> ages;
  std::vector<double> population_female, population_male;
};

inline void spmr_input_error(const std::string& message) {
  std::cerr << "SPMR input format 2: " << message << std::endl;
  std::exit(2);
}

inline std::vector<SpmrStockMetadata> spmr_read_metadata() {
  std::ifstream input("spm_input_v2.dat");
  if (!input) spmr_input_error("required spm_input_v2.dat is missing; regenerate inputs with the v2 R interface.");
  std::string magic;
  int version = 0, count = 0;
  if (!(input >> magic >> version >> count) || magic != "SPMR_INPUT_V2" || version != 2 || count < 1 || count > 20)
    spmr_input_error("invalid header; expected SPMR_INPUT_V2 2 <stock_count>, with 1 through 20 stocks.");
  std::vector<SpmrStockMetadata> stocks;
  for (int i = 0; i < count; ++i) {
    SpmrStockMetadata stock;
    if (!(input >> stock.filename >> stock.nsexes >> stock.nages >> stock.basis
                >> stock.abundance_unit >> stock.weight_unit >> stock.biomass_unit >> stock.male_substitute))
      spmr_input_error("truncated stock metadata at stock " + std::to_string(i + 1) + ".");
    if ((stock.nsexes != 1 && stock.nsexes != 2) || stock.nages < 2 || stock.nages > 69)
      spmr_input_error("invalid sex or age dimensions for " + stock.filename + ".");
    if (stock.basis != 1 && stock.basis != 2)
      spmr_input_error("recruitment basis must be 1 (total) or 2 (per sex) for " + stock.filename + ".");
    if (stock.nsexes == 1 && stock.basis != 1)
      spmr_input_error("pooled-sex inputs require total recruitment for " + stock.filename + ".");
    if (stock.abundance_unit < 1 || stock.abundance_unit > 3 || stock.weight_unit != 1 ||
        stock.biomass_unit != stock.abundance_unit)
      spmr_input_error("incompatible units for " + stock.filename + "; weights require kg and abundance/biomass codes must match.");
    if (stock.male_substitute < 0 || stock.male_substitute > 2)
      spmr_input_error("invalid male population-weight substitution code for " + stock.filename + ".");
    for (int a = 0; a < stock.nages; ++a) {
      double age = -1;
      if (!(input >> age) || !std::isfinite(age) || age < 0 || age != std::floor(age) ||
          (a > 0 && age != stock.ages.back() + 1.0))
        spmr_input_error("ages must be consecutive ascending nonnegative integers for " + stock.filename + ".");
      stock.ages.push_back(static_cast<int>(age));
    }
    for (int sex = 0; sex < 2; ++sex) {
      std::vector<double>& weights = sex == 0 ? stock.population_female : stock.population_male;
      for (int a = 0; a < stock.nages; ++a) {
        double weight = 0;
        if (!(input >> weight) || !std::isfinite(weight) || weight <= 0)
          spmr_input_error("population weights must contain nages finite positive values for " + stock.filename + ".");
        weights.push_back(weight);
      }
    }
    for (const auto& existing : stocks)
      if (existing.filename == stock.filename) spmr_input_error("duplicate stock filename " + stock.filename + ".");
    stocks.push_back(stock);
  }
  std::string end, extra;
  if (!(input >> end) || end != "END_SPMR_INPUT_V2") spmr_input_error("missing END_SPMR_INPUT_V2 sentinel.");
  if (input >> extra) spmr_input_error("unexpected content after END_SPMR_INPUT_V2.");
  return stocks;
}
#endif
