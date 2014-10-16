
#include <Rcpp.h>

using namespace std;

// [[Rcpp::export]]
Rcpp::DataFrame expand_tkey(Rcpp::DataFrame tkey) {
    // Fast access to columns in input data
    Rcpp::IntegerVector key    = tkey["key_id"],
                        phase  = tkey["phase_id"],
                        offset = tkey["period_offset"],
                        length = tkey["length"];
    
    // Calculate output size
    int outputSize = sum(length);
    
    // Create output containers
    Rcpp::IntegerVector outKey(outputSize), outPhase(outputSize), outPeriod(outputSize);
    
    // Fill output
    int currLine = 0;
    for (int i = 0; i < key.size(); ++i) {
        for (int j = 1 + offset[i]; j <= length[i] + offset[i]; ++j) {
            outKey[currLine]    = key[i];
            outPhase[currLine]  = phase[i];
            outPeriod[currLine] = j;
            ++currLine;
        }
    }
    
    // Retun results
    return Rcpp::DataFrame::create(Rcpp::_["key"]       = outKey,
                                   Rcpp::_["phase_id"]  = outPhase,
                                   Rcpp::_["period_id"] = outPeriod);
}
