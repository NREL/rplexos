#include <Rcpp.h>

Rcpp::Function rplexos(const std::string& fun){
  Rcpp::Environment env = Rcpp::Environment::namespace_env("rplexos");
  return env[fun];
}

// [[Rcpp::export]]
std::string zip_buffer(const std::string& zip_path, const std::string& pattern) {
    Rcpp::Function read_file_in_zip = rplexos("read_file_in_zip");
    
    Rcpp::CharacterVector xml = Rcpp::as<Rcpp::CharacterVector>(read_file_in_zip(zip_path, pattern));
    
    // Merge all the strings into one
    long xml_len = 0;
    for (int i = 0; i < xml.size(); ++i)
        xml_len += xml[i].size();
    
    std::string result;
    result.reserve(xml_len + 1);
    int k = 0;
    
    for (int i = 0; i < xml.size(); ++i) {
        for (int j = 0; j < xml[i].size(); ++j) {
            result.replace(k, k + xml[i].size(), Rcpp::as<std::string>(xml[i]));
        }
        k += xml[i].size();
    }
    
    return result;
}
