#include <Rcpp.h>

Rcpp::Function rplexos(const std::string& fun) {
  Rcpp::Environment env = Rcpp::Environment::namespace_env("rplexos");
  return env[fun];
}

std::string join_strings(Rcpp::CharacterVector xml) {
    // Merge all the strings into one
    long xml_len = 0;
    for (int i = 0; i < xml.size(); ++i)
        xml_len += xml[i].size();
    
    // Merge results into one vector of characters
    std::vector<char> contents(xml_len + 1);
    std::vector<char>::iterator it = contents.begin();
    for (int i = 0; i < xml.size(); ++i)
        it = copy(xml[i].begin(), xml[i].end(), it);
    
    // Convert to string to retung
    std::string result(contents.begin(), contents.end());
    
    return(result);
}

// [[Rcpp::export]]
std::string zip_buffer(const std::string& zip_path, const std::string& pattern) {
    Rcpp::Function read_file_in_zip = rplexos("read_file_in_zip");
    Rcpp::CharacterVector xml = Rcpp::as<Rcpp::CharacterVector>(read_file_in_zip(zip_path, pattern));
    return(join_strings(xml));
}
