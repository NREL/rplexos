
#include <vector>
#include <string>
#include <Rcpp.h>
#include "rapidxml.h"

#define ALT_INDEX "index2"

using namespace std;
using namespace rapidxml;

// Find a string in a vector
int find_position(vector<string> x, string y, int guess = 0) {
    int vec_size = x.size() - 1;
    int guess2 = max(0, min(vec_size, guess));

    for (int i = guess2; i <= vec_size; ++i) {
        if (x.at(i) == y) {
            return(i);
        }
    }

    for (int i = 0; i < guess2; ++i) {
        if (x.at(i) == y) {
            return(i);
        }
    }

    return(-1);
}

// Is column name an id?
bool column_is_id(std::string col_name) {
    int size = col_name.length();
    bool out = false;
    if (size>=3) {
      out = (col_name.substr(size - 3, size - 1) == "_id");
    }

    return(out);
}

// [[Rcpp::export]]
Rcpp::List process_xml(Rcpp::CharacterVector xml) {
    // Start XML variables
    xml_document<> doc;
    xml_node<> *root_node;

    // Merge all the strings into one
    long xml_len = 0;
    for (int i = 0; i < xml.size(); ++i)
        xml_len += xml[i].size();

    vector<char> contents(xml_len + 1);
    vector<char>::iterator it = contents.begin();
    for (int i = 0; i < xml.size(); ++i)
        it = copy(xml[i].begin(), xml[i].end(), it);

    // Parse XML file and find root node
    doc.parse<0> (&contents[0]);
    root_node = doc.first_node();

    // Variables for the loop
    vector<string> table_names;
    vector<int> table_size;
    vector< vector<string> > table_heads;
    string prev_table = "", curr_table;
    int prev_pos = -1, pos = -1;

    // Get list of tables and headers
    for (xml_node<> *param_node = root_node->first_node(); param_node; param_node = param_node->next_sibling()) {
        vector<string> names(0);
        curr_table = string(param_node->name());

        // Check if this is a new or existing table
        if (curr_table == prev_table) {
            pos = prev_pos;
        } else {
            pos = find_position(table_names, curr_table);
        }

        if (pos < 0) {
            // Add headers for new table
            for (xml_node<> *attr_node = param_node->first_node(); attr_node; attr_node = attr_node->next_sibling()) {
                names.push_back(attr_node->name());
            }

            table_names.push_back(curr_table);
            table_size.push_back(1);
            table_heads.push_back(names);
            pos = table_names.size() - 1;
        } else {
            // Table exists, check that headers exist
            for (xml_node<> *attr_node = param_node->first_node(); attr_node; attr_node = attr_node->next_sibling()) {
                int k = 0;
                string nm = attr_node->name();
                vector<string> *the_names = &(table_heads[pos]);

                if (find_position(*the_names, nm, k) < 0) {
                    the_names->push_back(nm);
                }
                ++k;
            }

            ++table_size[pos];
        }

        // Prepare for next iteration
        prev_table = curr_table;
        prev_pos = pos;
    }

    // Create output object
    Rcpp::List out(table_names.size());
    out.attr("names") = table_names;

    // Create empty tables to return the results
    for (unsigned int i = 0; i < table_names.size(); ++i) {
        // Add new table
        Rcpp::List new_table(table_heads.at(i).size());

        // Add columns
        for (unsigned int j = 0; j < table_heads.at(i).size(); ++j) {
            if (column_is_id(table_heads.at(i).at(j))) {
                new_table[j] = Rcpp::IntegerVector(table_size[i]);;
            } else {
                new_table[j] = Rcpp::CharacterVector(table_size[i]);;
            }
        }

        // Convert new table to data.frame
        Rcpp::IntegerVector rowNames(2);
        rowNames[0] = NA_INTEGER;
        rowNames[1] = -table_size.at(i);
        new_table.attr("row.names") = rowNames;
        new_table.attr("names") = table_heads.at(i);
        new_table.attr("class") = "data.frame";

        // Add to output
        out[i] = new_table;
    }

    // Prepare variable for loop that reads the actual data
    Rcpp::IntegerVector table_pos(table_names.size(), 0);
    table_pos.attr("names") = table_names;

    // Read data from XML and copy it to the output object
    for (xml_node<> *param_node = root_node->first_node(); param_node; param_node = param_node->next_sibling()) {
        curr_table = string(param_node->name());

        // Find position for this table
        Rcpp::List this_table = out[curr_table];
        int this_pos = table_pos[curr_table];

        // Loop through the attributes
        for (xml_node<> *attr_node = param_node->first_node(); attr_node; attr_node = attr_node->next_sibling()) {
            if (column_is_id(attr_node->name())) {
                Rcpp::IntegerVector this_column = this_table[attr_node->name()];
                this_column[this_pos] = atoi(attr_node->value());
            } else {
                Rcpp::CharacterVector this_column = this_table[attr_node->name()];
                this_column[this_pos] = attr_node->value();
            }
        }

        // Prepare for next iteration
        table_pos[curr_table] = this_pos + 1;
    }

    // Avoid table header 'index' (it creates problems with SQLite)
    for (unsigned int i = 0; i < table_names.size(); ++i) {
        // Add new table
        Rcpp::List this_table = out[i];

        // Add columns
        for (unsigned int j = 0; j < table_heads.at(i).size(); ++j) {
            if (table_heads.at(i).at(j) == "index") {
                Rcpp::CharacterVector this_names = this_table.attr("names");
                this_names[j] = ALT_INDEX;
            }
        }
    }

    // Return output
    return(out);
}
