# rplexos 0.10

## Enhancements

* Data queries now mimic the nomenclature used in the PLEXOS GUI. In particular
  collections and property names are the same as the GUI and have changed from
  previous versions of rplexos. Existing code might need to be tweaked.
* The version of rplexos used to process a solution is stored in the database.
  When a database is open, that version is checked against the installed
  version of the package. Warnings are produced warnings if the differ.
* The log file is now processed and some statistics (solve time,
  infeasibilities, relative gap) are saved and can be queried (use the new
  functions `query_log` and `query_log_steps`).
* Better support of stochastic results. Sample results and statistics will be
  returned in the queries. Use `is_sample_stats` to separate them.
* Compatible with dplyr 0.3 and RSQLite 1.0. Both are required.


# rplexos 0.9.1

## Enhancements

* Check for existing folders in process_folder and plexos_open.
* Add function query_config to get information from configuration tables.
* Add rplexos version to config table during processing of database.
* Add class_group and class to property and query_property results.
* Add a warning if a summary table does not contain data.

## Bugs

* Fix detection of empty folders in plexos_open.
* Fix "rbind_all" warning in plexos_open.
* Fix error in "time_from" and "time_to" columns in the interval data.
* Fix error parsing XML files with entries with varying number of elements.
* plexos_open now ignores temporary SQLite databases.


# rplexos 0.9

## Enhancements

* Removed SQLite code from the package. Now using RSQLite for all database
  operations.
* `process_folder` and `plexos_open` can now accept the wildcard `"*"`
  as an input. This will consider all the folders in the working
  directory by calling the `list_folder` internally.
* Queries now support multiple properties within a collection. If the wildcard
  `"*"` is provided, all the properties for the given collection are queried.
* Automatically detect if the query contains multiple bands, timeslices or
  samples. If so, they are reported in the output.

## Bugs

* Avoid a warning in `plexos_open` if a vector of folders is provided.
* Fix numbering of databases, which caused problem in `print` and 
  `summary` of PLEXOS objects.
* Avoid a warning when querying multiple databases if the columns returned
  were factors with different levels.
* Fix error in the algorithm that deals with time ties (multiple scenarios
  could be compared at once).


# rplexos 0.8

Initial submission to CRAN.
