# rplexos 1.1.12

* Speed improvements for the otf Plexos functions

# rplexos 1.1.11

* Implemented a new experimental function to process a solution on the fly. This means that data is only added
when the data is queried. Consequent queries should therefore go quicker since the data is already in the sql db.
This feature is useful when only a handful report items are required in a Plexos file with many more report items.

# rplexos 1.1.10

* Also allow for negative filtering in the queries (e.g. filter = list(Name = c('-Wind')) to select all plants except Wind).
* Fix dependencies: RSQLite (#55) and lubridate (#53).
* Fix for missing regions in batteries (#54).
* Added a more specific regex to look for the xml inside the solution zip (solves #52).
* Log files are now processed successfully (#47).
* More tests on the package.
* Improved documentation (especially for the time.range part).

# rplexos 1.1.8

* Solves a few bugs to prepare the package for CRAN
 * The package does not work anymore on input files. All the examples have been commented. A future release will hopefully fix this issue.

# rplexos 1.1.5 and 1.1.6

* Bugfix in the C code to cope columns (properties) that have 3 or less characters
* Change rbind() to bind_rows() because the former will be depreciated in a future dplyr release
* Improve the collect call so that it includes the n=Inf argument. The default value changed in dplyr.

# rplexos 1.1.2 and 1.1.3

* Minor revision to the description to update package maintainer.


# rplexos 1.1.1

* Minor revision to the description, as requested by CRAN maintainers.


# rplexos 1.1

## Enhancements

* Reduce the dependency on a few other packages.
* `time.range` can be defined as a R date or datetime object.
* Use the built-in data in the documentation examples. (#35)
* Add filter to `query_generator` so that it only returns the list of generators.
* Delete unused columns from `query_zone` and `query_region`.


## Bug fixes

* Correctly handle interval queries for databases that don't have the requested data.
* Fix an error in `process_folder` when dplyr 0.4.2 is installed (#37).


## Database structure

Structure has not changed since v1.0.


# rplexos 1.0

## Enhancements

* Add example of input and output database. Updated examples and vignette with these
  example files (#1).
* Add tests to file processing and queries (#1).
* `process_folder` does not print to screen by default.


## Bug fixes

* Fixed erroneous results when the second entry in `time.range` is given as date.
* Fixed `sum_master` and associated functions (#31).
* Fixed error in interval queries for databases with multiple phases (#29).


## Database structure

Structure has changed since v0.13. Do not run queries on solutions processed with an
older version.


# rplexos 0.13

## Enhancements

* Add capabilities to query (#10) and process files in parallel (#23). This mode can
  be initiated and stopped with `start_parallel_rplexos()` and `stop_parallel_rplexos()`,
  respectively
* Add shortcut functions to enable/disable debug mode: `start_debug_rplexos()` and
  `stop_debug_rplexos()`
* `plexos_open` no longer opens the SQLite connections, so `plexos_close` is not needed
  anymore
* Expose function `query_sql`, which can be used to execute a SQL query on each PLEXOS
  database (#20)
* Add fast queries for phases, timeslices, samples, objects, classes, time, etc. in the
  solutions. See `help("query_time")` and related functions. (#20)
* `process_folder` now processes all input and solution files that it can find


## Bug fixes

* Non-time filters are now correclty applied if `time.range` is not defined (#15)
* Ability to read large (>3 GB) XML files
* Fix detection of invalid properties in queries


# rplexos 0.12.1

## Bug fixes

* `plexos_open` tried to open any SQLite database in the folder. The default name has
  been changed and only databases ending in "-rplexos.db" are opened. (#13)
* Create several tables that might not always exist in input databases, which caused
  `process_input` to fail. (#14)
* Fixed problem using time query filters. (#12)


# rplexos 0.12

## Enhancements

* Input databases can now be converted to SQLite with `process_input`. (#5)
* Added error detection for very large files in Mac/Linux (files that need
  64-bit ZIP compresion to be read). (#)
* SQLite `id` fields are stored as integers.


# rplexos 0.11

## Enhancements

* Option to weight by time in `sum_interval` (e.g., to correctly when energy from
  power time series). (#2)
* Print PLEXOS and rplexos versions in `summary(db)`. (#5)
* Catch common errors reading XML and log files.
* Add debug mode that prints more information on screen. It can be activated by running
  `options(rplexos.debug = TRUE)` before executing.
* Avoid crash when processing a really big file in Mac or Linux. For now it only returns
  a warning with information. A future update will fix this error, rather than skipping
  the file.


## Bugs

* Solution processing used to fail if a generator is assigned to more than one
  region. (#8)
* Parsing log file can fail for several reasons (file too big, old version of PLEXOS).
  An error in parsing does not stop the process and only throws a warning. (#6)
* Interval queries were not being expanded correctly if the simulation had less
  entries than the `time` table.


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
