# rplexos 0.12.1.99

## Enhancements

* Added shortcut functions to enable/disable debug mode: `start_debug_rplexos()` and
  `stop_debug_rplexos()`.
* `plexos_open` no longer opens the SQLite connections, so `plexos_close` is not needed
  anymore.
* Expose function `query_sql`, which can be used to execute a SQL query on each PLEXOS
  database. (#20)
* Add fast queries for phases, timeslices, samples, objects, classes, time, etc. in the
  solutions. See `help("query_time")` and related functions. (#20)


## Bug fixes

* Non-time filters are now correclty applied if `time.range` is not defined. (#15)
* Ability to read large (>3 GB) input files.
* Fix detection of invalid properties in queries.


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
