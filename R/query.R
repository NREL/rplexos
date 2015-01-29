
# Get query for all scenarios
query_scenario <- function(db, query) {
  # Check inputs
  assert_that(is.rplexos(db), is.string(query))
  
  db %>%
    do(query_one_row(., query)) %>%
    ungroup
}

# Correctly get query for a row (won't be necessary with future version of dplyr)
query_one_row <- function(db, query) {
  res <- dbGetQuery(db$db$con, query)
  if (nrow(res) == 0)
    return(data.frame())
  
  data.frame(scenario = db$scenario[1],
             position = db$position[1],
             res)
}

# Get a table for all scenarios
get_table_scenario <- function(db, from, columns = c("scenario", "position")) {
  # Check inputs
  assert_that(is.rplexos(db), is.string(from))
  
  db %>%
    do(get_table_one_scenario(., from, columns)) %>%
    ungroup
}

# Correctly get query for a row (won't be necessary with future version of dplyr)
get_table_one_scenario <- function(db, from, columns) {
  # Check that table exists
  if (!from %in% src_tbls(db$db)) {
    return(data.frame())
  }
  res <- tbl(db$db, from) %>% collect
  if (nrow(res) == 0)
    return(data.frame())
  
  cbind(db[columns] %>% as.data.frame, res)
}

#' Get list of available properties
#'
#' Produce a list of available properties, their units and the collections they belong to.
#' Additionally, a column is created for each scenario that indicates in how many databases
#' the property appears.
#'
#' @param db PLEXOS database object
#'
#' @seealso \code{\link{plexos_open}} to create the PLEXOS database object
#'
#' @export
query_property <- function(db) {
  out <- get_table_scenario(db, "property")
  phases <- c("LT", "PASA", "MT", "ST")
  phases.df <- data.frame(phase_id = 1:4, phase = factor(phases, levels = phases))
  out %>%
    inner_join(phases.df, by = "phase_id") %>%
    reshape2::dcast(phase_id + phase + is_summary + class_group + class + collection + property + unit ~ scenario,
                    length, value.var = "unit")
}


#' Query configuration tables
#'
#' Get information from the \code{config} table, which includes: PLEXOS version, solution
#' date and time, machine and location of PLEXOS input database, model description and user
#' name. Additionally, it stores the version of rplexos used to process the PLEXOS database.
#'
#' @param db PLEXOS database object
#'
#' @seealso \code{\link{plexos_open}} to create the PLEXOS database object
#'
#' @export
query_config <- function(db) {
  data <- get_table_scenario(db, "config", columns = c("scenario", "position", "filename"))
  reshape2::dcast(data, position + scenario + filename ~ element, value.var = "value")
}

#' Query log file information
#'
#' During the processing of the PLEXOS databases, information from the log file is saved
#' into the database. This includes solution times and infeasibilities for the different phases.
#'
#' @param db PLEXOS database object
#'
#' @seealso \code{\link{plexos_open}} to create the PLEXOS database object
#'
#' @export
query_log <- function(db) {
  get_table_scenario(db, "log_info", c("scenario", "filename"))
}

#' @rdname query_log
#' @export
query_log_steps <- function(db) {
  get_table_scenario(db, "log_steps", c("scenario", "filename"))
}

# Query databases ***********************************************************************

#' Query data and aggregate data
#' 
#' This collection of functions retrieves data from the processed PLEXOS solutions and
#' returns it in a convenient format.
#' 
#' The family \code{query_*} returns the raw data in the databases, while \code{sum_*}
#' aggregates the data according to \code{columns}.
#'
#' The functions \code{*_day}, \code{*_week}, \code{*_month} and \code{*_year} are
#' shortcuts for the corresponding, \code{*_master} function.
#' 
#' The following is a list of valid items for \code{columns} and filtering. Additionally,
#' \code{time} can be specified for summary data (interval data always includes \code{time}).
#' \itemize{
#'   \item{\code{category}}
#'   \item{\code{property}}
#'   \item{\code{name} (default for columns)}
#'   \item{\code{parent} (automatically selected when \code{name} is selected)}
#'   \item{\code{category}}
#'   \item{\code{region} (only meaningful for generators)}
#'   \item{\code{zone} (only meaningful for generators)}
#'   \item{\code{period_type}}
#'   \item{\code{band}}
#'   \item{\code{sample}}
#'   \item{\code{timeslice}}
#' }
#' 
#' If defined, the \code{filter} parameter must be a \code{list}. The elements must be chracter
#' vectors and need to have a valid column name (see previous bullet points). For example, one
#' could define it as follows:
#' 
#' \code{filter = list(name = c("Generator1", "Generator2"), region = "Region1")}
#' 
#' To filter by time use the \code{time.range} parameter, instead of adding it as an entry in the
#' \code{filter} parameter.
#' 
#' If a scenario has multiple databases, the data will be aggregated automatically. If two or more
#' databases within the same scenario have overlapping time periods, the default is to select the
#' data from the last database (execute \code{summary(db)} so see the order). To change this behavior
#' set the global option \code{rplexos.tiebreak} to \code{first}, \code{last}, or \code{all} to
#' select data from the first database, the last one or keep all of them.
#' 
#' Multiple properties can be queried within a collection. If \code{prop} equals the widcard
#' \code{"*"}, all the properties within a collection are returned.
#' 
#' The parameter \code{multiply.time} allows to multiply values by interval duration (in hours) when
#' doing the sum of interval data. This can be used, for example, to obtain total energy (in MWh)
#' from power time series (in MW).
#' 
#' @param db PLEXOS database object
#' @param time character. Table to query from (interval, day, week, month, year)
#' @param col character. Collection to query
#' @param prop character vector. Property or properties to query
#' @param columns character. Data columns to query or aggregate by (defaults to \code{name})
#' @param time.range character. Range of dates (Give in 'ymdhms' or 'ymd' format)
#' @param filter list. Used to filter by data columns (see details)
#' @param phase integer. PLEXOS optimization phase (1-LT, 2-PASA, 3-MT, 4-ST)
#' @param multiply.time boolean. When summing interval data, provide the value multiplied by interval duration (See details).
#' @param ... parameters passed from shortcut functions to master (all except \code{time})
#' 
#' @return A data frame that contains data summarized/aggregated by scenario.
#' 
#' @seealso \code{\link{plexos_open}} to create the PLEXOS database object
#' 
#' @export
query_master <- function(db, time, col, prop, columns = "name", time.range = NULL, filter = NULL, phase = 4) {
  # Check inputs
  assert_that(is.rplexos(db))
  assert_that(is.string(time), is.string(col), is.character(prop), is.character(columns), is.scalar(phase))
  assert_that(correct_phase(phase))
  assert_that(are_columns(columns))
  new <- master_checks(db, time, col, prop, columns, time.range, filter, phase)
  
  # Expand db to include multiple properties (if necessary)
  db.temp <- expand.grid(position = db$position, property = new$prop) %>%
    mutate(collection = col)
  db.prop <- db.temp %>%
    left_join(db, by = "position")
  
  # This function does the queries and attaches columns to the result
  # until https://github.com/hadley/dplyr/issues/448 is solved
  # TODO: remove after dplyr 0.3.1 (here and possibly other places)
  safe_query <- function(x, ...) {
    out <- query_master_each(x$db, ...)
    if (nrow(out) == 0)
      return(data.frame())
    
    data.frame(scenario   = x$scenario,
               position   = x$position,
               collection = x$collection,
               property   = x$property,
               out)
  }
  
  # Query data for each property
  out <- db.prop %>%
    rowwise() %>%
    do(safe_query(., time, .$collection, .$property, new$columns, new$time.range, filter, phase))
  
  # Return empty dataframe if no results were returned
  if (nrow(out) == 0) {
    warning("Query returned no results", call. = FALSE)
    return(data.frame())
  }
  
  # Check if any scenario is missing from the results
  missing.scenario <- setdiff(unique(db$scenario), unique(out$scenario))
  if (length(missing.scenario) > 0L) {
    warning("Query returned no results for scenarios: ",
            paste(missing.scenario, collapse = ", "),
            call. = FALSE)
  }
  
  # Solve ties if they exist
  out <- out %>%
    solve_ties()
  
  # Convert columns to factors
  for (i in setdiff(columns, "time"))
    out[[i]] <- factor(out[[i]])
  
  out
}

# Deal with repeats
solve_ties <- function(x, opt = getOption("rplexos.tiebreak")) {
  # Get option to see how to deal with ties (defaults to last)
  if (is.null(opt)) {
    opt <- "last"
  } else if (!opt %in% c("first", "last", "all")) {
    warning("Invalid 'rplexos.tiebreak' option (must be one of: first, last, all). Using last instead", call. = FALSE)
    opt <- "last"
  }
  
  if (opt %in% c("first", "last")) {
    # Group by time
    x2 <- x %>%
      ungroup() %>%
      group_by(scenario, time)
    
    if (opt == "last") {
      # If there are repeats, use the latter entry
      x2 <- x2 %>%
        filter(position == max(position))
    } else {
      # If there are repeats, use the latter entry
      x2 <- x2 %>%
        filter(position == min(position))
    }
    
    # Ungroup and delete path column
    x2 <- x2 %>%
      ungroup() %>%
      select(-position)
  }
  
  x2
}

#' @rdname query_master
#' @export
query_interval <- function(db, ...) query_master(db, "interval", ...)
#' @rdname query_master
#' @export
query_day      <- function(db, ...) query_master(db, "day", ...)
#' @rdname query_master
#' @export
query_week     <- function(db, ...) query_master(db, "week", ...)
#' @rdname query_master
#' @export
query_month    <- function(db, ...) query_master(db, "month", ...)
#' @rdname query_master
#' @export
query_year     <- function(db, ...) query_master(db, "year", ...)

# Query interval for each database
#' @importFrom data.table data.table CJ
query_master_each <- function(db, time, col, prop, columns, time.range, filter, phase, table.name) {
  # Divide time.range vector
  if (!is.null(time.range)) {
    time.r.min <- time.range[1]
    time.r.max <- time.range[2]
  }
  
  # Summary data
  if (time != "interval") {
    # Check that time table has any data
    count <- tbl(db, time) %>%
      summarize(rows = n()) %>%
      collect
    if (count$rows == 0) {
        warning("Summary table '", time, "' is empty for database '", db$info$dbname, "'.\n",
                "    Returning an empty result.", call. = FALSE)
        return(data.frame())
    }
    
    # Get data
    out <- tbl(db, time) %>%
      filter(collection == col, property == prop, phase_id == phase) %>%
      filter_rplexos(filter)
    
    # Add time filter condition
    if (!is.null(time.range)) {
      out <- out %>% filter(between(time, time.r.min, time.r.max))
    }
    
    # Add time and value columns
    columns.dots <- c("unit", setdiff(columns, "time"), "time", "value") %>%
      as.list %>%
      lapply(as.symbol)
    
    # Query and format
    out <- out %>%
      select_(.dots = columns.dots) %>%
      collect %>%
      mutate(time = lubridate::ymd_hms(time, quiet = TRUE))
    
    return(out)
  }
  
  # Get the table name that stores the data
  t.name <- tbl(db, "property") %>%
    filter(collection == col, property == prop, phase_id == phase, is_summary == 0) %>%
    collect
  the.table.name <- gsub("data_interval_", "", t.name$table_name)
  
  # Get max/min time existing in the table to be queried
  #   In case time table has more time stamps than those in the dataset
  time.limit <- tbl(db, the.table.name) %>%
      filter(phase_id == phase) %>%
      summarize(time_from = min(time_from), time_to = max(time_to)) %>%
      collect
  min.time.data <- time.limit$time_from
  max.time.data <- time.limit$time_to
  
  # Interval data, Get time data
  time.data <- tbl(db, "time") %>%
    filter(between(time, min.time.data, max.time.data))
  
  # Get interval data
  out <- tbl(db, the.table.name) %>%
    filter(phase_id == phase) %>%
    select(-time_to) %>%
    rename(time = time_from)
  
  # Add time filter conditions
  if (!is.null(time.range)) {
    time.data <- time.data %>%
      filter(between(time, time.r.min, time.r.max))
    out <- out %>%
      filter(time_from <= time.r.max, time_to >= time.r.min) %>%
      filter_rplexos(filter)
  }
  
  # Collect time data
  time.data <- time.data %>%
    select(time) %>%
    collect
  
  # If time data is empty, return an empty data frame
  if (nrow(time.data) == 0)
    return(data.frame())
  
  # Convert into R time-data format
  time.data$time <- lubridate::ymd_hms(time.data$time)
    
  # Add key, time and value columns
  columns.dots <- c("key", "unit", setdiff(columns, "time"), "time", "value") %>%
    as.list %>%
    lapply(as.symbol)
  
  # Query and format
  out <- out %>%
    select_(.dots = columns.dots) %>%
    collect %>%
      mutate(time = lubridate::ymd_hms(time, quiet = TRUE))
  
  # Expand data
  #   This will be easier when dplyr supports rolling joins
  out2 <- data.table(out, key = "key,time")
  cj2 <- CJ(key = unique(out$key), time = time.data$time)
  
  out3 <- out2[cj2, roll = TRUE]
  out3 <- out3 %>%
    as.data.frame %>%
    select(-key)
  
  # Restore time zone
  attributes(out3$time) <- attributes(time.data$time)
  
  out3
}


# Aggregation ***************************************************************************

#' @rdname query_master
#' @export
sum_master <- function(db, time, col, prop, columns = "name", time.range = NULL, filter = NULL, phase = 4, multiply.time = FALSE) {
  # Check inputs to unique
  assert_that(is.flag(multiply.time))

  # Make sure to include time
  columns2 <- c(setdiff(columns, "time"), "time")
  
  # Run query_master to get the raw data
  df <- query_master(db, time, col, prop, columns2, time.range, filter, phase)
  
  # If empty query is returned, return empty data.frame
  if(nrow(df) == 0)
    return(data.frame())
  
  # Aggregate the data
  out <- df %>% group_by_char(c("scenario", "collection", "property", columns2)) %>%
    summarise(value = sum(value))
  
  if ((time == "interval") & multiply.time) {
    # Get length of intervals in hours
    times <- get_table_scenario(d, "time", "scenario")
    delta <- times %>%
      group_by(scenario) %>%
      mutate(time = lubridate::ymd_hms(time)) %>%
      summarize(interval = min(lubridate::int_length(lubridate::int_diff(time))) / 3600)
    
    # Add interval duration to the sum
    out <- out %>%
      inner_join(delta, by = "scenario") %>%
      summarise(value = sum(value * interval))  
    
    # If unit is a column, modify column
    if ("unit" %in% names(out)) {
      out <- out %>%
        mutate(unit = paste(unit, "* h"))
    }
  } else {
    # Sum values
    out <- out %>%
      summarise(value = sum(value))  
  }
  
  # Convert columns to factors
  for (i in setdiff(columns2, "time"))
    out[[i]] <- factor(out[[i]])
  
  out
}

#' @rdname query_master
#' @export
sum_interval <- function(db, ...) sum_master(db, "interval", ...)
#' @rdname query_master
#' @export
sum_day      <- function(db, ...) sum_master(db, "day", ...)
#' @rdname query_master
#' @export
sum_week     <- function(db, ...) sum_master(db, "week", ...)
#' @rdname query_master
#' @export
sum_month    <- function(db, ...) sum_master(db, "month", ...)
#' @rdname query_master
#' @export
sum_year     <- function(db, ...) sum_master(db, "year", ...)

# Checks and common data maniputation for query_master and sum_master
master_checks <- function(db, time, col, prop, columns, time.range, filter, phase) {
  # Get list of properties for the collection
  is.summ <- ifelse(time == "interval", 0, 1)
  is.summ.txt <- ifelse(time == "interval", "interval", "summary")
  res <- get_table_scenario(db, "property") %>%
    filter(collection == col, is_summary == is.summ, phase_id == phase) %>%
    collect
  
  # Check that collection is valid
  if (nrow(res) == 0) {
    stop("Collection '", col, "' is not valid for ", is.summ.txt, " data and phase '", phase, "'.\n",
         "   Use query_property() for list of collections and properties.",
         call. = FALSE)
  }
  
  # Checks if property is the wildcard symbol
  if (length(prop) == 1L) {
    if (prop == "*")
      prop <- unique(res$property)
  }
  
  # Check that all properties are valid
  invalid.prop <- setdiff(prop, res$property)
  if (length(invalid.prop) == 1L) {
    stop("Property '", invalid.prop, "' in collection '", col, "' is not valid for ", is.summ.txt, " data and phase '", phase, "'.\n",
         "   Use query_property() for list of available collections and properties.",
         call. = FALSE)
  } else if (length(invalid.prop) > 1L) {
    stop("Properties ", paste0("'", invalid.prop, "'", collapse = ", "), " in collection '", col,
         "' are not valid for ", is.summ.txt, " data and phase '", phase, "'.\n",
         "   Use query_property() for list of available collections and properties.",
         call. = FALSE)
  }
  
  # Find if the data is going to have multiple sample, timeslices or bands
  res2 <- res %>%
    ungroup() %>%
    filter(property %in% prop) %>%
    summarize(is_multi_band      = max(count_band) > 1,
              is_multi_sample    = max(count_sample) > 1,
              is_multi_timeslice = max(count_timeslice) > 1)
  if (res2$is_multi_timeslice)
    columns <- c(setdiff(columns, "timeslice"), "timeslice")
  if (res2$is_multi_band)
    columns <- c(setdiff(columns, "band"), "band")
  if (res2$is_multi_sample)
    columns <- c(setdiff(columns, "sample"), "sample")
  
  # Key filter checks
  if (!is.null(filter)) {
    assert_that(is.list(filter))
    assert_that(names_are_columns(filter))
    assert_that(time_not_a_name(filter))
  }
  
  # Columns should not include collection and property; they are always reported
  columns <- setdiff(columns, c("collection", "property"))
  
  # If columns include name, add parent automatically
  if ("name" %in% columns)
    columns <- c("name", "parent", setdiff(columns, c("name", "parent")))
  
  # Time range checks and convert to POSIXct
  if (!is.null(time.range)) {
    assert_that(is.character(time.range), length(time.range) == 2L)
    time.range <- lubridate::parse_date_time(time.range, c("ymdhms", "ymd"), quiet = TRUE)
    assert_that(correct_date(time.range))
  }
  
  list(prop = prop, columns = columns, time.range = time.range) 
}

# Fix problem with filtering times in dplyr
#    May be addressed in dplyr in the future
#    https://github.com/hadley/dplyr/issues/857
escape.POSIXct <- dplyr:::escape.Date


# Filtering *****************************************************************************

# Other filters
filter_rplexos <- function(out, filt) {
  # Do nothing if filter is empty
  if (is.null(filt))
    return(out)
  if (length(filt) == 0L)
    return(out)
  
  # Write the condition as text
  vals <- lapply(filt, function(x)
    paste0("\"", x, "\"", collapse = ", ")) %>%
    paste0("c(", ., ")")
  cons <- ifelse(lapply(filt, length) == 1, "==", "%in%")
  cond <- paste(names(filt), cons, vals)
  
  # Apply condition
  out %>% filter_(.dots = cond)
}
