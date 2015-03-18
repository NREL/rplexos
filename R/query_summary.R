# Functions to quickly query certain information from the solution

get_samples <- function(db) {
  sql <- "SELECT DISTINCT phase_id, period_type_id AS is_summary, sample FROM key"
  query_sql(db, sql) %>%
    add_phase_names %>%
    select(scenario, position, phase_id, phase, is_summary, sample) %>%
    arrange(position, phase_id, is_summary)
}


get_timeslices <- function(db) {
  sql <- "SELECT DISTINCT phase_id, period_type_id AS is_summary, timeslice FROM key"
  query_sql(db, sql) %>%
    add_phase_names %>%
    select(scenario, position, phase_id, phase, is_summary, timeslice) %>%
    arrange(position, phase_id, is_summary)
}

get_bands <- function(db) {
  sql <- "SELECT DISTINCT phase_id, period_type_id AS is_summary, band FROM key"
  query_sql(db, sql) %>%
    add_phase_names %>%
    select(scenario, position, phase_id, phase, is_summary, band) %>%
    arrange(position, phase_id, is_summary)
}

get_classes <- function(db) {
  sql <- "SELECT DISTINCT class_group, class FROM key"
  query_sql(db, sql) %>%
    select(-filename) %>%
    arrange(position, class_group, class)
}

get_class_members <- function(db, class) {
  sql <- sprintf("SELECT DISTINCT name, parent, region, zone FROM key WHERE class = '%s'", class)
  query_sql(db, sql) %>%
    select(-filename) %>%
    arrange(position, name)
}

get_generators <- function(db) get_class_members(db, "Generator")
get_regions    <- function(db) get_class_members(db, "Region")
get_zones      <- function(db) get_class_members(db, "Zone")


# Shortcut to add phase names to a result
add_phase_names <- function(x) {
  phases <- c("LT", "PASA", "MT", "ST")
  phases.df <- data.frame(phase_id = 1:4, phase = factor(phases, levels = phases))
  x %>% inner_join(phases.df, by = "phase_id")
}