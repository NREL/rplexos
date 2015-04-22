library(rplexos)
context("Query solutions")

loc <- location_solution_rplexos()
locERR <- system.file("extdata", package = "rplexos")

process_folder(loc)
db <- plexos_open(loc)

test_that("rplexos attributes", {
  expect_is(db, "rplexos")
  expect_is(db, "data.frame")
  expect_identical(nrow(db), 1L)
  expect_named(db, c("scenario", "position", "filename", "tables", "properties"))
})

test_that("Expected errors and warnings", {
  expect_error(plexos_open(locERR))
  expect_warning(plexos_open(c(loc, locERR)))
})

try(qday <- query_day(db, "Generator", "Generation"))
try(qint <- query_interval(db, "Generator", "Generation"))
try(qday2 <- query_day(db, "Generator", "Generation", c("region", "name")))
try(qint2 <- query_interval(db, "Generator", "Generation", c("region", "name")))
try(qday3 <- query_day(db, "Generator", "*"))
try(qint3 <- query_interval(db, "Generator", "*"))

test_that("Basic query functions", {
  expect_is(qday, "tbl_df")
  expect_named(qday, c("scenario", "collection", "property", "unit", "name", "parent", "time", "value"))
  expect_nrow(qday, 3L)
  
  expect_is(qint, "tbl_df")
  expect_named(qint, c("scenario", "collection", "property", "unit", "name", "parent", "time", "value"))
  expect_nrow(qint, 72L)
  
  expect_is(qday2, "tbl_df")
  expect_named(qday2, c("scenario", "collection", "property", "unit", "name", "parent", "region", "time", "value"))
  expect_nrow(qday2, 3L)
  
  expect_is(qint2, "tbl_df")
  expect_named(qint2, c("scenario", "collection", "property", "unit", "name", "parent", "region", "time", "value"))
  expect_nrow(qint2, 72L)
  
  expect_is(qday3, "tbl_df")
  expect_named(qday, c("scenario", "collection", "property", "unit", "name", "parent", "time", "value"))
  expect_nrow(qday3, 9L)
  
  expect_is(qint3, "tbl_df")
  expect_named(qint3, c("scenario", "collection", "property", "unit", "name", "parent", "time", "value"))
  expect_nrow(qint3, 360L)
  
  expect_error(query_day(db, "Generator", "Generation", phase = 3))
  expect_error(query_interval(db, "Generator", "Generation", phase = 3))
  expect_error(query_day(db, "Generator", "Generation2"))
  expect_error(query_interval(db, "Generator", "Generation2"))
  expect_error(query_day(db, "Generator2", "Generation"))
  expect_error(query_interval(db, "Generator2", "Generation"))
})

test_that("Log queries", {
  expect_is(query_log(db), "tbl_df")
  expect_named(query_log(db), c("scenario", "filename", "phase", "time", "rel_gap_perc", "infeas"))
  expect_nrow(query_log(db), 5L)
  
  expect_is(query_log_steps(db), "tbl_df")
  expect_named(query_log_steps(db), c("scenario", "filename", "phase", "step", "total_step", "time", "elapsed"))
  expect_nrow(query_log_steps(db), 25L)
})

test_that("Auxiliary queries", {
  expect_is(query_phase(db), "tbl_df")
  expect_named(query_phase(db), c("scenario", "position", "phase_id", "phase", "is_summary"))
  expect_nrow(query_phase(db), 2L)
  
  expect_is(query_band(db), "tbl_df")
  expect_named(query_band(db), c("scenario", "position", "phase_id", "phase", "is_summary", "band"))
  expect_nrow(query_band(db), 2L)
  
  expect_is(query_sample(db), "tbl_df")
  expect_named(query_sample(db), c("scenario", "position", "phase_id", "phase", "is_summary", "sample"))
  expect_nrow(query_sample(db), 2L)
  
  expect_is(query_timeslice(db), "tbl_df")
  expect_named(query_timeslice(db), c("scenario", "position", "phase_id", "phase", "is_summary", "timeslice"))
  expect_nrow(query_timeslice(db), 2L)
  
  expect_is(query_class(db), "tbl_df")
  expect_named(query_class(db), c("scenario", "position", "class_group", "class"))
  expect_nrow(query_class(db), 4L)
  
  expect_is(query_class_member(db, "Generator"), "tbl_df")
  expect_named(query_class_member(db, "Generator"), c("scenario", "position", "name", "parent", "region", "zone"))
  expect_nrow(query_class_member(db, "Generator"), 3L)
  expect_nrow(query_class_member(db, "Node"), 3L)
  expect_nrow(query_class_member(db, "Line"), 3L)
  expect_nrow(query_class_member(db, "Region"), 1L)
  
  expect_identical(query_class_member(db, "Generator"), query_generator(db))
  expect_identical(query_class_member(db, "Region"), query_region(db))
  expect_identical(query_class_member(db, "Zone"), query_zone(db))
  
  expect_is(query_config(db), "data.frame")
  expect_named(query_config(db),
               c("position", "scenario", "filename", "Computer", "Date", "Description", "File",
                 "Model", "Path", "rplexos", "Sim Samples", "Time", "Username", "Version"))
  expect_nrow(query_config(db), 1L)
  
  expect_is(query_property(db), "data.frame")
  expect_named(query_property(db),
               c("phase_id", "phase", "is_summary", "class_group", "class", "collection", "property",
                 "unit", db$scenario[1] %>% as.character))
  expect_nrow(query_property(db), 35L)
})


