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
  expect_identical(qday %>% nrow, 3L)
  
  expect_is(qint, "tbl_df")
  expect_named(qint, c("scenario", "collection", "property", "unit", "name", "parent", "time", "value"))
  expect_identical(qint %>% nrow, 72L)
  
  expect_is(qday2, "tbl_df")
  expect_named(qday2, c("scenario", "collection", "property", "unit", "name", "parent", "region", "time", "value"))
  expect_identical(qday2 %>% nrow, 3L)
  
  expect_is(qint2, "tbl_df")
  expect_named(qint2, c("scenario", "collection", "property", "unit", "name", "parent", "region", "time", "value"))
  expect_identical(qint2 %>% nrow, 72L)
  
  expect_is(qday3, "tbl_df")
  expect_named(qday, c("scenario", "collection", "property", "unit", "name", "parent", "time", "value"))
  expect_identical(qday3 %>% nrow, 9L)
  
  expect_is(qint3, "tbl_df")
  expect_named(qint3, c("scenario", "collection", "property", "unit", "name", "parent", "time", "value"))
  expect_identical(qint3 %>% nrow, 360L)
  
  expect_error(query_day(db, "Generator", "Generation", phase = 3))
  expect_error(query_interval(db, "Generator", "Generation", phase = 3))
  expect_error(query_day(db, "Generator", "Generation2"))
  expect_error(query_interval(db, "Generator", "Generation2"))
  expect_error(query_day(db, "Generator2", "Generation"))
  expect_error(query_interval(db, "Generator2", "Generation"))
})

lstWin <- list(name = "Wind")
lstAll <- list(name = c("Baseload", "Peaker", "Wind"))
allTimeDD <- c("2015-03-14", "2015-03-15")
allTimeHH <- c("2015-03-14 00:00:00", "2015-03-15 00:00:00")

oneTimeDD <- c("2015-03-14", "2015-03-14")
oneTimeHD <- c("2015-03-14 00:00:00", "2015-03-14")
oneTimeDH <- c("2015-03-14", "2015-03-14 00:00:00")
oneTimeHH <- c("2015-03-14 00:00:00", "2015-03-14 00:00:00")

halfTimeHD <- c("2015-03-14", "2015-03-14 11:00:00")
halfTimeHH <- c("2015-03-14 00:00:00", "2015-03-14 11:00:00")

test_that("Filters in queries", {
  expect_equal(query_day(db, "Generator", "Generation", filter = lstAll), qday)
  expect_equal(query_interval(db, "Generator", "Generation", filter = lstAll), qint)
  
  expect_identical(query_day(db, "Generator", "Generation", filter = lstWin) %>% nrow, 1L)
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin) %>% nrow, 24L)
  
  expect_equal(query_day(db, "Generator", "Generation", time.range = allTimeDD), qday)
  expect_equal(query_day(db, "Generator", "Generation", time.range = allTimeHH), qday)
  expect_equal(query_interval(db, "Generator", "Generation", time.range = allTimeDD), qint)
  expect_equal(query_interval(db, "Generator", "Generation", time.range = allTimeHH), qint)
  
  expect_identical(query_day(db, "Generator", "Generation", time.range = oneTimeDD) %>% nrow, 3L)
  expect_identical(query_day(db, "Generator", "Generation", time.range = oneTimeHD) %>% nrow, 3L)
  expect_identical(query_day(db, "Generator", "Generation", time.range = oneTimeDH) %>% nrow, 3L)
  expect_identical(query_day(db, "Generator", "Generation", time.range = oneTimeHH) %>% nrow, 3L)
  
  expect_identical(query_interval(db, "Generator", "Generation", time.range = oneTimeDD) %>% nrow, 3L)
  expect_identical(query_interval(db, "Generator", "Generation", time.range = oneTimeHD) %>% nrow, 3L)
  expect_identical(query_interval(db, "Generator", "Generation", time.range = oneTimeDH) %>% nrow, 3L)
  expect_identical(query_interval(db, "Generator", "Generation", time.range = oneTimeHH) %>% nrow, 3L)
  
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin, time.range = oneTimeDD) %>% nrow, 1L)
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin, time.range = oneTimeHD) %>% nrow, 1L)
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin, time.range = oneTimeDH) %>% nrow, 1L)
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin, time.range = oneTimeHH) %>% nrow, 1L)
  
  expect_identical(query_interval(db, "Generator", "Generation", time.range = halfTimeHD) %>% nrow, 36L)
  expect_identical(query_interval(db, "Generator", "Generation", time.range = halfTimeHH) %>% nrow, 36L)
  
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin, time.range = halfTimeHD) %>% nrow, 12L)
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin, time.range = halfTimeHH) %>% nrow, 12L)
})

test_that("Sum queries", {
  expect_equal(sum_interval(db, "Generator", "Generation", filter = lstWin)$value,
               query_day(db, "Generator", "Generation", filter = lstWin)$value * 1000)
  expect_equal(sum_day(db, "Generator", "Generation", NULL)$value,
               sum_day(db, "Region", "Load")$value)
  expect_equal(sum_interval(db, "Generator", "Generation", "time")$value,
               sum_interval(db, "Region", "Load", "time")$value)
  expect_equal(sum_interval(db, "Generator", "Generation")$value,
               sum_interval(db, "Generator", "Generation", multiply.time = TRUE)$value)
})

test_that("Log queries", {
  expect_is(query_log(db), "tbl_df")
  expect_named(query_log(db), c("scenario", "filename", "phase", "time", "rel_gap_perc", "infeas"))
  expect_identical(query_log(db) %>% nrow, 5L)
  
  expect_is(query_log_steps(db), "tbl_df")
  expect_named(query_log_steps(db), c("scenario", "filename", "phase", "step", "total_step", "time", "elapsed"))
  expect_identical(query_log_steps(db) %>% nrow, 25L)
})

test_that("Auxiliary queries", {
  expect_is(query_time(db), "tbl_df")
  expect_named(query_time(db), c("scenario", "position", "phase_id", "phase", "start", "end", "count", "timestep"))
  expect_identical(query_time(db) %>% nrow, 1L)
  expect_equal(query_time(db)$count, 24)
  expect_equal(query_time(db)$timestep %>% as.numeric, 60)
  
  expect_is(query_phase(db), "tbl_df")
  expect_named(query_phase(db), c("scenario", "position", "phase_id", "phase", "is_summary"))
  expect_identical(query_phase(db) %>% nrow, 2L)
  
  expect_is(query_band(db), "tbl_df")
  expect_named(query_band(db), c("scenario", "position", "phase_id", "phase", "is_summary", "band"))
  expect_identical(query_band(db) %>% nrow, 2L)
  
  expect_is(query_sample(db), "tbl_df")
  expect_named(query_sample(db), c("scenario", "position", "phase_id", "phase", "is_summary", "sample"))
  expect_identical(query_sample(db) %>% nrow, 2L)
  
  expect_is(query_timeslice(db), "tbl_df")
  expect_named(query_timeslice(db), c("scenario", "position", "phase_id", "phase", "is_summary", "timeslice"))
  expect_identical(query_timeslice(db) %>% nrow, 2L)
  
  expect_is(query_class(db), "tbl_df")
  expect_named(query_class(db), c("scenario", "position", "class_group", "class"))
  expect_identical(query_class(db) %>% nrow, 4L)
  
  expect_is(query_class_member(db, "Generator"), "tbl_df")
  expect_named(query_class_member(db, "Generator"), c("scenario", "position", "name", "parent", "region", "zone"))
  expect_identical(query_class_member(db, "Generator") %>% nrow, 3L)
  expect_identical(query_class_member(db, "Node") %>% nrow, 3L)
  expect_identical(query_class_member(db, "Line") %>% nrow, 3L)
  expect_identical(query_class_member(db, "Region") %>% nrow, 1L)
  
  expect_identical(query_class_member(db, "Generator"), query_generator(db))
  expect_identical(query_class_member(db, "Region"), query_region(db))
  expect_identical(query_class_member(db, "Zone"), query_zone(db))
  
  expect_is(query_config(db), "data.frame")
  expect_named(query_config(db),
               c("position", "scenario", "filename", "Computer", "Date", "Description", "File",
                 "Model", "Path", "rplexos", "Sim Samples", "Time", "Username", "Version"))
  expect_identical(query_config(db) %>% nrow, 1L)
  
  expect_is(query_property(db), "data.frame")
  expect_named(query_property(db),
               c("phase_id", "phase", "is_summary", "class_group", "class", "collection", "property",
                 "unit", db$scenario[1] %>% as.character))
  expect_identical(query_property(db) %>% nrow, 35L)
})


