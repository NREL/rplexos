context("Query solutions")

disable_otf_rplexos(F)

loc <- location_solution_rplexos()
locERR <- system.file("extdata", package = "rplexos")

process_folder(loc)
db <- plexos_open(loc)

loc_LT <- location_solution_rplexos('LT')

process_folder(loc_LT)
db_LT <- plexos_open(loc_LT)

time_range_db <- query_time(db) %>% dplyr::select(start,end)
time_range_db_LT <- query_time(db_LT) %>% dplyr::select(start,end)
time_range <- c(format(time_range_db$start[1], '%Y-%m-%d %H:%M:%S'),
                format(time_range_db$end[1], '%Y-%m-%d %H:%M:%S'))
time_range_LT <- c(format(time_range_db_LT$start[1], '%Y-%m-%d %H:%M:%S'),
                   format(time_range_db_LT$end[1], '%Y-%m-%d %H:%M:%S'))
time_range_UTC <- as.POSIXct(c(format(time_range_db$start[1], '%Y-%m-%d %H:%M:%S'),
                               format(time_range_db$end[1], '%Y-%m-%d %H:%M:%S')),
                             tz = 'UTC')

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

test_that("Query errors", {
  expect_error(query_interval(db, "Generator", "Gen"))
  expect_error(query_interval(db, "Gen", "Generation"))
  expect_error(query_interval(db, "Generator", "Generation", phase = 3))
  expect_error(query_interval(db, "Generator", "Generation", filter = c("2010-13-10", "2011-14-01")))
  expect_error(query_interval(db, "Generator", "Generation", filter = c("2010-01-10", "2011-01-01")))
  expect_error(query_interval(db, "Generator", "Generation", filter = list(time = c("2010-01-10", "2011-01-01"))))
  expect_warning(query_interval(db, "Generator", "Generation", filter = list(name = "test")))
  expect_warning(query_interval(db, "Generator", "Generation", time.range = c("2010-01-10", "2011-01-01")))
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
lstPos <- list(name = c("Baseload", "Peaker"))
lstNeg <- list(name = c("-Wind"))
lstNeg2 <- list(name = c("-Baseload", "-Peaker"))
lstComb <- list(name = c("-Baseload", "Wind"))

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

  expect_identical(query_day(db, "Generator", "Generation", time.range = as.POSIXct(oneTimeDD)) %>% nrow, 3L)
  expect_identical(query_day(db, "Generator", "Generation", time.range = as.POSIXct(oneTimeHH)) %>% nrow, 3L)

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

  expect_identical(query_interval(db, "Generator", "Generation", filter = lstPos, time.range = halfTimeHD),
                   query_interval(db, "Generator", "Generation", filter = lstNeg, time.range = halfTimeHD)) # positive versus negative filter
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstPos, time.range = halfTimeHH),
                   query_interval(db, "Generator", "Generation", filter = lstNeg, time.range = halfTimeHH)) # positive versus negative filter


  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin, time.range = halfTimeHD),
                   query_interval(db, "Generator", "Generation", filter = lstNeg2, time.range = halfTimeHD)) # double negation
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstWin, time.range = halfTimeHH),
                   query_interval(db, "Generator", "Generation", filter = lstNeg2, time.range = halfTimeHH)) # double negation

  expect_identical(query_interval(db, "Generator", "Generation", filter = lstComb, time.range = halfTimeHD),
                   query_interval(db, "Generator", "Generation", filter = lstWin, time.range = halfTimeHD)) # combined filter (positive is always stronger)
  expect_identical(query_interval(db, "Generator", "Generation", filter = lstComb, time.range = halfTimeHH),
                   query_interval(db, "Generator", "Generation", filter = lstWin, time.range = halfTimeHH)) # combined filter (positive is always stronger)
})

lstNot <- list(name = "GenNotPresent")
test_that("Empty queries", {
  suppressWarnings(expect_identical(query_day(db, "Generator", "Generation", filter = lstNot) %>% nrow, 0L))
  suppressWarnings(expect_identical(query_interval(db, "Generator", "Generation", filter = lstNot) %>% nrow, 0L))
  expect_warning(query_day(db, "Generator", "Generation", filter = lstNot), 'Query returned no results')
  expect_warning(query_interval(db, "Generator", "Generation", filter = lstNot), 'Query returned no results')
})

test_that("Sum queries", {
  expect_equal(sum_interval(db, "Generator", "Generation", filter = lstWin)$value,
               query_day(db, "Generator", "Generation", filter = lstWin)$value * 1000)
  expect_equal(sum_day(db, "Generator", "Generation", NULL)$value +
                 sum_day(db, "Battery", "Generation", NULL)$value, # battery and efficiency
               sum_day(db, "Region", "Load")$value)
  expect_equal(sum_interval(db, "Generator", "Generation", "time")$value +
                 sum_interval(db, "Battery", "Generation", "time")$value, # battery and efficiency
               sum_interval(db, "Region", "Load", "time")$value)
  expect_equal(sum_interval(db, "Generator", "Generation")$value,
               sum_interval(db, "Generator", "Generation", multiply.time = TRUE)$value)
})

# test_that("Log queries", {
#   qlog <- query_log(db)
#   expect_is(qlog, "tbl_df")
#   expect_named(qlog, c("scenario", "filename", "phase", "time", "rel_gap_perc", "infeas"))
#   expect_identical(nrow(qlog), 5L)
#
#   qlogsteps <- query_log_steps(db)
#   expect_is(qlogsteps, "tbl_df")
#   expect_named(qlogsteps, c("scenario", "filename", "phase", "step", "total_step", "time", "elapsed"))
#   expect_identical(nrow(qlogsteps), 25L)
# })

test_that("Auxiliary queries", {
  qtime <- query_time(db)
  expect_is(qtime, "tbl_df")
  expect_named(qtime, c("scenario", "position", "phase_id", "phase", "start", "end", "count", "timestep"))
  expect_identical(nrow(qtime), 1L)
  expect_equal(qtime$count, 24)
  expect_equal(qtime$timestep %>% as.numeric, 60)

  qphase <- query_phase(db)
  expect_is(qphase, "tbl_df")
  expect_named(qphase, c("scenario", "position", "phase_id", "phase", "is_summary"))
  expect_identical(nrow(qphase), 2L)

  qband <- query_band(db)
  expect_is(qband, "tbl_df")
  expect_named(qband, c("scenario", "position", "phase_id", "phase", "is_summary", "band"))
  expect_identical(nrow(qband), 2L)

  qsample <- query_sample(db)
  expect_is(qsample, "tbl_df")
  expect_named(qsample, c("scenario", "position", "phase_id", "phase", "is_summary", "sample"))
  expect_identical(nrow(qsample), 2L)

  qtimeslice <- query_timeslice(db)
  expect_is(qtimeslice, "tbl_df")
  expect_named(qtimeslice, c("scenario", "position", "phase_id", "phase", "is_summary", "timeslice"))
  expect_identical(nrow(qtimeslice), 2L)

  expect_is(query_class(db), "tbl_df")
  expect_named(query_class(db), c("scenario", "position", "class_group", "class"))
  expect_identical(query_class(db) %>% nrow, 6L)

  expect_is(query_class_member(db, "Generator"), "tbl_df")
  expect_named(query_class_member(db, "Generator"), c("scenario", "position", "name", "parent", "region", "zone"))
  expect_identical(query_class_member(db, "Generator") %>% nrow, 3L)
  expect_identical(query_class_member(db, "Node") %>% nrow, 3L)
  expect_identical(query_class_member(db, "Line") %>% nrow, 3L)
  expect_identical(query_class_member(db, "Region") %>% nrow, 1L)

  qconfig <- query_config(db)
  expect_is(qconfig, "data.frame")
  expect_identical(nrow(qconfig), 1L)

  qproperty <- query_property(db)
  expect_is(qproperty, "data.frame")
  expect_named(qproperty,
               c("phase_id", "phase", "is_summary", "class_group", "class", "collection", "property",
                 "unit", db$scenario[1] %>% as.character))
  expect_identical(nrow(qproperty), 55L)
})

test_that("Time range", {
  expect_identical(query_interval(db, "Generator", "Generation", time.range = time_range),
                   query_interval(db, "Generator", "Generation")) # test to see if it indeed queries the whole solution
  expect_identical(query_interval(db, "Generator", "Generation", time.range = time_range_UTC),
                   query_interval(db, "Generator", "Generation")) # test to see if the Plexos data is indeed
                                                                  # parsed under UTC
})

test_that("All tables exist", {
  expect_true(query_interval(db, "Battery", "Generation", time.range = time_range) %>% nrow == 24L)
  expect_true(query_interval(db, "Battery", "Load", time.range = time_range) %>% nrow == 24L)
  expect_true(query_interval(db, "Battery", "Net Generation", time.range = time_range) %>% nrow == 24L)
  expect_true(query_interval(db, "Battery", "SoC", time.range = time_range) %>% nrow == 24L)
  expect_true(query_interval(db, "Generator", "Capacity Curtailed", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Generation", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Generation Cost", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Price Received", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Units Generating", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Line", "Export Limit", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Line", "Flow", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Line", "Import Limit", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Node", "Generation", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Node", "Load", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Node", "Price", time.range = time_range) %>% nrow == 72L)
})

test_that("Process LT queries", {
  expect_true(query_interval(db_LT, "Battery", "Generation", time.range = time_range_LT, phase = 1) %>% nrow == 365L)
  expect_true(query_interval(db_LT, "Battery", "Load", time.range = time_range_LT, phase = 1) %>% nrow == 365L)
  expect_true(query_interval(db_LT, "Battery", "Net Generation", time.range = time_range_LT, phase = 1) %>% nrow == 365L)
  expect_true(query_interval(db_LT, "Battery", "SoC", time.range = time_range_LT, phase = 1) %>% nrow == 365L)
  expect_true(query_interval(db_LT, "Generator", "Generation", time.range = time_range_LT, phase = 1) %>% nrow == 1095L)
  expect_equal(query_year(db_LT, "Generator", "Generation", time.range = time_range_LT, phase = 1) %>% .$value %>% as.numeric %>% sum(),
               query_interval(db_LT, "Generator", "Generation", time.range = time_range_LT, phase = 1) %>% .$value %>% as.numeric %>% sum() * 24 / 1000)
})

test_that("Process queries on the fly", {
  skip_on_os('linux')
  skip_on_os('solaris')
  skip_on_os('mac')
  
  enable_otf_rplexos(F)
  
  loc <- location_solution_rplexos()
  locERR <- system.file("extdata", package = "rplexos")
  
  process_folder(loc)
  db <- plexos_open(loc)
  
  expect_true(query_interval(db, "Battery", "Generation", time.range = time_range) %>% nrow == 24L)
  expect_true(query_interval(db, "Battery", "Generation", time.range = time_range) %>% nrow == 24L) # no double processing
  expect_true(query_interval(db, "Battery", "Load", time.range = time_range) %>% nrow == 24L)
  expect_true(query_interval(db, "Battery", "Net Generation", time.range = time_range) %>% nrow == 24L)
  expect_true(query_interval(db, "Battery", "SoC", time.range = time_range) %>% nrow == 24L)
  expect_true(query_interval(db, "Generator.Fuels", "Generation", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Capacity Curtailed", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Generation", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Generation Cost", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Price Received", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Generator", "Units Generating", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Line", "Export Limit", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Line", "Flow", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Line", "Import Limit", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Node", "Generation", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Node", "Load", time.range = time_range) %>% nrow == 72L)
  expect_true(query_interval(db, "Node", "Price", time.range = time_range) %>% nrow == 72L)
  expect_equal(query_day(db, "Generator", "Generation", filter = lstWin) %>% nrow, 1L)
})
