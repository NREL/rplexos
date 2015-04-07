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
