library(rplexos)
context("Process files")

options(rplexos.process_on_the_fly = F)

loc <- location_input_rplexos()
locXML <- file.path(loc, "three_nodes.xml")
locDB  <- file.path(loc, "three_nodes-input.db")

loc2 <- location_solution_rplexos()
loc2ZIP <- file.path(loc2, "Model_Base_Solution.zip")
loc2DB  <- file.path(loc2, "Model_Base_Solution-rplexos.db")

loc3 <- location_solution_rplexos()
loc3ZIP <- file.path(loc3, "Model_Base_Solution.zip")
loc3DB  <- file.path(loc3, "Model_Base_Solution-rplexos.db")
loc3TEMP  <- file.path(loc3, "Model_Base_Solution-temp.db")

locWAR <- system.file("extdata", package = "rplexos")
locERR <- system.file("doc", package = "rplexos")

test_that("Process database", {
  skip_on_cran()
  expect_true(process_folder(loc))
  expect_equal(process_input(locXML), locDB)
})

test_that("Process solution", {
  skip_on_cran()
  expect_true(process_folder(loc2))
  expect_equal(process_solution(loc2ZIP), loc2DB)
})

test_that("Process vector of folders", {
  skip_on_cran()
  expect_true(process_folder(c(loc, loc2)))
})

test_that("Expected errors and warnings", {
  skip_on_cran()
  expect_warning(process_folder(locWAR))
  expect_error(process_folder(locERR))
  expect_error(process_folder(loc2ZIP))
  expect_warning(process_folder(c(loc, locERR)))
})

test_that("Process on the fly", {
  skip_on_cran()
  options(rplexos.process_on_the_fly = T)
  expect_true(process_folder(loc3))
  expect_equal(process_solution(loc3ZIP), loc3DB)
  expect_true(file.exists(loc3TEMP))
})
