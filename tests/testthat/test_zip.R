context("Seek zipped files")

file_zip <- tempfile(fileext = '.zip')
file_bin <- tempfile(fileext = '.bin')
file_bin_rel <- basename(file_bin)
if(Sys.getenv("R_ARCH") == '/i386'){ # 32 bit cannot write larger than 2GB
  writeBin(rep(as.raw(1:255),1e6), file_bin) # write big binary file
}else{
  writeBin(rep(as.raw(1:255),1e6*4*2), file_bin) # write big binary file
}
x <- zip(file_zip, file_bin, flags = '-j') # zip file

test_that("Binary file not corrupt", {
  expect_equal(readBin(file_bin, raw(1), n = 16),
               as.raw(1:16)) # read part of written binary to check for corruptness
  expect_silent(readBin(file_zip, raw(1), n = 16)) # read part of written zip to check for corruptness
})

test_that("Read zip", {
  zip.con <- unz(file_zip, file_bin_rel, open = "rb")
  expect_equal(readBin(zip.con, raw(1), n = 16),
               as.raw(1:16))
  expect_equal(readBin(zip.con, raw(1), n = 16),
               as.raw(17:32)) # pointer moves automatically for readBin
  expect_equal(read_zip(file_zip,  file_bin_rel, raw(1), 0, 16),
               as.raw(1:16))
  expect_equal(read_zip(file_zip,  file_bin_rel, raw(1), 16, 16),
               as.raw(17:32)) # pointer moves automatically for readBin
  close(zip.con)
})

test_that("Seek", {
  expect_equal(read_zip(file_zip,  file_bin_rel, raw(1), 5, 4),
               as.raw(1:16)[6:9]) # check if the read_zip function works
  expect_equal(read_zip(file_zip,  file_bin_rel, raw(1), 5, 4),
               as.raw(1:16)[6:9]) # check if the read_zip function works
})

file.remove(file_zip, file_bin) # remove large files
