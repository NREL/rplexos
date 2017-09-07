read_zip <- function(zip, file, what, offset = 0L, n = 1L, size = NA_integer_, signed = TRUE, endian = .Platform$endian){
  zip <- gsub('\\\\','/',zip)
  file <- gsub('\\\\','/',file)
  size_n <- 1L
  if(!is.na(size) & is.integer(size)){
    size_n <- size
  }
  if(.Platform$OS.type != 'windows'){
    stop('The on-the-fly mode currently only works for Windows.')
  }
  suppressWarnings(
    system(paste0(file.path(unzip_bin_path(),'sh'),
                  ' -c "',
                  file.path(unzip_bin_path(),'unzip'),' -p ',zip,' ',file,' | ',
                  file.path(unzip_bin_path(),'dd'),' of=tempfile.bin bs=1c skip=',offset,'c count=',n * size_n,'c"'),
           show.output.on.console = F))
  out <- readBin(con = "tempfile.bin", what = what, n = n, size = size, signed = signed, endian = endian)
  file.remove("tempfile.bin")
  return(out)
}

unzip_bin_path <- function(){
  system.file("executables", "win32", "zip_bin", package = "rplexos")
}

# file_zip <- "file.zip"
# file_bin <- "file.bin"
# writeBin(rep(as.raw(1:255),1e6*4*2), file_bin)
# readBin(file_bin, raw(1), n = 16)
# zip(file_zip, file_bin)
# readBin(file_zip, raw(1), n = 16)
# 
# seek_zip(file_zip,  file_bin, 5, 4)
