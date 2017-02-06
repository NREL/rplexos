seek_zip <- function(zip, file, offset, n){
  system(paste0(file.path(unzip_bin_path(),'sh'),
                ' -c "',
                file.path(unzip_bin_path(),'unzip'),' -p ',zip,' ',file,' | ',
                file.path(unzip_bin_path(),'dd'),' of=tempfile.bin bs=1c skip=',offset,'c count=',n,'c"'))
  readBin("tempfile.bin", raw(1), n = 16)
}

unzip_bin_path <- function(){
  system.file("extdata", "zip_bin", package = "rplexos")
}

# file_zip <- "file.zip"
# file_bin <- "file.bin"
# writeBin(rep(as.raw(1:255),1e6*4*2), file_bin)
# readBin(file_bin, raw(1), n = 16)
# zip(file_zip, file_bin)
# readBin(file_zip, raw(1), n = 16)
# 
# seek_zip(file_zip,  file_bin, 5, 4)
