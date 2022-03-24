################################################################################
### import medical claim data and sort by month
### version controlled by git
################################################################################
## running whole script need around 5 hours on server at 10/10/2018


## setup tools and packages
library(methods)
source("E:/CT_APCD/ct-apcd/R/utils.R")
source("functions.R")
need_pkgs <- c("data.table", "bit64","tools","dplyr")
need.packages(need_pkgs)


## directory of raw data and output
inDir <- input_dir("E:/CT_APCD/Commercial and Medicare data 5-24-18/")
outDir <- output_dir("E:/CT_APCD/shared/intermediate_data/data_sorted_by_month/")


## setup a function
SortByMonth <- function(bigfile, dir.raw = inDir, 
                        dir.out = NULL, chunksize = 1e6, sep = '|', header = T){
  ## preparation part
  if(is.null(dir.out) | is.null(dir.raw)) stop('Need "dir.out" and "dir.raw" in function "SortByMonth"!')
  if(!dir.exists(dir.raw)) stop('Need "dir.raw" exist! ')
  if(!dir.exists(dir.out)) dir.create(dir.out)
  flog <- paste0(dir.out, 'log.txt')
  ## end of preparation part 
  ## main Part 
  con <- file(paste0(dir.raw, bigfile), open="r")
  nolines <- 0 
  nn <- 1
  mycat('chunksize = ', chunksize, '\n')
  if (header){
    fhead<- scan(con, nlines=1, what = character(), sep=sep)
  }
  readnlines <- chunksize
  while(readnlines == chunksize){
    datachunk <- as.data.table(fread(con, nrows=chunksize, header = FALSE, sep = sep, quote = "", fill = TRUE, 
                                          colClasses = 'character', encoding = 'UTF-8'))
    colnames(datachunk) <- fhead
    readnlines <- dim(datachunk)[1]
    mycat('...')
    nolines <- nolines + readnlines
    mycat(nn)
    mycat('\n # lines = ', nolines, '\n')
    datachunk[, month := format(as.Date(as.character(first_service_dt), format = "%m/%d/%Y"), "%Y_%m")]
    datachunk[, 
              fwrite(.SD, 
                          file = file.loc <- paste0(dir.out, '/', "MEDICAL_",month, ".csv"), 
                          append = TRUE, 
                          sep = ",", 
                          col.names = FALSE,
                          row.names = FALSE),
              by = .(month)]
    nn <- nn + 1
  } 
  close(con) 
  mycat('\n #', nn, 'chunk \n # lines = ', nolines, '\n')
}
## end of function

## start to sort
for (year in 2017:2012){
  bigfile <- paste0('MEDICAL_', year, '.txt')
  dir.raw <- inDir
  dir.out <- outDir
  sep = '|'
  ## initiate empty table with only colnames.
  if (!dir.exists(dir.out)) dir.create(dir.out)
  con1 <- file(paste0(dir.raw, bigfile), open = 'r')
  fhead <- scan(con1, nlines= 1, what = character(), sep = sep)
  close(con1)
  for (month in sprintf("%02d", 1:12))
    fwrite(as.data.table(matrix(fhead, nrow = 1)), paste0(dir.out, "MEDICAL_",year, '_', month, ".csv"), 
                sep = ",", row.names = FALSE, col.names = FALSE, eol = "\n")
  SortByMonth(bigfile = paste0('MEDICAL_', year, '.txt'),
              dir.out = dir.out,
              chunksize = 4e5,
              sep = sep, 
              header = T)
}