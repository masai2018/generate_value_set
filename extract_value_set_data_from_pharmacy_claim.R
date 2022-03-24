################################################################################
### extract value set data from pharmacy claim
### version controlled by git
################################################################################
## "yyyy" will be replaced by .sh file

## setup tools and packages
library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
source("functions.R")
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
need_pkgs <- c("data.table", "bit64","tools","dplyr","readxl","ggplot2","microbenchmark")
need.packages(need_pkgs)

## directory of raw data and output
inDir <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/merged_data/')
outDir <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/pharmacy/")

## load(file='E:/CT APCD/Sai/intermediate_data/new_value_set/2018_v2_vs.regexp.rda')
## load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/myfile.rda')
# fvs <- read_xlsx('E:/CT APCD/ToShare/2018 NDC lists/NDClist_2018.xlsx')
# fvs <- fvs[,1:2]
# colnames(fvs) <- c('Value Set Name', 'Code')
# fvs.regexp <- get_regexp(fvs)
# save(fvs.regexp, file = "E:/CT APCD/ToShare/intermediate_data/APCD_modified/hedis/phar.regexp.rda")
load(file = 'E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/phar.regexp.rda')
load("E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/phar.long.regexp.rda")
vs.long[2, 1] <- "ACE Inhibitor ARB Medications"
## extract Drug code
for(yr in c(2019:2012, 2020)){
  # match_regexp(
  #   vs = fvs.regexp, fclaim = paste0("pharmacy_", yr, ".csv"),
  #   col.match = 'national_drug_code',
  #   chunksize = 0, col.out = NULL, test = F, verbose = T, towrite = T, 
  #   header = T, sep = ',',
  #   dir_raw = inDir, dir_out = outDir, long.vs.nchunk = 1000)
  
  for (lvs in vs.long$`Value Set Name`){
    vs.tmp <- filter(vs.long, `Value Set Name` %in% lvs)
    tmp <- strsplit(vs.tmp$code_regexp, split = '|', fixed = T)[[1]]
    nn <- ceiling(length(tmp)/500)
    vs.tmp <- data.frame(`Value Set Name` = rep(lvs, nn), code_regexp=NA)
    for(ii in 1:nn){
      vs.tmp$code_regexp[ii] <- paste(tmp[((ii - 1)*500 + 1) : (ii*500)],
                                      collapse = '|')
      vs.tmp$`Value Set Name`[ii] <- paste(lvs, ii)  # added by Sai
    }
    match_regexp(
      vs = vs.tmp, fclaim = paste0("pharmacy_", yr, ".csv"),
      col.match = 'national_drug_code',
      chunksize = 0, col.out = NULL, test = F, verbose = T, towrite = T,
      header = T , sep = ',',
      dir_raw = inDir, dir_out = outDir)
  }
}