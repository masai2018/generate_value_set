################################################################################
### extract value set data from header
### version controlled by git
################################################################################

## setup tools and packages
library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
source("functions.R")
source("docs.R")
need_pkgs <- c("data.table", "bit64", "tools", "dplyr", "readxl", "ggplot2", "microbenchmark")
need.packages(need_pkgs)

## directory of raw data and output
inDir <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/merged_data/')
# inDir <- input_dir('E:/CT_APCD/Uconn_extract_20180521_12312017/')
outDir <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/medical_claim/")
load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2_vs.regexp.rda')
load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/') #finfo

## matching Header file
for(yr in c(2019:2014, 2020)){
  match_regexp(
    vs=vs.proc.icd.regexp, fclaim =paste0("medical_claim_header_", yr, ".csv"),
    col.match='icd_procedure_code',
    chunksize=0, col.out = NULL,
    test=F, verbose=T, towrite=T, header=T, sep=',',
    dir_raw = inDir, dir_out = outDir
  )
}
