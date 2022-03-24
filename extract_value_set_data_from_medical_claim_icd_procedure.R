################################################################################
### extract value set data from medical claim_icd_procedure
### version controlled by git
################################################################################

## setup tools and packages
library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "dplyr", "readxl", "ggplot2", "microbenchmark")
need.packages(need_pkgs)
source("functions.R")
source("docs.R")

## directory of raw data and output
inDir <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/merged_data/')
outDir <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/icd_procedure/")
load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2_vs.regexp.rda')

## matching MC_ICD_PROCEDURE file
match_regexp(
  vs = vs.proc.icd.regexp, fclaim = "medical_claim_icd_procedure.csv",
  col.match = 'icd_procedure_code',
  chunksize = 0, col.out = NULL,
  test = F, verbose = T, towrite = T, header = T, sep = ',',
  dir_raw = inDir, dir_out = outDir
)


# special case for PPP Code
load("E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/long_vs_icd_proc.rda")
for (lvs in vs.long[1, ]$`Value Set Name`) {
  vs.tmp <- filter(vs.long, `Value Set Name` %in% lvs)
  tmp <- strsplit(vs.tmp$code_regexp, split='|', fixed=T)[[1]]
  nn <- ceiling(length(tmp) / 500)
  vs.tmp <- data.frame(`Value Set Name` = rep(lvs, nn), code_regexp=NA)
  for(ii in 1:nn){
    vs.tmp$code_regexp[ii] <- paste(tmp[((ii-1)*500+1) : (ii*500)], collapse='|')
    vs.tmp$`Value Set Name`[ii] <- paste(lvs, ii)
  }
  match_regexp( 
    vs = vs.tmp, fclaim = "medical_claim_icd_procedure.csv",
    col.match = 'icd_procedure_code',
    chunksize = 0, col.out = NULL,
    test = F, verbose = T, towrite = T, header = T, sep = ',',
    dir_raw = inDir, dir_out = outDir
  )
}
