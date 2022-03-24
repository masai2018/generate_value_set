################################################################################
### Type of bill code
################################################################################

## setup tools and packages
library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "tidyverse",
               "dplyr", "readxl", "ggplot2", "microbenchmark")
need.packages(need_pkgs)
source("functions.R")
source("docs.R")
memory.limit()

## directory of raw data and output
inDir <- input_dir('E:/CT_APCD/LDS_12-5-19/')
outDir <- output_dir("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim/")

vs.tob <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2.csv",
                # header = "TRUE",
                encoding = "UTF-8",
                colClasses = "character",
                select = c("Value Set Name",
                           "Code System",
                           "Code"))[`Code System` == "UBTOB"]
vs.tob[, Code := gsub("(^|[^0-9])0+", "\\1", vs.tob$Code, perl = TRUE)] 
vs.tob <- vs.tob[, Code := substr(vs.tob$Code, 0, 2)] %>% unique(use.key = FALSE)
vs.tob.regexp <- get_regexp(vs.tob)


match_regexp(
  vs = vs.tob.regexp, fclaim = "ct_medical_split_2015.txt",
  col.match = 'TYPE_OF_BILL_CODE',
  chunksize = 0, col.out = NULL,
  test = F, verbose = T, towrite = T, header = T, sep = '|',
  dir_raw = inDir, dir_out = outDir
)