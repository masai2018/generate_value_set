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
inDir <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/merged_data/')
outDir <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/medical_claim_CPT_CAT_II/")

vs.cat <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2.csv",
                # header = "TRUE",
                encoding = "UTF-8",
                colClasses = "character",
                select = c("Value Set Name",
                           "Code System",
                           "Code"))[`Code System` == "CPT-CAT-II"]
# vs.tob[, Code := gsub("(^|[^0-9])0+", "\\1", vs.tob$Code, perl = TRUE)] 
# vs.tob <- vs.tob[, Code := substr(vs.tob$Code, 0, 2)] %>% unique(use.key = FALSE)
vs.cat.regexp <- get_regexp(vs.cat)

for(yr in 2020:2014){
  match_regexp(
    vs = vs.cat.regexp, fclaim = paste0("medical_", yr, ".csv"),
    col.match = 'procedure_code',
    chunksize = 0, col.out = NULL,
    test = F, verbose = T, towrite = T, header = T, sep = ',',
    dir_raw = inDir, dir_out = outDir
  )
}
