################################################################################
### collapse medical diagnosis intermediate file
### version controlled by git
################################################################################

## setup tools and packages
library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
source("functions.R")
need_pkgs <- c("data.table", "bit64", "tools", "dplyr",
               "readxl", "ggplot2", "microbenchmark")
need.packages(need_pkgs)
source("docs.R")
memory.limit()

## directory of raw data and output
inDir <- input_dir("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure/")
outDir <- output_dir("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure/")
load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2_vs.regexp.rda')
vs.long <- filter(vs.system.regexp, nchunk >= 1000)
vs_long_name <- vs.long$`Value Set Name`
## processing
for (vs_i in vs.long$`Value Set Name`[c(3)]){
  vs.tmp <- filter(vs.long, `Value Set Name` %in% vs_i)
  tmp <- strsplit(vs.tmp$code_regexp, split='|', fixed=T)[[1]]
  n.chunk <- ceiling(length(tmp) / 500)
  fdir <- paste0(inDir, "medical_claim_icd_procedure#ICD_PROCEDURE_CODE#",
                 vs_i,  " ", sprintf("%01d", 1:n.chunk), ".csv")
  for (fdir_i in fdir){
    f_i <- fread(fdir_i, header = TRUE, colClasses = 'character', encoding = 'UTF-8')
    fwrite(f_i, paste0(outDir, "medical_claim_icd_procedure#ICD_PROCEDURE_CODE#", vs_i, ".csv"), append = TRUE)
    cat(paste0(fdir_i, " done!\n"))
  }
  Sys.time()
}
