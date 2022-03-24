################################################################################
### collapse medical diagnosis intermediate file
### version controlled by git
################################################################################

## setup tools and packages
library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "dplyr")
need.packages(need_pkgs)
source("functions.R")
source("docs.R")


## directory of raw data and output
inDir <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/diagnosis_10/')
outDir <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/diagnosis_10/")
# collapse mc dgx
load("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx/long_vs.rda")
vs_long_name <- vs.long$`Value Set Name`
## processing
for(year in 2018:2014){
  for (vs_i in vs.long$`Value Set Name`){
    vs.tmp <- filter(vs.long, `Value Set Name` %in% vs_i)
    tmp <- strsplit(vs.tmp$code_regexp, split='|', fixed=T)[[1]]
    n.chunk <- ceiling(length(tmp) / 500)
    fdir <- paste0(inDir, "diagnosis_10_", year, "#diagnosis_code#",
                   vs_i,  " ", sprintf("%01d", 1:n.chunk), ".csv")
    for (fdir_i in fdir){
      f_i <- fread(fdir_i, header = TRUE, colClasses = 'character', encoding = 'UTF-8')
      fwrite(f_i, paste0(outDir, "diagnosis_10_", year, "#diagnosis_code#",
                         vs_i, ".csv"), append = TRUE)
      cat(paste0(fdir_i, " done!\n"))
    }
    Sys.time()
  }
}
