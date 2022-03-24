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
outDir <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/medical_claim_modifier/")

load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2_vs.regexp.rda')
vs.modifier <- fread("E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2.csv",
                # header = "TRUE",
                encoding = "UTF-8",
                colClasses = "character",
                select = c("Value Set Name",
                           "Code System",
                           "Code"))[`Value Set Name` %in% 
                                      vs.proc.other.regexp[c(25, 39, 88, 144, 155), ]$`Value Set Name`]
# vs.tob[, Code := gsub("(^|[^0-9])0+", "\\1", vs.tob$Code, perl = TRUE)] 
# vs.tob <- vs.tob[, Code := substr(vs.tob$Code, 0, 2)] %>% unique(use.key = FALSE)
# vs.cat.regexp <- get_regexp(vs.cat)
vs.modifier.name <- unique(vs.modifier$`Value Set Name`)

for(yr in 2020:2014){
  dat <- fread(paste0(inDir, "medical_", yr, ".csv"),
               header = TRUE, encoding = "UTF-8",
               colClasses = "character")
  for(name in vs.modifier.name){
    for (colmatch in c('procedure_modifier_code_1', 'procedure_modifier_code_1',
                       'procedure_modifier_code_1', 'procedure_modifier_code_1')){
      tmp1 <- vs.modifier[`Value Set Name` %in% name][, .(Code)]
      setnames(tmp1, "Code", colmatch)
      tmp2 <- dat[tmp1, on = colmatch, nomatch = 0]
      fwrite(tmp2, paste0(outDir, "medical_", yr, "#", colmatch,
                          '#', name, ".csv"))
      cat(paste0(yr, ' ', name, ' ',
                 colmatch, "done at ", 
                 Sys.time(), "\n"))
    }
  }
}