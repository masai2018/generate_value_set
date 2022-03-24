################################################################################
### extract value set data from medical claim diagnosis
### version controlled by git
################################################################################
## "0006" will be replaced by .sh file

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
inDir <- input_dir('E:/CT_APCD/Sai/medicaid/medicaid_files/modified_data/dgs_sub_files/')
outDir <- output_dir("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis/")
# load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/bunch2.regexp.rda')
load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2_vs.regexp.rda')
# load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2017_vs.regexp.rda') #finfo


# match_regexp(
#   vs=vs.dx.icd.regexp.bunch2[c(-89, -90, -102, -68, -69), ], fclaim = "MEDICAL_CLAIM_DIAGNOSIS_0006.txt",
#   col.match='DIAGNOSIS_CODE',
#   chunksize = 0, col.out = NULL,
#   test=F, verbose=T, towrite=T, header=T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# in bunch2, already have 89, 90, 102, 68, 69
# match_regexp(
#   vs=vs.other.regexp.bunch2, fclaim = "MEDICAL_CLAIM_DIAGNOSIS_0006.txt",
#   col.match='DIAGNOSIS_CODE',
#   chunksize = 0, col.out = NULL,
#   test=F, verbose=T, towrite=T, header=T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# match_regexp(
#   vs=vs.proc.icd.regexp.bunch2, fclaim = "MEDICAL_CLAIM_DIAGNOSIS_0006.txt",
#   col.match='DIAGNOSIS_CODE',
#   chunksize = 0, col.out = NULL,
#   test=F, verbose=T, towrite=T, header=T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# match_regexp(
#   vs=vs.proc.other.regexp.bunch2, fclaim = "MEDICAL_CLAIM_DIAGNOSIS_0006.txt",
#   col.match='DIAGNOSIS_CODE',
#   chunksize = 0, col.out = NULL,
#   test=F, verbose=T, towrite=T, header=T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# match_regexp(
#   vs=vs.system.regexp.bunch2, fclaim = "MEDICAL_CLAIM_DIAGNOSIS_0006.txt",
#   col.match='DIAGNOSIS_CODE',
#   chunksize = 0, col.out = NULL,
#   test=F, verbose=T, towrite=T, header=T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# match_regexp(
#   vs=vs.ubrev.regexp.bunch2, fclaim = "MEDICAL_CLAIM_DIAGNOSIS_0006.txt",
#   col.match='DIAGNOSIS_CODE',
#   chunksize = 0, col.out = NULL,
#   test=F, verbose=T, towrite=T, header=T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )

vs.dx.icd.regexp[36, 1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
match_regexp(
  vs = vs.dx.icd.regexp, fclaim = "ct_medical_claim_diagnosis_0006.txt",
  col.match ='DIAGNOSIS_CODE',
  chunksize = 0, col.out = NULL,
  test = F, verbose = T, towrite = T, header = T, sep = '|',
  dir_raw = inDir, dir_out = outDir
)
# special case for Acute Condition Code
load("E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/dx.long.regexp.rda")
lvs <- vs.long$`Value Set Name`
vs.tmp <- filter(vs.long, `Value Set Name` %in% lvs)
tmp <- strsplit(vs.tmp$code_regexp, split = '|', fixed = T)[[1]]
nn <- ceiling(length(tmp)/500)
vs.tmp <- data.frame(`Value Set Name` = rep(lvs, nn), code_regexp=NA)
for(ii in 1:nn){
  vs.tmp$code_regexp[ii] <- paste(tmp[((ii - 1)*500 + 1) : (ii*500)], collapse='|')
  vs.tmp$`Value Set Name`[ii] <- paste(lvs, ii)  # added by Sai
}
match_regexp(
  vs = vs.tmp, fclaim = "ct_medical_claim_diagnosis_0006.txt",
  col.match = 'DIAGNOSIS_CODE',
  chunksize = 0, col.out = NULL,
  test = F, verbose = T, towrite = T, header = T, sep='|',
  dir_raw = inDir, dir_out = outDir
)

# # special case for Chronic Respiratory Conditions Due To Fumes/Vapors
# vs.dx.icd.regexp[36, 1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
# match_regexp(
#   vs = vs.dx.icd.regexp[36, ], fclaim = "MEDICAL_CLAIM_DIAGNOSIS.txt",
#   col.match ='DIAGNOSIS_CODE',
#   chunksize = 0, col.out = NULL,
#   test = F, verbose = T, towrite = T, header = T, sep = '|',
#   dir_raw = inDir, dir_out = outDir
# )
