################################################################################
### extract value set data from medical claim
### version controlled by git
################################################################################
## .sh file will replace "2015" by "2012-2017"

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
inDir <- input_dir('E:/CT_APCD/LDS_12-5-19/')
# inDir <- input_dir('E:/CT_APCD/Uconn_extract_20180521_12312017/')
outDir <- output_dir("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim/")
load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2_vs.regexp.rda')
## load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/myfile.rda') #finfo

# ## grab "special" vs.regexp with more than 999 chunks.
# vs.long <- filter(vs.system.regexp, nchunk >= 1000)
# vs_long_name <- vs.long$`Value Set Name`
# ## [1] c('Acute Condition', 'ED Procedure Code', 'Potentially Planned Procedures')
# 
# ## part 1 UBREV code
# 
# vs.ubrev.regexp[c(-1, -23, -32), ]
# match_regexp(
#   vs = vs.ubrev.regexp[c(-1, -23, -32), ], fclaim = "ct_medical_split_2015.txt",
#   col.match = 'REVENUE_CODE',
#   chunksize = 0, col.out = NULL,
#   test = F, verbose = T, towrite = T, header = T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# # ## part 2 CPT, HCPCS
# match_regexp(
#   vs = vs.proc.other.regexp[c(123:155, 157:168), ], fclaim = "ct_medical_split_2015.txt",
#   col.match = 'PROCEDURE_CODE',
#   chunksize = 0, col.out = NULL,
#   test = F, verbose = T, towrite = T, header = T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# ## part 3 ICD10PCS, ICD9PCS
# match_regexp(
#   vs = vs.proc.icd.regexp, fclaim = "ct_medical_split_2015.txt",
#   col.match = 'ICD_PROCEDURE_CODE',
#   chunksize = 0, col.out = NULL,
#   test = F, verbose = T, towrite = T, header = T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# # special case for ED Procedure Code
# lvs <- vs.long[2, ]$`Value Set Name`
# vs.tmp <- filter(vs.long, `Value Set Name` %in% lvs)
# tmp <- strsplit(vs.tmp$code_regexp, split='|', fixed=T)[[1]]
# nn <- ceiling(length(tmp)/500)
# vs.tmp <- data.frame(`Value Set Name` = rep(lvs, nn), code_regexp=NA)
# for(ii in 1:nn){
#   vs.tmp$code_regexp[ii] <- paste(tmp[((ii-1)*500+1) : (ii*500)], collapse='|')
#   vs.tmp$`Value Set Name`[ii] <- paste(lvs, ii)  # added by Sai
# }
# match_regexp(
#   vs = vs.tmp, fclaim = "ct_medical_split_2015.txt",
#   col.match = 'PROCEDURE_CODE',
#   chunksize = 0, col.out = NULL,
#   test = F, verbose = T, towrite = T, header = T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# # special case for PPP Code
# lvs <- vs.long[3, ]$`Value Set Name`
# vs.tmp <- filter(vs.long, `Value Set Name` %in% lvs)
# tmp <- strsplit(vs.tmp$code_regexp, split='|', fixed=T)[[1]]
# nn <- ceiling(length(tmp) / 500)
# vs.tmp <- data.frame(`Value Set Name` = rep(lvs, nn), code_regexp=NA)
# for(ii in 1:nn){
#   vs.tmp$code_regexp[ii] <- paste(tmp[((ii-1)*500+1) : (ii*500)], collapse='|')
#   vs.tmp$`Value Set Name`[ii] <- paste(lvs, ii)
# }
# match_regexp(
#   vs = vs.tmp, fclaim = "ct_medical_split_2015.txt",
#   col.match = 'ICD_PROCEDURE_CODE',
#   chunksize = 0, col.out = NULL,
#   test = F, verbose = T, towrite = T, header = T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# ## special case for Acute Inpatient  Inpatient Stay Nonacute Inpatient Stay
# ## removed leading zero
# 
# load("E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/vs.cost.regexp.rda")
# match_regexp(
#   vs = vs.cost.regexp, fclaim = "ct_medical_split_2015.txt",
#   col.match = 'REVENUE_CODE',
#   chunksize = 0, col.out = NULL,
#   test = F, verbose = T, towrite = T, header = T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# match_regexp(
#   vs = vs.ubrev.regexp[c(1, 23, 32), ], fclaim = "ct_medical_split_2015.txt",
#   col.match = 'REVENUE_CODE',
#   chunksize = 0, col.out = NULL,
#   test = F, verbose = T, towrite = T, header = T, sep='|',
#   dir_raw = inDir, dir_out = outDir
# )
# 
# ## some other value sets for Abby ?????
# 
# 
# # vs.system.regexp[426:430, 1]
# # match_regexp(
# #   vs = vs.system.regexp[426:430, ], fclaim = "ct_medical_split_2015.txt",
# #   col.match = 'icd_procedure_code',
# #   chunksize = 1e7, col.out = NULL,
# #   test = F, verbose = T, towrite = T, header = T, sep='|',
# #   dir_raw = inDir, dir_out = outDir
# # )
# 
# ##  special cases for Bilateral Modifier CPT CAT II Modifier Left Modifier
# 
# ## Right Modifier Telehealth Modifier
# vs.proc.other.regexp[c(25, 39, 88, 144, 155), 1]
# for (colmatch in c('PROCEDURE_MODIFIER_CODE_1', 'PROCEDURE_MODIFIER_CODE_2',
#             'PROCEDURE_MODIFIER_CODE_3', 'PROCEDURE_MODIFIER_CODE_4')){
#   match_regexp(
#     vs = vs.proc.other.regexp[c(25, 39, 88, 144, 155), ],
#     fclaim = "ct_medical_split_2015.txt",
#     col.match = colmatch,
#     chunksize = 0, col.out = NULL,
#     test = F, verbose = T, towrite = T, header = T, sep='|',
#     dir_raw = inDir, dir_out = outDir
#   )
# }

##  dgx from medical claim
outDir2 <- output_dir("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx/")
vs.dx.icd.regexp[36, 1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
match_regexp(
  vs = vs.dx.icd.regexp, fclaim = "ct_medical_split_2015.txt",
  col.match ='DIAGNOSIS_CODE',
  chunksize = 0, col.out = NULL,
  test = F, verbose = T, towrite = T, header = T, sep = '|',
  dir_raw = inDir, dir_out = outDir2
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
  vs = vs.tmp, fclaim = "ct_medical_split_2015.txt",
  col.match = 'DIAGNOSIS_CODE',
  chunksize = 0, col.out = NULL,
  test = F, verbose = T, towrite = T, header = T, sep='|',
  dir_raw = inDir, dir_out = outDir2
)