################################################################################
### some special cases for extracting calue set
### version controlled by git
################################################################################

## setup tools and packages
## library(methods)
## source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
## source("functions.R")
## source("docs.R")
## need_pkgs <- c("data.table", "bit64", "tools", "dplyr", "readxl", "ggplot2", "microbenchmark")
## need.packages(need_pkgs)

## directory of raw data and output
## inDir <- input_dir('E:/CT_APCD/Commercial_and_Medicare_data_5-24-18/')
## outDir <- output_dir("E:/CT_APCD/shared/intermediate_data/APCD_modified/medical_claim_icd_procedure_by_value_set/")
## load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/2018_v2_vs.regexp.rda')
## load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/myfile.rda')
## fvs <- fread('E:/CT_APCD/shared/intermediate_data/APCD_modified/2017_hedis_value_set_NDC.csv')

## Add some special codes and value sets
## vs.tmp <- data.frame(code_regexp = c('88155', '9146', 'Z124', 'G940[2-7]'),
##                      `Value Set Name` = c('Cytopathology',
##                                           'Papanicolaou Smear',
##                                           'CCS',
##                                           'Follow Up') )
## vs.tmp$code_regexp <- as.character(vs.tmp$code_regexp)
## colnames(vs.tmp)[2] <- 'Value Set Name'
## vs.tmp$`Value Set Name` <- as.character(vs.tmp$`Value Set Name`)

# CSS
## vs.CCS <- vs.tmp[3,]
## colnames(vs.CCS)[1] <- 'Code'
## match_regexp(vs=vs.CCS, fclaim=finfo$fname[9], col.match='DIAGNOSIS_CODE', chunksize=1e6,
##              col.out=NULL,
##              test= F, verbose=T, towrite=T, header=T, sep='|',
##              dir_raw = inDir,
##              dir_out = outDir, long.vs.nchunk = 1000)

# Papanicolaou Smear
# vs.Pap <- vs.tmp[2,]
# colnames(vs.Pap)[1] <- 'Code'
# match_regexp(vs=vs.Pap, fclaim=finfo$fname[10], col.match='ICD_PROCEDURE_CODE', chunksize=0,
#              col.out=NULL,
#              test=F, verbose=T, towrite=T, header=T, sep='|',
#              dir_raw = inDir,
#              dir_out = outDir, long.vs.nchunk = 1000)

# Cytopathology and Follow Up
# vs.proc <- vs.tmp[c(1,4),]
# match_regexp(vs=vs.proc, fclaim="medical_yyyy.txt", col.match='PROCEDURE_CODE', chunksize=0,
#              col.out= mc.col.out,
#              test=F, verbose=T, towrite=T, header=T, sep='|',
#              dir_raw = inDir,
#              dir_out = outDir, long.vs.nchunk = 1000)
