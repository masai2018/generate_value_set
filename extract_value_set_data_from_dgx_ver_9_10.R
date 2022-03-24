################################################################################
### extract value set data from medical claim
### version controlled by git
################################################################################
## .sh file will replace "yyyy" by "2012-2017"

## setup tools and packages
library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
source("functions.R")
need_pkgs <- c("data.table", "bit64", "tools", "dplyr",
               "readxl", "ggplot2", "microbenchmark")
need.packages(need_pkgs)
source("docs.R")

## directory of raw data and output
inDir <- input_dir('E:/CT_APCD/Sai/lds2021/lds2021file/merged_data/')
# inDir <- input_dir('E:/CT_APCD/Uconn_extract_20180521_12312017/')
outDir9 <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/diagnosis_9/")
outDir10 <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/diagnosis_10/")
load(file='E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2_vs.regexp.rda')
vs.dx.icd.regexp[36,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
vs.dx.icd9.regexp[17,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
vs.dx.icd10.regexp[35,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"

for(yr in 2019:2014){
  dgx <- fread(paste0(inDir, "diagnosis_", yr, ".csv"), colClasses = "character")
  dgx9 <- dgx[icd_version_ind == 9]
  dgx10 <- dgx[icd_version_ind == 0]
  rm(dgx)
  gc()
  if(dim(dgx9)[1] > 0){
    fwrite(dgx9, paste0(inDir, "diagnosis_9_", yr, ".csv"))
    rm(dgx9)
    gc()
    match_regexp(
      vs = vs.dx.icd9.regexp, fclaim = paste0("diagnosis_9_", yr, ".csv"),
      col.match ='diagnosis_code',
      chunksize = 0, col.out = NULL,
      test = F, verbose = T, towrite = T, header = T, sep = ',',
      dir_raw = inDir, dir_out = outDir9
    )
  }
  
  if(dim(dgx10)[1] > 0){
    fwrite(dgx10, paste0(inDir, "diagnosis_10_", yr, ".csv"))
    rm(dgx10)
    gc()
    match_regexp(
      vs = vs.dx.icd10.regexp, fclaim = paste0("diagnosis_10_", yr, ".csv"),
      col.match ='diagnosis_code',
      chunksize = 0, col.out = NULL,
      test = F, verbose = T, towrite = T, header = T, sep = ',',
      dir_raw = inDir, dir_out = outDir10
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
      vs = vs.tmp, fclaim = paste0("diagnosis_10_", yr, ".csv"),
      col.match = 'diagnosis_code',
      chunksize = 0, col.out = NULL,
      test = F, verbose = T, towrite = T, header = T, sep=',',
      dir_raw = inDir, dir_out = outDir10
    )
  }
}


outDir9_2 <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/medical_claim_dgx_9/")
outDir10_2 <- output_dir("E:/CT_APCD/Sai/lds2021/lds2021file/value_sets/medical_claim_dgx_10/")

for(yr in 2019:2014){
  dgx <- fread(paste0(inDir, "medical_", yr, ".csv"), colClasses = "character")
  dgx9 <- dgx[icd_version_ind == 9]
  dgx10 <- dgx[icd_version_ind == 0]
  rm(dgx)
  gc()
  if(dim(dgx9)[1] > 0){
    fwrite(dgx9, paste0(inDir, "medical_diagnosis_9_", yr, ".csv"))
    rm(dgx9)
    gc()
    match_regexp(
      vs = vs.dx.icd9.regexp, fclaim = paste0("medical_diagnosis_9_", yr, ".csv"),
      col.match ='diagnosis_code',
      chunksize = 0, col.out = NULL,
      test = F, verbose = T, towrite = T, header = T, sep = ',',
      dir_raw = inDir, dir_out = outDir9_2
    )
  }
  
  if(dim(dgx10)[1] > 0){
    fwrite(dgx10, paste0(inDir, "medical_diagnosis_10_", yr, ".csv"))
    rm(dgx10)
    gc()
    match_regexp(
      vs = vs.dx.icd10.regexp, fclaim = paste0("medical_diagnosis_10_", yr, ".csv"),
      col.match ='diagnosis_code',
      chunksize = 0, col.out = NULL,
      test = F, verbose = T, towrite = T, header = T, sep = ',',
      dir_raw = inDir, dir_out = outDir10_2
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
      vs = vs.tmp, fclaim = paste0("medical_diagnosis_10_", yr, ".csv"),
      col.match = 'diagnosis_code',
      chunksize = 0, col.out = NULL,
      test = F, verbose = T, towrite = T, header = T, sep=',',
      dir_raw = inDir, dir_out = outDir10_2
    )
  }
}
