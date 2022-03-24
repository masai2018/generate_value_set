library(methods)
source("E:/CT_APCD/gitrepo/ct-apcd/R/utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "dplyr", "readxl", "ggplot2", "microbenchmark")
need.packages(need_pkgs)
source("functions.R")
source("docs.R")

load("E:/CT_APCD/shared/intermediate_data/APCD_modified/hedis/2018_v2_vs.regexp.rda")


# icd from medical claim
for (vsname in vs.proc.icd.regexp$`Value Set Name`) {
  for(yr in 2015:2018){
    medical_crt_icd(vs.name = vsname,
                    year = yr,
                    dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim",
                    dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_crt10",
                    dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_crt9")
  }

}

# icd from medical claim special case PPP
for(yr in 2015:2018){
  medical_crt_icd_proc_long2(vs.name = "Potentially Planned Procedures", year = yr)
}
load("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/backup/icd_procedure/long_vs.rda")
for(year in 2015:2018){
  for (vs_i in vs.long$`Value Set Name`){
    vs.tmp <- filter(vs.long, `Value Set Name` %in% vs_i)
    tmp <- strsplit(vs.tmp$code_regexp, split='|', fixed=T)[[1]]
    n.chunk <- ceiling(length(tmp) / 500)
    fdir <- paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_crt10/",
                   "medical_", year, "#ICD_PROCEDURE_CODE#",
                   vs_i,  " ", sprintf("%01d", 1:n.chunk), ".csv")
    for (fdir_i in fdir){
      f_i <- fread(fdir_i, header = TRUE, colClasses = 'character', encoding = 'UTF-8')
      fwrite(f_i, paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_crt10/",
                         "medical_", year, "#ICD_PROCEDURE_CODE#", vs_i, ".csv"), append = TRUE)
      cat(paste0(fdir_i, " done!\n"))
    }
    Sys.time()
  }
}

# dx from medical claim
vs.dx.icd.regexp[36,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
vs.dx.icd9.regexp[17,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
vs.dx.icd10.regexp[35,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
for (vsname in vs.dx.icd.regexp$`Value Set Name`) {
  for(yr in 2015:2018){
    medical_crt_dx_icd2(vs.name = vsname,
                    year = yr,
                    dir.raw = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx",
                    dir.crt10 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx_crt10",
                    dir.crt9 = "E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx_crt9")
  }
  
}


## medical dgx procedure special case acute condition
for(yr in 2015:2018){
  medical_crt_dx_icd_long2(vs.name = "Acute Condition", year = yr)
}
load("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx/long_vs.rda")
for(year in 2015:2018){
  for (vs_i in vs.long$`Value Set Name`){
    vs.tmp <- filter(vs.long, `Value Set Name` %in% vs_i)
    tmp <- strsplit(vs.tmp$code_regexp, split='|', fixed=T)[[1]]
    n.chunk <- ceiling(length(tmp) / 500)
    fdir <- paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx_crt10/",
                   "medical_", year, "#DIAGNOSIS_CODE#",
                   vs_i,  " ", sprintf("%01d", 1:n.chunk), ".csv")
    for (fdir_i in fdir){
      f_i <- fread(fdir_i, header = TRUE, colClasses = 'character', encoding = 'UTF-8')
      fwrite(f_i, paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/medical_claim_dgx_crt10/",
                         "medical_", year, "#DIAGNOSIS_CODE#", vs_i, ".csv"), append = TRUE)
      cat(paste0(fdir_i, " done!\n"))
    }
    Sys.time()
  }
}

## dx
for (vsname in vs.dx.icd.regexp$`Value Set Name`[-36]) {
    medical_crt_dx_icd(vs.name = vsname)

}

## dx special case "Chronic Respiratory Conditions Due To Fumes/Vapors"
vs.dx.icd.regexp[36,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
vs.dx.icd9.regexp[17,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
vs.dx.icd10.regexp[35,1] <- "Chronic Respiratory Conditions Due To Fumes Vapors"
for (vsname in "Chronic Respiratory Conditions Due To Fumes Vapors") {
  medical_crt_dx_icd(vs.name = vsname)
}

## dx special case Acute Condition
medical_crt_dx_icd_long(vs.name = "Acute Condition")
fdir <- paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis_crt10/",
                "#Acute Condition", " ", sprintf("%1d", 1:9), ".csv")
for (fdir_i in fdir){
  f_i <- fread(fdir_i, header = TRUE, colClasses = 'character', encoding = 'UTF-8')
  fwrite(f_i, paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/diagnosis_crt10/",
                     "#Acute Condition.csv"), append = TRUE)
  cat(paste0(fdir_i, " done!\n"))
}
Sys.time()


## icd procedure
for (vsname in vs.proc.icd.regexp$`Value Set Name`) {
  medical_crt_icd_proc(vs.name = vsname)
}

## icd procedure special case PPP
medical_crt_icd_proc_long(vs.name = "Potentially Planned Procedures")
fdir <- paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure_crt10/",
               "MEDICAL_CLAIM_ICD_PROCEDURE#ICD_PROCEDURE_CODE#Potentially Planned Procedures", " ", sprintf("%1d", 1:20), ".csv")
for (fdir_i in fdir){
  f_i <- fread(fdir_i, header = TRUE, colClasses = 'character', encoding = 'UTF-8')
  fwrite(f_i, paste0("E:/CT_APCD/Sai/medicaid/medicaid_files/value_sets/icd_procedure_crt10/",
                     "MEDICAL_CLAIM_ICD_PROCEDURE#ICD_PROCEDURE_CODE#Potentially Planned Procedures.csv"), append = TRUE)
  cat(paste0(fdir_i, " done!\n"))
  Sys.time()
}

# 
# a <- get_medical_vs_CRT("Well-Care",
#                    2013:2014,
#                    code.sys = c("procedure_code", "revenue_code", "icd_procedure_code"),
#                    takeUnique = takeUnique,
#                    fill = TRUE)
# b <- get_medical_vs_CRT("Nonacute Inpatient Stay",
#                         2013:2014,
#                         code.sys = c("type_of_bill_code"),
#                         takeUnique = takeUnique,
#                         fill = TRUE)
# 
# medical_crt_icd(vs.name = "PCI",
#                 year = 2014)
# 
# medical_crt_dx_icd(vs.name = "Varicella Zoster")
