################################################################################
### create regular expression (regexp) for each 2018 HEDIS value set
### version controlled by git
################################################################################


## setup tools and packages
library(methods)
source("E:/CT_APCD/ct-apcd/R/utils.R")
source("functions.R")
need_pkgs <- c("data.table", "bit64","tools","dplyr","readxl","ggplot2","microbenchmark")
need.packages(need_pkgs) 

## directory of raw data and output
dir_vs <- input_dir('E:/CT_APCD/shared/intermediate_data/APCD_modified/HEDIS_value_sets_8.27.18/')
outDir <- output_dir('E:/CT_APCD/shared/intermediate_data/APCD_modified/')

## read in the HEDIS value set of diagnoses/procedure codes
f.vs <- paste0(dir_vs, 'O. 2018 Volume 2 Value Set Directory 2018-01-16.xlsx')
vs <- read_xlsx(f.vs, sheet = 'Volume 2 Value Sets to Codes')
fwrite(vs, paste0(outDir, '2018_v2.csv'))
length(unique(vs$`Value Set Name`)) == 359 #11/25/2018 Sai

## quick summary
table(vs$`Code System`)
# CPT           CPT-CAT-II                  CVX 
# 8444                   39                   60 
# HCPCS                  HL7              ICD10CM 
# 645                    2                83377 
# ICD10PCS               ICD9CM              ICD9PCS 
# 23646                 7815                  111 
# LOINC                MSDRG                  POS 
# 589                  761                  158 
# SNOMED CT US Edition                UBREV                UBTOB 
# 1388                  738                  227 
vs_cvx <- vs %>% filter(`Code System` %in% c("CVX"))
vs_cdt <- vs %>% filter(`Code System` %in% c("CDT")) #empty
unique(vs_cvx$Code)
# [1] "20"  "50"  "106" "107" "110" "120" "17"  "46"  "47"  "48"  "49"  "51" 
# [13] "148" "31"  "83"  "85"  "08"  "44"  "45"  "62"  "118" "137" "165" "10" 
# [25] "89"  "88"  "135" "140" "141" "150" "153" "155" "158" "161" "05"  "03" 
# [37] "94"  "04"  "108" "136" "147" "07"  "133" "33"  "100" "152" "119" "116"
# [49] "122" "06"  "115" "21" 
unique(vs_cvx$`Value Set Name`)
# [1] "DTaP Vaccine Administered"                               
# [2] "Haemophilus Influenzae Type B (HiB) Vaccine Administered"
# [3] "Hepatitis A Vaccine Administered"                        
# [4] "Hepatitis B Vaccine Administered"                        
# [5] "HPV Vaccine Administered"                                
# [6] "Inactivated Polio Vaccine (IPV) Administered"            
# [7] "Influenza Vaccine Administered"                          
# [8] "Measles Vaccine Administered"                            
# [9] "Measles, Mumps and Rubella (MMR) Vaccine Administered"   
# [10] "Measles/Rubella Vaccine Administered"                    
# [11] "Meningococcal Vaccine Administered"                      
# [12] "Mumps Vaccine Administered"                              
# [13] "Pneumococcal 13-Valent Vaccine Administered"             
# [14] "Pneumococcal 23-Valent Vaccine Administered"             
# [15] "Pneumococcal Conjugate Vaccine Administered"             
# [16] "Rotavirus Vaccine (2 Dose Schedule) Administered"        
# [17] "Rotavirus Vaccine (3 Dose Schedule) Administered"        
# [18] "Rubella Vaccine Administered"                            
# [19] "Tdap Vaccine Administered"                               
# [20] "Varicella Zoster (VZV) Vaccine Administered" 

table(substr(vs$Code, 1,1), vs$`Code System`)

## seperate value set code
vs.dx.icd <- vs %>% filter(`Code System` %in% c(
  "ICD10CM", 
  "ICD9CM",
  NULL
  ))
vs.proc.icd <- vs %>% filter(`Code System` %in% c(
  "ICD10PCS",
  "ICD9PCS"
  ))
vs.proc.other <- vs %>% filter(`Code System` %in% c(
  "CPT", 
  "HCPCS",
  # "CDT",
  # "CVX",
  NULL
  ))
vs.other <- vs %>% filter(`Code System` %in% c(
  'LOINC',
  'POS', 
  'UBREV', 
  'UBTOB',
  NULL
  ))
vs.ubrev <- vs %>% filter(`Code System` %in% c('UBREV') )
filter(vs.dx.icd, `Value Set Name` =='Asthma')$Code
vs.dx.icd.regexp <- get_regexp(vs.dx.icd) # 135
vs.proc.icd.regexp <- get_regexp(vs.proc.icd)  # 35
vs.proc.other.regexp <- get_regexp(vs.proc.other) # 158
vs.other.regexp <- get_regexp(vs.other) # 72
vs.ubrev.regexp <- get_regexp(vs.ubrev)  # 31
tmp <- vs %>% mutate(`Value Set Name` = paste(`Value Set Name`, `Code System`, sep='*')) %>% select(`Value Set Name`, Code) 
vs.system.regexp <- get_regexp(tmp)
vs.system.regexp$`Code System` <- unlist(lapply(strsplit(vs.system.regexp$`Value Set Name`, '*', fixed = T), function(a) a[2]))
vs.system.regexp$`Value Set Name` <- unlist(lapply(strsplit(vs.system.regexp$`Value Set Name`, '*', fixed = T), function(a) a[1]))

## save result
fwrite(vs.dx.icd.regexp, file= paste0(outDir, '2018_v2_vs.dx.icd.regexp.csv'), row.names = F)
fwrite(vs.proc.icd.regexp, file= paste0(outDir, '2018_v2_vs.proc.icd.regexp.csv'), row.names = F)
fwrite(vs.proc.other.regexp, file= paste0(outDir, '2018_v2_vs.proc.other.regexp.csv'), row.names = F)
fwrite(vs.other.regexp, file= paste0(outDir, '2018_v2_vs.other.regexp.csv'), row.names = F)
fwrite(vs.system.regexp, file = paste0(outDir, '2018_v2_vs.system.regexp.csv'), row.names = F)
save(vs.dx.icd.regexp, vs.proc.icd.regexp, vs.proc.other.regexp, 
     vs.other.regexp, vs.system.regexp, vs.ubrev.regexp, file = paste0(outDir, '2018_v2_vs.regexp.rda'))

## Value sets with NDC 
dir_vs <- 'E:/CT_APCD/shared/intermediate_data/APCD_modified/Measurement-Codes/NCQA tables/HEDIS 2017 NDC Lists (Final)/'
list.files(dir_vs, full.names = T)
fvs <- data.frame(`Value Set Name` = NULL, Code = NULL)
for(i in list.files(dir_vs, pattern = '.xlsx')){
  `Value Set Name` <- substr(i, 1, 5)
  ftmp = read_xlsx(paste0(dir_vs, i))
  if(!('NDC Code' %in% colnames(ftmp))) print(colnames(ftmp))
  fvsi <- data.frame(`Value Set Name`, ftmp[, grep('ndc', colnames(ftmp), ignore.case = T)])
  colnames(fvsi) <- c('Value Set Name', 'Code')
  fvs <- rbind(fvs, fvsi)
  cat(`Value Set Name`, 'read in: # NDC = ',  dim(fvsi)[1], ' \n')
}
fwrite(fvs, file = paste0(outDir, '2017_hedis_value_set_NDC.csv'), row.names = F)  ## 98573
vs.pharmacy.ndc.regexp <- get_regexp(fvs)
range(vs.pharmacy.ndc.regexp$nchunk)
vs.pharmacy.ndc <- fvs
save(vs.pharmacy.ndc, vs.pharmacy.ndc.regexp, file = paste0(outDir, '2017_hedis_vs_ndc.regexp.rda'))
