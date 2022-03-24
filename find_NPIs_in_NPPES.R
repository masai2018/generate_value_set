################################################################################
### find NPIs in NPPES
### version controlled by git
################################################################################


## setup tools and packages
library(methods)
source("E:/CT_APCD/ct-apcd/R/utils.R")
source("functions.R")
need_pkgs <- c("data.table", "bit64","tools","dplyr")
need.packages(need_pkgs)

## directory of raw data and output
inDir <- input_dir("E:/CT_APCD/Commercial and Medicare data 5-24-18/")
outDir <- output_dir("E:/CT_APCD/shared/intermediate_data/APCD_modified/")

## read data and column names
nppes <- fread('E:/CT APCD/shared/intermediate_data/APCD_modified/npidata_20050523-20180213.csv', 
      colClasses = 'character',   nrow=100000  ) 
colnames(nppes)[1:60]

## find needed column names and change these names
nppes_col_missing_rate <- apply(nppes, 2, function(a) mean(a=='', na.rm=T))
nppes_col_keep <-  sort(union(1:59, which(nppes_col_missing_rate < 0.95)))
nppes_names <- table(unlist(lapply(strsplit(names(nppes), '_'), function(a) a[1])))

## output nppes names
fwrite(nppes_names, 'E:/CT_APCD/shared/intermediate_data/APCD_modified/nppes_names.csv')

## find the column names which have 'state', 'date' and 'stat'
colnames(nppes)[which(grepl('state', colnames(nppes), ignore.case = T))]  # 24 32 50 54 58 62 66 70 74 78 82
# [1] "Provider Business Mailing Address State Name"           "Provider Business Practice Location Address State Name"
# [3] "Provider License Number State Code_1"                   "Provider License Number State Code_2"                  
# [5] "Provider License Number State Code_3"                   "Other Provider Identifier State_1"                     
# [7] "Other Provider Identifier State_2"                      "Other Provider Identifier State_3"                     
# [9] "Other Provider Identifier State_4"                      "Other Provider Identifier State_5"                     
# [11] "Other Provider Identifier State_6"                     

colnames(nppes)[which(grepl('date', colnames(nppes), ignore.case = T))]
colnames(nppes)[which(grepl('stat', colnames(nppes), ignore.case = T))]

## change these column names to 'CT'
nppes_ct <- nppes[`Provider Business Mailing Address State Name` == 'CT' 
                  | `Provider Business Practice Location Address State Name` == 'CT'
                  | `Provider License Number State Code_1` == 'CT'
                  | `Provider License Number State Code_2` == 'CT'
                  | `Provider License Number State Code_3` == 'CT'
                  | `Other Provider Identifier State_1` == 'CT'
                  | `Other Provider Identifier State_2` == 'CT'
                  | `Other Provider Identifier State_3` == 'CT'
                  | `Other Provider Identifier State_4` == 'CT'
                  | `Other Provider Identifier State_5` == 'CT'
                  | `Other Provider Identifier State_6` == 'CT', ]  # 76307   # 75674 


## time period of the data
range(as.Date(nppes_ct$`Last Update Date`, format='%m/%d/%Y'))
# "2007-07-08" "2018-02-13"  

## number of NPI (duplicated NPI removed)
dim(nppes_ct)
length(unique(nppes_ct$NPI))

## output nppes CT file
fwrite(nppes_ct, 'E:/CT APCD/shared/intermediate_data/APCD_modified/npidata_CT_NPPES2018_76307x86.csv', row.names = F)  
# 34 MB


## number of some names
table(nppes_ct$`Entity Type Code`, nppes_ct$`Provider Organization Name (Legal Business Name)` != '')  
# 62526 ind + 13781 orga
nppes_ct[`Entity Type Code` == '1', 1:7]
names(nppes_ct)
nppes <- nppes[, c(1:59, 84)][, CT:=as.numeric(NPI %in% nppes_ct$NPI)]
table(nppes$`Is Organization Subpart`)
#               N       Y 
# 4386064  986457  103625
table(nppes$`Entity Type Code` )   
#                  1       2 
# 119,037 4,096,454 1,260,655
# c(1:2, 5:11, 21:42, 48, 51, 52, 55, 56, 59:61)


## output nppes file
nppes0 <- nppes[, c(1:2, 5, 37, 48, 60:61 )]
fwrite(nppes, 'E:/CT APCD/shared/intermediate_data/APCD_modified/npidata_NPPES2018_5476146x61.csv', row.names = F)  
# 34 MB
nppes0[`Entity Type Code`=='2' & grepl('Community Medical Group', `Provider Organization Name (Legal Business Name)`, ignore.case = T)]
nppes <- nppes[, c(1:59, 84)][, CT:=nppes0$CT]


## NPIs in NPPES are all 10-digit, starting with '1'
table(substr(nppes$NPI, 1, 1))
table(nchar(nppes$NPI)) 