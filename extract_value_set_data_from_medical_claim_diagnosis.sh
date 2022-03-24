#!/bin/bash

## This file is used to split 'MEDICAL_CLAIM_DIAGNOSIS.txt' into small files, and then
## create corresponding 'extract_value_set_data_from_medical_claim_diagnosis_****.R' 
## script files.

## define some variables
indir=E:/CT_APCD/Sai/medicaid/medicaid_files/modified_data/ #directory of raw data file 
outdir=E:/CT_APCD/Sai/medicaid/medicaid_files/modified_data/dgs_sub_files/ # output directory of sub raw data
rawfile=ct_medical_claim_diagnosis  #Do NOT include extention of raw data file
rwex=.txt  #extention of raw data file
lofe=100000000    #lines of each sub file
rindir=E:/CT_APCD/Sai/medicaid/medicaid_scripts/generate_value_sets/R/ #directory of r script file
routdir=E:/CT_APCD/Sai/medicaid/medicaid_scripts/generate_value_sets/R/ # output directory of sub r script files
rfile=extract_value_set_data_from_medical_claim_diagnosis #name of r script file
rex=.R  #extention of r script file

# # ## calculate the total number of rows
# SECONDS=0
# line=`sed -n '$=' $indir$rawfile$rwex`
# timer_end1=`date "+%Y-%m-%d %H:%M:%S"`
# duration1="$(($SECONDS / 3600))hrs $((($SECONDS / 60) % 60))min $(($SECONDS % 60))sec"
# echo "Duration of getting the line number is: $duration1"
# echo "The number of lines is: $line"
# 
# ## split data file, each file has 1e8 rows
# SECONDS=0
# { read header && sed "1~$((${lofe}-1)) s/^/${header}\n/g" | split -l $lofe --numeric-suffixes=1 -d -a 4 --additional-suffix=$rwex - $outdir$rawfile\_ ; } < $indir$rawfile$rwex
# nofsf=`printf "%04d" $((${line}/${lofe}+1))`
# duration2="$(($SECONDS / 3600))hrs $((($SECONDS / 60) % 60))min $(($SECONDS % 60))sec"
# echo "Total number of sub file is $nofsf"
# echo "Duration of cutting the file is: $duration2"


## create r script files
for i in $(seq -w 0001 0007)
do
sed "s/nnnn/${i}/g" $rindir$rfile$rex > $routdir$rfile\_$i$rex
done


