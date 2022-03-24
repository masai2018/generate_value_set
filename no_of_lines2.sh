#!/bin/bash

indir="E:/CT_APCD/LDS_12-5-19/"
outdir="E:/CT_APCD/Sai/medicaid/medicaid_files/"
outfile="no_of_lines_medicaid.txt"
: > $outdir$outfile 
for file in ${indir}*
do
    SECONDS=0
    temp_file=`basename $file`
    line=`sed -n '$=' ${indir}*$temp_file`
    timer_end1=`date "+%Y-%m-%d %H:%M:%S"`
    duration1="$(($SECONDS / 3600))hrs $((($SECONDS / 60) % 60))min $(($SECONDS % 60))sec"
    echo -e "File: $temp_file, duration: $duration1, number of lines: $line.\r\n" >> $outdir$outfile
done