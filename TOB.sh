#!/bin/bash

## This file is used to create 'TOB****.R'
## script files from 2012 to 2018.

##define some variables
rindir="E:/CT_APCD/Sai/medicaid/medicaid_scripts/generate_value_sets/R/" # directory of r script file
routdir="E:/CT_APCD/Sai/medicaid/medicaid_scripts/generate_value_sets/R/" # output directory of sub r script files
rfile=TOB  # name of r script file
rex=.R  #extention of r script file
year=$(seq -w 2015 2018)  #year range

## create r script files
for i in $year
do
sed "s/yyyy/${i}/g" "$rindir"$rfile$rex > "$routdir"$rfile\_$i$rex
done
