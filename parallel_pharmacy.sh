#!/bin/bash

## This file is used to run r script files parallelly.

## define some variables
rfile=extract_value_set_data_from_pharmacy_claim   # name of r script file
rex=.R    # extention of r script file
Fjob=2015	  # first r script file
Njob=2016  # total number of r script files
Nproc=2  # number of r script to run every time

## set up a function
function CMD {
    echo -e "Begin Job $1 \r\n" >> log.txt
    echo -e "Begin Job $1 \r\n"
    Rscript $rfile\_$i$rex
    n=$((random % 5 + 1))
    sleep $n
    echo -e "Job $1 $Ijob $2 exiting ...\r\n" >> log.txt
    echo -e "Job $1 $Ijob $2 exiting ...\r\n" 
    
}

function PushQue { 
	Que="$Que $1"
	Nrun=$(($Nrun+1))
}
function GenQue {   
	OldQue=$Que
	Que=""; Nrun=0
	for PID in $OldQue; do
		if [[ -d /proc/$PID ]]; then
			PushQue $PID
		fi
	done
}
function ChkQue {
	OldQue=$Que
	for PID in $OldQue; do
		if [[ ! -d /proc/$PID ]] ; then
			GenQue; break
		fi
	done
}
for i in $(seq -s " " -f %04g $Fjob $Njob); do
	CMD $i &
	PID=$!
	PushQue $PID
	while [[ $Nrun -ge $Nproc ]]; do
		ChkQue
		sleep 1
	done
done
wait


# PID=()
# for((i=1; i<=$Njob; )); do
#     for((Ijob=0; Ijob<$Nproc; Ijob++)); do
# 	if [[ $i -gt $Njob ]]; then
# 	    break;
# 	fi
# 	if [[ ! "${PID[Ijob]}"]] || ! kill -0  ${PID[Ijob]} 2> log.txt; then
# 	    CMD $i $Ijob &
# 	    PID[Ijob]=$!
# 	    i=$((i+1))
# 	fi
#     done
#     sleep 10
# done
# wait
