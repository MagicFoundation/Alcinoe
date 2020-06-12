#!/bin/bash

echo FossilRepository=$1
echo GitRepository=$2
echo GitExe=$3
echo DescFile=$4
#echo DevPath=$5
echo
echo
echo SynProject repository
echo ---------------------

cd $2
cd ../SynProject
$3 commit -a --file=$4
$3 push

