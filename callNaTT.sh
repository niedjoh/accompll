#!/usr/bin/bash
outfile=`mktemp`
errfile=`mktemp`

timeout 2 NaTT.exe 1> "$outfile" 2> "$errfile"

errcode=`echo $?`

if [ "$errcode" = "124" -o "$errcode" = "137" ] # timeout reached
then
    echo "NO"
else
    cat $outfile
    cat $errfile 1>&2
fi
