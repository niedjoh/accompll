#!/usr/bin/bash
cd ~/muterm
infile=`mktemp`
cat > $infile
./muterm -t 4 -i $infile | head -n 1


