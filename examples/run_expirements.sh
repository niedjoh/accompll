#!/usr/bin/bash

./run.rb 60 accompll-ttt2-R.sh
./run.rb 60 accompll-ttt2-R-PCP.sh
./run.rb 60 accompll-ttt2-RSlashAC.sh
./run.rb 60 accompll-ttt2-RSlashAC-PCP.sh

./run.rb 60 accompll-ttt2i-R.sh
./run.rb 60 accompll-ttt2i-R-PCP.sh
./run.rb 60 accompll-ttt2i-RSlashAC.sh
./run.rb 60 accompll-ttt2i-RSlashAC-PCP.sh

./run.rb 60 maedmax.sh

./run.rb 60 mkbtt.sh

./table.cgi > a.html # please press control-d
