#!/usr/bin/bash

./run.rb 60 accompll-ttt2.sh
./run.rb 60 accompll-ttt2e.sh
./run.rb 60 accompll-muterm.sh
./run.rb 60 maedmax.sh
./run.rb 60 mkbtt.sh
./run.rb 60 mkbtt-muterm.sh

./table.cgi > index.html # please press control-d
