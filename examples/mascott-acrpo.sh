#!/usr/bin/bash

timeout $2s mascott -th <(grep '(THEORY' $1) -ct -it acrpo $1
