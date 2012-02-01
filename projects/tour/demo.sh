#!/bin/bash

./smon
./grid

./arrows
./choice
./loop

./play
./play1
./play3
./play3 '../../data/videos/rot4.avi -fps 30' --live
./play3 '../../data/videos/rot4.avi -fps 30' --chan
./play4
./play5
./play6
./play0

./param

./stand1
./stand2
./stand3
./interface

./nogui '../../data/videos/rot4.avi -fps 30 -benchmark'
./single ../../data/images/transi/dscn2070.jpg \
         ../../data/images/transi/dscn2070.jpg

