#!/bin/bash

./mirror
./hessharr
./warp
./points
./imagproc
./spline
./transi ../../data/images/transi
./imagproc --photos=../../data/images/transi
./imagproc --photos=../../data/images/transi --variable-size

