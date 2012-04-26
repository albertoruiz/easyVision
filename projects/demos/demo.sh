#!/bin/bash

./mirror
./hessharr
./warp
./points
./imagproc
./trail
./pose ../../data/videos/rot4.avi
./spline
./transi ../../data/images/transi
# ./imagproc --photosmp=../../data/images/transi
./imagproc --photos=../../data/images/transi

