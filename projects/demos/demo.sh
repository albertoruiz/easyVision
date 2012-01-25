#!/bin/bash

./mirror
./hessharr
./warp
./points
./spline
./transi ../../data/images/transi
./mirror --photos=../../data/images/transi
./mirror --photos=../../data/images/transi --variable-size

