#!/bin/bash

./mirror
./hessharr
./warp
./points
./transi ../../data/images/transi
./mirror --photos=../../data/images/transi
./mirror --photos=../../data/images/transi --variable-size

