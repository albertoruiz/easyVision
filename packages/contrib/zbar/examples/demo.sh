#!/bin/bash

./zbar --photos=../../../../data/images/barcode
./zbar2 `ls ../../../../data/images/barcode/*.jpg`

