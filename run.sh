#!/bin/sh
gfortran -g cvfortran.o "$1" -L/usr/local/lib -lopencv_core -lopencv_highgui -lopencv_imgproc 
