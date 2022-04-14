#!/bin/sh
sudo apt install libopencv-dev 

gfortran -g -c src/cvfortran.f90 -L/usr/local/lib -lopencv_core -lopencv_highgui -lopencv_imgproc 
