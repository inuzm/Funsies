#!/usr/bin/env bash

mkdir ./images

# First argument is directory where images will be saved

Rscript --vanilla ./video4.R ./images 30 15

cd ./images

# Black and white video
ffmpeg -framerate 15 -i %04d_bw.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p ../bw.mp4

# Color video
ffmpeg -framerate 15 -i %04d_col.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p ../col.mp4

cd ..
rm -r ./images
