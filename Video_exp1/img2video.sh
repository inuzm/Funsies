#!/usr/bin/env bash

# Streamlined version to produce a video with images via ffmpeg

# Replace the given path with the actual path where the directories or files are

cd  ~/Documents/whatevs/graphs/video_exp

# Black and white video
ffmpeg -framerate 15 -i %03d_bw.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p bw.mp4

# Color video
ffmpeg -framerate 15 -i %03d_col.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p col.mp4

# -framerate 15 : number of frames per second
# -i %03d_ : leading name of the images ffmpeg will use
# The other parameters are things I found out googling (DuckDuckGo)
