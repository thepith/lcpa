ffmpeg -y -r 30 -f image2 -s 800x800 -i snap.%04d_trim.bmp -vcodec libx264 -crf 30 -pix_fmt yuv420p dlcpa.mp4
