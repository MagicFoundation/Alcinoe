to convert MP4 to animated gif :

Generate the palette:
"c:\Program Files\ffmpeg\bin\ffmpeg.exe" -i input.mp4 -vf "fps=60,scale=640:-1:flags=lanczos,palettegen=stats_mode=diff" -y palette.png

Create the GIF using the palette:
"c:\Program Files\ffmpeg\bin\ffmpeg.exe" -i input.mp4 -i palette.png -filter_complex "fps=60,scale=640:-1:flags=lanczos[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=3" -loop 0 output.gif