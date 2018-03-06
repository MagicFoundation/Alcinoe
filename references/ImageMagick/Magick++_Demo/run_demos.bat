@echo off
set srcdir=.\
set bindir=.\
set outdir=.\
set PATH=%bindir%;%PATH%

echo button ...
%bindir%button.exe

echo executing demo ...
%bindir%demo.exe

echo executing flip ...
%bindir%flip.exe

echo executing gravity ...
%bindir%gravity.exe

echo executing piddle ...
%bindir%piddle.exe

echo executing shapes ...
%bindir%shapes.exe

echo executing zoom -filter point -geometry 600x600 model.miff zoom_point_out.miff
%bindir%zoom.exe -filter point -geometry 600x600 %srcdir%\model.miff %outdir%zoom_point_out.miff

echo executing zoom -filter box -geometry 600x600 model.miff zoom_box_out.miff
%bindir%zoom.exe -filter box -geometry 600x600 %srcdir%\model.miff %outdir%zoom_box_out.miff

echo executing zoom -filter triangle -geometry 600x600 model.miff zoom_triangle_out.miff
%bindir%zoom.exe -filter triangle -geometry 600x600 %srcdir%\model.miff %outdir%zoom_triangle_out.miff

echo executing hermite -geometry 600x600 model.miff zoom_hermite_out.miff
%bindir%zoom.exe -filter hermite -geometry 600x600 %srcdir%\model.miff %outdir%zoom_hermite_out.miff

echo zoom -filter hanning -geometry 600x600 model.miff zoom_hanning_out.miff
%bindir%zoom.exe -filter hanning -geometry 600x600 %srcdir%\model.miff %outdir%zoom_hanning_out.miff

echo zoom -filter hamming -geometry 600x600 model.miff zoom_hamming_out.miff
%bindir%zoom.exe -filter hamming -geometry 600x600 %srcdir%\model.miff %outdir%zoom_hamming_out.miff

echo zoom -filter blackman -geometry 600x600 model.miff zoom_blackman_out.miff
%bindir%zoom.exe -filter blackman -geometry 600x600 %srcdir%\model.miff %outdir%zoom_blackman_out.miff

echo zoom -filter gaussian -geometry 600x600 model.miff zoom_gaussian_out.miff
%bindir%zoom.exe -filter gaussian -geometry 600x600 %srcdir%\model.miff %outdir%zoom_gaussian_out.miff

echo zoom -filter quadratic -geometry 600x600 model.miff zoom_quadratic_out.miff
%bindir%zoom.exe -filter quadratic -geometry 600x600 %srcdir%\model.miff %outdir%zoom_quadratic_out.miff

echo zoom -filter cubic -geometry 600x600 model.miff zoom_cubic_out.miff
%bindir%zoom.exe -filter cubic -geometry 600x600 %srcdir%\model.miff %outdir%zoom_cubic_out.miff

echo zoom -filter catrom -geometry 600x600 model.miff zoom_catrm_out.miff
%bindir%zoom.exe -filter catrom -geometry 600x600 %srcdir%\model.miff %outdir%zoom_catrm_out.miff

echo zoom -filter mitchell -geometry 600x600 model.miff zoom_mitchell_out.miff
%bindir%zoom.exe -filter mitchell -geometry 600x600 %srcdir%\model.miff %outdir%zoom_mitchell_out.miff

echo zoom -filter lanczos -geometry 600x600 model.miff zoom_lanczos_out.miff
%bindir%zoom.exe -filter lanczos -geometry 600x600 %srcdir%\model.miff %outdir%zoom_lanczos_out.miff

echo zoom -filter bessel -geometry 600x600 model.miff zoom_bessel_out.miff
%bindir%zoom.exe -filter bessel -geometry 600x600 %srcdir%\model.miff %outdir%zoom_bessel_out.miff

echo zoom -filter sinc -geometry 600x600 model.miff zoom_sinc_out.miff
%bindir%zoom.exe -filter sinc -geometry 600x600 %srcdir%\model.miff %outdir%zoom_sinc_out.miff


