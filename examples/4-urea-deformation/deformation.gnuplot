# Draw a deformation-density map written by Tonto's `plot` with
# plot_format= gnuplot.  Usage:
#
#     gnuplot -e "f='<grid file>'; out='<picture>.png'" deformation.gnuplot
#
# The grid file holds one value per line, in n rows of n separated by blank
# lines, so gnuplot's pseudo-columns do the indexing: column(0) is the point
# number and column(-1) the row number.

if (!exists("f"))   f   = 'urea_deformation.deformation_density_grid,gnuplot'
if (!exists("out")) out = 'deformation.png'
if (!exists("n"))   n   = 101          # n_points in the job file
if (!exists("w"))   w   = 6.0          # x_width and y_width, in Angstrom
if (!exists("c"))   c   = 0.5          # contour range, e/Angstrom^3

set terminal pngcairo size 760,700 font ",12"
set output out
set pm3d map
set size square
set palette defined (-c "blue", 0 "white", c "red")
set cbrange [-c:c]
set xlabel "x / Angstrom   (C to O)"
set ylabel "y / Angstrom   (C to N)"
set cblabel "deformation density / e Angstrom^{-3}"
unset key

s(i) = (i - (n-1)/2.0) * w/(n-1)       # grid index -> Angstrom, centred on C

splot f using (s(column(0)-column(-1)*n)):(s(column(-1))):1 with pm3d
