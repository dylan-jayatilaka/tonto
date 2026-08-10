# Draw a deformation-density contour map from a grid written by Tonto's
# `plot` with plot_format= gnuplot.  Usage:
#
#     gnuplot -e "f='<grid file>'; out='<picture>.png'" deformation.gnuplot
#
# Optional: n= points per side, w= width in Angstrom, d= contour interval.
# Positive contours are solid red, negative dashed blue, zero grey -- the
# usual convention for a deformation density map.
#
# The grid file holds one value per line, in n rows of n separated by blank
# lines, so gnuplot's pseudo-columns index it: column(0) is the point number
# and column(-1) the row number.

if (!exists("f"))   f   = 'urea_deformation.deformation_density_grid,gnuplot'
if (!exists("out")) out = 'deformation.png'
if (!exists("n"))   n   = 201          # n_points in the job file
if (!exists("w"))   w   = 6.0          # x_width and y_width, in Angstrom
if (!exists("d"))   d   = 0.05         # contour interval, e/Angstrom^3
if (!exists("c"))   c   = 1.0          # highest contour drawn

s(i) = (i - (n-1)/2.0) * w/(n-1)       # grid index -> Angstrom, centred on C

# Contour lines into a temporary file, since gnuplot computes contours only
# while making a surface.
set contour base
unset surface
set view map
set cntrparam bspline
set cntrparam points 12
set cntrparam levels incremental -c, d, c
unset clabel
set table '.contours.tmp'
splot f using (s(column(0)-column(-1)*n)):(s(column(-1))):1 with lines
unset table

# Draw them, styled by sign.
reset
set terminal pngcairo size 860,820 font ",13"
set output out
set size square
set xrange [-w/2:w/2]
set yrange [-w/2:w/2]
set xlabel "x / Angstrom   (C to O)"
set ylabel "y / Angstrom   (C to N)"
set xtics 1
set ytics 1
set grid lc rgb "#e8e8e8"
unset key

plot '.contours.tmp' using 1:($3 >  1e-9 ? $2 : NaN) with lines lc rgb "#b2182b" lw 1.6, \
     '.contours.tmp' using 1:($3 < -1e-9 ? $2 : NaN) with lines lc rgb "#2166ac" lw 1.2 dt 2, \
     '.contours.tmp' using 1:(abs($3) <= 1e-9 ? $2 : NaN) with lines lc rgb "#808080" lw 1
