# Draw a deformation-density map from a grid written by Tonto's `plot` with
# plot_format= gnuplot.  Usage:
#
#     gnuplot -e "f='<grid file>'; out='<picture>.png'" deformation.gnuplot
#
# Optional: n= points per side, w= width in Angstrom, c= colour range.
#
# The picture is a colour map with a scale bar, overlaid with contours at
# LOGARITHMIC levels -- +-0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2
# e/Angstrom^3 -- because a density spans decades and evenly spaced contours
# either crowd at the nuclei or vanish in the bonds. Positive contours are
# solid, negative dashed.
#
# The grid file holds one value per line, in n rows of n separated by blank
# lines, so gnuplot's pseudo-columns index it: column(0) is the point number
# and column(-1) the row number.

if (!exists("f"))   f   = 'urea_deformation.deformation_density_grid,gnuplot'
if (!exists("out")) out = 'deformation.png'
if (!exists("n"))   n   = 201          # n_points in the job file
if (!exists("w"))   w   = 6.0          # x_width and y_width, in Angstrom
if (!exists("c"))   c   = 0.5          # colour range, e/Angstrom^3
if (!exists("lv"))  lv  = "0.002 0.005 0.01 0.02 0.05 0.1 0.2 0.5 1.0 2.0"

s(i) = (i - (n-1)/2.0) * w/(n-1)       # grid index -> Angstrom, centred on C
x(i) = s(column(0)-column(-1)*n)
y(i) = s(column(-1))

# The signed logarithmic ladder, as a discrete level list for gnuplot.
levels = ""
do for [i=words(lv):1:-1] { levels = levels.sprintf("%s,", "-".word(lv,i)) }
do for [i=1:words(lv)]    { levels = levels.sprintf("%s%s", word(lv,i), i<words(lv) ? "," : "") }

# Pass 1: contours to a table. gnuplot computes contours only while making a
# surface, and cannot draw them over a pm3d map in the same pass.
set contour base
unset surface
set view map
set cntrparam bspline
set cntrparam points 12
eval "set cntrparam levels discrete ".levels
unset clabel
set table '.contours.tmp'
splot f using (x(0)):(y(0)):1 with lines
unset table

# Pass 2: a 2D image with its scale bar, and the contours drawn on top as
# ordinary lines -- a 2D plot, so nothing can colour or occlude them.
set terminal pngcairo size 900,820 font ",13"
set output out
unset contour
set surface
set palette defined (-c "#2166ac", -c/3 "#92c5de", 0 "white", c/3 "#f4a582", c "#b2182b")
set cbrange [-c:c]
set colorbox
set cblabel "deformation density / e Angstrom^{-3}"
set xlabel "x / Angstrom   (C to O)"
set ylabel "y / Angstrom   (C to N)"
set xrange [-w/2:w/2]
set yrange [-w/2:w/2]
set xtics 1
set ytics 1
set size square
unset key

plot f using (x(0)):(y(0)):1 with image, \
     '.contours.tmp' using 1:2 with lines lc rgb "black" lw 1
