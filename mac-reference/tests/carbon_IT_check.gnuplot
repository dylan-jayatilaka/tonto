# Fit of a spherical atom form factor to Gaussians, written by Tonto.
# Run with:  gnuplot 'carbon_IT_check.gnuplot'
f(x) = A1**2*exp(-B1**2*x**2) + A2**2*exp(-B2**2*x**2) + A3**2*exp(-B3**2*x**2) + A4**2*exp(-B4**2*x**2) + C**2
A1 = 1.59405144208084E+00
B1 = 4.33122499992785E+00
A2 = 1.05924501414923E+00
B2 = 3.03096519280575E+00
A3 = 1.32191527716416E+00
B3 = 7.15422951826400E-01
A4 = 9.75448614740930E-01
B4 = 6.81807010817577E+00
C = 4.86990759665930E-01
set fit quiet
set fit errorvariables
set fit limit 1e-10
set fit maxiter 2000
set fit logfile 'carbon_IT_check.fit.log'
fit [0:2.0000] f(x) 'carbon_IT_check.dat' using 1:2 via A1,B1,A2,B2,A3,B3,A4,B4,C
set print 'carbon_IT_check.coeffs'
print A1**2; print 2*abs(A1)*A1_err
print B1**2; print 2*abs(B1)*B1_err
print A2**2; print 2*abs(A2)*A2_err
print B2**2; print 2*abs(B2)*B2_err
print A3**2; print 2*abs(A3)*A3_err
print B3**2; print 2*abs(B3)*B3_err
print A4**2; print 2*abs(A4)*A4_err
print B4**2; print 2*abs(B4)*B4_err
print C**2; print 2*abs(C)*C_err
print FIT_WSSR; print FIT_NDF
set terminal pngcairo size 900,600
set output 'carbon_IT_check.png'
set xlabel 'sin(theta)/lambda / 1/Angstrom'
set ylabel 'f(s) - fit(s) / electrons'
set grid
plot 'carbon_IT_check.dat' using 1:($2-f($1)) with lines title 'f(s) - fit'
