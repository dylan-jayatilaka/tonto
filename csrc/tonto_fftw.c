/* Fortran-callable glue between Tonto and FFTW.

   Called by the gfortran convention, as csrc/tonto_libxc.c is: lower-case
   name with one trailing underscore, every argument by reference. The Foo
   interface declares the routine pure; it changes only its array argument.
   Tonto runs one thread per process, so planning needs no lock. */

#include <complex.h>
#include <fftw3.h>

/* Replace the complex array "a", stored as a Fortran array a(n1,n2,n3), by
   its three-dimensional discrete Fourier transform

      a(j1,j2,j3) <- sum_k a(k1,k2,k3) exp(sign 2 pi i sum_d j_d k_d / n_d),

   with indices counted from 0 and "sign" -1 or +1. There is no scaling.
   FFTW stores arrays with the last index fastest, so the dimensions are
   passed to it in reverse. FFTW_ESTIMATE plans without trial runs, and so
   leaves "a" untouched until the transform itself. */
void tonto_fft_3d_(double complex *a, const int *n1, const int *n2,
                   const int *n3, const int *sign)
{
   fftw_plan plan;
   plan = fftw_plan_dft_3d(*n3, *n2, *n1, (fftw_complex *) a,
                           (fftw_complex *) a, *sign, FFTW_ESTIMATE);
   fftw_execute(plan);
   fftw_destroy_plan(plan);
}
