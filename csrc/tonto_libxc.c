/* Fortran-callable glue between Tonto and the C routines of libxc.

   Tonto's Foo code declares these routines in an interface block, as it does
   dgemm, and calls them by the gfortran convention: lower-case name with one
   trailing underscore, every argument by reference, and the length of a
   character argument appended by value. A functional is known to Tonto by a
   small integer "handle", an index into the table below, so that no C pointer
   has to be held on the Fortran side.

   The evaluation routines change only their output arrays, which is why the
   Foo interface may declare them pure. Setting up and ending a functional
   changes the table, and must be done outside any parallel loop. */

#include <stddef.h>
#include <string.h>
#include <xc.h>

#define TONTO_XC_MAX_FUNC 64

static xc_func_type *func[TONTO_XC_MAX_FUNC];

/* The functional for "handle", or NULL if the handle is not in use. */
static xc_func_type *lookup(const int *handle)
{
   if (*handle < 1 || *handle > TONTO_XC_MAX_FUNC) return NULL;
   return func[*handle - 1];
}

/* Set "id" to libxc's number for the functional called "name", e.g.
   "gga_x_pbe", or to -1 if libxc has none of that name. */
void tonto_xc_number_(const char *name, int *id, size_t len)
{
   char buf[256];
   while (len > 0 && name[len - 1] == ' ') len--;
   if (len >= sizeof buf) { *id = -1; return; }
   memcpy(buf, name, len);
   buf[len] = '\0';
   *id = xc_functional_get_number(buf);
}

/* Set up functional number "id" for "nspin" spin channels (1 or 2) and
   return its "handle", or 0 if that could not be done. */
void tonto_xc_init_(int *handle, const int *id, const int *nspin)
{
   int i;
   *handle = 0;
   for (i = 0; i < TONTO_XC_MAX_FUNC; i++) if (func[i] == NULL) break;
   if (i == TONTO_XC_MAX_FUNC) return;
   func[i] = xc_func_alloc();
   if (func[i] == NULL) return;
   if (xc_func_init(func[i], *id, *nspin == 2 ? XC_POLARIZED : XC_UNPOLARIZED) != 0) {
      xc_func_free(func[i]);
      func[i] = NULL;
      return;
   }
   *handle = i + 1;
}

/* End the functional "handle" and set the handle to 0. */
void tonto_xc_end_(int *handle)
{
   xc_func_type *p = lookup(handle);
   if (p != NULL) {
      xc_func_end(p);
      xc_func_free(p);
      func[*handle - 1] = NULL;
   }
   *handle = 0;
}

/* Set "rung" to what the functional needs as input: 1 the density, 2 also
   its gradient, 3 also the kinetic energy density; 0 for anything else. */
void tonto_xc_rung_(const int *handle, int *rung)
{
   const xc_func_type *p = lookup(handle);
   *rung = 0;
   if (p == NULL) return;
   switch (p->info->family) {
      case XC_FAMILY_LDA:  case XC_FAMILY_HYB_LDA:  *rung = 1; break;
      case XC_FAMILY_GGA:  case XC_FAMILY_HYB_GGA:  *rung = 2; break;
      case XC_FAMILY_MGGA: case XC_FAMILY_HYB_MGGA: *rung = 3; break;
   }
}

/* Set "a0" to the fraction of exact exchange in the functional; zero for
   one that is not a hybrid. */
void tonto_xc_exact_exchange_(const int *handle, double *a0)
{
   const xc_func_type *p = lookup(handle);
   *a0 = 0.0;
   if (p == NULL) return;
   switch (p->info->family) {
      case XC_FAMILY_HYB_LDA: case XC_FAMILY_HYB_GGA: case XC_FAMILY_HYB_MGGA:
         *a0 = xc_hyb_exx_coef(p);
   }
}

/* Set the density below which the functional returns zero. */
void tonto_xc_set_rho_cutoff_(const int *handle, const double *cutoff)
{
   xc_func_type *p = lookup(handle);
   if (p != NULL) xc_func_set_dens_threshold(p, *cutoff);
}

/* The evaluation routines. "np" is the number of points. With two spin
   channels "rho" holds (alpha,beta) for each point in turn, and "sigma"
   holds the three products (aa,ab,bb) of the density gradients; "vrho" and
   "vsigma" are laid out the same way. "exc" is the energy per particle. */

void tonto_xc_lda_exc_vxc_(const int *handle, const int *np, const double *rho,
                           double *exc, double *vrho)
{
   xc_lda_exc_vxc(lookup(handle), (size_t)*np, rho, exc, vrho);
}

void tonto_xc_gga_exc_vxc_(const int *handle, const int *np, const double *rho,
                           const double *sigma, double *exc, double *vrho,
                           double *vsigma)
{
   xc_gga_exc_vxc(lookup(handle), (size_t)*np, rho, sigma, exc, vrho, vsigma);
}

void tonto_xc_mgga_exc_vxc_(const int *handle, const int *np, const double *rho,
                            const double *sigma, const double *lapl,
                            const double *tau, double *exc, double *vrho,
                            double *vsigma, double *vlapl, double *vtau)
{
   xc_mgga_exc_vxc(lookup(handle), (size_t)*np, rho, sigma, lapl, tau, exc,
                   vrho, vsigma, vlapl, vtau);
}
