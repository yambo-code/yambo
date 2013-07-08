## Process this file with automake to produce Makefile.in

## Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.
##
## $Id: Makefile.am 2433 2006-09-28 11:06:54Z marques $

lib_LTLIBRARIES = libxc.la

libxc_la_FUNC_SOURCES = \
	special_functions.c integrate.c util.c mix_func.c functionals.c \
	lda.c lda_x.c lda_x_1d.c lda_x_2d.c \
	lda_c_wigner.c lda_c_gombas.c lda_c_rpa.c lda_c_hl.c \
	lda_c_vwn.c lda_c_pz.c lda_c_pw.c lda_c_ml1.c lda_xc_teter93.c \
	lda_c_1d_csc.c \
	lda_c_2d_amgb.c lda_c_2d_prm.c \
	lda_k_tf.c \
	gga.c \
	gga_x_lg93.c gga_x_pbe.c gga_x_rpbe.c gga_x_pbea.c gga_x_mpbe.c gga_x_herman.c gga_x_b86.c gga_x_b86_mgc.c \
	gga_x_b88.c gga_x_g96.c gga_x_pw86.c gga_x_pw91.c gga_x_optx.c gga_x_airy.c gga_x_c09x.c \
	gga_x_dk87.c gga_x_ft97.c gga_x_wc.c gga_x_am05.c gga_x_bayesian.c gga_x_kt.c gga_x_htbs.c \
	gga_c_pbe.c gga_c_lyp.c gga_c_p86.c gga_c_pw91.c gga_c_am05.c gga_c_lm.c gga_c_wl.c gga_c_wi.c \
	gga_x_lb.c gga_xc_b97.c gga_xc_edf1.c gga_xc_1w.c \
	gga_x_sogga11.c gga_c_sogga11.c gga_xc_th.c \
	gga_x_2d_b86.c gga_x_2d_b86_mgc.c gga_x_2d_b88.c gga_x_2d_pbe.c \
	gga_k_tflw.c gga_k_pearson.c gga_k_ol1.c gga_k_ol2.c gga_k_dk.c \
	hyb_gga_xc_b3lyp.c hyb_gga_xc_o3lyp.c hyb_gga_xc_pbeh.c hyb_gga_xc_b1wc.c \
	hyb_gga_xc_b97.c \
	mgga.c \
	mgga_x_lta.c mgga_x_tpss.c mgga_x_br89.c mgga_xc_vsxc.c mgga_x_m06l.c mgga_x_tau_hcth.c \
	mgga_c_tpss.c mgga_x_2d_prhg07.c

libxc_la_FUNC_SINGLE_SOURCES = $(libxc_la_FUNC_SOURCES:.c=_s.c)

libxc_la_SOURCES = $(libxc_la_FUNC_SOURCES)

if COMPILE_FORTRAN
  libxc_la_SOURCES += xc_f.c libxc_funcs.f90 libxc.f90
endif
if COMPILE_SINGLE
  nodist_libxc_la_SOURCES = $(libxc_la_FUNC_SINGLE_SOURCES)
if COMPILE_FORTRAN
    nodist_libxc_la_SOURCES += xc_f_s.c libxc_s.f90
endif
endif

# libtool stuff
libxc_la_LDFLAGS = -version-info 0:9:0
# this is a hack to go around buggy libtool/automake versions
libxc_la_LIBTOOLFLAGS = --tag=F77
LTFCCOMPILE = $(LIBTOOL) --mode=compile --tag=F77 $(FC) $(AM_FCFLAGS) $(FCFLAGS)
FCLINK = $(LIBTOOL) --mode=link --tag=F77 $(FCLD) $(AM_FCFLAGS) $(FCFLAGS) \
        $(AM_LDFLAGS) $(LDFLAGS) -o $@


dist_noinst_SCRIPTS = get_funcs.pl

noinst_HEADERS = \
	string_f.h util.h work_lda.c \
	work_gga_x.c work_gga_c.c work_gga_becke.c \
	work_mgga_x.c work_mgga_c.c \
	libxc_master.F90
include_HEADERS = xc.h xc_config.h
nodist_include_HEADERS = xc_funcs.h

if COMPILE_SINGLE
  include_HEADERS += xc_s.h
endif

BUILT_SOURCES = xc_funcs.h

## declare fortran modules so they are installed
if COMPILE_FORTRAN

if F90_MOD_UPPERCASE
  LIBFUNCMOD = LIBXC_FUNCS_M.@ax_cv_f90_modext@
else
  LIBFUNCMOD = libxc_funcs_m.@ax_cv_f90_modext@
endif
nodist_include_HEADERS += $(LIBFUNCMOD)

if COMPILE_SINGLE

if F90_MOD_UPPERCASE
  XCLIBMODS_S = XC_S_F90_LIB_M.@ax_cv_f90_modext@ XC_S_F90_TYPES_M.@ax_cv_f90_modext@
else
  XCLIBMODS_S = xc_s_f90_lib_m.@ax_cv_f90_modext@ xc_s_f90_types_m.@ax_cv_f90_modext@
endif
nodist_include_HEADERS += $(XCLIBMODS_S)

else

if F90_MOD_UPPERCASE
  XCLIBMODS = XC_F90_LIB_M.@ax_cv_f90_modext@ XC_F90_TYPES_M.@ax_cv_f90_modext@
else
  XCLIBMODS = xc_f90_lib_m.@ax_cv_f90_modext@ xc_f90_types_m.@ax_cv_f90_modext@
endif
nodist_include_HEADERS += $(XCLIBMODS)

endif

endif

SUFFIXES = _s.c
.c_s.c:
	@CPP@ -D SINGLE_PRECISION=1 @CPPFLAGS@ $(INCLUDES) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $< > $*_s.c

CLEANFILES = *~ *.bak *.mod *.il *.d *.pc* ifc* funcs_*.c funcs.h \
	libxc.f90 libxc_funcs.f90 xc_funcs.h *_s.c *_s.f90

xc_funcs.h libxc_funcs.f90: get_funcs.pl
	$(srcdir)/get_funcs.pl $(srcdir) $(top_builddir)

$(LIBFUNCMOD):
	$(MAKE) libxc_funcs.o

libxc.f90 $(XCLIBMODS): libxc_master.F90 libxc_funcs.o
	@FCCPP@ @CPPFLAGS@ $(AM_CPPFLAGS) $(srcdir)/libxc_master.F90 > $(top_builddir)/src/libxc.f90
	@if [ "@F90_ACCEPTS_LINE_NUMBERS@" = "no" ]; then \
		grep -v "^#" $(top_builddir)/src/libxc.f90 > $(top_builddir)/src/libxc.f91; \
		mv -f $(top_builddir)/src/libxc.f91 $(top_builddir)/src/libxc.f90; \
	fi
libxc_s.f90 $(XCLIBMODS_S): libxc_master.F90 libxc_funcs.o
	@FCCPP@ -D SINGLE_PRECISION=1 @CPPFLAGS@ $(AM_CPPFLAGS) $(srcdir)/libxc_master.F90 > $(top_builddir)/src/libxc_s.f90
	@if [ "@F90_ACCEPTS_LINE_NUMBERS@" = "no" ]; then \
		grep -v "^#" $(top_builddir)/src/libxc_s.f90 > $(top_builddir)/src/libxc_s.f91; \
		mv -f $(top_builddir)/src/libxc_s.f91 $(top_builddir)/src/libxc_s.f90; \
	fi
