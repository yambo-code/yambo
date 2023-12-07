#
# License-Identifier: GPL
#
# Copyright (C) 2020 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
# EXT_LIBS imported 
#
ifeq ($(wildcard config/mk/global/defs.mk),config/mk/global/defs.mk)
  include config/mk/defs.mk
endif
include lib/archive/package.list
#
INT_LIBS      = slatec math77 local
ifeq ($(do_p2y),yes)
 INT_LIBS+=qe_pseudo
endif
YAMBO_INT_LIBS= Yio 
YLIBDRIVER    = interface main options 
YLIBDRIVER_LD = _Ydriver_options _Ydriver_interface _Ydriver_main 
YLIBIO        = modules Yio
YLIBIO_LD     = $(YLIBIO)
#
# Yambo folders 
#===============
#
BASIC_LIBS   = driver tools modules memory allocations matrices linear_algebra parallel parser communicate output common timing Yio io $(IO_MODE) \
               xc_functionals interface stop_and_restart wf_and_fft bz_ops coulomb
BASIC_LIBS_LD= tools memory allocations communicate modules matrices linear_algebra bz_ops parallel parser output common timing Yio io $(IO_MODE) \
               xc_functionals interface stop_and_restart wf_and_fft coulomb

MAIN_LIBS    = $(BASIC_LIBS) interpolate qp_control setup tddft dipoles pol_function qp acfdt bse
MAIN_LIBS_LD = $(BASIC_LIBS_LD) interpolate qp_control setup tddft dipoles pol_function qp acfdt bse

PJ_PHLIBS    = $(BASIC_LIBS) interpolate qp_control setup tddft dipoles pol_function el-ph exc-ph qp acfdt bse
PJ_PHLIBS_LD = $(BASIC_LIBS_LD) interpolate qp_control setup tddft dipoles pol_function el-ph exc-ph qp acfdt bse

PJ_SCLIBS    = $(MAIN_LIBS) collisions hamiltonian sc
PJ_SCLIBS_LD = $(MAIN_LIBS_LD) hamiltonian collisions sc

PJ_RTLIBS   = $(BASIC_LIBS) interpolate qp_control setup \
                   tddft dipoles pol_function qp acfdt bse collisions hamiltonian \
                   real_time_control real_time_hamiltonian real_time_propagation \
                   real_time_initialize real_time_drivers
PJ_RTLIBS_LD= $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
                   tddft dipoles pol_function qp acfdt bse hamiltonian collisions \
                   real_time_hamiltonian real_time_propagation \
                   real_time_initialize real_time_drivers

PJ_NLLIBS    = $(BASIC_LIBS) interpolate qp_control setup \
               tddft dipoles pol_function qp acfdt bse collisions hamiltonian \
               real_time_control real_time_hamiltonian real_time_propagation \
               real_time_initialize real_time_drivers nloptics
PJ_NLLIBS_LD = $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
               tddft dipoles pol_function qp acfdt bse hamiltonian collisions \
               real_time_hamiltonian real_time_propagation \
               real_time_initialize real_time_drivers nloptics
#
# Yambo folders needed by Interfaces
#=====================================
#
2Y_LIBS       = driver tools modules memory allocations matrices linear_algebra parallel parser communicate output common timing Yio io $(IO_MODE) \
                setup interface stop_and_restart bz_ops 
2Y_LIBS_LD    = tools memory allocations communicate modules matrices linear_algebra parallel parser output common timing Yio io $(IO_MODE) \
                setup interface stop_and_restart bz_ops 
#
# YPP folders 
#===============
#
YPP_BASIC_LIBS     = modules interface qp plotting k-points symmetries bits electrons dipoles
YPP_BASIC_LIBS_LD  = modules interface qp plotting k-points symmetries bits electrons dipoles 
YPP_LIBS           = $(YPP_BASIC_LIBS) excitons
YPP_LIBS_LD        = $(YPP_BASIC_LIBS_LD) excitons
YPPSC_LIBS         = $(YPP_LIBS)
YPPSC_LIBS_LD      = $(YPP_LIBS_LD)
YPPPH_LIBS         = $(YPP_BASIC_LIBS) el-ph excitons
YPPPH_LIBS_LD      = $(YPP_BASIC_LIBS_LD) el-ph excitons
YPPRT_LIBS         = $(YPP_BASIC_LIBS) real_time excitons
YPPRT_LIBS_LD      = $(YPP_BASIC_LIBS_LD) real_time excitons
YPPNL_LIBS         = $(YPPRT_LIBS)
YPPNL_LIBS_LD      = $(YPPRT_LIBS_LD)
#
# YAMBO sources needed by YPP
#
YPP_MAIN_LIBS      = $(BASIC_LIBS) interpolate qp_control setup interface tddft dipoles pol_function qp bse
YPP_MAIN_LIBS_LD   = $(BASIC_LIBS_LD) interpolate qp_control setup interface tddft dipoles pol_function qp bse
YPPPH_MAIN_LIBS    = $(YPP_MAIN_LIBS) el-ph exc-ph
YPPPH_MAIN_LIBS_LD = $(YPP_MAIN_LIBS_LD) el-ph exc-ph
YPPSC_MAIN_LIBS    = $(YPP_MAIN_LIBS) collisions hamiltonian sc
YPPSC_MAIN_LIBS_LD = $(YPP_MAIN_LIBS_LD) collisions hamiltonian sc
YPPRT_MAIN_LIBS    = $(BASIC_LIBS) real_time_control interpolate qp_control setup interface \
                     dipoles pol_function qp bse collisions hamiltonian 
YPPRT_MAIN_LIBS_LD = $(BASIC_LIBS_LD) real_time_control interpolate qp_control setup interface \
                     dipoles pol_function qp bse hamiltonian collisions
YPPNL_MAIN_LIBS    = $(BASIC_LIBS) real_time_control interpolate qp_control setup interface \
                     dipoles pol_function qp bse collisions hamiltonian nloptics
YPPNL_MAIN_LIBS_LD = $(BASIC_LIBS_LD) real_time_control interpolate qp_control setup interface \
                     dipoles pol_function qp bse hamiltonian collisions nloptics
