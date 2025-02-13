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
INT_LIBS      = qe_pseudo slatec math77 local
#
# Yambo folders 
#===============
#
BASIC_LIBS   = driver tools modules parser memory matrices allocations linear_algebra parallel communicate output common timing Yio io $(IO_MODE) \
               xc_functionals interface stop_and_restart wf_and_fft bz_ops coulomb
BASIC_LIBS_LD= driver tools memory allocations communicate modules matrices linear_algebra bz_ops parallel parser output common timing Yio io $(IO_MODE) \
               xc_functionals interface stop_and_restart wf_and_fft coulomb

MAIN_LIBS    = $(BASIC_LIBS) interpolate qp_control setup tddft dipoles pol_function qp acfdt bse
MAIN_LIBS_LD = $(BASIC_LIBS_LD) interpolate qp_control setup tddft dipoles pol_function qp acfdt bse

PJ_PHLIBS    = el-ph interface_ph qp_ph
PJ_PHLIBS_LD = $(MAIN_LIBS_LD) interface_ph el-ph qp_ph

HAM_LIBS    = hamiltonian common_sc_rt collisions
HAM_LIBS_LD = common_sc_rt collisions hamiltonian

PJ_SCLIBS    = sc interface_sc
PJ_SCLIBS_LD = $(MAIN_LIBS_LD) $(HAM_LIBS_LD) interface_sc sc

PJ_RTLIBS   = real_time_control real_time_hamiltonian real_time_propagation \
              real_time_initialize interface_rt io_rt
PJ_RTLIBS_LD= $(MAIN_LIBS_LD) $(HAM_LIBS_LD) interface_rt \
                   real_time_control io_rt  \
                   real_time_hamiltonian real_time_propagation \
                   real_time_initialize

PJ_NLLIBS    = nloptics interface_nl collisions_nl
PJ_NLLIBS_LD = $(PJ_RTLIBS_LD) collisions_nl nloptics interface_nl
#
# Yambo folders needed by Interfaces
#=====================================
#
2Y_LIBS_LD    = $(MAIN_LIBS_LD) interface stop_and_restart bz_ops
#
# YPP folders 
#===============
#
YPP_BASIC_LIBS     = YPPmodules interface qp plotting k-points symmetries bits electrons dipoles
YPP_BASIC_LIBS_LD  = YPPmodules interface qp plotting k-points symmetries bits electrons dipoles 
YPP_LIBS           = $(YPP_BASIC_LIBS) excitons
YPP_LIBS_LD        = $(YPP_BASIC_LIBS_LD) excitons
YPPSC_LIBS         = sc interface_sc
YPPSC_LIBS_LD      = $(YPP_LIBS_LD) sc interfaces_sc
YPPPH_LIBS         = el-ph interface_ph
YPPPH_LIBS_LD      = $(YPP_LIBS_LD) el-ph interface_ph
YPPRT_LIBS         = real_time interface_rt
YPPRT_LIBS_LD      = $(YPP_LIBS_LD) real_time interface_rt
YPPNL_LIBS         = nloptics interface_nl
YPPNL_LIBS_LD      = $(YPPRT_LIBS_LD) nloptics interface_nl
#
# YAMBO sources needed by YPP
#
YPP_MAIN_LIBS_LD   = $(MAIN_LIBS_LD)
YPPSC_MAIN_LIBS_LD = $(PJ_SCLIBS_LD)
YPPPH_MAIN_LIBS_LD = $(PJ_PHLIBS_LD)
YPPRT_MAIN_LIBS_LD = $(PJ_RTLIBS_LD)
YPPNL_MAIN_LIBS_LD = $(PJ_NLLIBS_LD)
