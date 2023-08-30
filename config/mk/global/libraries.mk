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
YAMBO_INT_LIBS= Yio 
YLIBDRIVER    = interface main options 
YLIBDRIVER_LD = _Ydriver_options _Ydriver_interface _Ydriver_main 
YLIBIO        = modules Yio
YLIBIO_LD     = $(YLIBIO)
#
# Yambo folders 
#===============
#
Y_LIBS       = driver tools modules memory allocations matrices linear_algebra parallel parser communicate output common timing Yio io $(IO_MODE) \
               xc_functionals interface stop_and_restart wf_and_fft bz_ops coulomb interpolate qp_control setup \
               tddft dipoles pol_function el-ph qp acfdt bse collisions hamiltonian sc \
               real_time_control real_time_hamiltonian real_time_propagation real_time_lifetimes real_time_iterative_el-ph \
               real_time_initialize ph-el real_time_drivers nloptics
Y_LIBS_LD    = tools memory allocations communicate modules matrices linear_algebra bz_ops parallel parser output common timing Yio io $(IO_MODE) \
               xc_functionals interface stop_and_restart wf_and_fft coulomb interpolate real_time_control qp_control setup \
               tddft dipoles pol_function el-ph qp acfdt bse hamiltonian collisions sc \
               real_time_hamiltonian real_time_propagation real_time_lifetimes real_time_iterative_el-ph \
               real_time_initialize ph-el real_time_drivers nloptics
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
YPP_LIBS     = modules interface qp plotting k-points symmetries bits electrons dipoles el-ph real_time excitons
YPP_LIBS_LD  = $(YPP_LIBS)
