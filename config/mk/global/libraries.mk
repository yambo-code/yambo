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
# Source code
#
BASIC_LIBS   = driver tools modules memory allocations matrices linear_algebra parallel parser communicate output common timing Yio io $(IO_MODE) \
               xc_functionals interface stop_and_restart wf_and_fft bz_ops coulomb
BASIC_LIBS_LD= tools memory allocations communicate modules matrices linear_algebra bz_ops parallel parser output common timing Yio io $(IO_MODE) \
               xc_functionals interface stop_and_restart wf_and_fft coulomb

ALL_LIBS     = $(BASIC_LIBS) interpolate qp_control setup \
               tddft dipoles pol_function el-ph qp acfdt bse collisions hamiltonian sc \
               real_time_control real_time_hamiltonian real_time_propagation real_time_lifetimes \
               real_time_initialize ph-el real_time_drivers nloptics
ALL_LIBS_LD  = $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
               tddft dipoles pol_function el-ph qp acfdt bse hamiltonian collisions sc \
               real_time_hamiltonian real_time_propagation real_time_lifetimes \
               real_time_initialize ph-el real_time_drivers nloptics

MAIN_LIBS    = $(ALL_LIBS)
MAIN_LIBS_LD = $(ALL_LIBS_LD)

PJ_PHLIBS    = $(ALL_LIBS)
PJ_PHLIBS_LD = $(ALL_LIBS_LD) 

PJ_SCLIBS    = $(ALL_LIBS)
PJ_SCLIBS_LD = $(ALL_LIBS_LD) 

PJ_RTITLIBS    = $(ALL_LIBS)
PJ_RTITLIBS_LD = $(ALL_LIBS_LD) 

PJ_RTLIBS      = $(ALL_LIBS)
PJ_RTLIBS_LD   = $(ALL_LIBS_LD) 

PJ_RT_GPL_LIBS    = $(ALL_LIBS)
PJ_RT_GPL_LIBS_LD = $(ALL_LIBS_LD) 

PJ_NLLIBS    = $(ALL_LIBS)
PJ_NLLIBS_LD = $(ALL_LIBS_LD) 

#MAIN_LIBS    = $(BASIC_LIBS)    interpolate qp_control setup tddft dipoles pol_function qp acfdt bse
#MAIN_LIBS_LD = $(BASIC_LIBS_LD) interpolate qp_control setup tddft dipoles pol_function qp acfdt bse

#PJ_PHLIBS    = $(BASIC_LIBS)    interpolate qp_control setup tddft dipoles pol_function el-ph qp acfdt bse real_time_initialize ph-el
#PJ_PHLIBS_LD = $(BASIC_LIBS_LD) interpolate qp_control setup tddft dipoles pol_function el-ph qp acfdt bse real_time_initialize ph-el

#PJ_SCLIBS    = $(MAIN_LIBS) collisions hamiltonian sc
#PJ_SCLIBS_LD = $(MAIN_LIBS_LD) hamiltonian collisions sc
#
#PJ_RT_GPL_LIBS   = $(BASIC_LIBS) interpolate real_time_control qp_control setup \
#                   tddft dipoles pol_function qp acfdt bse collisions hamiltonian \
#                   real_time_hamiltonian real_time_propagation real_time_initialize real_time_drivers
#PJ_RT_GPL_LIBS_LD= $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
#                   tddft dipoles pol_function qp acfdt bse hamiltonian collisions \
#                   real_time_hamiltonian real_time_propagation real_time_initialize real_time_drivers
#
#PJ_RTITLIBS    = $(BASIC_LIBS) interpolate qp_control setup \
#               tddft dipoles pol_function el-ph qp acfdt bse collisions hamiltonian \
#               real_time_control real_time_hamiltonian real_time_propagation real_time_lifetimes real_time_iterative_el-ph \
#               real_time_initialize ph-el real_time_drivers
#PJ_RTITLIBS_LD = $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
#               tddft dipoles pol_function el-ph qp acfdt bse hamiltonian collisions \
#               real_time_hamiltonian real_time_propagation real_time_lifetimes real_time_iterative_el-ph \
#               real_time_initialize ph-el real_time_drivers
#
#PJ_RTLIBS    = $(BASIC_LIBS) interpolate qp_control setup \
#               tddft dipoles pol_function el-ph qp acfdt bse collisions hamiltonian \
#               real_time_control real_time_hamiltonian real_time_propagation real_time_lifetimes \
#               real_time_initialize ph-el real_time_drivers
#PJ_RTLIBS_LD = $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
#               tddft dipoles pol_function el-ph qp acfdt bse hamiltonian collisions \
#               real_time_hamiltonian real_time_propagation real_time_lifetimes \
#               real_time_initialize ph-el real_time_drivers
#
#PJ_NLLIBS    = $(PJ_RTLIBS) nloptics
#PJ_NLLIBS_LD = $(PJ_RTLIBS_LD) nloptics
#
# YAMBO sources needed by Interfaces
#
2YLIBS       = driver tools modules memory allocations matrices linear_algebra parallel parser communicate output common timing Yio io $(IO_MODE) \
               setup interface stop_and_restart bz_ops 
2YLIBS_LD    = tools memory allocations communicate modules matrices linear_algebra parallel parser output common timing Yio io $(IO_MODE) \
               setup interface stop_and_restart bz_ops 
#
# YPP
#
YPP_BASIC_LIBS     = modules interface qp plotting k-points symmetries bits electrons dipoles
YPP_BASIC_LIBS_LD  = modules interface qp plotting k-points symmetries bits electrons dipoles 
YPP_ALL_LIBS       = $(YPP_BASIC_LIBS) el-ph real_time excitons
YPP_ALL_LIBS_LD    = $(YPP_BASIC_LIBS_LD) el-ph real_time excitons

YPP_LIBS           = $(YPP_ALL_LIBS)
YPP_LIBS_LD        = $(YPP_ALL_LIBS_LD)
YPPPH_LIBS         = $(YPP_ALL_LIBS)
YPPPH_LIBS_LD      = $(YPP_ALL_LIBS_LD)
YPPRT_LIBS         = $(YPP_ALL_LIBS)
YPPRT_LIBS_LD      = $(YPP_ALL_LIBS_LD)
#
#YPPPH_LIBS         = $(YPP_BASIC_LIBS) el-ph excitons
#YPPPH_LIBS_LD      = $(YPP_BASIC_LIBS_LD) el-ph excitons
#YPPRT_LIBS         = $(YPP_BASIC_LIBS) el-ph real_time excitons
#YPPRT_LIBS_LD      = $(YPP_BASIC_LIBS_LD) el-ph real_time excitons
#
# YAMBO sources needed by YPP
#
#YPP_MAIN_ALL_LIBS    = $(BASIC_LIBS) interpolate qp_control setup interface tddft dipoles pol_function qp bse  \
#                       real_time_control interpolate qp_control setup interface \
#                       dipoles pol_function el-ph qp bse collisions hamiltonian nloptics
#YPP_MAIN_ALL_LIBS_LD = $(BASIC_LIBS_LD) interpolate qp_control setup interface tddft dipoles pol_function qp bse \
#                       real_time_control interpolate qp_control setup interface \
#                       dipoles pol_function el-ph qp bse hamiltonian collisions nloptics 

YPP_MAIN_ALL_LIBS    = $(ALL_LIBS)
YPP_MAIN_ALL_LIBS_LD = $(ALL_LIBS_LD)

YPP_MAIN_LIBS      = $(YPP_MAIN_ALL_LIBS)
YPP_MAIN_LIBS_LD   = $(YPP_MAIN_ALL_LIBS_LD)
YPPSC_MAIN_LIBS    = $(YPP_MAIN_ALL_LIBS)
YPPSC_MAIN_LIBS_LD = $(YPP_MAIN_ALL_LIBS_LD)
YPPRT_MAIN_LIBS    = $(YPP_MAIN_ALL_LIBS)
YPPRT_MAIN_LIBS_LD = $(YPP_MAIN_ALL_LIBS_LD)
YPPNL_MAIN_LIBS    = $(YPP_MAIN_ALL_LIBS)
YPPNL_MAIN_LIBS_LD = $(YPP_MAIN_ALL_LIBS_LD)

#YPP_MAIN_LIBS      = $(BASIC_LIBS) interpolate qp_control setup interface tddft dipoles pol_function qp bse
#YPP_MAIN_LIBS_LD   = $(BASIC_LIBS_LD) interpolate qp_control setup interface tddft dipoles pol_function qp bse
#YPPSC_MAIN_LIBS    = $(YPP_MAIN_LIBS) collisions hamiltonian sc
#YPPSC_MAIN_LIBS_LD = $(YPP_MAIN_LIBS_LD) collisions hamiltonian sc
#YPPRT_MAIN_LIBS    = $(BASIC_LIBS) real_time_control interpolate qp_control setup interface \
#                     dipoles pol_function el-ph qp bse collisions hamiltonian 
#YPPRT_MAIN_LIBS_LD = $(BASIC_LIBS_LD) real_time_control interpolate qp_control setup interface \
#                     dipoles pol_function el-ph qp bse hamiltonian collisions
#YPPNL_MAIN_LIBS    = $(BASIC_LIBS) real_time_control interpolate qp_control setup interface \
#YPPNL_MAIN_LIBS_LD = $(BASIC_LIBS_LD) real_time_control interpolate qp_control setup interface \
#                     dipoles pol_function el-ph qp bse collisions hamiltonian nloptics 
