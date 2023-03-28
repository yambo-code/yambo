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

MAIN_LIBS    = $(BASIC_LIBS) interpolate qp_control setup tddft dipoles pol_function collisions qp acfdt bse
MAIN_LIBS_LD = $(BASIC_LIBS_LD) interpolate qp_control setup tddft dipoles pol_function collisions qp acfdt bse

PJ_PHLIBS    = $(BASIC_LIBS) interpolate qp_control setup tddft dipoles pol_function collisions el-ph qp acfdt bse real_time_initialize ph-el
PJ_PHLIBS_LD = $(BASIC_LIBS_LD) interpolate qp_control setup tddft dipoles pol_function collisions el-ph qp acfdt bse real_time_initialize ph-el

PJ_SCLIBS    = $(MAIN_LIBS) hamiltonian sc
PJ_SCLIBS_LD = $(MAIN_LIBS_LD) hamiltonian sc

PJ_RT_GPL_LIBS   = $(BASIC_LIBS) interpolate real_time_control qp_control setup \
                   tddft dipoles pol_function qp acfdt bse collisions hamiltonian \
                   real_time_hamiltonian real_time_propagation real_time_initialize real_time_drivers
PJ_RT_GPL_LIBS_LD= $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
                   tddft dipoles pol_function qp acfdt bse hamiltonian collisions \
                   real_time_hamiltonian real_time_propagation real_time_initialize real_time_drivers

PJ_RTITLIBS    = $(BASIC_LIBS) interpolate qp_control setup \
               tddft dipoles pol_function collisions el-ph qp acfdt bse hamiltonian \
               real_time_control real_time_hamiltonian real_time_propagation real_time_lifetimes real_time_iterative_el-ph \
               real_time_initialize ph-el real_time_drivers
PJ_RTITLIBS_LD = $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
               tddft dipoles pol_function collisions el-ph qp acfdt bse hamiltonian \
               real_time_hamiltonian real_time_propagation real_time_lifetimes real_time_iterative_el-ph \
               real_time_initialize ph-el real_time_drivers

PJ_RTLIBS    = $(BASIC_LIBS) interpolate qp_control setup \
               tddft dipoles pol_function collisions el-ph qp acfdt bse hamiltonian \
               real_time_control real_time_hamiltonian real_time_propagation real_time_lifetimes \
               real_time_initialize ph-el real_time_drivers
PJ_RTLIBS_LD = $(BASIC_LIBS_LD) interpolate real_time_control qp_control setup \
               tddft dipoles pol_function collisions el-ph qp acfdt bse hamiltonian \
               real_time_hamiltonian real_time_propagation real_time_lifetimes \
               real_time_initialize ph-el real_time_drivers

PJ_MODLIBS    = $(BASIC_LIBS) models interpolate qp_control setup \
               tddft dipoles pol_function collisions el-ph qp acfdt bse hamiltonian \
               real_time_control real_time_hamiltonian real_time_propagation real_time_lifetimes \
               real_time_initialize ph-el real_time_drivers
PJ_MODLIBS_LD = $(BASIC_LIBS_LD) models interpolate real_time_control qp_control setup \
               tddft dipoles pol_function collisions el-ph qp acfdt bse hamiltonian \
               real_time_hamiltonian real_time_propagation real_time_lifetimes \
               real_time_initialize ph-el real_time_drivers

PJ_NLLIBS    = $(PJ_RTLIBS) nloptics
PJ_NLLIBS_LD = $(PJ_RTLIBS_LD) nloptics
#
# YAMBO sources needed by Interfaces
#
2YLIBS       = driver tools modules memory allocations matrices linear_algebra parallel parser communicate output common timing Yio io $(IO_MODE) \
               setup interface stop_and_restart bz_ops 
2YLIBS_LD    = tools memory allocations communicate modules matrices linear_algebra parallel parser output common timing Yio io $(IO_MODE) \
               setup interface stop_and_restart bz_ops 
#
# YPP
#=====
YPP_BASIC_LIBS     = modules interface qp plotting k-points symmetries bits electrons dipoles
YPP_LIBS           = $(YPP_BASIC_LIBS) excitons
YPPPH_LIBS         = $(YPP_BASIC_LIBS) el-ph excitons
YPPRT_LIBS         = $(YPP_BASIC_LIBS) el-ph real_time excitons
YPPMOD_LIBS        = $(YPPPH_LIBS) models 
# .. LD
YPP_BASIC_LIBS_LD  = $(YPP_BASIC_LIBS)
YPP_LIBS_LD        = $(YPP_LIBS) 
YPPPH_LIBS_LD      = $(YPPPH_LIBS)
YPPRT_LIBS_LD      = $(YPPRT_LIBS)
YPPMOD_LIBS_LD     = $(YPPMOD_LIBS)
#
# YAMBO sources needed by YPP
#=============================
YPP_MAIN_LIBS      = $(BASIC_LIBS) interpolate qp_control setup interface tddft dipoles pol_function collisions qp bse
YPP_MAIN_LIBS_LD   = $(BASIC_LIBS_LD) interpolate qp_control setup interface tddft dipoles pol_function collisions qp bse
YPPSC_MAIN_LIBS    = $(YPP_MAIN_LIBS) hamiltonian sc
YPPSC_MAIN_LIBS_LD = $(YPP_MAIN_LIBS_LD) hamiltonian sc
YPPRT_MAIN_LIBS    = $(BASIC_LIBS) real_time_control interpolate qp_control setup interface \
                     dipoles pol_function collisions el-ph qp bse hamiltonian 
YPPRT_MAIN_LIBS_LD = $(BASIC_LIBS_LD) real_time_control interpolate qp_control setup interface \
                     dipoles pol_function collisions el-ph qp bse hamiltonian 
YPPNL_MAIN_LIBS    = $(BASIC_LIBS) real_time_control interpolate qp_control setup interface \
                     dipoles pol_function collisions el-ph qp bse hamiltonian nloptics 
YPPNL_MAIN_LIBS_LD = $(BASIC_LIBS_LD) real_time_control interpolate qp_control setup interface \
                     dipoles pol_function collisions el-ph qp bse hamiltonian nloptics 
YPPMOD_MAIN_LIBS   = $(BASIC_LIBS) interpolate qp_control setup interface tddft dipoles pol_function collisions el-ph ph-el qp bse
YPPMOD_MAIN_LIBS_LD= $(BASIC_LIBS_LD) interpolate qp_control setup interface tddft dipoles pol_function collisions el-ph ph-el qp bse

