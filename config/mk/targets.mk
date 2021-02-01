ifeq ($(do_p2y),yes)
  p2y   = p2y
endif
ifeq ($(do_e2y),yes)
  e2y   = e2y
endif
INTERFCS = a2y c2y $(p2y) $(e2y)
CORE     = yambo ypp $(INTERFCS)
GPL      = yambo_rt_gpl
UTILS    = changelog
CLEANS   = clean_fast clean clean_all distclean
PH_PROJ  = yambo_ph ypp_ph 
SC_PROJ  = yambo_sc ypp_sc
RT_PROJ  = yambo_rt ypp_rt
NL_PROJ  = yambo_nl ypp_nl
RTE_PROJ = yambo_rt ypp_rt
MAIN     = $(CORE) $(PH_PROJ) $(SC_PROJ) $(RT_PROJ)  $(NL_PROJ)
ALL      = $(CORE) $(PH_PROJ) $(SC_PROJ) $(RTE_PROJ) $(NL_PROJ) $(GPL)
BROKEN   = yambo_phdyn
SCRIPTS  = ydb.pl
EXE      = $(CORE) $(PH_PROJ) $(SC_PROJ) $(RTE_PROJ) $(NL_PROJ) $(BROKEN) $(SCRIPTS)
