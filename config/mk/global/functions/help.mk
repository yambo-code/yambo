define yambo_help
  $(ECHO) "\n This is $(y_version)";
  if [ "$(1)" = "header" ] || [ -z "$(1)" ] ; then \
    $(ECHO) "\n Use the following commands to get specific help:\n" ;\
    $(ECHO) "  to get general info on the Yambo projects included in this source use:\n   >make help what=intro" ;\
    $(ECHO) "  to get info on the compilation targets use:\n   >make help what=compilation\n" ;\
  fi;\
  if [ "$(1)" = "compilation" ] ; then \
   $(ECHO) "\n *** Collective components *** ";\
   $(ECHO) " [all]            all";\
   $(ECHO) "\n *** Main components *** ";\
   $(ECHO) -n " [core]          "; for target in $(CORE); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) -n " [main]          "; for target in $(MAIN); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) "\n *** Projects ***";\
   $(ECHO) -n " [sc-project]    "; for target in $(SC_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) -n " [ph-project]    "; for target in $(PH_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) -n " [rt-project]    "; for target in $(RT_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) -n " [nl-project]    "; for target in $(NL_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) -n " [rtext-project] "; for target in $(RTE_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) "\n *** GPL components *** ";\
   $(ECHO) -n " [gpl]           "; for target in $(GPL); do $(ECHO) -n "$$target" ; done;$(ECHO) ;\
   $(ECHO) "\n *** Libraries ***"; \
   $(ECHO) -n " [libs]           int-libs ext-libs download";$(ECHO) ;\
   $(ECHO) -n " [int-libs]      "; for target in $(INT_LIBS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) -n " [ext-libs]      "; for target in $(EXT_LIBS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) -n " [yambo-int-libs]"; for target in $(YAMBO_INT_LIBS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO) "\n *** Utils ***";\
   $(ECHO) -n " [utils]         "; for target in $(UTILS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
   $(ECHO)  "\n *** Cleaning ***" ;\
   $(ECHO)  " The cleaning procedure of yambo is divided in several modules: bin int-libs driver Ydriver src ypp interfaces conf dep";\
   $(ECHO)  " Each of these module can be called by using";\
   $(ECHO)  " make clean what=<MODULE> \n";\
   $(ECHO)  " make clean               =  remove all modules except int-libs, ext-libs, Ydriver, dependencies and configure files.";\
   $(ECHO)  " make clean_all           =  remove all modules. Complete cleaning.";\
   $(ECHO)  " make reset               =  clean  + int-libs.";\
   $(ECHO);\
  fi
  if [ "$(1)" = "intro" ] ; then \
   $(ECHO)  "\n *** Interfaces ***\n " ; \
   $(ECHO)  " a2y =  ABINIT to Yambo interface ";\
   $(ECHO)  " p2y =  QuantumEspresso to Yambo interface ";\
   $(ECHO)  "        http://www.yambo-code.org/wiki/index.php?title=Bulk_material:_h-BN ";\
   $(ECHO)  " c2y =  CPMD to Yambo interface";\
   $(ECHO)  "\n *** Main Components***\n" ;\
   $(ECHO)  " yambo =  main Yambo code ";\
   $(ECHO)  " ypp   =  Yambo Post Processing utility ";\
   $(ECHO)  "\n *** Other projects ***\n" ;\
   $(ECHO)  " yambo_sc =  Self-consistent (COHSEX, HF, DFT) project";\
   $(ECHO)  " yambo_rt =  Real-time dynamics project";\
   $(ECHO)  "             http://www.yambo-code.org/wiki/index.php?title=Linear_response_from_real_time_simulations";\
   $(ECHO)  " yambo_nl =  Non-linear optics project ";\
   $(ECHO)  "             http://www.yambo-code.org/wiki/index.php?title=Tutorials#Non_linear_response";\
   $(ECHO)  " yambo_ph =  Electron-phonon coupling project ";\
   $(ECHO)  "             http://www.yambo-code.org/wiki/index.php?title=Tutorials#Electron_phonon_coupling\n";\
  fi
endef
