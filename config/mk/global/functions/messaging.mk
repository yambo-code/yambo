define make_message
  $(ECHO) ;$(ECHO)  " This is $(y_version)";
  $(ECHO) ; $(ECHO)  " Use\n\n  >make help\n\n to get more informations";$(ECHO) ;\
  $(ECHO)  " *** Collective components *** ";\
  $(ECHO)  " [all]            all";\
  $(ECHO)  " [components]     core sc-project (...) libs (...) utils clean";      $(ECHO) ;\
  $(ECHO)  " *** Main components *** ";\
  $(ECHO) -n " [core]          "; for target in $(CORE); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) -n " [main]          "; for target in $(MAIN); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO)  ; $(ECHO)  " *** Projects ***";\
  $(ECHO) -n " [sc-project]    "; for target in $(SC_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) -n " [ph-project]    "; for target in $(PH_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) -n " [rt-project]    "; for target in $(RT_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) -n " [nl-project]    "; for target in $(NL_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) -n " [rtext-project] "; for target in $(RTE_PROJ); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) ; $(ECHO)  " *** GPL components *** ";\
  $(ECHO) -n " [gpl]           "; for target in $(GPL); do $(ECHO) -n "$$target" ; done;$(ECHO) ;\
  $(ECHO)  ; $(ECHO)  " *** Libraries ***"; \
  $(ECHO) -n " [libs]           int-libs ext-libs download";$(ECHO) ;\
  $(ECHO) -n " [int-libs]      "; for target in $(INT_LIBS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) -n " [ext-libs]      "; for target in $(EXT_LIBS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) -n " [yambo-int-libs]"; for target in $(YAMBO_INT_LIBS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO)  ; $(ECHO)  " *** Utils & Cleans ***";\
  $(ECHO) -n " [utils]         "; for target in $(UTILS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO) -n " [clean]         "; for target in $(CLEANS); do $(ECHO) -n " $$target" ; done;$(ECHO) ;\
  $(ECHO)
endef

