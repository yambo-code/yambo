define make_message
  echo -e;echo -e " This is $(y_version)";
  echo -e; echo -e " Use\n\n  >make help\n\n to get more informations";echo -e;\
  echo -e " *** Collective components *** ";\
  echo -e " [all]            all";\
  echo -e " [components]     core sc-project (...) libs (...) utils clean";      echo -e;\
  echo -e " *** Main components *** ";\
  echo -e -n " [core]          "; for target in $(CORE); do echo -e -n " $$target" ; done;echo -e;\
  echo -e -n " [main]          "; for target in $(MAIN); do echo -e -n " $$target" ; done;echo -e;\
  echo -e ; echo -e " *** Projects ***";\
  echo -e -n " [sc-project]    "; for target in $(SC_PROJ); do echo -e -n " $$target" ; done;echo -e;\
  echo -e -n " [ph-project]    "; for target in $(PH_PROJ); do echo -e -n " $$target" ; done;echo -e;\
  echo -e -n " [rt-project]    "; for target in $(RT_PROJ); do echo -e -n " $$target" ; done;echo -e;\
  echo -e -n " [nl-project]    "; for target in $(NL_PROJ); do echo -e -n " $$target" ; done;echo -e;\
  echo -e -n " [rtext-project] "; for target in $(RTE_PROJ); do echo -e -n " $$target" ; done;echo -e;\
  echo -e; echo -e " *** GPL components *** ";\
  echo -e -n " [gpl]           "; for target in $(GPL); do echo -e -n " $$target" ; done;echo -e;\
  echo -e ; echo -e " *** Libraries ***"; \
  echo -e -n " [libs]           int-libs ext-libs download";echo -e;\
  echo -e -n " [int-libs]      "; for target in $(INT_LIBS); do echo -e -n " $$target" ; done;echo -e;\
  echo -e -n " [ext-libs]      "; for target in $(EXT_LIBS); do echo -e -n " $$target" ; done;echo -e;\
  echo -e -n " [yambo-int-libs]"; for target in $(YAMBO_INT_LIBS); do echo -e -n " $$target" ; done;echo -e;\
  echo -e ; echo -e " *** Utils & Cleans ***";\
  echo -e -n " [utils]         "; for target in $(UTILS); do echo -e -n " $$target" ; done;echo -e;\
  echo -e -n " [clean]         "; for target in $(CLEANS); do echo -e -n " $$target" ; done;echo -e;\
  echo
endef

