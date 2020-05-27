define warn_if_conf_not_done
  if test ! -f "$(topdir)/config/mk/defs.mk" ; then \
    echo;echo " Run ./configure first";echo; \
  fi
endef
define make_message
 if test -f "$(topdir)/config/mk/defs.mk" ; then \
  echo;echo " This is $(y_version)";echo;\
  echo " [all]        all";\
  echo " [components] core, rt-project, ext-libs, yambo-libs...";      echo;\
  echo " *** Main components *** ";\
  echo -n " [core]         "; for target in $(CORE); do echo -n " $$target" ; done;echo;\
  echo ; echo " *** Projects ***";\
  echo -n " [sc-project]   "; for target in $(SC_PROJ); do echo -n " $$target" ; done;echo;\
  echo -n " [magn-project] "; for target in $(MAG_PROJ); do echo -n " $$target" ; done;echo;\
  echo -n " [ph-project]   "; for target in $(PH_PROJ); do echo -n " $$target" ; done;echo;\
  echo -n " [rt-project]   "; for target in $(RT_PROJ); do echo -n " $$target" ; done;echo;\
  echo -n " [nl-project]   "; for target in $(NL_PROJ); do echo -n " $$target" ; done;echo;\
  echo -n " [rtext-project]"; for target in $(RTE_PROJ); do echo -n " $$target" ; done;echo;\
  echo -n " [kerr-project] "; for target in $(KERR_PROJ); do echo -n " $$target" ; done;echo;\
  echo ; echo " *** Libraries ***";\
  echo -n " [libs]          int-libs ext-libs yambo-libs";echo;\
  echo -n " [int-libs]     "; for target in $(INT_LIBS); do echo -n " $$target" ; done;echo;\
  echo -n " [ext-libs]     "; for target in $(EXT_LIBS); do echo -n " $$target" ; done;echo;\
  echo -n " [yambo-libs]   "; for target in $(YAMBO_LIBS); do echo -n " $$target" ; done;echo;\
  echo ; echo " *** Utils & Cleans ***";\
  echo -n " [utils]        "; for target in $(UTILS); do echo -n " $$target" ; done;echo;\
  echo -n " [clean]        "; for target in $(CLEANS); do echo -n " $$target" ; done;echo;\
  echo; \
 fi
endef

