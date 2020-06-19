define mk_yambo
 LLIBS="";for exe in $$XLIBS; do LLIBS="$$LLIBS -l$$exe" ; done ; \
 for exe in $$X2DO; do \
  DLIBS="";for lib in $$DRILIBS; do DLIBS="$$DLIBS -l$$exe$$lib" ; done ; \
  echo " " ; \
  echo ">>>[Linking $$exe]<<<" ; \
  if test ! -f "$(bindir)/$$exe" || test "$(keep_objs)" = yes  ; then \
   if test ! -d $$XPATH ; then mkdir -p $$XPATH ; fi ; \
   if [ "$(topdir)" != "$(prefix)" ] && [ -f $$VPATH/.objects ] ; then \
    cp $$VPATH/.objects $$XPATH ; \
   fi ; \
   ./sbin/make_makefile.sh $$XPATH $$exe .objects x "$$DLIBS $$LLIBS" $(xcpp) $$ADF ; \
   cd $$XPATH ; $(MAKE) VPATH=$$VPATH || exit "$$?" ; \
  fi ; \
  echo " " ; \
 done
endef
define mk_ypp
 LLIBS="";for exe in $$XLIBS; do LLIBS="$$LLIBS -l$$exe" ; done ; \
 for exe in $$X_ypp_LIBS; do LLIBS="$$LLIBS -l_ypp_$$exe" ; done ; \
 for exe in $$X2DO; do \
  DLIBS="";for lib in $$DRILIBS; do DLIBS="$$DLIBS -l$$exe$$lib" ; done ; \
  echo " " ; \
  echo ">>>[Linking $$exe]<<<" ; \
  if test ! -f "$(bindir)/$$exe" || test "$(keep_objs)" = yes  ; then \
   if [ "$(topdir)" != "$(prefix)" ] && [ -f $$VPATH/.objects ] ; then \
    cp $$VPATH/.objects $$XPATH ; \
   fi ; \
   ./sbin/make_makefile.sh $$XPATH $$exe .objects x "$$DLIBS $$LLIBS" $(xcpp) $$ADF ; \
   cd $$XPATH ; $(MAKE) VPATH=$$VPATH || exit "$$?" ; \
  fi ; \
  echo " " ; \
 done
endef
