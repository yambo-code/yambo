define mk_src
 if [ "$(topdir)" != "$(prefix)" ] ; then cp -r $(topdir)/include/* include/ ; fi ; \
 for ldir in $$LIBS2DO; do \
  if test ! -f "$(libdir)/lib$$ldir.a" || test "$(keep_objs)" = yes  ; then \
   rm -f "$(libdir)/lib$$ldir.a" ; \
   echo " " ; \
   echo ">>>[Making $$ldir]<<<" ; \
   if test ! -d "$$XPATH/$$ldir" ; then mkdir -p "$$XPATH/$$ldir" ; fi ; \
   if [ "$(topdir)" != "$(prefix)" ] && [ -f $$VPATH/$$ldir/.objects ] ; then \
    cp $$VPATH/$$ldir/.objects $$XPATH/$$ldir ; \
   fi ; \
   ./sbin/make_makefile.sh $$XPATH/$$ldir lib$$ldir.a .objects l $(xcpp) $$ADF ; \
   cd $$XPATH/$$ldir ; $(MAKE) VPATH=$$VPATH/$$ldir || exit "$$?" ; cd ../../ ; \
  fi ; \
 done
endef

