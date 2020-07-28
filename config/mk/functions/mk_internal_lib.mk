define mk_internal_lib
 for ldir in $$LIBS2DO; do \
  if test ! -f "$(libdir)/lib$$ldir.a" || test "$(keep_objs)" = yes  ; then \
   rm -f "$(libdir)/lib$$ldir.a" ; \
   echo " " ; \
   echo ">>>[Making $$ldir]<<<" ; \
   if test ! -d "$$DIR2GO/$$ldir" ; then mkdir -p "$$DIR2GO/$$ldir" ; fi ; \
   if [ "$(topdir)" != "$(prefix)" ] && [ -f $$VPATH/$$ldir/.objects ] ; then \
    cp $$VPATH/$$ldir/.objects $$DIR2GO/$$ldir ; \
   fi ; \
   ./sbin/make_makefile.sh $$DIR2GO/$$ldir lib$$ldir.a .objects l $(xcpp) $$ADF ; \
   cd $$DIR2GO/$$ldir ; $(MAKE) VPATH=$$VPATH/$$ldir || exit "$$?" ; cd ../../; \
  fi \
 done
endef
