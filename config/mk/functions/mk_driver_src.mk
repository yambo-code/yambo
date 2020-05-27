define mk_driver_src
 cp -r $(topdir)/driver/include/*.h include/ ;\
 for ldir in $$LIBS2DO; do \
  if test ! -f "$(libdir)/libdriver_$$ldir.a" || test "$(keep_objs)" = yes  ; then \
   rm -f "$(libdir)/libdriver_$$ldir.a" ; \
   echo " " ; \
   echo ">>>[Making driver/$$ldir]<<<" ; \
   if test ! -d "$$XPATH/$$ldir" ; then mkdir -p "$$XPATH/$$ldir" ; fi ; \
   if [ "$(topdir)" != "$(prefix)" ] && [ -f $$VPATH/$$ldir/.objects ] ; then \
    cp $$VPATH/$$ldir/.objects $$XPATH/$$ldir ; \
   fi ; \
   ./sbin/make_makefile.sh $$XPATH/$$ldir libdriver_$$ldir.a .objects l $(xcpp) $$ADF ; \
   cd $$XPATH/$$ldir ; $(MAKE) VPATH=$$VPATH/$$ldir || exit "$$?" ; cd ../../; \
  fi ; \
 done
endef
