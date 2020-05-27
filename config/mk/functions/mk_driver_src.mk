define mk_driver_src
 cp -r $(topdir)/lib/yambo/driver/include/*.h include/ ;\
 for ldir in $(DRIVER_LIBS); do \
  if test ! -f "$(libdir)/libdriver_$$ldir.a" || test "$(keep_objs)" = yes  ; then \
   rm -f "$(libdir)/libdriver_$$ldir.a" ; \
   echo " " ; \
   echo ">>>[Making lib/yambo/driver/$$ldir]<<<" ; \
   if test ! -d "lib/yambo/driver/$$ldir" ; then mkdir -p "lib/yambo/driver/$$ldir" ; fi ; \
   if [ "$(topdir)" != "$(prefix)" ] && [ -f $(topdir)/lib/yambo/driver/$$ldir/.objects ] ; then \
    cp $(topdir)/lib/yambo/driver/$$ldir/.objects lib/yambo/driver/$$ldir ; \
   fi ;pwd; \
   ./sbin/make_makefile.sh lib/yambo/driver/$$ldir libdriver_$$ldir.a .objects l $(xcpp) $$ADF ; \
   cd lib/yambo/driver/$$ldir ; $(MAKE) VPATH=$(topdir)/lib/yambo/driver/$$ldir || exit "$$?" ; cd ../../../../; \
  fi ; \
 done
endef
