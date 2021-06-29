define mk_internal_lib
 for ldir in $$LIBS2DO; do \
   if test ! -d "$$DIR2GO/$$ldir" ; then mkdir -p "$$DIR2GO/$$ldir" ; fi ; \
   if [ "$(topdir)" != "$(prefix)" ] && [ -f $$VPATH/$$ldir/.objects ] ; then \
    cp $$VPATH/$$ldir/.objects $$DIR2GO/$$ldir ; \
   fi ; \
   ./sbin/compilation/helper.sh -d $$DIR2GO/$$ldir -t lib$$ldir.a -o .objects -m l -- "$(xcpp) $$ADF" ; \
    cd $$DIR2GO/$$ldir ; $(MAKE) VPATH=$$VPATH/$$ldir lib || exit "$$?" ; cd ../../; \
 done
endef
