define mk_ypp_src
 if test "$(compdir)" != "$(prefix)" ; then cp -r $(compdir)/include/* include/ ; fi ; \
 for ldir in $$LIBS2DO; do \
  if test ! -f $(compdir)/config/stamps_and_lists/lib_ypp_$$ldir.a.stamp; then \
   if test ! -d "$$XPATH/$$ldir" ; then mkdir -p "$$XPATH/$$ldir" ; fi ; \
   if [ "$(compdir)" != "$(prefix)" ] && [ -f $$VPATH/$$ldir/.objects ] ; then \
    cp $$VPATH/$$ldir/.objects $$XPATH/$$ldir ; \
   fi ; \
   ./sbin/compilation/helper.sh -d $$XPATH/$$ldir -t lib_ypp_$$ldir.a -o .objects -m l -- "$(xcpp) $$ADF" ; \
   cd $$XPATH/$$ldir ; $(MAKE) VPATH=$$VPATH/$$ldir lib || exit "$$?" ; cd ../../; \
  fi;\
 done
endef
