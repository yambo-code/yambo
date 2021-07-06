define mk_external_yambo_lib
 for lib in $$LIBS; do \
  if test ! -f $(compdir)/config/stamps_and_lists/lib$$NAME$$lib.a.stamp; then \
   if test ! -d "$$BASE/$$lib" ; then mkdir -p "$$BASE/$$lib" ; fi ; \
   ./sbin/compilation/helper.sh -d $$BASE/$$lib -t lib$$NAME$$lib.a -o .objects -m l -g $@ -- "$(precision) $(xcpp) $$ADF" ; \
   cd $$BASE/$$lib ; $(MAKE) VPATH=$$VPATH/$$lib lib || exit "$$?" ; cd ../../../../../; \
  fi; \
 done
endef
