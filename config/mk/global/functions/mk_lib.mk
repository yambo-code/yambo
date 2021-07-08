define mk_lib
 for lib in $$LIBS; do \
  if test ! -f $(compdir)/config/stamps_and_lists/lib$$LAB$$lib.a.stamp; then \
   if test ! -d "$$BASE/$$lib" ; then mkdir -p "$$BASE/$$lib" ; fi ; \
   ./sbin/compilation/helper.sh -d $$BASE/$$lib -t lib$$LAB$$lib.a -o .objects -m l -g $@ -- "$(xcpp) $$ADF" ; \
   cd $$BASE/$$lib ; $(MAKE) VPATH=$(srcdir)/$$BASE/$$lib lib || exit "$$?" ; cd $(compdir); \
  fi;\
 done
endef
