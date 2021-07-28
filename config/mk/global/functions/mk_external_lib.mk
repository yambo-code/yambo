define mk_external_lib
 for lib in $$LIBS; do \
  if test ! -f "$(libdir)/lib$$lib.a" ; then \
   cd $$BASE/$$lib ; cp Makefile.loc Makefile ; $(MAKE) VPATH=$(srcdir)/$$BASE/$$lib || exit "$$?" ; cd ../../ ; \
  fi \
 done
endef
