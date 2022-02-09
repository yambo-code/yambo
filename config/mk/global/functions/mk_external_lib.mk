define mk_external_lib
 for lib in $$LIBS; do \
  if test ! -f "$(libdir)/lib$$lib.a" ; then \
   cd $$BASE/$$lib ; cp $(srcdir)/$$BASE/$$lib/Makefile.loc Makefile ; $(MAKE) VPATH=$(srcdir)/$$BASE/$$lib || exit "$$?" ; cd ../../ ; \
  fi \
 done
endef
