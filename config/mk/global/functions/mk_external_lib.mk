define mk_external_lib
 echo $$LIBS;\
 for lib in $$LIBS; do \
  if test ! -f "$(libdir)/lib$$lib.a" ; then \
   cd $$BASE/$$lib ; cp Makefile.loc Makefile ; $(MAKE) VPATH=$(compdir)/$$BASE/$$lib || exit "$$?" ; cd ../../ ; \
  fi \
 done
endef
