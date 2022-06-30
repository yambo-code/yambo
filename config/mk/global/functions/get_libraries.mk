# Download external libs #
define get_external_libraries
 mkdir -p lib/archive; \
 cd lib/archive; \
 if [ "$$LIB2DO" = "all" ] ; then \
  $(MAKE) -f Makefile.loc $(MAKEFLAGS) all ; \
 else \
  $(MAKE) -f Makefile.loc $(MAKEFLAGS) $$LIB2DO ; \
 fi
endef
