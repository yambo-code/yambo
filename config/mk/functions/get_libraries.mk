# Download external libs #
define get_external_libraries
 mkdir -p lib/archive; \
 if [ $(topdir) != $(prefix) ] ; then \
 cp $(topdir)/lib/archive/* lib/archive; \
 cp $(topdir)/config/missing config/ ; \
 fi ; \
 cd lib/archive; \
 if [ "$$LIB2DO" = "all" ] ; then \
   $(MAKE) -f Makefile.loc all ; \
 else \
   $(MAKE) -f Makefile.loc tarball_$$LIB2DO ; \
 fi
endef
