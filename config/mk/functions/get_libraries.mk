# Download external libs #
define download_external_libraries
 @+mkdir -p lib/archive; \
 if [ $(topdir) != $(prefix) ] ; then \
 cp $(topdir)/lib/archive/* lib/archive; \
 cp $(topdir)/config/missing config/ ; \
 fi ; \
 cd lib/archive; $(MAKE) -f Makefile.loc tarball_$(LIB2DO);
endef
