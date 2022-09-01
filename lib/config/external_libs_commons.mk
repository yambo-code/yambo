#
define uncompress
if ! test -e uncompressed.stamp; then \
 gunzip < ../archive/$(PACKAGE).tar.gz | ../../config/missing --run tar xf -; \
 if ! test -d $(PACKAGE)  ; then \
  for folder in ./* ; do \
   if ! test -d $$folder; then continue; fi; \
   mv $$folder $(PACKAGE) ; \
  done; \
 fi; \
 touch uncompressed.stamp; \
fi
endef
#
define configure
if ! test -e configured.stamp && test -d $(PACKAGE); then \
 echo "\t[$(PACKAGE)] configuration"; \
 rm -f ${compdir}/log/config_$(PACKAGE).log; \
 CWD=`pwd`;\
 cd $(PACKAGE);  ./configure $(CONFFLAGS) >> ${compdir}/log/config_$(PACKAGE).log 2>&1 ; \
 touch $$CWD/configured.stamp;\
fi
endef
#
define compile
if ! test -e compiled.stamp && test -d $(PACKAGE); then \
 echo "\t[$(PACKAGE)] $(1) compilation"; \
 rm -f ${compdir}/log/compile_$(PACKAGE).log; \
 CWD=`pwd`;\
 FLGS="$(MAKEFLAGS)";\
 if [ "$(PACKAGE)" = "scalapack-2.1.0" ]; then \
  FLGS=`echo "$(MAKEFLAGS)" | sed 's/-j//g'`; \
 fi;\
 cd $(PACKAGE); \
 $(make) $(MAKEFLAGS) $(1) >> ${compdir}/log/compile_$(PACKAGE).log 2>&1;  \
 if [ ! "$(1)" = "blaslib" ] &&  [ ! "$(1)" = "loclib_only" ]; then touch $$CWD/compiled.stamp; fi;\
fi
endef
#
define install_via_make
if ! test -e installed.stamp ; then \
 echo "\t[$(PACKAGE)] installation"; \
 CWD=`pwd`;\
 cd $(PACKAGE); \
 $(make) install >> ${compdir}/log/install_$(PACKAGE).log 2>&1 ; \
 chmod u+x $(LIBPATH)/lib*/*${1}*a; \
 touch $$CWD/installed.stamp; \
fi
endef
#
define clean_the_lib
if test -d $(PACKAGE) && test -e configured.stamp ; then \
 CWD=`pwd`;\
 cd $(PACKAGE); $(make) $(1) >> ${compdir}/log/clean_$(PACKAGE).log 2>&1  ; \
 cd $$CWD;\
 rm -rf compiled.stamp configured.stamp installed.stamp; \
fi
endef
#
define rm_the_lib
 if test -d $(PACKAGE) ; \
  then ( rm -rf $(PACKAGE) ) ; \
  rm -f make.sys;\
  rm -f make_iotk.inc; \
  rm -f uncompressed.stamp ; \
 fi 
endef
