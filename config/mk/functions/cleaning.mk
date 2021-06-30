#
# Drivers (top)
#
define clean_driver
 if [ "$(1)" = "bin"       ] || [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then $(clean_bin); fi;\
 if [ "$(1)" = "libs"      ] ||                  [ "$(1)" = "all" ] ; then \
   $(clean_ext_libs); \
   EXTS="\.f90 \.o \.lock \.tmp_source";WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_dir_driver); \
   WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_lib_driver); \
   WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_mod_driver); \
 fi; \
 if [ "$(1)" = "stamps"    ] ||                  [ "$(1)" = "all" ] ; then $(clean_stamps); fi; \
 if [ "$(1)" = "driver"    ] ||                  [ "$(1)" = "all" ] ; then \
  EXTS="\.f90 \.o \.lock \.tmp_source";WDIR="$(topdir)";TARG="driver";$(clean_dir_driver);\
 fi
 if [ "$(1)" = "Ydriver"   ] ||                  [ "$(1)" = "all" ] ; then \
   EXTS="\.f90 \.o \.lock \.tmp_source";WDIR="$(libdir)/yambo/driver/src";TARG="$(YLIBDRIVER)";$(clean_dir_driver);\
   WDIR="$(libdir)";TARG="Ydriver";$(clean_lib_driver);\
   WDIR="$(libdir)/yambo/driver/src";TARG="$(YLIBDRIVER)";$(clean_mod_driver);\
 fi;\
 if                             [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then \
   $(call clean_src_driver,"src");\
   $(call clean_src_driver,"ypp");\
   $(call clean_src_driver,"interfaces");\
 fi; \
 if [ "$(1)" = "conf"      ] ||                  [ "$(1)" = "alld" ] ; then $(clean_config); fi; \
 if [ "$(1)" = "dep"       ] ||                  [ "$(1)" = "alld" ] ; then $(clean_dependencies); fi 
endef
#
# Drivers (intermediate)
#
define clean_src_driver
 if [ "$(1)" = "src" ] || [ "$(1)" = "ypp" ] || [ "$(1)" = "interfaces" ] ; then \
  if  test -f config/stamps_and_lists/active_directories.list; then \
   TARG="";MSG="$(1)";EXTS="\.f90 \.o \.lock \.tmp_source";WDIR="$(topdir)";\
   for FOLD in `cat config/stamps_and_lists/active_directories.list|grep $(1)`;do TARG="$$TARG $$FOLD";done;\
   $(clean_dir_driver);$(clean_lib_driver);$(clean_mod_driver); \
  fi;\
 fi
endef
define clean_dir_driver
 if test -n "$$MSG"; then LMSG="$$MSG"; else LMSG="$$TARG";fi; \
 echo  "\t[CLEANING $$LMSG] Extension(s): $$EXTS" ; \
 for ext in $$EXTS; do \
  for dirtoclean in $$TARG; do \
   find $$WDIR/$$dirtoclean \( -name '*'$$ext  \) |  xargs rm -fr ; \
  done;\
 done
endef
define clean_mod_driver
 if test -n "$$MSG"; then LMSG="$$MSG"; else LMSG="$$TARG";fi; \
 echo  "\t[CLEANING $$LMSG] Modules" ; \
 for dirtoclean in $$TARG; do \
  if test -f $$WDIR/$$dirtoclean/modules.list; then \
   for file in `cat $$WDIR/$$dirtoclean/modules.list`; do rm -fr $(includedir)/$$file".mod"; done; \
  fi; \
 done
endef
define clean_lib_driver
 if test -n "$$MSG"; then LMSG="$$MSG"; else LMSG="$$TARG";fi; \
 echo  "\t[CLEANING $$LMSG] Libraries" ; \
 for dirtoclean in $$TARG; do \
  ldir=`basename $$dirtoclean`;  \
  find $$WDIR \( -name '*'$$ldir'*.a' \) |  xargs rm -fr ; \
 done
endef
#
# Components
#
define clean_config
 echo  "\t[CLEANING] Configure, Make-related files and lists"; \
 if test -f config/stamps_and_lists/autoconf_generated_files.list; then \
  for file in `cat config/stamps_and_lists/autoconf_generated_files.list` ; do rm -fr $$file; done; fi; \
 if test -f config/stamps_and_lists/active_directories.list; then \
  for dir in `cat config/stamps_and_lists/active_directories.list` ; do \
   rm -fr $$dir/Makefile; \
   rm -fr $$dir/*.mk; \
  done;\
 fi;\
 rm -fr $(prefix)/config/stamps_and_lists/*.list;\
 rm -fr $(prefix)/*.log;\
 rm -fr $(prefix)/*.status;\
 rm -fr $(prefix)/autom4te.cache
endef
define clean_bin
 echo  "\t[CLEANING] bin" ;\
 rm -fr $(prefix)/bin/*; \
 rm -fr $(prefix)/compile*
endef
define clean_ext_libs
 echo  "\t[CLEANING] External libraries" ; \
 rm -fr  $(prefix)/lib/bin/*; \
 rm -fr $(prefix)/include/system
endef
define clean_stamps
 echo  "\t[CLEANING] Stamps" ; \
 rm -fr $(prefix)/config/stamps_and_lists/*keep*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/*2y.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/lib*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/yambo*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/ypp*.stamp
endef
define clean_dependencies
 echo  "\t[CLEANING] Dependencies" ; \
 find . \( -name '*.dep' -o -name '*.rules' -o -name 'modules.list' -o -name 'modulesdep.list' \) | xargs rm -f ;\
 rm -fr $(prefix)/config/stamps_and_lists/dependencies.stamp
endef
