#
# Drivers (top)
#
define clean_driver
 if [ "$(1)" = "bin"       ] || [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then $(clean_bin); fi;\
 if [ "$(1)" = "int-libs"  ] ||                  [ "$(1)" = "all" ] ; then \
   EXTS="\.f90 \.o \.lock \.mk \.mod \.save \.tmp_source";WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_dir_driver); \
   WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_lib_driver); \
   WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_mod_driver); \
 fi;\
 if [ "$(1)" = "ext-libs"  ] || [ -z "$(1)" ]                       ; then \
   ACTION="clean";WDIR="$(libdir)";TARG="$(EXT_LIBS)";$(clean_ext_lib_dir); \
 fi; \
 if [ "$(1)" = "ext-libs"  ] ||                  [ "$(1)" = "all" ] ; then \
   ACTION="clean_all";WDIR="$(libdir)";TARG="$(EXT_LIBS)";$(clean_ext_lib_dir); \
   $(clean_ext_libs_bin_and_include); \
   EXTS="Makefile .stamp";WDIR="$(libdir)";TARG="$(EXT_LIBS)";$(clean_dir_driver); \
 fi; \
 if [ "$(1)" = "stamps"    ] || [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then $(clean_stamps); fi; \
 if [ "$(1)" = "driver"    ] || [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then \
  EXTS="\.f90 \.o \.lock \.mk \.mod \.save \.tmp_source";WDIR="$(compdir)";TARG="driver";$(clean_dir_driver);\
 fi
 if [ "$(1)" = "Ydriver"   ] ||                  [ "$(1)" = "all" ] ; then \
   EXTS="\.f90 \.o \.lock \.mk \.mod \.save \.tmp_source";WDIR="$(libdir)/yambo/driver/src";TARG="$(YLIBDRIVER)";$(clean_dir_driver);\
   WDIR="$(libdir)";TARG="Ydriver";$(clean_lib_driver);\
   WDIR="$(libdir)/yambo/driver/src";TARG="$(YLIBDRIVER)";$(clean_mod_driver);\
 fi;\
 if                             [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then \
   $(call clean_src_driver,"src");\
   $(call clean_src_driver,"ypp");\
   $(call clean_src_driver,"interfaces");\
 fi; \
 if [ "$(1)" = "ypp"        ] ; then $(call clean_src_driver,"ypp"); fi; \
 if [ "$(1)" = "src"        ] ; then $(call clean_src_driver,"src"); fi; \
 if [ "$(1)" = "interfaces" ] ; then $(call clean_src_driver,"interfaces"); fi; \
 if [ "$(1)" = "conf"      ] ||                  [ "$(1)" = "all" ] ; then $(clean_config); fi; \
 if [ "$(1)" = "dep"       ] ||                  [ "$(1)" = "all" ] ; then $(clean_dependencies); fi 
endef
#
# Drivers (intermediate)
#
define clean_src_driver
 if [ "$(1)" = "src" ] || [ "$(1)" = "ypp" ] || [ "$(1)" = "interfaces" ] ; then \
  if  test -f config/stamps_and_lists/active_directories.list; then \
   TARG="";MSG="$(1)";EXTS="\.f90 \.o \.lock \.mk \.mod \.save \.tmp_source";WDIR="$(compdir)";\
   for FOLD in `cat config/stamps_and_lists/active_directories.list|grep $(1)`;do TARG="$$TARG $$FOLD";done;\
   $(clean_dir_driver);$(clean_lib_driver);$(clean_mod_driver); \
  fi;\
  rm -f $(compdir)/config/stamps_and_lists/compilation_*.stamp; \
 fi
endef
define clean_dir_driver
 if test -n "$$MSG"; then LMSG="$$MSG"; else LMSG="$$TARG";fi; \
 $(ECHO) "\t[CLEANING $$LMSG] Extension(s): $$EXTS" ; \
 for ext in $$EXTS; do \
  for dirtoclean in $$TARG; do \
   if test -d $$WDIR/$$dirtoclean; then\
    find $$WDIR/$$dirtoclean \( -name '*'$$ext  \) |  xargs rm -fr ; \
   fi;\
  done;\
 done
endef
define clean_ext_lib_dir
 $(ECHO) -n "\t[CLEANING external-libraries]" ; \
 for dirtoclean in $$TARG; do \
  if test -f $$WDIR/$$dirtoclean/Makefile.loc; then \
   $(ECHO) -n " $$dirtoclean" ; \
   CWD=`pwd`;\
   cd $$WDIR/$$dirtoclean;\
   $(MAKE) -s -f Makefile.loc $$ACTION;\
   cd $$CWD;\
  fi;\
 done; \
 find lib/archive/* -type d  |xargs rm -fr; \
 $(ECHO)
endef
define clean_mod_driver
 if test -n "$$MSG"; then LMSG="$$MSG"; else LMSG="$$TARG";fi; \
 $(ECHO) "\t[CLEANING $$LMSG] Modules" ; \
 for dirtoclean in $$TARG; do \
  if test -f $$WDIR/$$dirtoclean/modules.list; then \
   for file in `cat $$WDIR/$$dirtoclean/modules.list`; do rm -fr $(includedir)/$$file".mod"; done; \
  fi; \
 done
endef
define clean_lib_driver
 if test -n "$$MSG"; then LMSG="$$MSG"; else LMSG="$$TARG";fi; \
 $(ECHO) "\t[CLEANING $$LMSG] Libraries" ; \
 for dirtoclean in $$TARG; do \
  ldir=`basename $$dirtoclean`;  \
  if test -d $$dirtoclean; then find $$dirtoclean \( -name '*'$$ldir'*.a' \) |  xargs rm -fr ; fi; \
  if test -d $$WDIR/$$dirtoclean; then find $$WDIR/$$dirtoclean \( -name '*'$$ldir'*.a' \) |  xargs rm -fr ; fi; \
  rm -f $(prefix)/config/stamps_and_lists/lib"$$ldir.a.stamp"; \
 done
endef
#
# Components
#
define clean_config
 $(ECHO) "\t[CLEANING] Configure, Make-related files and lists"; \
 if test -f config/stamps_and_lists/autoconf_generated_files.list; then \
  for file in `cat config/stamps_and_lists/autoconf_generated_files.list` ; do rm -fr $$file; done; fi; \
 if test -f config/stamps_and_lists/active_directories.list; then \
  for dir in `cat config/stamps_and_lists/active_directories.list` ; do \
   rm -fr $$dir/Makefile; \
   rm -fr $$dir/*.mk; \
  done;\
 fi;\
 rm -fr $(prefix)/config/stamps_and_lists/*.list;\
 rm -fr $(prefix)/log;\
 rm -fr $(prefix)/bin;\
 rm -fr $(prefix)/*.log;\
 rm -fr $(prefix)/*.status;\
 rm -fr $(prefix)/autom4te.cache;\
 rm -fr $(prefix)/config/mk/local/static_variables.mk;\
 rm -fr $(prefix)/lib/archive/Makefile
endef
define clean_bin
 $(ECHO) "\t[CLEANING] bin" ;\
 for file in $(prefix)/bin/*; do \
  exe=`basename $$file`;\
  rm -f $(prefix)/bin/$$exe; \
  rm -f $(prefix)/config/stamps_and_lists/"$$exe.stamp"; \
 done;\
 rm -fr $(prefix)/log/*
endef
define clean_ext_libs_bin_and_include
 $(ECHO) "\t[CLEANING external-libraries] bin(s) and include(s)" ; \
 rm -fr $(prefix)/lib/bin/*; \
 rm -fr $(prefix)/include/system
endef
define clean_stamps
 $(ECHO) "\t[CLEANING] Stamps" ; \
 rm -fr $(prefix)/config/stamps_and_lists/*keep*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/*2y.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/lib*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/yambo*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/ypp*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/compiling*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/*.lock;\
 rm -fr $(prefix)/include/*.save
endef
define clean_dependencies
 $(ECHO) "\t[CLEANING] Dependencies" ; \
 find . \( -name '*.dep' -o -name '*.rules' -o -name 'modules.list' -o -name 'modulesdep.list'\
           -o -name 'global_modules_dep.list' \) | xargs rm -f ;\
 rm -fr $(prefix)/config/stamps_and_lists/dependencies.stamp
endef
