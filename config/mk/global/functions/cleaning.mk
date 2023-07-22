#
# Drivers (top)
#
define clean_driver
 if [ "$(1)" = "archive"   ] ||                  [ "$(1)" = "all" ] ; then $(clean_archive); fi ;\
 if [ "$(1)" = "bin"       ] || [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then $(clean_bins); fi;\
 if [ "$(1)" = "int-libs"  ] ||                  [ "$(1)" = "all" ] ; then \
   EXTS="\.f90 \.o \.lock \.mk \.mod \.save \.tmp_source to_save";WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_dir_driver); \
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
 if [ "$(1)" = "projects-stamp" ] ; then $(clean_projects_stamp); fi; \
 if [ "$(1)" = "driver"    ] || [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then \
  EXTS="\.f90 \.o \.lock \.mk \.mod \.save \.tmp_source to_save";WDIR="$(compdir)";TARG="driver";$(clean_dir_driver);\
 fi
 if [ "$(1)" = "Ydriver"   ] ||                  [ "$(1)" = "all" ] ; then \
   EXTS="\.f90 \.o \.lock \.mk \.mod \.save \.tmp_source to_save";WDIR="$(libdir)/yambo/Ydriver/src";TARG="$(YLIBDRIVER)";$(clean_dir_driver);\
   WDIR="$(libdir)";TARG="Ydriver";$(clean_lib_driver);\
   WDIR="$(libdir)/Ydriver/src";TARG="$(YLIBDRIVER)";$(clean_mod_driver);\
 fi;\
 if                             [ -z "$(1)" ] || [ "$(1)" = "all" ] ; then \
   $(clean_libs_using_stamps_driver); \
   $(call clean_src_driver,"src");\
   $(call clean_src_driver,"ypp");\
   $(call clean_src_driver,"interfaces");\
 fi; \
 if [ "$(1)" = "ypp"        ] ; then $(call clean_src_driver,"ypp"); fi; \
 if [ "$(1)" = "src"        ] ; then $(call clean_src_driver,"src"); fi; \
 if [ "$(1)" = "interfaces" ] ; then $(call clean_src_driver,"interfaces"); fi; \
 if [ "$(1)" = "conf"      ] ||                  [ "$(1)" = "all" ] ; then $(clean_config); fi; \
 if [ "$(1)" = "dep"       ] ||                  [ "$(1)" = "all" ] ; then $(clean_dependencies); fi ; \
 if                                              [ "$(1)" = "all" ] ; then $(clean_log_and_Ydriver_folder); fi 
endef
#
# Drivers (intermediate)
#
define clean_src_driver
 if [ "$(1)" = "src" ] || [ "$(1)" = "ypp" ] || [ "$(1)" = "interfaces" ] ; then \
  if  test -f config/stamps_and_lists/active_directories.list; then \
   TARG="";MSG="$(1)";EXTS="\.f90 \.o \.lock \.mk \.mod \.save \.tmp_source to_save";WDIR="$(compdir)";\
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
define clean_libs_using_stamps_driver
 $(ECHO) "\t[CLEANING] Stamped Libraries" ; \
 for file in $(prefix)/config/stamps_and_lists/lib*stamp; do \
  ldir=`basename $$file | sed 's/.stamp//'`; \
  rm -f $(prefix)/lib/$$ldir; \
  rm -f $$file; \
 done
endef
define clean_lib_driver
 if test -n "$$MSG"; then LMSG="$$MSG"; else LMSG="$$TARG";fi; \
 $(ECHO) "\t[CLEANING $$LMSG] Libraries" ; \
 for dirtoclean in $$TARG; do \
  ldir=`basename $$dirtoclean`;  \
  if test -d $$dirtoclean; then find $$dirtoclean \( -name '*'$$ldir'*.a' \) |  xargs rm -fr ; fi; \
  if test -d $$WDIR/$$dirtoclean; then find $$WDIR \( -name '*'$$ldir'*.a' \) |  xargs rm -fr ; fi; \
  if [ "$$MSG" != "ypp"    ]; then rm -f $(prefix)/config/stamps_and_lists/lib"$$ldir.a.stamp"; fi; \
  if [ "$$MSG"  = "ypp"    ]; then rm -f $(prefix)/config/stamps_and_lists/lib_ypp_"$$ldir.a.stamp"; fi; \
 done
endef
#
# Components
#
define clean_config
 $(ECHO) "\t[CLEANING] Configure, Make-related files and lists"; \
 if test -f $(prefix)/config/stamps_and_lists/autoconf_generated_files.list; then \
  for file in `cat $(prefix)/config/stamps_and_lists/autoconf_generated_files.list` ; do rm -fr $$file; done; fi; \
 if test -f $(prefix)/config/stamps_and_lists/active_directories.list; then \
  for dir in `cat $(prefix)/config/stamps_and_lists/active_directories.list` ; do \
   rm -fr $$dir/Makefile; \
   rm -fr $$dir/*.mk; \
  done;\
 fi;\
 rm -fr $(prefix)/config/stamps_and_lists/*.list;\
 rm -fr $(prefix)/config/msg_ydb;\
 rm -fr $(prefix)/config/setup;\
 rm -fr $(prefix)/config/report;\
 rm -fr $(prefix)/config/mk/defs.mk $(prefix)/config/mk/local/defs.mk $(prefix)/config/mk/global/defs.mk;   \
 rm -fr $(prefix)/bin;\
 rm -fr $(prefix)/*.log;\
 rm -fr $(prefix)/*.status;\
 rm -fr $(prefix)/autom4te.cache;\
 rm -fr $(prefix)/include/version/version.h;\
 rm -fr $(prefix)/config/mk/local/static_variables.mk;\
 rm -fr $(prefix)/lib/archive/Makefile;\
 rm -fr $(prefix)/src/tools/.objects;\
 rm -fr $(prefix)/src/wf_and_fft/sgfft.F;\
 rm -fr $(prefix)/lib/archive/git.list;\
 rm -fr $(prefix)/sbin/compilation/helper.inc.sh
endef
define clean_bins
 $(ECHO) "\t[CLEANING] bin(s)" ;\
 for file in $(prefix)/bin/*; do \
  exe=`basename $$file`;\
  rm -f $(prefix)/bin/$$exe; \
  rm -f $(prefix)/config/stamps_and_lists/"$$exe.stamp"; \
 done
endef
define clean_ext_libs_bin_and_include
 $(ECHO) "\t[CLEANING external-libraries] bin(s) and include(s)" ; \
 rm -fr $(prefix)/lib/bin/*; \
 rm -fr $(prefix)/include/system; \
 rm -fr $(prefix)/include/branch.inc; \
 rm -fr $(prefix)/include/driver/editor.h; \
 rm -fr $(prefix)/include/headers/common/have_malloc.h
endef
define clean_stamps
 $(ECHO) "\t[CLEANING] Stamps" ; \
 rm -fr $(prefix)/config/stamps_and_lists/*keep*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/*2y.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/yambo*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/ypp*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/compiling*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/mods*.stamp; \
 rm -fr $(prefix)/config/stamps_and_lists/*.lock;\
 rm -fr $(prefix)/include/*.save
endef
define clean_projects_stamp
 $(ECHO) "\t[CLEANING] Project stamps" ; \
 rm -fr $(prefix)/config/stamps_and_lists/project_dependencies.stamp
endef
define clean_dependencies
 $(ECHO) "\t[CLEANING] Dependencies" ; \
 find . \( -name '*.rules' -o -name 'modules.list' -o -name 'modulesdep.list'\
           -o -name 'global_modules_dep.list' -o -name 'local_modules*.dep' \) | xargs rm -f ;\
 rm -fr $(prefix)/config/stamps_and_lists/dependencies.stamp
endef
define clean_project_dependencies_stamp
 if [ "$(1)" = "update"  ]; then $(ECHO) "\t[CLEANING] Project dependencies stamp" ; \
 rm -f $(prefix)/config/stamps_and_lists/dependencies.stamp; \
 rm -f $(prefix)/config/stamps_and_lists/project_dependencies.stamp; \
 fi
endef
define clean_log_and_Ydriver_folder
 $(ECHO) "\t[CLEANING] folders and log" ; \
 rm -fr $(srcdir)/lib/yambo;\
 rm -fr $(prefix)/lib/yambo;\
 rm -fr $(prefix)/log 
endef
define clean_archive
 $(ECHO) "\t[CLEANING] Libraries archive" ; \
 CWD=`pwd`;\
 cd lib/archive;  $(MAKE) -s -f Makefile.loc  clean; cd $$CWD
endef

