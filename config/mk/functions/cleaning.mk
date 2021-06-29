define clean_configure
 echo  "\t[CLEANING] Configure, Make-related files and lists"; \
 for file in `cat config/stamps_and_lists/autoconf_generated_files.list` ; do rm -f $$file; done;
 for dir in `cat config/stamps_and_lists/active_directories.list` ; do \
  rm -f $$dir/Makefile; \
  rm -f $$dir/*.mk; \
 done; \
 rm -f $(prefix)/config/stamps_and_lists/*.list \
 rm -f $(prefix)/*.log \
 rm -f $(prefix)/*.status 
endef
define clean_bin
 echo  "\t[CLEANING] bin"; \
 rm -f $(prefix)/bin/*
endef
define clean_ext_libs
 echo  "\t[CLEANING external libs]" ; \
 rm -f  $(prefix)/lib/bin/*; \
 rm -fr $(prefix)/include/system
endef
define clean_stamps
 echo  "\t[CLEANING] Stamps" ; \
 rm -f $(prefix)/config/stamps_and_lists/lib*.stamp; \
 rm -f $(prefix)/config/stamps_and_lists/yambo*.stamp; \
 rm -f $(prefix)/config/stamps_and_lists/ypp*.stamp
endef
define clean_dependencies
 echo  "\t[CLEANING] Depependencies" ; \
 find . \( -name '*.dep' -o -name '*.rules' -o -name 'modules.list' -o -name 'modulesdep.list' \) | xargs rm -f ;\
 rm -f $(prefix)/config/stamps_and_lists/dependencies.stamp;
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
   for file in `cat $$WDIR/$$dirtoclean/modules.list`; do rm -f $(includedir)/$$file".mod"; done; \
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
