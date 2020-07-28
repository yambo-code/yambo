define makefiles_clean
 find . \( -name 'Makefile' -o -name '*make.dep' \) -type f -print | grep -v '\.\/Makefile' | \
        grep -v '.*yaml.*\/Makefile' |  \
        grep -v '.*futile.*\/Makefile' |  \
        grep -v '.*iotk.*\/Makefile' |  \
        grep -v '.*etsf_io.*\/Makefile' | \
        grep -v '.*hdf5.*\/Makefile' | grep -v '.*netcdf.*\/Makefile' | grep -v '.*libxc.*\/Makefile' | \
        grep -v '.*fftw.*\/Makefile' | grep -v '.*fftqe.*\/Makefile' | grep -v '.*driver\/Makefile'| xargs rm -f
 echo "[CLEAN] Makefiles ... done"
endef
define objects_clean
 find . \( -name '*.o' -o -name '*.f90' \
        -o -name '*_cpp.f' -o -name 'ifc*' -o -name '__*' -o -name '*.s' -o -name 'penmp' \) \
        -type f -print | \
        grep -v '.*yaml.*\/*f90' | \
        grep -v '.*futile.*\/*f90' | \
        grep -v '.*iotk.*\/*f90' | \
        grep -v '.*etsf_io.*\/*f90' | \
        grep -v '.*lapack*' | grep -v '.*blacs*' |  \
        grep -v '.*scalapack*' | grep -v '.*/lib/slepc*' | \
        grep -v '.*/lib/petsc*'  | xargs rm -f
 echo "[CLEAN] Objects ... done"
 echo "[CLEAN] Broken files ... done"
 if test "$(keep_objs)" = yes ; then \
 find . -name '.objects__lock*' | xargs rm -fr ; \
 echo "[CLEAN] Objects locks and directories ... done" ; \
 fi
endef
define lib_ext_clean
 rm -f $(STAMPS)
 find . \( -name '*.a' -o -name '*.la' -o -name '*.mod' \
           -o -name 'H5*.h' -o -name 'hdf5*.h' -o -name 'netcdf*h' -o -name 'netcdf*inc' \
           -o -name 'fftw3*h' -o -name 'fftw3*f' -o -name 'fftw*f03' \) -type f -print | xargs rm -f
 find . -name 'xc*.h' -type f -print | xargs rm -f
 echo "[CLEAN] External Libraries ... " 
 @for libtoclean in "libxc" "yaml" "futile" "iotk" "netcdff" "netcdf" "hdf5" "etsf_io" "lapack" "blacs" "scalapack" "petsc" "slepc" "fftw" "fftqe" ; do \
  if test -d $(libdir)/$$libtoclean ; then \
   echo "[CLEAN] ... $$libtoclean" ; \
   cd $(libdir)/$$libtoclean ; \
   $(MAKE) -s -f Makefile.loc clean_all > /dev/null ; rm -f Makefile *stamp *.inc ; \
   cd ../.. ; \
   if [ "$(topdir)" != "$(prefix)" ] ; then rm -r $(libdir)/$$libtoclean ; fi ; \
  fi ; \
 done
 @if test -d $(libdir)/archive ; then \
  cd $(libdir)/archive; \
  $(MAKE) -s -f Makefile.loc clean_all > /dev/null ; rm -f Makefile *stamp ; \
  cd ../.. ; \
 fi
endef
define sysincs_clean
 @if test -d $(prefix)/include; then rm -f $(prefix)/include/*.h ; fi 
 @if test -d $(prefix)/include/system/; then \
   rm -rf $(prefix)/include/system ; \
   echo "[CLEAN] Libraries ... done"; \
 fi 
endef
define lib_ext_remove
 @if test -f $(libs_prefix)/../../driver/driver.c; then \
  echo "[CLEAN] Pre-compiled internal libraries ... done"; \
  rm -fr "$(libs_prefix)"; \
 fi
 @if test -f $(prefix)/include/system/../../driver/driver.c; then \
  echo "[CLEAN] Local include directory ... done"; \
  rm -fr "$(prefix)/include/system"; \
 fi
 @if test -f $(prefix)/scripts/../driver/driver.c; then \
  echo "[CLEAN] Local scripts directory ... done"; \
  rm -fr "$(prefix)/scripts"; \
 fi
endef
define lib_mod_clean
 find . \( -name '*.a' -o -name '*.la' -o -name '*.mod' \) -type f -print | \
       grep -v hdf5 | grep -v netcdf | grep -v xc | grep -v yaml | grep -v futile | grep -v iotk | grep -v typesize | grep -v etsf_io | grep -v fftw | xargs rm -f 
 @if test -d $(libbindir); then \
  cd $(libbindir) ; rm -f * ; cd .. ; \
 fi
 for folder in $(includedir)/modules__*; do \
  if test -d $$folder ; then \
   cd $$folder ; rm -f * ; cd .. ; rmdir $$folder; \
  fi \
 done
 echo "[CLEAN] Libraries ... done" 
 echo "[CLEAN] Modules ... done" 
endef
define xclean
 for exe in $(EXE); do rm -f $(bindir)/$$exe; done
 @if test -d $(bindir); then \
  cd $(bindir) ; rm -f * ; cd .. ; rmdir $(bindir); \
 fi
 echo "[CLEAN] Targets ... done" 
endef
define conf_clean
 rm -f $(CFGFILES)
 rm -f config.status config.log
 rm -fr autom4te.cache
 echo "[CLEAN] Autoconf files ... done" 
endef
