define mk_exe
 LLIBS="";for exe in $$XLIBS; do LLIBS="$$LLIBS -l$$exe" ; done ; \
 for exe in $$X_ypp_LIBS; do LLIBS="$$LLIBS -l_ypp_$$exe" ; done ; \
 for exe in $$X2DO; do \
  if test ! -f $(compdir)/config/stamps_and_lists/$$exe.stamp && test ! -f  $(compdir)/config/stamps_and_lists/compilation_stop_$$exe.stamp; then \
   DLIBS="-ldriver";for lib in $(YLIBDRIVER_LD); do DLIBS="$$DLIBS -l$$exe$$lib" ; done ; \
   ./sbin/compilation/helper.sh -d $$BASE -t $$exe -o .objects -m x -g $@ -- "$$DLIBS $$LLIBS $(xcpp) $$ADF"; \
   cd $$BASE ; $(MAKE) $(MAKEFLAGS) VPATH=$(srcdir)/$$BASE exe || { grep Error $(compdir)/log/compile_$@.log ; exit "$$?"; } ; cd $(compdir); \
  fi;\
  if test ! -f $(compdir)/config/stamps_and_lists/$$exe.stamp; then \
   echo "$$exe linking failed. Check log/compile_$$exe.log";\
   grep Error $(compdir)/log/compile_$$exe.log;\
   exit 1;\
  fi;\
 done
endef
