clean:     clean_Ydriver clean_ypp clean_src clean_stamps
distclean: clean_new clean_ext_libs clean_libs clean_stamps clean_deps clean_configure
#
clean_ext_libs:
	@$(clean_ext_libs)
clean_configure:
	@$(clean_configure)
clean_stamps:
	@$(clean_stamps)
clean_deps:
	@$(clean_dependencies)
clean_driver:
	@(EXTS=".f90 .o .tmp_source";WDIR="$(topdir)";TARG="driver";$(clean_dir_driver))
clean_Ydriver:
	@(EXTS=".f90 .o .tmp_source";WDIR="$(libdir)/yambo/driver/src";TARG="$(YLIBDRIVER)";$(clean_dir_driver))
	@(WDIR="$(libdir)";TARG="Ydriver";$(clean_lib_driver))
	@(WDIR="$(libdir)/yambo/driver/src";TARG="$(YLIBDRIVER)";$(clean_mod_driver))
clean_ypp: clean_stamps clean_driver
	@(TARG="";MSG="ypp";EXTS=".f90 .o .tmp_source";WDIR="$(topdir)";\
	 for FOLD in `cat config/stamps_and_lists/active_directories.list|grep ypp`;do TARG="$$TARG $$FOLD";done;\
         $(clean_dir_driver);$(clean_lib_driver);$(clean_mod_driver))
clean_src: clean_stamps clean_driver
	@(TARG="";MSG="ypp";EXTS=".f90 .o .tmp_source";WDIR="$(topdir)";\
	 for FOLD in `cat config/stamps_and_lists/active_directories.list|grep src`;do TARG="$$TARG $$FOLD";done;\
         $(clean_dir_driver);$(clean_lib_driver);$(clean_mod_driver))
clean_libs: clean_stamps clean_driver
	@(EXTS=".f90 .o .tmp_source";WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_dir_driver))
	@(WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_lib_driver))
	@(WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_mod_driver))
