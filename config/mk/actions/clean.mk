clean_new:     clean_Ydriver clean_ypp clean_src 
clean_all_new: clean_new clean_ext_libs clean_libs clean_setup clean_configure
#
clean_ext_libs:
	@$(clean_ext_libs)
clean_configure:
	@$(clean_configure)
clean_setup:
	@$(clean_stamps)
	@$(clean_dependencies)
clean_Ydriver:
	@(EXTS=".f90 .o .tmp_source";WDIR="$(libdir)/yambo/driver/src";TARG="$(YLIBDRIVER)";$(clean_ext_driver))
	@(WDIR="$(libdir)";TARG="Ydriver";$(clean_lib_driver))
	@(WDIR="$(libdir)/yambo/driver/src";TARG="$(YLIBDRIVER)";$(clean_mod_driver))
clean_ypp:
	@(MSG="ypp";EXTS=".f90 .o .tmp_source";WDIR="$(topdir)/ypp";for FOLD in $(YPP_LIBS);do TARG="$$TARG $$FOLD";done;$(clean_ext_driver))
	@(MSG="ypp";WDIR="$(topdir)/ypp";for FOLD in $(YPP_LIBS);do TARG="$$TARG $$FOLD";done;$(clean_lib_driver))
	@(MSG="ypp";WDIR="$(topdir)/ypp";for FOLD in $(YPP_LIBS);do TARG="$$TARG $$FOLD";done;$(clean_mod_driver))
clean_src:
	@(MSG="src";EXTS=".f90 .o .tmp_source";WDIR="$(topdir)/src";for FOLD in $(MAIN_LIBS);do TARG="$$TARG $$FOLD";done;$(clean_ext_driver))
	@(MSG="src";WDIR="$(topdir)/src";for FOLD in $(MAIN_LIBS);do TARG="$$TARG $$FOLD";done;$(clean_lib_driver))
	@(MSG="src";WDIR="$(topdir)/src";for FOLD in $(MAIN_LIBS);do TARG="$$TARG $$FOLD";done;$(clean_mod_driver))
clean_libs:
	@(EXTS=".f90 .o .tmp_source";WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_ext_driver))
	@(WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_lib_driver))
	@(WDIR="$(libdir)";TARG="$(INT_LIBS)";$(clean_mod_driver))





clean_fast: 
	@$(objects_clean)
	@$(lib_mod_clean)
	@$(xclean)
clean:
	@$(objects_clean)
	@$(makefiles_clean)
	@$(lib_mod_clean)
	@$(sysincs_clean)
	@$(xclean)
clean_all:
	@$(objects_clean)
	@$(makefiles_clean)
	@$(lib_mod_clean)
	@$(lib_ext_clean)
	@$(sysincs_clean)
	@$(conf_clean)
	@$(xclean)
remove_ext_libs: 
	@$(lib_ext_remove)
distclean: clean_all remove_ext_libs
