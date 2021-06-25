new_clean: lib_clean o_clean setup_clean

setup_clean:
	@$(stamps_clean)
	@$(dependencies_clean)

o_clean:
	@$(o_clean)

lib_clean:
	@$(o_int-lib_clean)
	@$(a_int-lib_clean)

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
