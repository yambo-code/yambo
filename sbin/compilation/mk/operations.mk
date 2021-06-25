exe: $(objs)
	$(driver)
	$(link)
	$(modmove)
	$(dircheck)
	$(PREFIX)(mv $@ $(exec_prefix))
#
lib: $(objs)
	$(mk_lib) 
	$(modmove) 
#
objects_lock:
	$(mk_mod_dir)
	$(o_and_mod_clean)
	$(o_save)
#
#
# Sources that do not want optimization
#
$(F77_NOOPT_SRC):
	$(f77_no_opt_elemental_compilation)
$(FC_NOOPT_SRC):
	$(F90_no_opt_elemental_compilation)
#
# Special sources
#
$(FC_LOCAL_SRC):
	$(F90_local_elemental_compilation)
