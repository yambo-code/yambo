exe: $(objs)
	$(driver)
	$(link)
	$(modmove)
	$(bindir)
	$(PREFIX)(if test -f $(target); then mv $(target) $(exec_prefix);fi)
#
lib: $(objs) 
	$(mk_lib) 
	$(modmove) 
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
