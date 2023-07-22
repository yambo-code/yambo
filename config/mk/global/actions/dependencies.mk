dependencies: 
	@$(call clean_project_dependencies_stamp,$(what))
	@LIB2DO="Ydriver"; $(get_external_libraries)
	@if ! test -e $(compdir)/config/stamps_and_lists/dependencies.stamp; then ./sbin/compilation/helper.sh -D -N $(MAKEFLAGS);\
	touch $(compdir)/config/stamps_and_lists/dependencies.stamp; fi
