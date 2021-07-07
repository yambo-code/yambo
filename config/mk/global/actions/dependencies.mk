dependencies:  
	@if ! test -e $(compdir)/config/stamps_and_lists/dependencies.stamp; then ./sbin/compilation/helper.sh -D -N $(MAKEFLAGS); fi
	@touch $(compdir)/config/stamps_and_lists/dependencies.stamp
