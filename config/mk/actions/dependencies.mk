dependencies:  
	@if ! test -e $(compdir)/config/stamps_and_lists/dependencies.stamp; then ./sbin/compilation/helper.sh -D; fi
	@touch $(compdir)/config/stamps_and_lists/dependencies.stamp
