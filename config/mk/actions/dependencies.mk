dependencies:  
	@if ! test -e $(topdir)/config/stamps_and_lists/dependencies.stamp; then ./sbin/compilation/helper.sh -D; fi
	@touch $(topdir)/config/stamps_and_lists/dependencies.stamp
