dependencies:  
	@if ! test -e $(topdir)/config/stamps/dependencies.stamp; then ./sbin/compilation/helper.sh -D; fi
	@touch $(topdir)/config/stamps/dependencies.stamp
