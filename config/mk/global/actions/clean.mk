gitclean: clean_all
	@git clean -fdx
distclean: clean_all
clean_all: 
	@$(call clean_driver,all)
clean: 
	@$(call clean_driver,$(what))
	@$(call clean_driver,"stamps")
reset:
	@$(call clean_driver,"")
	@$(call clean_driver,"int-libs")
check: 
	@FILES=`git ls-files --others|grep -v .tar.gz|grep -v .sw`; for target in $$FILES ; do echo $$target; done
	@find . -empty -type d

