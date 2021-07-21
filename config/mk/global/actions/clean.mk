clean_all: dependencies
	@$(call clean_driver,all)
clean: dependencies
	@$(call clean_driver,$(what))
reset: dependencies
	@$(call clean_driver,"")
	@$(call clean_driver,"int-libs")
check: 
	@git ls-files --others | grep -v .sw | grep -v tar.gz
	@find . -empty -type d
