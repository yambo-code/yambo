clean_all: dependencies
	@$(call clean_driver,all)
clean: dependencies
	@$(call clean_driver,$(what))

