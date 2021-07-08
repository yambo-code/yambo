CLEANING_COMPONENTS=bin int-libs driver Ydrive src ypp interfaces conf dep
help:
	@echo -e "\n\n * * * Yambo Makefile help * * * \n" ;
	@echo -e "Interfaces: " ;
	@echo -e "=========== " 
	@echo -e " a2y =  ABINIT to Yambo interface \n"
	@echo -e " p2y =  QuantumEspresso to Yambo interface "
	@echo -e "        http://www.yambo-code.org/wiki/index.php?title=Bulk_material:_h-BN \n"
	@echo -e " c2y =  CPMD to Yambo interface \n"
	@echo -e "Main code: " ;
	@echo -e "========== " 
	@echo -e " yambo =  main Yambo code "
	@echo -e " ypp   =  Yambo Post Processing utility "
	@echo -e "\nOther projects: " ;
	@echo -e "=============== " 
	@echo -e " yambo_sc =  Self-consistent (COHSEX, HF, DFT) project\n"
	@echo -e " yambo_rt =  Real-time dynamics project"
	@echo -e "             http://www.yambo-code.org/wiki/index.php?title=Linear_response_from_real_time_simulations\n"
	@echo -e " yambo_nl =  Non-linear optics project "
	@echo -e "             http://www.yambo-code.org/wiki/index.php?title=Tutorials#Non_linear_response\n"
	@echo -e " yambo_ph =  Electron-phonon coupling project "
	@echo -e "             http://www.yambo-code.org/wiki/index.php?title=Tutorials#Electron_phonon_coupling"
	@echo -e "\nCleaning: " ;
	@echo -e "========= " 
	@echo -e " The cleaning procedure of yambo is divided in several modules: $(CLEANING_COMPONENTS)"
	@echo -e " Each of these module can be called by using"
	@echo -e " make clean what=<MODULE> \n"
	@echo -e " make clean               =  remove all modules except int-libs, ext-libs, Ydriver, dependencies and configure files."
	@echo -e " make clean_all           =  remove all modules. Complete cleaning."
	@echo -e "\n";
