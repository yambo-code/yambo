#
# License-Identifier: GPL
#
# Copyright (C) 2021 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
CLEANING_COMPONENTS=bin int-libs driver Ydrive src ypp interfaces conf dep
help:
	@$(ECHO)  "\n\n * * * Yambo Makefile help * * * \n" ;
	@$(ECHO)  "Interfaces: " ;
	@$(ECHO)  "=========== " 
	@$(ECHO)  " a2y =  ABINIT to Yambo interface \n"
	@$(ECHO)  " p2y =  QuantumEspresso to Yambo interface "
	@$(ECHO)  "        http://www.yambo-code.org/wiki/index.php?title=Bulk_material:_h-BN \n"
	@$(ECHO)  " c2y =  CPMD to Yambo interface \n"
	@$(ECHO)  "Main code: " ;
	@$(ECHO)  "========== " 
	@$(ECHO)  " yambo =  main Yambo code "
	@$(ECHO)  " ypp   =  Yambo Post Processing utility "
	@$(ECHO)  "\nOther projects: " ;
	@$(ECHO)  "=============== " 
	@$(ECHO)  " yambo_sc     =  Self-consistent (COHSEX, HF, DFT) project\n"
	@$(ECHO)  " yambo_rt     =  Real-time dynamics project"
	@$(ECHO)  "                 http://www.yambo-code.org/wiki/index.php?title=Linear_response_from_real_time_simulations\n"
	@$(ECHO)  " yambo_nl     =  Non-linear optics project "
	@$(ECHO)  "                 http://www.yambo-code.org/wiki/index.php?title=Tutorials#Non_linear_response\n"
	@$(ECHO)  " yambo_ph     =  Electron-phonon coupling project "
	@$(ECHO)  "                 http://www.yambo-code.org/wiki/index.php?title=Tutorials#Electron_phonon_coupling"
	@$(ECHO)  "\nCleaning: " ;
	@$(ECHO)  "========= " 
	@$(ECHO)  " The cleaning procedure of yambo is divided in several modules: $(CLEANING_COMPONENTS)"
	@$(ECHO)  " Each of these module can be called by using"
	@$(ECHO)  " make clean what=<MODULE> \n"
	@$(ECHO)  " make clean               =  remove all modules except int-libs, ext-libs, Ydriver, dependencies and configure files."
	@$(ECHO)  " make clean_all           =  remove all modules. Complete cleaning."
	@$(ECHO)  " make reset               =  clean  + int-libs."
	@$(ECHO)  "\n";
