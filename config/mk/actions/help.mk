help:
	@echo "\n\n * * * Yambo Makefile help * * * \n" ;
	@echo "Interfaces: " ;
	@echo "=========== " 
	@echo " a2y =  ABINIT to Yambo interface \n"
	@echo " p2y =  QuantumEspresso to Yambo interface "
	@echo "        http://www.yambo-code.org/wiki/index.php?title=Bulk_material:_h-BN \n"
	@echo " c2y =  CPMD to Yambo interface \n"
	@echo "Main code: " ;
	@echo "========== " 
	@echo " yambo =  main Yambo code "
	@echo " ypp   =  Yambo Post Processing utility "
	@echo "\nOther projects: " ;
	@echo "=============== " 
	@echo " yambo_sc =  Self-consistent (COHSEX, HF, DFT) project\n"
	@echo " yambo_rt =  Real-time dynamics project"
	@echo "             http://www.yambo-code.org/wiki/index.php?title=Linear_response_from_real_time_simulations\n"
	@echo " yambo_nl =  Non-linear optics project "
	@echo "             http://www.yambo-code.org/wiki/index.php?title=Tutorials#Non_linear_response\n"
	@echo " yambo_ph =  Electron-phonon coupling project "
	@echo "             http://www.yambo-code.org/wiki/index.php?title=Tutorials#Electron_phonon_coupling"
	@echo "\nCleaning: " ;
	@echo "========= " 
	@echo " make clean_fast =  remove all Yambo objects, libraries and modules "
	@echo " make clean      =  clean_fast + remove makefiles, and executables "
	@echo " make clean_all  =  clean + remove external libraries and configuration files "
	@echo " make distclean  =  clean_all + remove external libraries source files "
	@echo "\n";
