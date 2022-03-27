Yio: ext-libs int-libs
	@+LIBS="$(YLIBIO)"; BASE="src"; ADF="-D_io_lib"; $(todo_lib); $(mk_lib)
