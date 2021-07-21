qe_pseudo: dependencies
	@$(global_check)
	@+LIBS="qe_pseudo"; BASE="lib" ; ADF="$(DOUBLE_PRECMP)"; LAB=""; $(todo_lib); $(mk_lib)
slatec: dependencies
	@+LIBS="slatec";    BASE="lib" ; ADF="$(DOUBLE_PRECMP)"; LAB=""; $(todo_lib); $(mk_lib)
math77: dependencies
	@+LIBS="math77";    BASE="lib" ; ADF="$(DOUBLE_PRECMP)"; LAB=""; $(todo_lib); $(mk_lib)
local: dependencies
	@+LIBS="local" ;    BASE="lib" ; ADF="$(DOUBLE_PRECMP)"; LAB=""; $(todo_lib); $(mk_lib)

