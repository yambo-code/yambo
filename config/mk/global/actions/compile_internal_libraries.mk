qe_pseudo: 
	@+LIBS="qe_pseudo"; BASE="lib" ; ADF="$(DOUBLE_PRECMP)"; LAB=""; $(todo_lib); $(mk_lib)
slatec: 
	@+LIBS="slatec";    BASE="lib" ; ADF="$(DOUBLE_PRECMP)"; LAB=""; $(todo_lib); $(mk_lib)
math77: 
	@+LIBS="math77";    BASE="lib" ; ADF="$(DOUBLE_PRECMP)"; LAB=""; $(todo_lib); $(mk_lib)
local: 
	@+LIBS="local" ;    BASE="lib" ; ADF="$(DOUBLE_PRECMP)"; LAB=""; $(todo_lib); $(mk_lib)

