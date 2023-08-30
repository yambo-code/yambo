#
# License-Identifier: GPL
#
# Copyright (C) 2020 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
qe_pseudo: 
	@+LIBS="qe_pseudo"; BASE="lib" ; ADF="$(STAMP_DBLE)"; LAB=""; $(todo_lib); $(mk_lib)
slatec: 
	@+LIBS="slatec";    BASE="lib" ; ADF="$(STAMP_DBLE)"; LAB=""; $(todo_lib); $(mk_lib)
math77: 
	@+LIBS="math77";    BASE="lib" ; ADF="$(STAMP_DBLE)"; LAB=""; $(todo_lib); $(mk_lib)
local: 
	@+LIBS="local" ;    BASE="lib" ; ADF="$(STAMP_DBLE)"; LAB=""; $(todo_lib); $(mk_lib)
