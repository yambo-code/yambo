#
# License-Identifier: GPL
#
# Copyright (C) 2020 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
Ydriver:Ydriver-dl
Yio: ext-libs int-libs
	@+LIBS="$(YLIBIO)"; BASE="src"; ADF="-D_io_lib"; $(todo_lib); $(mk_lib)
