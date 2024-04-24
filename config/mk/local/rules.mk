#
# License-Identifier: GPL
#
# Copyright (C) 2021 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
# Suffixes
#
.SUFFIXES: .F .f90 .c .f .o .a
#
# Rules
#
.F.o:
	$(F90_elemental_compilation)
.f.o:
	$(f77_elemental_compilation)
.c.o:
	$(c_elemental_compilation)
