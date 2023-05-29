#
#        Copyright (C) 2000-2021 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM
#
# This file is distributed under the terms of the GNU
# General Public License. You can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation;
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
#
# VARIABLES (static and dynamical)
#
include dyn_variables.mk
include $(compdir)/config/mk/local/static_variables.mk
include $(compdir)/config/mk/local/defs.mk
include $(compdir)/config/mk/defs.mk
#
# SETUP
#
include $(compdir)/config/setup
#
# Module dependencies
#
-include local_modules.dep
#
# VARIABLES
#
idriver=$(IFLAG)$(libdir)/yambo/Ydriver/include $(IFLAG)$(includedir)/driver $(IFLAG)$(includedir)/version
lf90include=$(IFLAG)$(includedir) $(IFLAG)$(includedir)/headers/common $(IFLAG)$(includedir)/headers/parser $(idriver)
lf90libinclude=$(iiotk) $(ipnetcdf) $(inetcdff) $(inetcdf) $(ipetsc) $(islepc) $(ihdf5) $(ilibxc) $(ifft) $(ifutile) $(iyaml) $(idriver)
mfiles=find . -maxdepth 1 -name '*.mod'
#
# OBJECTS
#
include objects.mk
#
# OPERATIONS
#
include $(compdir)/config/mk/local/operations.mk
#
# RULES
#
include $(compdir)/config/mk/local/rules.mk
#
# FUNCTIONS
#
include $(compdir)/config/mk/local/functions.mk
#
