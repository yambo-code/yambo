#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): HM
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
