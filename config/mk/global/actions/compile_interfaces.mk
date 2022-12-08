#
# Variable definitions
#
I_PRECMP=-D_ELPH
ifneq (,$(findstring p2y,$(MAKECMDGOALS)))
 I_PRECMP+=$(p2ycpp)
endif
#
GOALS=a2y c2y
ifeq ($(do_p2y),yes)
 GOALS+=p2y
endif
ifeq ($(do_e2y),yes)
 GOALS+=e2y
endif
#
# Compilation
#
$(GOALS):
	@rm -f ${compdir}/log/"compile_"$@".log"
	@rm -f ${compdir}/config/stamps_and_lists/compilation_stop_$@.stamp
	@touch ${compdir}/config/stamps_and_lists/compiling_$@.stamp
	@$(MAKE) $(MAKEFLAGS) dependencies
	@$(MAKE) $(MAKEFLAGS) ext-libs
	@$(MAKE) $(MAKEFLAGS) int-libs
	@+LIBS="$(YLIBDRIVER)";LAB="$@_Ydriver_";BASE="lib/yambo/Ydriver/src";ADF="$(I_PRECMP) -D_$@";$(todo_lib);$(mk_lib)
	@+LIBS="$(2YLIBS)";BASE="src";ADF="$(I_PRECMP)";$(todo_lib);$(mk_lib)
	@+LIBS="int_modules";BASE="interfaces";ADF="$(I_PRECMP)";$(todo_lib);$(mk_lib)
	@+X2DO="$@";BASE="interfaces/$@";XLIBS="$(2YLIBS_LD)";ADF="$(I_PRECMP)";$(todo_driver)
	@+X2DO="$@";BASE="interfaces/$@";XLIBS="$(2YLIBS_LD)";ADF="$(I_PRECMP)";$(mk_exe)
