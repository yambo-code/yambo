#!/bin/bash
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
# DEFS (1): configure-based
ARGS=$@;
source ./config/driver.sh.inc
#
# OPTIONS
source ./sbin/driver_options.sh
#
# CHECK
if [ ! -f $cdir/$ofile ]; then exit 0; fi
#
# CLEAN
if [ -f $cdir/Makefile ] ; then rm -f $cdir/Makefile ;  fi
#
# DEFS (2)
pjdep_file="project.dep"
moduledep_file="module.dep"
modlist_file="modfiles.list"
#
# Projects
source ./sbin/driver_projects.sh
#
# Lock files
source ./sbin/driver_lock_files.sh
#
# Libraries
source ./sbin/driver_libraries.sh
#
# Libraries
source ./sbin/driver_includes.sh
#
#echo "driver.sh DIR: $cdir"
#echo "driver.sh TARG: $target"
#echo "driver.sh OFILE: $ofile"
#echo "driver.sh mode: $mode"
#echo "driver.sh LIBS: $libs"
#echo "driver.sh PRECOMP: $precomp_flags"
#echo "driver.sh M LOCKS: $modules_lock"
#echo "driver.sh O LOCKS: $objects_lock"
#
# Project dependencies
#if [ ! -f $cdir/project.dep ] ; then 
# for project in _SC _RT _ELPH _PHEL _NL _QED _YPP_ELPH _YPP_RT _YPP_NL _YPP_SC _DOUBLE
# do
#   @compdir@/sbin/projectdep.sh $cdir $project
# done
# for exe in _a2y _c2y _p2y _yambo _ypp
# do
#   @compdir@/sbin/projectdep.sh $cdir $exe
# done
#fi
#
# Makefile creation: (I) header
source ./sbin/driver_make_makefile.sh HEADER
#
# Makefile creation: (II) OBJECTS list
source ./sbin/driver_make_makefile.sh OBJECTS
#


if [ "$mode" = "x" ] ; then 
cat << EOF >> $cdir/Makefile
\$(target): \$(moduledep_file) \$(objs)
	\$(driver)
	\$(link)
	\$(modmove)
	\$(dircheck)
	@mv \$@ \$(exec_prefix)
EOF
else
rm -f ./lib/$target
cat << EOF >> $cdir/Makefile
\$(target): \$(moduledep_file) arcreate
	\$(modmove) 
EOF
fi

cat << EOF >> $cdir/Makefile
\$(objects_lock):
	\$(modpath)
	${PREFIX}if test ! -f \$(objects_lock) && test "\$(keep_objs)" = "no" ; then \
	find . \( -name '*.o' -o -name '*.mod' -o -name '__*' \) | xargs rm -f ; \
	touch \$(objects_lock); rm -f \$(moduledep_file) \$(modlist_file); fi
	${PREFIX}if test "\$(keep_objs)" = "yes"; \
	then \$(compdir)/sbin/objects_store.sh \$(objects_lock); rm -f \$(moduledep_file) \$(modlist_file); fi
	${PREFIX}if test "\$(keep_objs)" = "no"; then rm -f \$(moduledep_file) \$(modlist_file); fi

\$(moduledep_file): \$(objects_lock)
	cd \$(srcdir)/$cdir; \$(srcdir)/sbin/moduledep.sh \$(objs) > \$(compdir)/$cdir/\$(moduledep_file)

arcreate: \$(objs)
	${PREFIX}(eval \$(ar) \$(arflags) \$(target) \$(objs)) > /dev/null
	${PREFIX}mv \$(target) \$(libdir)  
	chmod u+x \$(libdir)/\$(target)
#
# Functions
#
define driver
 ${PREFIX}( eval \$(cc) \$(cflags) \$(precomp_flags) \$(linclude) -L\$(libdir) -D_\$@ -c \$(libdir)/yambo/driver/src/driver/driver.c > /dev/null)
 @echo
endef
define link
 ${PREFIX}(eval \$(fc) \$(fcflags) \$(lf90include) \$(lf90libinclude) -o \$@ driver.o \$(objs) \$(libs) ) > /dev/null
endef
define modpath
 ${PREFIX}if test ! -d \$(modinclude); then echo "creating folder \$(modinclude)" ; fi
 ${PREFIX}if test ! -d \$(modinclude); then mkdir \$(modinclude) ; fi
endef
define modmove
 ${PREFIX}test \`\$(mfiles) | wc -l\` -eq 0 || \$(mfiles) > \$(modlist_file)
 ${PREFIX}test \`\$(mfiles) | wc -l\` -eq 0 ||  mv *.mod \$(modinclude)
endef
define dircheck
 ${PREFIX}if test ! -d \$(exec_prefix); then mkdir \$(exec_prefix);fi
endef
#
# Sources that do not want optimization
#
F77_NOOPT_SRC= xerbla.o slamch.o
#
\$(F77_NOOPT_SRC):
	${PREFIX}(eval \$(f77) -c \$(fuflags) \$(srcdir)/$cdir/\$*.f) > /dev/null
EOF
rm_command="@rm -f \$*\$(f90suffix)"
if [ "$KEEPSRC" == "yes" ]; then rm_command=" "; fi ;
FC_NOOPT_SRC="mod_parser_m.o mod_logo.o"
for arg in $@; do
 case $arg in
  -D_PGI)
   FC_NOOPT_SRC="$FC_NOOPT_SRC bz_samp_indexes.o" ;;
 esac
done
cat << EOF >> $cdir/Makefile
#
FC_NOOPT_SRC= ${FC_NOOPT_SRC}
#
\$(FC_NOOPT_SRC):
	${rm_command}
	${PREFIX}(eval \$(fpp) \$(precomp_flags) \$(linclude) \$(srcdir)/$cdir/\$*.F > \$*.tmp_source)
	@\$(srcdir)/sbin/replacer.sh \$*.tmp_source
	@mv \$*.tmp_source_space \$*\$(f90suffix)
	${PREFIX}(eval \$(fc) -c \$(fcuflags) \$(lf90include) \$(lf90libinclude) \$*\$(f90suffix)) > /dev/null
	@echo $ECHO_N \$* " "
	${rm_command}
#
# Special sources
#
FC_LOCAL_SRC= sgfft.o
\$(FC_LOCAL_SRC):
	@rm -f \$*\$(f90suffix)
	${PREFIX}(eval \$(fpp) \$(precomp_flags) \$*.F > \$*\$(f90suffix)) > /dev/null
	${PREFIX}(\$(fc) -c \$(fcflags) \$(lf90include) \$(lf90libinclude) \$*\$(f90suffix)) > /dev/null
	@echo $ECHO_N \$*".F "

#
# Suffixes
#
.SUFFIXES: .F .f90 .c .f .o .a
#
# Includes
#
-include \$(moduledep_file)
#
# Rules
#
EOF
cat << EOF >> $cdir/Makefile
.F.o:
	${rm_command}
	${PREFIX}(eval \$(fpp) \$(precomp_flags) \$(lf90include) \$(lf90libinclude) \$(srcdir)/$cdir/\$*.F > \$*.tmp_source)
	@\$(srcdir)/sbin/replacer.sh \$*.tmp_source
	@mv \$*.tmp_source_space \$*\$(f90suffix)
	${PREFIX}(\$(fc) -c \$(fcflags) \$(lf90include) \$(lf90libinclude) \$*\$(f90suffix)) > /dev/null
	${rm_command}
	@echo $ECHO_N \$*".F "
EOF
cat << EOF >> $cdir/Makefile
.f.o:
	${PREFIX}(eval \$(f77) -c \$(fflags) \$(srcdir)/$cdir/\$*.f)
	@echo $ECHO_N \$*".f "
.c.o:
	${PREFIX}(eval \$(cc) \$(cflags) \$(precomp_flags) \$(linclude) -c \$(srcdir)/$cdir/\$*.c) > /dev/null
	@echo $ECHO_N \$*".c"
EOF
