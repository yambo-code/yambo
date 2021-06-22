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
if [ "$1" = "HEADER" ] ; then
cat config/setup >>$cdir/Makefile
cat << EOF >> $cdir/Makefile
#
# Local Variables
#
libs=$libs
linclude=$lf90include
lf90libinclude=$lf90libinclude
lf90include=$lf90include
modinclude=$INCLUDEDIR//$modules_lock
mfiles=find . -maxdepth 1 -name '*.mod'
target=$target
precomp_mpi=$precomp_mpi
precomp_flags=$precomp_flags -D_\$(os)
objects_lock=$objects_lock
moduledep_file=$moduledep_file
modlist_file=$modlist_file
EOF
fi

if [ "$1" = "OBJECTS" ] ; then
cp $cdir/$ofile $cdir/$ofile.c
$cpp $cppflags $precomp_flags -D_$os -D_$target $cdir/$ofile.c >> $cdir/Makefile
rm -f $cdir/$ofile.c
fi

if [ "$1" = "x" ] ; then
cat << EOF >> $cdir/Makefile
\$(target): \$(moduledep_file) \$(objs)
	\$(driver)
	\$(link)
	\$(modmove)
	\$(dircheck)
	@mv \$@ \$(exec_prefix)
EOF
fi

if [ "$1" = "l" ] ; then
rm -f ./lib/$target
cat << EOF >> $cdir/Makefile
\$(target): \$(moduledep_file) arcreate
	\$(modmove) 
EOF
fi

if [ "$1" = "OPERATIONS" ] ; then
cat << EOF >> $cdir/Makefile
\$(objects_lock):
	\$(modpath)
	${PREFIX}if test ! -f \$(objects_lock) && test "\$(keep_objs)" = "no" ; then \
	find . \( -name '*.o' -o -name '*.mod' -o -name '__*' \) | xargs rm -f ; \
	touch \$(objects_lock); rm -f \$(moduledep_file) \$(modlist_file); fi
	${PREFIX}if test "\$(keep_objs)" = "yes"; \
	then \$(compdir)/sbin/objects_store.sh \$(objects_lock); rm -f \$(moduledep_file) \$(modlist_file); fi
	${PREFIX}if test "\$(keep_objs)" = "no"; then rm -f \$(moduledep_file) \$(modlist_file); fi
#
\$(moduledep_file): \$(objects_lock)
	cd \$(srcdir)/$cdir; \$(srcdir)/sbin/moduledep.sh \$(objs) > \$(compdir)/$cdir/\$(moduledep_file)
#
arcreate: \$(objs)
	${PREFIX}(eval \$(ar) \$(arflags) \$(target) \$(objs)) > /dev/null
	${PREFIX}mv \$(target) \$(libdir)  
	chmod u+x \$(libdir)/\$(target)
#
# Sources that do not want optimization
#
F77_NOOPT_SRC= xerbla.o slamch.o
#
\$(F77_NOOPT_SRC):
	${PREFIX}(eval \$(f77) -c \$(fuflags) \$(srcdir)/$cdir/\$*.f) > /dev/null
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

EOF
fi

if [ "$1" = "FUNCTIONS" ] ; then
cat << EOF >> $cdir/Makefile
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
EOF
fi

if [ "$1" = "RULES" ] ; then
cat << EOF >> $cdir/Makefile
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
.F.o:
	${rm_command}
	${PREFIX}(eval \$(fpp) \$(precomp_flags) \$(lf90include) \$(lf90libinclude) \$(srcdir)/$cdir/\$*.F > \$*.tmp_source)
	@\$(srcdir)/sbin/replacer.sh \$*.tmp_source
	@mv \$*.tmp_source_space \$*\$(f90suffix)
	${PREFIX}(\$(fc) -c \$(fcflags) \$(lf90include) \$(lf90libinclude) \$*\$(f90suffix)) > /dev/null
	${rm_command}
	@echo $ECHO_N \$*".F "
.f.o:
	${PREFIX}(eval \$(f77) -c \$(fflags) \$(srcdir)/$cdir/\$*.f)
	@echo $ECHO_N \$*".f "
.c.o:
	${PREFIX}(eval \$(cc) \$(cflags) \$(precomp_flags) \$(linclude) -c \$(srcdir)/$cdir/\$*.c) > /dev/null
	@echo $ECHO_N \$*".c"
EOF
fi
