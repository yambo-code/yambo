#
#        Copyright (C) 2000-2017 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): DS
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

# ============================================================================= 
# PATH FOR EXT LIBS
AC_ARG_WITH(extlibs_path,
            AC_HELP_STRING([--with-extlibs-path=<path>], [Path to the external libs],[]),
            [extlibs_path="$with_extlibs_path"],[extlibs_path="${PWD}/ext_libs"])
if test x"$extlibs_path" = "xyes"; then extlibs_path="${PWD}/ext_libs"; fi
if test x"$extlibs_path" = "x"; then extlibs_path="${PWD}/ext_libs"; fi
AC_SUBST(extlibs_path)
#
# ============================================================================
# DEBUG
AC_ARG_ENABLE(debug, AC_HELP_STRING([--enable-debug],[Objects are not removed but saved in appropriate directories. Default is yes.]))
if test x"$enable_debug" = "x"; then enable_debug="yes"; fi
AC_SUBST(enable_debug)
#
# ============================================================================= 
# KEEP SOURCE FILES 
AC_ARG_ENABLE(keep-src, AC_HELP_STRING([--enable-keep-src], [Keep preprocessed.f90 file. Default is no.]))
if test x"$enable_keep_src" = "x";    then enable_keep_src="no" ; fi
if test x"$enable_keep_src" = "xyes"; then enable_keep_src="yes"; fi
AC_SUBST(enable_keep_src)
#
# ============================================================================
# KEEP EXT LIBS
AC_ARG_ENABLE(keep-extlibs, AC_HELP_STRING([--enable-keep-extlibs], [Keep downloaded packages as tar.gz . Default is yes.]))
if test x"$enable_keep_extlibs" = "x"; then enable_keep_extlibs="yes"; fi
if test x"$enable_keep_extlibs" = "xno";  then
   enable_keep_extlibs="no"; 
   if test -e ./lib/archive/keep-extlibs-stamp ; then rm ./lib/archive/keep-extlibs-stamp ; fi
fi
if test x"$enable_keep_extlibs" = "xyes"; then
  enable_keep_extlibs="yes";
  if ! test -d ./lib/archive; then mkdir -p ./lib/archive ; fi
  touch ./lib/archive/keep-extlibs-stamp ;
fi
AC_SUBST(enable_keep_extlibs)
#
# ============================================================================
# DP
AC_ARG_ENABLE(dp, AC_HELP_STRING([--enable-dp], [Double-precision build. Default is no.]))
def_dp=""
if test x"$enable_dp" = "x"; then enable_dp="no"; fi
if test x"$enable_dp" = "xyes"; then def_dp="-D_DOUBLE"; fi
AC_SUBST(enable_dp)
AC_SUBST(def_dp)
#
# ============================================================================
#
# Time Profiling (mod_timing)
#
AC_ARG_ENABLE(time-profile, AC_HELP_STRING([--enable-time-profile],
              [Extended timing profile of specific sections]))
if test x"$enable_time_profile" = "x"; then enable_time_profile="no"; fi
def_time_profile=" "
if test x"$enable_time_profile" = "xyes"; then 
 def_time_profile="-D_TIMING"
fi
AC_SUBST(def_time_profile)

# ============================================================================
#
# Verbose compilation
#
AC_ARG_ENABLE(msgs-comps, AC_HELP_STRING([--enable-msgs-comps],
              [Verbose compilation log]))
if test x"$enable_msgs_comps" = "x"; then enable_msgs_comps="no"; fi
MKMF_PREFIX=" "
if test x"$enable_msgs_comps" = "xno"; then MKMF_PREFIX="@"; fi
AC_SUBST(MKMF_PREFIX)
AC_SUBST(ECHO_N)
# ============================================================================
# EDITOR
AC_ARG_WITH(editor, AC_HELP_STRING([--with-editor=<exe>],
  [User-defined editor (none for no editor)],[32]),[],[with_editor="vim vi pico"]) 
AC_CHECK_PROGS(editor,[$with_editor],[none])
AC_SUBST(editor)
