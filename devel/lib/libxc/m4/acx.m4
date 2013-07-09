## Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.
##
## $Id: acx.m4 3881 2008-03-12 23:51:07Z xavier $
##

################################################
# Check whether the compiler accepts very long lines.
# ----------------------------------
AC_DEFUN([ACX_LONG_FORTRAN_LINES],
[AC_MSG_CHECKING([whether the compiler accepts very long lines])
AC_COMPILE_IFELSE( AC_LANG_PROGRAM( [], [
write(*, *) '456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678904567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789045678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890' ]), 
 [acx_long_lines_ok=yes; AC_DEFINE(LONG_LINES, 1, [compiler supports long lines])], [acx_long_lines_ok=no])
AC_SUBST([LONG_LINES], [$acx_long_lines_ok])
AC_MSG_RESULT($acx_long_lines_ok)
])


################################################
# Check whether the compiler accepts preprocessor "# line-number" lines.
# ----------------------------------
AC_DEFUN([ACX_F90_ACCEPTS_LINE_NUMBERS],
[
AC_MSG_CHECKING([whether the compiler accepts "line-number" lines cast by the preprocessor])
AC_COMPILE_IFELSE(
    AC_LANG_PROGRAM( [], [# 1]),
    [acx_f90_accepts_line_numbers_ok=yes
    AC_DEFINE(F90_ACCEPTS_LINE_NUMBERS, 1, [compiler supports line-number lines])],
    [acx_f90_accepts_line_numbers_ok=no])
AC_SUBST(F90_ACCEPTS_LINE_NUMBERS, $acx_f90_accepts_line_numbers_ok)
AC_MSG_RESULT($acx_f90_accepts_line_numbers_ok)
]
)

################################################
# Check for the presence of a given function in Fortran.
# It substitutes AC_CHECK_FUNC, since the latter
# seems to fail with some autotools versions, due to a call to some broken
# version of AC_LANG_FUNC_LINK_TRY.
AC_DEFUN([ACX_FORTRAN_CHECK_FUNC],
[
AC_MSG_CHECKING([for $1])
AC_LANG_PUSH(Fortran)dnl
AC_TRY_LINK_FUNC($1, 
[
acx_fortran_check_func=yes
AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_$1]),1, [Define if the $1 function can be called from Fortran])], 
[
acx_fortran_check_func=no
])dnl
AC_LANG_POP(Fortran)dnl
AC_MSG_RESULT($acx_fortran_check_func)
])


################################################
# AC_LANG_FUNC_LINK_TRY(Fortran)(FUNCTION)
# ----------------------------------
m4_define([AC_LANG_FUNC_LINK_TRY(Fortran)],
[AC_LANG_PROGRAM([], [call [$1]])])

################################################
# AC_LANG_PREPROC(Fortran)
# ---------------------------
m4_define([AC_LANG_PREPROC(Fortran)],[
  # this should not be hardwired
  if test -z "$FCCPP"; then FCCPP="/lib/cpp -C -ansi"; fi
  AC_ARG_VAR(FCCPP, [Fortran preprocessor. Defaults to '/lib/cpp -C -ansi'])
])
