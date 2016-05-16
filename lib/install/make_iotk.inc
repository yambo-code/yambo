# lib/install/make_iotk.inc.  Generated from make_iotk.inc.in by configure.

# -----------------------------
# compilation rules

.SUFFIXES :
.SUFFIXES : .o .c .F .f90

.F.o:
	@rm -f $*.f90
	(eval $(CPP) $(CPPFLAGS) $(dopts) $*.F > $*.f90 ) > /dev/null
	(eval $(F90) $(F90FLAGS) -c $*.f90 ) > /dev/null
	@rm -f $*.f90

#.f.o:
#	$(F77) $(FFLAGS) -c $<
#
#.c.o:
#	$(CC) $(CFLAGS) -c $<
#
# -----------------------------
# compiler definitions

make             = make
CC               = gcc
CFLAGS           = -g -O2 -D_C_US -D_FORTRAN_US $(IFLAGS)
CPP              = gcc -E -P
CPPFLAGS         = -P $(IFLAGS)
F90              = gfortran
MPIF90           = gfortran
F90FLAGS         = -g -O3 -mtune=native     $(IFLAGS)
F77              = gfortran
FFLAGS           = -g -O3 -mtune=native
LD               = gfortran
LDFLAGS          = -g -O3 -mtune=native     $(IFLAGS)
AR               = ar
ARFLAGS          = -ru
RANLIB           = echo
#RANLIB           = @ranlib@


# -----------------------------
# libraries

#
LIBS             = ./libiotk.a
IFLAGS           = -I./ -I../ -I../include
