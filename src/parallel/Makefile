
#====================================
#        Yambo 4.0.2 r.3451 h.3bfefb5  setup
#====================================
#
# Main setup
#
package_string    = Yambo 4.0.2 r.3451 h.3bfefb5 
cpu         = x86_64
os          = linux
debug       = yes
make        = make
ar          = ar
arflags     = -ru
cc          = gcc
cflags      = -g -O2 -D_C_US -D_FORTRAN_US
pcc         = mpicc
pccflags    = -g -O2 -D_C_US -D_FORTRAN_US
cpp         = gcc -E -P
cppflags    = -P 
fccpp       = cpp -E -P -ansi
f90         = gfortran
f90flags    = -g -O3 -mtune=native    
f90uflags   = -g -O0 -mtune=native   
f90suffix   = .f90
pf90        = mpif90
pf90flags   = -g -O3 -mtune=native    
f77         = gfortran
f77flags    = -g -O3 -mtune=native
f77uflags   = -g -O0 -mtune=native
package_bugreport = yambo@yambo-code.org
#
# Paths
#
prefix      = /home/marini/Yambo/yambo/master
exec_prefix = /home/marini/Yambo/yambo/master/bin
srcdir      = /home/marini/Yambo/yambo/master
libdir      = /home/marini/Yambo/yambo/master/lib
libpath     = -L/home/marini/Yambo/yambo/master/lib 
includedir  = /home/marini/Yambo/yambo/master/include
lblas       = /home/marini/Yambo/yambo/LIBs/gf/libblas.a
llapack     = /home/marini/Yambo/yambo/LIBs/gf/liblapack.a
lblacs      = 
lscalapack  = 
lmpi        =  
lopenmp     =  
lnetcdf     = -lnetcdff -lnetcdf
netcdf_aux  =  
netcdf_idir = 
llibxc      = /home/marini/Yambo/yambo/LIBs/gf/libxc.a
lfftqe      = 
lfft	    = -L/opt/fftw-3.3.4/lib -lfftw3
liotk	    = /home/marini/Yambo/yambo/LIBs/gf/libiotk.a
iotk_idir   = -I /home/marini/Yambo/yambo/INCLUDEs/gf
letsf	    =  
etsf_idir   =  
fc=$(pf90)
fcflags=$(pf90flags)
fcuflags=$(f90uflags)
llibs=$(libpath) 
linclude=-I$(includedir)
llibinclude=$(netcdf_idir)
lf90include=-I $(includedir) $(netcdf_idir)
mfiles=find . -name '*.mod'
target=libparallel.a
dmpi=yes
dopts= -D_MPI -D_FFTW -D_TIMING -D_$(os)
lock_file=__lock_D_FFTW_D_TIMING
objs = check_for_a_redundant_IO.o PARALLEL_defaults.o PARALLEL_live_message.o PARALLEL_clean_up_the_structure.o PARALLEL_Transitions_grouping.o $(RT_objs) PARALLEL_add_Q_to_K_list.o PARALLEL_minimal_index_from_overlaping.o PARALLEL_get_user_structure.o PARALLEL_assign_chains_and_COMMs.o PARALLEL_assign_LIN_ALGEBRA_COMMs.o PARALLEL_WF_index.o PARALLEL_structure.o PARALLEL_build_up_child_chains.o PARALLEL_Transitions_grouping.o PARALLEL_global_indexes.o PARALLEL_index.o PARALLEL_WF_distribute.o PARALLEL_Haydock_VEC_COMMs.o
$(target): locks $(objs) arcreate  
	$(modmove)$ 
locks:
	 if test ! -f $(lock_file) && test "$(debug)" = "no" ; then 	find . \( -name '*.o' -o -name '*.mod' -o -name '__*' \) | xargs rm -f ; 	touch $(lock_file); fi
	 if test "$(debug)" = "yes"; 	then "$(srcdir)"/sbin/objects_debug.sh $(lock_file); fi
arcreate:
	 (eval $(ar) $(arflags) $(target) $(objs)) > /dev/null
	 mv $(target) $(libdir) ; chmod u+x $(libdir)/$(target)
#
# Functions
#
define driver
  if test -f $(srcdir)/driver/yambo.h; then $(cpp) $(cppflags) $(dopts)                     $(srcdir)/driver/yambo.h > $(srcdir)/driver/yambo_cpp.h; fi
  if test -f $(srcdir)/driver/ypp.h; then $(cpp) $(cppflags) $(dopts)                    $(srcdir)/driver/ypp.h > $(srcdir)/driver/ypp_cpp.h; fi
  ( eval $(cc) $(cflags) -c $(srcdir)/driver/getopt.c > /dev/null)
  if test "$(dmpi)" = "";   then $(cc) $(cflags) $(dopts) -D_$@ -c $(srcdir)/driver/driver.c; fi
  if test ! "$(dmpi)" = "";   then $(pcc) $(pccflags) $(dopts) -D_$@ -c $(srcdir)/driver/driver.c; fi
  rm -f $(srcdir)/driver/yambo_cpp.h $(srcdir)/driver/ypp_cpp.h
 @echo
endef
define link
  (eval $(fc) $(fcflags) $(lf90include) -o $@ driver.o getopt.o $(objs) $(llibs) ) > /dev/null
endef
define modmove
  test `$(mfiles) | wc -l` -eq 0 || mv *.mod $(includedir)
endef
define dircheck
  if test ! -d $(exec_prefix); then mkdir $(exec_prefix);fi
endef
#
# Sources that do not want optimization
#
F77_NOOPT_SRC= xerbla.o slamch.o
#
$(F77_NOOPT_SRC):
	 (eval $(f77) -c $(f77uflags) $*.f) > /dev/null
#
FC_NOOPT_SRC= mod_parser_m.o mod_logo.o
#
$(FC_NOOPT_SRC):
	@rm -f $*$(f90suffix)
	 (eval $(cpp) $(cppflags) $(dopts) $*.F > $*$(f90suffix)) > /dev/null
	 (eval $(fc) -c $(fcuflags) $(lf90include) $*$(f90suffix)) > /dev/null
	@echo -n $* " "
	@rm -f $*$(f90suffix)
#
# Suffixes
#
.SUFFIXES: .F .f90 .c .f .o .a
#
# Rules
#
.F.o:
	@rm -f $*$(f90suffix)
	 (eval $(cpp) $(cppflags) $(dopts) $*.F > $*$(f90suffix)) > /dev/null
	 ($(fc) -c $(fcflags) $(lf90include) $(linclude) $*$(f90suffix)) > /dev/null
	@rm -f $*$(f90suffix)
	@echo -n $*".F "
.f.o:
	 (eval $(f77) -c $(f77flags) $*.f) > /dev/null
	@echo -n $*".f "
.c.o:
	 (eval $(cc) $(cflags) $(dopts) $(linclude) -c $<) > /dev/null
	@echo -n $< " "
