#
# Functions
#===========
#
# Driver
#--------
define driver
 $(PREFIX)(eval $(cc) $(cflags) $(precomp_flags) $(lf90include) -L$(libdir) -D_$(target) -c $(libdir)/yambo/driver/src/driver/driver.c >> $(STDLOG) 2>&1  )
endef
#
# Linking
#---------
define link
 $(PREFIX)(eval $(fc) $(fcflags) $(lf90include) $(lf90libinclude) -o $(target) driver.o $(objs) $(libs) >> $(STDLOG) 2>&1 ;\
 echo "\t[$(wdir)] $(target) (link)";\
 touch $(compdir)/config/stamps_and_lists/$(target).stamp )
endef
#
# Compilation
#-------------
define c_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(cc) $(cflags) $(precomp_flags) $(lf90include) -c $(srcdir)/$(wdir)/$*.c  >> $(STDLOG) 2>&1 )
 $(msg)
endef
define f77_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(f77) -c $(fflags) $(srcdir)/$(wdir)/$*.f   >> $(STDLOG) 2>&1 )
 $(msg)
endef
define f77_no_opt_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(f77) -c $(fuflags) $(srcdir)/$(wdir)/$*.f  >> $(STDLOG) 2>&1 )
 $(msg)
endef
define F90_no_opt_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(fpp) $(precomp_flags) $(lf90include) $(lf90libinclude) $(srcdir)/$(wdir)/$*.F >> $*.tmp_source)
 $(PREFIX)($(srcdir)/sbin/replacer.sh $*.tmp_source)
 $(PREFIX)(mv $*.tmp_source_space $*$(f90suffix))
 $(PREFIX)($(fc) -c $(fcuflags) $(lf90include) $(lf90libinclude) $*$(f90suffix)  >> $(STDLOG) 2>&1 )
 $(msg)
endef
define F90_local_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(fpp) $(precomp_flags) $*.F > $*$(f90suffix) >>& $(STDLOG) 2>&1 )
 $(PREFIX)($(fc) -c $(fcflags) $(lf90include) $(lf90libinclude) $*$(f90suffix) >>& $(STDLOG) 2>&1 )
 $(msg)
endef
define F90_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(fpp) $(precomp_flags) $(lf90include) $(lf90libinclude) $(srcdir)/$(wdir)/$*.F > $*.tmp_source)
 $(PREFIX)($(srcdir)/sbin/replacer.sh $*.tmp_source)
 $(PREFIX)(mv $*.tmp_source_space $*$(f90suffix))
 $(PREFIX)($(fc) -c $(fcflags) $(lf90include) $(lf90libinclude) $*$(f90suffix)  >> $(STDLOG) 2>&1)
 $(msg)
endef
define modmove
 $(PREFIX)MODS=`find . -name '*.mod'`;for modfile in $$MODS ; do mv $$modfile $(compdir)/include; done
endef
define mk_lib
 $(PREFIX)(eval $(ar) $(arflags) $(target) $(objs)  >> $(STDLOG) 2>&1  )
 $(PREFIX)(mv $(target) $(libdir))
 $(PREFIX)(chmod u+x $(libdir)/$(target))
 $(PREFIX)(echo "\t[$(wdir)] $(target) (lib)")
 $(PREFIX)(touch $(compdir)/config/stamps_and_lists/$(target).stamp)
endef
#
# Utils
#------- 
define bindir
 $(PREFIX)if test ! -d $(exec_prefix); then mkdir $(exec_prefix);fi
endef
define msg
 $(PREFIX)(echo "\t[$(wdir)] $*" )
endef

