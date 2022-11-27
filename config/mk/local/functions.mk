#
# Functions
#===========
#
# Driver
#--------
define driver
 $(PREFIX)(echo "$(cc) $(cflags) $(precomp_flags) $(lf90include) -L$(libdir) -D_$(target) -c $(srcdir)/lib/yambo/Ydriver/src/driver/driver.c" >> $(STDLOG) )
 $(PREFIX)(eval $(cc) $(cflags) $(precomp_flags) $(lf90include) -L$(libdir) -D_$(target) -c $(srcdir)/lib/yambo/Ydriver/src/driver/driver.c >> $(STDLOG) 2>&1  )
endef
#
# Linking
#---------
define link
 $(PREFIX)(echo "$(fc) $(fcflags) $(lf90include) $(lf90libinclude) -o $(target) driver.o $(objs) $(libs)" >> $(STDLOG) ;\
 eval $(fc) $(fcflags) $(lf90include) $(lf90libinclude) -o $(target) driver.o $(objs) $(libs) >> $(STDLOG) 2>&1;\
 $(ECHO) "\t[$(wdir)] $(target) (link)";\
 if test -f $(target); then \
   rm driver.o \
   rm -f $(compdir)/config/stamps_and_lists/compiling_$(target).stamp; \
   touch $(compdir)/config/stamps_and_lists/$(target).stamp; \
 fi )
endef
#
# Compilation
#-------------
define c_elemental_compilation
 $(rm_command)
 $(PREFIX)(echo "$(cc) $(cflags) $(precomp_flags) $(lf90include) -c $(srcdir)/$(wdir)/$*.c" >> $(STDLOG) )
 $(PREFIX)(eval $(cc) $(cflags) $(precomp_flags) $(lf90include) -c $(srcdir)/$(wdir)/$*.c  >> $(STDLOG) 2>&1 )
 $(msg)
endef
define f77_elemental_compilation
 $(rm_command)
 $(PREFIX)(echo "$(f77) -c $(fflags) $(srcdir)/$(wdir)/$*.f"   >> $(STDLOG) )
 $(PREFIX)(eval $(f77) -c $(fflags) $(srcdir)/$(wdir)/$*.f   >> $(STDLOG) 2>&1 )
 $(msg)
endef
define f77_no_opt_elemental_compilation
 $(rm_command)
 $(PREFIX)(echo "$(f77) -c $(fuflags) $(srcdir)/$(wdir)/$*.f"  >> $(STDLOG) )
 $(PREFIX)(eval $(f77) -c $(fuflags) $(srcdir)/$(wdir)/$*.f  >> $(STDLOG) 2>&1 )
 $(msg)
endef
define F90_no_opt_elemental_compilation
 $(rm_command)
 $(PREFIX)(echo "$(fpp) $(precomp_flags) $(lf90include) $(lf90libinclude) $(srcdir)/$(wdir)/$*.F >> $*.tmp_source" >> $(STDLOG) )
 $(PREFIX)(eval $(fpp) $(precomp_flags) $(lf90include) $(lf90libinclude) $(srcdir)/$(wdir)/$*.F >> $*.tmp_source)
 $(PREFIX)($(compdir)/sbin/replacer.sh $*.tmp_source)
 $(PREFIX)(mv $*.tmp_source_space $*$(f90suffix))
 $(PREFIX)(echo "$(fc) -c $(fcuflags) $(lf90include) $(lf90libinclude) $*$(f90suffix)"  >> $(STDLOG) )
 $(PREFIX)(eval $(fc) -c $(fcuflags) $(lf90include) $(lf90libinclude) $*$(f90suffix)  >> $(STDLOG) 2>&1 )
 $(msg)
endef
define F90_local_elemental_compilation
 $(rm_command)
 $(PREFIX)(echo "$(fpp) $(precomp_flags) $*.F > $*$(f90suffix)" >> $(STDLOG) )
 $(PREFIX)(eval $(fpp) $(precomp_flags) $*.F > $*$(f90suffix) >>& $(STDLOG) 2>&1 )
 $(PREFIX)($(fc) -c $(fcflags) $(lf90include) $(lf90libinclude) $*$(f90suffix) >>& $(STDLOG) 2>&1 )
 $(msg)
endef
define F90_elemental_compilation
 $(rm_command)
 $(PREFIX)(echo "$(fpp) $(precomp_flags) $(lf90include) $(lf90libinclude) $(srcdir)/$(wdir)/$*.F >> $*.tmp_source" >> $(STDLOG) )
 $(PREFIX)(eval $(fpp) $(precomp_flags) $(lf90include) $(lf90libinclude) $(srcdir)/$(wdir)/$*.F > $*.tmp_source)
 $(PREFIX)($(compdir)/sbin/replacer.sh $*.tmp_source)
 $(PREFIX)(mv $*.tmp_source_space $*$(f90suffix))
 $(PREFIX)(echo "$(fc) -c $(fcflags) $(lf90include) $(lf90libinclude) $*$(f90suffix)"  >> $(STDLOG))
 $(PREFIX)(eval $(fc) -c $(fcflags) $(lf90include) $(lf90libinclude) $*$(f90suffix)  >> $(STDLOG) 2>&1)
 $(msg)
endef
define modmove
 $(PREFIX)(MODS=`find $(compdir)/$(wdir) -name '*.mod' -not -path "$(compdir)/$(wdir)/*_.save/*"`; for modfile in $$MODS ; do cp $$modfile $(compdir)/include; done)
endef
define mk_lib
 $(PREFIX)(echo "$(ar) $(arflags) $(target) $(objs)"  >> $(STDLOG) )
 $(PREFIX)(eval $(ar) $(arflags) $(target) $(objs)  >> $(STDLOG) 2>&1  )
 $(PREFIX)(echo "mv $(target) $(libdir)" >> $(STDLOG) )
 $(PREFIX)(mv $(target) $(libdir))
 $(PREFIX)(chmod u+x $(libdir)/$(target))
 $(PREFIX)($(ECHO) "\t[$(wdir)] $(target) (lib)")
 $(PREFIX)(touch $(compdir)/config/stamps_and_lists/$(target).stamp)
endef
#
# Utils
#------- 
define bindir
 $(PREFIX)if test ! -d $(exec_prefix); then mkdir $(exec_prefix);fi
endef
define msg
 $(PREFIX)($(ECHO) "\t[$(wdir)] $*" )
endef

