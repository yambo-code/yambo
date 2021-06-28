#
# Functions
#===========
#
# Driver
#--------
define driver
 $(PREFIX)(eval $(cc) $(cflags) $(precomp_flags) $(lf90include) -L$(libdir) -D_$@ -c $(libdir)/yambo/driver/src/driver/driver.c $(LOGID)> /dev/null)
 $(msg)
endef
#
# Linking
#---------
define link
 $(PREFIX)(eval $(fc) $(fcflags) $(lf90include) $(lf90libinclude) -o $@ driver.o $(objs) $(libs) $(LOGID)> /dev/null)
 $(msg)
endef
#
# Compilation
#-------------
define c_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(cc) $(cflags) $(precomp_flags) $(lf90include) -c $(srcdir)/$(wdir)/$*.c $(LOGID)> /dev/null)
 $(msg)
endef
define f77_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(f77) -c $(fflags) $(srcdir)/$(wdir)/$*.f $(LOGID)> /dev/null)
 $(msg)
endef
define f77_no_opt_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(f77) -c $(fuflags) $(srcdir)/$(wdir)/$*.f $(LOGID)> /dev/null)
 $(msg)
endef
define F90_no_opt_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(fpp) $(precomp_flags) $(lf90include) $(lf90libinclude) $(srcdir)/$(wdir)/$*.F > $*.tmp_source)
 $(PREFIX)($(srcdir)/sbin/replacer.sh $*.tmp_source)
 $(PREFIX)(mv $*.tmp_source_space $*$(f90suffix))
 $(PREFIX)($(fc) -c $(fcuflags) $(lf90include) $(lf90libinclude) $*$(f90suffix) $(LOGID)> /dev/null)
 $(msg)
endef
define F90_local_elemental_compilation
 $(rm_command)
 $(PREFIX)(eval $(fpp) $(precomp_flags) $*.F > $*$(f90suffix)) > /dev/null
 $(PREFIX)($(fc) -c $(fcflags) $(lf90include) $(lf90libinclude) $*$(f90suffix) $(LOGID)> /dev/null)
 $(msg)
endef
define F90_elemental_compilation
 if ! test -f $(compdir)/stamps_and_lists/$(target).stamp; 
 then 
 $(rm_command)
 $(PREFIX)(eval $(fpp) $(precomp_flags) $(lf90include) $(lf90libinclude) $(srcdir)/$(wdir)/$*.F > $*.tmp_source)
 $(PREFIX)($(srcdir)/sbin/replacer.sh $*.tmp_source)
 $(PREFIX)(mv $*.tmp_source_space $*$(f90suffix))
 $(PREFIX)($(fc) -c $(fcflags) $(lf90include) $(lf90libinclude) $*$(f90suffix) $(LOGID)> /dev/null)
 $(msg)
 fi
endef
#
# Object(s) operations
#----------------------
#define o_save
# $(PREFIX)(if test "$(keep_objs)" = "yes"; \
# then $(compdir)/sbin/objects_store.sh $(objects_lock); rm -f $(moduledep_file) $(modlist_file); fi)
# $(PREFIX)(if test "$(keep_objs)" = "no"; then rm -f $(moduledep_file) $(modlist_file); fi)
#endef
#define o_and_mod_clean
# $(PREFIX)(if test ! -f $(objects_lock) && test "$(keep_objs)" = "no" ; then \
# find . \( -name '*.o' -o -name '*.mod' -o -name '__*' \) | xargs rm -f ; \
# touch $(objects_lock); rm -f $(moduledep_file) $(modlist_file); fi)
#endef
#define mk_mod_dir
# $(PREFIX)(if test ! -d $(modinclude); then echo "creating folder $(modinclude)" ; fi)
# $(PREFIX)(if test ! -d $(modinclude); then mkdir $(modinclude) ; fi)
#endef
define modmove
 $(PREFIX)MODS=`find . -name '*.mod'`;for modfile in $$MODS ; do mv $$modfile $(compdir)/include; done
endef
define mk_lib
 $(PREFIX)(eval $(ar) $(arflags) $(target) $(objs) $(LOGID)> /dev/null)
 $(PREFIX)(mv $(target) $(libdir))
 $(PREFIX)(chmod u+x $(libdir)/$(target))
 $(PREFIX)(echo "\t [$(wdir)] $(target)" )
 $(PREFIX)(touch $(compdir)/config/stamps_and_lists/$(target).stamp)
endef
#
# Utils
#------- 
define dircheck
 $(PREFIX)if test ! -d $(exec_prefix); then mkdir $(exec_prefix);fi
endef
define msg
 $(PREFIX)(echo "\t [$(wdir)] $*" )
endef

