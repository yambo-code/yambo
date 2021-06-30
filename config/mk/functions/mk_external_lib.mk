define mk_external_lib
 if test ! -d "lib/archive" ; then mkdir -p "lib/archive" ; fi ; \
 if [ "$(compdir)" != "$(prefix)" ] ; then \
   cp $(compdir)/lib/archive/* lib/archive; \
   cp $(compdir)/config/missing config/ ; \
 fi ; \
 for ldir in $$LIBS2DO; do \
  if test ! -f "$(libdir)/lib$$ldir.a" ; then \
   if [ "$(compdir)" != "$(prefix)" ] ; then \
    if test ! -d "$$DIR2GO/$$ldir" ; then mkdir -p "$$DIR2GO/$$ldir" ; fi ; \
    if test -e $$VPATH/$$ldir/Makefile.loc; then cp $$VPATH/$$ldir/Makefile.loc $$DIR2GO/$$ldir/ ; fi ;\
    if test -e $$VPATH/$$ldir/Makefile.lib; then cp $$VPATH/$$ldir/Makefile.lib $$DIR2GO/$$ldir/ ; fi ;\
    count=`ls -1 $$VPATH/$$ldir/*inc* 2>/dev/null | wc -l` ;\
    if [ $$count != 0 ] ; then  cp $$VPATH/$$ldir/*inc*  $$DIR2GO/$$ldir/ ;   fi ;\
   fi ; \
   echo " " ; \
   echo ">>>[Making $$ldir]<<<" ; \
   cd $$DIR2GO/$$ldir ; cp Makefile.loc Makefile ; $(MAKE) VPATH=$$VPATH/$$ldir || exit "$$?" ; cd ../../ ; \
  fi \
 done
endef
