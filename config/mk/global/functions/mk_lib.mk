#
# License-Identifier: GPL
#
# Copyright (C) 2021 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
define mk_lib
 for lib in $$LIBS; do \
  if test ! -f $(compdir)/config/stamps_and_lists/lib$$LAB$$lib.a.stamp; then \
   if test ! -d "$$BASE/$$lib" ; then mkdir -p "$$BASE/$$lib" ; fi ; \
   ./sbin/compilation/helper.sh -d $$BASE/$$lib -t lib$$LAB$$lib.a -o .objects -m l -g $@ -- "$(xcpp) $$ADF" ; \
   cd $$BASE/$$lib ; $(MAKE) $(MAKEFLAGS) VPATH=$(srcdir)/$$BASE/$$lib lib || { grep Error $(compdir)/log/compile_$@.log ; \
   touch $(compdir)/config/stamps_and_lists/compilation_stop_$@.stamp;  exit "$$?"; } ; cd $(compdir); \
  fi;\
 done
endef
