#
# License-Identifier: GPL
#
# Copyright (C) 2020 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
define mk_external_lib
 for lib in $$LIBS; do \
  if test ! -f "$(libdir)/lib$$lib.a" ; then \
   cd $$BASE/$$lib ; cp Makefile.loc Makefile ; $(MAKE) $(MAKEFLAGS) VPATH=$(srcdir)/$$BASE/$$lib || { grep Error $(compdir)/log/compile_$@*.log ; exit "$$?"; } ; cd ../../ ; \
  fi \
 done
endef
