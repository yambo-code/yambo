#
# License-Identifier: GPL
#
# Copyright (C) 2021 The Yambo Team
#
# Authors (see AUTHORS file for details): AM
#
define todo_lib
 for lib in $$LIBS; do \
  $(ECHO) "\t[$$BASE/$$lib] $$lib (setup)"; \
  ./sbin/compilation/helper.sh -n -t lib$$LAB$$lib -d $$BASE/$$lib -N $(MAKEFLAGS) -m $(fast) -g $@  -- $(xcpp) $$ADF;\
 done
endef
define todo_driver
 $(ECHO) "\t[$$BASE] $$X2DO (setup)";\
 ./sbin/compilation/helper.sh -n -t $$X2DO -d $$BASE -N $(MAKEFLAGS) -g $@  -- $(xcpp) $$ADF
endef
