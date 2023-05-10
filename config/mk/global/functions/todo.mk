define todo_lib
 for lib in $$LIBS; do \
  $(ECHO) "\t[$$BASE/$$lib] $$lib (checking work to be done)"; \
  ./sbin/compilation/helper.sh -n -t lib$$LAB$$lib -d $$BASE/$$lib -N $(MAKEFLAGS) -m $(fast) -g $@  -- $(xcpp) $$ADF;\
 done
endef
define todo_driver
 $(ECHO) "\t[$$BASE] $$X2DO (checking work to be done)";\
 ./sbin/compilation/helper.sh -n -t $$X2DO -d $$BASE -N $(MAKEFLAGS) -g $@  -- $(xcpp) $$ADF
endef

