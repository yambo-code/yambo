define todo
 for lib in $$LIBS; do \
  echo "\t[$$BASE/$$lib] $$lib (checking work to be done)"; \
  ./sbin/compilation/helper.sh -n -t lib$$LAB$$lib -d $$BASE/$$lib -N $(MAKEFLAGS)-g $@  -- $(xcpp) $$ADF;\
 done
endef
#
define todo_driver
 echo "\t[driver] $$X2DO (checking work to be done)";\
 ./sbin/compilation/helper.sh -n -t $$X2DO -d driver -N $(MAKEFLAGS) -g $@  -- $(xcpp) $$ADF
endef

