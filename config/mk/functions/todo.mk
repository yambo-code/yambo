define todo
 echo "\t[$(2)] $(1) (checking work to be done)"
 ./sbin/compilation/helper.sh -n -t $(1) -d $(2) -N $(MAKEFLAGS) -- $(xcpp) $(3)
endef
