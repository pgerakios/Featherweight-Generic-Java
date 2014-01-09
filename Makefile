.PHONY: .depend
all: .depend 
	$(MAKE) -f Makefile_dep

clean: .depend
	$(MAKE) -f Makefile_dep clean

# Rebuild intermodule dependencies
.depend: $(DEPEND) 
	ocamldep $(INCLUDE) *.mli *.ml > .depend
