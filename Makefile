# Makefile for Program Repair Tool (v2)

# see src/Makefile for more information

all: doc code lib

.PHONY: clean doc test

doc:
	$(MAKE) -C src/ $(MAKECMDGOALS)

code:
	$(MAKE) -C src/ $(MAKECMDGOALS)

lib:
	$(MAKE) -C src/ $(MAKECMDGOALS)

clean:
	$(MAKE) -C src/ $(MAKECMDGOALS);	\
	$(MAKE) -C test/ $(MAKECMDGOALS)

test:
	$(MAKE) -C test/ $(MAKECMDGOALS)
