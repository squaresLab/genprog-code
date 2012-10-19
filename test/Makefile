CC=gcc
OCAMLOPT = ocamlopt -w Aelzv

## Specific tests
.PHONY: clean test

all: test

%.cmx: %.ml
	$(MAKE) -C ../src/ $(MAKECMDGOALS)

# run all tests
test: *.test
	FAILED=0; \
	for test in *.test ;do \
		./$$test >/dev/null 2>/dev/null; \
		if [ "$$?" -eq "0" ];then result=PASS; \
		else result=FAIL; FAILED=$$(echo "$$FAILED+1"|bc); fi; \
		echo "=============== $$result $$test"; \
	done; echo "failed $$FAILED tests";

clean:
	$(MAKE) -C gcd-test/ $(MAKECMDGOALS);
